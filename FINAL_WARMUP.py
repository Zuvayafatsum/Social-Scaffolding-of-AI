# Dyadic image-classification warm-up — standalone version 2.3
#
# Main-task-matched warm-up for the current dyadic face experiment.
#
# Place this script directly inside:
#   /Users/mustafacvbe/Desktop/AI_FACE_EXPERIMENT/
#
# It expects:
#   AI_FACE_EXPERIMENT/Warmup_Images/
#   AI_FACE_EXPERIMENT/icons_for_experiment/Picture1.png
#   AI_FACE_EXPERIMENT/icons_for_experiment/Picture2.png
#
# It automatically creates and saves data in:
#   AI_FACE_EXPERIMENT/Warmup_Data/
#
# Warm-up response mapping is fixed:
#   LEFT  = AI
#   RIGHT = Real
#
# Trials 1–2 are guided and slower. Trials 3–6 use the normal main-task flow.
# No trigger tones are sent by this warm-up script.

import json
import math
import os
import random
import re
from datetime import datetime
from pathlib import Path

import pandas as pd
import psychopy
from psychopy import core, data, event, gui, visual
from psychopy.hardware import keyboard


# =============================================================================
# 1. Configuration
# =============================================================================
SCRIPT_DIR = Path(__file__).resolve().parent
WARMUP_IMAGE_DIR = SCRIPT_DIR / "Warmup_Images"
WARMUP_DATA_DIR = SCRIPT_DIR / "Warmup_Data"
ICONS_DIR = SCRIPT_DIR / "icons_for_experiment"

NO_TALK_ICON_FILE = "Picture1.png"
TALK_ICON_FILE = "Picture2.png"
PHASE_ICON_SIZE = 0.1
PHASE_ICON_POSITION = (0, 0.42)
INSTRUCTION_ICON_POSITION = (0, 0.28)
INSTRUCTION_TEXT_POSITION_WITH_ICON = (0, -0.07)

WARMUP_VERSION = "2.3-simplified-participant-instructions"

# Fixed mapping for every warm-up session.
LEFT_LABEL = "AI"
RIGHT_LABEL = "Real"

# IMPORTANT: Verify these authenticity labels before participant testing.
WARMUP_STIMULI = [
    {"filename": "000000167898.jpg", "authenticity": "Real"},
    {"filename": "33114-long-fooocus.png", "authenticity": "AI"},
    {"filename": "000000195045.jpg", "authenticity": "Real"},
    {"filename": "414795-long-fooocus.png", "authenticity": "AI"},
    {"filename": "000000298697.jpg", "authenticity": "Real"},
    {"filename": "447313-long-fooocus.png", "authenticity": "AI"},
]

# Participant 1 macropad: left / right
P1_KEYS = ["a", "d"]

# Participant 2 macropad: left / right
P2_KEYS = ["h", "k"]

ESCAPE_KEY = "escape"
ALL_RESPONSE_KEYS = P1_KEYS + P2_KEYS + [ESCAPE_KEY]

MACROPAD_CHECK_TIMEOUT_S = 5.0
FULLY_GUIDED_TRIALS = 2

# Main-task timing.
PRE_INDIVIDUAL_PROMPT_DURATION_S = 2.0
IMAGE_PRESENTATION_DURATION_S = 3.0
INDIVIDUAL_RESPONSE_DEADLINE_S = 15.0
INDIVIDUAL_COUNTDOWN_START_S = 5.0
JOINT_RESPONSE_DEADLINE_S = 60.0
JOINT_COUNTDOWN_START_S = 10.0
CONSENSUS_WARNING_DURATION_S = 3.0

# Guided trials are deliberately slower only at fixation.
GUIDED_FIXATION_DURATION_S = 1.5

# Natural trials reproduce the main-task fixation range.
NATURAL_FIXATION_MIN_S = 0.500
NATURAL_FIXATION_MAX_S = 0.900

# Exact main-task prompt colors.
PROMPT_RED_RGB255 = [136, 8, 8]    # Hex #880808
PROMPT_GREEN_RGB255 = [16, 66, 7]  # Hex #104207

# Window/display settings match the current main experiment.
WINDOW_SIZE = [3840, 2160]
WINDOW_SCREEN = 0
WINDOW_MONITOR = "testMonitor"
WINDOW_BACKGROUND = [0.9, 0.9, 0.9]

# Match the main experiment's stimulus centre and display height. Warm-up
# images are 640 × 480 pixels (4:3), so only their width differs in order to
# preserve their native aspect ratio.
IMAGE_POSITION = (0, 0.06)
IMAGE_DISPLAY_HEIGHT = 0.55
WARMUP_IMAGE_ASPECT_RATIO = 640.0 / 480.0
IMAGE_SIZE = (
    IMAGE_DISPLAY_HEIGHT * WARMUP_IMAGE_ASPECT_RATIO,
    IMAGE_DISPLAY_HEIGHT,
)
# Same arrow and option-label row as the main experiment.
RESPONSE_ROW_Y = -0.31
PRACTICE_COUNTER_Y = -0.485
FIXATION_CROSS_HEIGHT = 0.100

TEXT_COLOR = [-1, -1, -1]
FONT_TYPE = "Arial"
COMMON_TEXT_HEIGHT = 0.035
LABEL_TEXT_HEIGHT = 0.045

P1_BADGE_COLOR = [-0.8, -0.2, 1.0]    # Blue
P2_BADGE_COLOR = [1.0, -0.45, -0.45]  # Red
READY_GREEN = [-0.45, 0.75, -0.45]


# =============================================================================
# 2. Utility functions
# =============================================================================
def iso_now():
    """Return a local ISO-8601 timestamp with millisecond precision."""
    return datetime.now().astimezone().isoformat(timespec="milliseconds")


def rounded(value, digits=4):
    """Round numeric values while safely preserving None."""
    if value is None:
        return None
    return round(float(value), digits)


def seconds_to_ms(value, digits=3):
    """Convert seconds to milliseconds while safely preserving None."""
    if value is None:
        return None
    return round(float(value) * 1000.0, digits)


def safe_filename_component(value):
    """Convert a participant-entered value into a safe filename component."""
    cleaned = re.sub(r"[^A-Za-z0-9._-]+", "_", str(value).strip())
    return cleaned.strip("._") or "UNKNOWN"


def identify_first_responder(p1_rt_ms, p2_rt_ms):
    """Identify which participant responded first from two RT values."""
    if p1_rt_ms is None or p2_rt_ms is None:
        return None
    if p1_rt_ms < p2_rt_ms:
        return "Participant_1"
    if p2_rt_ms < p1_rt_ms:
        return "Participant_2"
    return "Simultaneous"


def response_rt_ms(key_event):
    """Return a PsychoPy Keyboard key-down RT in milliseconds."""
    rt_s = getattr(key_event, "rt", None)
    if rt_s is None:
        raise RuntimeError("A keyboard event was returned without an RT value.")

    rt_s = float(rt_s)
    if rt_s < -0.002 or rt_s >= 3600.0:
        raise RuntimeError(f"Invalid keyboard RT returned: {rt_s!r} seconds")

    return round(max(0.0, rt_s) * 1000.0, 3)


def key_event_sort_value(key_event):
    """Sort simultaneously retrieved key events by key-down timestamp."""
    rt = getattr(key_event, "rt", None)
    if rt is None:
        return float("inf")
    return float(rt)


def get_choice(key_name):
    """Translate a participant's left/right key into the fixed label mapping."""
    if key_name in [P1_KEYS[0], P2_KEYS[0]]:
        return LEFT_LABEL
    if key_name in [P1_KEYS[1], P2_KEYS[1]]:
        return RIGHT_LABEL
    return None


def atomic_write_csv(completed_rows, filename, current_row=None):
    """Atomically save completed trials plus an optional partial trial."""
    rows_to_write = [dict(row) for row in completed_rows]
    if current_row is not None:
        rows_to_write.append(dict(current_row))

    if not rows_to_write:
        return

    filename = str(filename)
    temporary_filename = filename + ".tmp"
    pd.DataFrame(rows_to_write).to_csv(temporary_filename, index=False)
    os.replace(temporary_filename, filename)


def show_setup_error(message):
    """Show a setup error before the full-screen window is created."""
    error_dialog = gui.Dlg(title="Warm-up setup error")
    error_dialog.addText(message)
    error_dialog.show()


# =============================================================================
# 3. Minimal GUI and preflight checks
# =============================================================================
exp_info = {"Dyad_ID": ""}

dlg = gui.DlgFromDict(
    dictionary=exp_info,
    title="Dyadic Classification Warm-up",
    order=["Dyad_ID"],
)

if not dlg.OK:
    core.quit()

if not str(exp_info["Dyad_ID"]).strip():
    show_setup_error("Dyad_ID cannot be empty.")
    core.quit()

missing_images = [
    stimulus["filename"]
    for stimulus in WARMUP_STIMULI
    if not (WARMUP_IMAGE_DIR / stimulus["filename"]).is_file()
]

no_talk_icon_path = ICONS_DIR / NO_TALK_ICON_FILE
talk_icon_path = ICONS_DIR / TALK_ICON_FILE
missing_icons = [
    str(path.name)
    for path in (no_talk_icon_path, talk_icon_path)
    if not path.is_file()
]

if missing_images or missing_icons:
    message_parts = []

    if missing_images:
        missing_text = "\n".join(f"• {name}" for name in missing_images)
        message_parts.append(
            "Missing warm-up image files in:\n"
            f"{WARMUP_IMAGE_DIR}\n\n{missing_text}"
        )

    if missing_icons:
        missing_text = "\n".join(f"• {name}" for name in missing_icons)
        message_parts.append(
            "Missing phase-icon files in:\n"
            f"{ICONS_DIR}\n\n{missing_text}"
        )

    show_setup_error("\n\n".join(message_parts))
    core.quit()

WARMUP_DATA_DIR.mkdir(parents=True, exist_ok=True)

session_start_iso = iso_now()
safe_dyad_id = safe_filename_component(exp_info["Dyad_ID"])
data_filename = WARMUP_DATA_DIR / (
    f"Warmup_Dyad_{safe_dyad_id}_{data.getDateStr()}.csv"
)


# =============================================================================
# 4. Hardware and window initialization
# =============================================================================
# One listener receives the distinct key codes generated by both macropads.
kb = keyboard.Keyboard()

win = visual.Window(
    size=WINDOW_SIZE,
    fullscr=True,
    monitor=WINDOW_MONITOR,
    units="height",
    screen=WINDOW_SCREEN,
    color=WINDOW_BACKGROUND,
    useRetina=True,
)

measured_refresh_rate_hz = win.getActualFrameRate(
    nIdentical=20,
    nMaxFrames=120,
    nWarmUpFrames=20,
    threshold=1,
)

if measured_refresh_rate_hz is None:
    fallback_frame_period_s = getattr(win, "monitorFramePeriod", None)
    if fallback_frame_period_s is not None and fallback_frame_period_s > 0:
        measured_refresh_rate_hz = 1.0 / fallback_frame_period_s
    else:
        measured_refresh_rate_hz = 60.0

frame_period_s = 1.0 / measured_refresh_rate_hz
pre_prompt_frame_count = max(
    1,
    int(round(PRE_INDIVIDUAL_PROMPT_DURATION_S / frame_period_s)),
)
image_frame_count = max(
    1,
    int(round(IMAGE_PRESENTATION_DURATION_S / frame_period_s)),
)
guided_fixation_frame_count = max(
    1,
    int(round(GUIDED_FIXATION_DURATION_S / frame_period_s)),
)

win.refreshThreshold = frame_period_s + 0.004
win.recordFrameIntervals = True


# =============================================================================
# 5. Visual elements
# =============================================================================
fixation_cross = visual.TextStim(
    win,
    text="+",
    pos=IMAGE_POSITION,
    height=FIXATION_CROSS_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
    bold=False,
)

stim_image = visual.ImageStim(
    win,
    size=IMAGE_SIZE,
    pos=IMAGE_POSITION,
)

phase_icon_position = PHASE_ICON_POSITION
no_talk_icon = visual.ImageStim(
    win,
    image=str(no_talk_icon_path),
    pos=phase_icon_position,
    size=(PHASE_ICON_SIZE, PHASE_ICON_SIZE),
)

talk_icon = visual.ImageStim(
    win,
    image=str(talk_icon_path),
    pos=phase_icon_position,
    size=(PHASE_ICON_SIZE, PHASE_ICON_SIZE),
)

instr_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0),
    height=COMMON_TEXT_HEIGHT,
    color=TEXT_COLOR,
    wrapWidth=1.35,
    font=FONT_TYPE,
)

indiv_prompt = visual.TextStim(
    win,
    text="No talking. Individual answers",
    pos=(0, 0),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_RED_RGB255,
    colorSpace="rgb255",
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

joint_prompt = visual.TextStim(
    win,
    text="You can talk now. Agree on one answer",
    pos=(0, 0.29),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_GREEN_RGB255,
    colorSpace="rgb255",
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

guided_joint_prompt = visual.TextStim(
    win,
    text=(
        "You can talk now. Discuss the image. Agree on one answer, "
        "then both enter it."
    ),
    pos=(0, 0.27),
    height=0.030,
    color=PROMPT_GREEN_RGB255,
    colorSpace="rgb255",
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

warning_text = visual.TextStim(
    win,
    text=(
        "Your answers did not match.\n"
        "Discuss again and enter the same answer."
    ),
    pos=(0, 0.29),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_RED_RGB255,
    colorSpace="rgb255",
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

individual_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0.31),
    height=COMMON_TEXT_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

joint_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0.18),
    height=COMMON_TEXT_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
    bold=True,
    wrapWidth=1.4,
)

practice_counter_text = visual.TextStim(
    win,
    text="",
    pos=(0, PRACTICE_COUNTER_Y),
    height=0.025,
    color=TEXT_COLOR,
    font=FONT_TYPE,
)

arrow_vertices = [
    (0, 0.02),
    (0.1, 0.02),
    (0.1, 0.05),
    (0.15, 0),
    (0.1, -0.05),
    (0.1, -0.02),
    (0, -0.02),
]

arrow_left = visual.ShapeStim(
    win,
    vertices=arrow_vertices,
    ori=180,
    fillColor=TEXT_COLOR,
    lineColor=TEXT_COLOR,
    pos=(-0.15, RESPONSE_ROW_Y),
)

arrow_right = visual.ShapeStim(
    win,
    vertices=arrow_vertices,
    ori=0,
    fillColor=TEXT_COLOR,
    lineColor=TEXT_COLOR,
    pos=(0.15, RESPONSE_ROW_Y),
)

label_left = visual.TextStim(
    win,
    text=LEFT_LABEL,
    pos=(-0.38, RESPONSE_ROW_Y),
    height=LABEL_TEXT_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
)

label_right = visual.TextStim(
    win,
    text=RIGHT_LABEL,
    pos=(0.38, RESPONSE_ROW_Y),
    height=LABEL_TEXT_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
)

# Startup macropad-check visual elements.
check_prompt_text = visual.TextStim(
    win,
    text="",
    pos=(0, -0.03),
    height=COMMON_TEXT_HEIGHT,
    color=TEXT_COLOR,
    font=FONT_TYPE,
    wrapWidth=1.2,
)

participant_badge = visual.Circle(
    win,
    radius=0.055,
    pos=(0, 0.15),
    edges=64,
    fillColor=P1_BADGE_COLOR,
    lineColor=None,
)

participant_badge_number = visual.TextStim(
    win,
    text="1",
    pos=(0, 0.15),
    height=0.048,
    color=[1, 1, 1],
    font=FONT_TYPE,
    bold=True,
)

p1_ready_badge = visual.Circle(
    win,
    radius=0.05,
    pos=(-0.09, 0.15),
    edges=64,
    fillColor=P1_BADGE_COLOR,
    lineColor=None,
)

p2_ready_badge = visual.Circle(
    win,
    radius=0.05,
    pos=(0.09, 0.15),
    edges=64,
    fillColor=P2_BADGE_COLOR,
    lineColor=None,
)

p1_ready_number = visual.TextStim(
    win,
    text="1",
    pos=(-0.09, 0.15),
    height=0.043,
    color=[1, 1, 1],
    font=FONT_TYPE,
    bold=True,
)

p2_ready_number = visual.TextStim(
    win,
    text="2",
    pos=(0.09, 0.15),
    height=0.043,
    color=[1, 1, 1],
    font=FONT_TYPE,
    bold=True,
)

ready_checkmark = visual.ShapeStim(
    win,
    vertices=[(-0.075, 0.00), (-0.025, -0.055), (0.095, 0.075)],
    closeShape=False,
    lineColor=READY_GREEN,
    fillColor=None,
    lineWidth=9,
    pos=(0, 0.13),
)


# =============================================================================
# 6. Drawing and instruction functions
# =============================================================================
def set_practice_counter(trial_number):
    practice_counter_text.text = (
        f"Practice trial {trial_number} of {len(WARMUP_STIMULI)}"
    )


def draw_practice_counter(trial_number):
    set_practice_counter(trial_number)
    practice_counter_text.draw()


def draw_labels():
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()


def draw_pre_individual_prompt(trial_number):
    no_talk_icon.draw()
    indiv_prompt.draw()
    draw_practice_counter(trial_number)


def draw_fixation_screen(trial_number):
    no_talk_icon.draw()
    fixation_cross.draw()
    draw_practice_counter(trial_number)


def draw_individual_image_screen(trial_number):
    stim_image.draw()
    no_talk_icon.draw()
    draw_labels()
    draw_practice_counter(trial_number)


def draw_individual_countdown(seconds_remaining):
    if 0.0 < seconds_remaining <= INDIVIDUAL_COUNTDOWN_START_S:
        displayed_seconds = max(1, int(math.ceil(seconds_remaining)))
        second_word = "second" if displayed_seconds == 1 else "seconds"
        individual_countdown_text.text = (
            f"{displayed_seconds} {second_word} left to provide your "
            "individual answers."
        )
        individual_countdown_text.draw()


def draw_individual_wait_screen(trial_number, seconds_remaining):
    no_talk_icon.draw()
    draw_individual_countdown(seconds_remaining)
    draw_labels()
    draw_practice_counter(trial_number)


def draw_joint_countdown(seconds_remaining):
    if 0.0 < seconds_remaining <= JOINT_COUNTDOWN_START_S:
        displayed_seconds = max(1, int(math.ceil(seconds_remaining)))
        second_word = "second" if displayed_seconds == 1 else "seconds"
        joint_countdown_text.text = (
            f"{displayed_seconds} {second_word} left to provide a joint answer."
        )
        joint_countdown_text.draw()


def draw_joint_screen(trial_number, seconds_remaining, guided_mode):
    talk_icon.draw()
    if guided_mode:
        guided_joint_prompt.draw()
    else:
        joint_prompt.draw()
    draw_joint_countdown(seconds_remaining)
    draw_labels()
    draw_practice_counter(trial_number)


def draw_consensus_warning(trial_number, seconds_remaining):
    talk_icon.draw()
    warning_text.draw()
    draw_joint_countdown(seconds_remaining)
    draw_labels()
    draw_practice_counter(trial_number)


def show_text_and_wait_for_space(message, icon=None):
    """Show a full-screen instruction and continue with Space.

    When an icon is supplied, it is displayed in a dedicated instruction
    position and the text is shifted downward so the two elements do not
    overlap. The icon and text positions are restored before returning.
    """
    event.clearEvents(eventType="keyboard")

    original_text_position = tuple(instr_text.pos)
    original_icon_position = tuple(icon.pos) if icon is not None else None

    try:
        if icon is not None:
            icon.pos = INSTRUCTION_ICON_POSITION
            instr_text.pos = INSTRUCTION_TEXT_POSITION_WITH_ICON
            icon.draw()
        else:
            instr_text.pos = (0, 0)

        instr_text.text = message
        instr_text.draw()
        win.flip()

        pressed = event.waitKeys(keyList=["space", ESCAPE_KEY])
        if pressed and ESCAPE_KEY in pressed:
            raise KeyboardInterrupt
    finally:
        instr_text.pos = original_text_position
        if icon is not None:
            icon.pos = original_icon_position


def show_feedback_and_wait_for_space(message):
    show_text_and_wait_for_space(message)


def draw_participant_badge(participant_number):
    if participant_number == 1:
        participant_badge.fillColor = P1_BADGE_COLOR
        participant_badge_number.text = "1"
    elif participant_number == 2:
        participant_badge.fillColor = P2_BADGE_COLOR
        participant_badge_number.text = "2"
    else:
        raise ValueError("participant_number must be 1 or 2")

    participant_badge.draw()
    participant_badge_number.draw()


def draw_both_participant_badges():
    p1_ready_badge.draw()
    p2_ready_badge.draw()
    p1_ready_number.draw()
    p2_ready_number.draw()


def show_check_message(
    message,
    duration_s=None,
    participant_number=None,
    show_both_participants=False,
):
    check_prompt_text.text = message

    if participant_number is not None:
        draw_participant_badge(participant_number)
    elif show_both_participants:
        draw_both_participant_badges()

    check_prompt_text.draw()
    win.flip()

    if duration_s is not None:
        core.wait(duration_s)


def show_ready_check_animation():
    check_prompt_text.text = (
        "Macropad check complete.\n\n"
        "Both participants are ready for the warm-up."
    )

    animation_clock = core.Clock()
    animation_duration_s = 0.55

    while animation_clock.getTime() < animation_duration_s:
        progress = min(1.0, animation_clock.getTime() / animation_duration_s)
        ready_checkmark.size = 0.65 + (0.35 * progress)
        ready_checkmark.opacity = progress
        check_prompt_text.opacity = progress

        ready_checkmark.draw()
        check_prompt_text.draw()
        win.flip()

    ready_checkmark.size = 1.0
    ready_checkmark.opacity = 1.0
    check_prompt_text.opacity = 1.0
    ready_checkmark.draw()
    check_prompt_text.draw()
    win.flip()
    core.wait(1.2)


def wait_for_macropad_button(participant_number, button_name, expected_key):
    """Verify one specified macropad button with two timed attempts."""
    button_name_upper = button_name.upper()
    prompt = (
        f"Participant {participant_number}, press the "
        f"{button_name_upper} button on your macropad."
    )

    for attempt_number in (1, 2):
        event.clearEvents(eventType="keyboard")
        show_check_message(prompt, participant_number=participant_number)

        pressed = event.waitKeys(
            maxWait=MACROPAD_CHECK_TIMEOUT_S,
            keyList=[expected_key, ESCAPE_KEY],
        )

        if pressed is not None:
            if ESCAPE_KEY in pressed:
                raise KeyboardInterrupt
            return

        if attempt_number == 1:
            show_check_message(
                "No button press was detected.\n\nPlease try again.",
                duration_s=1.5,
                participant_number=participant_number,
            )

    event.clearEvents(eventType="keyboard")
    show_check_message(
        "Please call the experimenter.\n\n"
        "It seems that your macropad is not connected.\n\n"
        f"After the connection has been checked, press the "
        f"{button_name_upper} button again.",
        participant_number=participant_number,
    )

    pressed = event.waitKeys(keyList=[expected_key, ESCAPE_KEY])
    if pressed and ESCAPE_KEY in pressed:
        raise KeyboardInterrupt


def run_macropad_check(participant_number, left_key, right_key):
    wait_for_macropad_button(
        participant_number=participant_number,
        button_name="left",
        expected_key=left_key,
    )

    show_check_message(
        f"Participant {participant_number}: left button detected.",
        duration_s=0.8,
        participant_number=participant_number,
    )

    wait_for_macropad_button(
        participant_number=participant_number,
        button_name="right",
        expected_key=right_key,
    )

    show_check_message(
        f"Participant {participant_number}: your macropad is connected.",
        duration_s=1.5,
        participant_number=participant_number,
    )


def wait_for_both_left_buttons_to_start():
    """Begin the warm-up only after both participants press LEFT."""
    event.clearEvents(eventType="keyboard")
    show_check_message(
        "The warm-up is ready.\n\n"
        "To begin, both participants should press LEFT on their own "
        "macropad.",
        show_both_participants=True,
    )

    p1_left_detected = False
    p2_left_detected = False

    while not (p1_left_detected and p2_left_detected):
        pressed = event.waitKeys(
            keyList=[P1_KEYS[0], P2_KEYS[0], ESCAPE_KEY],
        )

        if pressed and ESCAPE_KEY in pressed:
            raise KeyboardInterrupt

        if pressed and P1_KEYS[0] in pressed:
            p1_left_detected = True

        if pressed and P2_KEYS[0] in pressed:
            p2_left_detected = True


# =============================================================================
# 7. Main-task-matched phase functions
# =============================================================================
def present_pre_individual_prompt(trial_number, record):
    """Show the no-talk icon and red prompt for exactly two seconds."""
    timing_holder = {}

    def mark_prompt_onset():
        onset = session_clock.getTime()
        timing_holder["onset"] = onset
        record["Pre_Individual_Prompt_Onset_Session_Elapsed_s"] = rounded(onset)

    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(mark_prompt_onset)

    for _ in range(pre_prompt_frame_count):
        draw_pre_individual_prompt(trial_number)
        win.flip()

        escape_events = kb.getKeys(
            keyList=[ESCAPE_KEY],
            waitRelease=False,
            clear=True,
        )
        if escape_events:
            raise KeyboardInterrupt

    return timing_holder


def present_fixation(trial_number, record, guided_mode, pre_prompt_holder):
    """Show the no-talk icon with guided or natural fixation timing."""
    timing_holder = {}

    if guided_mode:
        sampled_duration_s = GUIDED_FIXATION_DURATION_S
        frame_count = guided_fixation_frame_count
    else:
        sampled_duration_s = random.uniform(
            NATURAL_FIXATION_MIN_S,
            NATURAL_FIXATION_MAX_S,
        )
        frame_count = max(
            1,
            int(round(sampled_duration_s / frame_period_s)),
        )

    planned_duration_s = frame_count * frame_period_s

    def mark_fixation_onset():
        onset = session_clock.getTime()
        timing_holder["onset"] = onset
        record["Pre_Individual_Prompt_Offset_Session_Elapsed_s"] = rounded(onset)
        record["Pre_Individual_Prompt_Actual_Duration_ms"] = seconds_to_ms(
            onset - pre_prompt_holder["onset"]
        )
        record["Fixation_Onset_Session_Elapsed_s"] = rounded(onset)

    record["Fixation_Sampled_Duration_ms"] = seconds_to_ms(sampled_duration_s)
    record["Fixation_Planned_Duration_ms"] = seconds_to_ms(planned_duration_s)
    record["Fixation_Frame_Count"] = frame_count

    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(mark_fixation_onset)

    for _ in range(frame_count):
        draw_fixation_screen(trial_number)
        win.flip()

        escape_events = kb.getKeys(
            keyList=[ESCAPE_KEY],
            waitRelease=False,
            clear=True,
        )
        if escape_events:
            raise KeyboardInterrupt

    return timing_holder


def register_individual_key(
    key_event,
    p1_state,
    p2_state,
    onset_time,
    record,
    during_image,
):
    """Register one valid individual key event and update trial state."""
    if key_event.name == ESCAPE_KEY:
        raise KeyboardInterrupt

    key_rt_ms = response_rt_ms(key_event)
    if key_rt_ms > seconds_to_ms(INDIVIDUAL_RESPONSE_DEADLINE_S):
        return

    if key_event.name in P1_KEYS and not p1_state["responded"]:
        p1_state["choice"] = get_choice(key_event.name)
        p1_state["rt_ms"] = key_rt_ms
        p1_state["responded"] = True
        record["P1_Indiv_Choice"] = p1_state["choice"]
        record["P1_Indiv_RT_ms"] = p1_state["rt_ms"]
        record["P1_Indiv_Response_During_Image"] = int(during_image)
        record["P1_Indiv_Response_Session_Elapsed_s"] = rounded(
            onset_time + (key_rt_ms / 1000.0)
        )
        record["P1_Indiv_Response_Within_Deadline"] = 1
        record["P1_Indiv_Timed_Out"] = 0

    if key_event.name in P2_KEYS and not p2_state["responded"]:
        p2_state["choice"] = get_choice(key_event.name)
        p2_state["rt_ms"] = key_rt_ms
        p2_state["responded"] = True
        record["P2_Indiv_Choice"] = p2_state["choice"]
        record["P2_Indiv_RT_ms"] = p2_state["rt_ms"]
        record["P2_Indiv_Response_During_Image"] = int(during_image)
        record["P2_Indiv_Response_Session_Elapsed_s"] = rounded(
            onset_time + (key_rt_ms / 1000.0)
        )
        record["P2_Indiv_Response_Within_Deadline"] = 1
        record["P2_Indiv_Timed_Out"] = 0


def collect_individual_phase(trial_number, record, fixation_holder):
    """
    Match the main task: fixed 3-second image, 15-second answer deadline,
    and final-five-second countdown after image offset when needed.
    """
    p1_state = {"responded": False, "choice": None, "rt_ms": None}
    p2_state = {"responded": False, "choice": None, "rt_ms": None}
    onset_holder = {}

    def mark_image_onset():
        onset = session_clock.getTime()
        onset_holder["time"] = onset
        record["Fixation_Offset_Session_Elapsed_s"] = rounded(onset)
        record["Stimulus_Onset_Session_Elapsed_s"] = rounded(onset)
        record["Individual_Response_Deadline_Session_Elapsed_s"] = rounded(
            onset + INDIVIDUAL_RESPONSE_DEADLINE_S
        )
        record["Fixation_Actual_Duration_ms"] = seconds_to_ms(
            onset - fixation_holder["onset"]
        )

    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(kb.clock.reset)
    win.callOnFlip(mark_image_onset)

    draw_individual_image_screen(trial_number)
    win.flip()

    # Initial onset flip is frame 1.
    for _ in range(max(0, image_frame_count - 1)):
        key_events = kb.getKeys(
            keyList=ALL_RESPONSE_KEYS,
            waitRelease=False,
            clear=True,
        )

        for key_event in sorted(key_events, key=key_event_sort_value):
            register_individual_key(
                key_event=key_event,
                p1_state=p1_state,
                p2_state=p2_state,
                onset_time=onset_holder["time"],
                record=record,
                during_image=True,
            )

        draw_individual_image_screen(trial_number)
        win.flip()

    # Capture any event still queued from the final visible image frame.
    final_visible_events = kb.getKeys(
        keyList=ALL_RESPONSE_KEYS,
        waitRelease=False,
        clear=True,
    )
    for key_event in sorted(final_visible_events, key=key_event_sort_value):
        register_individual_key(
            key_event=key_event,
            p1_state=p1_state,
            p2_state=p2_state,
            onset_time=onset_holder["time"],
            record=record,
            during_image=True,
        )

    image_offset_holder = {}

    def mark_image_offset():
        offset = session_clock.getTime()
        image_offset_holder["time"] = offset
        record["Image_Offset_Session_Elapsed_s"] = rounded(offset)
        record["Image_Visible_Duration_ms"] = seconds_to_ms(
            offset - onset_holder["time"]
        )

    both_responded_during_image = (
        p1_state["responded"] and p2_state["responded"]
    )
    record["Both_Responded_While_Image_Visible"] = int(
        both_responded_during_image
    )

    win.callOnFlip(mark_image_offset)
    seconds_remaining = max(
        0.0,
        INDIVIDUAL_RESPONSE_DEADLINE_S - kb.clock.getTime(),
    )
    draw_individual_wait_screen(trial_number, seconds_remaining)
    win.flip()

    # Continue collecting only missing responses until 15 seconds from onset.
    while (
        not (p1_state["responded"] and p2_state["responded"])
        and kb.clock.getTime() < INDIVIDUAL_RESPONSE_DEADLINE_S
    ):
        key_events = kb.getKeys(
            keyList=ALL_RESPONSE_KEYS,
            waitRelease=False,
            clear=True,
        )

        for key_event in sorted(key_events, key=key_event_sort_value):
            register_individual_key(
                key_event=key_event,
                p1_state=p1_state,
                p2_state=p2_state,
                onset_time=onset_holder["time"],
                record=record,
                during_image=False,
            )

        if not (p1_state["responded"] and p2_state["responded"]):
            seconds_remaining = max(
                0.0,
                INDIVIDUAL_RESPONSE_DEADLINE_S - kb.clock.getTime(),
            )
            if (
                seconds_remaining <= INDIVIDUAL_COUNTDOWN_START_S
                and record[
                    "Individual_Countdown_First_Shown_Session_Elapsed_s"
                ] is None
            ):
                record[
                    "Individual_Countdown_First_Shown_Session_Elapsed_s"
                ] = rounded(session_clock.getTime())

            draw_individual_wait_screen(trial_number, seconds_remaining)
            win.flip()

    # Final timestamp-bounded queue read.
    deadline_events = kb.getKeys(
        keyList=ALL_RESPONSE_KEYS,
        waitRelease=False,
        clear=True,
    )
    for key_event in sorted(deadline_events, key=key_event_sort_value):
        register_individual_key(
            key_event=key_event,
            p1_state=p1_state,
            p2_state=p2_state,
            onset_time=onset_holder["time"],
            record=record,
            during_image=False,
        )

    if not p1_state["responded"]:
        record["P1_Indiv_Response_Within_Deadline"] = 0
        record["P1_Indiv_Timed_Out"] = 1
    if not p2_state["responded"]:
        record["P2_Indiv_Response_Within_Deadline"] = 0
        record["P2_Indiv_Timed_Out"] = 1

    complete = p1_state["responded"] and p2_state["responded"]
    phase_end = session_clock.getTime()
    record["Individual_Phase_End_Session_Elapsed_s"] = rounded(phase_end)
    record["Individual_Phase_Duration_ms"] = seconds_to_ms(
        phase_end - onset_holder["time"]
    )
    record["Individual_Responses_Complete"] = int(complete)
    record["Individual_Missing_Response_Count"] = int(
        not p1_state["responded"]
    ) + int(not p2_state["responded"])

    if complete:
        completion_time = max(
            record["P1_Indiv_Response_Session_Elapsed_s"],
            record["P2_Indiv_Response_Session_Elapsed_s"],
        )
        record[
            "Individual_Responses_Complete_Session_Elapsed_s"
        ] = rounded(completion_time)

    return {
        "complete": complete,
        "p1_choice": p1_state["choice"],
        "p2_choice": p2_state["choice"],
        "p1_rt_ms": p1_state["rt_ms"],
        "p2_rt_ms": p2_state["rt_ms"],
        "image_offset_time": image_offset_holder["time"],
    }


def collect_consensus_phase(
    trial_number,
    record,
    image_offset_time,
    guided_mode,
):
    """Collect matching joint responses with the main-task deadline and retry."""
    joint_deadline_session_s = (
        image_offset_time + JOINT_RESPONSE_DEADLINE_S
    )
    record["Joint_Deadline_Start_Session_Elapsed_s"] = rounded(
        image_offset_time
    )
    record["Joint_Deadline_Session_Elapsed_s"] = rounded(
        joint_deadline_session_s
    )
    record["Discussion_Allowed_Onset_Session_Elapsed_s"] = None

    consensus_reached = False
    consensus_timed_out = False
    attempt_count = 0
    final_choice = None
    first_joint_prompt_onset = None
    first_joint_responder_overall = None
    attempts = []
    failed_attempts = []

    while not consensus_reached and not consensus_timed_out:
        if session_clock.getTime() >= joint_deadline_session_s:
            consensus_timed_out = True
            record["Consensus_Timeout_Phase"] = "Before_Joint_Attempt"
            break

        attempt_count += 1
        p1_state = {"responded": False, "choice": None, "rt_ms": None}
        p2_state = {"responded": False, "choice": None, "rt_ms": None}
        attempt_onset_holder = {}

        def mark_joint_onset():
            onset = session_clock.getTime()
            attempt_onset_holder["time"] = onset

            if record["Discussion_Allowed_Onset_Session_Elapsed_s"] is None:
                record[
                    "Discussion_Allowed_Onset_Session_Elapsed_s"
                ] = rounded(onset)

            if record["Joint_Prompt_First_Onset_Session_Elapsed_s"] is None:
                record[
                    "Joint_Prompt_First_Onset_Session_Elapsed_s"
                ] = rounded(onset)
                record["Joint_Prompt_Delay_After_Image_Offset_ms"] = (
                    seconds_to_ms(onset - image_offset_time)
                )

        win.callOnFlip(kb.clearEvents)
        win.callOnFlip(kb.clock.reset)
        win.callOnFlip(mark_joint_onset)

        seconds_remaining = max(
            0.0,
            joint_deadline_session_s - session_clock.getTime(),
        )
        draw_joint_screen(trial_number, seconds_remaining, guided_mode)
        win.flip()

        if first_joint_prompt_onset is None:
            first_joint_prompt_onset = attempt_onset_holder["time"]

        while not (p1_state["responded"] and p2_state["responded"]):
            key_events = kb.getKeys(
                keyList=ALL_RESPONSE_KEYS,
                waitRelease=False,
                clear=True,
            )

            for key_event in sorted(key_events, key=key_event_sort_value):
                if key_event.name == ESCAPE_KEY:
                    raise KeyboardInterrupt

                key_rt_ms = response_rt_ms(key_event)
                key_session_time = (
                    attempt_onset_holder["time"] + (key_rt_ms / 1000.0)
                )
                if key_session_time > joint_deadline_session_s:
                    continue

                if key_event.name in P1_KEYS and not p1_state["responded"]:
                    p1_state["choice"] = get_choice(key_event.name)
                    p1_state["rt_ms"] = key_rt_ms
                    p1_state["responded"] = True

                if key_event.name in P2_KEYS and not p2_state["responded"]:
                    p2_state["choice"] = get_choice(key_event.name)
                    p2_state["rt_ms"] = key_rt_ms
                    p2_state["responded"] = True

            if p1_state["responded"] and p2_state["responded"]:
                break

            seconds_remaining = max(
                0.0,
                joint_deadline_session_s - session_clock.getTime(),
            )
            if seconds_remaining <= 0.0:
                consensus_timed_out = True
                record["Consensus_Timeout_Phase"] = "Joint_Response_Attempt"
                break

            if (
                seconds_remaining <= JOINT_COUNTDOWN_START_S
                and record[
                    "Joint_Countdown_First_Shown_Session_Elapsed_s"
                ] is None
            ):
                record[
                    "Joint_Countdown_First_Shown_Session_Elapsed_s"
                ] = rounded(session_clock.getTime())

            draw_joint_screen(trial_number, seconds_remaining, guided_mode)
            win.flip()

        if consensus_timed_out:
            available_rts = [
                rt
                for rt in (p1_state["rt_ms"], p2_state["rt_ms"])
                if rt is not None
            ]
            attempts.append(
                {
                    "attempt": attempt_count,
                    "attempt_onset_session_elapsed_s": rounded(
                        attempt_onset_holder["time"]
                    ),
                    "p1_choice": p1_state["choice"],
                    "p1_rt_ms": p1_state["rt_ms"],
                    "p2_choice": p2_state["choice"],
                    "p2_rt_ms": p2_state["rt_ms"],
                    "first_responder": identify_first_responder(
                        p1_state["rt_ms"],
                        p2_state["rt_ms"],
                    ),
                    "attempt_duration_ms": (
                        max(available_rts) if available_rts else None
                    ),
                    "responses_matched": 0,
                    "attempt_completed": 0,
                    "timed_out": 1,
                }
            )
            break

        attempt_duration_ms = max(
            p1_state["rt_ms"],
            p2_state["rt_ms"],
        )
        attempt_end_time = (
            attempt_onset_holder["time"] + (attempt_duration_ms / 1000.0)
        )
        attempt_matched = p1_state["choice"] == p2_state["choice"]
        attempt_first_responder = identify_first_responder(
            p1_state["rt_ms"],
            p2_state["rt_ms"],
        )

        if first_joint_responder_overall is None:
            first_joint_responder_overall = attempt_first_responder

        attempt_record = {
            "attempt": attempt_count,
            "attempt_onset_session_elapsed_s": rounded(
                attempt_onset_holder["time"]
            ),
            "p1_choice": p1_state["choice"],
            "p1_rt_ms": p1_state["rt_ms"],
            "p2_choice": p2_state["choice"],
            "p2_rt_ms": p2_state["rt_ms"],
            "first_responder": attempt_first_responder,
            "attempt_duration_ms": attempt_duration_ms,
            "responses_matched": int(attempt_matched),
            "attempt_completed": 1,
            "timed_out": 0,
        }
        attempts.append(attempt_record)

        if attempt_matched:
            consensus_reached = True
            final_choice = p1_state["choice"]
            record["Consensus_Reached_Session_Elapsed_s"] = rounded(
                attempt_end_time
            )
            record["Total_Consensus_Duration_ms"] = seconds_to_ms(
                attempt_end_time - first_joint_prompt_onset
            )
            record["First_Joint_Responder_Overall"] = (
                first_joint_responder_overall
            )
            record["First_Consensus_Responder"] = attempt_first_responder
            record["Final_Joint_P1_Choice"] = p1_state["choice"]
            record["Final_Joint_P1_RT_ms"] = p1_state["rt_ms"]
            record["Final_Joint_P2_Choice"] = p2_state["choice"]
            record["Final_Joint_P2_RT_ms"] = p2_state["rt_ms"]
            record["Final_Joint_Choice"] = final_choice
        else:
            failed_attempts.append(dict(attempt_record))
            warning_end_session_s = min(
                session_clock.getTime() + CONSENSUS_WARNING_DURATION_S,
                joint_deadline_session_s,
            )

            while session_clock.getTime() < warning_end_session_s:
                seconds_remaining = max(
                    0.0,
                    joint_deadline_session_s - session_clock.getTime(),
                )
                if (
                    seconds_remaining <= JOINT_COUNTDOWN_START_S
                    and record[
                        "Joint_Countdown_First_Shown_Session_Elapsed_s"
                    ] is None
                ):
                    record[
                        "Joint_Countdown_First_Shown_Session_Elapsed_s"
                    ] = rounded(session_clock.getTime())

                draw_consensus_warning(trial_number, seconds_remaining)
                win.flip()

                escape_events = kb.getKeys(
                    keyList=[ESCAPE_KEY],
                    waitRelease=False,
                    clear=True,
                )
                if escape_events:
                    raise KeyboardInterrupt

            if session_clock.getTime() >= joint_deadline_session_s:
                consensus_timed_out = True
                record["Consensus_Timeout_Phase"] = "Consensus_Warning"

        record["Consensus_Attempts"] = attempt_count
        record["Failed_Consensus_Attempts"] = len(failed_attempts)
        record["Joint_Attempts_Detail_JSON"] = json.dumps(
            attempts,
            ensure_ascii=False,
        )
        record["Failed_Joint_Attempts_JSON"] = json.dumps(
            failed_attempts,
            ensure_ascii=False,
        )
        checkpoint()

    record["Consensus_Attempts"] = attempt_count
    record["Failed_Consensus_Attempts"] = len(failed_attempts)
    record["Joint_Attempts_Detail_JSON"] = json.dumps(
        attempts,
        ensure_ascii=False,
    )
    record["Failed_Joint_Attempts_JSON"] = json.dumps(
        failed_attempts,
        ensure_ascii=False,
    )

    if consensus_timed_out:
        record["Consensus_Time_Limit_Exceeded"] = 1
        record["Consensus_Timeout_Session_Elapsed_s"] = rounded(
            joint_deadline_session_s
        )
        if first_joint_prompt_onset is not None:
            record["Total_Consensus_Duration_ms"] = seconds_to_ms(
                joint_deadline_session_s - first_joint_prompt_onset
            )

    win.flip()

    return {
        "consensus_reached": consensus_reached,
        "consensus_timed_out": consensus_timed_out,
        "final_choice": final_choice,
    }


# =============================================================================
# 8. Session metadata and data checkpointing
# =============================================================================
session_clock = core.Clock()
trial_results = []
current_trial_record = None
current_phase = "Setup"

session_metadata = {
    "Dyad_ID": exp_info["Dyad_ID"],
    "Session_Type": "Standalone_Warmup",
    "Warmup_Version": WARMUP_VERSION,
    "Experiment_Start_ISO": session_start_iso,
    "Warmup_Image_Folder": str(WARMUP_IMAGE_DIR),
    "Warmup_Data_Folder": str(WARMUP_DATA_DIR),
    "Icons_Folder": str(ICONS_DIR),
    "No_Talk_Icon_File": NO_TALK_ICON_FILE,
    "Talk_Icon_File": TALK_ICON_FILE,
    "Phase_Icon_Size_Height_Units": PHASE_ICON_SIZE,
    "Phase_Icon_Position": json.dumps(PHASE_ICON_POSITION),
    "Instruction_Icon_Position": json.dumps(INSTRUCTION_ICON_POSITION),
    "Response_Mapping": "L-R (Left=AI, Right=Real)",
    "Left_Side_Label": LEFT_LABEL,
    "Right_Side_Label": RIGHT_LABEL,
    "PsychoPy_Version": psychopy.__version__,
    "Requested_Window_Resolution": "3840x2160",
    "Actual_Window_Size": "x".join(str(int(value)) for value in win.size),
    "Monitor_Profile": WINDOW_MONITOR,
    "Screen_Index": WINDOW_SCREEN,
    "Measured_Refresh_Rate_Hz": rounded(measured_refresh_rate_hz, 3),
    "Frame_Period_ms": seconds_to_ms(frame_period_s),
    "Pre_Individual_Prompt_Duration_ms": seconds_to_ms(
        PRE_INDIVIDUAL_PROMPT_DURATION_S
    ),
    "Fixed_Image_Presentation_Duration_ms": seconds_to_ms(
        IMAGE_PRESENTATION_DURATION_S
    ),
    "Individual_Response_Deadline_From_Image_Onset_ms": seconds_to_ms(
        INDIVIDUAL_RESPONSE_DEADLINE_S
    ),
    "Individual_Countdown_Start_ms": seconds_to_ms(
        INDIVIDUAL_COUNTDOWN_START_S
    ),
    "Joint_Response_Deadline_From_Image_Offset_ms": seconds_to_ms(
        JOINT_RESPONSE_DEADLINE_S
    ),
    "Joint_Countdown_Start_ms": seconds_to_ms(
        JOINT_COUNTDOWN_START_S
    ),
    "Trigger_Tones_Enabled": 0,
    "Include_In_Main_Analysis": 0,
}


def checkpoint():
    """Save completed warm-up trials plus the current partial trial."""
    if current_trial_record is not None:
        current_trial_record["Last_Checkpoint_ISO"] = iso_now()

    atomic_write_csv(
        completed_rows=trial_results,
        filename=data_filename,
        current_row=current_trial_record,
    )


# =============================================================================
# 9. Guided-then-natural warm-up procedure
# =============================================================================
instruction_pages = [
    (
        (
            "Welcome to the warm-up.\n\n"
            "You will practise deciding whether an image is AI-generated or real. "
            "These practice images are not faces.\n\n"
            "The first two trials include step-by-step guidance. The last four "
            "trials run at normal speed.\n\n"
            "Press SPACE to continue."
        ),
        None,
    ),
    (
        (
            "Use your own macropad to answer:\n\n"
            "AI = LEFT\n"
            "Real = RIGHT\n\n"
            "Press SPACE to continue."
        ),
        None,
    ),
    (
        (
            "This symbol means no talking.\n\n"
            "Decide on your own. Do not share your answer.\n\n"
            "The image stays on screen for 3 seconds. Both of you must answer "
            "within 15 seconds.\n\n"
            "Press SPACE to continue."
        ),
        no_talk_icon,
    ),
    (
        (
            "This symbol means you may talk.\n\n"
            "Discuss the image and agree on one answer. Try not to talk over "
            "each other.\n\n"
            "Then both enter the same answer on your own macropads.\n\n"
            "Press SPACE to start the macropad check."
        ),
        talk_icon,
    ),
]

try:
    session_clock.reset()

    current_phase = "Instructions"
    for page_text, page_icon in instruction_pages:
        show_text_and_wait_for_space(page_text, icon=page_icon)

    current_phase = "Macropad_Check_Participant_1"
    run_macropad_check(
        participant_number=1,
        left_key=P1_KEYS[0],
        right_key=P1_KEYS[1],
    )

    current_phase = "Macropad_Check_Participant_2"
    run_macropad_check(
        participant_number=2,
        left_key=P2_KEYS[0],
        right_key=P2_KEYS[1],
    )

    current_phase = "Macropad_Check_Complete"
    show_ready_check_animation()

    current_phase = "Ready_To_Begin_Warmup"
    wait_for_both_left_buttons_to_start()

    kb.clearEvents()
    event.clearEvents(eventType="keyboard")

    total_trials = len(WARMUP_STIMULI)

    for index, stimulus in enumerate(WARMUP_STIMULI):
        trial_number = index + 1
        filename = stimulus["filename"]
        authenticity = stimulus["authenticity"]
        image_path = WARMUP_IMAGE_DIR / filename
        stim_image.image = str(image_path)

        guided_mode = trial_number <= FULLY_GUIDED_TRIALS
        trial_dropped_frames_start = int(win.nDroppedFrames)
        current_phase = "Warmup_Trial_Initialization"

        current_trial_record = dict(session_metadata)
        current_trial_record.update(
            {
                "Warmup_Trial": trial_number,
                "Warmup_Trial_Order_Position": trial_number,
                "Total_Warmup_Trials": total_trials,
                "Warmup_Flow_Mode": (
                    "Guided_Main_Task_Mechanics"
                    if guided_mode
                    else "Natural_Main_Task_Style"
                ),
                "Filename": filename,
                "Authenticity": authenticity,
                "Trial_Start_ISO": iso_now(),
                "Trial_Start_Session_Elapsed_s": rounded(
                    session_clock.getTime()
                ),
                "Pre_Individual_Prompt_Onset_Session_Elapsed_s": None,
                "Pre_Individual_Prompt_Offset_Session_Elapsed_s": None,
                "Pre_Individual_Prompt_Actual_Duration_ms": None,
                "Fixation_Onset_Session_Elapsed_s": None,
                "Fixation_Offset_Session_Elapsed_s": None,
                "Fixation_Sampled_Duration_ms": None,
                "Fixation_Planned_Duration_ms": None,
                "Fixation_Actual_Duration_ms": None,
                "Fixation_Frame_Count": None,
                "Stimulus_Onset_Session_Elapsed_s": None,
                "Image_Offset_Session_Elapsed_s": None,
                "Image_Visible_Duration_ms": None,
                "Both_Responded_While_Image_Visible": None,
                "Individual_Response_Deadline_Session_Elapsed_s": None,
                "Individual_Countdown_First_Shown_Session_Elapsed_s": None,
                "Individual_Phase_End_Session_Elapsed_s": None,
                "Individual_Phase_Duration_ms": None,
                "Individual_Responses_Complete": None,
                "Individual_Responses_Complete_Session_Elapsed_s": None,
                "Individual_Missing_Response_Count": None,
                "P1_Indiv_Choice": None,
                "P1_Indiv_Correct": None,
                "P1_Indiv_RT_ms": None,
                "P1_Indiv_Response_During_Image": None,
                "P1_Indiv_Response_Session_Elapsed_s": None,
                "P1_Indiv_Response_Within_Deadline": None,
                "P1_Indiv_Timed_Out": None,
                "P2_Indiv_Choice": None,
                "P2_Indiv_Correct": None,
                "P2_Indiv_RT_ms": None,
                "P2_Indiv_Response_During_Image": None,
                "P2_Indiv_Response_Session_Elapsed_s": None,
                "P2_Indiv_Response_Within_Deadline": None,
                "P2_Indiv_Timed_Out": None,
                "First_Individual_Responder": None,
                "Individual_RT_Difference_ms": None,
                "Initial_Agreement": None,
                "Initial_Agreement_Choice": None,
                "Discussion_Allowed_Onset_Session_Elapsed_s": None,
                "Joint_Deadline_Start_Session_Elapsed_s": None,
                "Joint_Deadline_Session_Elapsed_s": None,
                "Joint_Prompt_First_Onset_Session_Elapsed_s": None,
                "Joint_Prompt_Delay_After_Image_Offset_ms": None,
                "Joint_Countdown_First_Shown_Session_Elapsed_s": None,
                "Consensus_Reached_Session_Elapsed_s": None,
                "Total_Consensus_Duration_ms": None,
                "Consensus_Time_Limit_Exceeded": 0,
                "Consensus_Timeout_Session_Elapsed_s": None,
                "Consensus_Timeout_Phase": None,
                "Consensus_Attempts": 0,
                "Failed_Consensus_Attempts": 0,
                "Failed_Joint_Attempts_JSON": "[]",
                "Joint_Attempts_Detail_JSON": "[]",
                "First_Joint_Responder_Overall": None,
                "First_Consensus_Responder": None,
                "Final_Joint_P1_Choice": None,
                "Final_Joint_P1_RT_ms": None,
                "Final_Joint_P2_Choice": None,
                "Final_Joint_P2_RT_ms": None,
                "Final_Joint_Choice": None,
                "Joint_Correct": None,
                "Dropped_Frames_Before_Trial": trial_dropped_frames_start,
                "Dropped_Frames_Trial": None,
                "Dropped_Frames_Cumulative": None,
                "Trial_Status": "Initialized",
                "Trial_Completed": 0,
                "Warmup_Aborted_During_Trial": 0,
                "Abort_Phase": None,
                "Abort_Reason": None,
                "Session_Ended_Early": 0,
                "Trial_End_ISO": None,
                "Trial_End_Session_Elapsed_s": None,
                "Last_Checkpoint_ISO": None,
            }
        )
        checkpoint()

        if guided_mode:
            current_phase = "Guided_Trial_Explanation"
            current_trial_record["Trial_Status"] = current_phase
            show_text_and_wait_for_space(
                f"Practice trial {trial_number} of {total_trials}.\n\n"
                "First, you will see the no-talking symbol and a red reminder. "
                "Then look at the fixation cross.\n\n"
                "When the image appears, answer on your own macropad. Do not talk.\n\n"
                "Press SPACE to begin.",
                icon=no_talk_icon,
            )

        current_phase = "Pre_Individual_No_Talk_Prompt"
        current_trial_record["Trial_Status"] = current_phase
        pre_prompt_holder = present_pre_individual_prompt(
            trial_number,
            current_trial_record,
        )

        current_phase = (
            "Guided_Fixation" if guided_mode else "Natural_Variable_Fixation"
        )
        current_trial_record["Trial_Status"] = current_phase
        fixation_holder = present_fixation(
            trial_number=trial_number,
            record=current_trial_record,
            guided_mode=guided_mode,
            pre_prompt_holder=pre_prompt_holder,
        )

        current_phase = "Individual_Response"
        current_trial_record["Trial_Status"] = current_phase
        individual_result = collect_individual_phase(
            trial_number=trial_number,
            record=current_trial_record,
            fixation_holder=fixation_holder,
        )

        if individual_result["complete"]:
            p1_choice = individual_result["p1_choice"]
            p2_choice = individual_result["p2_choice"]
            p1_rt_ms = individual_result["p1_rt_ms"]
            p2_rt_ms = individual_result["p2_rt_ms"]

            current_trial_record["P1_Indiv_Correct"] = int(
                p1_choice == authenticity
            )
            current_trial_record["P2_Indiv_Correct"] = int(
                p2_choice == authenticity
            )
            current_trial_record["First_Individual_Responder"] = (
                identify_first_responder(p1_rt_ms, p2_rt_ms)
            )
            current_trial_record["Individual_RT_Difference_ms"] = round(
                abs(p1_rt_ms - p2_rt_ms),
                3,
            )
            initial_agreement = int(p1_choice == p2_choice)
            current_trial_record["Initial_Agreement"] = initial_agreement
            current_trial_record["Initial_Agreement_Choice"] = (
                p1_choice if initial_agreement else None
            )

            current_phase = "Joint_Consensus"
            current_trial_record["Trial_Status"] = current_phase
            consensus_result = collect_consensus_phase(
                trial_number=trial_number,
                record=current_trial_record,
                image_offset_time=individual_result["image_offset_time"],
                guided_mode=guided_mode,
            )

            final_joint_choice = consensus_result["final_choice"]
            current_trial_record["Final_Joint_Choice"] = final_joint_choice
            current_trial_record["Joint_Correct"] = (
                int(final_joint_choice == authenticity)
                if final_joint_choice is not None
                else None
            )

            if consensus_result["consensus_reached"]:
                final_status = "Completed"
            else:
                final_status = "Completed_Consensus_Timeout"

        else:
            current_phase = "Individual_Response_Timeout"
            current_trial_record["Trial_Status"] = current_phase
            consensus_result = {
                "consensus_reached": False,
                "consensus_timed_out": False,
                "final_choice": None,
            }
            final_joint_choice = None
            final_status = "Completed_Individual_Timeout"
            win.flip()

        dropped_frames_cumulative = int(win.nDroppedFrames)
        current_trial_record["Dropped_Frames_Trial"] = (
            dropped_frames_cumulative - trial_dropped_frames_start
        )
        current_trial_record[
            "Dropped_Frames_Cumulative"
        ] = dropped_frames_cumulative

        current_trial_record["Trial_Status"] = final_status
        current_trial_record["Trial_Completed"] = 1
        current_trial_record["Trial_End_ISO"] = iso_now()
        current_trial_record["Trial_End_Session_Elapsed_s"] = rounded(
            session_clock.getTime()
        )

        trial_results.append(dict(current_trial_record))
        current_trial_record = None
        atomic_write_csv(
            completed_rows=trial_results,
            filename=data_filename,
            current_row=None,
        )

        if guided_mode:
            if not individual_result["complete"]:
                feedback_message = (
                    "Time is up.\n\n"
                    "Both of you must answer before discussion can begin."
                )
            elif consensus_result["consensus_reached"]:
                result_sentence = (
                    "Your joint answer was correct."
                    if final_joint_choice == authenticity
                    else "Your joint answer was not correct."
                )
                feedback_message = (
                    "Consensus reached.\n\n"
                    f"Correct answer: {authenticity}\n"
                    f"Your joint answer: {final_joint_choice}\n\n"
                    f"{result_sentence}"
                )
            else:
                feedback_message = (
                    "Time is up.\n\n"
                    "You did not enter the same joint answer."
                )

            continuation_text = (
                "\n\nPress SPACE to begin the normal-speed practice trials."
                if trial_number == FULLY_GUIDED_TRIALS
                else "\n\nPress SPACE to continue."
            )
            show_feedback_and_wait_for_space(
                feedback_message + continuation_text
            )

    current_phase = "Warmup_Complete"
    show_text_and_wait_for_space(
        "Warm-up complete.\n\n"
        "Please ask the experimenter if anything is unclear.\n\n"
        "Press SPACE to close the warm-up."
    )

except KeyboardInterrupt:
    print("Warm-up aborted by the user via the Escape key.")

    for completed_row in trial_results:
        completed_row["Session_Ended_Early"] = 1

    if current_trial_record is not None:
        current_trial_record["Warmup_Aborted_During_Trial"] = 1
        current_trial_record["Session_Ended_Early"] = 1
        current_trial_record["Abort_Phase"] = current_phase
        current_trial_record["Abort_Reason"] = "Escape_Key"
        current_trial_record["Trial_Status"] = "Aborted"
        current_trial_record["Trial_Completed"] = 0
        current_trial_record["Trial_End_ISO"] = iso_now()
        current_trial_record["Trial_End_Session_Elapsed_s"] = rounded(
            session_clock.getTime()
        )

except Exception as exc:
    print(f"Warm-up stopped because of an unexpected error: {exc}")

    for completed_row in trial_results:
        completed_row["Session_Ended_Early"] = 1

    if current_trial_record is not None:
        current_trial_record["Warmup_Aborted_During_Trial"] = 1
        current_trial_record["Session_Ended_Early"] = 1
        current_trial_record["Abort_Phase"] = current_phase
        current_trial_record["Abort_Reason"] = repr(exc)
        current_trial_record["Trial_Status"] = "Error"
        current_trial_record["Trial_Completed"] = 0
        current_trial_record["Trial_End_ISO"] = iso_now()
        current_trial_record["Trial_End_Session_Elapsed_s"] = rounded(
            session_clock.getTime()
        )

    raise

finally:
    if trial_results or current_trial_record is not None:
        atomic_write_csv(
            completed_rows=trial_results,
            filename=data_filename,
            current_row=current_trial_record,
        )
        print(f"Warm-up data saved to: {data_filename}")

    win.close()
    core.quit()
