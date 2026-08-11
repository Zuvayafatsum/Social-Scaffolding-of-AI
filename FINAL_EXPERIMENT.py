# Dyadic face-classification experiment — version 3.16.1
# Stable v3.14 baseline simplified for direct transparent PNG phase icons
# (Picture1.png and Picture2.png) in the icons_for_experiment folder
import psychopy
from psychopy import prefs

# Prefer the low-latency PTB backend for trigger playback, while retaining
# fallback backends so the experiment can still start on systems without PTB.
# This must be set before importing psychopy.sound.
prefs.hardware["audioLib"] = ["ptb", "sounddevice", "pygame"]

# Ignore any device name saved in PsychoPy's global preferences (for example,
# "External Headphones"). Use the output device currently selected as the
# computer's default when this script starts. This must be set before importing
# psychopy.sound.
prefs.hardware["audioDevice"] = "default"

from psychopy import visual, core, event, gui, data, sound
from psychopy.hardware import keyboard

import json
import math
import os
import secrets
from datetime import datetime

import numpy as np
import pandas as pd


# =============================================================================
# Configuration
# =============================================================================
IMAGE_FOLDER = "Pilot_selected_images"
STIMULI_CSV = "pilot_stimuli_48_final.csv"
DATA_DIR = "data_dyad_experiment"
TRIGGER_FOLDER = "psychopy_trigger_tones"
ICONS_FOLDER = "icons_for_experiment"
NO_TALK_ICON_FILE = "Picture1.png"
TALK_ICON_FILE = "Picture2.png"
PHASE_ICON_SIZE = 0.1  # Shared size for both phase icons in height units
INSTRUCTION_ICON_POSITION = (0, 0.28)
INSTRUCTION_TEXT_POSITION_WITH_ICON = (0, -0.07)

MACROPAD_CHECK_TIMEOUT_S = 5.0

# Main-task timing. The individual deadline is measured from image onset.
# The joint deadline remains measured from the fixed image-offset flip.
PRE_INDIVIDUAL_PROMPT_DURATION_S = 2.0
IMAGE_PRESENTATION_DURATION_S = 3.0
INDIVIDUAL_RESPONSE_DEADLINE_S = 15.0
INDIVIDUAL_COUNTDOWN_START_S = 5.0
JOINT_RESPONSE_DEADLINE_S = 60.0
JOINT_COUNTDOWN_START_S = 10.0

# Three attention/catch trials are inserted after normal trials 16, 32, and 48.
# They do not alter the ordering or numbering of the 48 face trials.
CATCH_AFTER_NORMAL_TRIALS = (16, 32, 48)
CATCH_WAIT_BEFORE_COUNTDOWN_S = 2.0
CATCH_COUNTDOWN_DURATION_S = 5.0
CATCH_RESPONSE_DEADLINE_S = 10.0
CATCH_RESPONSE_COUNTDOWN_START_S = 5.0
CATCH_EXPECTED_RESPONSE = "LEFT"

# Exact requested prompt colors in standard 0-255 RGB coordinates.
PROMPT_RED_RGB255 = [136, 8, 8]    # Hex #880808
PROMPT_GREEN_RGB255 = [16, 66, 7]  # Hex #104207

# WAV trigger files. The folder should sit beside the experiment script, in the
# same project directory as the stimulus CSV and image folder.
TRIGGER_VOLUME = 1.0
TRIGGER_FILES = {
    "experiment_start": "01_experiment_start.wav",
    "image_onset": "02_image_onset.wav",
    "first_individual_response": "03_first_individual_response.wav",
    "second_individual_response": "04_second_individual_response.wav",
    "first_consensus_press": "05_first_consensus_press.wav",
    "second_consensus_fail": "06_second_consensus_fail.wav",
    "consensus_reached": "07_consensus_reached.wav",
    "experiment_end": "08_experiment_end.wav",
}
TRIGGER_CODES = {
    "experiment_start": 1,
    "image_onset": 2,
    "first_individual_response": 3,
    "second_individual_response": 4,
    "first_consensus_press": 5,
    "second_consensus_fail": 6,
    "consensus_reached": 7,
    "experiment_end": 8,
}

# Pre-stimulus timing: the fixation remains continuously visible for a
# uniformly sampled total duration from 500 to 900 ms. The image appears on
# the immediately following display refresh, with no blank offset interval.
FIXATION_BASE_DURATION_S = 0.500
FIXATION_JITTER_MIN_S = 0.000
FIXATION_JITTER_MAX_S = 0.400

# Participant 1 macropad: left/right
P1_KEYS = ["a", "d"]

# Participant 2 macropad: left/right
P2_KEYS = ["h", "k"]

ESCAPE_KEY = "escape"
ALL_RESPONSE_KEYS = P1_KEYS + P2_KEYS + [ESCAPE_KEY]


# =============================================================================
# Utility functions
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


def identify_first_responder(p1_rt_ms, p2_rt_ms):
    """Identify which participant responded first from two RT values."""
    if p1_rt_ms is None or p2_rt_ms is None:
        return None
    if p1_rt_ms < p2_rt_ms:
        return "Participant_1"
    if p2_rt_ms < p1_rt_ms:
        return "Participant_2"
    return "Simultaneous"


def response_side_from_key(key_name):
    """Return the physical LEFT/RIGHT side generated by either macropad."""
    if key_name in [P1_KEYS[0], P2_KEYS[0]]:
        return "LEFT"
    if key_name in [P1_KEYS[1], P2_KEYS[1]]:
        return "RIGHT"
    return None


def response_rt_ms(key_event, onset_holder=None):
    """
    Return the PsychoPy key-down RT in milliseconds.

    This preserves the response method used by the working v2 script:
    KeyPress.rt from one shared PsychoPy Keyboard listener. The keyboard clock
    is reset on the same display flip that presents the response screen.
    """
    rt_s = getattr(key_event, "rt", None)
    if rt_s is None:
        raise RuntimeError("A keyboard event was returned without an RT value.")

    rt_s = float(rt_s)
    if rt_s < -0.002 or rt_s >= 3600.0:
        raise RuntimeError(f"Invalid keyboard RT returned: {rt_s!r} seconds")

    return round(max(0.0, rt_s) * 1000.0, 3)


def atomic_write_csv(completed_rows, filename, current_row=None):
    """
    Atomically overwrite the checkpoint CSV.

    The CSV contains all completed trials and, when present, the current
    incomplete trial as its final row. Writing to a temporary file first
    reduces the chance of leaving a corrupted CSV if writing is interrupted.
    """
    rows_to_write = [dict(row) for row in completed_rows]

    if current_row is not None:
        rows_to_write.append(dict(current_row))

    if not rows_to_write:
        return

    temp_filename = filename + ".tmp"
    pd.DataFrame(rows_to_write).to_csv(temp_filename, index=False)
    os.replace(temp_filename, filename)


def atomic_write_trigger_log(trigger_rows, filename):
    """Atomically write the event-trigger log without delaying timed phases."""
    if not trigger_rows:
        return

    temp_filename = filename + ".tmp"
    pd.DataFrame([dict(row) for row in trigger_rows]).to_csv(
        temp_filename,
        index=False,
    )
    os.replace(temp_filename, filename)


def get_stimulus_id(row):
    """
    Use an existing stable stimulus-ID column when available.
    Otherwise, use the filename as the stable stimulus identifier.
    """
    candidate_columns = [
        "Stimulus_ID",
        "StimulusID",
        "Image_ID",
        "ImageID",
        "ID",
    ]

    for column in candidate_columns:
        if column in row.index and pd.notna(row[column]):
            return row[column]

    return row["Filename"]


# =============================================================================
# 1. Setup and participant information
# =============================================================================
exp_info = {
    "Dyad_ID": "",
    "Participant_1_ID": "",
    "Participant_1_Age": "",
    "Participant_1_Gender": ["Female", "Male", "Non-binary"],
    "Participant_1_Ethnicity": ["White", "Black", "East Asian", "South Asian"],
    "Participant_2_ID": "",
    "Participant_2_Age": "",
    "Participant_2_Gender": ["Female", "Male", "Non-binary"],
    "Participant_2_Ethnicity": ["White", "Black", "East Asian", "South Asian"],
    "D_Familiarity": ["Yes", "No", "NA"],
    "Randomization": ["L-R", "R-L"],
    "Session_Type": "Collective",
}

dlg = gui.DlgFromDict(
    dictionary=exp_info,
    title="Dyadic Face Classification Experiment",
    order=[
        "Dyad_ID",
        "Participant_1_ID",
        "Participant_1_Age",
        "Participant_1_Gender",
        "Participant_1_Ethnicity",
        "Participant_2_ID",
        "Participant_2_Age",
        "Participant_2_Gender",
        "Participant_2_Ethnicity",
        "D_Familiarity",
        "Randomization",
        "Session_Type",
    ],
)

if not dlg.OK:
    core.quit()

# The selected mapping remains fixed for the whole session.
# L-R means AI on the left and Real on the right.
# R-L means Real on the left and AI on the right.
if exp_info["Randomization"] == "L-R":
    left_txt, right_txt = "AI", "Real"
elif exp_info["Randomization"] == "R-L":
    left_txt, right_txt = "Real", "AI"
else:
    raise ValueError(
        "Randomization must be either L-R or R-L. "
        f"Received: {exp_info['Randomization']!r}"
    )

ai_button_side = "LEFT" if left_txt == "AI" else "RIGHT"
real_button_side = "LEFT" if left_txt == "Real" else "RIGHT"
response_mapping_description = (
    f"{exp_info['Randomization']} "
    f"(Left={left_txt}, Right={right_txt})"
)

# Generate and save one seed for this experimental session.
randomization_seed = secrets.randbits(32)

session_start_iso = iso_now()

os.makedirs(DATA_DIR, exist_ok=True)
session_file_stamp = data.getDateStr()
data_filename = os.path.join(
    DATA_DIR,
    f"Dyad_{exp_info['Dyad_ID']}_{session_file_stamp}.csv",
)
trigger_log_filename = os.path.join(
    DATA_DIR,
    f"Dyad_{exp_info['Dyad_ID']}_{session_file_stamp}_trigger_log.csv",
)


# =============================================================================
# 2. Load and randomize stimuli
# =============================================================================
try:
    stim_df = pd.read_csv(STIMULI_CSV)
except Exception as exc:
    print(f"Error: Could not read {STIMULI_CSV}. {exc}")
    core.quit()

required_columns = {"Filename", "Group"}
missing_columns = required_columns.difference(stim_df.columns)

if missing_columns:
    print(
        "Error: The stimulus CSV is missing required column(s): "
        + ", ".join(sorted(missing_columns))
    )
    core.quit()

# Preserve the original CSV row number before randomization.
stim_df["_Original_CSV_Row"] = range(1, len(stim_df) + 1)
stim_df["_Stimulus_ID"] = stim_df.apply(get_stimulus_id, axis=1)

# Reproducible random order based on the saved session seed.
stim_df = stim_df.sample(
    frac=1,
    random_state=randomization_seed,
).reset_index(drop=True)

# A separate reproducible generator supplies one fixation-jitter value per
# trial without changing the stimulus-order randomization.
jitter_rng = np.random.default_rng(randomization_seed ^ 0xA5A5A5A5)

# Catch trials use an independent reproducible jitter stream. This prevents the
# insertion of catch trials from changing any fixation-jitter value assigned to
# the 48 normal face trials.
catch_jitter_rng = np.random.default_rng(randomization_seed ^ 0xC3C3C3C3)


# =============================================================================
# 2b. Validate and preload WAV event triggers
# =============================================================================
trigger_paths = {
    trigger_name: os.path.join(TRIGGER_FOLDER, filename)
    for trigger_name, filename in TRIGGER_FILES.items()
}
missing_trigger_files = [
    path for path in trigger_paths.values() if not os.path.isfile(path)
]
if missing_trigger_files:
    print("Error: The following trigger WAV file(s) could not be found:")
    for missing_path in missing_trigger_files:
        print(f"  - {missing_path}")
    print(
        "Place the psychopy_trigger_tones folder in the experiment project "
        "directory and run the script again."
    )
    core.quit()

try:
    trigger_sounds = {
        trigger_name: sound.Sound(
            value=path,
            volume=TRIGGER_VOLUME,
            name=f"Trigger_{TRIGGER_CODES[trigger_name]:02d}_{trigger_name}",
            autoLog=False,
        )
        for trigger_name, path in trigger_paths.items()
    }
except Exception as exc:
    print(f"Error: Could not initialize the trigger WAV files. {exc}")
    core.quit()


# =============================================================================
# 3. Hardware initialization
# =============================================================================
# One keyboard listener receives the distinct key codes generated by both
# macropads. Participant identity is inferred from the assigned key set.
kb = keyboard.Keyboard()


# =============================================================================
# 4. Window and visual elements
# =============================================================================
win = visual.Window(
    size=[3840, 2160],
    fullscr=True,
    monitor="testMonitor",
    units="height",
    screen=0,
    color=[0.9, 0.9, 0.9],
    useRetina=True,
)

text_color = [-1, -1, -1]
font_type = "Arial"
COMMON_TEXT_HEIGHT = 0.035
LABEL_TEXT_HEIGHT = 0.045

# Shared screen position for the fixation cross and the face-image center.
IMAGE_POSITION = (0, 0.06)
FIXATION_CROSS_HEIGHT = 0.100

# Measure the actual display refresh rate so fixation timing is controlled by
# display refreshes rather than by an imprecise sleep call.
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
pre_individual_prompt_frame_count = max(
    1,
    int(round(PRE_INDIVIDUAL_PROMPT_DURATION_S / frame_period_s)),
)
pre_individual_prompt_planned_s = (
    pre_individual_prompt_frame_count * frame_period_s
)
base_fixation_frame_count = max(
    1,
    int(round(FIXATION_BASE_DURATION_S / frame_period_s)),
)
image_presentation_frame_count = max(
    1,
    int(round(IMAGE_PRESENTATION_DURATION_S / frame_period_s)),
)
image_presentation_planned_s = (
    image_presentation_frame_count * frame_period_s
)
catch_wait_frame_count = max(
    1,
    int(round(CATCH_WAIT_BEFORE_COUNTDOWN_S / frame_period_s)),
)
catch_countdown_frame_count = max(
    1,
    int(round(CATCH_COUNTDOWN_DURATION_S / frame_period_s)),
)
catch_wait_planned_s = catch_wait_frame_count * frame_period_s
catch_countdown_planned_s = catch_countdown_frame_count * frame_period_s

# Record frame intervals after refresh-rate estimation. A frame is counted as
# dropped when it exceeds the expected refresh interval by more than 4 ms.
win.refreshThreshold = frame_period_s + 0.004
win.recordFrameIntervals = True

# Black fixation cross shown alone before every face image.
fixation_cross = visual.TextStim(
    win,
    text="+",
    pos=IMAGE_POSITION,
    height=FIXATION_CROSS_HEIGHT,
    color=text_color,
    font=font_type,
    bold=False,
)

# Visual identity cues matching the physical macropad LED colors.
P1_BADGE_COLOR = [-0.8, -0.2, 1.0]   # Blue
P2_BADGE_COLOR = [1.0, -0.45, -0.45] # Red
READY_GREEN = [-0.45, 0.75, -0.45]

# The image and response options are shown together during the individual phase.
stim_image = visual.ImageStim(
    win,
    size=(0.55, 0.55),
    pos=IMAGE_POSITION,
)

no_talk_icon_path = os.path.join(ICONS_FOLDER, NO_TALK_ICON_FILE)
talk_icon_path = os.path.join(ICONS_FOLDER, TALK_ICON_FILE)

if not os.path.isfile(no_talk_icon_path):
    print(f"Error: Could not find the no-talk phase icon: {no_talk_icon_path}")
    win.close()
    core.quit()

if not os.path.isfile(talk_icon_path):
    print(f"Error: Could not find the talk-permitted phase icon: {talk_icon_path}")
    win.close()
    core.quit()

phase_icon_position = (0, 0.42)
no_talk_icon = visual.ImageStim(
    win,
    image=no_talk_icon_path,
    pos=phase_icon_position,
    size=(PHASE_ICON_SIZE, PHASE_ICON_SIZE),
)

talk_icon = visual.ImageStim(
    win,
    image=talk_icon_path,
    pos=phase_icon_position,
    size=(PHASE_ICON_SIZE, PHASE_ICON_SIZE),
)

instr_text = visual.TextStim(
    win,
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    wrapWidth=1.2,
    font=font_type,
)

indiv_prompt = visual.TextStim(
    win,
    text="No talking. Individual answers",
    pos=(0, 0),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_RED_RGB255,
    colorSpace="rgb255",
    font=font_type,
    bold=True,
    wrapWidth=1.4,
)

joint_prompt = visual.TextStim(
    win,
    text="You can talk now. Try to reach a consensus decision",
    pos=(0, 0.29),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_GREEN_RGB255,
    colorSpace="rgb255",
    font=font_type,
    bold=True,
    wrapWidth=1.4,
)

warning_text = visual.TextStim(
    win,
    text=(
        "Your joint answers did not match.\n"
        "Discuss again. Then both of you must enter the same answer again."
    ),
    pos=(0, 0.29),
    height=COMMON_TEXT_HEIGHT,
    color=PROMPT_RED_RGB255,
    colorSpace="rgb255",
    font=font_type,
    bold=True,
    wrapWidth=1.4,
)

individual_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0.31),
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
    bold=True,
    wrapWidth=1.4,
)

joint_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0.18),
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
    bold=True,
    wrapWidth=1.4,
)

# Catch-trial text is deliberately separated vertically from the countdown so
# that the two elements cannot overlap. No AI/Real labels or arrows are drawn
# during catch-trial instruction, countdown, or response screens.
catch_wait_text = visual.TextStim(
    win,
    text=(
        "If you read this sentence, please wait.\n\n"
        "Do not respond. Wait until the countdown is finished."
    ),
    pos=(0, 0.08),
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
    bold=False,
    wrapWidth=1.2,
)

catch_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, -0.10),
    height=0.070,
    color=text_color,
    font=font_type,
    bold=True,
    wrapWidth=0.5,
)

catch_response_prompt = visual.TextStim(
    win,
    text="Now you can press the LEFT button.",
    pos=(0, 0),
    height=0.045,
    color=text_color,
    font=font_type,
    bold=True,
    wrapWidth=1.2,
)

catch_response_countdown_text = visual.TextStim(
    win,
    text="",
    pos=(0, 0),
    height=0.045,
    color=text_color,
    font=font_type,
    bold=True,
    wrapWidth=1.2,
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
    fillColor=text_color,
    lineColor=text_color,
    pos=(-0.15, -0.31),
)

arrow_right = visual.ShapeStim(
    win,
    vertices=arrow_vertices,
    ori=0,
    fillColor=text_color,
    lineColor=text_color,
    pos=(0.15, -0.31),
)

label_left = visual.TextStim(
    win,
    text="",
    pos=(-0.38, -0.31),
    height=LABEL_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
)

label_right = visual.TextStim(
    win,
    text="",
    pos=(0.38, -0.31),
    height=LABEL_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
)

label_left.text = left_txt
label_right.text = right_txt

bar_width = 0.8
bar_height = 0.02
bar_pos_y = -0.47

progress_outline = visual.Rect(
    win,
    width=bar_width,
    height=bar_height,
    pos=(0, bar_pos_y),
    lineColor=text_color,
    fillColor=[0.9, 0.9, 0.9],
    lineWidth=2,
)

progress_fill = visual.Rect(
    win,
    width=0,
    height=bar_height,
    pos=(-bar_width / 2, bar_pos_y),
    fillColor=[-1,-1,-1],
    lineColor=None,
)

thanks_text = visual.TextStim(
    win,
    text="This is the end of the experiment. Thank you for participating!",
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
    wrapWidth=1.2,
)

# Macropad-check visual elements. These are used only during the startup check
# and do not alter the experimental task screens or response logic.
check_prompt_text = visual.TextStim(
    win,
    text="",
    pos=(0, -0.03),
    height=COMMON_TEXT_HEIGHT,
    color=text_color,
    font=font_type,
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
    font=font_type,
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
    font=font_type,
    bold=True,
)

p2_ready_number = visual.TextStim(
    win,
    text="2",
    pos=(0.09, 0.15),
    height=0.043,
    color=[1, 1, 1],
    font=font_type,
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


def draw_progress(current_trial, total_trials):
    """Draw progress based on the number of previously completed trials."""
    fill_width = (current_trial / total_trials) * bar_width
    progress_fill.width = fill_width
    progress_fill.pos = ((-bar_width / 2) + (fill_width / 2), bar_pos_y)
    progress_outline.draw()
    progress_fill.draw()


def draw_pre_individual_prompt(completed_trials, total_trials):
    """Draw the no-talk icon together with the two-second red prompt."""
    no_talk_icon.draw()
    indiv_prompt.draw()
    draw_progress(completed_trials, total_trials)


def draw_individual_image_interface(completed_trials, total_trials):
    """Draw the face, the no-talk icon, and the response labels."""
    stim_image.draw()
    no_talk_icon.draw()
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()
    draw_progress(completed_trials, total_trials)


def draw_response_labels_only(completed_trials, total_trials):
    """Draw the no-talk icon and the response labels during the individual phase."""
    no_talk_icon.draw()
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()
    draw_progress(completed_trials, total_trials)


def draw_individual_countdown(seconds_remaining):
    """Draw the individual deadline only during its final five seconds."""
    if 0.0 < seconds_remaining <= INDIVIDUAL_COUNTDOWN_START_S:
        displayed_seconds = max(1, int(math.ceil(seconds_remaining)))
        second_word = "second" if displayed_seconds == 1 else "seconds"
        individual_countdown_text.text = (
            f"{displayed_seconds} {second_word} left to provide your individual answers."
        )
        individual_countdown_text.draw()


def draw_individual_wait_interface(
    completed_trials,
    total_trials,
    seconds_remaining,
):
    """Draw the no-talk icon and labels after image offset while waiting for missing responses."""
    no_talk_icon.draw()
    draw_individual_countdown(seconds_remaining)
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()
    draw_progress(completed_trials, total_trials)


def draw_joint_countdown(seconds_remaining):
    """Draw the concealed joint deadline only during its final 10 seconds."""
    if 0.0 < seconds_remaining <= JOINT_COUNTDOWN_START_S:
        displayed_seconds = max(1, int(math.ceil(seconds_remaining)))
        second_word = "second" if displayed_seconds == 1 else "seconds"
        joint_countdown_text.text = (
            f"{displayed_seconds} {second_word} left to provide a joint answer."
        )
        joint_countdown_text.draw()


def draw_joint_interface(
    prompt,
    completed_trials,
    total_trials,
    seconds_remaining,
):
    """Draw the talk-permitted icon and a joint-response screen."""
    talk_icon.draw()
    prompt.draw()
    draw_joint_countdown(seconds_remaining)
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()
    draw_progress(completed_trials, total_trials)


def draw_consensus_warning(
    completed_trials,
    total_trials,
    seconds_remaining,
):
    """Show failed-consensus feedback while the joint deadline continues."""
    talk_icon.draw()
    warning_text.draw()
    draw_joint_countdown(seconds_remaining)
    arrow_left.draw()
    arrow_right.draw()
    label_left.draw()
    label_right.draw()
    draw_progress(completed_trials, total_trials)


# =============================================================================
# 5. Instructions and macropad check
# =============================================================================
instruction_pages = [
    (
        (
            "Welcome to the main experiment!\n\n"
            "In this part, you will see images of human faces. Decide whether each face is AI-generated "
            "or real.\n\n"
            "There is no deception. Some faces are indeed real humans, and some are AI generated.\n\n"
            "Please try to be as accurate and as fast as possible.\n\n"
            f"For this session:\n{left_txt} = LEFT\n{right_txt} = RIGHT\n\n"
            "Press SPACE to continue."
        ),
        None,
    ),
    (
        (
            "First, answer on your own. Do not talk.\n\n"
            f"AI: press {ai_button_side} on your macropad.\n"
            f"Real: press {real_button_side} on your macropad.\n\n"
            "The no-talking symbol appears before each face. It stays on screen "
            "while you give your individual answers.\n\n"
            "Each face is shown for 3 seconds. Both of you must answer within "
            "15 seconds. A countdown appears for the last 5 seconds.\n\n"
            "Press SPACE to continue."
        ),
        no_talk_icon,
    ),
    (
        (
            "After both individual answers, the green symbol means you may talk.\n\n"
            "Discuss the face and agree on one joint answer. The face will be gone, "
            "but the answer labels will remain.\n\n"
            "Both of you must enter the same answer on your own macropads.\n\n"
            "Please try not to talk over each other. Speak one at a time.\n\n"
            "Press SPACE to start the macropad check."
        ),
        talk_icon,
    ),
]


def show_text_and_wait_for_space(message, icon=None):
    """Show one instruction page and continue with Space; Escape aborts.

    When an icon is supplied, show it in the same instruction layout used by
    the warm-up. The task-phase icon position is restored before returning.
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
        if ESCAPE_KEY in pressed:
            raise KeyboardInterrupt
    finally:
        instr_text.pos = original_text_position
        if icon is not None:
            icon.pos = original_icon_position


def draw_participant_badge(participant_number):
    """Draw the blue Participant 1 badge or red Participant 2 badge."""
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
    """Draw both colored participant badges for the shared start screen."""
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
    """Display a macropad-check message with the requested visual cue."""
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
    """Animate a green checkmark after both macropads pass verification."""
    check_prompt_text.text = (
        "Macropad check complete.\n\n"
        "Both participants are ready."
    )

    animation_clock = core.Clock()
    animation_duration_s = 0.35

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
    """
    Verify one specified macropad button.

    The participant receives two five-second attempts. After the first timeout,
    a retry warning is shown. After the second timeout, the experiment pauses
    on an experimenter-warning screen until the expected button is detected.
    """
    button_name_upper = button_name.upper()
    prompt = (
        f"Participant {participant_number}, press the "
        f"{button_name_upper} button on your macropad."
    )

    for attempt_number in (1, 2):
        event.clearEvents(eventType="keyboard")
        show_check_message(
            prompt,
            participant_number=participant_number,
        )

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

    # Two timed attempts failed. Keep the experiment paused until the expected
    # button is detected after the experimenter has checked the connection.
    event.clearEvents(eventType="keyboard")
    show_check_message(
        "Please call the experimenter.\n\n"
        "It seems that your macropad is not connected.\n\n"
        f"After the connection has been checked, press the "
        f"{button_name_upper} button again.",
        participant_number=participant_number,
    )

    pressed = event.waitKeys(keyList=[expected_key, ESCAPE_KEY])
    if ESCAPE_KEY in pressed:
        raise KeyboardInterrupt


def run_macropad_check(participant_number, left_key, right_key):
    """Verify the left and right buttons of one participant's macropad."""
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
    """
    Start the task only after both participants press their left buttons.

    The presses may occur in either order. Each participant is counted once.
    """
    event.clearEvents(eventType="keyboard")
    show_check_message(
        "Both macropads are connected.\n\n"
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

        if ESCAPE_KEY in pressed:
            raise KeyboardInterrupt

        if P1_KEYS[0] in pressed:
            p1_left_detected = True

        if P2_KEYS[0] in pressed:
            p2_left_detected = True


# =============================================================================
# 6. Main experiment
# =============================================================================
trial_results = []
trigger_events = []
trial_trigger_events = []
current_trial_record = None
current_phase = "Instructions"
experiment_aborted = False

total_trials = len(stim_df)
session_clock = core.Clock()


def emit_trigger(
    trigger_name,
    trial_number=None,
    phase=None,
    attempt_number=None,
    participant_number=None,
    response_value=None,
    source_event_session_s=None,
):
    """
    Play one preloaded WAV marker and append a software-side trigger log row.

    For response-linked markers, source_event_session_s is the asynchronous
    keyboard key-down timestamp reconstructed from the recorded RT. The
    dispatch-delay field therefore quantifies polling/audio-call delay. The
    recorded audio channel remains the ground truth for actual acoustic onset.
    """
    if trigger_name not in trigger_sounds:
        raise KeyError(f"Unknown trigger name: {trigger_name!r}")

    dispatch_time_s = session_clock.getTime()
    trigger_sounds[trigger_name].play()

    trigger_record = {
        "Trigger_Index": len(trigger_events) + 1,
        "Trigger_Code": TRIGGER_CODES[trigger_name],
        "Trigger_Name": trigger_name,
        "Trigger_File": TRIGGER_FILES[trigger_name],
        "Trigger_Dispatch_ISO": iso_now(),
        "Trigger_Dispatch_Session_Elapsed_s": rounded(dispatch_time_s, 6),
        "Source_Event_Session_Elapsed_s": (
            rounded(source_event_session_s, 6)
            if source_event_session_s is not None
            else None
        ),
        "Dispatch_Delay_From_Source_ms": (
            seconds_to_ms(dispatch_time_s - source_event_session_s)
            if source_event_session_s is not None
            else None
        ),
        "Trial": trial_number,
        "Trial_Type": (
            current_trial_record.get("Trial_Type")
            if current_trial_record is not None
            else None
        ),
        "Normal_Trial_Number": (
            current_trial_record.get("Normal_Trial_Number")
            if current_trial_record is not None
            else None
        ),
        "Catch_Trial_Number": (
            current_trial_record.get("Catch_Trial_Number")
            if current_trial_record is not None
            else None
        ),
        "Catch_After_Normal_Trial": (
            current_trial_record.get("Catch_After_Normal_Trial")
            if current_trial_record is not None
            else None
        ),
        "Phase": phase,
        "Joint_Attempt": attempt_number,
        "Participant": participant_number,
        "Response": response_value,
    }
    trigger_events.append(trigger_record)

    if (
        current_trial_record is not None
        and trial_number is not None
        and current_trial_record.get("Trial") == trial_number
    ):
        trial_trigger_events.append(dict(trigger_record))
        current_trial_record["Trigger_Events_JSON"] = json.dumps(
            trial_trigger_events,
            ensure_ascii=False,
        )
        current_trial_record["Last_Trigger_Name"] = trigger_name
        current_trial_record[
            "Last_Trigger_Dispatch_Session_Elapsed_s"
        ] = rounded(dispatch_time_s, 6)


def emit_individual_response_trigger(
    participant_number,
    response_count,
    response_session_s,
    response_value,
    trial_number,
):
    """Emit trigger 03 for the first and trigger 04 for the second response."""
    if response_count == 1:
        trigger_name = "first_individual_response"
    elif response_count == 2:
        trigger_name = "second_individual_response"
    else:
        raise RuntimeError(
            f"Unexpected accepted individual-response count: {response_count}"
        )

    emit_trigger(
        trigger_name=trigger_name,
        trial_number=trial_number,
        phase="Individual_Response",
        participant_number=participant_number,
        response_value=response_value,
        source_event_session_s=response_session_s,
    )


def emit_catch_response_trigger(
    participant_number,
    response_count,
    response_session_s,
    response_side,
    catch_trial_id,
):
    """Use the existing first/second individual-response tones for catch responses."""
    if response_count == 1:
        trigger_name = "first_individual_response"
    elif response_count == 2:
        trigger_name = "second_individual_response"
    else:
        raise RuntimeError(
            f"Unexpected accepted catch-response count: {response_count}"
        )

    emit_trigger(
        trigger_name=trigger_name,
        trial_number=catch_trial_id,
        phase="Catch_Response",
        participant_number=participant_number,
        response_value=response_side,
        source_event_session_s=response_session_s,
    )


def emit_joint_response_trigger(
    participant_number,
    response_count,
    response_session_s,
    response_value,
    p1_choice,
    p2_choice,
    trial_number,
    attempt_number,
):
    """Emit trigger 05 on first press and trigger 06/07 on the second press."""
    if response_count == 1:
        trigger_name = "first_consensus_press"
    elif response_count == 2:
        if p1_choice is None or p2_choice is None:
            raise RuntimeError(
                "Second joint press was registered before both choices existed."
            )
        trigger_name = (
            "consensus_reached"
            if p1_choice == p2_choice
            else "second_consensus_fail"
        )
    else:
        raise RuntimeError(
            f"Unexpected accepted joint-response count: {response_count}"
        )

    emit_trigger(
        trigger_name=trigger_name,
        trial_number=trial_number,
        phase="Joint_Consensus",
        attempt_number=attempt_number,
        participant_number=participant_number,
        response_value=response_value,
        source_event_session_s=response_session_s,
    )

# Base metadata copied into every output row.
session_metadata = dict(exp_info)
session_metadata.update(
    {
        "Experiment_Start_ISO": session_start_iso,
        "Randomization_Seed": randomization_seed,
        "Stimulus_List_ID": f"Seed_{randomization_seed}",
        "Stimulus_CSV": STIMULI_CSV,
        "Image_Folder": IMAGE_FOLDER,
        "Icons_Folder": ICONS_FOLDER,
        "No_Talk_Icon_File": NO_TALK_ICON_FILE,
        "Talk_Icon_File": TALK_ICON_FILE,
        "Phase_Icon_Size_Height_Units": PHASE_ICON_SIZE,
        "No_Talk_Icon_Loaded_Path": no_talk_icon_path,
        "Talk_Icon_Loaded_Path": talk_icon_path,
        "Experiment_Version": "3.16.1",
        "Catch_Trials_Enabled": 1,
        "Catch_After_Normal_Trials_JSON": json.dumps(
            list(CATCH_AFTER_NORMAL_TRIALS)
        ),
        "Catch_Wait_Before_Countdown_ms": seconds_to_ms(
            CATCH_WAIT_BEFORE_COUNTDOWN_S
        ),
        "Catch_Countdown_Duration_ms": seconds_to_ms(
            CATCH_COUNTDOWN_DURATION_S
        ),
        "Catch_Response_Deadline_ms": seconds_to_ms(
            CATCH_RESPONSE_DEADLINE_S
        ),
        "Catch_Response_Countdown_Start_ms": seconds_to_ms(
            CATCH_RESPONSE_COUNTDOWN_START_S
        ),
        "Catch_Expected_Response": CATCH_EXPECTED_RESPONSE,
        "Response_Mapping": response_mapping_description,
        "Left_Response_Label": left_txt,
        "Right_Response_Label": right_txt,
        "PsychoPy_Version": psychopy.__version__,
        "Monitor_Profile_Name": "testMonitor",
        "Requested_Window_Size_px": json.dumps([3840, 2160]),
        "Actual_Window_Size_px": json.dumps([int(v) for v in win.size]),
        "Full_Screen": 1,
        "Display_Screen_Index": 0,
        "Window_Units": "height",
        "Use_Retina": 1,
        "Measured_Refresh_Rate_Hz": rounded(measured_refresh_rate_hz, 3),
        "Display_Frame_Period_ms": seconds_to_ms(frame_period_s, 4),
        "Dropped_Frame_Threshold_ms": seconds_to_ms(win.refreshThreshold, 4),
        "Fixation_Base_Duration_ms": seconds_to_ms(FIXATION_BASE_DURATION_S),
        "Jitter_Distribution": "Uniform extension while fixation remains visible",
        "Jitter_Min_ms": seconds_to_ms(FIXATION_JITTER_MIN_S),
        "Jitter_Max_ms": seconds_to_ms(FIXATION_JITTER_MAX_S),
        "Fixation_Total_Range_ms": "500-900",
        "Pre_Individual_Prompt_Duration_ms": seconds_to_ms(
            PRE_INDIVIDUAL_PROMPT_DURATION_S
        ),
        "Pre_Individual_Prompt_Frame_Count": pre_individual_prompt_frame_count,
        "Pre_Individual_Prompt_Planned_Duration_ms": seconds_to_ms(
            pre_individual_prompt_planned_s
        ),
        "Fixed_Image_Presentation_Duration_ms": seconds_to_ms(
            IMAGE_PRESENTATION_DURATION_S
        ),
        "Image_Presentation_Frame_Count": image_presentation_frame_count,
        "Image_Presentation_Planned_Duration_ms": seconds_to_ms(
            image_presentation_planned_s
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
        "Audio_Trigger_Folder": TRIGGER_FOLDER,
        "Audio_Trigger_Files_JSON": json.dumps(TRIGGER_FILES),
        "Audio_Trigger_Log_File": trigger_log_filename,
        "Audio_Trigger_Volume": TRIGGER_VOLUME,
        "Audio_Trigger_Backend_Class": (
            f"{sound.Sound.__module__}.{sound.Sound.__name__}"
        ),
        "Audio_Output_Device_Mode": "System_Default_At_Script_Start",
        "Audio_Trigger_Method": (
            "Preloaded PsychoPy Sound objects; image-onset trigger dispatched "
            "with Window.callOnFlip; response triggers dispatched immediately "
            "after accepted asynchronous key-down events"
        ),
        "RT_Unit": "milliseconds",
        "Session_Timestamp_Unit": "seconds elapsed since experiment start",
        "Response_Timing_Method": (
            "v2-compatible shared PsychoPy Keyboard KeyPress.rt; "
            "keyboard clock reset on response-screen onset flip"
        ),
    }
)


def checkpoint():
    """Save completed trials plus the current partial trial."""
    if current_trial_record is not None:
        current_trial_record["Last_Checkpoint_ISO"] = iso_now()

    atomic_write_csv(
        completed_rows=trial_results,
        filename=data_filename,
        current_row=current_trial_record,
    )
    atomic_write_trigger_log(
        trigger_rows=trigger_events,
        filename=trigger_log_filename,
    )


def run_catch_trial(catch_trial_number, after_normal_trial):
    """
    Run one attention/catch trial without altering the normal face-trial logic.

    Sequence:
      1. Standard two-second no-talk prompt.
      2. Standard 500-900 ms fixation (independent catch jitter RNG).
      3. Catch instruction appears; no response should be made.
      4. After two seconds, a five-second countdown appears below the text.
      5. The countdown is replaced by "Now you can press the LEFT button."
      6. Participants have 10 seconds to respond; during the final five
         seconds, the prompt becomes a 5-to-1 response countdown.
      7. The first response from each participant is recorded independently.

    The normal image-onset tone (02) is reused at catch-instruction onset, and
    the normal first/second individual-response tones (03/04) are reused for
    the two accepted catch responses. Trigger-log phase/trial-type fields make
    these events unambiguous without requiring any new WAV files.
    """
    global current_trial_record, current_phase, trial_trigger_events

    # Keep the Trial column numeric for downstream analysis. Normal face trials
    # remain 1-48; catch trials use 49-51 and are identified explicitly by
    # Trial_Type/Catch_Trial_Number and their insertion position.
    catch_trial_id = len(stim_df) + catch_trial_number
    catch_trial_label = f"Catch_{catch_trial_number}"
    overall_sequence_position = (
        after_normal_trial
        + sum(
            1
            for catch_after in CATCH_AFTER_NORMAL_TRIALS
            if catch_after <= after_normal_trial
        )
    )

    trial_trigger_events = []
    trial_dropped_frames_start = int(win.nDroppedFrames)

    catch_jitter_sample_s = float(
        catch_jitter_rng.uniform(
            FIXATION_JITTER_MIN_S,
            FIXATION_JITTER_MAX_S,
        )
    )
    catch_fixation_sampled_total_s = (
        FIXATION_BASE_DURATION_S + catch_jitter_sample_s
    )
    catch_fixation_frame_count = max(
        1,
        int(round(catch_fixation_sampled_total_s / frame_period_s)),
    )
    catch_fixation_planned_s = catch_fixation_frame_count * frame_period_s

    current_phase = "Catch_Trial_Initialization"
    current_trial_record = dict(session_metadata)
    current_trial_record.update(
        {
            "Trial": catch_trial_id,
            "Trial_Type": "Catch",
            "Normal_Trial_Number": None,
            "Catch_Trial_Number": catch_trial_number,
            "Catch_After_Normal_Trial": after_normal_trial,
            "Overall_Sequence_Position": overall_sequence_position,
            "Trial_Order_Position": None,
            "Block_Number": 1,
            "Trial_In_Block": None,
            "Original_CSV_Row": None,
            "Stimulus_ID": catch_trial_label,
            "Filename": None,
            "Authenticity": None,
            "Parsed_Type": None,
            "Parsed_Race": None,
            "Parsed_Gender": None,
            "Difficulty_Group": "Catch",
            "Left_Side_Label": None,
            "Right_Side_Label": None,
            "Trial_Start_ISO": iso_now(),
            "Trial_Start_Session_Elapsed_s": rounded(session_clock.getTime()),
            "Pre_Individual_Prompt_Onset_Session_Elapsed_s": None,
            "Pre_Individual_Prompt_Offset_Session_Elapsed_s": None,
            "Pre_Individual_Prompt_Frame_Count": pre_individual_prompt_frame_count,
            "Pre_Individual_Prompt_Planned_Duration_ms": seconds_to_ms(
                pre_individual_prompt_planned_s
            ),
            "Pre_Individual_Prompt_Actual_Duration_ms": None,
            "Fixation_Onset_Session_Elapsed_s": None,
            "Fixation_Offset_Session_Elapsed_s": None,
            "Fixation_Base_Frame_Count": base_fixation_frame_count,
            "Fixation_Frame_Count": catch_fixation_frame_count,
            "Fixation_Sampled_Total_Duration_ms": seconds_to_ms(
                catch_fixation_sampled_total_s
            ),
            "Fixation_Planned_Duration_ms": seconds_to_ms(
                catch_fixation_planned_s
            ),
            "Fixation_Actual_Duration_ms": None,
            "Jitter_Sampled_Duration_ms": seconds_to_ms(catch_jitter_sample_s),
            "Jitter_Frame_Count_Equivalent": max(
                0,
                catch_fixation_frame_count - base_fixation_frame_count,
            ),
            "Jitter_Planned_Duration_ms": seconds_to_ms(
                max(
                    0.0,
                    catch_fixation_planned_s - FIXATION_BASE_DURATION_S,
                )
            ),
            "Jitter_Actual_Duration_ms": None,
            "Catch_Instruction_Text": catch_wait_text.text,
            "Catch_Instruction_Onset_Session_Elapsed_s": None,
            "Catch_Instruction_Lead_Frame_Count": catch_wait_frame_count,
            "Catch_Instruction_Lead_Planned_Duration_ms": seconds_to_ms(
                catch_wait_planned_s
            ),
            "Catch_Instruction_Lead_Actual_Duration_ms": None,
            "Catch_Countdown_Onset_Session_Elapsed_s": None,
            "Catch_Countdown_Offset_Session_Elapsed_s": None,
            "Catch_Countdown_Frame_Count": catch_countdown_frame_count,
            "Catch_Countdown_Planned_Duration_ms": seconds_to_ms(
                catch_countdown_planned_s
            ),
            "Catch_Countdown_Actual_Duration_ms": None,
            "Catch_Response_Prompt_Text": catch_response_prompt.text,
            "Catch_Response_Prompt_Onset_Session_Elapsed_s": None,
            "Catch_Response_Deadline_Session_Elapsed_s": None,
            "Catch_Response_Countdown_First_Shown_Session_Elapsed_s": None,
            "Catch_Expected_Response": CATCH_EXPECTED_RESPONSE,
            "Catch_Premature_Response_Count": 0,
            "Catch_P1_Premature_Response_Count": 0,
            "Catch_P2_Premature_Response_Count": 0,
            "Catch_Premature_Responses_JSON": "[]",
            "Catch_P1_Response": None,
            "Catch_P1_Response_Key": None,
            "Catch_P1_Response_RT_ms": None,
            "Catch_P1_Response_Session_Elapsed_s": None,
            "Catch_P1_Correct": None,
            "Catch_P1_Timed_Out": None,
            "Catch_P2_Response": None,
            "Catch_P2_Response_Key": None,
            "Catch_P2_Response_RT_ms": None,
            "Catch_P2_Response_Session_Elapsed_s": None,
            "Catch_P2_Correct": None,
            "Catch_P2_Timed_Out": None,
            "Catch_Both_Responded": None,
            "Catch_Both_Final_Responses_Correct": None,
            "Joint_Decision_Required": 0,
            "Dropped_Frames_Before_Trial": trial_dropped_frames_start,
            "Dropped_Frames_Trial": None,
            "Dropped_Frames_Cumulative": None,
            "Trigger_Events_JSON": "[]",
            "Last_Trigger_Name": None,
            "Last_Trigger_Dispatch_Session_Elapsed_s": None,
            "Trial_Status": "Initialized_Catch",
            "Trial_Completed": 0,
            "Experiment_Aborted_During_Trial": 0,
            "Abort_Phase": None,
            "Abort_Reason": None,
            "Session_Ended_Early": 0,
            "Trial_End_ISO": None,
            "Trial_End_Session_Elapsed_s": None,
            "Last_Checkpoint_ISO": None,
        }
    )
    checkpoint()

    # ------------------------------------------------------------------
    # Standard two-second no-talk prompt
    # ------------------------------------------------------------------
    prestim_holder = {}

    def mark_catch_pre_prompt_onset():
        onset = session_clock.getTime()
        prestim_holder["pre_prompt_onset"] = onset
        current_trial_record[
            "Pre_Individual_Prompt_Onset_Session_Elapsed_s"
        ] = rounded(onset)

    current_phase = "Catch_Pre_Individual_No_Talk_Prompt"
    current_trial_record["Trial_Status"] = current_phase
    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(mark_catch_pre_prompt_onset)

    for _ in range(pre_individual_prompt_frame_count):
        draw_pre_individual_prompt(
            completed_trials=after_normal_trial,
            total_trials=total_trials,
        )
        win.flip()

        if kb.getKeys(
            keyList=[ESCAPE_KEY],
            waitRelease=False,
            clear=True,
        ):
            raise KeyboardInterrupt

    # ------------------------------------------------------------------
    # Standard variable fixation using the independent catch jitter RNG
    # ------------------------------------------------------------------
    fixation_holder = {}

    def mark_catch_fixation_onset():
        onset = session_clock.getTime()
        fixation_holder["onset"] = onset
        current_trial_record[
            "Pre_Individual_Prompt_Offset_Session_Elapsed_s"
        ] = rounded(onset)
        current_trial_record[
            "Pre_Individual_Prompt_Actual_Duration_ms"
        ] = seconds_to_ms(onset - prestim_holder["pre_prompt_onset"])
        current_trial_record["Fixation_Onset_Session_Elapsed_s"] = rounded(onset)

    current_phase = "Catch_Variable_Fixation"
    current_trial_record["Trial_Status"] = current_phase
    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(mark_catch_fixation_onset)

    for _ in range(catch_fixation_frame_count):
        no_talk_icon.draw()
        fixation_cross.draw()
        win.flip()

        if kb.getKeys(
            keyList=[ESCAPE_KEY],
            waitRelease=False,
            clear=True,
        ):
            raise KeyboardInterrupt

    # ------------------------------------------------------------------
    # Catch instruction: 2 s without countdown, then 5 s countdown
    # ------------------------------------------------------------------
    catch_onset_holder = {}
    premature_events = []

    def mark_catch_instruction_onset():
        onset = session_clock.getTime()
        catch_onset_holder["instruction_onset"] = onset
        current_trial_record["Fixation_Offset_Session_Elapsed_s"] = rounded(onset)
        current_trial_record[
            "Catch_Instruction_Onset_Session_Elapsed_s"
        ] = rounded(onset)

        fixation_actual_ms = seconds_to_ms(onset - fixation_holder["onset"])
        current_trial_record["Fixation_Actual_Duration_ms"] = fixation_actual_ms
        current_trial_record["Jitter_Actual_Duration_ms"] = round(
            max(
                0.0,
                fixation_actual_ms - seconds_to_ms(FIXATION_BASE_DURATION_S),
            ),
            3,
        )

    def log_premature_keys(phase_name):
        key_events = kb.getKeys(
            keyList=ALL_RESPONSE_KEYS,
            waitRelease=False,
            clear=True,
        )
        key_events = sorted(
            key_events,
            key=lambda key_event: float(
                getattr(key_event, "rt", float("inf"))
            ),
        )

        for key_event in key_events:
            if key_event.name == ESCAPE_KEY:
                raise KeyboardInterrupt

            key_rt_ms = response_rt_ms(key_event, catch_onset_holder)
            if key_event.name in P1_KEYS:
                participant_number = 1
                current_trial_record[
                    "Catch_P1_Premature_Response_Count"
                ] += 1
            elif key_event.name in P2_KEYS:
                participant_number = 2
                current_trial_record[
                    "Catch_P2_Premature_Response_Count"
                ] += 1
            else:
                continue

            response_side = response_side_from_key(key_event.name)
            premature_events.append(
                {
                    "participant": participant_number,
                    "key": key_event.name,
                    "response_side": response_side,
                    "phase": phase_name,
                    "rt_from_catch_instruction_ms": key_rt_ms,
                    "session_elapsed_s": rounded(
                        catch_onset_holder["instruction_onset"]
                        + (key_rt_ms / 1000.0)
                    ),
                }
            )

        current_trial_record[
            "Catch_Premature_Response_Count"
        ] = len(premature_events)
        current_trial_record[
            "Catch_Premature_Responses_JSON"
        ] = json.dumps(premature_events, ensure_ascii=False)

    current_phase = "Catch_Instruction_Wait"
    current_trial_record["Trial_Status"] = current_phase
    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(kb.clock.reset)
    win.callOnFlip(mark_catch_instruction_onset)
    # Reuse tone 02 as the post-fixation catch-onset acoustic anchor.
    win.callOnFlip(
        emit_trigger,
        "image_onset",
        trial_number=catch_trial_id,
        phase="Catch_Instruction_Onset",
    )

    catch_wait_text.draw()
    win.flip()

    for _ in range(max(0, catch_wait_frame_count - 1)):
        log_premature_keys("Instruction_Wait")
        catch_wait_text.draw()
        win.flip()

    # Capture events from the final instruction-only frame before countdown.
    log_premature_keys("Instruction_Wait")

    countdown_holder = {}

    def mark_catch_countdown_onset():
        onset = session_clock.getTime()
        countdown_holder["onset"] = onset
        current_trial_record[
            "Catch_Countdown_Onset_Session_Elapsed_s"
        ] = rounded(onset)
        current_trial_record[
            "Catch_Instruction_Lead_Actual_Duration_ms"
        ] = seconds_to_ms(
            onset - catch_onset_holder["instruction_onset"]
        )

    current_phase = "Catch_Countdown"
    current_trial_record["Trial_Status"] = current_phase
    win.callOnFlip(mark_catch_countdown_onset)

    for frame_index in range(catch_countdown_frame_count):
        if frame_index > 0:
            log_premature_keys("Countdown")

        displayed_number = max(
            1,
            int(CATCH_COUNTDOWN_DURATION_S)
            - int(
                (frame_index * int(CATCH_COUNTDOWN_DURATION_S))
                / catch_countdown_frame_count
            ),
        )
        catch_countdown_text.text = str(displayed_number)
        catch_wait_text.draw()
        catch_countdown_text.draw()
        win.flip()

    # Capture events from the final countdown frame before enabling responses.
    log_premature_keys("Countdown")

    # ------------------------------------------------------------------
    # Response prompt: each participant's first response is the catch response
    # ------------------------------------------------------------------
    response_holder = {}

    def mark_catch_response_prompt_onset():
        onset = session_clock.getTime()
        response_holder["onset"] = onset
        current_trial_record[
            "Catch_Countdown_Offset_Session_Elapsed_s"
        ] = rounded(onset)
        current_trial_record[
            "Catch_Countdown_Actual_Duration_ms"
        ] = seconds_to_ms(onset - countdown_holder["onset"])
        current_trial_record[
            "Catch_Response_Prompt_Onset_Session_Elapsed_s"
        ] = rounded(onset)
        current_trial_record[
            "Catch_Response_Deadline_Session_Elapsed_s"
        ] = rounded(onset + CATCH_RESPONSE_DEADLINE_S)

    current_phase = "Catch_Response"
    current_trial_record["Trial_Status"] = current_phase
    win.callOnFlip(kb.clearEvents)
    win.callOnFlip(kb.clock.reset)
    win.callOnFlip(mark_catch_response_prompt_onset)
    catch_response_prompt.draw()
    win.flip()

    p1_responded = False
    p2_responded = False
    p1_side = None
    p2_side = None
    p1_rt_ms = None
    p2_rt_ms = None
    response_countdown_onset_marked = False

    def mark_catch_response_countdown_onset():
        current_trial_record[
            "Catch_Response_Countdown_First_Shown_Session_Elapsed_s"
        ] = rounded(session_clock.getTime())

    def draw_catch_response_screen(seconds_remaining):
        """Draw the catch response prompt or its final-five-second countdown."""
        if 0.0 < seconds_remaining <= CATCH_RESPONSE_COUNTDOWN_START_S:
            displayed_seconds = max(1, int(math.ceil(seconds_remaining)))
            second_word = "second" if displayed_seconds == 1 else "seconds"
            catch_response_countdown_text.text = (
                f"You have {displayed_seconds} {second_word} left to press "
                "the LEFT button."
            )
            catch_response_countdown_text.draw()
        else:
            catch_response_prompt.draw()

    def register_catch_response(key_event):
        nonlocal p1_responded, p2_responded
        nonlocal p1_side, p2_side, p1_rt_ms, p2_rt_ms

        if key_event.name == ESCAPE_KEY:
            raise KeyboardInterrupt

        key_rt_ms = response_rt_ms(key_event, response_holder)
        if key_rt_ms > seconds_to_ms(CATCH_RESPONSE_DEADLINE_S):
            return

        if key_event.name in P1_KEYS and not p1_responded:
            p1_side = response_side_from_key(key_event.name)
            p1_rt_ms = key_rt_ms
            p1_responded = True
            current_trial_record["Catch_P1_Response"] = p1_side
            current_trial_record["Catch_P1_Response_Key"] = key_event.name
            current_trial_record["Catch_P1_Response_RT_ms"] = p1_rt_ms
            current_trial_record[
                "Catch_P1_Response_Session_Elapsed_s"
            ] = rounded(response_holder["onset"] + (p1_rt_ms / 1000.0))
            current_trial_record["Catch_P1_Correct"] = int(
                p1_side == CATCH_EXPECTED_RESPONSE
            )
            current_trial_record["Catch_P1_Timed_Out"] = 0
            emit_catch_response_trigger(
                participant_number=1,
                response_count=int(p1_responded) + int(p2_responded),
                response_session_s=(
                    response_holder["onset"] + (p1_rt_ms / 1000.0)
                ),
                response_side=p1_side,
                catch_trial_id=catch_trial_id,
            )

        if key_event.name in P2_KEYS and not p2_responded:
            p2_side = response_side_from_key(key_event.name)
            p2_rt_ms = key_rt_ms
            p2_responded = True
            current_trial_record["Catch_P2_Response"] = p2_side
            current_trial_record["Catch_P2_Response_Key"] = key_event.name
            current_trial_record["Catch_P2_Response_RT_ms"] = p2_rt_ms
            current_trial_record[
                "Catch_P2_Response_Session_Elapsed_s"
            ] = rounded(response_holder["onset"] + (p2_rt_ms / 1000.0))
            current_trial_record["Catch_P2_Correct"] = int(
                p2_side == CATCH_EXPECTED_RESPONSE
            )
            current_trial_record["Catch_P2_Timed_Out"] = 0
            emit_catch_response_trigger(
                participant_number=2,
                response_count=int(p1_responded) + int(p2_responded),
                response_session_s=(
                    response_holder["onset"] + (p2_rt_ms / 1000.0)
                ),
                response_side=p2_side,
                catch_trial_id=catch_trial_id,
            )

    while (
        not (p1_responded and p2_responded)
        and kb.clock.getTime() < CATCH_RESPONSE_DEADLINE_S
    ):
        key_events = kb.getKeys(
            keyList=ALL_RESPONSE_KEYS,
            waitRelease=False,
            clear=True,
        )
        key_events = sorted(
            key_events,
            key=lambda key_event: float(
                getattr(key_event, "rt", float("inf"))
            ),
        )
        for key_event in key_events:
            register_catch_response(key_event)

        if not (p1_responded and p2_responded):
            seconds_remaining = max(
                0.0,
                CATCH_RESPONSE_DEADLINE_S - kb.clock.getTime(),
            )
            if (
                seconds_remaining <= CATCH_RESPONSE_COUNTDOWN_START_S
                and not response_countdown_onset_marked
                and seconds_remaining > 0.0
            ):
                response_countdown_onset_marked = True
                win.callOnFlip(mark_catch_response_countdown_onset)

            draw_catch_response_screen(seconds_remaining)
            win.flip()

    # Final timestamp-bounded read, matching the defensive handling used in
    # the normal individual-response phase.
    final_key_events = kb.getKeys(
        keyList=ALL_RESPONSE_KEYS,
        waitRelease=False,
        clear=True,
    )
    final_key_events = sorted(
        final_key_events,
        key=lambda key_event: float(
            getattr(key_event, "rt", float("inf"))
        ),
    )
    for key_event in final_key_events:
        register_catch_response(key_event)

    if not p1_responded:
        current_trial_record["Catch_P1_Timed_Out"] = 1
        current_trial_record["Catch_P1_Correct"] = None
    if not p2_responded:
        current_trial_record["Catch_P2_Timed_Out"] = 1
        current_trial_record["Catch_P2_Correct"] = None

    current_trial_record["Catch_Both_Responded"] = int(
        p1_responded and p2_responded
    )
    current_trial_record["Catch_Both_Final_Responses_Correct"] = (
        int(
            p1_side == CATCH_EXPECTED_RESPONSE
            and p2_side == CATCH_EXPECTED_RESPONSE
        )
        if p1_responded and p2_responded
        else None
    )

    dropped_frames_cumulative = int(win.nDroppedFrames)
    current_trial_record["Dropped_Frames_Trial"] = (
        dropped_frames_cumulative - trial_dropped_frames_start
    )
    current_trial_record[
        "Dropped_Frames_Cumulative"
    ] = dropped_frames_cumulative
    current_trial_record["Trial_Status"] = (
        "Completed_Catch"
        if p1_responded and p2_responded
        else "Completed_Catch_Response_Timeout"
    )
    current_trial_record["Trial_Completed"] = 1
    current_trial_record["Trial_End_ISO"] = iso_now()
    current_trial_record["Trial_End_Session_Elapsed_s"] = rounded(
        session_clock.getTime()
    )

    # Clear the catch display before the next normal trial (or experiment end).
    win.flip()

    trial_results.append(dict(current_trial_record))
    current_trial_record = None
    atomic_write_csv(
        completed_rows=trial_results,
        filename=data_filename,
        current_row=None,
    )
    atomic_write_trigger_log(
        trigger_rows=trigger_events,
        filename=trigger_log_filename,
    )


try:
    session_clock.reset()

    current_phase = "Instructions"
    for instruction_page, instruction_icon in instruction_pages:
        show_text_and_wait_for_space(
            instruction_page,
            icon=instruction_icon,
        )

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

    current_phase = "Ready_To_Begin"
    wait_for_both_left_buttons_to_start()

    current_phase = "Experiment_Start_Trigger"
    emit_trigger(
        trigger_name="experiment_start",
        phase="Experiment_Start",
    )

    # Clear any residual check/start events before the first trial. The trial's
    # existing flip-synchronized keyboard reset remains unchanged.
    kb.clearEvents()
    event.clearEvents(eventType="keyboard")

    for index, row in stim_df.iterrows():
        trial_trigger_events = []
        current_phase = "Trial_Initialization"

        img_filename = str(row["Filename"])
        parts = img_filename.split(".")
        img_type, img_race, img_gender = parts[1], parts[2], parts[3]
        auth_label = "Real" if img_type == "R" else "AI"

        stim_image.image = os.path.join(IMAGE_FOLDER, img_filename)

        def get_choice(key_name):
            if key_name in [P1_KEYS[0], P2_KEYS[0]]:
                return left_txt
            if key_name in [P1_KEYS[1], P2_KEYS[1]]:
                return right_txt
            return None

        jitter_sample_s = float(
            jitter_rng.uniform(
                FIXATION_JITTER_MIN_S,
                FIXATION_JITTER_MAX_S,
            )
        )
        fixation_sampled_total_s = FIXATION_BASE_DURATION_S + jitter_sample_s
        fixation_frame_count = max(
            1,
            int(round(fixation_sampled_total_s / frame_period_s)),
        )
        fixation_planned_s = fixation_frame_count * frame_period_s
        trial_dropped_frames_start = int(win.nDroppedFrames)

        # A complete schema is created before the trial begins so that an
        # interrupted trial can still be written as a structurally complete row.
        current_trial_record = dict(session_metadata)
        current_trial_record.update(
            {
                "Trial": index + 1,
                "Trial_Type": "Main",
                "Normal_Trial_Number": index + 1,
                "Catch_Trial_Number": None,
                "Catch_After_Normal_Trial": None,
                "Overall_Sequence_Position": (
                    (index + 1)
                    + sum(
                        1
                        for catch_after in CATCH_AFTER_NORMAL_TRIALS
                        if catch_after < (index + 1)
                    )
                ),
                "Trial_Order_Position": index + 1,
                "Block_Number": 1,
                "Trial_In_Block": index + 1,
                "Original_CSV_Row": int(row["_Original_CSV_Row"]),
                "Stimulus_ID": row["_Stimulus_ID"],
                "Filename": img_filename,
                "Authenticity": auth_label,
                "Parsed_Type": img_type,
                "Parsed_Race": img_race,
                "Parsed_Gender": img_gender,
                "Difficulty_Group": row["Group"],
                "Left_Side_Label": left_txt,
                "Right_Side_Label": right_txt,
                "Trial_Start_ISO": iso_now(),
                "Trial_Start_Session_Elapsed_s": rounded(session_clock.getTime()),
                "Pre_Individual_Prompt_Onset_Session_Elapsed_s": None,
                "Pre_Individual_Prompt_Offset_Session_Elapsed_s": None,
                "Pre_Individual_Prompt_Frame_Count": pre_individual_prompt_frame_count,
                "Pre_Individual_Prompt_Planned_Duration_ms": seconds_to_ms(
                    pre_individual_prompt_planned_s
                ),
                "Pre_Individual_Prompt_Actual_Duration_ms": None,
                "Fixation_Onset_Session_Elapsed_s": None,
                "Fixation_Offset_Session_Elapsed_s": None,
                "Fixation_Base_Frame_Count": base_fixation_frame_count,
                "Fixation_Frame_Count": fixation_frame_count,
                "Fixation_Sampled_Total_Duration_ms": seconds_to_ms(
                    fixation_sampled_total_s
                ),
                "Fixation_Planned_Duration_ms": seconds_to_ms(
                    fixation_planned_s
                ),
                "Fixation_Actual_Duration_ms": None,
                "Jitter_Sampled_Duration_ms": seconds_to_ms(jitter_sample_s),
                "Jitter_Frame_Count_Equivalent": max(
                    0,
                    fixation_frame_count - base_fixation_frame_count,
                ),
                "Jitter_Planned_Duration_ms": seconds_to_ms(
                    max(
                        0.0,
                        fixation_planned_s - FIXATION_BASE_DURATION_S,
                    )
                ),
                "Jitter_Actual_Duration_ms": None,
                "Stimulus_Onset_Session_Elapsed_s": None,
                "Individual_Prompt_Onset_Session_Elapsed_s": None,
                "Image_Offset_Session_Elapsed_s": None,
                "Image_Presentation_Frame_Count": image_presentation_frame_count,
                "Image_Presentation_Planned_Duration_ms": seconds_to_ms(
                    image_presentation_planned_s
                ),
                "Image_Visible_Duration_ms": None,
                "Image_Offset_Reason": None,
                "Both_Responded_While_Image_Visible": None,
                "Post_Image_Individual_Screen_Onset_Session_Elapsed_s": None,
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
                "Initial_Disagreement": None,
                "Correct_Participant_During_Disagreement": None,
                "Joint_Decision_Required": None,
                "Joint_Deadline_Start_Session_Elapsed_s": None,
                "Joint_Deadline_Session_Elapsed_s": None,
                "Joint_Prompt_First_Onset_Session_Elapsed_s": None,
                "Joint_Prompt_Delay_After_Image_Offset_ms": None,
                "Countdown_First_Shown_Session_Elapsed_s": None,
                "Consensus_Reached_Session_Elapsed_s": None,
                "Total_Consensus_Duration_ms": None,
                "Dyadic_Window_Elapsed_To_Outcome_ms": None,
                "Consensus_Time_Limit_Exceeded": 0,
                "Consensus_Timeout_Session_Elapsed_s": None,
                "Consensus_Timeout_Phase": None,
                "Consensus_Attempts": 0,
                "Failed_Consensus_Attempts": 0,
                "Failed_Attempts_Log": "None",
                "Failed_Joint_Attempts_RT_JSON": "[]",
                "Joint_Attempts_Detail_JSON": "[]",
                "First_Joint_Responder_Overall": None,
                "First_Consensus_Responder": None,
                "Final_Joint_P1_Choice": None,
                "Final_Joint_P1_RT_ms": None,
                "Final_Joint_P2_Choice": None,
                "Final_Joint_P2_RT_ms": None,
                "Final_Joint_Attempt_Duration_ms": None,
                "Final_Joint_Choice": None,
                "Group_Followed": None,
                "Joint_Correct": None,
                "Dropped_Frames_Before_Trial": trial_dropped_frames_start,
                "Dropped_Frames_Trial": None,
                "Dropped_Frames_Cumulative": None,
                "Current_Joint_Attempt": None,
                "Current_Joint_P1_Choice": None,
                "Current_Joint_P1_RT_ms": None,
                "Current_Joint_P2_Choice": None,
                "Current_Joint_P2_RT_ms": None,
                "Trigger_Events_JSON": "[]",
                "Last_Trigger_Name": None,
                "Last_Trigger_Dispatch_Session_Elapsed_s": None,
                "Trial_Status": "Initialized",
                "Trial_Completed": 0,
                "Experiment_Aborted_During_Trial": 0,
                "Abort_Phase": None,
                "Abort_Reason": None,
                "Session_Ended_Early": 0,
                "Trial_End_ISO": None,
                "Trial_End_Session_Elapsed_s": None,
                "Last_Checkpoint_ISO": None,
            }
        )

        # Save the initialized row before the precisely timed fixation begins.
        checkpoint()

        # ---------------------------------------------------------------------
        # Two-second no-talk PNG plus red prompt before fixation
        # ---------------------------------------------------------------------
        prestim_holder = {}

        def mark_pre_individual_prompt_onset():
            prompt_onset_time = session_clock.getTime()
            prestim_holder["pre_individual_prompt_onset"] = prompt_onset_time
            current_trial_record[
                "Pre_Individual_Prompt_Onset_Session_Elapsed_s"
            ] = rounded(prompt_onset_time)
            current_trial_record[
                "Individual_Prompt_Onset_Session_Elapsed_s"
            ] = rounded(prompt_onset_time)

        current_phase = "Pre_Individual_No_Talk_Prompt"
        current_trial_record["Trial_Status"] = current_phase
        win.callOnFlip(kb.clearEvents)
        win.callOnFlip(mark_pre_individual_prompt_onset)

        for _ in range(pre_individual_prompt_frame_count):
            draw_pre_individual_prompt(
                completed_trials=index,
                total_trials=total_trials,
            )
            win.flip()

            if kb.getKeys(
                keyList=[ESCAPE_KEY],
                waitRelease=False,
                clear=True,
            ):
                raise KeyboardInterrupt

        # ---------------------------------------------------------------------
        # Variable fixation: no-talk PNG remains visible for 500-900 ms total
        # ---------------------------------------------------------------------
        def mark_fixation_onset():
            fixation_onset_time = session_clock.getTime()
            prestim_holder["fixation_onset"] = fixation_onset_time
            current_trial_record[
                "Pre_Individual_Prompt_Offset_Session_Elapsed_s"
            ] = rounded(fixation_onset_time)
            current_trial_record[
                "Pre_Individual_Prompt_Actual_Duration_ms"
            ] = seconds_to_ms(
                fixation_onset_time
                - prestim_holder["pre_individual_prompt_onset"]
            )
            current_trial_record[
                "Fixation_Onset_Session_Elapsed_s"
            ] = rounded(fixation_onset_time)

        current_phase = "Variable_Fixation"
        current_trial_record["Trial_Status"] = current_phase

        # Responses during fixation are ignored. Events are cleared again on
        # the actual image-onset flip before the individual-response clock starts.
        win.callOnFlip(kb.clearEvents)
        win.callOnFlip(mark_fixation_onset)

        for _ in range(fixation_frame_count):
            no_talk_icon.draw()
            fixation_cross.draw()
            win.flip()

            if kb.getKeys(
                keyList=[ESCAPE_KEY],
                waitRelease=False,
                clear=True,
            ):
                raise KeyboardInterrupt

        # ---------------------------------------------------------------------
        # Individual phase: fixed 3-second image exposure; 15-second deadline
        # ---------------------------------------------------------------------
        current_phase = "Individual_Response_Image_Visible"
        current_trial_record["Trial_Status"] = current_phase

        p1_indiv_responded = False
        p2_indiv_responded = False
        p1_indiv_choice = None
        p2_indiv_choice = None
        p1_indiv_rt_ms = None
        p2_indiv_rt_ms = None

        onset_holder = {}

        def mark_individual_onset():
            onset_time = session_clock.getTime()
            onset_holder["time"] = onset_time
            prestim_holder["fixation_offset"] = onset_time
            current_trial_record[
                "Fixation_Offset_Session_Elapsed_s"
            ] = rounded(onset_time)
            current_trial_record[
                "Stimulus_Onset_Session_Elapsed_s"
            ] = rounded(onset_time)
            # The red individual prompt was shown before fixation; no verbal
            # prompt is present during image viewing.
            current_trial_record[
                "Individual_Response_Deadline_Session_Elapsed_s"
            ] = rounded(onset_time + INDIVIDUAL_RESPONSE_DEADLINE_S)

            fixation_actual_ms = seconds_to_ms(
                onset_time - prestim_holder["fixation_onset"]
            )
            current_trial_record[
                "Fixation_Actual_Duration_ms"
            ] = fixation_actual_ms
            current_trial_record[
                "Jitter_Actual_Duration_ms"
            ] = round(
                max(
                    0.0,
                    fixation_actual_ms
                    - seconds_to_ms(FIXATION_BASE_DURATION_S),
                ),
                3,
            )

        # The image immediately replaces the fixation cross. The shared
        # Keyboard clock is reset on this physical display flip, so individual
        # RTs are measured from actual image onset.
        win.callOnFlip(kb.clearEvents)
        win.callOnFlip(kb.clock.reset)
        win.callOnFlip(mark_individual_onset)
        win.callOnFlip(
            emit_trigger,
            "image_onset",
            trial_number=index + 1,
            phase="Image_Onset",
        )

        draw_individual_image_interface(
            completed_trials=index,
            total_trials=total_trials,
        )
        win.flip()

        # Keep the image visible for a fixed number of display frames. The
        # initial onset flip above presents frame 1; these remaining flips keep
        # the same image visible until the planned 3-second offset refresh.
        for _ in range(max(0, image_presentation_frame_count - 1)):
            keys = kb.getKeys(
                keyList=ALL_RESPONSE_KEYS,
                waitRelease=False,
                clear=True,
            )

            for key_event in keys:
                if key_event.name == ESCAPE_KEY:
                    raise KeyboardInterrupt

                key_rt_ms = response_rt_ms(key_event, onset_holder)
                if key_rt_ms > seconds_to_ms(INDIVIDUAL_RESPONSE_DEADLINE_S):
                    continue

                if key_event.name in P1_KEYS and not p1_indiv_responded:
                    p1_indiv_choice = get_choice(key_event.name)
                    p1_indiv_rt_ms = key_rt_ms
                    p1_indiv_responded = True

                    current_trial_record["P1_Indiv_Choice"] = p1_indiv_choice
                    current_trial_record["P1_Indiv_RT_ms"] = p1_indiv_rt_ms
                    current_trial_record["P1_Indiv_Response_During_Image"] = 1
                    current_trial_record[
                        "P1_Indiv_Response_Session_Elapsed_s"
                    ] = rounded(onset_holder["time"] + (p1_indiv_rt_ms / 1000.0))
                    current_trial_record[
                        "P1_Indiv_Response_Within_Deadline"
                    ] = 1
                    current_trial_record["P1_Indiv_Timed_Out"] = 0
                    emit_individual_response_trigger(
                        participant_number=1,
                        response_count=(
                            int(p1_indiv_responded) + int(p2_indiv_responded)
                        ),
                        response_session_s=(
                            onset_holder["time"] + (p1_indiv_rt_ms / 1000.0)
                        ),
                        response_value=p1_indiv_choice,
                        trial_number=index + 1,
                    )

                if key_event.name in P2_KEYS and not p2_indiv_responded:
                    p2_indiv_choice = get_choice(key_event.name)
                    p2_indiv_rt_ms = key_rt_ms
                    p2_indiv_responded = True

                    current_trial_record["P2_Indiv_Choice"] = p2_indiv_choice
                    current_trial_record["P2_Indiv_RT_ms"] = p2_indiv_rt_ms
                    current_trial_record["P2_Indiv_Response_During_Image"] = 1
                    current_trial_record[
                        "P2_Indiv_Response_Session_Elapsed_s"
                    ] = rounded(onset_holder["time"] + (p2_indiv_rt_ms / 1000.0))
                    current_trial_record[
                        "P2_Indiv_Response_Within_Deadline"
                    ] = 1
                    current_trial_record["P2_Indiv_Timed_Out"] = 0
                    emit_individual_response_trigger(
                        participant_number=2,
                        response_count=(
                            int(p1_indiv_responded) + int(p2_indiv_responded)
                        ),
                        response_session_s=(
                            onset_holder["time"] + (p2_indiv_rt_ms / 1000.0)
                        ),
                        response_value=p2_indiv_choice,
                        trial_number=index + 1,
                    )

            draw_individual_image_interface(
                completed_trials=index,
                total_trials=total_trials,
            )
            win.flip()

        # Retrieve any key-down event that occurred while the final image frame
        # was still visible but had not yet been read from the keyboard queue.
        final_visible_keys = kb.getKeys(
            keyList=ALL_RESPONSE_KEYS,
            waitRelease=False,
            clear=True,
        )
        for key_event in final_visible_keys:
            if key_event.name == ESCAPE_KEY:
                raise KeyboardInterrupt

            key_rt_ms = response_rt_ms(key_event, onset_holder)
            if key_rt_ms > seconds_to_ms(INDIVIDUAL_RESPONSE_DEADLINE_S):
                continue

            if key_event.name in P1_KEYS and not p1_indiv_responded:
                p1_indiv_choice = get_choice(key_event.name)
                p1_indiv_rt_ms = key_rt_ms
                p1_indiv_responded = True
                current_trial_record["P1_Indiv_Choice"] = p1_indiv_choice
                current_trial_record["P1_Indiv_RT_ms"] = p1_indiv_rt_ms
                current_trial_record["P1_Indiv_Response_During_Image"] = 1
                current_trial_record[
                    "P1_Indiv_Response_Session_Elapsed_s"
                ] = rounded(onset_holder["time"] + (p1_indiv_rt_ms / 1000.0))
                current_trial_record["P1_Indiv_Response_Within_Deadline"] = 1
                current_trial_record["P1_Indiv_Timed_Out"] = 0
                emit_individual_response_trigger(
                    participant_number=1,
                    response_count=(
                        int(p1_indiv_responded) + int(p2_indiv_responded)
                    ),
                    response_session_s=(
                        onset_holder["time"] + (p1_indiv_rt_ms / 1000.0)
                    ),
                    response_value=p1_indiv_choice,
                    trial_number=index + 1,
                )

            if key_event.name in P2_KEYS and not p2_indiv_responded:
                p2_indiv_choice = get_choice(key_event.name)
                p2_indiv_rt_ms = key_rt_ms
                p2_indiv_responded = True
                current_trial_record["P2_Indiv_Choice"] = p2_indiv_choice
                current_trial_record["P2_Indiv_RT_ms"] = p2_indiv_rt_ms
                current_trial_record["P2_Indiv_Response_During_Image"] = 1
                current_trial_record[
                    "P2_Indiv_Response_Session_Elapsed_s"
                ] = rounded(onset_holder["time"] + (p2_indiv_rt_ms / 1000.0))
                current_trial_record["P2_Indiv_Response_Within_Deadline"] = 1
                current_trial_record["P2_Indiv_Timed_Out"] = 0
                emit_individual_response_trigger(
                    participant_number=2,
                    response_count=(
                        int(p1_indiv_responded) + int(p2_indiv_responded)
                    ),
                    response_session_s=(
                        onset_holder["time"] + (p2_indiv_rt_ms / 1000.0)
                    ),
                    response_value=p2_indiv_choice,
                    trial_number=index + 1,
                )

        both_responded_during_image = (
            p1_indiv_responded and p2_indiv_responded
        )

        # The dyadic 60-second deadline starts on the fixed image-offset flip,
        # even when an individual response is still missing at that moment.
        image_offset_holder = {}

        def mark_fixed_image_offset():
            offset_time = session_clock.getTime()
            image_offset_holder["time"] = offset_time
            current_trial_record[
                "Image_Offset_Session_Elapsed_s"
            ] = rounded(offset_time)
            current_trial_record["Image_Visible_Duration_ms"] = seconds_to_ms(
                offset_time - onset_holder["time"]
            )
            current_trial_record["Image_Offset_Reason"] = "Fixed_3s_Exposure"

        win.callOnFlip(mark_fixed_image_offset)
        if both_responded_during_image:
            draw_response_labels_only(
                completed_trials=index,
                total_trials=total_trials,
            )
        else:
            individual_seconds_remaining = max(
                0.0,
                INDIVIDUAL_RESPONSE_DEADLINE_S - kb.clock.getTime(),
            )
            draw_individual_wait_interface(
                completed_trials=index,
                total_trials=total_trials,
                seconds_remaining=individual_seconds_remaining,
            )
        win.flip()

        current_trial_record[
            "Both_Responded_While_Image_Visible"
        ] = int(both_responded_during_image)

        # If needed, continue collecting only the missing individual response(s)
        # until 15 seconds from image onset. The face remains absent, while the
        # labels and response mapping stay visible.
        if not both_responded_during_image:
            current_phase = "Individual_Response_Image_Off"
            current_trial_record["Trial_Status"] = current_phase
            current_trial_record[
                "Post_Image_Individual_Screen_Onset_Session_Elapsed_s"
            ] = rounded(image_offset_holder["time"])

            while (
                not (p1_indiv_responded and p2_indiv_responded)
                and kb.clock.getTime() < INDIVIDUAL_RESPONSE_DEADLINE_S
            ):
                keys = kb.getKeys(
                    keyList=ALL_RESPONSE_KEYS,
                    waitRelease=False,
                    clear=True,
                )

                for key_event in keys:
                    if key_event.name == ESCAPE_KEY:
                        raise KeyboardInterrupt

                    key_rt_ms = response_rt_ms(key_event, onset_holder)
                    if key_rt_ms > seconds_to_ms(INDIVIDUAL_RESPONSE_DEADLINE_S):
                        continue

                    if key_event.name in P1_KEYS and not p1_indiv_responded:
                        p1_indiv_choice = get_choice(key_event.name)
                        p1_indiv_rt_ms = key_rt_ms
                        p1_indiv_responded = True
                        current_trial_record["P1_Indiv_Choice"] = p1_indiv_choice
                        current_trial_record["P1_Indiv_RT_ms"] = p1_indiv_rt_ms
                        current_trial_record[
                            "P1_Indiv_Response_During_Image"
                        ] = 0
                        current_trial_record[
                            "P1_Indiv_Response_Session_Elapsed_s"
                        ] = rounded(
                            onset_holder["time"] + (p1_indiv_rt_ms / 1000.0)
                        )
                        current_trial_record[
                            "P1_Indiv_Response_Within_Deadline"
                        ] = 1
                        current_trial_record["P1_Indiv_Timed_Out"] = 0
                        emit_individual_response_trigger(
                            participant_number=1,
                            response_count=(
                                int(p1_indiv_responded) + int(p2_indiv_responded)
                            ),
                            response_session_s=(
                                onset_holder["time"] + (p1_indiv_rt_ms / 1000.0)
                            ),
                            response_value=p1_indiv_choice,
                            trial_number=index + 1,
                        )

                    if key_event.name in P2_KEYS and not p2_indiv_responded:
                        p2_indiv_choice = get_choice(key_event.name)
                        p2_indiv_rt_ms = key_rt_ms
                        p2_indiv_responded = True
                        current_trial_record["P2_Indiv_Choice"] = p2_indiv_choice
                        current_trial_record["P2_Indiv_RT_ms"] = p2_indiv_rt_ms
                        current_trial_record[
                            "P2_Indiv_Response_During_Image"
                        ] = 0
                        current_trial_record[
                            "P2_Indiv_Response_Session_Elapsed_s"
                        ] = rounded(
                            onset_holder["time"] + (p2_indiv_rt_ms / 1000.0)
                        )
                        current_trial_record[
                            "P2_Indiv_Response_Within_Deadline"
                        ] = 1
                        current_trial_record["P2_Indiv_Timed_Out"] = 0
                        emit_individual_response_trigger(
                            participant_number=2,
                            response_count=(
                                int(p1_indiv_responded) + int(p2_indiv_responded)
                            ),
                            response_session_s=(
                                onset_holder["time"] + (p2_indiv_rt_ms / 1000.0)
                            ),
                            response_value=p2_indiv_choice,
                            trial_number=index + 1,
                        )

                if not (p1_indiv_responded and p2_indiv_responded):
                    individual_seconds_remaining = max(
                        0.0,
                        INDIVIDUAL_RESPONSE_DEADLINE_S - kb.clock.getTime(),
                    )
                    if (
                        individual_seconds_remaining
                        <= INDIVIDUAL_COUNTDOWN_START_S
                        and current_trial_record[
                            "Individual_Countdown_First_Shown_Session_Elapsed_s"
                        ] is None
                    ):
                        current_trial_record[
                            "Individual_Countdown_First_Shown_Session_Elapsed_s"
                        ] = rounded(session_clock.getTime())

                    draw_individual_wait_interface(
                        completed_trials=index,
                        total_trials=total_trials,
                        seconds_remaining=individual_seconds_remaining,
                    )
                    win.flip()

            # Final deadline-bound queue read: retain only key-down events whose
            # timestamps occurred no later than 15000 ms from image onset.
            deadline_keys = kb.getKeys(
                keyList=ALL_RESPONSE_KEYS,
                waitRelease=False,
                clear=True,
            )
            for key_event in deadline_keys:
                if key_event.name == ESCAPE_KEY:
                    raise KeyboardInterrupt

                key_rt_ms = response_rt_ms(key_event, onset_holder)
                if key_rt_ms > seconds_to_ms(INDIVIDUAL_RESPONSE_DEADLINE_S):
                    continue

                if key_event.name in P1_KEYS and not p1_indiv_responded:
                    p1_indiv_choice = get_choice(key_event.name)
                    p1_indiv_rt_ms = key_rt_ms
                    p1_indiv_responded = True
                    current_trial_record["P1_Indiv_Choice"] = p1_indiv_choice
                    current_trial_record["P1_Indiv_RT_ms"] = p1_indiv_rt_ms
                    current_trial_record[
                        "P1_Indiv_Response_During_Image"
                    ] = 0
                    current_trial_record[
                        "P1_Indiv_Response_Session_Elapsed_s"
                    ] = rounded(onset_holder["time"] + (p1_indiv_rt_ms / 1000.0))
                    current_trial_record[
                        "P1_Indiv_Response_Within_Deadline"
                    ] = 1
                    current_trial_record["P1_Indiv_Timed_Out"] = 0
                    emit_individual_response_trigger(
                        participant_number=1,
                        response_count=(
                            int(p1_indiv_responded) + int(p2_indiv_responded)
                        ),
                        response_session_s=(
                            onset_holder["time"] + (p1_indiv_rt_ms / 1000.0)
                        ),
                        response_value=p1_indiv_choice,
                        trial_number=index + 1,
                    )

                if key_event.name in P2_KEYS and not p2_indiv_responded:
                    p2_indiv_choice = get_choice(key_event.name)
                    p2_indiv_rt_ms = key_rt_ms
                    p2_indiv_responded = True
                    current_trial_record["P2_Indiv_Choice"] = p2_indiv_choice
                    current_trial_record["P2_Indiv_RT_ms"] = p2_indiv_rt_ms
                    current_trial_record[
                        "P2_Indiv_Response_During_Image"
                    ] = 0
                    current_trial_record[
                        "P2_Indiv_Response_Session_Elapsed_s"
                    ] = rounded(onset_holder["time"] + (p2_indiv_rt_ms / 1000.0))
                    current_trial_record[
                        "P2_Indiv_Response_Within_Deadline"
                    ] = 1
                    current_trial_record["P2_Indiv_Timed_Out"] = 0
                    emit_individual_response_trigger(
                        participant_number=2,
                        response_count=(
                            int(p1_indiv_responded) + int(p2_indiv_responded)
                        ),
                        response_session_s=(
                            onset_holder["time"] + (p2_indiv_rt_ms / 1000.0)
                        ),
                        response_value=p2_indiv_choice,
                        trial_number=index + 1,
                    )

        individual_phase_end = session_clock.getTime()
        current_trial_record[
            "Individual_Phase_End_Session_Elapsed_s"
        ] = rounded(individual_phase_end)
        current_trial_record["Individual_Phase_Duration_ms"] = seconds_to_ms(
            individual_phase_end - onset_holder["time"]
        )

        if not p1_indiv_responded:
            current_trial_record["P1_Indiv_Response_Within_Deadline"] = 0
            current_trial_record["P1_Indiv_Timed_Out"] = 1
        if not p2_indiv_responded:
            current_trial_record["P2_Indiv_Response_Within_Deadline"] = 0
            current_trial_record["P2_Indiv_Timed_Out"] = 1

        individual_responses_complete = (
            p1_indiv_responded and p2_indiv_responded
        )
        current_trial_record["Individual_Responses_Complete"] = int(
            individual_responses_complete
        )
        current_trial_record[
            "Individual_Responses_Complete_Session_Elapsed_s"
        ] = (
            rounded(
                max(
                    current_trial_record[
                        "P1_Indiv_Response_Session_Elapsed_s"
                    ],
                    current_trial_record[
                        "P2_Indiv_Response_Session_Elapsed_s"
                    ],
                )
            )
            if individual_responses_complete
            else None
        )
        current_trial_record["Individual_Missing_Response_Count"] = int(
            not p1_indiv_responded
        ) + int(not p2_indiv_responded)

        current_trial_record["First_Individual_Responder"] = (
            identify_first_responder(p1_indiv_rt_ms, p2_indiv_rt_ms)
            if individual_responses_complete
            else (
                "Participant_1"
                if p1_indiv_responded and not p2_indiv_responded
                else (
                    "Participant_2"
                    if p2_indiv_responded and not p1_indiv_responded
                    else None
                )
            )
        )
        current_trial_record["Individual_RT_Difference_ms"] = (
            round(abs(p1_indiv_rt_ms - p2_indiv_rt_ms), 3)
            if individual_responses_complete
            else None
        )

        p1_indiv_correct = (
            int(p1_indiv_choice == auth_label)
            if p1_indiv_choice is not None
            else None
        )
        p2_indiv_correct = (
            int(p2_indiv_choice == auth_label)
            if p2_indiv_choice is not None
            else None
        )
        initial_agreement = (
            int(p1_indiv_choice == p2_indiv_choice)
            if individual_responses_complete
            else None
        )
        initial_disagreement = (
            int(not initial_agreement)
            if initial_agreement is not None
            else None
        )

        current_trial_record["P1_Indiv_Correct"] = p1_indiv_correct
        current_trial_record["P2_Indiv_Correct"] = p2_indiv_correct
        current_trial_record["Initial_Agreement"] = initial_agreement
        current_trial_record["Initial_Disagreement"] = initial_disagreement
        current_trial_record["Initial_Agreement_Choice"] = (
            p1_indiv_choice if initial_agreement == 1 else None
        )

        if initial_disagreement == 1:
            if p1_indiv_correct and not p2_indiv_correct:
                correct_during_disagreement = "Participant_1"
            elif p2_indiv_correct and not p1_indiv_correct:
                correct_during_disagreement = "Participant_2"
            elif p1_indiv_correct and p2_indiv_correct:
                correct_during_disagreement = "Both"
            else:
                correct_during_disagreement = "Neither"
        else:
            correct_during_disagreement = None

        current_trial_record[
            "Correct_Participant_During_Disagreement"
        ] = correct_during_disagreement
        current_trial_record["Trial_Status"] = "Individual_Complete"

        # ---------------------------------------------------------------------
        # Joint consensus phase: shown only after both individual responses
        # ---------------------------------------------------------------------
        if individual_responses_complete:
            # ---------------------------------------------------------------------
            # Joint consensus phase: 60 seconds from fixed image offset
            # ---------------------------------------------------------------------
            current_phase = "Joint_Consensus"
            current_trial_record["Trial_Status"] = current_phase
            current_trial_record["Joint_Decision_Required"] = 1

            consensus_reached = False
            consensus_timed_out = False
            consensus_timeout_phase = None
            attempt_count = 0
            final_joint_choice = None
            joint_attempts_detail = []
            failed_attempts_detail = []
            failed_attempts_text = []
            first_joint_onset = None
            joint_deadline_session_s = (
                image_offset_holder["time"] + JOINT_RESPONSE_DEADLINE_S
            )
            current_trial_record[
                "Joint_Deadline_Start_Session_Elapsed_s"
            ] = rounded(image_offset_holder["time"])
            current_trial_record[
                "Joint_Deadline_Session_Elapsed_s"
            ] = rounded(joint_deadline_session_s)

            while not consensus_reached and not consensus_timed_out:
                if session_clock.getTime() >= joint_deadline_session_s:
                    consensus_timed_out = True
                    consensus_timeout_phase = "Before_Joint_Attempt"
                    break

                attempt_count += 1
                current_trial_record["Current_Joint_Attempt"] = attempt_count
                current_trial_record["Current_Joint_P1_Choice"] = None
                current_trial_record["Current_Joint_P1_RT_ms"] = None
                current_trial_record["Current_Joint_P2_Choice"] = None
                current_trial_record["Current_Joint_P2_RT_ms"] = None

                p1_joint_responded = False
                p2_joint_responded = False
                p1_joint_choice = None
                p2_joint_choice = None
                p1_joint_rt_ms = None
                p2_joint_rt_ms = None

                attempt_onset_holder = {}

                def mark_joint_attempt_onset():
                    attempt_onset_time = session_clock.getTime()
                    attempt_onset_holder["time"] = attempt_onset_time

                    if current_trial_record[
                        "Joint_Prompt_First_Onset_Session_Elapsed_s"
                    ] is None:
                        current_trial_record[
                            "Joint_Prompt_First_Onset_Session_Elapsed_s"
                        ] = rounded(attempt_onset_time)
                        current_trial_record[
                            "Joint_Prompt_Delay_After_Image_Offset_ms"
                        ] = seconds_to_ms(
                            attempt_onset_time - image_offset_holder["time"]
                        )

                win.callOnFlip(kb.clearEvents)
                win.callOnFlip(kb.clock.reset)
                win.callOnFlip(mark_joint_attempt_onset)

                # The first attempt uses the green joint prompt. After a
                # mismatch, the red warning becomes the next active response
                # screen immediately; there is no input-blocking delay.
                attempt_is_retry = attempt_count > 1
                seconds_remaining = max(
                    0.0,
                    joint_deadline_session_s - session_clock.getTime(),
                )
                if attempt_is_retry:
                    draw_consensus_warning(
                        completed_trials=index,
                        total_trials=total_trials,
                        seconds_remaining=seconds_remaining,
                    )
                else:
                    draw_joint_interface(
                        prompt=joint_prompt,
                        completed_trials=index,
                        total_trials=total_trials,
                        seconds_remaining=seconds_remaining,
                    )
                win.flip()

                if first_joint_onset is None:
                    first_joint_onset = attempt_onset_holder["time"]

                while not (p1_joint_responded and p2_joint_responded):
                    keys_j = kb.getKeys(
                        keyList=ALL_RESPONSE_KEYS,
                        waitRelease=False,
                        clear=True,
                    )

                    for key_event in keys_j:
                        if key_event.name == ESCAPE_KEY:
                            raise KeyboardInterrupt

                        key_rt_ms = response_rt_ms(key_event, attempt_onset_holder)
                        key_session_time = (
                            attempt_onset_holder["time"] + (key_rt_ms / 1000.0)
                        )
                        if key_session_time > joint_deadline_session_s:
                            continue

                        if key_event.name in P1_KEYS and not p1_joint_responded:
                            p1_joint_choice = get_choice(key_event.name)
                            p1_joint_rt_ms = key_rt_ms
                            p1_joint_responded = True
                            current_trial_record[
                                "Current_Joint_P1_Choice"
                            ] = p1_joint_choice
                            current_trial_record[
                                "Current_Joint_P1_RT_ms"
                            ] = p1_joint_rt_ms
                            emit_joint_response_trigger(
                                participant_number=1,
                                response_count=(
                                    int(p1_joint_responded)
                                    + int(p2_joint_responded)
                                ),
                                response_session_s=key_session_time,
                                response_value=p1_joint_choice,
                                p1_choice=p1_joint_choice,
                                p2_choice=p2_joint_choice,
                                trial_number=index + 1,
                                attempt_number=attempt_count,
                            )

                        if key_event.name in P2_KEYS and not p2_joint_responded:
                            p2_joint_choice = get_choice(key_event.name)
                            p2_joint_rt_ms = key_rt_ms
                            p2_joint_responded = True
                            current_trial_record[
                                "Current_Joint_P2_Choice"
                            ] = p2_joint_choice
                            current_trial_record[
                                "Current_Joint_P2_RT_ms"
                            ] = p2_joint_rt_ms
                            emit_joint_response_trigger(
                                participant_number=2,
                                response_count=(
                                    int(p1_joint_responded)
                                    + int(p2_joint_responded)
                                ),
                                response_session_s=key_session_time,
                                response_value=p2_joint_choice,
                                p1_choice=p1_joint_choice,
                                p2_choice=p2_joint_choice,
                                trial_number=index + 1,
                                attempt_number=attempt_count,
                            )

                    if p1_joint_responded and p2_joint_responded:
                        break

                    seconds_remaining = max(
                        0.0,
                        joint_deadline_session_s - session_clock.getTime(),
                    )
                    if seconds_remaining <= 0.0:
                        consensus_timed_out = True
                        consensus_timeout_phase = "Joint_Response_Attempt"
                        break

                    if (
                        seconds_remaining <= JOINT_COUNTDOWN_START_S
                        and current_trial_record[
                            "Countdown_First_Shown_Session_Elapsed_s"
                        ] is None
                    ):
                        current_trial_record[
                            "Countdown_First_Shown_Session_Elapsed_s"
                        ] = rounded(session_clock.getTime())

                    if attempt_is_retry:
                        draw_consensus_warning(
                            completed_trials=index,
                            total_trials=total_trials,
                            seconds_remaining=seconds_remaining,
                        )
                    else:
                        draw_joint_interface(
                            prompt=joint_prompt,
                            completed_trials=index,
                            total_trials=total_trials,
                            seconds_remaining=seconds_remaining,
                        )
                    win.flip()

                if consensus_timed_out:
                    available_rts = [
                        rt for rt in (p1_joint_rt_ms, p2_joint_rt_ms)
                        if rt is not None
                    ]
                    if p1_joint_rt_ms is not None and p2_joint_rt_ms is None:
                        partial_first_responder = "Participant_1"
                    elif p2_joint_rt_ms is not None and p1_joint_rt_ms is None:
                        partial_first_responder = "Participant_2"
                    else:
                        partial_first_responder = identify_first_responder(
                            p1_joint_rt_ms,
                            p2_joint_rt_ms,
                        )

                    timeout_attempt_record = {
                        "attempt": attempt_count,
                        "attempt_onset_session_elapsed_s": rounded(
                            attempt_onset_holder["time"]
                        ),
                        "p1_choice": p1_joint_choice,
                        "p1_rt_ms": p1_joint_rt_ms,
                        "p2_choice": p2_joint_choice,
                        "p2_rt_ms": p2_joint_rt_ms,
                        "first_responder": partial_first_responder,
                        "attempt_duration_ms": (
                            max(available_rts) if available_rts else None
                        ),
                        "responses_matched": 0,
                        "attempt_completed": 0,
                        "timed_out": 1,
                    }
                    joint_attempts_detail.append(timeout_attempt_record)
                    current_trial_record["Consensus_Attempts"] = attempt_count
                    current_trial_record[
                        "Joint_Attempts_Detail_JSON"
                    ] = json.dumps(joint_attempts_detail, ensure_ascii=False)
                    if current_trial_record["First_Joint_Responder_Overall"] is None:
                        current_trial_record[
                            "First_Joint_Responder_Overall"
                        ] = partial_first_responder
                    break

                attempt_duration_ms = max(p1_joint_rt_ms, p2_joint_rt_ms)
                response_session_time = (
                    attempt_onset_holder["time"] + (attempt_duration_ms / 1000.0)
                )
                attempt_matched = p1_joint_choice == p2_joint_choice
                attempt_first_responder = identify_first_responder(
                    p1_joint_rt_ms,
                    p2_joint_rt_ms,
                )

                attempt_record = {
                    "attempt": attempt_count,
                    "attempt_onset_session_elapsed_s": rounded(
                        attempt_onset_holder["time"]
                    ),
                    "p1_choice": p1_joint_choice,
                    "p1_rt_ms": p1_joint_rt_ms,
                    "p2_choice": p2_joint_choice,
                    "p2_rt_ms": p2_joint_rt_ms,
                    "first_responder": attempt_first_responder,
                    "attempt_duration_ms": attempt_duration_ms,
                    "responses_matched": int(attempt_matched),
                    "attempt_completed": 1,
                    "timed_out": 0,
                }
                joint_attempts_detail.append(attempt_record)

                if current_trial_record["First_Joint_Responder_Overall"] is None:
                    current_trial_record[
                        "First_Joint_Responder_Overall"
                    ] = attempt_first_responder

                current_trial_record["Consensus_Attempts"] = attempt_count
                current_trial_record["Joint_Attempts_Detail_JSON"] = json.dumps(
                    joint_attempts_detail,
                    ensure_ascii=False,
                )

                if attempt_matched:
                    consensus_reached = True
                    final_joint_choice = p1_joint_choice

                    current_trial_record[
                        "Consensus_Reached_Session_Elapsed_s"
                    ] = rounded(response_session_time)
                    current_trial_record["Total_Consensus_Duration_ms"] = seconds_to_ms(
                        response_session_time - first_joint_onset
                    )
                    current_trial_record[
                        "Dyadic_Window_Elapsed_To_Outcome_ms"
                    ] = seconds_to_ms(
                        response_session_time - image_offset_holder["time"]
                    )
                    current_trial_record[
                        "First_Consensus_Responder"
                    ] = attempt_first_responder
                    current_trial_record["Final_Joint_P1_Choice"] = p1_joint_choice
                    current_trial_record["Final_Joint_P1_RT_ms"] = p1_joint_rt_ms
                    current_trial_record["Final_Joint_P2_Choice"] = p2_joint_choice
                    current_trial_record["Final_Joint_P2_RT_ms"] = p2_joint_rt_ms
                    current_trial_record[
                        "Final_Joint_Attempt_Duration_ms"
                    ] = attempt_duration_ms
                    current_trial_record["Final_Joint_Choice"] = final_joint_choice

                else:
                    failed_record = dict(attempt_record)
                    failed_attempts_detail.append(failed_record)
                    failed_attempts_text.append(
                        f"Attempt {attempt_count}: "
                        f"P1={p1_joint_choice} (RT={p1_joint_rt_ms:.3f} ms) | "
                        f"P2={p2_joint_choice} (RT={p2_joint_rt_ms:.3f} ms)"
                    )

                    current_trial_record[
                        "Failed_Consensus_Attempts"
                    ] = len(failed_attempts_detail)
                    current_trial_record[
                        "Failed_Joint_Attempts_RT_JSON"
                    ] = json.dumps(failed_attempts_detail, ensure_ascii=False)
                    current_trial_record["Failed_Attempts_Log"] = "; ".join(
                        failed_attempts_text
                    )

                    # Start the next joint attempt immediately. The red
                    # mismatch prompt is itself the active response screen, so
                    # both participants can respond as soon as they see it.
                    current_phase = "Consensus_Retry"
                    current_trial_record["Trial_Status"] = current_phase

            if consensus_timed_out:
                current_trial_record["Consensus_Time_Limit_Exceeded"] = 1
                current_trial_record[
                    "Consensus_Timeout_Session_Elapsed_s"
                ] = rounded(joint_deadline_session_s)
                current_trial_record[
                    "Consensus_Timeout_Phase"
                ] = consensus_timeout_phase
                current_trial_record[
                    "Dyadic_Window_Elapsed_To_Outcome_ms"
                ] = seconds_to_ms(JOINT_RESPONSE_DEADLINE_S)
                if first_joint_onset is not None:
                    current_trial_record[
                        "Total_Consensus_Duration_ms"
                    ] = seconds_to_ms(
                        joint_deadline_session_s - first_joint_onset
                    )

            # Clear the response display immediately after success or timeout. Disk
            # writing occurs only after the time-critical trial period has ended.
            win.flip()

        else:
            # The individual deadline expired before both responses were
            # received. Do not show the green discussion prompt and do not
            # collect a joint response on this trial.
            current_phase = "Individual_Response_Timeout"
            current_trial_record["Trial_Status"] = current_phase
            current_trial_record["Joint_Decision_Required"] = 0
            consensus_reached = False
            consensus_timed_out = False
            consensus_timeout_phase = None
            attempt_count = 0
            final_joint_choice = None
            joint_attempts_detail = []
            failed_attempts_detail = []
            failed_attempts_text = []
            first_joint_onset = None
            win.flip()

        # ---------------------------------------------------------------------
        # Final trial-level derived variables
        # ---------------------------------------------------------------------
        if final_joint_choice is None or not individual_responses_complete:
            group_followed = None
        elif initial_agreement == 1:
            group_followed = "Initial_Agreement"
        elif final_joint_choice == p1_indiv_choice:
            group_followed = "Participant_1"
        elif final_joint_choice == p2_indiv_choice:
            group_followed = "Participant_2"
        else:
            group_followed = "Neither"

        current_trial_record["Group_Followed"] = group_followed
        current_trial_record["Joint_Correct"] = (
            int(final_joint_choice == auth_label)
            if final_joint_choice is not None
            else None
        )
        current_trial_record["Failed_Consensus_Attempts"] = len(
            failed_attempts_detail
        )
        current_trial_record["Failed_Joint_Attempts_RT_JSON"] = json.dumps(
            failed_attempts_detail,
            ensure_ascii=False,
        )
        current_trial_record["Failed_Attempts_Log"] = (
            "; ".join(failed_attempts_text)
            if failed_attempts_text
            else "None"
        )
        current_trial_record["Dropped_Frames_Trial"] = int(
            win.nDroppedFrames - trial_dropped_frames_start
        )
        current_trial_record["Dropped_Frames_Cumulative"] = int(
            win.nDroppedFrames
        )
        current_trial_record["Current_Joint_Attempt"] = None
        current_trial_record["Current_Joint_P1_Choice"] = None
        current_trial_record["Current_Joint_P1_RT_ms"] = None
        current_trial_record["Current_Joint_P2_Choice"] = None
        current_trial_record["Current_Joint_P2_RT_ms"] = None
        if not individual_responses_complete:
            current_trial_record["Trial_Status"] = "Completed_Individual_Timeout"
        elif consensus_reached:
            current_trial_record["Trial_Status"] = "Completed"
        else:
            current_trial_record["Trial_Status"] = "Completed_Consensus_Timeout"
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
        atomic_write_trigger_log(
            trigger_rows=trigger_events,
            filename=trigger_log_filename,
        )

        # Insert one catch trial after normal trials 16, 32, and 48. The 48
        # normal-trial loop, numbering, stimulus order, and response logic are
        # otherwise unchanged.
        if (index + 1) in CATCH_AFTER_NORMAL_TRIALS:
            catch_trial_number = (
                CATCH_AFTER_NORMAL_TRIALS.index(index + 1) + 1
            )
            run_catch_trial(
                catch_trial_number=catch_trial_number,
                after_normal_trial=index + 1,
            )

    current_phase = "Experiment_Complete"
    win.callOnFlip(
        emit_trigger,
        "experiment_end",
        phase="Experiment_End",
    )
    draw_progress(total_trials, total_trials)
    thanks_text.draw()
    win.flip()
    core.wait(3.0)

except KeyboardInterrupt:
    experiment_aborted = True
    print("Experiment aborted by the user via the Escape key.")

    # Mark all rows as belonging to a session that ended early.
    for completed_row in trial_results:
        completed_row["Session_Ended_Early"] = 1

    if current_trial_record is not None:
        current_trial_record["Experiment_Aborted_During_Trial"] = 1
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
    experiment_aborted = True
    print(f"Experiment stopped because of an unexpected error: {exc}")

    for completed_row in trial_results:
        completed_row["Session_Ended_Early"] = 1

    if current_trial_record is not None:
        current_trial_record["Experiment_Aborted_During_Trial"] = 1
        current_trial_record["Session_Ended_Early"] = 1
        current_trial_record["Abort_Phase"] = current_phase
        current_trial_record["Abort_Reason"] = repr(exc)
        current_trial_record["Trial_Status"] = "Error"
        current_trial_record["Trial_Completed"] = 0
        current_trial_record["Trial_End_ISO"] = iso_now()
        current_trial_record["Trial_End_Session_Elapsed_s"] = rounded(
            session_clock.getTime()
        )

    # The finally block writes the latest checkpoint before the exception is
    # re-raised, preserving the original traceback for debugging.
    raise

finally:
    # A final atomic save preserves all completed trials and the latest partial
    # state of an interrupted trial.
    if trial_results or current_trial_record is not None:
        atomic_write_csv(
            completed_rows=trial_results,
            filename=data_filename,
            current_row=current_trial_record,
        )
        print(f"Data saved to: {data_filename}")

    if trigger_events:
        atomic_write_trigger_log(
            trigger_rows=trigger_events,
            filename=trigger_log_filename,
        )
        print(f"Trigger log saved to: {trigger_log_filename}")

    win.close()
    core.quit()
