# -------------------------------------------------------------------------
# Script: AI Image Detection - Balanced 48-Image Selection and Validation
# -------------------------------------------------------------------------

# 1. Setup Folders and File Name
input_folder  <- "/Users/mustafacvbe/Desktop/AI_Detection_Stimuli_Sets_and_Code/"
output_folder <- "/Users/mustafacvbe/Desktop/AI_Detection_Stimuli_Sets_and_Code/"
input_file    <- file.path(input_folder, "Expt1_acc_raw_data.xlsx")

if (!require("openxlsx")) install.packages("openxlsx")
if (!require("tidyverse")) install.packages("tidyverse")
library(openxlsx)
library(tidyverse)

# 2. Load and Prepare Raw Data
raw_df <- read.xlsx(input_file, colNames = FALSE, skipEmptyCols = FALSE, skipEmptyRows = FALSE)

# 3. Extract and Reconstruct Headers
row1 <- as.character(raw_df[1, ])
row2 <- as.character(raw_df[2, ])

for (i in 2:length(row1)) {
  if (is.na(row1[i]) || row1[i] == "" || row1[i] == "NA") {
    row1[i] <- row1[i-1]
  }
}
col_names <- paste0(row1, "___", row2)

# 4. Process Data Structure
clean_data <- raw_df[-(1:2), ]
colnames(clean_data) <- col_names
image_cols <- names(clean_data)[str_detect(names(clean_data), "images 1 to 50")]

# 5. Calculate Accuracy per Image
image_summary <- clean_data %>%
  select(all_of(image_cols)) %>%
  mutate(across(everything(), ~as.numeric(as.character(.x)))) %>%
  summarise(across(everything(), list(
    accuracy = ~mean(.x, na.rm = TRUE),
    count    = ~sum(!is.na(.x))
  ))) %>%
  pivot_longer(everything(), names_to = "key", values_to = "value") %>%
  mutate(
    Stat = if_else(str_detect(key, "_accuracy$"), "Accuracy", "Count"),
    Label = str_remove(key, "_accuracy$|_count$")
  ) %>%
  select(-key) %>%
  pivot_wider(names_from = Stat, values_from = value)

# 6. Metadata Extraction
final_df <- image_summary %>%
  mutate(
    Type   = if_else(str_detect(Label, "Real"), "R", "S"),
    Gender = if_else(str_detect(Label, "Female"), "F", "M"),
    Race   = case_when(
      str_detect(Label, "East Asian") ~ "EA",
      str_detect(Label, "South Asian") ~ "SA",
      str_detect(Label, "Black") ~ "B",
      str_detect(Label, "White") ~ "W"
    ),
    Index = str_extract(Label, "\\d+$") %>% as.integer(),
    Filename = paste0(Index, ".", Type, ".", Race, ".", Gender, ".png")
  ) %>%
  select(Filename, Accuracy, Count, Type, Race, Gender)

# -------------------------------------------------------------------------
# STEP 7: DYNAMIC SYMMETRIC TARGET CALCULATION
# -------------------------------------------------------------------------

synth_pool <- final_df %>% filter(Type == "S")

target_hard_acc <- synth_pool %>% 
  slice_min(Accuracy, n = 8, with_ties = FALSE) %>% 
  pull(Accuracy) %>% mean()

target_easy_acc <- synth_pool %>% 
  slice_max(Accuracy, n = 8, with_ties = FALSE) %>% 
  pull(Accuracy) %>% mean()

target_avg_acc <- mean(synth_pool$Accuracy, na.rm = TRUE)

# -------------------------------------------------------------------------
# STEP 8: MEAN-BALANCED STRATIFIED SELECTION
# -------------------------------------------------------------------------

get_perfect_grid_group <- function(df, target, group_label, exclude_list = c()) {
  
  types   <- c("R", "S")
  genders <- c("M", "F")
  races   <- c("W", "B", "SA", "EA")
  
  group_results <- data.frame()
  current_excludes <- exclude_list
  
  for (t in types) {
    for (g in genders) {
      for (r in races) {
        candidates <- df %>%
          filter(Type == t, Gender == g, Race == r, !(Filename %in% current_excludes)) %>%
          mutate(dist = abs(Accuracy - target)) %>%
          slice_min(dist, n = 3, with_ties = FALSE)
        
        has_type_entries <- FALSE
        if (nrow(group_results) > 0 && "Type" %in% colnames(group_results)) {
          if (any(group_results$Type == t)) has_type_entries <- TRUE
        }
        
        if (!has_type_entries) {
          best_match <- candidates %>% slice_min(dist, n = 1, with_ties = FALSE)
        } else {
          type_subset <- group_results %>% filter(Type == t)
          current_sum <- sum(type_subset$Accuracy)
          current_n   <- nrow(type_subset)
          
          candidates <- candidates %>%
            mutate(potential_mean = (current_sum + Accuracy) / (current_n + 1),
                   mean_diff = abs(potential_mean - target))
          
          best_match <- candidates %>% slice_min(mean_diff, n = 1, with_ties = FALSE)
        }
        
        group_results <- bind_rows(group_results, best_match)
        current_excludes <- c(current_excludes, best_match$Filename)
      }
    }
  }
  return(group_results %>% 
           mutate(Group = group_label) %>% 
           select(Filename, Accuracy, Count, Type, Race, Gender, Group))
}

# Execute Initial Selections
hard_48 <- get_perfect_grid_group(final_df, target_hard_acc, "Hard")
avg_48  <- get_perfect_grid_group(final_df, target_avg_acc, "Average", exclude_list = hard_48$Filename)
easy_48 <- get_perfect_grid_group(final_df, target_easy_acc, "Easy", exclude_list = c(hard_48$Filename, avg_48$Filename))

pilot_48_initial <- bind_rows(hard_48, avg_48, easy_48)

# -------------------------------------------------------------------------
# STEP 9: DIAGNOSTIC - IDENTIFYING THE CEILING EFFECT
# -------------------------------------------------------------------------

cat("\n--- DIAGNOSTIC: Investigating Difficulty Ceiling for Synthetic White Males ---\n")

# Identifying problematic image in current selection
problem_img <- pilot_48_initial %>% 
  filter(Group == "Easy", Type == "S", Race == "W", Gender == "M")

cat("Problematic image in 'Easy' category:", problem_img$Filename, 
    "| Accuracy:", round(problem_img$Accuracy, 4), "\n")
cat("Note: This accuracy is closer to the 'Average' target than the 'Easy' target.\n")

# Checking if better candidates exist in the full dataset for this demographic
cat("\nChecking top 5 easiest Synthetic White Males in whole dataset:\n")
final_df %>%
  filter(Type == "S", Gender == "M", Race == "W") %>%
  arrange(desc(Accuracy)) %>%
  slice_head(n = 5) %>%
  select(Filename, Accuracy) %>%
  print()

cat("\nConclusion: Even the easiest Synthetic White Male is approx 47% accurate.")
cat("\nReplacement is required to preserve psychometric validity of the 'Easy' group.\n")

# Searching for high-accuracy Synthetic Male candidates from other races
cat("\nFinding replacement candidates (Synthetic Males, Non-White, not in current set):\n")
already_used <- pilot_48_initial$Filename

candidates <- final_df %>%
  filter(Type == "S", Gender == "M", Race != "W", !(Filename %in% already_used)) %>%
  mutate(dist_to_easy_target = abs(Accuracy - target_easy_acc)) %>%
  arrange(dist_to_easy_target) %>%
  slice_head(n = 5) %>%
  select(Filename, Accuracy, Race, Gender, dist_to_easy_target)

print(candidates)

# -------------------------------------------------------------------------
# STEP 10: REPLACEMENT PROCEDURE & FINAL VALIDATION
# -------------------------------------------------------------------------

# Swapping the outlier for the best accuracy match (36.S.B.M.png)
old_filename <- "37.S.W.M.png"
new_filename <- "36.S.B.M.png"

replacement_data <- final_df %>% 
  filter(Filename == new_filename) %>%
  mutate(Group = "Easy")

pilot_48_final <- pilot_48_initial %>%
  filter(Filename != old_filename) %>%
  bind_rows(replacement_data)

# Save the finalized, validated CSV
write_csv(pilot_48_final, file.path(output_folder, "pilot_stimuli_48_final.csv"))

cat("\n======================================================\n")
cat("      FINAL PILOT 48: STATISTICAL VALIDATION REPORT\n")
cat("      (Replacement: 37.S.W.M.png -> 36.S.B.M.png)\n")
cat("======================================================\n")

# A. Demographic Distribution
cat("\n--- Final Demographic Distribution ---\n")
print(table(pilot_48_final$Gender))
print(table(pilot_48_final$Race))

# B. Final ANOVA & Post-Hoc
cat("\n--- ANOVA: Difficulty Group Separation ---\n")
final_anova <- aov(Accuracy ~ Group, data = pilot_48_final)
print(summary(final_anova))

cat("\n--- Tukey HSD (Post-Hoc Comparisons) ---\n")
print(TukeyHSD(final_anova))

# C. Final T-Tests: Authenticity Balance per Group
cat("\n--- T-Tests: Real vs Synthetic Balance ---\n")
for (g in c("Hard", "Average", "Easy")) {
  cat("\nGroup:", g, "\n")
  group_data <- pilot_48_final %>% filter(Group == g)
  res <- t.test(Accuracy ~ Type, data = group_data)
  cat("p-value:", round(res$p.value, 4), " | t-stat:", round(res$statistic, 3), "\n")
}

# D. Final Mean Accuracy Summary
cat("\n--- Final Mean Accuracy Summary ---\n")
pilot_48_final %>% 
  group_by(Group, Type) %>% 
  summarise(Mean = round(mean(Accuracy), 4), .groups='drop') %>% 
  print()

cat("\n======================================================\n")
cat("SUCCESS: Final balanced list saved as pilot_stimuli_48_final.csv\n")
