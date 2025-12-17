library(tidyverse)
library(here)
library(janitor)
library(dplyr)

# ---- READ IN DATA, EXCLUDE PARTICIPANTS, AND CLEAN ----

S2_raw_data <- read_csv(here("study_2", "data", "raw", "S2_raw_data_15_12_2025.csv"))

# remove preview trials, and then exclude participants who failed the attention check or are presumed to have cheated
S2_clean_data <- S2_raw_data %>% clean_names() %>% filter(distribution_channel != "preview")

# no one failed the attention check after manual checking

S2_clean_data <- S2_clean_data %>% filter(used_calculator == 0) # N = 18

S2_clean_data <- S2_clean_data %>% filter(answer1000 != 9543306) # N = 25

S2_clean_data <- S2_clean_data %>% filter(q_recaptcha_score > 0.7) # N = 7


# select columns and rename question fields
# S2_clean_data <- S2_clean_data %>% select(!(q1_537:q16_552) &
#                                             !(start_date:free_consent))

# S2_clean_data <- S2_clean_data %>% select(x1_judgement_matrix_x1:x30_difficulty_1,
#                                           q1_517:q16_532,
#                                           sc0:time_on_question16) #select relevant columns
S2_clean_data <- S2_clean_data %>% rename(total_score = sc0)
S2_clean_data$total_score <- as.numeric(S2_clean_data$total_score)

# S2_clean_data <- S2_clean_data %>% rename_with( ~ sub("_\\d+$", "", .x), matches("^q\\d+_\\d+$")) # rename question columns

# ---- create long data format ----

# create a mapping with stim_seen
stims = S2_raw_data[1,] %>%
  select(contains("seen")) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  mutate(
    value=gsub(" - stim_seen", "", value),
    name=gsub("_stim_seen", "", name),
  )

# to get data in long format, use labels of judgement matrix and difficulty columns
S2_long_data <- S2_clean_data %>%
  select(contains("judgement"), contains("difficulty"), prolific_pid, total_score) %>%
  # gather all the x*-judgement & x*-difficulty columns
  pivot_longer(
    cols = matches("^x\\d+_"),
    names_to = c("condition", "measure", "question_evaluated"),
    # break the names like "x12_judgement_matrix_x7" or "x12_difficulty_1" into:
    #   condition = 12
    #   measure   = judgement_matrix  or  difficulty
    #   question_evaluated  = 7    (or 1 for difficulty)
    names_pattern = "^x(\\d+)_(judgement_matrix|difficulty)_x?(\\d+)$",
    values_to = "value"
  ) %>%
  pivot_wider(names_from  = measure, values_from = value) %>%
  # drop rows where both are NA
  filter(!(is.na(judgement_matrix) & is.na(difficulty)))

# rename judgement matrix and difficulty
S2_long_data <- S2_long_data %>% rename(human_evaluation = judgement_matrix)
S2_long_data <- S2_long_data %>% rename(diff_100_obs = difficulty)

# fill diff_100_obs values for all rows, this is done because each condition = 1 judgement of difficulty
S2_long_data <- S2_long_data %>%
  mutate(
    question_evaluated = as.integer(question_evaluated),
    condition = as.integer(condition),
    diff_100_obs = as.integer(diff_100_obs)
  ) %>%
  # make sure question rows are ordered
  arrange(prolific_pid, condition, question_evaluated) %>%
  group_by(prolific_pid, condition) %>%
  # fill the diff_100 value down and then up within each group
  fill(diff_100_obs, .direction = "downup") %>%
  ungroup()

# map condition integer in S2_long_data to actual name using stims df: 
S2_long_data = left_join(S2_long_data, stims %>% mutate(name = as.integer(name)), by = c("condition" = "name")) %>% 
  rename(condition_label = value)

# add success_observed, question_observed, time_observed based on condition_label
S2_long_data = S2_long_data %>% 
  mutate(
    success_observed = if_else(grepl("right", condition_label), 1, 0),
    time_observed = if_else(grepl("fast", condition_label), 1, 0),
    question_observed = str_extract(condition_label, "^[0-9]+\\.[0-9]+") %>%
      str_replace("\\.", "_")
  )

# rename question_evaluated

S2_long_data = S2_long_data %>% 
  mutate(question_evaluated = paste0("1_", question_evaluated))


# rearrange columns
S2_long_data <- S2_long_data %>%
  select(prolific_pid, question_observed, success_observed, time_observed, question_evaluated, human_evaluation, everything())

# ---- create wide format ----

S2_wide_answers = S2_clean_data %>% 
  select(contains("answer"), contains("time"), prolific_pid, quiz_phase_condition)

# create marking columns for q1_1:q2_5
answer_key <- c(
  # define answer key (alternating Quiz 1 and Quiz 2)
  "1_1" = 821,      # Quiz 1: 390 + 431
  "2_1" = 831,      # Quiz 2: 340 + 491
  "1_2" = 4906,     # Quiz 1: 3719 + 1187
  "2_2" = 6807,     # Quiz 2: 3219 + 3588
  "1_3" = 930,      # Quiz 1: 128 + 86 + 716
  "2_3" = 938,      # Quiz 2: 324 + 99 + 515
  "1_4" = 2370000,  # Quiz 1: 870000 + 1500000
  "2_4" = 3490000,  # Quiz 2: 790000 + 2700000
  "1_5" = 92575,    # Quiz 1: 89630 + 2739 + 18 + 188
  "2_5" = 82947     # Quiz 2: 79820 + 2817 + 12 + 298
)

# add mark columns (remove commas from numbers before comparing)
S2_wide_answers <- S2_wide_answers %>%
  mutate(across(
    .cols = matches("^answer[12]_[1-5]$"),
    .fns  = ~ as.integer(as.numeric(str_remove_all(.x, ",")) == answer_key[str_remove(cur_column(), "^answer")]),
    .names = "{.col}_mark"
  ))

# ----- Compute true conditional probabilities P(q_j = 1 | q_i solved/not solved) -----
quiz1_mark_cols <- paste0("answer1_", 1:5, "_mark")
quiz2_mark_cols <- paste0("answer2_", 1:5, "_mark")

# question_observed from Quiz 2
marks_long_obs <- S2_wide_answers %>%
  select(prolific_pid, all_of(quiz2_mark_cols), quiz_phase_condition) %>%
  rename(time_observed = quiz_phase_condition) %>% 
  pivot_longer(
    cols = -c(prolific_pid, time_observed),
    names_to = "question_observed",
    values_to = "observed_mark"
  ) %>%
  mutate(question_observed = str_extract(question_observed, "2_[1-5]"))

# question_evaluated from Quiz 1
marks_long_eval <- S2_wide_answers %>%
  select(prolific_pid, all_of(quiz1_mark_cols)) %>%
  pivot_longer(
    cols = -prolific_pid,
    names_to = "question_evaluated",
    values_to = "evaluated_mark"
  ) %>%
  mutate(question_evaluated = str_extract(question_evaluated, "1_[1-5]"))

S2_prob_df <- marks_long_obs %>%
  inner_join(marks_long_eval, by = "prolific_pid") %>%
  group_by(question_observed, question_evaluated, success_observed = observed_mark, time_observed) %>%
  summarise(
    true_conditional_prob = mean(evaluated_mark, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(question_observed, success_observed, question_evaluated)

#----- Calculate Perceived Difficulty -----

S2_difficulty <- S2_long_data %>%
  group_by(question_observed) %>%
  summarise(perceived_difficulty = 1 - (mean(diff_100_obs) / 100))

S2_difficulty <- S2_difficulty %>% rename(question = question_observed)

S2_long_data$observed_q_perceived_difficulty <- S2_difficulty$perceived_difficulty[match(S2_long_data$question_observed,
                                                                                                   S2_difficulty$question)]

# we did not measure for new question, if needed we can take them from S1 as it is the same stims used
# S2_long_data$new_q_perceived_difficulty <- S2_difficulty$perceived_difficulty[match(S2_long_data$question_evaluated,
#                                                                                               S2_difficulty$question)]

S2_long_data = S2_long_data %>% 
  mutate(
    observed_q_perceived_difficulty_indiv = 1 - (diff_100_obs / 100)
  )


#----- Calculate Objective Difficulty -----
# inverse of percentage of people who get the question right

S2_objective_difficulty <- S2_wide_answers %>%
  select(prolific_pid, all_of(quiz1_mark_cols), all_of(quiz2_mark_cols)) %>%
  distinct(prolific_pid, .keep_all = TRUE) %>%
  pivot_longer(
    cols      = ends_with("_mark"),
    names_to  = "question",
    values_to = "mark"
  ) %>%
  # extract question format (e.g., "1_1" from "answer1_1_mark")
  mutate(question = str_extract(question, "[12]_[1-5]")) %>%
  group_by(question) %>%
  summarise(
    objective_difficulty           = 1 - mean(mark),
    .groups = "drop"
  )


S2_long_data <- S2_long_data %>% select(!diff_100_obs & !condition)

S2_difficulty <- left_join(S2_difficulty, S2_objective_difficulty, by = "question")

S2_long_data$observed_q_objective_difficulty <- S2_difficulty$objective_difficulty[match(S2_long_data$question_observed,
                                                                                         S2_difficulty$question)]
# ---- Create a long format for wide answers matching the structure of evaluated data ----

S2_long_q1 = S2_wide_answers %>%
  select(prolific_pid, all_of(quiz1_mark_cols)) %>% 
  pivot_longer(
    cols = -prolific_pid,
    names_to = "question_evaluated",
    values_to = "success_evaluated"
  ) %>% 
  mutate(question_evaluated = str_extract(question_evaluated, "[12]_[1-5]"))

S2_long_q2 = S2_wide_answers %>% 
  select(prolific_pid, all_of(quiz2_mark_cols), quiz_phase_condition) %>%
  rename(time_observed = quiz_phase_condition) %>% 
  pivot_longer(
    cols = -c(prolific_pid, time_observed),
    names_to = "question_observed",
    values_to = "success_observed"
  ) %>% 
  mutate(question_observed = str_extract(question_observed, "[12]_[1-5]"))

S2_long_answers = left_join(S2_long_q2, S2_long_q1, by = "prolific_pid", relationship = "many-to-many")


# ---- process demographics ----

S2_demo = read_csv(here("study_2", "data", "raw", "S2_demo_15_12_2025.csv"))

S2_demo = S2_demo %>% 
  filter(
    `Participant id` %in% S2_wide_answers$prolific_pid
  )

mean(as.numeric(S2_demo$Age), na.rm = TRUE)
sd(as.numeric(S2_demo$Age), na.rm = TRUE)
table(S2_demo$Sex)

# ---- Exporting all files ----

write.csv(
  S2_difficulty,
  here::here("study_2", "data", "clean", "S2_difficulty.csv"),
  row.names = FALSE
)
write.csv(
  S2_prob_df,
  here::here("study_2", "data", "clean", "S2_prob_df.csv"),
  row.names = FALSE
)

write.csv(
  S2_long_data,
  here("study_2", "data", "clean", "S2_long_data.csv"),
  row.names = FALSE
)

write.csv(
  S2_long_answers,
  here("study_2", "data", "clean", "S2_long_answers.csv"),
  row.names = FALSE
)

