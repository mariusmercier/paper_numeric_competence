library(tidyverse)
library(janitor)
library(here)
library(dplyr)
options(scipen = 999) #no scientific notation

# ---- READ IN DATA, EXCLUDE PARTICIPANTS, AND CLEAN ----

S1_raw_data <- read_csv(here("study_1", "data", "raw", "S1_raw_data.csv"))

# remove preview trials, and then exclude participants who failed the attention check or are presumed to have cheated
# note: participants who changed the pre-ticked answer when the question observed equals the question evaluated are removed later on
S1_clean_data <- S1_raw_data %>% clean_names() %>% filter(distribution_channel != "preview")
S1_clean_data <- S1_clean_data %>% filter(grepl("2025-07-11", end_date)) # remove preview


S1_clean_data <- S1_clean_data %>% filter(
  grepl("pay", attention_check, ignore.case = TRUE) |
    grepl("attention", attention_check, ignore.case = TRUE)
) # remove attention check fails, N =2

S1_clean_data <- S1_clean_data %>% filter(used_calculator == 0) # N = 28

S1_clean_data <- S1_clean_data %>% filter(q16_532 != 9543306) # N = 7 

S1_clean_data <- clean_names(S1_clean_data)

# select columns and rename question fields
S1_clean_data <- S1_clean_data %>% select(!(q1_537:q16_552) &
                                            !(start_date:free_consent))

S1_clean_data <- S1_clean_data %>% select(x1_judgement_matrix_x1:x30_difficulty_1,
                                          q1_517:q16_532,
                                          sc0:time_on_question16) #select relevant columns
S1_clean_data <- S1_clean_data %>% rename(total_score = sc0)
S1_clean_data$total_score <- as.numeric(S1_clean_data$total_score)
S1_clean_data <- S1_clean_data %>% rename_with( ~ sub("_\\d+$", "", .x), matches("^q\\d+_\\d+$")) # rename question columns

# ---- create long data format ----

# to get data in long format, use labels of judgement matrix and difficulty columns
S1_long_data <- S1_clean_data %>%
  select(matches("^x\\d+_"), prolific_pid, total_score) %>% 
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
S1_long_data <- S1_long_data %>% rename(human_evaluation = judgement_matrix)
S1_long_data <- S1_long_data %>% rename(diff_100_obs = difficulty)

# fill diff_100_obs values for all rows, this is done because each condition = 1 judgement of difficulty
S1_long_data <- S1_long_data %>%
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

# add columns for success and question_observed, based on the condition number 1-30
S1_long_data <- S1_long_data %>%
  mutate(
    success_observed = if_else(
      between(condition, 1, 15),
      1L,
      # success: conditions 1–15
      0L
    ),
    # failure: conditions 16–30
    # map conditions to question numbers: 1→1, 2→2, …15→15, 16→1, 17→2, …30→15 (doubles for each Q because success and failure conditions):
    question_observed = ((condition - 1) %% 15) + 1
  )

# rearrange columns
S1_long_data <- S1_long_data %>%
  select(prolific_pid, question_observed, success_observed, question_evaluated, human_evaluation, everything())


# ---- Exclude participants who remove pre-tick of success/failure on observed question -----
removed_pretick_ids <- S1_long_data %>%
  filter(question_observed == question_evaluated,
         human_evaluation != success_observed) %>%
  distinct(prolific_pid) %>%
  pull(prolific_pid)

# Remove them from the main data
S1_long_data <- S1_long_data %>%
  filter(!prolific_pid %in% removed_pretick_ids) # N = 46

S1_clean_data = S1_clean_data %>% 
  filter(!prolific_pid %in% removed_pretick_ids)

# ---- create wide format ----

S1_wide_answers = S1_clean_data %>% 
  select(!matches("^x\\d+_"))

# create marking columns for q1:q16
answer_key <- c(
  # define answer key
  q1  = 60,
  q2  = 821,
  q3  = 102,
  q4  = 1209,
  q5  = 5917,
  q6  = 4906,
  q7  = 11240,
  q8  = 930,
  q9  = 16100,
  q10 = 100035113,
  q11 = 563240,
  q12 = 1528,
  q13 = 2370000,
  q14 = 1062,
  q15 = 92575,
  q16 = 9543306
)

# add mark columns 
S1_wide_answers <- S1_wide_answers %>%
  mutate(across(
    .cols = q1:q16,
    .fns  = ~ as.integer(.x == answer_key[cur_column()]),
    .names = "{.col}mark"
  ))

#----- Calculate Perceived Difficulty -----

S1_difficulty <- S1_long_data %>%
  group_by(question_observed) %>%
  summarise(perceived_difficulty = 1 - (mean(diff_100_obs) / 100)) %>%
  mutate(unbounded_perceived_difficulty = qlogis(perceived_difficulty))

S1_difficulty <- S1_difficulty %>% rename(question = question_observed)

S1_long_data$observed_q_perceived_difficulty_unbounded <- S1_difficulty$unbounded_perceived_difficulty[match(S1_long_data$question_observed,
                                                                                                                       S1_difficulty$question)]
S1_long_data$new_q_perceived_difficulty_unbounded <- S1_difficulty$unbounded_perceived_difficulty[match(S1_long_data$question_evaluated,
                                                                                                                  S1_difficulty$question)]
S1_long_data$observed_q_perceived_difficulty <- S1_difficulty$perceived_difficulty[match(S1_long_data$question_observed,
                                                                                                   S1_difficulty$question)]
S1_long_data$new_q_perceived_difficulty <- S1_difficulty$perceived_difficulty[match(S1_long_data$question_evaluated,
                                                                                              S1_difficulty$question)]

#----- Calculate Objective Difficulty -----
# inverse of percentage of people who get the question right

S1_objective_difficulty <- S1_wide_answers %>%
  select(prolific_pid, q1mark:q15mark) %>%
  distinct(prolific_pid, .keep_all = TRUE) %>%
  pivot_longer(
    cols      = starts_with("q") & ends_with("mark"),
    names_to  = "question",
    values_to = "mark"
  ) %>%
  # remove the trailing "mark", strip "q", convert to integer
  mutate(
    question = str_remove(question, "mark"),
    question = str_remove(question, "^q"),
    question = as.integer(question)
  ) %>%
  group_by(question) %>%
  summarise(
    objective_difficulty           = 1 - mean(mark),
    unbounded_objective_difficulty = qlogis(objective_difficulty),
    .groups = "drop"
  )


S1_long_data <- S1_long_data %>% select(!diff_100_obs & !condition)


S1_difficulty = left_join(S1_difficulty, S1_objective_difficulty, by = "question")

# ---- Exporting all files ----

write.csv(
  S1_perceived_difficulty,
  here::here("study_1", "data", "clean", "S1_difficulty.csv"),
  row.names = FALSE
)

write.csv(
  S1_long_data,
  here("study_1", "data", "clean", "S1_long_data.csv"),
  row.names = FALSE
)
write.csv(
  S1_wide_answers,
  here("study_1", "data", "clean", "S1_wide_answers.csv"),
  row.names = FALSE
)

