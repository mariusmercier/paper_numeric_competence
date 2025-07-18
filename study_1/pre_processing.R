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

S1_excluded_participants <- S1_clean_data %>%
  filter(
    (!grepl("pay", attention_check,ignore.case = TRUE) &
       !grepl("attention", attention_check, ignore.case = TRUE))|
      used_calculator == 1|
      q16_532 == 9543306)

S1_clean_data <- S1_clean_data %>% filter(grepl("pay", attention_check, ignore.case = TRUE) | grepl("attention", attention_check, ignore.case = TRUE)) #remove attention check fails
S1_clean_data <- S1_clean_data %>% filter(used_calculator == 0) #remove cheaters - answer yes to 'used a calc'
S1_clean_data <- S1_clean_data %>% filter(q16_532 != 9543306) # remove cheaters - answered 'impossible' q16 correctly)

S1_clean_data <- clean_names(S1_clean_data)

# select columns, rename, map participant_ids
S1_clean_data <- S1_clean_data %>% select(!(q1_537:q16_552) &!(start_date:free_consent))
S1_clean_data <- S1_clean_data %>% select(x1_judgement_matrix_x1:x30_difficulty_1, q1_517:q16_532, sc0:time_on_question16) #select relevant columns 
S1_clean_data <- S1_clean_data %>% rename(total_score = sc0)
S1_clean_data$total_score <- as.numeric(S1_clean_data$total_score)
S1_clean_data <- S1_clean_data %>% rename_with(~ sub("_\\d+$", "", .x), matches("^q\\d+_\\d+$")) # rename question columns 
S1_clean_data <- S1_clean_data %>% mutate(participant_id = as.integer(factor(prolific_pid)))
S1_clean_data <- S1_clean_data %>% select(!prolific_pid)

# to get data in long format, use labels of judgement matrix and difficulty columns
S1_long_data <- S1_clean_data %>%
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
  pivot_wider(
    names_from  = measure,
    values_from = value
  ) %>%
  # drop rows where both are NA
  filter(!(is.na(judgement_matrix) & is.na(difficulty)))

rm(S1_clean_data)

# rename judgement matrix and difficulty
S1_long_data <- S1_long_data %>% rename(human_evaluation = judgement_matrix)
S1_long_data <- S1_long_data %>% rename(diff_100_obs = difficulty)

# fill diff_100_obs values for all rows
S1_long_data <- S1_long_data %>%
  mutate(
    question_evaluated = as.integer(question_evaluated), 
    condition = as.integer(condition), 
    diff_100_obs = as.integer(diff_100_obs)) %>%
  # make sure question rows are ordered
  arrange(participant_id, condition, question_evaluated) %>%
  group_by(participant_id, condition) %>%
  # fill the diff_100 value down and then up within each group
  fill(diff_100_obs, .direction = "downup") %>%
  ungroup()

# add columns for success and question_observed, based on the condition number 1-30
S1_long_data <- S1_long_data %>%
  mutate(success_observed = if_else(between(condition, 1, 15), 
                      1L,  # success: conditions 1–15
                      0L), # failure: conditions 16–30
    # map conditions to question numbers: 1→1, 2→2, …15→15, 16→1, 17→2, …30→15 (doubles for each Q because success and failure conditions):
    question_observed = ((condition - 1) %% 15) + 1
  )

# rearrange columns 
S1_long_data <- S1_long_data %>%
  select(
    participant_id:question_observed,
    everything()
  )

# create marking columns for q1:q16
answer_key <- c( # define answer key
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
  q13 = 237000,
  q14 = 1026,
  q15 = 92575,
  q16 = 9543306
)
# add mark columns
S1_long_data <- S1_long_data %>%
  mutate(
    across(
      .cols = q1:q16,
      .fns  = ~ as.integer(.x == answer_key[cur_column()]),
      .names = "{.col}mark"
    )
  )

# set data type
S1_long_data$participant_id <- as.numeric(S1_long_data$participant_id)

# ---- Exclude participants who remove pre-tick of success/failure on observed question -----
removed_pretick_ids <- S1_long_data %>%
  filter(question_observed == question_evaluated,
         human_evaluation != success_observed) %>%
  distinct(participant_id) %>%
  pull(participant_id)

removed_pretick <- S1_long_data %>%
  filter(participant_id %in% removed_pretick_ids)

# Remove them from the main data
S1_long_data <- S1_long_data %>%
  filter(!participant_id %in% removed_pretick_ids)

# note: removed_pretick participants added to excluded_participants_long in the next section

#----- Calculate Perceived Difficulty -----

S1_perceived_difficulty <- S1_long_data %>%
  group_by(question_observed) %>%
  summarise(perceived_difficulty = 1 - (mean(diff_100_obs) / 100)) %>%
  mutate(unbounded_perceived_difficulty = qlogis(perceived_difficulty))

S1_perceived_difficulty <- S1_perceived_difficulty %>% rename(question = question_observed)
S1_long_data$observed_q_perceived_difficulty_unbounded <- S1_perceived_difficulty$unbounded_perceived_difficulty[match(S1_long_data$question_observed, S1_perceived_difficulty$question)]
S1_long_data$new_q_perceived_difficulty_unbounded <- S1_perceived_difficulty$unbounded_perceived_difficulty[match(S1_long_data$question_evaluated, S1_perceived_difficulty$question)]
S1_long_data$observed_q_perceived_difficulty <- S1_perceived_difficulty$perceived_difficulty[match(S1_long_data$question_observed, S1_perceived_difficulty$question)]
S1_long_data$new_q_perceived_difficulty <- S1_perceived_difficulty$perceived_difficulty[match(S1_long_data$question_evaluated, S1_perceived_difficulty$question)]


write.csv(S1_perceived_difficulty, here::here("study_1", "data", "clean", "S1_perceived_difficulty.csv"), row.names = FALSE)


#----- Calculate Objective Difficulty ----- 
# inverse of percentage of people who get the question right 

S1_objective_difficulty <- S1_long_data %>%
  select(participant_id, q1mark:q15mark) %>%
  distinct(participant_id, .keep_all = TRUE) %>%
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

S1_long_data$observed_q_objective_difficulty_unbounded <- S1_objective_difficulty$unbounded_objective_difficulty[match(S1_long_data$question_observed, S1_objective_difficulty$question)]
S1_long_data$new_q_objective_difficulty_unbounded <- S1_objective_difficulty$unbounded_objective_difficulty[match(S1_long_data$question_evaluated, S1_objective_difficulty$question)]
S1_long_data$observed_q_objective_difficulty <- S1_objective_difficulty$objective_difficulty[match(S1_long_data$question_observed, S1_objective_difficulty$question)]
S1_long_data$new_q_objective_difficulty <- S1_objective_difficulty$objective_difficulty[match(S1_long_data$question_evaluated, S1_objective_difficulty$question)]


write.csv(S1_objective_difficulty, here::here("study_1", "data", "clean", "S1_objective_difficulty.csv"), row.names = FALSE)

# rearrange columns 
S1_long_data <- S1_long_data %>%
  select(
    participant_id:question_observed,
    observed_q_perceived_difficulty_unbounded:new_q_objective_difficulty,
    everything()
  )
S1_long_data <- S1_long_data %>% select(!diff_100_obs & !condition)
S1_long_data <- S1_long_data %>% select(!q1:q16)

long_data_with_timing <- S1_long_data
S1_long_data <- S1_long_data %>% select(!time_on_question1:time_on_question16)

write.csv(S1_long_data, file = here("study_1", "data", "clean", "S1_long_data.csv"), row.names = FALSE)
write.csv(long_data_with_timing, file = here("study_1", "data", "clean", "long_data_with_timing.csv"), row.names = FALSE)

rm(long_data_with_timing)

#----- CLEANING EXCLUDED PARTICIPANTS DATA -----
S1_excluded_participants <- clean_names(S1_excluded_participants)

#select columns, rename, map participant_ids
S1_excluded_participants <- S1_excluded_participants %>% select(!(q1_537:q16_552) &!(start_date:free_consent)) 
S1_excluded_participants <- S1_excluded_participants %>% select(x1_judgement_matrix_x1:x30_difficulty_1, q1_517:used_calculator, sc0:time_on_question16) #select relevant columns 
S1_excluded_participants <- S1_excluded_participants %>% rename(total_score = sc0)
S1_excluded_participants$total_score <- as.numeric(S1_excluded_participants$total_score)
S1_excluded_participants <- S1_excluded_participants %>% rename_with(~ sub("_\\d+$", "", .x), matches("^q\\d+_\\d+$")) # rename question columns 
S1_excluded_participants <- S1_excluded_participants %>% mutate(participant_id = as.integer(factor(prolific_pid)))
S1_excluded_participants <- S1_excluded_participants %>% select(!prolific_pid)

# to get data in long format, use labels of judgement matrix and difficulty columns
S1_excluded_participants_long <- S1_excluded_participants %>%
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
  pivot_wider(
    names_from  = measure,
    values_from = value
  ) %>%
  # drop rows where both are NA
  filter(!(is.na(judgement_matrix) & is.na(difficulty)))

#rename judgement matrix and difficulty
S1_excluded_participants_long <- S1_excluded_participants_long %>% rename(human_evaluation = judgement_matrix)
S1_excluded_participants_long <- S1_excluded_participants_long %>% rename(diff_100 = difficulty)

S1_excluded_participants_long <- S1_excluded_participants_long %>%
  mutate(
    question_evaluated = as.integer(question_evaluated), 
    condition = as.integer(condition), 
    diff_100 = as.integer(diff_100)) %>%
  # make sure question rows are ordered so fill() works predictably
  arrange(participant_id, condition, question_evaluated) %>%
  group_by(participant_id, condition) %>%
  # fill the diff_100 value down and then up within each group
  fill(diff_100, .direction = "downup") %>%
  ungroup()


# add columns for success and question_observed, based on the condition number 1-30
S1_excluded_participants_long <- S1_excluded_participants_long %>%
  mutate(success_observed = if_else(between(condition, 1, 15), 
                                    1L,  # success: conditions 1–15
                                    0L), # failure: conditions 16–30
         # map conditions to question numbers: 1→1, 2→2, …15→15, 16→1, 17→2, …30→15 (doubles for each Q because success and failure conditions):
         question_observed = ((condition - 1) %% 15) + 1
  )

# rearrange columns 
S1_excluded_participants_long <- S1_excluded_participants_long %>%
  select(
    participant_id:question_observed,
    everything()
  )

# add mark columns
S1_excluded_participants_long <- S1_excluded_participants_long %>%
  mutate(
    across(
      .cols = q1:q16,
      .fns  = ~ as.integer(.x == answer_key[cur_column()]),
      .names = "{.col}mark"
    )
  )

# set data types
S1_excluded_participants_long$participant_id <- as.numeric(S1_excluded_participants_long$participant_id)

# Add removed pretick exclusions to the main exclusion df 
S1_excluded_participants_long <- bind_rows(S1_excluded_participants_long, removed_pretick) %>%
  distinct()

rm(removed_pretick)
rm(S1_excluded_participants)

count_excluded <- S1_excluded_participants_long %>% 
  summarise(count_excluded <- length(unique(S1_excluded_participants_long$participant_id))) # 76 participants excluded


write.csv(S1_excluded_participants_long, file = here("study_1", "data", "clean", "S1_excluded_participants_long.csv"), row.names = FALSE)
