library(here)
library(tidyverse)
library(dplyr)
library(lme4)
library(car)

data_clean <- read_csv(here("study_2", "S1_reaction_times", "S1_long_data_with_timing.csv"))

# ---- DESCRIPTIVE STATS AND PLOTS ----

# remove question 16 as it is a cheat test
data_clean<- data_clean %>% select(-q16mark, time_on_question16)

# distribution of scores
hist(data_clean$total_score)
print(hist)
summary_total_scores <- summarise(data_clean,
                                  mean_total_score = mean(total_score),
                                  sd_total_score = sd(total_score))



# columns that hold correctness (0/1) for each question
mark_cols <- paste0("q", 1:15, "mark")

# if any of those are character
data_clean <- data_clean %>%
  mutate(across(all_of(mark_cols), ~ as.numeric(.)))

n_participants <- n_distinct(data_clean$participant_id)

# proportion of participants who answered each question correctly
question_correct_props <- data_clean %>%
  summarise(across(all_of(mark_cols), ~ mean(. == 1, na.rm = TRUE))) %>% 
  pivot_longer(
    everything(),
    names_to = "question_raw",
    values_to = "prop_correct"
  ) %>%
  mutate(
    question = sub("mark$", "", question_raw),
    question = factor(question, levels = paste0("q", 1:15))
  ) %>%
  select(question, prop_correct)

# plot
ggplot(question_correct_props, aes(x = question, y = prop_correct, fill = "seagreen")) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Correct Answers per Question",
    x = "Question",
    y = "Proportion correct"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# average time per question bar chart   
average_times <- data_clean %>%
  select(starts_with("time_on_question")) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

# Convert to a long format for ggplot2, where each row is a question and its average time
average_times_long <- average_times %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Average_Time"
  )

# Extract numerical part from question names for proper ordering and cleaner labels
average_times_long <- average_times_long %>%
  mutate(
    Question_Num = as.numeric(gsub("time_on_question_?(\\d+)", "\\1", Question)),
    Question_Label = paste("Q", Question_Num)
  ) %>%
  arrange(Question_Num)

# Plot the bar chart with individual bars for each question
ggplot(average_times_long, aes(x = reorder(Question_Label, Question_Num), y = Average_Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Time Spent per Question (Q1-Q15)",
    x = "Question Number",
    y = "Average Time (seconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------- LONG DATA FOR SCORES AND TIMES -------

scores_long <- data_clean %>%
  select(participant_id, matches("^q\\d+mark$")) %>%
  pivot_longer(
    cols = -participant_id,
    names_to   = "question_num",
    values_to  = "score",
    names_pattern = "^q(\\d+)mark$",
    names_transform = list(question_num = as.integer)
  ) %>%
  distinct()

times_long <- data_clean %>%
  select(participant_id, matches("^time_on_question\\d+$")) %>%
  pivot_longer(
    cols = -participant_id,
    names_to   = "question_num",
    values_to  = "time",
    names_pattern = "^time_on_question(\\d+)$",
    names_transform = list(question_num = as.integer)
  ) %>%
  distinct()

# --- 3) Join them to get: participant_id, question, score, time ---
long_score_time <- inner_join(
  scores_long, times_long,
  by = c("participant_id", "question_num")
) %>%
  mutate(
    question = paste0("q", question_num),
    score = as.numeric(score),
    time  = as.numeric(time)
  ) %>%
  select(participant_id, question, score, time) %>%
  distinct()

# ---- STATISTICAL MODELS OF SCORES ----

# glmer for whether RT predicts correctness per participant, per question
score_by_RT <- glmer(score ~ time + (1 | participant_id), 
                     family = binomial, 
                     data = long_score_time)

summary(score_by_RT)
Anova(score_by_RT, type = "III")
# time on a question has a highly significant effect on whether they get the question right or wrong

fixef(score_by_RT)
# fixed effect of time = -0.148. since beta is negative, longer RTs are associated with lower probability of being correct


exp(fixef(score_by_RT)) # convert from log odds so odds.
# for every 1 second increase in response times, the probability of answering correctly decreases by about 13.8% (1-0.862). 

# glm for whether total time per participant predicts total_score (do faster participants get more right)
# create participant-level summary data
long_score_time <- long_score_time %>% 
  group_by(participant_id) %>%
  mutate(total_score = sum(score), 
         total_time = sum(time, na.rm = TRUE)
  )

participant_summary <- long_score_time %>%
  group_by(participant_id) %>%
  summarise(
    total_time = first(total_time),
    total_score = first(total_score),  # total_score should be same for all rows per participant
    .groups = 'drop'
  )

# Run the model
total_score_by_RT <- glm(total_score ~ total_time, 
                         family = "poisson",  # total_score is count data
                         data = participant_summary)
summary(total_score_by_RT)

#plot total time vs total score
ggplot(participant_summary, aes(x = total_time, y = total_score)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Total Scores vs Total Time per Participant",
    x = "Total Time",
    y = "Total Score"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(100, 380), ylim = c(0, 15))
# --------------------------------------------------------------------------------------------------

# for all pairs of participants who got the same question right, for any question, in what proportion of these paris did the participant who asnwered the question faster get a higher total score?

# Filter only correct responses
correct_responses <- long_score_time %>%
  distinct(participant_id, question, .keep_all = TRUE) %>%
  filter(score == 1) %>%
  select(participant_id, question, time, total_score)


pairwise_all <- correct_responses %>%
  inner_join(correct_responses, by = "question", suffix = c("_a", "_b")) %>%
  filter(participant_id_a < participant_id_b,
         !is.na(time_a), !is.na(time_b))

# order each pair into faster/slower, then classify outcome
proportions_of_pairs <- pairwise_all %>%
  filter(time_a != time_b) %>%                                  # exclude RT ties
  transmute(
    faster_score = if_else(time_a < time_b, total_score_a, total_score_b),
    slower_score = if_else(time_a < time_b, total_score_b, total_score_a),
    outcome = case_when(
      faster_score >  slower_score ~ "faster_better",
      faster_score == slower_score ~ "score_tie",
      TRUE                        ~ "faster_worse"
    )
  ) %>%
  count(outcome) %>%
  mutate(prop = n / sum(n))

proportions_of_pairs


# --------------------------------------------------------------------------------------------------

# ANOTHER MODEL: For each anchor question q measure how fast each participant was relative to peers who got q correct, then test whether being faster on q predicts their correctness on other questions r != q.

# Compute "speed on anchor question q" relative to peers who got q correct
anchor_speed <- long_score_time %>%
  filter(score == 1) %>%                        # only those correct on anchor q
  group_by(question) %>%
  mutate(speed_z = as.numeric(scale(-time))) %>% # higher = faster than peers on q
  ungroup() %>%
  transmute(participant_id,
            anchor_q = question,
            speed_z_anchor = speed_z)

# Build modeling dataset:
#    For each participant and each anchor_q they got correct,
#    predict correctness on every other target_q.
df_relative_speed_model <- long_score_time %>%
  rename(target_q = question,
         target_score = score) %>%
  inner_join(anchor_speed, by = "participant_id") %>%
  filter(target_q != anchor_q)   # exclude the anchor question itself

# 3) Mixed-effects logistic regression:
#    Does "being faster on anchor_q" predict correctness on other questions?
relative_speed_model <- glmer(
  target_score ~ speed_z_anchor + (1 | participant_id) + (1 | target_q) + (1 | anchor_q),
  family = binomial(),
  data   = df_relative_speed_model,
  control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
)

summary(relative_speed_model)






