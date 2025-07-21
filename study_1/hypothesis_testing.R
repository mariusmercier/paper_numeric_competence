# ----- 1: LOAD LIBRARIES AND DATA -----------

library(here)
library(tidyverse)
library(lmerTest)

S1_long_data <- read_csv(here::here("study_1", "data", "clean", "S1_long_data.csv"))
S1_fit_results <-  read_csv(here::here("study_1", "results", "S1_fit_results.csv"))
source(here::here("pre-registered code", "pre-reg_model_implementation.R"))


# ----- 2: CREATE DFS FOR WORST PERFOMRERS AND FOR CONDITIONAL PROBABILITIES -----------

# ------ Subset S1_data_long to worst 30% of participants -------
n_participants <- S1_long_data %>% 
  distinct(participant_id) %>% 
  nrow()
n_worst <- floor(n_participants * 0.30)

worst_ids <- S1_long_data %>%
  distinct(participant_id, total_score) %>%
  arrange(total_score) %>%
  slice_head(n = n_worst) %>%
  pull(participant_id)

data_worst_30 <- S1_long_data %>%
  filter(participant_id %in% worst_ids)

#------True and Judged Conditional Probabilities (FULL DATASET)-------

questions <- paste0("q", 1:15, "mark")

# compute true P(q_j=1 | q_i=1) - for success == 1
true_prob_mat_correct <- sapply(questions, function(i) {
  sapply(questions, function(j) {
    mean(S1_long_data[[j]][ S1_long_data[[i]] == 1 ], na.rm=TRUE)
  })
})
diag(true_prob_mat_correct) <- 1
true_df_correct <- as.data.frame(true_prob_mat_correct)
colnames(true_df_correct) <- paste0("given_q", 1:15, "correct")
rownames(true_df_correct) <- paste0("P(q", 1:15, "_correct)")

# compute true P(q_j=1 | q_i=0) - for success == 0
true_prob_mat_incorrect <- sapply(questions, function(i) {
  sapply(questions, function(j) {
    mean(S1_long_data[[j]][ S1_long_data[[i]] == 0 ], na.rm=TRUE)
  })
})
# set diag to NA
diag(true_prob_mat_incorrect) <- NA
true_df_incorrect <- as.data.frame(true_prob_mat_incorrect)
colnames(true_df_incorrect) <- paste0("given_q", 1:15, "incorrect")
rownames(true_df_incorrect) <- paste0("P(q", 1:15, "_correct)")

# create true long by combining correct and incorrect
true_long_correct <- true_df_correct %>%
  as_tibble(rownames="question_evaluated") %>%
  mutate(
    question_evaluated = str_extract(question_evaluated, "\\d+") %>% as.integer(),
    success_observed   = 1
  ) %>%
  pivot_longer(
    cols      = starts_with("given_"),
    names_to  = "given",
    values_to = "true_conditional_prob"
  )

true_long_incorrect <- true_df_incorrect %>%
  as_tibble(rownames="question_evaluated") %>%
  mutate(
    question_evaluated = str_extract(question_evaluated, "\\d+") %>% as.integer(),
    success_observed   = 0
  ) %>%
  pivot_longer(
    cols      = starts_with("given_"),
    names_to  = "given",
    values_to = "true_conditional_prob"
  )

true_long <- bind_rows(true_long_correct, true_long_incorrect)


# create judged long df
judged_long <- S1_long_data %>%
  group_by(success_observed, question_observed, question_evaluated) %>%
  summarise(
    average_judged_conditional_prob = mean(human_evaluation == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    given = paste0("given_q", question_observed,
                   if_else(success_observed == 1, "correct", "incorrect"))
  ) %>%
  select(question_evaluated, success_observed, given, average_judged_conditional_prob)

# combine judged and true into a single df
full_prob_df <- inner_join(true_long, judged_long,
                           by = c("question_evaluated","given", "success_observed")) %>%
  filter(question_evaluated != as.integer(str_extract(given, "(?<=given_q)\\d+"))) %>%
  mutate(
    # pull out question_observed
    question_observed = as.integer(str_extract(given, "(?<=given_q)\\d+"))
  ) %>%
  # reorder columns
  select(success_observed, question_observed, everything())


#------True and Judged Conditional Probabilities (WORST 30% DATASET)-------
# repeat process above to create a true and judged conditional probability table for the worst 30% of participants

questions <- paste0("q", 1:15, "mark")

true_mat_worst_correct <- sapply(questions, function(i) {
  sapply(questions, function(j) {
    mean(data_worst_30[[j]][ data_worst_30[[i]] == 1 ], na.rm = TRUE)
  })
})
diag(true_mat_worst_correct) <- 1
true_df_worst_correct <- as.data.frame(true_mat_worst_correct)
colnames(true_df_worst_correct) <- paste0("given_q", 1:15, "correct")
rownames(true_df_worst_correct) <- paste0("P(q", 1:15, "_correct)")

true_mat_worst_incorrect <- sapply(questions, function(i) {
  sapply(questions, function(j) {
    mean(data_worst_30[[j]][ data_worst_30[[i]] == 0 ], na.rm = TRUE)
  })
})
diag(true_mat_worst_incorrect) <- NA
true_df_worst_incorrect <- as.data.frame(true_mat_worst_incorrect)
colnames(true_df_worst_incorrect) <- paste0("given_q", 1:15, "incorrect")
rownames(true_df_worst_incorrect) <- paste0("P(q", 1:15, "_correct)")

true_long_worst <- bind_rows(
  true_df_worst_correct %>%
    as_tibble(rownames = "question_evaluated") %>%
    mutate(
      question_evaluated = str_extract(question_evaluated, "\\d+") %>% as.integer(),
      success_observed   = 1
    ) %>%
    pivot_longer(
      cols      = starts_with("given_q"),
      names_to  = "given",
      values_to = "true_conditional_prob"
    ),
  
  true_df_worst_incorrect %>%
    as_tibble(rownames = "question_evaluated") %>%
    mutate(
      question_evaluated = str_extract(question_evaluated, "\\d+") %>% as.integer(),
      success_observed   = 0
    ) %>%
    pivot_longer(
      cols      = starts_with("given_q"),
      names_to  = "given",
      values_to = "true_conditional_prob"
    )
)

judged_long_worst <- data_worst_30 %>%
  group_by(success_observed, question_observed, question_evaluated) %>%
  summarise(
    average_judged_conditional_prob = mean(human_evaluation == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    given = paste0("given_q", question_observed,
                   if_else(success_observed == 1, "correct", "incorrect"))
  ) %>%
  select(question_evaluated, success_observed, given, average_judged_conditional_prob)

worst_30_prob_df <- inner_join(
  true_long_worst,
  judged_long_worst,
  by = c("question_evaluated", "success_observed", "given")
) %>%
  filter(question_evaluated != as.integer(str_extract(given, "(?<=given_q)\\d+"))) %>%
  mutate(
    # pull out question_observed
    question_observed = as.integer(str_extract(given, "(?<=given_q)\\d+"))
  )



rm(judged_long, judged_long_worst, true_df_correct, true_df_incorrect, true_df_worst_correct, true_df_worst_incorrect, true_long, true_long_correct, true_long_incorrect, true_long_worst)
rm(true_mat_worst_correct, true_mat_worst_incorrect, true_prob_mat_correct, true_prob_mat_incorrect)


# ---- CREATE SUMMARIZED DF -----
S1_summarized_data <- S1_long_data %>%
  group_by(success_observed, question_observed, question_evaluated) %>%
  summarise(
    sd_judged_conditional_prob                = sd(human_evaluation, na.rm=TRUE),
    observed_q_perceived_difficulty_unbounded = mean(observed_q_perceived_difficulty_unbounded),
    new_q_perceived_difficulty_unbounded      = mean(new_q_perceived_difficulty_unbounded),
    .groups = "drop"
  ) %>%
  filter(question_observed != question_evaluated)

S1_summarized_data <- S1_summarized_data %>%
  left_join(
    full_prob_df %>%
      select(success_observed, question_observed, question_evaluated,
             average_judged_conditional_prob,
             true_conditional_prob),
    by = c("success_observed","question_observed","question_evaluated")
  )


# ------ EXTRACT RESULTS FROM COMPUTATIONAL MODEL  ----

# ---- Compute BICs ----
S1_MAIN_BIC <- -2 * S1_fit_results[S1_fit_results$model == "S1_MAIN", ]$LLMax + 4 * log(nrow(S1_long_data))
S1_NULL_BIC <- -2 * S1_fit_results[S1_fit_results$model == "S1_NULL", ]$LLMax + 1 * log(nrow(S1_long_data))
S1_NULL_2_BIC <- -2 * S1_fit_results[S1_fit_results$model == "S1_NULL_2", ]$LLMax + 2 * log(nrow(S1_long_data))

delta <- 0.005
COMP <- seq(-4, 4, by = delta)

mu <- S1_fit_results[S1_fit_results$model == "S1_MAIN", ]$mu
sigma <- S1_fit_results[S1_fit_results$model == "S1_MAIN", ]$sigma
noise <- S1_fit_results[S1_fit_results$model == "S1_MAIN", ]$noise
beta <- S1_fit_results[S1_fit_results$model == "S1_MAIN", ]$beta

S1_prior_COMP <- dnorm(COMP, mu, sigma)
S1_prior_COMP <- S1_prior_COMP / sum(S1_prior_COMP * delta)


S1_summarized_data$prob_bayes <- mapply(
  FUN = update_and_predict_p_success,
  observed_q_perceived_difficulty_unbounded = S1_summarized_data$observed_q_perceived_difficulty_unbounded,
  new_q_perceived_difficulty_unbounded = S1_summarized_data$new_q_perceived_difficulty_unbounded,
  MoreArgs = list(
    prior = S1_prior_COMP, # Pass entire vector as constant
    success = 1,
    noise = noise,
    beta = beta
  )
)

beta <- S1_fit_results[S1_fit_results$model == "S1_NULL", ]$beta
S1_NULL_COMP <- rep(1, length(COMP))
S1_NULL_COMP <- S1_NULL_COMP / sum(S1_NULL_COMP * delta)

S1_summarized_data$prob_null <- mapply(
  FUN = update_and_predict_p_success,
  observed_q_perceived_difficulty_unbounded = S1_summarized_data$observed_q_perceived_difficulty_unbounded,
  new_q_perceived_difficulty_unbounded = S1_summarized_data$new_q_perceived_difficulty_unbounded,
  MoreArgs = list(
    prior = S1_NULL_COMP,
    success = 1,
    noise = 0,
    beta = beta
  )
)

beta <- S1_fit_results[S1_fit_results$model == "S1_NULL_2", ]$beta
difference <- beta <- S1_fit_results[S1_fit_results$model == "S1_NULL_2", ]$diff

S1_summarized_data$prob_null_2 <- mapply(
  FUN = function(obs, new) {
    soft_max(ifelse(obs + difference >= new, 1, 0), beta = beta)
  },
  obs = S1_summarized_data$observed_q_perceived_difficulty_unbounded,
  new = S1_summarized_data$new_q_perceived_difficulty_unbounded
)

# -------- PRE-REGISTERED ANALYSIS FOR HYPOTHESIS TESTS ----------

model_h1 = lmer(scale(observed_q_objective_difficulty) ~ scale(observed_q_perceived_difficulty) + (1|question_observed), S1_long_data)
model_h2 <- lm(scale(average_judged_conditional_prob) ~ scale(true_conditional_prob), data = S1_summarized_data)
model_h1m = lmer(scale(observed_q_objective_difficulty) ~ scale(observed_q_perceived_difficulty) + (1|participant_id) + (1|question_observed), data_worst_30)
model_h2m = lm(scale(average_judged_conditional_prob) ~ scale(true_conditional_prob), worst_30_prob_df)
bayes_corr_h3 <- cor.test(S1_summarized_data$average_judged_conditional_prob, S1_summarized_data$prob_bayes)

summary(model_h1)
#for H4: S1_MAIN_BIC < S1_NULL_BIC 
# for H5: S1_MAIN_BIC < S1_NULL_2_BIC

# for h1 and h1m, lmer not converging. this is because the random effect for question_observed
#is highly correlated with the fixed effect for perceived question difficulty 
#(in fact the fixed effect is constant for any value of question_observed) 

#create adjusted models which exclude random_effects for question_observed
model_h1adjusted <- lmer(scale(observed_q_objective_difficulty) ~ scale(observed_q_perceived_difficulty) + (1|participant_id),
                data = S1_long_data)
summary(model_h1adjusted)

model_h1madjusted = lmer(scale(observed_q_objective_difficulty) ~ scale(observed_q_perceived_difficulty) + (1|participant_id), data_worst_30)
summary(model_h1madjusted)


# ----- EXPLORATORY RESEARCH QUESTIONS ---------

model_bayes <- lm(scale(average_judged_conditional_prob) ~ scale(prob_bayes), data = S1_summarized_data)
model_null1 <- lm(scale(average_judged_conditional_prob) ~ scale(prob_null), data = S1_summarized_data)
model_null2 <- lm(scale(average_judged_conditional_prob) ~ scale(prob_null_2), data = S1_summarized_data)

# research question 1 
rq1 <- S1_summarized_data %>%
  group_by(success_observed) %>%
  summarise(
    cor = cor(average_judged_conditional_prob, true_conditional_prob, use = "complete.obs"),
    p.value = cor.test(average_judged_conditional_prob, true_conditional_prob)$p.value,
    n.cells = n(),
    .groups = "drop"
  )

# rq2 and rq3 use individual level data (supercomputer)

# other correlations outside hypotheses
null_cor <- cor.test(S1_summarized_data$average_judged_conditional_prob, S1_summarized_data$prob_null)
null2_cor <- cor.test(S1_summarized_data$average_judged_conditional_prob, S1_summarized_data$prob_null_2)
prob_corr <- cor.test(S1_summarized_data$true_conditional_prob, S1_summarized_data$average_judged_conditional_prob)
diff_corr <- cor.test(S1_long_data$observed_q_perceived_difficulty_unbounded, S1_long_data$observed_q_objective_difficulty_unbounded)


# ------ Manipulation Checks --------

# for MC1 see 'nestedness.R'

# MC1' analysis below

# Calculate average performance score
S1_long_data$average_performance_score <- S1_long_data$total_score / 15

# Select relevant columns and melt the qxmark columns to long format
mc_data <- S1_long_data %>%
  select(participant_id, average_performance_score, question_observed, observed_q_objective_difficulty, paste0("q", 1:15, "mark")) %>%
  pivot_longer(
    cols = starts_with("q") & ends_with("mark"),
    names_to = "question_number_str",
    values_to = "correctness"
  )

# Extract the numerical question ID from the 'question_number_str' (e.g., "q1mark" becomes 1)
mc_data <- mc_data %>%
  mutate(question_number = as.integer(gsub("q([0-9]+)mark", "\\1", question_number_str)))

mc_data <- mc_data %>%
  filter(question_number == question_observed, correctness == 1)

# Group the filtered data by both 'question_observed' and 'observed_q_objective_difficulty'
# Then, calculate the mean of 'average_performance_score' for each unique question/difficulty pair.
mc_data <- mc_data %>%
  group_by(question_observed, observed_q_objective_difficulty) %>%
  summarise(average_performance_score = mean(average_performance_score, na.rm = TRUE), .groups = 'drop') %>% # Added .groups = 'drop' for cleaner output
  arrange(observed_q_objective_difficulty) # Arrange by difficulty for better readability

# Run  linear model
model_mc <- lm(observed_q_objective_difficulty ~ average_performance_score, data = mc_data)
print(summary(model_mc))

plot_mc <- ggplot(mc_data, aes(x = average_performance_score, y = observed_q_objective_difficulty)) +
  geom_point(color = "darkgreen", size = 3) + # Original scatter points
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") + # Linear regression line
  labs(
    title = "Question Difficulty vs. Average Performance Score (for Correct Answers)",
    x = "Average Performance Score (of those who got it right)",
    y = "Objective Question Difficulty"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot_mc

# count of participants who got each q correct
q_marks_long <- S1_long_data %>%
  select(participant_id, starts_with("q") & ends_with("mark")) %>%
  select(participant_id, paste0("q", 1:15, "mark")) # Explicitly select q1mark to q15mark

q_marks_long <- q_marks_long %>%
  pivot_longer(
    cols = -participant_id, # Pivot all columns except participant_id
    names_to = "question_column",
    values_to = "correctness"
  )

# Filter for correct answers (where correctness == 1)
correct_answers <- q_marks_long %>%
  filter(correctness == 1)

# Count unique participants for each question
# Group by the 'question_column' and then count the number of distinct 'participant_id's
participants_per_question <- correct_answers %>%
  group_by(question_column) %>%
  summarise(unique_participants_correct = n_distinct(participant_id)) %>%
  ungroup() %>%
  arrange(question_column)

print(participants_per_question)



