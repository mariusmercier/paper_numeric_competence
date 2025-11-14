
# ---- Load packages and Data ----
library(here)
library(tidyverse)
library(parallel)
library(pbapply)
library(tibble)

S1_long_data <- read_csv(here::here("study_1", "data", "clean", "S1_long_data.csv"))
# Import necessary functions from pre-reg
source(here::here("pre-registered code", "pre-reg_model_implementation.R"))

# Define constants
delta <- 0.005
COMP <- seq(-4, 4, by = delta)

# ---- Prepare data for model fitting ----
S1_bayes_data <- S1_long_data %>%
  group_by(question_observed, question_evaluated, success_observed) %>%
  summarise(
    observed_q_perceived_difficulty_unbounded = first(observed_q_perceived_difficulty_unbounded),
    new_q_perceived_difficulty_unbounded = first(new_q_perceived_difficulty_unbounded),
    .groups = "drop"
  )

# ---- Model fitting functions ----

S1_find_LLMax_optim <- function(S1_long_data, bayes_data, n_starts = 3) {
  objective_function <- function(params) {
    mu <- params[1]
    sigma <- params[2]
    noise <- params[3]
    beta <- params[4]
    
    prior_COMP <- dnorm(COMP, mu, sigma)
    prior_COMP <- prior_COMP / sum(prior_COMP * delta)
    
    bayes_data$bayes_eval <- mapply(
      function(obs_diff, new_diff, success_observed) {
        update_and_predict_p_success(
          prior = prior_COMP,
          observed_q_perceived_difficulty_unbounded = obs_diff,
          new_q_perceived_difficulty_unbounded = new_diff,
          success = success_observed,
          noise = noise,
          beta = beta
        )
      },
      bayes_data$observed_q_perceived_difficulty_unbounded,
      bayes_data$new_q_perceived_difficulty_unbounded,
      bayes_data$success_observed
    )
    
    epsilon <- 1e-10
    bayes_data$bayes_eval <- pmin(pmax(bayes_data$bayes_eval, epsilon), 1 - epsilon)
    
    S1_long_data <- S1_long_data %>%
      left_join(bayes_data %>% select(question_observed, question_evaluated, success_observed, bayes_eval),
                by = c("question_observed", "question_evaluated", "success_observed")
      )
    
    if (anyNA(S1_long_data$bayes_eval)) {
      stop(paste0("bayes_data contains NA in bayes_eval. Parameters are: mu: ", mu, ", sigma: ", sigma, ", noise: ", noise, ", beta: ", beta))
    }
    
    S1_long_data <- S1_long_data %>%
      mutate(log_likelihoods = human_evaluation * log(bayes_eval) + (1 - human_evaluation) * log(1 - bayes_eval))
    
    -sum(S1_long_data$log_likelihoods)
  }
  
  lower_bounds <- c(-4, 0.1, 0.1, 0)
  upper_bounds <- c(4, 4, 4, 10)
  
  best_value <- Inf
  best_params <- NULL
  
  for (start_ix in seq_len(n_starts)) {
    init_params <- c(
      mu = runif(1, min = lower_bounds[1], max = upper_bounds[1]),
      sigma = runif(1, min = lower_bounds[2], max = upper_bounds[2]),
      noise = runif(1, min = lower_bounds[3], max = upper_bounds[3]),
      beta = runif(1, min = lower_bounds[4], max = upper_bounds[4])
    )
    
    optim_result <- optim(
      par = init_params,
      fn = objective_function,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(trace = 1, maxit = 100)
    )
    
    if (optim_result$value < best_value) {
      best_value <- optim_result$value
      best_params <- optim_result$par
    }
  }
  
  LLMax <- -best_value
  return(list(LLMax = LLMax, best_params = best_params))
}

S1_find_LLMax_alternative <- function(S1_long_data, bayes_data, n_starts = 10) {
  objective_function <- function(params) {
    beta <- params[1]
    
    COMP_null <- rep(1, length(COMP))
    COMP_null <- COMP_null / sum(COMP_null * delta)
    
    bayes_data$bayes_eval <- mapply(
      function(obs_diff, new_diff, success_observed) {
        update_and_predict_p_success(
          prior = COMP_null,
          observed_q_perceived_difficulty_unbounded = obs_diff,
          new_q_perceived_difficulty_unbounded = new_diff,
          success = success_observed,
          noise = 0,
          beta = beta
        )
      },
      bayes_data$observed_q_perceived_difficulty_unbounded,
      bayes_data$new_q_perceived_difficulty_unbounded,
      bayes_data$success_observed
    )
    
    epsilon <- 1e-10
    bayes_data$bayes_eval <- pmin(pmax(bayes_data$bayes_eval, epsilon), 1 - epsilon)
    
    S1_long_data <- S1_long_data %>%
      left_join(bayes_data %>% select(question_observed, question_evaluated, success_observed, bayes_eval),
                by = c("question_observed", "question_evaluated", "success_observed")
      )
    
    S1_long_data <- S1_long_data %>%
      mutate(log_likelihoods = human_evaluation * log(bayes_eval) + (1 - human_evaluation) * log(1 - bayes_eval))
    
    -sum(S1_long_data$log_likelihoods)
  }
  
  lower_bounds <- c(0)
  upper_bounds <- c(10)
  
  best_value <- Inf
  best_params <- NULL
  
  for (start_ix in seq_len(n_starts)) {
    init_params <- c(beta = runif(1, min = lower_bounds[1], max = upper_bounds[1]))
    
    optim_result <- optim(
      par = init_params,
      fn = objective_function,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(trace = 1, maxit = 100)
    )
    
    if (optim_result$value < best_value) {
      best_value <- optim_result$value
      best_params <- optim_result$par
    }
  }
  
  LLMax <- -best_value
  return(list(LLMax = LLMax, best_params = best_params))
}

S1_find_LLMax_alternative_2 <- function(S1_long_data, bayes_data, n_starts = 25) {
  objective_function <- function(params) {
    beta <- params[1]
    diff <- params[2]
    
    COMP_null <- rep(1, length(COMP))
    COMP_null <- COMP_null / sum(COMP_null * delta)
    
    bayes_data$bayes_eval <- mapply(
      function(obs_diff, new_diff, success, diff, beta) {
        if (success == 1) {
          soft_max(ifelse(obs_diff + diff >= new_diff, 1, 0), beta)
        } else {
          soft_max(ifelse(obs_diff - diff >= new_diff, 1, 0), beta)
        }
      },
      bayes_data$observed_q_perceived_difficulty_unbounded,
      bayes_data$new_q_perceived_difficulty_unbounded,
      bayes_data$success_observed,
      MoreArgs = list(diff = diff, beta = beta)
    )
    
    epsilon <- 1e-10
    bayes_data$bayes_eval <- pmin(pmax(bayes_data$bayes_eval, epsilon), 1 - epsilon)
    
    S1_long_data <- S1_long_data %>%
      left_join(bayes_data %>% select(question_observed, question_evaluated, success_observed, bayes_eval),
                by = c("question_observed", "question_evaluated", "success_observed")
      )
    
    S1_long_data <- S1_long_data %>%
      mutate(log_likelihoods = human_evaluation * log(bayes_eval) + (1 - human_evaluation) * log(1 - bayes_eval))
    
    -sum(S1_long_data$log_likelihoods)
  }
  
  lower_bounds <- c(beta = 0, diff = 0.01)
  upper_bounds <- c(beta = 10, diff = 5)
  
  best_obj <- Inf
  best_params <- NULL
  
  for (start_ix in seq_len(n_starts)) {
    init_params <- c(
      beta = runif(1, min = lower_bounds[1], max = upper_bounds[1]),
      diff = runif(1, min = lower_bounds[2], max = upper_bounds[2])
    )
    
    optim_result <- optim(
      par = init_params,
      fn = objective_function,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 100, trace = 1)
    )
    
    if (optim_result$value < best_obj) {
      best_obj <- optim_result$value
      best_params <- optim_result$par
    }
  }
  
  LLMax <- -best_obj
  return(list(LLMax = LLMax, best_params = best_params))
}

# ---- Run model fitting ----
S1_MAIN_LLMax <- S1_find_LLMax_optim(S1_long_data, S1_bayes_data)
S1_NULL_LLMax <- S1_find_LLMax_alternative(S1_long_data, S1_bayes_data)
S1_NULL_2_LLMax <- S1_find_LLMax_alternative_2(S1_long_data, S1_bayes_data)


# ---- INDIVIDUAL MODEL FITTING ----

library(parallel)
library(pbapply)

S1_cl <- makeCluster(detectCores())

clusterEvalQ(S1_cl, {
 library(tidyverse)
})

clusterExport(S1_cl, c("S1_long_data", "S1_bayes_data", "S1_find_LLMax_optim", "S1_find_LLMax_alternative", "S1_find_LLMax_alternative_2", "update_and_predict_p_success", "updating", "COMP", "delta", "compute_p_success", "soft_max"))


S1_prolific_ids <- unique(S1_long_data$prolific_pid)

S1_results_list <- pblapply(
 S1_prolific_ids,
 cl = S1_cl,
 FUN = function(pid) {
   pdata <- S1_long_data[S1_long_data$prolific_pid == pid, ]

   p_main_LLMax <- S1_find_LLMax_optim(pdata, S1_bayes_data)
   p_null_LLMax <- S1_find_LLMax_alternative(pdata, S1_bayes_data)
   p_null_2_LLMax <- S1_find_LLMax_alternative_2(pdata, S1_bayes_data)

   data.frame(
    prolific_pid = pid,
     MAIN_LLMax = p_main_LLMax$LLMax,
     MAIN_mu = p_main_LLMax$best_params["mu"],
     MAIN_sigma = p_main_LLMax$best_params["sigma"],
     MAIN_noise = p_main_LLMax$best_params["noise"],
     MAIN_beta = p_main_LLMax$best_params["beta"],
     MAIN_BIC = -2 * p_main_LLMax$LLMax + 4 * log(nrow(pdata)),
     NULL_LLMax = p_null_LLMax$LLMax,
     NULL_beta = p_null_LLMax$best_params["beta"],
     NULL_BIC = -2 * p_null_LLMax$LLMax + 1 * log(nrow(pdata)),
     NULL_2_LLMax = p_null_2_LLMax$LLMax,
     NULL_2_beta = p_null_2_LLMax$best_params["beta"],
     NULL_2_diff = p_null_2_LLMax$best_params["diff"],
     NULL_2_BIC = -2 * p_null_2_LLMax$LLMax + 2 * log(nrow(pdata)),
     stringsAsFactors = FALSE
   )
 }
)

stopCluster(S1_cl)
S1_individual_fits <- do.call(rbind, S1_results_list)

# ---- EXPORT FIT RESULTS ----

S1_fit_results <- data.frame(
  model = c("S1_MAIN", "S1_NULL", "S1_NULL_2"),
  LLMax = c(S1_MAIN_LLMax$LLMax, S1_NULL_LLMax$LLMax, S1_NULL_2_LLMax$LLMax),
  mu = c(S1_MAIN_LLMax$best_params["mu"], NA, NA),
  sigma = c(S1_MAIN_LLMax$best_params["sigma"], NA, NA),
  noise = c(S1_MAIN_LLMax$best_params["noise"], NA, NA),
  beta = c(S1_MAIN_LLMax$best_params["beta"], S1_NULL_LLMax$best_params["beta"], S1_NULL_2_LLMax$best_params["beta"]),
  diff = c(NA, NA, S1_NULL_2_LLMax$best_params["diff"])
)

write.csv(S1_fit_results, here::here("study_1", "results", "S1_fit_results.csv"), row.names = FALSE)
write.csv(S1_individual_fits, here::here("study_1", "results", "S1_individual_fits.csv"))








