# ---- Model Functions ----

# 'updating' uses Bayes' rule to update the Ideal Observer's
# posterior estimate of competence based on:
#   - prior:         current distribution over competence
#   - difficulty:    difficulty of the observed question
#   - success:       outcome (1 = success, 0 = failure)
#   - noise:         standard deviation for the likelihood
updating = function(prior, difficulty, success, noise) {
  # Compute the probability of success for each competence level in COMP
  # under a Normal(0, noise) assumption around (C - difficulty).
  likelihood = pnorm(COMP - difficulty, mean = 0, sd = noise)
  
  # If we observed a failure, flip that probability to get P(failure)
  if (success == 0) {
    likelihood = 1 - likelihood
  }
  
  # Multiply prior by likelihood to get an unnormalized posterior
  posterior_unnormalized = likelihood * prior
  
  # Normalize so that the sum over all competence values = 1
  posterior = posterior_unnormalized / (sum(posterior_unnormalized) * delta)
  
  return(posterior)
}

# 'compute_p_success' calculates the predicted probability of success
# on a question of a given difficulty, given a posterior distribution
# over competence and the noise parameter.
compute_p_success = function(posterior, perceived_difficulty, noise) {
  likelihood = pnorm(COMP - perceived_difficulty, mean = 0, sd = noise)
  weighted_likelihood = posterior * likelihood
  p_success = sum(weighted_likelihood) * delta
  return(p_success)
}

# 'soft_max' transforms a probability of success into a choice probability
# using the softmax rule, parameterized by beta:
#   - larger beta => more deterministic choices
#   - smaller beta => more random choices
soft_max = function(p_success, beta) {
  numerator = exp(beta * p_success)
  denominator = numerator + exp(beta * (1 - p_success))
  soft_p_success = numerator / denominator
  return(soft_p_success)
}

# 'update_and_predict_p_success' first updates the posterior for the agent's
# competence after observing success/failure on a question of difficulty
# 'observed_q_difficulty'. Then it computes (and softmax-transforms) the
# predicted success probability for a new question of difficulty
# 'new_q_difficulty'.
update_and_predict_p_success = function(prior,
                                        observed_q_perceived_difficulty_unbounded,
                                        new_q_perceived_difficulty_unbounded,
                                        success,
                                        noise,
                                        beta) {
  # 1) Update the posterior distribution of competence
  virtual_agent = updating(prior, observed_q_perceived_difficulty_unbounded, success, noise)
  
  # 2) Predict success probability on the new question
  p_success = compute_p_success(virtual_agent, new_q_perceived_difficulty_unbounded, noise)
  
  # 3) Apply softmax to model stochastic choice
  soft_p_success = soft_max(p_success, beta)
  
  return(soft_p_success)
}