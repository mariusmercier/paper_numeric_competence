#### 1.2. NESTEDNESS ANALYSIS ####

# Adapted from Olivier Morin

# 1.2.0. Loading the data & packages #####

#Packages:
library(tidyverse)
library(vegan) #for our two nestedness meaasures
library(dplyr)
# Ensure bipartite is NOT loaded here to avoid conflicts with vegan.
# It will be loaded later specifically for plotting.


# Define nsimul (number of simulated matrices)
nsimul <- 500

# 1.2.1. Constituting the datasets #######
data_clean <- read.csv("data_clean.csv")

# make prolific ID numeric
pid_map <- data_clean %>%
  distinct(prolific_pid) %>%
  arrange(prolific_pid) %>%
  mutate(id = row_number())

# Step 2: Join back to original data
data_scored <- data_clean %>%
  left_join(pid_map, by = "prolific_pid") %>%
  relocate(id, .before = 1) %>%
  select(id, q1:q15)

data_scored <- data_scored %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "question",
    values_to = "answer"
  )

# to do scoring, create a df of correct answers, join dfs,
correct_answers <- data.frame(
  question = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15"),
  correct = c(60, 91, 820, 102, 1209, 5917, 4906, 11240, 930,
              16100, 2370000, 563240, 1528, 92575, 9543306)
)

data_scored <- data_scored %>%
  left_join(correct_answers, by = "question")

data_scored <- data_scored %>%
  mutate(score = if_else(answer == correct, 1, 0))

# We need to make sure that our data is a binary numeric matrix - this will be crucial later to build the simulated matrices
data_nesting <- data_scored %>%
  select(id,question,score) %>%
  pivot_wider(names_from = question, values_from = score)

data_nesting <- data_nesting %>% select(!id) %>% as.matrix()
data_nesting <- apply(data_nesting, 2, as.integer) # Ensure all values are integers (0 or 1)

#  Measuring nestedness #### 

# Define single observed NODF value and Temperature value for consistent display
# These are calculated once directly from the data_nesting matrix.
observed_nodf_value <- nestednodf(data_nesting)$statistic['NODF']
observed_temperature_value <- nestedtemp(data_nesting)$statistic

# Bootstrapping analysis ####

#Are nestedness measures higher / lower than the simulated baselines?
#Some reminders:
# *Low* temperature indicates *high* nestedness, whereas high nodf => high nestedness
# Let alpha be 0.005
# It's not abnormal for a bootstrapped p value to reach exactly 0.
# oecosimu automates the same process as the manual bootstrapping carried out in Dubourg et al., 2025

set.seed(123)

result_nodf_r1 <- oecosimu(data_nesting, nestednodf, "r1", nsimul = nsimul)
result_nodf_curveball <- oecosimu(data_nesting, nestednodf, "curveball", nsimul = nsimul)
result_temp_r1 <- oecosimu(data_nesting, nestedtemp, "r1", nsimul = nsimul)
result_temp_curveball <- oecosimu(data_nesting, nestedtemp, "curveball", nsimul = nsimul)

# Create plots
png("nestedness_density_plots.png", width = 12, height = 8, units = "in", res = 300)
# Set up a 2x2 plotting area, with adjusted margins for titles and labels
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1) + 0.1, oma = c(0,0,2,0)) # oma for overall title

# 1. NODF - R1 (Density)
nodf_r1_simulated_values <- result_nodf_r1$oecosimu$simulated[3,] # Access the NODF value
plot(density(nodf_r1_simulated_values),
     main = "NODF Distribution - R1 Null Model",
     xlab = "NODF Values",
     ylab = "Density",
     col = "blue", lwd = 2,
     # Ensure x-axis range includes observed value, extended slightly
     xlim = range(c(nodf_r1_simulated_values, observed_nodf_value)) * c(0.98, 1.02)
)
polygon(density(nodf_r1_simulated_values),
        col = rgb(0, 0, 1, 0.3), border = "blue")
abline(v = observed_nodf_value, # Use the single consistent value
       col = "red", lwd = 3, lty = 1)
# Add label for observed value, adjust position as needed
text(x = observed_nodf_value,
     y = max(density(nodf_r1_simulated_values)$y) * 0.95, # Place label near the peak
     labels = paste0("Observed = ", round(observed_nodf_value, 1)), # Rounded for display
     col = "red", pos = 2, cex = 0.8) # pos=4 for right of point

# 2. NODF - Curveball (Density)
nodf_curveball_simulated_values <- result_nodf_curveball$oecosimu$simulated[3,] # Access the NODF value
plot(density(nodf_curveball_simulated_values),
     main = "NODF Distribution - Curveball Null Model",
     xlab = "NODF Values",
     ylab = "Density",
     col = "green", lwd = 2,
)
polygon(density(nodf_curveball_simulated_values),
        col = rgb(0, 1, 0, 0.3), border = "green")
abline(v = observed_nodf_value, # Use the single consistent value
       col = "red", lwd = 3, lty = 1)
# Add label for observed value, adjust position as needed
text(x = observed_nodf_value,
     y = max(density(nodf_curveball_simulated_values)$y) * 0.95, # Place label near the peak
     labels = paste0("Observed = ", round(observed_nodf_value, 1)), # Rounded for display
     col = "red", pos = 2, cex = 0.8)


# 3. Temperature - R1 (Density)
temp_r1_simulated_values <- result_temp_r1$oecosimu$simulated
plot(density(temp_r1_simulated_values),
     main = "Temperature Distribution - R1 Null Model",
     xlab = "Temperature Values",
     ylab = "Density",
     col = "coral", lwd = 2,
     # Ensure x-axis range includes observed value, extended slightly
     xlim = range(c(temp_r1_simulated_values, observed_temperature_value)) * c(0.98, 1.02)
)
polygon(density(temp_r1_simulated_values),
        col = rgb(1, 0.5, 0.3, 0.3), border = "coral")
abline(v = observed_temperature_value, # Use the single consistent value
       col = "red", lwd = 3, lty = 1)
# Add label for observed value, adjust position as needed
text(x = observed_temperature_value,
     y = max(density(temp_r1_simulated_values)$y) * 0.95, # Place label near the peak
     labels = paste0("Observed = ", round(observed_temperature_value, 1)), # Rounded for display
     col = "red", pos = 4, cex = 0.8) # pos=2 for left of point, as lower temp is more nested

# 4. Temperature - Curveball (Density)
temp_curveball_simulated_values <- result_temp_curveball$oecosimu$simulated
plot(density(temp_curveball_simulated_values),
     main = "Temperature Distribution - Curveball Null Model",
     xlab = "Temperature Values",
     ylab = "Density",
     col = "orange", lwd = 2,
     # Ensure x-axis range includes observed value, extended slightly
     xlim = range(c(temp_curveball_simulated_values, observed_temperature_value)) * c(0.98, 1.02)
)
polygon(density(temp_curveball_simulated_values),
        col = rgb(1, 0.65, 0, 0.3), border = "orange")
abline(v = observed_temperature_value, # Use the single consistent value
       col = "red", lwd = 3, lty = 1)
# Add label for observed value, adjust position as needed
text(x = observed_temperature_value,
     y = max(density(temp_curveball_simulated_values)$y) * 0.95, # Place label near the peak
     labels = paste0("Observed = ", round(observed_temperature_value, 1)), # Rounded for display
     col = "red", pos = 4, cex = 0.8) # pos=2 for left of point

mtext("Bootstrapping Distribution of Nestedness Measures", side = 3, line = 0, outer = TRUE, cex = 1.5, font = 2)

dev.off()
par(mfrow=c(1,1)) # Reset par to default


# Create the summary data frame for easier export
nestedness_summary_df <- data.frame(
  Measure = c("NODF", "Temperature"),
  Observed = c(observed_nodf_value, observed_temperature_value), # Use the single consistent values
  Mean_Sim_R1 = c(mean(result_nodf_r1$oecosimu$simulated[3,]), mean(result_temp_r1$oecosimu$simulated)),
  SD_Sim_R1 = c(sd(result_nodf_r1$oecosimu$simulated[3,]), sd(result_temp_r1$oecosimu$simulated)), # Added SD
  Z_score_R1 = c(result_nodf_r1$oecosimu$z[3], result_temp_r1$oecosimu$z), # Added Z-score
  p_value_R1 = c(result_nodf_r1$oecosimu$pval[3], result_temp_r1$oecosimu$pval),
  Mean_Sim_Curveball = c(mean(result_nodf_curveball$oecosimu$simulated[3,]), mean(result_temp_curveball$oecosimu$simulated)),
  SD_Sim_Curveball = c(sd(result_nodf_curveball$oecosimu$simulated[3,]), sd(result_temp_curveball$oecosimu$simulated)), # Added SD
  Z_score_Curveball = c(result_nodf_curveball$oecosimu$z[3], result_temp_curveball$oecosimu$z), # Added Z-score
  p_value_Curveball = c(result_nodf_curveball$oecosimu$pval[3], result_temp_curveball$oecosimu$pval)
)

# Print a neat summary table to console
cat("\n=== NESTEDNESS ANALYSIS SUMMARY ===\n")
# Using format() for consistent decimal places and alignment
print(format(nestedness_summary_df, digits = 3, nsmall = 3, justify = "left"), row.names = FALSE, quote = FALSE)
cat("\nNote: Lower temperature indicates higher nestedness.\n")
cat("      For Z-scores, positive values for NODF and negative for Temperature indicate greater nestedness than random.\n")


# Export summary table as a CSV file
write.csv(nestedness_summary_df, "nestedness_stats.csv", row.names = FALSE)
cat("\nSummary statistics exported to 'nestedness_stats.csv'\n")
