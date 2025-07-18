#### NESTEDNESS ANALYSIS ####

# Adapted from Olivier Morin (Dubourg et al., 2025)

# ------ LOAD DATA AND PACKAGES -------

library(here)
library(dplyr)
library(tidyverse)
library(vegan) #for the two nestedness measures

S1_long_data <- read_csv(here("study_1", "data", "clean", "S1_long_data.csv"))

# Define nsimul (number of simulated matrices)
nsimul <- 500

#---- CREATE A SCORING MATRIX WITH PARTICIPANTS AS ROWS AND QUESTIONS AS Qs AS COLUMNS ----

# keep only relevant data columns for nestedness, removee q16 which is the cheat test
data_nesting <- S1_long_data %>% select(participant_id, q1mark:q15mark)
# there are 30 identical rows per participant, keep only 1
data_nesting <- data_nesting %>%
  distinct(participant_id, .keep_all = TRUE)
# remove participant_id, rows are participants, columns are questions
data_nesting <- data_nesting %>% select(q1mark:q15mark)
data_nesting <- data_nesting %>% rename()

# set data as a matrix of 1s and 0s
data_nesting <- data_nesting %>% as.matrix()
data_nesting <- apply(data_nesting, 2, as.integer) # Ensure all values are integers (0 or 1)


# ---- MEASURING NESTEDNESS -------

# calculate observed values
observed_nodf_value <- nestednodf(data_nesting)$statistic['NODF']
observed_temperature_value <- nestedtemp(data_nesting)$statistic

set.seed(123) # for reproducibility

# bootstrapping with oecosimu
result_nodf_r1 <- oecosimu(data_nesting, nestednodf, "r1", nsimul = nsimul)
result_temp_r1 <- oecosimu(data_nesting, nestedtemp, "r1", nsimul = nsimul)

# build paths for saving figures
fig_dir <- here::here("figures")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}
png_path  <- file.path(fig_dir, "nestedness_density_plots.png")
csv_path  <- file.path(fig_dir, "nestedness_stats.csv")

# create density plot of temperature and nodf simulated values
png(
  filename = png_path,
  width    = 10,    
  height   = 8,    
  units    = "in",
  res      = 300
)

par(
  mfrow = c(2, 1),
  mar   = c(4.5, 4.5, 3, 1) + 0.1
)

# 1) NODF
nodf_r1_vals <- result_nodf_r1$oecosimu$simulated[3, ]
d1 <- density(nodf_r1_vals)
# compute x‑limits to include observed value
xlim1 <- range(d1$x, observed_nodf_value) * c(0.98, 1.02)

plot(
  d1,
  main  = "NODF Distribution (R1 Null Model)",
  xlab  = "NODF", 
  ylab  = "Density",
  lwd   = 2,
  xlim  = xlim1
)
polygon(d1, col = rgb(0, 0, 1, 0.3), border = "blue")

# draw & label observed line
abline(v = observed_nodf_value, col = "red", lwd = 2)
text(
  x      = observed_nodf_value,
  y      = max(d1$y) * 0.9,
  labels = paste0("Observed = ", round(observed_nodf_value, 3)),
  col    = "red",
  pos    = 2,    # left of the line
  cex    = 0.8
)

# 2) Temperature
temp_r1_vals <- result_temp_r1$oecosimu$simulated
d2 <- density(temp_r1_vals)
# include observed temperature on left
xlim2 <- range(d2$x, observed_temperature_value) * c(0.98, 1.02)

plot(
  d2,
  main  = "Temperature Distribution (R1 Null Model)",
  xlab  = "Temperature", 
  ylab  = "Density",
  lwd   = 2,
  xlim  = xlim2
)
polygon(d2, col = rgb(1, 0.5, 0.3, 0.3), border = "coral")

# draw & label observed line
abline(v = observed_temperature_value, col = "red", lwd = 2)
text(
  x      = observed_temperature_value,
  y      = max(d2$y) * 0.9,
  labels = paste0("Observed = ", round(observed_temperature_value, 3)),
  col    = "red",
  pos    = 4,    # right of the line
  cex    = 0.8
)

dev.off()
par(mfrow = c(1,1))

# summary table
nestedness_summary_df <- data.frame(
  Measure     = c("NODF", "Temperature"),
  Observed    = c(observed_nodf_value, observed_temperature_value),
  Mean_Sim = c(mean(nodf_r1_vals),          mean(temp_r1_vals)),
  SD_Sim   = c(sd(nodf_r1_vals),            sd(temp_r1_vals)),
  Z_score  = c(result_nodf_r1$oecosimu$z[3], result_temp_r1$oecosimu$z),
  p_value  = c(result_nodf_r1$oecosimu$pval[3], result_temp_r1$oecosimu$pval)
)

cat("\n=== NESTEDNESS ANALYSIS SUMMARY ===\n")
print(
  format(nestedness_summary_df, digits = 3, nsmall = 3, justify = "left"),
  row.names = FALSE, quote = FALSE
)

# Export summary
write.csv(nestedness_summary_df, csv_path, row.names = FALSE)
cat("Final contents of figures/: ", paste(list.files(fig_dir), collapse = ", "), "\n")

