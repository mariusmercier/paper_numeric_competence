library(tidyverse)
library(janitor)
library(lmerTest)
library(car)
library(combinat)
library(here)


# ---- READ IN DATA, EXCLUDE PARTICIPANTS, AND CLEAN ----

data_raw <- read_csv(here("pilot", "data", "raw", "Pilot_data.csv"))

#exclude participants who fail attention check and preview trials
data <- data_raw %>% clean_names() %>% 
  filter(distribution_channel != "preview")
data <- data %>% filter(grepl("2025-06-24", end_date))
excluded_participants <- data %>% filter(!grepl("pay", attention_check)&(!grepl("attention", attention_check))) #none excluded
data <- data %>% filter(grepl("pay", attention_check)|grepl("attention", attention_check))
data <- data %>% rename(total_score = sc0)
data$total_score <- as.numeric(data$total_score)
data_clean <- data %>% select(prolific_pid, q1:q15, total_score, time_on_question1:time_on_question15)

# exclude likely cheaters (got 'impossible' question right)
possible_cheaters <- data_clean %>% filter(q15 == 9543306)
data_clean <- data_clean %>% 
  filter(!prolific_pid %in% (data_clean %>% 
                     filter(q15 == 9543306) %>% 
                     pull(prolific_pid)))

data_clean <- data_clean %>%
  mutate(across(starts_with("time_on_question"), as.numeric))

#create data_clean csv file
write.csv(data_clean, file = here("pilot", "data", "clean", "data_clean.csv"), row.names = FALSE)


# ---- CREATING LONGER DATAFRAME WITH SCORING ----

#mapping prolific IDs to integers
pid_map <- data_clean %>%
  distinct(prolific_pid) %>%
  mutate(id = row_number())

# Correct answers list
correct_answers <- c(60, 91, 820, 102, 1209, 5917, 4906, 11240, 930, 16100, 2370000, 563240, 1528, 92575, 9543306)

# Create longer data frame with scoring for each question
data_long <- data_clean %>%
  left_join(pid_map, by = "prolific_pid") %>%
  pivot_longer(cols = q1:q15, names_to = "question", values_to = "answer") %>%
  mutate(
    question_num = as.numeric(str_extract(question, "\\d+")),
    time = case_when(
      question_num == 1 ~ time_on_question1,
      question_num == 2 ~ time_on_question2,
      question_num == 3 ~ time_on_question3,
      question_num == 4 ~ time_on_question4,
      question_num == 5 ~ time_on_question5,
      question_num == 6 ~ time_on_question6,
      question_num == 7 ~ time_on_question7,
      question_num == 8 ~ time_on_question8,
      question_num == 9 ~ time_on_question9,
      question_num == 10 ~ time_on_question10,
      question_num == 11 ~ time_on_question11,
      question_num == 12 ~ time_on_question12,
      question_num == 13 ~ time_on_question13,
      question_num == 14 ~ time_on_question14,
      question_num == 15 ~ time_on_question15
    ),
    score = if_else(answer == correct_answers[question_num], 1, 0)
  ) %>%
  select(id, question, answer, score, time, total_score)

data_long$time <- as.numeric(data_long$time)

# ---- DESCRIPTIVE STATS AND PLOTS ----

# distribution of scores
  hist(data_clean$total_score)
  print(hist)
  summary_total_scores <- summarise(data_clean,
                                    mean_total_score = mean(total_score),
                                    sd_total_score = sd(total_score))


# number of correct answers per question
    question_correct_counts <- data_long %>%
      filter(score == 1) %>%
      group_by(question) %>%
      summarise(correct_count = n()) %>%
      ungroup() 
    
    # Ensure all questions from q1 to q15 are present
    all_questions <- data.frame(question = paste0("q", 1:15))
    question_correct_counts <- all_questions %>%
      left_join(question_correct_counts, by = "question") %>%
      mutate(correct_count = replace_na(correct_count, 0))
    
    # Order questions numerically
    question_correct_counts$question <- factor(question_correct_counts$question,
                                               levels = paste0("q", sort(as.numeric(sub("q", "", unique(question_correct_counts$question))))))
    
    ggplot(question_correct_counts, aes(x = question, y = correct_count, fill = question)) +
      geom_bar(stat = "identity", color = "black") + 
      labs(title = "Count of Correct Answers per Question",
           x = "Question",
           y = "Number of People Who Answered Correctly") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "none", 
            plot.title = element_text(hjust = 0.5, face = "bold")) + 
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 


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
 
# ---- STATISTICAL MODELS OF SCORES ----
   
# glmer for whether RT predicts correctness per participant, per question
    score_by_RT <- glmer(score ~ time + (1|id) +(1|question), family = "binomial", data_long)
    print(score_by_RT)
    # to test significance
    # Anova(score_by_RT, type = "III") 
    
    
# WE TEST IF THERE IS A QUADRATIC RELATION: 
    data_long$time_c <- scale(data_long$time, center = TRUE, scale = FALSE)
    m_lin  <- glmer(score ~ time_c +                 # linear effect only
                      (1 | id) + (1 | question),
                    family = binomial, data = data_long)
    m_quad <- glmer(score ~ time_c + I(time_c^2) +   # linear + quadratic
                      (1 | id) + (1 | question),
                    family = binomial, data = data_long)
    
    anova(m_lin, m_quad, test = "Chisq")
    
    newdat <- data.frame(time_c = seq(min(data_long$time_c),
                                      max(data_long$time_c), length = 200))
    preds  <- predict(m_quad, newdat, type = "response", re.form = NA)
    plot(data_long$time, data_long$score)          # raw points
    lines(newdat$time_c + attr(data_long$time_c,"scaled:center"),
          preds, lwd = 2)
    
    
# glm for whether total time per participant predicts total_score (do faster participants get more right)
    # Create participant-level summary data
    participant_summary <- data_long %>%
      group_by(id) %>%
      summarise(
        total_time = sum(time, na.rm = TRUE),
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


# comparing two people who got the same question right, the one who did it faster tended to perform better on the whole (removing the cheaters)
    
    # Filter only correct responses
    correct_responses <- data_long %>% 
      filter(score == 1)
    
    # For each question, get all pairs of participants who got it right
    pairwise_time <- correct_responses %>%
      group_by(question) %>%
      do({
        df <- .
        if (nrow(df) < 2) return(NULL)# Skip if fewer than 2 participants got it right
        
        pairs <- combn(1:nrow(df), 2)
        comparisons <- apply(pairs, 2, function(p) {
          a <- df[p[1], ]
          b <- df[p[2], ]
          
          # Identify which one was faster
          faster <- if (a$time < b$time) a else b
          slower <- if (a$time < b$time) b else a
          
          data.frame(
            question = a$question,
            faster_id = faster$id,
            slower_id = slower$id,
            faster_score = faster$total_score,
            slower_score = slower$total_score,
            faster_time = faster$time,
            slower_time = slower$time,
            faster_better = faster$total_score > slower$total_score
          )
        })
        
        do.call(rbind, comparisons)
      }) %>%
      ungroup()

    print(mean(pairwise_time$faster_better, na.rm = TRUE))
    binom.test(sum(pairwise_time$faster_better), nrow(results), p = 0.5)
    
    
    
    
    
    
    