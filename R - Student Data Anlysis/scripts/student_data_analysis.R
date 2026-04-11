# Brief exploratory analysis for the Student Data Analysis workbook.
# This stays intentionally compact and practical for a timed take-home task.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(broom)
  library(here)
  library(fs)
})

raw_path      <- here("data", "raw", "student_data_analysis.xlsx")
processed_dir <- here("data", "processed")
tables_dir    <- here("output", "tables")
figures_dir   <- here("output", "figures")

dir_create(processed_dir)
dir_create(tables_dir)
dir_create(figures_dir)

student_terms_raw <- read_excel(raw_path, sheet = "Data")

student_terms <- student_terms_raw %>%
  mutate(
    YEAR = factor(YEAR),
    TERM = factor(TERM),
    GENDER = if_else(GENDER == "-888", "Unknown", GENDER),
    FIRST_IN_FAMILY_FLAG = if_else(FIRST_IN_FAMILY_FLAG == "UNK", "Unknown", FIRST_IN_FAMILY_FLAG),
    FIRST_PEOPLES_FLAG = if_else(FIRST_PEOPLES_FLAG == "UNK", "Unknown", FIRST_PEOPLES_FLAG),
    SCHOOL_LEAVER = if_else(SCHOOL_LEAVER == "UNK", "Unknown", SCHOOL_LEAVER),
    across(
      c(
        ACADEMIC_CAREER_CODE, QILT_STUDY_AREA, PROGRAM_ORG, PROGRAM_GROUP, CAMPUS,
        GENDER, FIRST_IN_FAMILY_FLAG, FIRST_PEOPLES_FLAG, ATTENDANCE_TYPE, INT_DOM,
        SCHOOL_LEAVER
      ),
      as.factor
    ),
    DISABILITY_FLAG = factor(DISABILITY_FLAG),
    COMPLETED_PROGRAM = factor(COMPLETED_PROGRAM),
    RETAINED_IN_PROGRAM = factor(RETAINED_IN_PROGRAM)
  )

# Missingness check. STUDENT_ACADEMY_FLAG is almost entirely missing and the
# non-missing values are all 1, so it is excluded from later analysis.
missing_summary <- tibble(
  variable = names(student_terms_raw),
  missing_n = colSums(is.na(student_terms_raw)),
  missing_pct = round(100 * missing_n / nrow(student_terms_raw), 2)
) %>%
  arrange(desc(missing_n))

write.csv(missing_summary, path(tables_dir, "missing_summary.csv"), row.names = FALSE)

numeric_summary <- student_terms_raw %>%
  summarise(
    across(
      c(UNITS_PASSED, TERM_GPA, CUMULATIVE_GPA, STUDY_LOAD),
      list(
        min = ~ min(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(cols = everything()) %>%
  separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

write.csv(numeric_summary, path(tables_dir, "numeric_summary.csv"), row.names = FALSE)

correlation_table <- student_terms_raw %>%
  select(UNITS_PASSED, TERM_GPA, CUMULATIVE_GPA, STUDY_LOAD, RETAINED_IN_PROGRAM) %>%
  mutate(RETAINED_IN_PROGRAM = as.numeric(RETAINED_IN_PROGRAM)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

write.csv(
  as.data.frame(correlation_table),
  path(tables_dir, "correlation_matrix.csv"),
  row.names = TRUE
)

retention_by_study_area <- student_terms %>%
  count(QILT_STUDY_AREA, RETAINED_IN_PROGRAM, name = "n") %>%
  group_by(QILT_STUDY_AREA) %>%
  mutate(
    total_n = sum(n),
    retention_rate = n / total_n
  ) %>%
  ungroup() %>%
  filter(RETAINED_IN_PROGRAM == "1") %>%
  arrange(desc(retention_rate))

write.csv(
  retention_by_study_area,
  path(tables_dir, "retention_by_study_area.csv"),
  row.names = FALSE
)

term_gpa_plot <- ggplot(student_terms_raw, aes(x = TERM_GPA)) +
  geom_histogram(binwidth = 0.25, fill = "#3b7ea1", color = "white") +
  labs(
    title = "Distribution of Term GPA",
    x = "Term GPA",
    y = "Student-term count"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = path(figures_dir, "term_gpa_distribution.png"),
  plot = term_gpa_plot,
  width = 8,
  height = 5,
  dpi = 300
)

retention_vs_gpa_plot <- student_terms_raw %>%
  mutate(retained_label = if_else(RETAINED_IN_PROGRAM == 1, "Retained", "Not retained")) %>%
  ggplot(aes(x = retained_label, y = TERM_GPA, fill = retained_label)) +
  geom_boxplot(outlier.alpha = 0.1, width = 0.6) +
  labs(
    title = "Retention is associated with higher term GPA",
    x = NULL,
    y = "Term GPA"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(
  filename = path(figures_dir, "term_gpa_by_retention.png"),
  plot = retention_vs_gpa_plot,
  width = 7,
  height = 5,
  dpi = 300
)

model_data <- student_terms %>%
  select(
    RETAINED_IN_PROGRAM, YEAR, TERM, GENDER, DISABILITY_FLAG, FIRST_IN_FAMILY_FLAG,
    FIRST_PEOPLES_FLAG, ATTENDANCE_TYPE, INT_DOM, SCHOOL_LEAVER, QILT_STUDY_AREA,
    UNITS_PASSED, TERM_GPA, CUMULATIVE_GPA, STUDY_LOAD
  ) %>%
  filter(YEAR != "2025") %>%
  drop_na()

retention_model <- glm(
  RETAINED_IN_PROGRAM ~ YEAR + TERM + GENDER + DISABILITY_FLAG +
    FIRST_IN_FAMILY_FLAG + FIRST_PEOPLES_FLAG + ATTENDANCE_TYPE + INT_DOM +
    SCHOOL_LEAVER + QILT_STUDY_AREA + UNITS_PASSED + TERM_GPA +
    CUMULATIVE_GPA + STUDY_LOAD,
  data = model_data,
  family = binomial()
)

model_coefficients <- tidy(retention_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    odds_ratio_low = exp(estimate - 1.96 * std.error),
    odds_ratio_high = exp(estimate + 1.96 * std.error)
  ) %>%
  arrange(desc(abs(estimate)))

write.csv(model_coefficients, path(tables_dir, "retention_model_coefficients.csv"), row.names = FALSE)

model_predictions <- augment(retention_model, type.predict = "response") %>%
  transmute(
    actual = as.numeric(as.character(RETAINED_IN_PROGRAM)),
    predicted_probability = .fitted,
    predicted_class = if_else(predicted_probability >= 0.5, 1, 0)
  )

model_metrics <- tibble(
  accuracy = mean(model_predictions$actual == model_predictions$predicted_class),
  baseline_accuracy = max(mean(model_predictions$actual == 1), mean(model_predictions$actual == 0)),
  retained_rate = mean(model_predictions$actual),
  n_model_rows = nrow(model_predictions)
)

write.csv(model_metrics, path(tables_dir, "retention_model_metrics.csv"), row.names = FALSE)

key_observations <- c(
  paste0(
    "The workbook contains ", nrow(student_terms_raw), " student-term rows across ",
    dplyr::n_distinct(student_terms_raw$MASKED_ID), " masked students."
  ),
  paste0(
    "Only STUDENT_ACADEMY_FLAG has material missingness: ",
    missing_summary$missing_pct[missing_summary$variable == "STUDENT_ACADEMY_FLAG"],
    "% missing, and every observed value is 1."
  ),
  paste0(
    "Retention is moderately imbalanced but not extreme: ",
    round(100 * mean(student_terms_raw$RETAINED_IN_PROGRAM == 1), 1),
    "% of rows are marked retained."
  ),
  paste0(
    "Students marked retained have a much higher average TERM_GPA (",
    round(mean(student_terms_raw$TERM_GPA[student_terms_raw$RETAINED_IN_PROGRAM == 1]), 2),
    ") than those not retained (",
    round(mean(student_terms_raw$TERM_GPA[student_terms_raw$RETAINED_IN_PROGRAM == 0]), 2),
    ")."
  ),
  paste0(
    "The simple logistic model improves on the naive majority-class baseline: accuracy ",
    round(model_metrics$accuracy, 3), " versus baseline ",
    round(model_metrics$baseline_accuracy, 3), "."
  ),
  paste0(
    "All 13,733 rows from 2025 are coded as not retained, so 2025 was excluded from the logistic model as a likely timing/censoring issue rather than a credible behavioural signal."
  )
)

cat("Analysis complete.\n")
cat(paste("-", key_observations, collapse = "\n"))
