# ==============================================================================
# ESTOPA ANALYSIS SCRIPT - PERCEPTION & ACADEMIC PERFORMANCE INTEGRATED
# ==============================================================================
# Author: Alfonso
# Date: 2026-02-03
# ==============================================================================

# 0. LOAD LIBRARIES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, ggplot2, psych, rstatix, ggpubr, scales, effsize)

# --- PATH CONFIGURATION ---
base_path  <- "C:/Users/Alfonso/Desktop/AlfonsoOA_MSI/2Docencia/12_ESTOPA_v2"
survey_file <- file.path(base_path, "Survey_results.xlsx")
marks_file  <- file.path(base_path, "Calificaciones", "GlobalMarks_03022026_1.xlsx")
output_dir  <- file.path(base_path, "ESTOPA_Analysis_Results_2026_v0")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- DATA LOADING ---
data_survey_raw <- read_excel(survey_file)
data_marks <- read_excel(marks_file) %>%
  mutate(
    # Ensure ESTOPA is a factor with specific order for coloring
    ESTOPA = factor(ESTOPA, levels = c("No", "Yes")), 
    Subject = as.factor(Subject),
    AcademicYear = as.factor(AcademicYear),
    Mark = as.numeric(Mark),
    Passed = ifelse(Mark >= 5, "Pass", "Fail")
  )

# ==============================================================================
# 1. PERCEPTION SURVEY ANALYSIS
# ==============================================================================
cat("\n[1/4] Processing perception surveys...\n")

dimension_map <- list(
  TLP = c("Q2", "Q4", "Q9", "Q12", "Q15", "Q17", "Q18", "Q19"),
  AC  = c("Q3", "Q5", "Q8", "Q10", "Q13", "Q16", "Q20"),
  ASA = c("Q6", "Q7", "Q11", "Q14")
)

data_long <- data_survey_raw %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "Score") %>%
  mutate(Score = as.numeric(Score), Subject = as.factor(Subject)) %>%
  filter(question != "Q1") %>%
  mutate(Dimension = case_when(
    question %in% dimension_map$TLP ~ "TLP",
    question %in% dimension_map$AC  ~ "AC",
    question %in% dimension_map$ASA ~ "ASA"
  ))

# Boxplots by Dimension
for (dim_name in names(dimension_map)) {
  plot_data <- data_long %>% filter(Dimension == dim_name)
  stat_test <- plot_data %>% group_by(question) %>%
    wilcox_test(Score ~ Subject) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance() %>% add_y_position(step.increase = 0.1)
  
  p <- ggplot(plot_data, aes(x = question, y = Score, fill = Subject)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1), size = 1.2, alpha = 0.3) +
    stat_pvalue_manual(stat_test, label = "p.adj.signif", hide.ns = TRUE) +
    labs(title = paste("Dimension:", dim_name, "- Comparison by Subject"),
         x = "Question", y = "Score (0-10)") +
    theme_pubr() + scale_fill_manual(values = c("#66c2a5", "#fc8d62"))
  
  ggsave(file.path(output_dir, paste0("Survey_Boxplot_", dim_name, ".png")), p, width = 9, height = 5)
}

# Relative Frequencies
data_ranges <- data_long %>%
  mutate(Range = cut(Score, breaks = c(0, 2.5, 5.0, 7.5, 10.1), 
                     labels = c("0-2.4", "2.5-4.9", "5-7.4", "7.5-10"), include.lowest = TRUE)) %>%
  group_by(Subject, Dimension, Range) %>%
  summarise(N = n(), .groups = 'drop_last') %>%
  mutate(Relative_Freq = N / sum(N))

plot_bars <- ggplot(data_ranges, aes(x = Dimension, y = Relative_Freq, fill = Range)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ Subject) + scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "RdYlGn") + theme_bw() +
  labs(title = "Perception Profile by Subject", y = "Frequency (%)")

ggsave(file.path(output_dir, "Survey_Overall_Bars.png"), plot_bars, width = 8, height = 5)

# ==============================================================================
# 2. ACADEMIC PERFORMANCE ANALYSIS
# ==============================================================================
cat("[2/4] Analyzing academic performance...\n")

marks_summary <- data_marks %>%
  group_by(Subject, AcademicYear, ESTOPA) %>%
  summarise(Mean = mean(Mark, na.rm=T), SD = sd(Mark, na.rm=T), n = n(), .groups = 'drop') %>%
  mutate(SE = SD / sqrt(n))

# A. Mean Marks Bars (Dark Green for ESTOPA) - WITHOUT ASTERISK
plot_marks_bar <- ggplot(marks_summary, aes(x = AcademicYear, y = Mean, fill = ESTOPA)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(0.8)) +
  facet_wrap(~Subject) +
  scale_fill_manual(values = c("#708090", "#006400"), labels = c("Traditional", "ESTOPA")) +
  # Se ha eliminado geom_text que generaba el asterisco
  labs(title = "Impact of ESTOPA on Mean Marks", 
       subtitle = "Green bars indicate ESTOPA methodology implementation",
       y = "Mean Score (0-10)", x = "Academic Year", fill = "Methodology") +
  theme_minimal()

ggsave(file.path(output_dir, "Marks_Performance_Bars.png"), plot_marks_bar, width = 10, height = 6)

ggsave(file.path(output_dir, "Marks_Performance_Bars.png"), plot_marks_bar, width = 10, height = 6)

# B. Performance Boxplots
plot_marks_box <- ggplot(data_marks, aes(x = AcademicYear, y = Mark, fill = ESTOPA)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~Subject) +
  scale_fill_manual(values = c("#708090", "#006400")) +
  labs(title = "Grade Distribution: Traditional vs ESTOPA", y = "Mark", x = "Academic Year") +
  theme_light()

ggsave(file.path(output_dir, "Marks_Performance_Boxplot.png"), plot_marks_box, width = 10, height = 6)

# ==============================================================================
# 3. STATISTICAL METRICS & COHEN'D
# ==============================================================================
cat("[3/4] Calculating impact statistics...\n")

cohen_results <- data_marks %>%
  group_by(Subject) %>%
  summarise(
    d_value = cohen.d(Mark ~ ESTOPA)$estimate,
    magnitude = cohen.d(Mark ~ ESTOPA)$magnitude,
    .groups = 'drop'
  )

pass_rate <- data_marks %>%
  group_by(Subject, AcademicYear, ESTOPA) %>%
  summarise(Pass_Rate = mean(Mark >= 5) * 100, .groups = 'drop')

# Consensus Table (Mean/SD by Subject for the survey)
means_summary_survey <- data_long %>%
  group_by(question, Subject) %>%
  summarise(Mean = mean(Score, na.rm=T), SD = sd(Score, na.rm=T), .groups = 'drop') %>%
  pivot_wider(names_from = Subject, values_from = c(Mean, SD))

# ==============================================================================
# 4. FINAL REPORT GENERATION
# ==============================================================================
cat("[4/4] Writing final report...\n")

sink(file.path(output_dir, "ESTOPA_Detailed_Report_2026.txt"))
cat("==========================================================\n")
cat("DETAILED STATISTICAL REPORT - ESTOPA PROJECT v2\n")
cat("Generated on:", as.character(Sys.Date()), "\n")
cat("Author: Alfonso\n")
cat("==========================================================\n\n")

cat("1. IMPACT ON ACADEMIC PERFORMANCE\n")
cat("----------------------------------------------------------\n")
cat("Mean Comparison by Year and Method:\n")
print(as.data.frame(marks_summary))
cat("\nPass Rates (%):\n")
print(as.data.frame(pass_rate))

cat("\nEFFECT SIZE (Cohen's d):\n")
cat("This metric measures the magnitude of the improvement between Traditional and ESTOPA.\n")
print(as.data.frame(cohen_results))
cat("\nInterpretation d: <0.2 Negligible | 0.2-0.5 Small | 0.5-0.8 Medium | >0.8 Large\n\n")

cat("2. PERCEPTION ANALYSIS (SURVEYS)\n")
cat("----------------------------------------------------------\n")
cat("INTERNAL RELIABILITY (Cronbach's Alpha):\n")
alpha_all <- psych::alpha(data_survey_raw %>% select(starts_with("Q"), -Q1) %>% mutate(across(everything(), as.numeric)), check.keys = FALSE)
cat(paste("- Overall Alpha (Q2-Q20):", round(alpha_all$total$raw_alpha, 3), "\n"))

for (dim_name in names(dimension_map)) {
  data_dim <- data_survey_raw %>% select(all_of(dimension_map[[dim_name]])) %>% mutate(across(everything(), as.numeric))
  a_dim <- psych::alpha(data_dim, check.keys = FALSE)
  cat(paste0("- Alpha ", dim_name, ": ", round(a_dim$total$raw_alpha, 3), "\n"))
}

cat("\n3. MEAN SCORES AND CONSENSUS (Survey Questions)\n")
cat("----------------------------------------------------------\n")
print(as.data.frame(means_summary_survey))

cat("\n4. PERCEPTION COMPARISON BETWEEN SUBJECTS (Wilcoxon Test)\n")
comp_surv <- data_long %>% group_by(question) %>% wilcox_test(Score ~ Subject) %>% add_significance()
print(as.data.frame(comp_surv %>% select(question, n1, n2, statistic, p, p.signif)))

sink()

cat(paste("\nPROCESS COMPLETED SUCCESSFULLY.\n"))
cat(paste("All outputs are saved in:\n", output_dir, "\n"))