# ==============================================================================
# ESTOPA ANALYSIS SCRIPT
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
output_dir  <- file.path(base_path, "ESTOPA_Analysis_Results_2026_v2")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- DATA LOADING ---
data_marks <- read_excel(marks_file) %>%
  mutate(
    ESTOPA = factor(ESTOPA, levels = c("No", "Yes")), 
    Subject = as.factor(Subject),
    AcademicYear = factor(AcademicYear, levels = c("2023/2024", "2024/2025", "2025/2026")),
    Mark = as.numeric(Mark)
  )

data_survey_raw <- read_excel(survey_file)

# ==============================================================================
# 1. ACADEMIC PERFORMANCE (ROBUST STATS & PLOT)
# ==============================================================================
cat("\n[1/4] Analyzing academic performance...\n")

marks_stat_results <- data_marks %>%
  group_by(Subject) %>%
  wilcox_test(Mark ~ AcademicYear) %>%
  filter(group2 == "2025/2026") %>%
  add_significance(p.col = "p") %>%
  add_y_position(step.increase = 0.12)

marks_summary <- data_marks %>%
  group_by(Subject, AcademicYear, ESTOPA) %>%
  summarise(Mean = mean(Mark, na.rm=T), SD = sd(Mark, na.rm=T), n = n(), .groups = 'drop') %>%
  mutate(SE = SD / sqrt(n))

pass_rate <- data_marks %>%
  group_by(Subject, AcademicYear, ESTOPA) %>%
  summarise(Pass_Rate = mean(Mark >= 5) * 100, .groups = 'drop')

cohen_results <- data_marks %>%
  group_by(Subject) %>%
  summarise(d_value = cohen.d(Mark ~ ESTOPA)$estimate,
            magnitude = cohen.d(Mark ~ ESTOPA)$magnitude, .groups = 'drop')

plot_marks <- ggplot(data_marks, aes(x = AcademicYear, y = Mark, fill = ESTOPA)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 1) +
  stat_pvalue_manual(marks_stat_results, label = "p.signif", tip.length = 0.01, hide.ns = FALSE) +
  facet_wrap(~Subject, scales = "free_x") +
  scale_fill_manual(values = c("#708090", "#006400"), labels = c("Traditional", "ESTOPA")) +
  theme_pubr() + labs(title = "Academic Impact: Year-by-Year Comparison", y = "Mark (0-10)")

ggsave(file.path(output_dir, "Marks_Performance_Boxplot.png"), plot_marks, width = 10, height = 6)

# ==============================================================================
# 2. PERCEPTION SURVEY ANALYSIS (ALL FIGURES)
# ==============================================================================
cat("[2/4] Processing perception surveys...\n")

dimension_map <- list(TLP = c("Q2", "Q4", "Q9", "Q12", "Q15", "Q17", "Q18", "Q19"),
                      AC  = c("Q3", "Q5", "Q8", "Q10", "Q13", "Q16", "Q20"),
                      ASA = c("Q6", "Q7", "Q11", "Q14"))

data_long <- data_survey_raw %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "Score") %>%
  mutate(Score = as.numeric(Score), Subject = as.factor(Subject)) %>%
  filter(question != "Q1") %>%
  mutate(Dimension = case_when(question %in% dimension_map$TLP ~ "TLP",
                               question %in% dimension_map$AC  ~ "AC",
                               question %in% dimension_map$ASA ~ "ASA"))

# Figuras de Boxplots por dimensión
for (dim_name in names(dimension_map)) {
  plot_data <- data_long %>% filter(Dimension == dim_name)
  stat_surv <- plot_data %>% group_by(question) %>% wilcox_test(Score ~ Subject) %>% 
    add_significance(p.col = "p") %>% add_y_position()
  
  p <- ggplot(plot_data, aes(x = question, y = Score, fill = Subject)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    stat_pvalue_manual(stat_surv, label = "p.signif", hide.ns = TRUE) +
    labs(title = paste("Dimension:", dim_name), x = "Question", y = "Score (0-10)") +
    theme_pubr() + scale_fill_manual(values = c("#66c2a5", "#fc8d62"))
  
  ggsave(file.path(output_dir, paste0("Survey_Boxplot_", dim_name, ".png")), p, width = 9, height = 5)
}

# Gráfico de Frecuencias
data_ranges <- data_long %>%
  mutate(Range = cut(Score, breaks = c(0, 2.5, 5.0, 7.5, 10.1), 
                     labels = c("0-2.4", "2.5-4.9", "5-7.4", "7.5-10"), include.lowest = TRUE)) %>%
  group_by(Subject, Dimension, Range) %>%
  summarise(N = n(), .groups = 'drop_last') %>%
  mutate(Relative_Freq = N / sum(N))

plot_freq <- ggplot(data_ranges, aes(x = Dimension, y = Relative_Freq, fill = Range)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ Subject) + scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "RdYlGn") + theme_bw() + labs(y = "Frequency (%)")

ggsave(file.path(output_dir, "Survey_Overall_Bars.png"), plot_freq, width = 8, height = 5)

# ==============================================================================
# 3. FINAL REPORT GENERATION (THE ULTRA-REPORT)
# ==============================================================================
cat("[3/4] Writing final report...\n")

means_summary_survey <- data_long %>%
  group_by(question, Subject) %>%
  summarise(Mean = mean(Score, na.rm=T), SD = sd(Score, na.rm=T), .groups = 'drop') %>%
  pivot_wider(names_from = Subject, values_from = c(Mean, SD))

# Test de Wilcoxon para la comparación de percepción entre asignaturas
comp_surv <- data_long %>% group_by(question) %>% 
  wilcox_test(Score ~ Subject) %>% 
  add_significance(p.col = "p")

sink(file.path(output_dir, "ESTOPA_Detailed_Report_2026.txt"))
cat("==========================================================\n")
cat("DETAILED STATISTICAL REPORT - ESTOPA PROJECT\n")
cat("Generated on:", as.character(Sys.Date()), " | Author: Alfonso\n")
cat("==========================================================\n\n")

cat("1. IMPACT ON ACADEMIC PERFORMANCE\n")
cat("----------------------------------------------------------\n")
print(as.data.frame(marks_summary))
cat("\nWILCOXON TEST RESULTS (Comparison vs. ESTOPA 2025/2026):\n")
print(as.data.frame(marks_stat_results %>% select(Subject, group1, group2, statistic, p, p.signif)))
cat("\nPASS RATES (%):\n")
print(as.data.frame(pass_rate))
cat("\nEFFECT SIZE (Cohen's d):\n")
print(as.data.frame(cohen_results))

cat("\n2. PERCEPTION ANALYSIS (RELIABILITY)\n")
cat("----------------------------------------------------------\n")
try({
  alpha_all <- psych::alpha(data_survey_raw %>% select(starts_with("Q"), -Q1) %>% mutate(across(everything(), as.numeric)), check.keys = FALSE)
  cat(paste("- Overall Alpha (Q2-Q20):", round(alpha_all$total$raw_alpha, 3), "\n"))
}, silent = TRUE)

cat("\n3. MEAN SCORES AND CONSENSUS (Survey Questions)\n")
cat("----------------------------------------------------------\n")
print(as.data.frame(means_summary_survey))

cat("\n4. PERCEPTION COMPARISON BETWEEN SUBJECTS (Wilcoxon Test)\n")
cat("----------------------------------------------------------\n")
# Aquí incluimos la tabla que solicitaste con todas las columnas
print(as.data.frame(comp_surv %>% select(question, n1, n2, statistic, p, p.signif)))

cat("\n\nLEGEND: ns: p > 0.05 | *: p <= 0.05 | **: p <= 0.01 | ***: p <= 0.001\n")
sink()

cat("\n[4/4] DONE! All figures and the report are ready in the folder.\n")
