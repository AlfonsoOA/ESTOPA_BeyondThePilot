ESTOPA Project v2: Analysis of Perception & Academic Performance

This repository contains the R code and statistical workflows for the ESTOPA Project v2 (2026). The project evaluates the impact of a scalable, analog gamification methodology on academic performance and student perception in higher education bioscience subjects.
📋 Table of Contents

    Overview

    Requirements

    Data Structure

    Analysis Sections

    Outputs

🔍 Overview

The script integrates two main datasets to assess:

    Academic Performance: Comparison between traditional teaching and the ESTOPA methodology using Cohen's d effect size.

    Student Perception: Analysis of survey results across three dimensions (TLP, AC, ASA) using Cronbach’s alpha and Wilcoxon signed-rank tests.

🛠 Requirements

The script uses the pacman manager to handle dependencies. The following R packages are required:

    readxl, dplyr, tidyr: Data manipulation.

    ggplot2, ggpubr, scales: Visualizations.

    psych: Reliability analysis (Alpha).

    rstatix, effsize: Statistical testing and effect sizes.

📂 Data Structure

To run the script, ensure the following files are in the specified base_path:

    Survey_results.xlsx: Survey scores (Items Q1-Q20).

    Calificaciones/GlobalMarks_03022026_1.xlsx: Academic grades including columns for Subject, AcademicYear, ESTOPA (Yes/No), and Mark.

⚙️ Analysis Sections

The script is divided into four main blocks:

    Perception Survey Analysis: Groups questions into dimensions and generates boxplots and relative frequency bars.

    Academic Performance Analysis: Calculates means, standard errors, and generates longitudinal bar plots and boxplots.

    Statistical Metrics: Computes Cohen’s d for impact magnitude and pass rates.

    Final Report: Generates a comprehensive .txt report with all statistical outputs and p-values.

📊 Outputs

Results are automatically saved in the ESTOPA_Analysis_Results_2026_v0 directory, including:

    Visuals: PNG files for grade distributions and perception profiles.

    Report: ESTOPA_Detailed_Report_2026.txt (Complete statistical summary).

✍️ Author

    AlfonsoOA

    Date: 2026-02-03

    Context: Research on Innovation in Teaching and Assessment.
