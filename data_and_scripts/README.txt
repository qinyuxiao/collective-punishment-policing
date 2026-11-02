The preprocessing scripts (e.g., run_1_preprocessing.R) in the 'Data' folder did the bonus payment calculation and anonymization. The outputs are, e.g., run_1_usable.csv, which are further processed (by Cleaning.R). The raw data files and the admin data files (payment to participants) should not be shared.

- Cleaning.R

This script does the following:

1. It takes the outputs from the preprocessing scripts (e.g., run_1_usable.csv) and combines them.
2. It renames variables for conciseness and calculates additional meta-level variables.
3. It outputs 'data_full_cleaned.csv' for further processing by Preparation.qmd.

- Preparation.qmd

This Quarto file imports 'data_full_cleaned.csv' and prepares datasets that are ready for analysis. It outputs 'data_ready_for_analysis.RData', which Analysis.qmd imports and does analysis on.

Note about D:

The raw datasets already contain a D-score calculated inside the experiment program. But the item scores were not reverse-scored (if they need to be).