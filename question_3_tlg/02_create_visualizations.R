log_file <- file("02_create_visualizations_Log.txt", open = "wt")
sink(log_file, type = "output")
sink(log_file, type = "message")

################# Q3: TLG - Adverse Events Reporting #################
#Goal: Create outputs for adverse events summary using the ADAE dataset and {gtsummary}. 
#Author: Maria Isabel Moscardo Garcia
#Date: 10/03/2026
#Description: This script creates tables and figures describing the adverse events data
################################################################################

library(dplyr)
library(pharmaverseadam)
library(gtsummary)
library(forcats)
library(ggplot2)

#1. Load datasets
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

#2. Plot severity distribution per arm
plot_data <- pharmaverseadam::adae |>
  filter(TRTEMFL == "Y", SAFFL == "Y") |>
  mutate(
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")),
    ACTARM = stringr::str_wrap(ACTARM, width = 15)
  )

ae_severity_plot <- ggplot(plot_data, aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack", color = "white", linewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AE",
    fill = "Severity"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )
print(ae_severity_plot)

#3. Plot 10 most frequent AEs
total_n <- adsl |> 
  filter(SAFFL == "Y") |> 
  nrow()

ae_overall <- adae |>
  filter(TRTEMFL == "Y", SAFFL == "Y") |>
  distinct(USUBJID, AETERM) |>
  count(AETERM, name = "n_ae") |>
  rowwise() |>
  mutate(
    prop = n_ae / total_n,
    low  = binom.test(n_ae, total_n)$conf.int[1],
    high = binom.test(n_ae, total_n)$conf.int[2]
  ) |>
  ungroup() |>
  # Select the Top 10 most frequent
  slice_max(n_ae, n = 10, with_ties = FALSE) |>
  # Order for plotting (highest at the top)
  mutate(AETERM = fct_reorder(AETERM, prop))

ggplot(ae_overall, aes(x = prop, y = AETERM)) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.2, color = "steelblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_text(aes(label = paste0("n=", n_ae)), vjust = -1, size = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("Total Safety Population (N = ", total_n, ") | 95% Confidence Intervals"),
    x = "Percentage of Patients",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print("SUCCESS: The program ran without errors and the corresponding figures/tables have been generated.")

################################################################################
sink(type = "message")
sink(type = "output")