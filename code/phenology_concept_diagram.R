# Else Radeloff
# February 22nd, 2024
# phenology concept figure

# Loading packages and data ----

phenology_concept_data <- read_xlsx('data/phenology_concept_data.xlsx')

phenology_concept_data <- phenology_concept_data %>%
  mutate (date = ymd("2024-01-01") + days(doy - 1),
          Date = format(date, "%B %d"))
phenology_concept_date <- c("April 9", "May 29", "July 18", "September 6")

(
  phenology_concept <- ggplot (data = phenology_concept_data, aes (
    x = doy, y = n, col = Snowmelt
  )) +
    geom_line (linewidth = 1) +
    # earliest snowmelt in spectra data
    scale_color_manual(values = c("#4287f5", "#eb7e60")) +
    geom_vline (
      xintercept = 80,
      linetype = "dashed",
      linewidth = 1
    ) +
    geom_text (
      x = 95,
      y = 2.5,
      label = "Early snowmelt",
      col = "black",
      size = 3
    ) +
    # latest snowmelt in spectra data
    geom_vline (
      xintercept = 180,
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5.5)) +
    scale_x_continuous(breaks = c(100, 150, 200, 250), labels = phenology_concept_date) +
    geom_text (
      x = 195,
      y = 1.8,
      label = "Late snowmelt",
      col = "black",
      size = 3
    ) +
    # average sampling date
    geom_vline (
      xintercept = 210,
      col = "darkgrey",
      linetype = "dashed",
      linewidth = 1
    ) +
    geom_text(
      x = 225,
      y = 3.3,
      label = "Sampling date",
      col = "darkgrey",
      size = 3
    ) +
    ylab ("Leaf nitrogen concentration (%)\n") +
    xlab ("\nDate") +
    #scale_x_continuous(labels = phenology_concept_date) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.88, 0.1),
      legend.key.spacing.y = unit(0.3, "cm"),
      legend.text = element_text (size = 8)
    )
)

# theme (legend.title = element_blank(),
#        axis.text = element_text (size = 18),
#        axis.title=element_text(size=18,face="bold"),
#        legend.text = element_text (size = 18),
#        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
#        axis.title.y = element_text(margin = margin(r = 20))))
# intercept is 7.2, and slope is -0.021 based on linear model with snowfence_salix data (n~doy)

ggsave(
  "graphs/Figure7_phenology_conceptual_diagram.png",
  width = 7,
  height = 5
)
