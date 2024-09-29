## phenology concept figure

phenology_concept_data <- read_xlsx('data/phenology_concept_data.xlsx') 

phenology_concept_data <- phenology_concept_data %>% 
  mutate (date = ymd("2024-01-01") + days(doy - 1),
          Date = format(date, "%B %d"))
phenology_concept_date <- c("April 9", "May 29", "July 18", "September 6")

(phenology_concept <- ggplot (data = phenology_concept_data, aes (x = doy, y = n, col = Snowmelt)) +
  geom_line (linewidth = 2) +
    # earliest snowmelt in spectra data
    scale_color_manual(values = c("#117733", "#72C182")) +
    theme_classic () +
    geom_vline (xintercept = 80, linetype = "dashed", linewidth = 1) +
    geom_text (x = 95, y = 2.5, label = "Early snowmelt", col = "black", size = 5) +
    # latest snowmelt in spectra data 
    geom_vline (xintercept = 180, linetype = "dashed", linewidth = 1) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_x_continuous(breaks = c(100, 150, 200, 250), labels = phenology_concept_date) +
    geom_text (x = 193, y = 1.8, label = "Late snowmelt", col = "black", size = 5) +
    # average sampling date
    geom_vline (xintercept = 210, col = "#AA4499", linetype = "dashed", linewidth = 2) +
    geom_text(x = 225, y = 3.3, label = "Sampling date", col = "#AA4499", size = 5)+
    ylab ("Leaf nitrogen concentration (%)" ) +
    xlab ("Date") +
    #scale_x_continuous(labels = phenology_concept_date) +
    theme (legend.title = element_blank(),
           axis.text = element_text (size = 18),
           axis.title=element_text(size=18,face="bold"),
           legend.text = element_text (size = 18),
           axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
           axis.title.y = element_text(margin = margin(r = 20))))
# intercept is 7.2, and slope is -0.021 based on linear model with snowfence_salix data (n~doy)

ggsave ("graphs/phenology_conceptual_diagram.png")
