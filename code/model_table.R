# Model table 
# Code developed from Mariana Garcia Criado's repository ArcticPlantDynamics: https://github.com/marianagarciacriado/ArcticPlantDynamics/tree/v1

## LIBRARIES ----
library(dplyr)
library(broom)
library(stargazer)


## FUNCTION ----

# Jonathan Chang's function
p_summarize <- function(model) {
  brms::posterior_summary(model) %>% 
    as_tibble(rownames = "parameter")
}



## SCRIPTS MODEL TABLE ----

# Add model objects to list
models.list <- list(location_mod, species_mod, 
                precip_pft_mod, spectra_precip_mod,
                obs_snowmelt_mod, spectra_snowmelt_mod)


# Create dataframe with model names
models.name <- c("Snowfence Sites", "Snowfence PFTs",
                 "Winter Precip vs Obs of Leaf N", "Winter Precip vs Spectroscopic N",
                 "Snowmelt Date vs Obs of Leaf N", "Snowmelt Date vs Spectroscopic N")

# Bind them together
model_number <- 1:6
mod.df <- data.frame(model_number, models.name)

# Extract parameters
mod.table <- lapply(models.list, p_summarize) %>% 
  bind_rows(.id = "model_number") 

# Add model name to table
mod.table$model_number <- as.integer(mod.table$model_number)
mod.table.final <- left_join(mod.table, mod.df, by = "model_number")

# Clean model parameters
mod.table.final.2 <- mod.table.final %>% dplyr::filter(parameter != "lp__") %>% 
  dplyr::filter(!stringr::str_detect(parameter, "^r_"))

mod.table.final.2$models.name[duplicated(mod.table.final.2$models.name)] <- "  "
mod.table.final.2$models.name <- as.character(mod.table.final.2$models.name)
mod.table.final.2$model_number[duplicated(mod.table.final.2$model_number)] <- "  "

colnames(mod.table.final.2) <- c("Model number", "Term", "Estimate", "Std. error", "Lower 95% CI", "Upper 95% CI", "Model name")
mod.table.final.3 <- mod.table.final.2[, c(1, 7, 2, 3, 4, 5, 6)]

#  Round to 3 decimals only because not working on stargazer function
mod.table.final.4 <- mod.table.final.3 %>% mutate_if(is.numeric, round, digits = 3)

# Save in csv
write.csv(mod.table.final.4, "models/mod_table.csv")


## Models are put together in Excel and then in Word for Table S3.