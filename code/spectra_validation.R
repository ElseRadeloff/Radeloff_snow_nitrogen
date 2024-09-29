 # Else Radeloff 
# February 22nd, 2024

# Validating spectral estimates of nitrogen


##################################################################################

# load libraries 
library (tidyverse)
library (readxl)
library (readr)
library (lmtest)

# load data ---- 

# chemistry (direct measurements) data 
full_chemistry <- read_excel("data/2019_nitrogen_chemistry.xlsx")

# spectral estimates of plant traits 
full_spectra <- read_csv("data/spectra_traits_dry.csv")

### data cleaning to combine ---- 

unique (full_chemistry$Nitrogen)

chemistry <- full_chemistry %>% 
  rename (sample = Sample_ID) %>% 
  rename (chemistry_n = Nitrogen) %>% 
  filter (chemistry_n != "NE") %>% 
  mutate (chemistry_n = as.numeric(chemistry_n))

spectra <- full_spectra %>% 
  select (Sample, t_mean_Nitrogen) %>% 
  rename (sample = Sample) %>% 
  rename (spectral_n = t_mean_Nitrogen)

comparison <- chemistry %>% 
  left_join (spectra, by = "sample") %>% 
  filter (!is.na(spectral_n)) %>% 
  mutate (x = chemistry_n)


### Plot ----
str(comparison)

(validation_plot <- ggplot (comparison, aes(x = spectral_n, y = chemistry_n)) +
  geom_point () + 
   #geom_abline (slope = 1, intercept = 0, col = "red", size = 1) +
    #geom_line(data = theoretical_mod, aes(y = spectral_n), color = "red") +
    geom_smooth (method = lm) +
    geom_smooth (method = lm, formula = y~x-1, col = "red", linetype = "dashed", se = FALSE) +
    geom_abline (intercept = 0, slope = 1, col = "darkgreen", lwd = 1)+
    coord_cartesian(xlim = c(0.2, 4), ylim = c(0.2, 4)) +
  theme_classic ())

ggsave (validation_plot, filename = "graphs/validation_plot.png")


 ### Math ----

best_fit <- lm(chemistry_n ~ spectral_n, data = comparison)
summary (best_fit)
# R2 is 0.6299

# root mean square error 
sqrt(mean((comparison$chemistry_n - comparison$spectral_n)^2))
# 0.6679244


# I overcomplicated things but I don't want to fully delete this code 
one_to_one <- lm (chemistry_n ~  x, data = comparison)
intercept <- coef(one_to_one)[1]
print(intercept)
slope <- coef(one_to_one)[2]
print (slope)

summary(real_mod)
summary (theoretical_mod)

aov <- anova (best_fit, one_to_one)
print(aov)


# Compute the log-likelihood values for both models

lrtest(one_to_one, best_fit)
# p value <0.0001
#  strong evidence to suggest that theoretical mod provides a significantly 
#  better fit to the data  compared to real mod
# so the 1:1 is better than the model fit by 


jtest(one_to_one, best_fit)

coxtest(one_to_one, best_fit)
