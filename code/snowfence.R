# Snowfence data wrangling, modelling and graphing
# Else Radeloff 
# 26 Jan 2024

# Create combined snowfence data table 

library (tidyverse)
library (readr)
library (lubridate)
library (brms)  # for bayesian models 
library (ggeffects)
library (tidybayes)


# Reading in data ----

# Snow fence data 
toolik_raw <- read.csv('data/toolik.csv')
greenland_raw <- read_csv('data/greenland.csv')
svalbard_raw <- read_csv("data/svalbard.csv")

# Cleaning toolik data and creating new 'toolik' object for graphing
toolik <- toolik_raw %>% 
  # delete unnecessary columns 
  select ( -c (ID..Full, Date.Sampled, X.C)) %>% 
  # rename columns
  rename (doy = Julian.Date, plot = Plot, treat = Snow.Zone, n= X.N, plant_type = Functional.Group, year = Year.Sampled, species = Species) %>% 
  # standardizing plant type names 
  mutate (plant_type =ifelse(plant_type == "Deciduous Shrub", "Deciduous shrub", plant_type)) %>% 
  # deal with implicit nesting of plots within treatments 
  mutate (plot = ifelse(treat == "snow", paste0(plot, "_s"), plot)) %>%  # mark snow plots with a '_s'
  mutate (plot = ifelse(treat == "ambient", paste0(plot, "_a"), plot)) %>%   # mark ambient plots with a '_a'
  filter (treat != "Low")

# Further cleaning to match greenland data 
toolik_cleaned <- toolik %>% 
  # delete unnecessary columns 
  select ( -c (plant_type)) %>% 
  # reorder columns 
  relocate (plot, .after = doy) %>% 
  # add location column that says toolik 
  add_column (location = 'toolik') 


# Creating a toolik dataframe with just salix 
toolik_salix <- toolik_raw %>% 
  # filtering to just salix rows 
  filter (Species == "Salix") %>% 
  # delete unnecessary columns 
  select ( -c (ID..Full, Date.Sampled, Functional.Group, Species, Year.Sampled, X.C)) %>%  
  # rename columns
  rename (doy = Julian.Date, plot = Plot, treat = Snow.Zone, n= X.N) %>% 
  # Remove -snow treatment
  filter ( ! treat == "Low") %>% 
  # Rename snow treatments to match greenland data
  mutate(treat = ifelse(treat == "Intermediate", "snow", treat)) %>% 
  mutate(treat = ifelse(treat == "Ambient", "ambient", treat)) %>% 
  # add location column that says toolik 
  add_column (location = 'toolik')

# Cleaning greenland data - just adding location so they can be combined w/ toolik
greenland <- greenland_raw %>% 
  add_column (location = 'greenland')

# Cleaning svalbard data 
svalbard <- svalbard_raw %>% 
  # delete unnecessary columns 
  select ( c(Date, Fence, Regime, N)) %>% 
  # rename columns 
  rename (doy = Date, plot = Fence, treat = Regime, n= N) %>% 
  # convert dates to doy 
  mutate (doy = yday(doy)) %>% 
  # add location column 
  add_column (location = 'svalbard') 

# Combining greenland and toolik but only salix data
snowfence_salix <- rbind(greenland, toolik_salix, svalbard)

snowfence_salix <- snowfence_salix %>% 
  # remove the 'low' snow treatment from toolik for comparability
  filter ( ! treat == "Low") %>% 
  # rename snow treatments for consistency 
  mutate(treat = ifelse(treat == "Intermediate", "snow", treat)) %>% 
  mutate(treat = ifelse(treat == "Ambient", "ambient", treat)) %>% 
  mutate (treat = ifelse(treat == "Normal", "ambient", treat)) %>% 
  mutate (treat = ifelse(treat == "Deep", "snow", treat)) %>% 
  # need to deal with implicit nesting in the plot column
  mutate (plot = ifelse(location == "greenland", paste0(plot, "g"), plot)) %>% # mark greenland plots with a "g"
  mutate (plot = ifelse(location == "svalbard", paste0(plot, "s"), plot)) %>%  # mark svalbard plots with a "s"
  mutate (plot = ifelse(location == "toolik", paste0(plot, "t"), plot)) %>%  # mark toolik plots with a "t"
  mutate (plot = ifelse(treat == "snow", paste0(plot, "_s"), plot)) %>%  # mark snow plots with a '_s'
  mutate (plot = ifelse(treat == "ambient", paste0(plot, "_a"), plot)) %>%   # mark ambient plots with a '_a'
# so a plot that is '4g_s' is a plot 4 at greenland in snow treatment
# and plot '1s_a' is plot 1 at svalbard in an ambient snow treatment 
# although the svalbard plots already had a more unique naming system
  # adding a categorical 'season' to replace doy as a random effect 
  mutate (season = ifelse (doy > 222 , "late", NA)) %>% 
  mutate (season = ifelse (doy <= 222 , "mid", season)) %>% 
  mutate (season = ifelse (doy <= 200 , "early", season))

#table (snowfence_salix$season)  # yay roughly equal numbers in each season



#Models ----


### Location model ----

location_mod <- brm(n ~ treat * location + (1|plot) + (1|doy), data = snowfence_salix)

summary (location_mod)

plot (location_mod)
pairs (location_mod)
pp_check(location_mod)  # posterior predictive checks

random_effects_snowfence_location <- ranef(location_mod)
print(random_effects_snowfence_location)

# save model 
saveRDS(location_mod, "models/snowfence_location_mod.RDS")

location_mod <- readRDS ("models/snowfence_location_mod.RDS")


## Species model ----

species_mod <- brm(n ~ treat * plant_type + (1|plot) + (1|doy), data = toolik)

summary (species_mod)

plot (species_mod)  # caterpillar plots
pp_check(species_mod)  # posterior predictive checks

# save model 
saveRDS(species_mod, "models/snowfence_pft_mod.RDS")

species_mod <- readRDS ('models/snowfence_pft_mod.RDS')


# Graphing model reults ---- 

## location mod ----
location_mod <- readRDS("models/snowfence_location_mod.RDS")

# extract fixed effects
location_mod_fit <- as.data.frame(fixef(location_mod))
location_mod_fit2 <- location_mod_fit %>% rownames_to_column("Treatment")
location_mod_fit3 <- location_mod_fit2
# adding things together
# adding location effect to interactive snow treatments at locations 
location_mod_fit3$Estimate[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']

location_mod_fit3$Estimate[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']

# adding intercept to everything 
location_mod_fit3$Estimate <- location_mod_fit3$Estimate + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Estimate[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] /2

location_mod_fit3$Q2.5 <- location_mod_fit3$Q2.5 + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'Intercept'] - location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept']

location_mod_fit3$Q97.5 <- location_mod_fit3$Q97.5 + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'Intercept'] - location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept']

# renaming treatments 
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'Intercept'] <- 'Greenland Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'locationsvalbard'] <- 'Svalbard Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'locationtoolik'] <- 'Toolik Field station Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow'] <- 'Greenland Deep Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard'] <- 'Svalbard Deep Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow:locationtoolik'] <- 'Toolik Field Station Deep Snow'

location_mod_fit3 <- location_mod_fit3 %>% 
  mutate (Location  = case_when (
    Treatment == "Greenland Ambient Snow" ~ "Greenland",
    Treatment == "Greenland Deep Snow" ~ "Greenland",
    Treatment == "Svalbard Ambient Snow" ~ "Svalbard",
    Treatment == "Svalbard Deep Snow" ~ "Svalbard",
    Treatment == "Toolik Field station Ambient Snow" ~ "Toolik",
    Treatment == "Toolik Field Station Deep Snow" ~ "Toolik",
    TRUE ~ Treatment
  )) %>% 
  mutate (Snow  = case_when (
    Treatment == "Greenland Ambient Snow" ~ "Ambient",
    Treatment == "Greenland Deep Snow" ~ "Deep",
    Treatment == "Svalbard Ambient Snow" ~ "Ambient",
    Treatment == "Svalbard Deep Snow" ~ "Deep",
    Treatment == "Toolik Field station Ambient Snow" ~ "Ambient",
    Treatment == "Toolik Field Station Deep Snow" ~ "Deep",
    TRUE ~ Treatment
  ))

# plotting!!!!!!!!!!!!!!!!!!
(location_mod_plot <- ggplot (location_mod_fit3, aes (x = Location, y = Estimate, color = Treatment)) +
    geom_point (size = 7,  position = position_dodge(width = 0.5)) +
    geom_errorbar (aes(ymin = Q2.5, ymax = Q97.5), size = 1.5, width = 0.5,  position = 'dodge') +
    scale_color_manual(values = c("#E283AA", "#882255", "#A0CCFF", "#6699CC", "#CE7B75", "#661100")) +
    theme_classic()+
    ylab ("Leaf Nitrogen Concentration (%)") +
    theme(legend.title = element_blank(),
          axis.text = element_text (size = 18),
          axis.title=element_text(size=18,face="bold"),
          legend.text = element_text (size = 18),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave(location_mod_plot, filename = "graphs/snowfence_location_mod_plot.png")  


## species mod ----

species_mod <- readRDS ("models/snowfence_pft_mod.RDS")

species_mod_fit <- as.data.frame(fixef(species_mod))
species_mod_fit2 <- species_mod_fit %>% rownames_to_column("Treatment")
species_mod_fit3 <- species_mod_fit2 %>% 
  # add intercept to everything
  mutate (Estimate = case_when(
    #Treatment == "treatIntermediate" 
     Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Estimate + Estimate[Treatment == "Intercept"],
    TRUE ~ Estimate  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
    mutate (Estimate = case_when (
      Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
      ~ Estimate + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
      TRUE ~ Estimate
    )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Estimate = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Estimate + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Estimate )) %>% 
  ## same but for Q2.5 (lower bound)
  mutate (Q2.5 = case_when(
    Treatment == "treatIntermediate" 
    | Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Q2.5 + Estimate[Treatment == "Intercept"],
    TRUE ~ Q2.5  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
  mutate (Q2.5 = case_when (
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
    ~ Q2.5 + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q2.5
  )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Q2.5 = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Q2.5 + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q2.5 )) %>% 
  ## same but for Q97.5
  mutate (Q97.5 = case_when(
    Treatment == "treatIntermediate" 
    | Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Q97.5 + Estimate[Treatment == "Intercept"],
    TRUE ~ Q97.5  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
  mutate (Q97.5 = case_when (
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
    ~ Q97.5 + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q97.5
  )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Q97.5 = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Q97.5 + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q97.5 )) %>% 
  mutate (Estimate = case_when(
    Treatment == "treatIntermediate" 
    ~ Estimate + Estimate[Treatment == "Intercept"],
    TRUE ~ Estimate )) %>% 
  mutate (Treatment = case_when(
    Treatment == "Intercept" ~ "Deciduous shrub ambient snow",
    Treatment == "treatIntermediate" ~ "Deciduous shrub deep snow",
    Treatment == "plant_typeEvergreendwarfshrub" ~ "Evergreen shrub ambient snow",
    Treatment == "plant_typeSedge" ~ "Graminoid ambient snow",
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub" ~ "Evergreen shrub deep snow",
    Treatment == "treatIntermediate:plant_typeSedge" ~ "Graminoid deep snow",
    TRUE ~ Treatment)) %>% 
  mutate (plant_type = case_when(
    Treatment == "Deciduous shrub ambient snow" ~ "Deciduous shrub",
    Treatment == "Deciduous shrub deep snow" ~ "Deciduous shrub",
    Treatment == "Evergreen shrub ambient snow" ~ "Evergreen shrub",
    Treatment == "Evergreen shrub deep snow" ~ "Evergreen shrub",
    Treatment == "Graminoid ambient snow" ~ "Graminoid",
    Treatment == "Graminoid deep snow" ~ "Graminoid",
    TRUE ~ Treatment)) %>% 
  mutate (Snow = case_when(
    Treatment == "Deciduous shrub ambient snow" ~ "Ambient",
    Treatment == "Deciduous shrub deep snow" ~ "Deep",
    Treatment == "Evergreen shrub ambient snow" ~ "Ambient",
    Treatment == "Evergreen shrub deep snow" ~ "Deep",
    Treatment == "Graminoid ambient snow" ~ "Ambient",
    Treatment == "Graminoid deep snow" ~ "Deep",
    TRUE ~ Treatment))



(species_mod_plot <- ggplot (species_mod_fit3, aes (x = plant_type, y = Estimate, color = Treatment)) +
    geom_point (size = 7,  position = position_dodge(width = 0.5)) +
    geom_errorbar (aes(ymin = Q2.5, ymax = Q97.5), size = 1.5, width = 0.5,  position = 'dodge') +
   scale_color_manual(values = c("#72C182", "#117733", "#928BD9", "#332288", "#F292E0", "#AA4499")) +
   xlab ("Plant Functional Type") + 
   ylab ("Leaf Nitrogen Concentration (%)") +
    theme_classic()) +
  theme(legend.title = element_blank(),
        legend.position = "right",  # Move legend to the right
        axis.text = element_text (size = 18),
        axis.title = element_text(size=18,face="bold"),
        legend.text = element_text (size = 18),
        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
        axis.title.y = element_text(margin = margin(r = 20)))

ggsave(species_mod_plot, filename = "graphs/snowfence_species_mod_plot.png")  


