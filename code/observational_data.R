# Else Radeloff 
# Feb 9th 2024

# Looking at leaf n vs. precipitation 
# data from TTT and TRY databases, and Madelaine Anderson 
# Precipitation data from WorldClim 
# snowmelt data recorded in excel based on visual analysis of Landsat and Sentinel imagery using NASA Worldview


# Loading packages and data ---- 

library (terra)
library (tidyverse)
library (readxl)
library (geodata)
library (readr)
#library(nortest)
library (brms)
library (tidybayes)
library (lubridate)

# load data 

# TRY data
load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/try_for_else (2).RData")

#TTT data
TTT_cleaned_dataset_v1 <- read_csv("data/TTT_cleaned_dataset_v1.csv")

#  load Madi's data
madi_raw <- read_csv("data/madi.csv")

# spectra plot info 
above_plots <- read_csv("data/ABoVE_plots.csv")
above_points <- read_csv("data/ABoVE_points.csv")

# spectra data 
dry_spectra <- read_csv("data/spectra_traits_dry.csv")

# load functional trait groupings 
func_groups <- read.csv ("data/func_groups.csv")

# load WorldClim data with the geodata library 
# global precipitation data with 1 km resolution 
precip_year <- worldclim_global (var = "prec", res = 0.5, path = 'data')

# coarser data for mapping (otherwise it's really slow)
precip_year_10km <- worldclim_global (var = "prec", res = 10, path = 'data')


# define winter months (october - april)
months <- c(10,11,12,1,2,3,4)

# select precipitation data from winter months

precip_winter <- precip_year[[months]]
precip_winter_10km <- precip_year_10km[[months]]

# average precipitation across the winter to obtain one value per pixel 
# this ones slow 
precip <- mean(precip_winter)
precip_10km <- mean(precip_winter_10km)

# load arctic zones map (high arctic, low arctic, sub arctic)
high_low_arc <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")

# snowfence site locations (for mapping)
snowfence_site <- read_xlsx ("data/snowfence_sites.xlsx")

#Define projections
WGSCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Data Wrangling ----
 
# coordinates from google maps 
# 60°58'48.4"N 138°24'07.1"W - Kluane
# lat = 60.98, lon = 128.4019 Kluane 
# 69°34'45.9"N 138°53'38.2"W - Qikiqtaruk 
# lat = 69.5791, lon = 138.8939 Qikiqtaruk 

# tidy Madi's data to match other spreadsheets 
madi <- madi_raw %>% 
  # adding lat and lon by site 
  mutate (lat = case_when (
    grepl ("Kluane Plateau", site) ~ 60.98,
    grepl ("Qikiqtaruk", site) ~ 69.5971)) %>% 
  mutate (lon = case_when (
    grepl ("Kluane Plateau", site) ~ -138.5971,
    grepl ("Qikiqtaruk", site) ~ -138.8939)) %>% 
  # adding deciduous / evergreen info to shrub data 
  mutate (functional_group = case_when(
    grepl ("Cassiope", latin.genus) ~ "evergreen shrub",
    grepl ("Salix", latin.genus) ~ "deciduous shrub",
    grepl ("Betula", latin.genus) ~ "deciduous shrub",
    grepl ("Dryas integrifolia Vahl", species) ~ "evergreen shrub",
    TRUE ~ functional_group)) %>% 
  # change forbs to herbs to match spreadsheet
   mutate (functional_group = ifelse (functional_group == "forb", "herb", functional_group)) %>% 
  # removing columns to match the ttt and try data 
  select (-c(latin.species, measurement.date, site, percent_C, ...1)) %>% 
  # renaming nitrogen column
  rename (n = percent_N) %>% 
  rename (Genus = latin.genus) %>% 
  filter (functional_group != "herb") %>% 
  # adding a column for data source 
  mutate (data_source = "Madi")
  
  
# clean ttt dataset (select n observations, remove experimental plots, assign PFTs, etc)
ttt_n <- TTT_cleaned_dataset_v1 %>% 
  # select rows with nitrogen observations 
  filter (Trait == "Leaf nitrogen (N) content per leaf dry mass") %>% 
  # select only 'control' plots
  filter(Treatment != "warmed" 
         & Treatment != "N fertilized"
         & Treatment != "greenhouse"
         & Treatment != "N + warmed") %>% 
  # rename species column to match spreadsheet w/ pfts 
  rename (Species = AccSpeciesName) %>% 
  # join ttt data with pft spreadsheet to assign a functional group to each observation 
  left_join (func_groups, by = "Species") %>% 
  # remove trees 
  filter (PlantGrowthForm != "tree") %>% 
  # add deciduou/ evergreen info
  mutate (PlantGrowthForm = ifelse (PlantGrowthForm == "shrub", paste(LeafPhenology, PlantGrowthForm), PlantGrowthForm)) %>% 
  # add a column for data source 
  mutate (data_source = "Tundra Trait Team") %>% 
  # assign plant funcitonal group to species not specified in spreadsheet 
  mutate (PlantGrowthForm = case_when(
    grepl ("Lysimachia europaea", Species) ~ "herb",
    grepl ("Poa sp.", Species) ~ "graminoid",
    grepl ("Betula divaricata", Species) ~ "deciduous shrub",
    grepl ("Alchemilla sp", Species) ~ "herb",
    grepl ("Lactuca alpina", Species) ~ "herb",
    grepl ("Eutrema edwardsii", Species) ~ "herb",
    grepl ("Erigeron humilis", Species) ~ "herb",
    grepl ("Minuartia rossii", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Koenigia hadacii", Species) ~ "herb",
    grepl ("Saxifraga flagellaris", Species) ~ "herb",
    grepl ("Taraxacum brachyceras", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Cochlearia groenlandica", Species) ~ "herb",
    grepl ("Festuca sp.", Species) ~ "graminoid",
    grepl ("Potentilla rubricaulis", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Dupontia sp.", Species) ~ "graminoid",
    grepl ("Pedicularis dasyantha", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene uralensis", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Draba sp.", Species) ~ "herb",
    grepl ("Draba hirta", Species) ~ "herb",
    grepl ("Saxifraga hieraciifolia", Species) ~ "herb",
    grepl ("Saxifraga tenuis", Species) ~ "herb",
    grepl ("Carex marina", Species) ~ "graminoid",
    grepl ("Campanula gieseckeana", Species) ~ "herb",
    grepl ("Saxifraga foliolosa", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Carex sp.", Species) ~ "graminoid",
    grepl ("Salix sp.", Species) ~ "shrub",
    grepl ("Valeriana capitata", Species) ~ "herb",
    grepl ("Sedum roseum", Species) ~ "herb",
    grepl ("Taraxacum sp.", Species) ~ "herb",
    grepl ("Harrimanella hypnoides", Species) ~ "herb",
    grepl ("Persicaria vivipara", Species) ~ "herb",
    grepl ("Ranunculus pygmaeus", Species) ~ "herb",
    grepl ("Papaver radicatum", Species) ~ "herb",
    grepl ("Potentilla pulchella", Species) ~ "herb",
    grepl ("Polemonium boreale", Species) ~ "herb",
    grepl ("Chrysosplenium tetrandrum", Species) ~ "herb",
    grepl ("Ranunculus spitsbergensis", Species) ~ "herb",
    grepl ("Taraxacum arcticum", Species) ~ "herb",
    grepl ("Antennaria alpina", Species) ~ "herb",
    grepl ("Rubus chamaemorous", Species) ~ "herb",
    grepl ("Andromeda polifolia", Species) ~ "deciduous shrub",
    grepl ("Linnaea borealis", Species) ~ "herb",
    grepl ("Artemisia ordosica", Species) ~ "shrub",
    grepl ("Artemisia wellbyi", Species) ~ "shrub",
    grepl ("Lespedeza davurica", Species) ~ "herb",
    grepl ("Orthilia secunda", Species) ~ "herb",
    grepl ("Rubus saxatilis", Species) ~ "deciduous shrub",
    grepl ("Euphorbia clusiaefolia", Species) ~ "herb",
    grepl ("Symphyotrichum oolentangiense", Species) ~ "herb",
    grepl ("Helianthemum bicknellii", Species) ~ "herb",
    grepl ("Artemisia ludoviciana", Species) ~ "herb",
    grepl ("Helianthemum nummularium", Species) ~ "evergreen shrub",
    grepl ("Linum suffruticosum", Species) ~ "herb",
    grepl ("Rubus vestitus", Species) ~ "deciduous shrub",
    grepl ("Rubus ulmifolius", Species) ~ "deciduous srhub",
    grepl ("Solanum dulcamara", Species) ~ "herb",
    grepl ("Rubus pubescens", Species) ~ "herb",
    grepl ("Aralia nudicaulis", Species) ~ "herb",
    grepl ("Aralia racemosa", Species) ~ "herb",
    grepl ("Pedicularis canadensis", Species) ~ "herb",
    grepl ("Rubus caesius", Species) ~ "deciduous shrub",
    grepl ("Rubus canascens", Species) ~ "deciduous shrub",
    grepl ("Acinos alpinus", Species) ~ "herb",
    grepl ("Euphorbia characias", Species) ~ "herb",
    grepl ("Plomis fructicosa", Species) ~ "evergreen shrub",
    grepl ("Psoralea bituminosa", Species) ~ "herb",
    grepl ("Stackhousia brunonis", Species) ~ "herb",
    grepl ("Isotropis cuneifolia", Species) ~ "herb",
    grepl ("Mitchella repens", Species) ~ "herb",
    grepl ("Rubus idaeus", Species) ~ "deciduous shrub",
    grepl ("Andromeda glaucophylla", Species) ~ "evergreen shrub",
    grepl ("Rubus hawaiiensis", Species) ~ "herb",
    grepl ("Rubus plicatus", Species) ~ "deciduous shrub",
    grepl ("Rubus hispidus", Species) ~ "deciduous shrub",
    grepl ("Eremophila glabra", Species) ~ "deciduosus shrub",
    grepl ("Acamptopappus shockleyi", Species) ~ "herb",
    grepl ("Thymus praecox", Species) ~ "evergreen shrub",
    grepl ("Genista tinctoria", Species) ~ "deciduous shrub",
    grepl ("Rubus odoratus", Species) ~ "deciduous shrub",
    grepl ("Rubus allegheniensis", Species) ~ "deciduous shrub",
    grepl ("Petalostemum candidum", Species) ~ "herb",
    grepl ("Reynoutria japonica", Species) ~ "herb",
    grepl ("Rubus ursinus", Species) ~ "herb",
    grepl ("Lobelia yuccoides", Species) ~ "deciduous shrub",
    grepl ("Reynoutria sachalinensis", Species) ~ "herb",
    grepl ("Ageratina adenophora", Species) ~ "deciduous shrub",
    grepl ("Rubus arcticus", Species) ~ "deciduous shrub",
    grepl ("Betula nana", Species) ~ "deciduous shrub",
    grepl ("Betula fruticosa", Species) ~ "deciduous shrub",
    grepl ("Betula glandulosa", Species) ~ "deciduous shrub",
    grepl ("Betula pumila", Species) ~ "deciduous shrub",
    grepl ("Dryas octopetala", Species) ~ "evergreen shrub",
    grepl ("Rubus chamaemorus", Species) ~ "herb",
    grepl ("Salix glauca", Species) ~ "deciduous shrub",
    grepl ("Alnus incana", Species) ~ "deciduous shrub",
    grepl ("Sorbus aucuparia", Species) ~ "tree",
    grepl ("Juniperus communis", Species) ~ "evergreen shrub",
    grepl ("Salix pulchra", Species) ~ "deciduous shrub",
    grepl ("Celmisia brevifolia", Species) ~ "herb",
    grepl ("Celmisia sessiliflora", Species) ~ "herb",
    grepl ("Senecio kolenatianus", Species) ~ "herb",
    grepl ("Thymus nummularius", Species) ~ "herb",
    grepl ("Alchemilla fallax", Species) ~ "herb",
    grepl ("Hedysarum hedysaroides", Species) ~ "herb",
    grepl ("Podospermum canum", Species) ~ "herb",
    grepl ("Primula integrifolia", Species) ~ "herb",
    TRUE ~ PlantGrowthForm))

## clean ttt dataset (select n observations, remove experimental plots, assign PFTs, etc)
try_n <- mTRY2 %>% 
  # select only nitrogen measurements 
  filter (DataName == "Leaf nitrogen content per dry mass (Nmass)") %>%
  # rename species column so it matches plant functional group sheet
  rename (Species = AccSpeciesName) %>%   
  # merge sheets so that each observation has a plant functional type associated with it 
  left_join (func_groups, by = "Species") %>% 
  # add a column for data source 
  mutate (data_source = "TRY") %>% 
  # remove observations without a species 
  filter (Species != "Unidentified species") %>% 
  # remove trees 
  filter (PlantGrowthForm != "tree") %>% 
  # add deciduou/ evergreen info
  mutate (PlantGrowthForm = ifelse (PlantGrowthForm == "shrub", paste(LeafPhenology, PlantGrowthForm), PlantGrowthForm)) %>% 
  # assign plant growth forms for the species not in the spreadsheet already
  mutate (PlantGrowthForm = case_when(
    grepl ("Lysimachia europaea", Species) ~ "herb",
    grepl ("Poa sp.", Species) ~ "graminoid",
    grepl ("Poa", Species) ~ "graminoid",
    grepl ("Betula divaricata", Species) ~ "deciduous shrub",
    grepl ("Alchemilla sp", Species) ~ "herb",
    grepl ("Lactuca alpina", Species) ~ "herb",
    grepl ("Eutrema edwardsii", Species) ~ "herb",
    grepl ("Erigeron humilis", Species) ~ "herb",
    grepl ("Minuartia rossii", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Koenigia hadacii", Species) ~ "herb",
    grepl ("Saxifraga flagellaris", Species) ~ "herb",
    grepl ("Taraxacum brachyceras", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Cochlearia groenlandica", Species) ~ "herb",
    grepl ("Festuca sp.", Species) ~ "graminoid",
    grepl ("Potentilla rubricaulis", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Dupontia sp.", Species) ~ "graminoid",
    grepl ("Pedicularis dasyantha", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene uralensis", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Draba sp.", Species) ~ "herb",
    grepl ("Draba hirta", Species) ~ "herb",
    grepl ("Saxifraga hieraciifolia", Species) ~ "herb",
    grepl ("Saxifraga tenuis", Species) ~ "herb",
    grepl ("Carex marina", Species) ~ "graminoid",
    grepl ("Campanula gieseckeana", Species) ~ "herb",
    grepl ("Saxifraga foliolosa", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Carex sp.", Species) ~ "graminoid",
    grepl ("Salix sp.", Species) ~ "deciduous shrub",
    grepl ("Valeriana capitata", Species) ~ "herb",
    grepl ("Sedum roseum", Species) ~ "herb",
    grepl ("Taraxacum sp.", Species) ~ "herb",
    grepl ("Harrimanella hypnoides", Species) ~ "herb",
    grepl ("Festuca ovina subvar. novae-zelandiae", Species) ~ "herb",
    grepl ("Agrostis", Species) ~ "graminoid",
    grepl ("Stipa", Species) ~ "graminoid",
    grepl ("Marram", Species) ~ "graminoid",
    grepl ("Agropyron", Species) ~ "graminoid",
    grepl ("Festuca", Species) ~ "graminoid",
    grepl ("Koeleria", Species) ~ "graminoid",
    grepl ("Hierochloe", Species) ~ "graminoid",
    grepl ("Bromus", Species) ~ "graminoid",
    grepl ("Elymus", Species) ~ "graminoid",
    grepl ("Panicum", Species) ~ "graminoid",
    grepl ("Bouteloua", Species) ~ "graminoid",
    grepl ("Eragrostis", Species) ~ "graminoid",
    grepl ("Betula", Species) ~ "deciduous shrub",
    grepl ("Ledum palustre subsp. groenlandicum", Species) ~ "evergreen shrub",
    grepl ("Empetrum nigrum subsp. hermaphroditum", Species) ~ "evergreen shrub",
    grepl ("Salix", Species) ~ "deciduous shrub",
    grepl ("VACCINIUM VITIS-IDAEA", Species) ~ "evergreen shrub",
    grepl ("Carex", Species) ~ "graminoid",
    grepl ("Picea", Species) ~ "tree",
    grepl ("Luzula", Species) ~ "graminoid",
    grepl ("Vaccinium", Species) ~ "deciduous shrub",
    grepl ("Betula pubescens var. pumila", Species) ~ "tree",
    grepl ("ARCTOSTAPHYLOS UVA-URSI", Species) ~ "evergreen shrub",
    grepl ("Kobresia", Species) ~ "graminoid",
    grepl ("Aster", Species) ~ "herb",
    grepl ("Triticum", Species) ~ "graminoid",
    grepl ("Larix x eurolepis", Species) ~ "tree",
    grepl ("Potentilla bifurca", Species) ~ "herb",
    grepl ("Potentilla fructicosa", Species) ~ "herb",
    grepl ("Saussurea", Species) ~ "herb",
    grepl ("Rubus", Species) ~ "deciduous shrub",
    grepl ("Papaver radicatum", Species) ~ "herb",
    grepl ("Oxytropis deflexa", Species) ~ "herb",
    grepl ("Crepis sibirica", Species) ~ "herb",
    grepl ("Paeonia anomala", Species) ~ "herb",
    grepl ("Artemisia norvegica", Species) ~ "herb",  
    grepl ("Stellaria bungeana", Species) ~ "herb",
    grepl ("Persicaria vivipara", Species) ~ "herb",
    grepl ("Astragalus umbellatus", Species) ~ "herb",
    grepl ("Dryas octopetala", Species) ~ "deciduous shrub",
   grepl ("Rubus chamaemorous", Species) ~ "herb",
   grepl ("Andromeda polifolia", Species) ~ "deciduous shrub",
   grepl ("Linnaea borealis", Species) ~ "herb",
   grepl ("Artemisia ordosica", Species) ~ "deciduous shrub",
   grepl ("Artemisia wellbyi", Species) ~ "deciduous shrub",
   grepl ("Lespedeza davurica", Species) ~ "herb",
   grepl ("Orthilia secunda", Species) ~ "herb",
   grepl ("Rubus saxatilis", Species) ~ "deciduous shrub",
   grepl ("Euphorbia clusiaefolia", Species) ~ "herb",
   grepl ("Symphyotrichum oolentangiense", Species) ~ "herb",
   grepl ("Helianthemum bicknellii", Species) ~ "herb",
   grepl ("Artemisia ludoviciana", Species) ~ "herb",
   grepl ("Helianthemum nummularium", Species) ~ "evergreen shrub",
   grepl ("Linum suffruticosum", Species) ~ "herb",
   grepl ("Rubus vestitus", Species) ~ "deciduous shrub",
   grepl ("Rubus ulmifolius", Species) ~ "deciduous srhub",
   grepl ("Solanum dulcamara", Species) ~ "herb",
   grepl ("Rubus pubescens", Species) ~ "herb",
   grepl ("Aralia nudicaulis", Species) ~ "herb",
   grepl ("Aralia racemosa", Species) ~ "herb",
   grepl ("Pedicularis canadensis", Species) ~ "herb",
   grepl ("Rubus caesius", Species) ~ "deciduous shrub",
   grepl ("Rubus canascens", Species) ~ "deciduous shrub",
   grepl ("Acinos alpinus", Species) ~ "herb",
   grepl ("Euphorbia characias", Species) ~ "herb",
   grepl ("Phlomis fruticosa", Species) ~ "evergreen shrub",
   grepl ("Psoralea bituminosa", Species) ~ "herb",
   grepl ("Stackhousia brunonis", Species) ~ "herb",
   grepl ("Isotropis cuneifolia", Species) ~ "herb",
   grepl ("Mitchella repens", Species) ~ "herb",
   grepl ("Rubus idaeus", Species) ~ "deciduous shrub",
   grepl ("Andromeda glaucophylla", Species) ~ "evergreen shrub",
   grepl ("Rubus hawaiiensis", Species) ~ "herb",
   grepl ("Rubus plicatus", Species) ~ "deciduous shrub",
   grepl ("Rubus hispidus", Species) ~ "deciduous shrub",
   grepl ("Eremophila glabra", Species) ~ "deciduosus shrub",
   grepl ("Acamptopappus shockleyi", Species) ~ "herb",
   grepl ("Thymus praecox", Species) ~ "evergreen shrub",
   grepl ("Genista tinctoria", Species) ~ "deciduous shrub",
   grepl ("Rubus odoratus", Species) ~ "deciduous shrub",
   grepl ("Juniperus communis", Species) ~ "evergreen shrub",
   grepl ("Rubus allegheniensis", Species) ~ "deciduous shrub",
   grepl ("Petalostemum candidum", Species) ~ "herb",
   grepl ("Reynoutria japonica", Species) ~ "herb",
   grepl ("Rubus ursinus", Species) ~ "herb",
   grepl ("Lobelia yuccoides", Species) ~ "deciduous shrub",
   grepl ("Reynoutria sachalinensis", Species) ~ "herb",
   grepl ("Ageratina adenophora", Species) ~ "deciduous shrub",
   grepl ("Rubus arcticus", Species) ~ "deciduous shrub",
   grepl ("Betula nana", Species) ~ "deciduous shrub",
   grepl ("Betula fruticosa", Species) ~ "deciduous shrub",
   grepl ("Betula glandulosa", Species) ~ "deciduous shrub",
   grepl ("Betula pumila", Species) ~ "deciduous shrub",
   grepl ("Salix glauca", Species) ~ "deciduous shrub",
   grepl ("Alnus incana", Species) ~ "deciduous shrub",
   grepl ("Sorbus aucuparia", Species) ~ "tree",
   grepl ("Juniperus comunis", Species) ~ "evergreen shrub",
   grepl ("Salix pulchra", Species) ~ "deciduous shrub",
   grepl ("Dryas octopetala", Species) ~ "evergreen shrub",
   grepl ("Rubus chamaemorus", Species) ~ "herb",
    TRUE ~ PlantGrowthForm)) 

# further cleaning so datasheets can be merged 
ttt <- ttt_n %>% 
  # selecting columns to match Madi's data 
  select(Species, IndividualID, Value,PlantGrowthForm,  Latitude, Longitude, data_source, Genus) %>% 
 # renaming columns to match Madi's data 
   rename (lat = Latitude,
          lon = Longitude,
          functional_group = PlantGrowthForm,
          sample_id = IndividualID,
          n = Value,
          species = Species) %>% 
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  filter (!is.na(lat))

# further cleaning so datasheets can be merged 
try <- try_n %>% 
  # select columns to match Madi's data 
  select (SpeciesName, ObsDataID, StdValue, PlantGrowthForm, Lat, Lon, data_source, Genus) %>% 
  # rename columns to match Madi's data 
  rename (lat = Lat,
          lon = Lon,
          functional_group = PlantGrowthForm,
          sample_id = ObsDataID,
          n = StdValue,
          species = SpeciesName) %>% 
  # remove observations of trees and lichen
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  # remove observations without coordinates associated 
  filter (!is.na(lat))

# combining ttt, try and Madi's data into one big sheet 
obs_n <- rbind (ttt, try, madi) %>% 
  rename (genus = Genus) %>% 
  filter (genus != "Agrostis"
          & genus != "Dactylis"
          & genus != "Melica"
          & genus != "Phalaris"
          & genus != "Trichophorum")

# exploring number of observations per group 
#test_obs <- obs_n %>% 
#  group_by (genus) %>% 
#  tally()

## Wrangling spectra data ----

# adjusting column and plot names 
spectra <- dry_spectra %>% 
  select (Sample, t_mean_Nitrogen, ) %>% 
  rename (sample = Sample, n = t_mean_Nitrogen) %>% 
  # remove 'ABV_' from start 
  mutate (plot = str_remove (sample, "ABV_")) %>% 
  # then make it only the first 8-9 characters 
  mutate (plot = substr(plot, 1, 9))

# selecting and renaming columns in the plot coordinates spreadsheet 
points <- above_points %>% 
  select (Name, Lat, Lon) %>% 
  rename (plot = Name, lat = Lat, lon = Lon)

# selecting and renaming columns in the spreadsheet w plot level info 
plots <- above_plots %>% 
  select (title, lat_9_Plot_Center, long_9_Plot_Center, `10_Plot_Type`, `44_Species_1`, `46_Species_2`, `48_Species_3`) %>% 
  rename (plot = title, 
          lat = lat_9_Plot_Center, 
          lon = long_9_Plot_Center, 
          plot_type = `10_Plot_Type`, 
          species1 = `44_Species_1`, 
          species2=`46_Species_2`, 
          species3 = `48_Species_3`) %>% 
  mutate (species = ifelse (plot_type == "Single Species Patch Tree/Shrub", species1, NA)) %>% 
  mutate (species = ifelse (plot_type == "Single Species Box Ground", species1, species)) 

# adding spectra info to the plot level data 
spectra_plots <- spectra %>% 
  left_join (plots, by = 'plot') %>% 
filter (!is.na(lat)) 

# turning it into an excel spreadsheet so I can add species info 

#write_xlsx(spectra_plots, "data/above_plots_March7.xlsx")

# loading the data back in after editing in excel 
labled_plots  <- read_excel("data/above_plots_species.xlsx")

# a little data cleaning 
labled_plots <- labled_plots %>% 
  filter (!is.na(plant_type)) %>% 
  filter (plant_type != "tree") %>% 
  select (!c(species1, species2, species3)) %>% 
  mutate (plant_type = (ifelse(plant_type == "eergreen shrub", "evergreen shrub", plant_type)))


# Extracting Raster Precipitation Data ----

# creating a file of observation locations for extracting precip data 
obs_sites <- obs_n %>% 
  group_by (lat, lon) %>% 
  mutate (ID = row_number()) %>% 
  ungroup ()

# make a vector layer of the coordinates  
n_points <- vect (obs_n, geom=c("lon", "lat"), crs=WGSCRS)
spectra_vect_points <- vect (labled_plots, geom=c("lon", "lat"), crs=WGSCRS)
snowfence_points <- vect (snowfence_site, geom=c("lon", "lat"), crs = WGSCRS)


# extract mean precipitation 
arctic_precip <- terra::extract(precip, n_points)
spectra_points_precip <- terra::extract(precip, spectra_vect_points)



# add precipitation data to trait datasheet 
precip_trait <- obs_n %>% 
  mutate (ID = row_number()) %>% 
  left_join (arctic_precip, by = "ID") %>% 
  rename (mean_precip = mean) %>% 
  filter (!is.na(mean_precip))

# add precipitation data to spectra trait datasheet 
spectra_precip <- labled_plots %>% 
  mutate (ID = row_number()) %>% 
  left_join (spectra_points_precip, by = "ID") %>% 
  rename (mean_precip = mean) %>% 
  filter (!is.na(mean_precip))


# Add arctic zone to filter out non-arctic sites 

# load arctic zones map (high arctic, low arctic, sub arctic)
zones <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")
#crs (zones)


# changing projections to a top down view of the arctic to match zones map

#re-projecting raster into top down view of the Arctic 
precip_proj <- terra::project(precip_10km, "EPSG:3408")
#crs (precip_proj)


# re-projecting coordinates to the top down view of arctic 
n_points_proj <- project (n_points, "EPSG:3408")
snowfence_points_proj <- project (snowfence_points, "EPSG:3408")
spectra_vect_points_proj <- project (spectra_vect_points, "EPSG:3408")


# extract zone 
n_points_zone <- terra::extract(zones, n_points_proj) %>% 
  select (id.y, Zone) %>% 
  rename (ID = id.y, zone = Zone)

spectra_points_zone <- terra::extract(zones, spectra_vect_points_proj) %>% 
  select (id.y, Zone) %>% 
  rename (ID = id.y, zone = Zone)

# add zone data to trait datasheet 
n_precip_zones <- precip_trait %>%  
  left_join (n_points_zone, by = "ID") %>% 
  # removing points below sub arctic 
  filter (!is.na(zone)) %>% 
  # divide nitrogen by 10 so that units are % instead of mg/g
  mutate (leafn = n/10)

spectra_precip_zones <- spectra_precip %>% 
  left_join (spectra_points_zone, by = "ID") %>% 
  # removing points below sub arctic 
  filter (!is.na(zone)) %>% 
  mutate (zone2 = (ifelse(zone == "High arctic", "Low arctic", zone))) %>% 
  filter (plant_type != "fern" 
          & plant_type != "lichen"
          & plant_type != "moss"
          & plant_type != "herb") %>% 
  # remove undersampled genus's
  filter (Genus != "Chamaedaphne"
          & Genus != "Oxytropis"
          & Genus != "Rosa"
          & Genus != "Sheperdia"
          & !is.na (Genus))

# create a spreadsheet of points to export to excel for assigning snowmelt dates 
obs_points <- n_precip_zones %>% 
  group_by (lat, lon) %>% 
  tally ()

#write_xlsx(obs_points, "data/observation_snowmelt_base.xlsx")
#write_xlsx(spectra_precip_zones, "data/spectra_points_base.xlsx")

# Map ----

# averaging leaf nitrogen by site for mapping  
obs_n_sites <- obs_n %>% 
  group_by (lat, lon) %>% 
  summarise (avg_n = mean (n)) 
#str(obs_n_)

# filtering sites with high, med and low nitrogen averages for mapping
obs_high_n <- obs_n_sites %>% 
  filter (avg_n>30)

obs_med_n <- obs_n_sites %>% 
  filter (avg_n>20) %>% 
  filter (avg_n<30)

obs_low_n <- obs_n_sites %>% 
  filter (avg_n<20)

plots_avg_n <- labled_plots %>% 
  group_by (lat, lon) %>% 
  summarise (avg_n = mean(n))

# same but for spectra points
spectra_high_n_points <- plots_avg_n %>% 
  filter (avg_n >3)

spectra_med_n_points <- plots_avg_n %>% 
  filter (avg_n >2) %>% 
  filter (avg_n <3)

spectra_low_n_points <- plots_avg_n %>% 
  filter (avg_n <2)

# creating vector layers of those points 
points_high_n <- vect (obs_high_n, geom=c("lon", "lat"), crs = WGSCRS)
points_med_n <- vect (obs_med_n, geom=c("lon", "lat"), crs = WGSCRS)
points_low_n <- vect (obs_low_n, geom=c("lon", "lat"), crs = WGSCRS)

spectra_high_n_vect <- vect (spectra_high_n_points, geom=c("lon", "lat"), crs=WGSCRS)
spectra_med_n_vect <- vect (spectra_med_n_points, geom=c("lon", "lat"), crs=WGSCRS)
spectra_low_n_vect <- vect (spectra_low_n_points, geom=c("lon", "lat"), crs=WGSCRS)


# re-projecting those points to EPSG:3408
high_n_proj <- project (points_high_n, "EPSG:3408")
med_n_proj <- project (points_med_n, "EPSG:3408")
low_n_proj <- project (points_low_n, "EPSG:3408")

spectra_high_n_proj <- project (spectra_high_n_vect, "EPSG:3408")
spectra_med_n_proj <- project (spectra_med_n_vect, "EPSG:3408")
spectra_low_n_proj <- project (spectra_low_n_vect, "EPSG:3408")

# Limiting how high precipitation can go to highlight the changes in precip at the lower end
precip_scaled <- clamp (precip_proj, 0, 150)

# rectangle that defines the area we will crop to (xmin, xmax, ymin, ymax)
extent <- ext(-5500000, 5500000, -4500000, 4500000)
precip_crop <- crop(precip_scaled, extent)
pal <- colorRampPalette(c("lightblue", "darkblue"))

# time to actually plot the raster data! 
precip_map <- plot (precip_crop, col = pal(20), axes = FALSE)


# add lat lon lines (graticules)
precip_map <- lines(graticule(lon=seq(0, 360, by=20), lat=seq(0, 90, by=30), crs="EPSG:3408"), col="black", lwd= 0.5)
precip_map <- lines(graticule(lon=20, lat=seq(0, 90, by=20), crs="EPSG:3408"), col="black", lwd= 0.5)

# add obs data points with different sized for different average n values 
precip_map <- points (high_n_proj, cex = 2, col = "#CC6677")
precip_map <- points (med_n_proj, cex = 1.2, col = "#CC6677")
precip_map <- points (low_n_proj, cex = 0.7, col = "#CC6677")

#adding spectra points
precip_map <- points (spectra_high_n_proj, cex = 2, col = "goldenrod2")
precip_map <- points (spectra_med_n_proj, cex = 1.2, col = "goldenrod2")
precip_map <- points (spectra_low_n_proj, cex = 0.7, col = "goldenrod2")

# snowfence sites 
precip_map <- points (snowfence_points_proj, cex = 1.5, col = "#661100", bg = "#661100", pch = 23)

# adding a legend
legend(x = -5500000, y = -1900000,
       legend = c("Observation Data High Leaf N (>3%)", "Observation Data Med Leaf N (2-3%)", "Observation Data Low Leaf N (<2%)", 
                  "Spectra Data High Leaf N (>3%)", "Spectra Data Med Leaf N (2-3%)", "Spectra Data Low Leaf N (<2%)", "Snowfence Points"),
       pch = c(21, 21, 21, 21, 21, 21, 23),
       col = c("#CC6677", "#CC6677",  "#CC6677", "goldenrod2", "goldenrod2", "goldenrod2",  "#661100"),
       pt.bg = c("#CC6677", "#CC6677", "#CC6677", "goldenrod2","goldenrod2","goldenrod2",  "#661100"),
       bg = "white",
       pt.cex = c(2, 1.2, 0.7, 2, 1.2, 0.7, 1.5)
)


ggsave (precip_map, filename = "graphs/precip_map.png")

# Precipitation Models ----

# obs data 
# precipitation and pft as fixed effects 
#precip_pft_mod <- brm (n ~ mean_precip * functional_group + (1|genus), data = n_precip_zones)


# check model outputs and assumptions
summary (precip_pft_mod)
plot (precip_pft_mod) # yay happy fuzzy caterpillars 
pp_check (precip_pft_mod)

# check random effects table 
random_effects_obs_genus <- ranef(precip_pft_mod)
print(random_effects_obs_genus)

# happy model - no divergent transitions, rhats at 1 and lots of ess

# save model because bayesian takes forever to run 
#saveRDS(precip_pft_mod, "models/obs_precip_mod.RDS")

precip_pft_mod <- readRDS("models/obs_precip_mod_1km.RDS")

print(summary(precip_pft_mod), digits = 4)


# Spectra precip model 
spectra_precip_mod <- brm (n ~ mean_precip * plant_type + (1|Genus), data = spectra_precip_zones)

summary (spectra_precip_mod)

plot (spectra_precip_mod)
pairs (spectra_precip_mod)
pp_check(spectra_precip_mod)

random_effects_genus <- ranef(spectra_precip_mod)
print(random_effects_genus)

# save model 
#saveRDS(spectra_precip_mod, "models/spectra_precip_mod.RDS")

spectra_precip_mod <- readRDS("models/spectra_precip_1km_mod.RDS")

print(summary(spectra_precip_mod), digits = 4)

## plotting model results ----

# create a dummy spreadsheet 
obs_precip_mod_data <- expand_grid(mean_precip = seq(0, 90, by = 5), 
                                       n = seq(0, 5, by = 0.5),
                                       functional_group = levels (as.factor(n_precip_zones$functional_group)))
                                       #genus = levels (as.factor(n_precip_zones$genus)))

# fill dummy spreadsheet with model predicitons
obs_precip_mod_pred <- precip_pft_mod %>% 
  epred_draws(newdata = obs_precip_mod_data, allow_new_levels = TRUE)

(obs_precip_mod_fit <- ggplot() +
    geom_point(data = n_precip_zones, aes(x = mean_precip, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
   ylab("Leaf Nitrogen Concentration (%)") + 
   xlab("Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,75)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5)) +
   theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85),
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15)))

ggsave (obs_precip_mod_fit, filename = "graphs/obs_precip_mod_fit.png")

# making a plot without a legend so I can save the two plots together nicely 
(obs_precip_plot_simple <- ggplot() +
    geom_point(data = n_precip_zones, aes(x = mean_precip, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") + 
    xlab("Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,90)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text (size = 15),
          axis.title.x = element_blank(),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15)))


# same for spectra model 

# dummy datasheet 
spectra_precip_mod_data <- expand_grid(mean_precip = seq(4, 36, by = 1), 
                                       n = seq(0, 5, by = 0.5),
                                       plant_type = levels (as.factor(spectra_precip_zones$plant_type)),
                                       Genus = levels (as.factor(spectra_precip_zones$Genus)))

# fill datasheet with model predictions
spectra_precip_mod_pred <- spectra_precip_mod %>% 
  epred_draws(newdata = spectra_precip_mod_data, allow_new_levels = TRUE)

# plot! 
(spectra_precip_mod_fit <- ggplot () +
    geom_point(data = spectra_precip_zones, aes(x = mean_precip, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +   # raw data
    stat_lineribbon(data = spectra_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (plant_type), fill = ordered (plant_type)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    xlab("Mean Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,90)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text (size = 15),
          axis.title=element_text(size=14,face="bold"),
          legend.text = element_text (size = 15)))

# save plot 
ggsave (spectra_precip_mod_fit, filename = "graphs/spectra_precip_mod_fit_pft.png")

# put the two graphs (also obs model) on top of eachother in the same figure 
(precip_combined_plot <- grid.arrange(obs_precip_plot_simple, spectra_precip_mod_fit,  ncol = 1))

ggsave (precip_combined_plot, filename = "graphs/precip_combined_plot.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Snowmelt  ----

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# read in snowmelt data 
try_snowmelt_raw <- read_excel("data/try_snowmelt.xlsx")
ttt_snowmelt_raw <- read_excel("data/ttt_snowmelt.xlsx")

try_snowmelt <- try_snowmelt_raw %>% 
  select (species, year, snowmelt_date, sample_id, data_source) %>% 
  filter (!is.na(snowmelt_date)) %>% 
  mutate (snowmelt_date = yday(snowmelt_date)) %>% 
  #mutate(sample_id = as.character(sample_id)) %>% 
  left_join (n_precip_zones, by = "sample_id") %>% 
  select (-c(species.y, data_source.y)) %>% 
  rename (species = species.x,
          data_source = data_source.x)

ttt_snowmelt <- ttt_snowmelt_raw %>% 
  select (species, year, snowmelt_date, sample_id, data_source) %>% 
  filter (!is.na(snowmelt_date)) %>% 
  mutate (snowmelt_date = yday(snowmelt_date)) %>% 
  #mutate(sample_id = as.character(sample_id)) %>% 
  left_join (n_precip_zones, by = "sample_id") %>% 
  select (-c(species.y, data_source.y)) %>% 
  rename (species = species.x,
          data_source = data_source.x)

madi_snowmelt <- n_precip_zones %>% 
  filter (data_source == "Madi") %>% 
  mutate (snowmelt_date = case_when (
    grepl (60.98, lat) ~ 167,
    grepl (69.5971, lat) ~ 158)) %>% 
  mutate (year = "2021 - 2022")

# combine spreadsheets 
obs_snowmelt <- rbind (try_snowmelt, ttt_snowmelt, madi_snowmelt) %>% 
  filter (!is.na(functional_group)) %>% 
  mutate (leafn = n/10)

# spectra snowmelt data
spectra_snowmelt_full <- read_excel("data/snowmelt.xlsx")

spectra_snowmelt <- spectra_snowmelt_full %>% 
  select (plot, lat, lon, snowmelt_date) %>% 
  mutate (snowmelt_date = yday(snowmelt_date))

spectra_snowmelt_n <- spectra_precip_zones %>% 
  left_join(spectra_snowmelt, by = "plot") %>% 
  filter (!is.na(snowmelt_date)) 

### Models ----

# obs model
obs_snowmelt_mod <- brm (leafn ~ snowmelt_date * functional_group + (1|genus), data = obs_snowmelt)

summary (obs_snowmelt_mod)
plot (obs_snowmelt_mod)
pairs (obs_snowmelt_mod)
pp_check(obs_snowmelt_mod)

random_effects_obs_snowmelt <- ranef(obs_snowmelt_mod)
print(random_effects_obs_snowmelt)

print(summary(obs_snowmelt_mod), digits = 4)

# save model 
#saveRDS(obs_snowmelt_mod, "models/obs_snowmelt_mod.RDS")

obs_snowmelt_mod <- readRDS("models/obs_snowmelt_mod.RDS")

# spectra model
# model 
spectra_snowmelt_mod <- brm (n ~ snowmelt_date * plant_type + (1|Genus), data = spectra_snowmelt_n)

summary (spectra_snowmelt_mod)

plot (spectra_snowmelt_mod)
pairs (spectra_snowmelt_mod)
pp_check(spectra_snowmelt_mod)

random_effects_spectra_snowmelt <- ranef(spectra_snowmelt_mod)
print(random_effects_spectra_snowmelt)

# save model 
#saveRDS(spectra_snowmelt_mod, "models/spectra_snowmelt_mod.RDS")

spectra_snowmelt_mod  <- readRDS("models/spectra_snowmelt_mod.RDS")

print(summary(spectra_snowmelt_mod), digits = 4)

### Plot ----

# obs model plot 
# dummy data frame
obs_snowmelt_mod_data <- expand_grid(snowmelt_date = seq(100, 215, by = 5), 
                                     n = seq(0, 5, by = 0.5),
                                     functional_group = levels (as.factor(obs_snowmelt$functional_group)))

obs_snowmelt_mod_pred <- obs_snowmelt_mod %>% 
  epred_draws(newdata = obs_snowmelt_mod_data, allow_new_levels = TRUE)

# define dates to replace doy on axis label for better communication
date <- c("Feb 19", "April 9", "May 29", "July 18")

# plot model 
(obs_snowmelt_mod_fit <- ggplot() +
    geom_point(data = obs_snowmelt, aes(x = snowmelt_date, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    xlab("Snowmelt Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85),
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (obs_snowmelt_mod_fit, filename = "graphs/obs_snowmelt_mod_fit_pft.png")


# plot model without legend, axes lables, etc for clarity when combining
(obs_snowmelt_plot_simple <- ggplot() +
    geom_point(data = obs_snowmelt, aes(x = snowmelt_date, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    #add_predicted_draws(obs_snowmelt_mod) %>%  # adding the posterior distribution
    # ggplot(aes(x = snowmelt_date, y = n, color = ordered (functional_group), fill = ordered (functional_group))) +  
    #stat_lineribbon(aes(y = .prediction)) +
    #geom_point(data = obs_snowmelt_n, size = 3) +   # raw data
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    #xlab("Snowmelt date (doy)") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text (size = 15),
          axis.title.x = element_blank(),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.y = element_text(margin = margin(r = 20))))

# plot spectra snowmelt model

spectra_snowmelt_mod_data <- expand_grid(snowmelt_date = seq(75, 190, by = 1), 
                                         n = seq(0, 5, by = 0.5),
                                         plant_type = levels (as.factor(spectra_snowmelt_n$plant_type)))

spectra_snowmelt_mod_pred <- spectra_snowmelt_mod %>% 
  epred_draws(newdata = spectra_snowmelt_mod_data, allow_new_levels = TRUE)

(spectra_snowmelt_mod_fit <- ggplot() +
    geom_point(data = spectra_snowmelt_n, aes(x = snowmelt_date, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +   # raw data
    stat_lineribbon(data = spectra_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (plant_type), fill = ordered (plant_type)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    xlab("Snowmelt Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (spectra_snowmelt_mod_fit, filename = "graphs/spectra_snowmelt_mod_fit_pft.png")


# put the two graphs on top of eachother in the same figure 
(snowmelt_combined_plot <- grid.arrange(
  obs_snowmelt_plot_simple, 
  #ggtitle ("A") +
  spectra_snowmelt_mod_fit,
  # ggtitle ("B") +  
  ncol = 1))

ggsave (snowmelt_combined_plot, filename = "graphs/snowmelt_combined_plot.png")


# whittaker plot ----

# download and extract worldclim temp data

temp_months <- worldclim_global (var = "tavg", res = 10, path = 'data')

temp <- mean (temp_months)

precip_annual <- mean (precip_year)

arctic_temp <- terra::extract(temp, n_points)
arctic_annual_precip <- terra::extract(precip_annual, n_points)
arctic_spectra_temp <- terra::extract(temp, spectra_vect_points)

# add temp data to trait datasheet 
n_precip_temp <- n_precip_zones %>% 
  mutate (ID = row_number()) %>%  
  left_join (arctic_temp, by = "ID") %>% 
  rename (temp = mean) %>% 
  left_join (arctic_annual_precip, by = "ID") %>% 
  rename (annual_precip = mean)

spectra_precip_temp <- spectra_precip_zones %>% 
  mutate (ID = row_number()) %>%  
  left_join (arctic_spectra_temp, by = "ID") %>% 
  rename (temp = mean)
  
# common species to put on whittaker plot 
obs_common_species <- n_precip_zones %>% 
  group_by (species) %>% 
  tally () %>% 
  filter (n > 5) %>% 
  pull (species)

spectra_common_species <- spectra_precip_zones %>% 
  group_by (species) %>% 
  tally () %>% 
  filter (n > 1) %>% 
  pull (species)

# filtering out uncommon species and averaging nitrogen, precipitation and temp by species 
obs_species_avg <- n_precip_temp %>% 
  filter(species %in% obs_common_species) %>% 
  group_by (functional_group, genus, species) %>% 
  summarise(
    avg_nitrogen = mean(leafn),
    avg_precipitation = mean(annual_precip),
    avg_temp = mean(temp, na.rm = TRUE))

spectra_species_avg <- spectra_precip_temp %>% 
  filter(species %in% spectra_common_species) %>% 
  group_by (plant_type, Genus, species) %>% 
  summarise(
    avg_nitrogen = mean(n),
    avg_precipitation = mean(mean_precip),
    avg_temp = mean (temp)
  ) %>% 
  filter (!is.na (avg_temp))

# plot! 
(obs_whittaker <- ggplot (data = obs_species_avg, aes (x = avg_temp, 
                                                   y = avg_precipitation, 
                                                   col = functional_group)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
  geom_point(aes (size = avg_nitrogen)) +
    xlab ("Average Annual Temperature (°C)") +
    ylab ("Annual Preciptitaion (mm)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0,120)) +
  theme_classic()+ 
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

# save plot 
ggsave (obs_whittaker, filename = "graphs/obs_whittaker.png")

(whittaker_spectra <- ggplot (data = spectra_species_avg, aes (x = avg_temp, 
                                                               y = avg_precipitation,  
                                                               col = plant_type)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    xlab ("Average Annual Temperature (°C)") +
    ylab ("Annual Preciptitaion (mm)") +
    geom_point(aes(size = avg_nitrogen)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,120)) +
    theme_classic()+
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave(whittaker_spectra, filename = "graphs/whittaker_spectra.png")

  

# plot average leaf nitrogen vs average precipitation 
(precip_species_avg_n_obs_plot <- ggplot (data = obs_species_avg, aes (x = avg_precipitation, 
                                                                       y = avg_nitrogen, 
                                                                       col = functional_group)) +
    geom_point (size = 3) +
   # stat_ellipse(geom = "polygon", aes(x=avg_precipitation, y=avg_nitrogen, 
    #                                   fill = functional_group, 
     #                                  alpha = 0.2, size = 0.2)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration (%)") +
    xlab ("Annual Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,3.5)) +
    theme_classic () +
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (precip_species_avg_n_obs_plot, filename = "graphs/precip_species_avg_n_obs_plot.png")

precip_species_avg_n_obs_mod <- lm (avg_nitrogen ~ avg_precipitation, data = obs_species_avg)
summary (precip_species_avg_n_obs_mod)

# spectra average leaf nitrogen vs average precipitation 
(precip_species_avg_n_plot <- ggplot (data = spectra_species_avg, aes (x = avg_precipitation, 
                                                                       y = avg_nitrogen, 
                                                                       col = plant_type)) +
    geom_point (size = 3) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration (%)") +
    xlab ("Annual Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,3.5)) +
    theme_classic ()+
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (precip_species_avg_n_plot, filename = "graphs/precip_species_avg_n_spectra_plot.png")

precip_species_avg_n_spectra_mod <- brm (avg_nitrogen ~ avg_precipitation, data = spectra_species_avg)
summary (precip_species_avg_n_spectra_mod)

# make datasheet with snowmelt averages by species 
obs_species_snowmelt_avg <- obs_snowmelt %>% 
  filter(species %in% obs_common_species) %>% 
  group_by (functional_group, genus, species) %>% 
  summarise(
    avg_nitrogen = mean(leafn),
    avg_snowmelt = mean(snowmelt_date))


spectra_snowmelt_avg <- spectra_snowmelt_n %>% 
  filter (species %in% spectra_common_species) %>% 
  group_by (plant_type, Genus, species) %>% 
  summarise(
    avg_nitrogen = mean(n),
    avg_snowmelt = mean(snowmelt_date)
  )

average_dates <- c("March 21", "April 30", "June 9", "July 19")

# plot 
(snowmelt_species_avg_n_obs_plot <- ggplot (data = obs_species_snowmelt_avg, aes (x = avg_snowmelt, 
                                                                                  y = avg_nitrogen, 
                                                                                  col = functional_group)) +
    geom_point (size = 3) +
    # stat_ellipse(geom = "polygon", aes(x=avg_precipitation, y=avg_nitrogen, 
    #                                   fill = functional_group, 
    #                                  alpha = 0.2, size = 0.2)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration (%)") +
    xlab ("Snowmelt Date") +
    scale_x_continuous(limits = c(80,230), breaks = seq(80, 230, by = 40), labels = average_dates) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,4)) +
    theme_classic () +
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (snowmelt_species_avg_n_obs_plot, filename = "graphs/snowmelt_species_avg_n_obs_plot.png")


# spectra average leaf nitrogen vs average snowmelt date  
(snowmelt_species_avg_n_spectra_plot <- ggplot (data = spectra_snowmelt_avg, aes (x = avg_snowmelt, 
                                                                       y = avg_nitrogen, 
                                                                       col = plant_type)) +
    geom_point (size = 3) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration") +
    xlab ("Snowmelt Date") +
    scale_x_continuous(limits = c(80,230), breaks = seq(80, 230, by = 40), labels = average_dates) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,4)) +
    theme_classic ()+
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (snowmelt_species_avg_n_spectra_plot, filename = "graphs/snowmelt_species_avg_n_spectra_plot.png")

