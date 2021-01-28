##########################################################
## Title: EAVE II Cohort Profile
## Short title: EAVE II Cohort Profile
## DOI: TBC
## Code author(s): Rachel Mulholland <rachel.mulholland@ed.ac.uk> 
##                 Chris Robertson <chrisobertson@nhs.net>
## Description: 01_main - Main script for reading in data and 
##              performing analysis
##########################################################

#### 0 - Setting up ####

## Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(shapefiles)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(spdep)
library(sp)
library(RColorBrewer)
library(CARBayes)

## Functions
source("./code/00_functions.R")



##### 1 - Load in data ####
## Location
location <- "/conf/EAVE/GPanalysis/"  # Server

##### 1.1 -  EAVE II Demographics ####
# Baseline EAVE II cohort with key characteristics
EAVE_demographics <- readRDS(paste0(location, "data/EAVE_demographics.rds")) %>%
  mutate(age_gp2 = case_when( # Group age into the following bands
    ageYear < 5 ~ "0-4",
    ageYear < 15 ~ "5-14",
    ageYear < 25 ~ "15-24",
    ageYear < 45 ~ "25-44",
    ageYear < 65 ~ "45-64",
    ageYear < 75 ~ "65-74",
    ageYear < 85 ~ "75-84",
    is.na(ageYear) ~ NA_character_,
    TRUE ~ "85+"
  ) %>%
    factor(levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65-74", "75-84", "85+", "NA")))


#### 1.2 -  EAVE II End Points ####
## EAVE II to event e.g. death, hospitalisation
# Update: 10 November 2020
# More info - link to GitHub code

EAVE_endpts <- readRDS(paste0(location,"outputs/temp/CR_Cohort_Demog_Endpoints_Times2020-11-10.rds")) %>%
  filter(!duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers

#### 1.3 -  EAVE II Risk Groups ####
## EAVE II ID to EAVE II risk groups
# More info - link to GitHub code

EAVE_rgs <- readRDS(paste0(location,"outputs/temp/CR_Cohort_RG_EAVE.rds")) %>%
  filter(!duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers

#### 1.4 -  EAVE II Weights ####
## EAVE II ID to weight as of NRS 2019 mid year estimates
# More info - link to GitHub code
EAVE_weights <- readRDS(paste0(location,"outputs/temp/CR_Cohort_Weights.rds"))


#### 1.5 - Geographical lookup ####
## 2011 Data Zone to other Geographical outputs (e.g. NHS Health Board)
Datazone2011lookup <- readr::read_csv(paste0(location, "data/lookups/Datazone2011lookup.csv"))



#### 1.6 - Create weighted cohort ####
# Joins:
#   - End points
#   - Risk groups
#   - Weights (Adjusts for linkage between end points and risk groups)
#   - Geographical lookup

# Create cohort
EAVE_cohort <- EAVE_endpts %>% # Start with end points
  dplyr::select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, age_gp) %>% # Select variables
  left_join(EAVE_rgs, by="EAVE_LINKNO") %>% # Link with EAVE Risk Groups
  mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male")) %>% # Recode sex
  left_join(EAVE_weights, by="EAVE_LINKNO") %>% # Link EAVE Weights
  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear)) %>% # Group everyone over the age of 100
  mutate(EAVE_PREGNANCY = factor(case_when(Sex=="Male" ~ "No",
                                           ageYear <= 13 | ageYear >= 54 ~ "No",
                                           TRUE ~ as.character(EAVE_PREGNANCY)))) %>%# Fix errors in pregnancy variable
  mutate(age_gp2 = case_when( # Group age into the following bands
    ageYear < 5 ~ "0-4",
    ageYear < 15 ~ "5-14",
    ageYear < 25 ~ "15-24",
    ageYear < 45 ~ "25-44",
    ageYear < 65 ~ "45-64",
    ageYear < 75 ~ "65-74",
    ageYear < 85 ~ "75-84",
    is.na(ageYear) ~ NA_character_,
    TRUE ~ "85+"
  ) %>%
    factor(levels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65-74", "75-84", "85+", "NA"))) %>%
  left_join(Datazone2011lookup %>% # Link geographical lookup
              dplyr::select(DZ2011_Code, IZ2011_Code, HB_Code) %>% # Link Intermediate zone and NHS Health Board
              rename(DataZone = DZ2011_Code, IntermediateZone = IZ2011_Code))




#### 1.7 - Shapefiles of NHS Health Boards ####
# For plotting choropleth maps

shp <- read.shp(shp.name= "/conf/EAVE/GPanalysis/data/map_files/SG_NHS_HealthBoards_2019.shp")
dbf <- read.dbf(dbf.name="/conf/EAVE/GPanalysis/data/map_files/SG_NHS_HealthBoards_2019.dbf")





##### 2 - EAVE II Colours #####
eave_green <- rgb(54, 176, 136, maxColorValue = 255)
eave_blue <- rgb(71,93,167, maxColorValue = 255)
eave_blue2 <- rgb(0,192,209, maxColorValue = 255)
eave_gold <- rgb(255,192,0, maxColorValue = 255)
eave_orange <- rgb(244,143,32, maxColorValue = 255)

eave_cols <- c(eave_green, eave_blue, eave_gold, eave_blue2, eave_orange)
eave_gradient <- colorRampPalette(c(eave_green, eave_blue))


##### 3 - Summary statistics tables #####

##### 3.1 - Table 1: Baseline demographics summary statistics table #####
# Sex
characteristic_count_tbl_fn("Sex")
# Age
characteristic_count_tbl_fn("age_gp2")
# SIMD
characteristic_count_tbl_fn("simd2020_sc_quintile")
# Ubran Rural Score
characteristic_count_tbl_fn("ur6_2016_name")


##### 3.2 - Table 1: Baseline demographics summary statistics table #####
# Number of risk groups
characteristic_weight_tbl_fn("n_risk_gps")

# For each risk group (one go)
EAVE_rgs_summary <- EAVE_cohort %>% 
  dplyr::select(eave_weight, EAVE_ASTHMA:EAVE_ULCER_DIS) %>%  # Look at main EAVE II RGs
  dplyr::select(-EAVE_PREGNANCY) %>% # Exclude pregnancy
  pivot_longer(cols= -eave_weight) %>% # Pivot
  group_by(name) %>% # Group by name
  summarise(N=round(sum(eave_weight*(value=="Yes")))) %>% # Count the weights for 'Yes'
  mutate(Percent = round(N/sum(df$eave_weight)*100,1)) %>%  # Calculate the proportion
  arrange( desc(N)) %>% # Rearrange from descending N
  mutate(N.Perc = paste0(N, " (", Percent, ")")) # Add N (?%) column

EAVE_rgs_summary


##### 4 - Geographical plots ####

##### 4.1 - Figure 1: Map of baseline population by NHS Health Board #####
# Creates choropleth map of population by NHS HB

# Count population by NHS HB
hb_tbl_baseline <- characteristic_count_tbl_fn("hb2019") %>%
  dplyr::select(c(hb2019,n)) %>%
  drop_na(hb2019)
# Make NHS HB code the rownames (for linkage)
rownames(hb_tbl_baseline) <- hb_tbl_baseline$hb2019

# Link spatial objects with population count
sp.dat <- CARBayes::combine.data.shapefile(hb_tbl_baseline, shp, dbf)

# Create ID of rownames (for linkage)
sp.dat@data$id <- rownames(sp.dat@data)
# Fortify spatial object
temp1 <- fortify(sp.dat)
# Merge fortified spatial object and spatial data
sp.dat2 <- merge(temp1, sp.dat@data, by = "id")
# Lat and long
#sp.dat2$lat <- sp.dat2$lat
#sp.dat2$long <- sp.dat2$long

## Find polygon centroids (for labels)
# Use rgeos package to find centroids
centroids <- rgeos::gCentroid(sp.dat, byid=T) 
# Put centroids into a data frame with co-ordinates and HB codes
centroids_data <- data.frame(centroids@coords)
centroids_data$n <- 1:nrow(centroids_data)
centroids_data$HB_Code <- rownames(centroids_data)

# Link centroids to NHS HB name (for labels)
centroids_data <- centroids_data %>%
  left_join(EAVE_demographics %>%
              dplyr::select(hb2019, hb2019name) %>%
              distinct(), by=c("HB_Code"="hb2019"))

centroid_label <- paste(paste0(centroids_data$n, " = ", centroids_data$hb2019name), collapse="; ")

## Plots
# Overall population
ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(n))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(title = "", fill = "Population") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3)) +
  geom_text(data=centroids_data, aes(x=x, y=y, label=n), inherit.aes = F)



#### 4.2 - Figure 5: Map of outcomes by NHS HB ####

# Summarising by health board
hb_tbl_outcome <- EAVE_cohort %>% 
  dplyr::select(eave_weight, HB_Code, n_risk_gps, tested, result, hosp_covid, icu_death, death_covid)  %>% 
  drop_na(HB_Code) %>%
  group_by(HB_Code) %>% 
  summarise(N=round(sum(eave_weight)),
            gt3_risk_gps = round(sum(eave_weight*n_risk_gps %in% c("3-4","5+"))),
            tested=sum(tested),
            result=sum(result),
            hosp_covid=sum(hosp_covid),
            icu_death=sum(icu_death),
            death_covid=sum(death_covid)) %>% 
  mutate(gt3_risk_gps=gt3_risk_gps/N,
         P_tested=tested/N,
         P_result=result/N,
         P_hosp_covid=hosp_covid/N,
         P_icu_death=icu_death/N,
         P_death_covid=death_covid/N) %>%
  as.data.frame()


rownames(hb_tbl_outcome) <- hb_tbl_outcome$HB_Code



# Link spatial objects with population count
sp.dat_outcomes <- CARBayes::combine.data.shapefile(hb_tbl_outcome, shp, dbf)

# Create ID of rownames (for linkage)
sp.dat_outcomes@data$id <- rownames(sp.dat_outcomes@data)
# Fortify spatial object
temp1_outcomes <- fortify(sp.dat_outcomes)
# Merge fortified spatial object and spatial data
sp.dat2_outcomes <- merge(temp1_outcomes, sp.dat_outcomes@data, by = "id")
# Lat and long
#sp.dat2$lat <- sp.dat2$lat
#sp.dat2$long <- sp.dat2$long

## Plots
# Tested
map_tested <- ggplot(data = sp.dat2_outcomes, aes(x=long, y=lat, goup=group, fill = c(P_tested))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion tested") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3)) 


# Tested positive
map_result <- ggplot(data = sp.dat2_outcomes, aes(x=long, y=lat, goup=group, fill = c(P_result))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion tested positive\nfor COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))




# Hospitalised
map_hosp <- ggplot(data = sp.dat2_outcomes, aes(x=long, y=lat, goup=group, fill = c(P_hosp_covid))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion hospitalised\nwith COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))

# Severe (ICU or death)
map_icu_death <- ggplot(data = sp.dat2_outcomes, aes(x=long, y=lat, goup=group, fill = c(P_icu_death))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion admitted to ICU\nor died with COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))

# Death
map_death <- ggplot(data = sp.dat2_outcomes, aes(x=long, y=lat, goup=group, fill = c(death_covid))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion died with\nCOVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))


### Plot together
plot_grid(map_tested,map_result, map_hosp ,map_icu_death, map_death, ncol=3, align="hv", labels="AUTO")






##### 5 - Proportions by age and sex #####
# Barplots of the proportion of a variables levels by age and sex

##### 5.1 - Figure 3: Proportion of SIMD and EAVE Risk Groups by age and sex #####
p_risk_grp <- var_age_sex_fn(c("Number of risk groups"="n_risk_gps"))
p_simd <- var_age_sex_fn(c("SIMD Quintile"="simd2020_sc_quintile"))

plot_grid(p_simd,p_risk_grp, ncol=1, align="hv", labels="AUTO")


#### 5.2 - Figure 4: Proportion of outcomes by age and sex ####

var_outcomes_fn(c("Sex"="Sex"))


##### 5.3 - Supplementary: Proportion of outcomes by age and individual EAVE risk groups #####
# Creates a pdf of the top 20 EAVE risk groups


pdf("/conf/EAVE/GPanalysis/analyses/cohort_profile/outputs/EAVE_Characteristic_Age_Profiles.pdf", height=10, width=8, paper="a4")

var_outcomes_fn(c("SIMD Quintiles"="simd2020_sc_quintile"))
var_outcomes_fn(c("Number of risk groups"="n_risk_gps"))
var_outcomes_fn(c("Asthma"="EAVE_ASTHMA"))
var_outcomes_fn(c("Care home"="EAVE_CARE_HOME"))
var_outcomes_fn(c("Chronic heart disease"="EAVE_CHRONIC_HEART_DIS"))
var_outcomes_fn(c("Chronic liver disease"="EAVE_CHRONIC_LIVER_DIS"))
var_outcomes_fn(c("Chronic respiratory disease"="EAVE_CHRONIC_RESP_DIS"))
var_outcomes_fn(c("Dementia"="EAVE_DEMENTIA"))
var_outcomes_fn(c("Depression"="EAVE_DEPRESSION"))
var_outcomes_fn(c("Diabetes"="EAVE_DIABETES"))
var_outcomes_fn(c("Enlarged Spleen/Anaemia"="EAVE_SPLEEN_ANAEMIA"))
var_outcomes_fn(c("Haematological malignancy"="EAVE_HAEMAT_MALIGNANCY"))
var_outcomes_fn(c("Hypertension"="EAVE_HYPERTENSION"))
var_outcomes_fn(c("Immunosuppression"="EAVE_IMMUNOSUPPRESSION"))
var_outcomes_fn(c("MS and degenerative disease"="EAVE_MS_DEGEN_DISN"))
var_outcomes_fn(c("Nutritional deficiencies"="EAVE_NUTRITIONAL_DEF"))
var_outcomes_fn(c("Social care"="EAVE_SOCIAL_CARE"))
var_outcomes_fn(c("Stroke/Transient ischaemic attack (TIA)"="EAVE_STROKE_TIA"))
var_outcomes_fn(c("Ulcer disease"="EAVE_ULCER_DIS"))

dev.off()








