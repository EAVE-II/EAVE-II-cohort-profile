##########################################################
## Title: EAVE II Cohort Profile
## Short title: EAVE II Cohort Profile
## DOI: TBC
## Code author(s): Rachel Mulholland <rachel.mulholland@ed.ac.uk> 
##                 Chris Robertson <chrisobertson@nhs.net>
## Description of content: Reads in EAVE II data to plot and tabulate summary 
##                         statistics, using the EAVE II weights
##########################################################

#### 1 - Setting up ####

# Libraries
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

# Load data
Location <- "/conf/EAVE/GPanalysis/"  # Server

# Load in 'Cohort_Demog_Endpoints' dataset for:
## Baseline characteristics update - 23 June and outcomes update (10 November)
EAVE_cohort <- readRDS(paste0(Location,"outputs/temp/CR_Cohort_Demog_Endpoints_Times2020-11-10.rds")) %>%
  filter(!duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers

# Load in risk group data (update ?)
rg <- readRDS(paste0(Location,"outputs/temp/CR_Cohort_RG_EAVE.rds")) %>%
  filter(!duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers


# Load in weights (to adjust cohort population to ??)
EAVE_weights <- readRDS(paste0(Location,"outputs/temp/CR_Cohort_Weights.rds"))

# Data zone to Health Board lookup
Datazone2011lookup <- readr::read_csv("/conf/EAVE/GPanalysis/data/lookups/Datazone2011lookup.csv")


# Create cohort
df <- EAVE_cohort %>% 
  dplyr::select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, age_gp) %>% 
  left_join(rg, by="EAVE_LINKNO") %>%
  mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male")) %>%
  left_join(EAVE_weights, by="EAVE_LINKNO") %>% # Join using link numbers
  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear)) %>% # Group everyone over the age of 100
  mutate(EAVE_PREGNANCY = factor(case_when(Sex=="Male" ~ "No",
                                           ageYear <= 13 | ageYear >= 54 ~ "No",
                                           TRUE ~ as.character(EAVE_PREGNANCY)))) %>%# Fix errors in pregnancy variable
  mutate(age_gp2 = case_when(
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
  left_join(Datazone2011lookup %>%
              dplyr::select(DZ2011_Code, IZ2011_Code, HB_Code) %>%
              rename(DataZone = DZ2011_Code, IntermediateZone = IZ2011_Code))



##### 2 - EAVE II Colours #####
eave_green <- rgb(54, 176, 136, maxColorValue = 255)
eave_blue <- rgb(71,93,167, maxColorValue = 255)
eave_blue2 <- rgb(0,192,209, maxColorValue = 255)
eave_gold <- rgb(255,192,0, maxColorValue = 255)
eave_orange <- rgb(244,143,32, maxColorValue = 255)

eave_cols <- c(eave_green, eave_blue, eave_gold, eave_blue2, eave_orange)
eave_gradient <- colorRampPalette(c(eave_green, eave_blue))


##### 3 - Summary statistics table #####

# Total population
sum(df$eave_weight)

# Characteristics function
characteristic_tbl_fn <- function(z.var){
  df %>% dplyr::select(eave_weight, !!sym(z.var))  %>% 
    group_by(!!sym(z.var)) %>% 
    summarise(N=round(sum(eave_weight))) %>%
    mutate(P=round(N/sum(N)*100, 1)) %>%
    mutate(N.Perc = paste0(N, " (", P, ")"))
  
}

characteristic_tbl_fn("age_gp2")
characteristic_tbl_fn("Sex")
characteristic_tbl_fn("simd2020_sc_quintile")
characteristic_tbl_fn("ur6_2016_name")
characteristic_tbl_fn("n_risk_gps")


# Individual risk groups (Supplementary)
eave_risk_grp <- df %>% 
  dplyr::select(eave_weight, EAVE_ASTHMA:EAVE_ULCER_DIS) %>% 
  dplyr::select(-EAVE_PREGNANCY) %>% 
  pivot_longer(cols= -eave_weight) %>% 
  group_by(name) %>% 
  summarise(N=round(sum(eave_weight*(value=="Yes")))) %>%
  mutate(Percent = round(N/sum(df$eave_weight)*100,1)) %>% 
  arrange( desc(N)) %>% 
  mutate(N.Perc = paste0(N, " (", Percent, ")"))

# Outcomes
z.response.vars <- c("tested","result","hosp_covid","icu_death","death_covid")

eave_outcomes_tbl <- df %>% 
  dplyr::select(eave_weight, tested, result, hosp_covid, icu_death, death_covid) %>% 
  summarise_at(z.response.vars, sum)

eave_outcomes_tbl
round(eave_outcomes_tbl[1,]/sum(df$eave_weight)*100,1)

  
###### 4 - Plotting % of variable against age by sex #####
var_age_sex_fn <- function(z.var){
  var_age_sex_tbl <- df %>%
    mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male")) %>%
    filter(!!sym(as.character(z.var)) != "NA") %>%
    dplyr::select(age_gp2, Sex, !!sym(as.character(z.var)), eave_weight) %>%
    group_by(age_gp2,Sex, !!sym(as.character(z.var))) %>% 
    summarise(N=round(sum(eave_weight)))
  
  age_sex_total <- var_age_sex_tbl %>%
    group_by(age_gp2, Sex) %>% 
    summarise(N.Tot=sum(N)) 
  
  var_age_sex_tbl <- var_age_sex_tbl %>%
    left_join(age_sex_total) %>% 
    mutate(Percent=N/N.Tot*100)
  
  # Not using
  g0 <- ggplot(var_age_sex_tbl, aes(x=ageYear, y=Percent, col=Sex, shape=Sex)) + 
    facet_wrap(~get(z.var), scales="free") +
    geom_point(size=2) + 
    #geom_smooth(span=0.5) +
    labs(x="Age", title=names(z.var), y= "Percent (%)") +
    theme_light() +
    scale_color_manual(values=eave_cols)+
    theme(strip.background = element_rect(fill="white")) +
    theme(strip.text = element_text(colour="black")) +
    theme(strip.text.x = element_text(angle=0, hjust=0))
  
  g0 <- ggplot(var_age_sex_tbl, aes(x=age_gp2, y=Percent, fill=get(z.var))) + 
    facet_wrap(~Sex, scales="free") +
    geom_bar(stat="identity", width=0.75) + 
    labs(x="Age (grouped)", title=names(z.var), y= "Percent (%)", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols)+
    theme(strip.background = element_rect(fill="white")) +
    theme(strip.text = element_text(colour="black")) +
    theme(strip.text.x = element_text(angle=0, hjust=0))+
    theme(axis.text.x = element_text(angle=45, hjust=1))  +
    theme(legend.justification = "left")
  
  g0
  
}

p_risk_grp <- var_age_sex_fn(c("Number of risk groups"="n_risk_gps"))
p_simd <- var_age_sex_fn(c("SIMD Quintile"="simd2020_sc_quintile"))

plot_grid(p_simd,p_risk_grp, ncol=1, align="hv", labels="AUTO")




#### 5 - Plots of variables by tested, positive, hospitalised, ICU death and death ####

multiple_plot_fn <- function(z.var){

  multiple_tbl <- df %>%
    filter(!!sym(as.character(z.var)) != "NA") %>%
    dplyr::select(age_gp2, !!sym(as.character(z.var)), eave_weight, tested, result, hosp_covid, icu_death, death_covid)
  
  multiple_tbl_total <- multiple_tbl %>%
    group_by(age_gp2, !!sym(as.character(z.var))) %>%
    summarise(N=round(sum(eave_weight)))
  
  multiple_tbl_total_outcomes <- multiple_tbl %>%
    group_by(age_gp2, !!sym(as.character(z.var))) %>%
    summarise_at(z.response.vars, sum)
  
  multiple_tbl2 <- left_join(multiple_tbl_total, multiple_tbl_total_outcomes) %>% 
    mutate_at(z.response.vars, ~ ./N) %>% 
    rename_at(z.response.vars, ~paste0("P_",.))
  
  g1.count <- ggplot(multiple_tbl2, aes(x=age_gp2, y=N, fill=get(z.var))) + 
    geom_bar(stat="identity") +
    labs(x="", y="Count", title="Total count", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.tested <- ggplot(multiple_tbl2, aes(x=age_gp2, y=P_tested, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Tested for COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  
  g1.pos <- ggplot(multiple_tbl2, aes(x=age_gp2, y=P_result, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Tested positive for COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.hosp <- ggplot(multiple_tbl2, aes(x=age_gp2, y=P_hosp_covid, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Hospitalised with COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.severe <- ggplot(multiple_tbl2, aes(x=age_gp2, y=P_icu_death, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="Age (grouped)", y="Proportion", title="Admission to ICU or death with COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  
  g1.dth <- ggplot(multiple_tbl2, aes(x=age_gp2, y=P_death_covid, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="Age (grouped)", y="Proportion", title="Death with COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  legend_plot <- get_legend(g1.tested + theme(legend.position="bottom"))
  title_plot <- ggdraw() +
    draw_label(names(z.var), fontface = "bold")
  subtitle_plot <- ggdraw() +
    draw_label("Time frame: 01 Mar to 10 Nov 2020", hjust=1, size=8, x=0.30)
  
  plots <- plot_grid(g1.count + theme(legend.position="none"),
                     g1.tested+ theme(legend.position="none"), 
                     g1.pos + theme(legend.position="none"),
                     g1.hosp + theme(legend.position="none"),
                     g1.severe + theme(legend.position="none"),
                     g1.dth + theme(legend.position="none"), ncol=2,
                     labels="AUTO")
  
  plot_grid(title_plot,
            plots,
            legend_plot, 
            subtitle_plot, ncol=1, rel_heights = c(0.05,1,0.05,0.05))
  
  
  
}

multiple_plot_fn(c("Sex"="Sex"))

pdf("/conf/EAVE/GPanalysis/outputs/RM/Figures/EAVE_Characteristic_Age_Profiles.pdf", height=10, width=8, paper="a4")

multiple_plot_fn(c("SIMD Quintiles"="simd2020_sc_quintile"))
multiple_plot_fn(c("Number of risk groups"="n_risk_gps"))
multiple_plot_fn(c("Asthma"="EAVE_ASTHMA"))
multiple_plot_fn(c("Care home"="EAVE_CARE_HOME"))
multiple_plot_fn(c("Chronic heart disease"="EAVE_CHRONIC_HEART_DIS"))
multiple_plot_fn(c("Chronic liver disease"="EAVE_CHRONIC_LIVER_DIS"))
multiple_plot_fn(c("Chronic respiratory disease"="EAVE_CHRONIC_RESP_DIS"))
multiple_plot_fn(c("Dementia"="EAVE_DEMENTIA"))
multiple_plot_fn(c("Depression"="EAVE_DEPRESSION"))
multiple_plot_fn(c("Diabetes"="EAVE_DIABETES"))
multiple_plot_fn(c("Enlarged Spleen/Anaemia"="EAVE_SPLEEN_ANAEMIA"))
multiple_plot_fn(c("Haematological malignancy"="EAVE_HAEMAT_MALIGNANCY"))
multiple_plot_fn(c("Hypertension"="EAVE_HYPERTENSION"))
multiple_plot_fn(c("Immunosuppression"="EAVE_IMMUNOSUPPRESSION"))
multiple_plot_fn(c("MS and degenerative disease"="EAVE_MS_DEGEN_DISN"))
multiple_plot_fn(c("Nutritional deficiencies"="EAVE_NUTRITIONAL_DEF"))
multiple_plot_fn(c("Social care"="EAVE_SOCIAL_CARE"))
multiple_plot_fn(c("Stroke/Transient ischaemic attack (TIA)"="EAVE_STROKE_TIA"))
multiple_plot_fn(c("Ulcer disease"="EAVE_ULCER_DIS"))

dev.off()






#### 6 - Spatial map ######

# Summarising by health board
hb_tbl_final <- df %>% 
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


rownames(hb_tbl_final) <- hb_tbl_final$HB_Code


## Maps
shp <- read.shp(shp.name= "/conf/EAVE/GPanalysis/data/map_files/SG_NHS_HealthBoards_2019.shp")
dbf <- read.dbf(dbf.name="/conf/EAVE/GPanalysis/data/map_files/SG_NHS_HealthBoards_2019.dbf")

## Create spatial objects
sp.dat <- combine.data.shapefile(hb_tbl_final, shp, dbf)

sp.dat@data$id <- rownames(sp.dat@data)
temp1 <- fortify(sp.dat)
sp.dat2 <- merge(temp1, sp.dat@data, by = "id")
sp.dat2$lat <- sp.dat2$lat/1000
sp.dat2$long <- sp.dat2$long/1000


## Plots
# Overall population
ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(N))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(title = "", fill = "Population") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))

# Tested
map_tested <- ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(P_tested))) + 
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
map_result <- ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(P_result))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion tested positive for COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))


plot_grid(map_tested,map_result, ncol=2, align="hv", labels="AUTO")



## Severe outcomes
map_hosp <- ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(P_hosp_covid))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion hospitalised with COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))

map_icu_death <- ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(P_icu_death))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion admitted to ICU or\ndied with COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))

map_death <- ggplot(data = sp.dat2, aes(x=long, y=lat, goup=group, fill = c(death_covid))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(fill = "Proportion died with COVID-19") +  
  theme(title = element_text(size=12)) + theme_classic() +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_gradientn(colors=eave_gradient(3))


plot_grid(map_hosp ,map_icu_death, map_death, ncol=3, align="hv", labels="AUTO")
