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
library(gridExtra)
library(survival)
library(cowplot)

# Load data
Location <- "/conf/"  # Server

# Load in 'Cohort_Demog_Endpoints' dataset for:
## Baseline characteristics update - 23 June and outcomes update (11 November)
EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Demog_Endpoints_Times2020-11-10.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers

# Load in risk group data (update ?)
rg <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_RG_EAVE.rds"))
rg <- filter(rg, !duplicated(EAVE_LINKNO)) # Deleting duplicate link numbers

# Create data frame with characteristics and outcomes using the link numbers
df <- EAVE_cohort %>% select(EAVE_LINKNO:ur6_2016_name, result, death_covid, icu_death, hosp_covid, tested, Time.To.Test:age_gp) %>% 
  left_join(rg, by="EAVE_LINKNO")

# Load in weights (to adjust cohort population to ??)
EAVE_weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))

# Join weights and fix ages
df <- df %>%
  left_join(EAVE_weights, by="EAVE_LINKNO") %>% # Join using link numbers
  mutate(ageYear = ifelse(ageYear >= 100, 100, ageYear)) %>% # Group everyone over the age of 100
  mutate(EAVE_PREGNANCY = factor(case_when(Sex=="M" ~ "No",
                                           ageYear <= 13 | ageYear >= 54 ~ "No",
                                           TRUE ~ as.character(EAVE_PREGNANCY)))) # Fix errors in pregnancy variable
#mutate(ageYear5 = cut(ageYear, breaks=seq(0,100, by=5),  include.lowest =T))%>%
# mutate(age_gp = cut(ageYear, breaks=seq(0,100, by=10),  include.lowest =T))

##### 2- EAVE II Colours #####
eave_green <- rgb(54, 176, 136, maxColorValue = 255)
eave_blue <- rgb(71,93,167, maxColorValue = 255)
eave_blue2 <- rgb(0,192,209, maxColorValue = 255)
eave_gold <- rgb(255,192,0, maxColorValue = 255)
eave_orange <- rgb(244,143,32, maxColorValue = 255)

eave_cols <- c(eave_green, eave_blue, eave_gold, eave_blue2, eave_orange)

###### 3 - Plotting % of variable against age by sex #####
var_age_sex_fn <- function(z.var){
  var_age_sex_tbl <- df %>%
    mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male")) %>%
    filter(!!sym(as.character(z.var)) != "NA") %>%
    select(age_gp, Sex, !!sym(as.character(z.var)), eave_weight) %>%
    group_by(age_gp,Sex, !!sym(as.character(z.var))) %>% 
    summarise(N=round(sum(eave_weight)))
  
  age_sex_total <- var_age_sex_tbl %>%
    group_by(age_gp, Sex) %>% 
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
  
  g0 <- ggplot(var_age_sex_tbl, aes(x=age_gp, y=Percent, fill=get(z.var))) + 
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

plot_grid(p_risk_grp, p_simd, ncol=1, align="hv")




#### 4 - Plots of variables by tested, positive, hospitalised, ICU death and death ####

multiple_plot_fn <- function(z.var){
  z.response.vars <- c("tested","result","hosp_covid","icu_death","death_covid")
  
  multiple_tbl <- df %>%
    filter(!!sym(as.character(z.var)) != "NA") %>%
    select(age_gp, !!sym(as.character(z.var)), eave_weight, tested, result, hosp_covid, icu_death, death_covid)
  
  multiple_tbl_total <- multiple_tbl %>%
    group_by(age_gp, !!sym(as.character(z.var))) %>%
    summarise(N=round(sum(eave_weight)))
  
  multiple_tbl_total_outcomes <- multiple_tbl %>%
    group_by(age_gp, !!sym(as.character(z.var))) %>%
    summarise_at(z.response.vars, sum)
  
  multiple_tbl2 <- left_join(multiple_tbl_total, multiple_tbl_total_outcomes) %>% 
    mutate_at(z.response.vars, ~ ./N) %>% 
    rename_at(z.response.vars, ~paste0("P_",.))
  
  g1.prop <- ggplot(multiple_tbl2, aes(x=age_gp, y=N, fill=get(z.var))) + 
    geom_bar(stat="identity") +
    labs(x="", y="Count", title="Total count", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.tested <- ggplot(multiple_tbl2, aes(x=age_gp, y=P_tested, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Tested for COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  
  g1.pos <- ggplot(multiple_tbl2, aes(x=age_gp, y=P_result, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Tested positive for COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.hosp <- ggplot(multiple_tbl2, aes(x=age_gp, y=P_hosp_covid, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="", y="Proportion", title="Hospitalised with COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  g1.severe <- ggplot(multiple_tbl2, aes(x=age_gp, y=P_icu_death, fill=get(z.var))) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="Age (grouped)", y="Proportion", title="Admission to ICU or death with COVID-19", fill=names(z.var)) +
    theme_light() +
    scale_fill_manual(values=eave_cols) +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    theme(text = element_text(size=8))
  
  
  g1.dth <- ggplot(multiple_tbl2, aes(x=age_gp, y=P_death_covid, fill=get(z.var))) + 
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
    draw_label("Time frame: 01 Feb to 10 Nov 2020", hjust=0, size=10, x=0.75)
  
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

pdf("EAVE_Characteristic_Age_Profiles.pdf", height=12, width=10, paper="a4")

multiple_plot_fn(c("SIMD Quintiles"="simd2020_sc_quintile"))
multiple_plot_fn(c("Number of risk groups"="n_risk_gps"))
multiple_plot_fn(c("Asthma"="EAVE_ASTHMA"))
multiple_plot_fn(c("Care home"="EAVE_CARE_HOME"))
multiple_plot_fn(c("Chronic heart disease"="EAVE_CHRONIC_HEART_DIS"))
multiple_plot_fn(c("Chronic liver disease"="EAVE_CHRONIC_LIVER_DIS"))
dev.off()

### Summary table ###

# Comorbidities
z.df <- df %>% dplyr::select(eave_weight, EAVE_ASTHMA:EAVE_ULCER_DIS) %>% 
  dplyr::select(-EAVE_PREGNANCY) %>% 
  pivot_longer(cols= -eave_weight) %>% 
  group_by(name) %>% 
  dplyr::summarise(N=round(sum(eave_weight*(value=="Yes"))))
z.df <- z.df %>% mutate(Percent = round(N/sum(df$eave_weight)*100,1)) %>% 
  arrange( desc(N)) %>% as.data.frame()

# Characteristics
characteristic_tbl_fn <- function(z.var){
  df %>% select(eave_weight, !!sym(z.var))  %>% 
    group_by(!!sym(z.var)) %>% 
    summarise(N=round(sum(eave_weight))) %>%
    mutate(P=N/sum(N)*100)
  
}

characteristic_tbl_fn("age_gp")
characteristic_tbl_fn("Sex")
characteristic_tbl_fn("simd2020_sc_quintile")
characteristic_tbl_fn("ur6_2016_name")
characteristic_tbl_fn("n_risk_gps")

df %>% dplyr::select(eave_weight, Sex)  %>% 
  group_by(Sex) %>% 
  summarise(N=round(sum(eave_weight))) %>%
  mutate(prop=N/sum(N)*100)



#### 5- Spatial map ######




z.df <- df %>% dplyr::select(eave_weight, DataZone, n_risk_gps, death_covid, hosp_covid)  %>% 
  group_by(DataZone) %>% 
  dplyr::summarise(N=round(sum(eave_weight)), 
                   gt3_risk_gps = round(sum(eave_weight*n_risk_gps %in% c("3-4","5+"))),
                   death_covid=sum(death_covid), hosp_covid=sum(hosp_covid)) %>% 
  mutate(gt3_risk_gps=gt3_risk_gps/N,
         death_covid=death_covid/N,
         hosp_covid=hosp_covid/N)
cor(z.df[,3:5])  
write.csv(z.df,"DZ_Risk_Hosp_Death.csv",row.names = FALSE)
