##########################################################
## Title: EAVE II Cohort Profile
## Short title: EAVE II Cohort Profile
## DOI: TBC
## Code author(s): Rachel Mulholland <rachel.mulholland@ed.ac.uk> 
##                 Chris Robertson <chrisobertson@nhs.net>
## Description: 00_functions - Unique functions for analysis
##########################################################


#### 0 - Setting up ####

## Libraries
library(tidyverse)



##### 1 - Characteristic summaries #####

##### 1.1 - For counts (EAVE Demographics) ####
characteristic_count_tbl_fn <- function(z.var){
  EAVE_demographics %>% 
    dplyr::select(!!sym(z.var))  %>% 
    count(!!sym(z.var)) %>% 
    mutate(P=round(n/sum(n)*100, 1)) %>%
    mutate(N.Perc = paste0(n, " (", P, ")"))
  
}


##### 1.2 - For weights (linked data) ####
characteristic_weight_tbl_fn <- function(z.var){
  EAVE_cohort %>% dplyr::select(eave_weight, !!sym(z.var))  %>% 
    group_by(!!sym(z.var)) %>% 
    summarise(N=round(sum(eave_weight))) %>%
    mutate(P=round(N/sum(N)*100, 1)) %>%
    mutate(N.Perc = paste0(N, " (", P, ")"))
  
}


###### 2 - Plotting % of variable against age by sex #####
var_age_sex_fn <- function(z.var){
  var_age_sex_tbl <- EAVE_cohort %>%
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


##### 3 - Plotting % of variable by age for outcomes #####
# Outcomes: Total count, tested, tested positive, hospitalised, ICU/death and death

var_outcomes_fn <- function(z.var){
  
  multiple_tbl <- EAVE_cohort %>%
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





