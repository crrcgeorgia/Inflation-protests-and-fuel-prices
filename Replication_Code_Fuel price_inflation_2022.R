setwd("D:/Makhare Files/Makhare/Blogs/2021_Wissol protest_blog")
getwd()
library(haven)
library(ggplot2)
library(margins)
library(survey)
library(sjPlot)
library(ggeffects)
library(questionr)
library(ggpubr)
library(MASS)
library(stats)
library(wCorr)
library(EdSurvey)
library(mlogit)
library(stargazer)
library(nnet)
library(svyVGAM)
library(openxlsx)


library(tidyverse)
library(extrafont)
library(ggtext)
library(ggforce)
library(xfun)
library(survey)
library(srvyr)
library(svyVGAM)
#library(CMAverse)
loadfonts(device = "win")


ti15<-read_dta("TI_2015_only_responses_08072015.dta")
ti16<-read_dta("TI_2016_only_responses_25042016.dta")
ti18<-read_dta("TI_2018_March_04.05.18.dta")
ti19<-read_sav("TI_2019_public_20.03.19.sav")


## Weighted proportional tables 2015:
ti15$lukoil<-ti15$q86_1
ti15$lukoil[ti15$lukoil==-3]<-NA
ti15$lukoil[ti15$lukoil==-7]<-NA
prop.table(xtabs(indwt~lukoil, data=ti15, na.action = "na.omit"))
table(ti15$lukoil) #overall 520 case

ti15$rompetrol<-ti15$q86_2
ti15$rompetrol[ti15$rompetrol==-3]<-NA
ti15$rompetrol[ti15$rompetrol==-7]<-NA
prop.table(xtabs(indwt~rompetrol, data=ti15, na.action = "na.omit"))
table(ti15$rompetrol) #overall 520 case

ti15$gulf<-ti15$q86_3
ti15$gulf[ti15$gulf==-3]<-NA
ti15$gulf[ti15$gulf==-7]<-NA
prop.table(xtabs(indwt~gulf, data=ti15, na.action = "na.omit"))
table(ti15$gulf) #overall 520 case

ti15$socar<-ti15$q86_4
ti15$socar[ti15$socar==-3]<-NA
ti15$socar[ti15$socar==-7]<-NA
prop.table(xtabs(indwt~socar, data=ti15, na.action = "na.omit"))
table(ti15$socar) #overall 520 case

ti15$wissol<-ti15$q86_5
ti15$wissol[ti15$wissol==-3]<-NA
ti15$wissol[ti15$wissol==-7]<-NA
prop.table(xtabs(indwt~wissol, data=ti15, na.action = "na.omit"))
table(ti15$wissol) #overall 520 case

ti15$diff<-ti15$q86_6
ti15$diff[ti15$diff==-3]<-NA
ti15$diff[ti15$diff==-7]<-NA
prop.table(xtabs(indwt~diff, data=ti15, na.action = "na.omit"))
table(ti15$diff) #overall 520 case

ti15$other<-ti15$q86_7
ti15$other[ti15$other==-3]<-NA
ti15$other[ti15$other==-7]<-NA
prop.table(xtabs(indwt~other, data=ti15, na.action = "na.omit"))
table(ti15$other) #overall 520 case



## Weighted proportional tables 2016:
ti16$lukoil<-ti16$q76_1
ti16$lukoil[ti16$lukoil==-3]<-NA
ti16$lukoil[ti16$lukoil==-7]<-NA
prop.table(xtabs(indwt~lukoil, data=ti16, na.action = "na.omit"))
table(ti16$lukoil) #overall 700 case

ti16$rompetrol<-ti16$q76_2
ti16$rompetrol[ti16$rompetrol==-3]<-NA
ti16$rompetrol[ti16$rompetrol==-7]<-NA
prop.table(xtabs(indwt~rompetrol, data=ti16, na.action = "na.omit"))
table(ti16$rompetrol) #overall 700 case

ti16$gulf<-ti16$q76_3
ti16$gulf[ti16$gulf==-3]<-NA
ti16$gulf[ti16$gulf==-7]<-NA
prop.table(xtabs(indwt~gulf, data=ti16, na.action = "na.omit"))
table(ti16$gulf) #overall 700 case

ti16$socar<-ti16$q76_4
ti16$socar[ti16$socar==-3]<-NA
ti16$socar[ti16$socar==-7]<-NA
prop.table(xtabs(indwt~socar, data=ti16, na.action = "na.omit"))
table(ti16$socar) #overall 700 case

ti16$wissol<-ti16$q76_5
ti16$wissol[ti16$wissol==-3]<-NA
ti16$wissol[ti16$wissol==-7]<-NA
prop.table(xtabs(indwt~wissol, data=ti16, na.action = "na.omit"))
table(ti16$wissol) #overall 700 case

ti16$diff<-ti16$q76_6
ti16$diff[ti16$diff==-3]<-NA
ti16$diff[ti16$diff==-7]<-NA
prop.table(xtabs(indwt~diff, data=ti16, na.action = "na.omit"))
table(ti16$diff) #overall 700 case





## Weighted proportional tables 2018:
ti18$lukoil<-ti18$q65_1
ti18$lukoil[ti18$lukoil==-3]<-NA
ti18$lukoil[ti18$lukoil==-7]<-NA
prop.table(xtabs(indwt~lukoil, data=ti18, na.action = "na.omit"))
table(ti18$lukoil) #overall 637 case

ti18$rompetrol<-ti18$q65_2
ti18$rompetrol[ti18$rompetrol==-3]<-NA
ti18$rompetrol[ti18$rompetrol==-7]<-NA
prop.table(xtabs(indwt~rompetrol, data=ti18, na.action = "na.omit"))
table(ti18$rompetrol) #overall 637 case

ti18$gulf<-ti18$q65_3
ti18$gulf[ti18$gulf==-3]<-NA
ti18$gulf[ti18$gulf==-7]<-NA
prop.table(xtabs(indwt~gulf, data=ti18, na.action = "na.omit"))
table(ti18$gulf) #overall 637 case

ti18$socar<-ti18$q65_4
ti18$socar[ti18$socar==-3]<-NA
ti18$socar[ti18$socar==-7]<-NA
prop.table(xtabs(indwt~socar, data=ti18, na.action = "na.omit"))
table(ti18$socar) #overall 637 case

ti18$wissol<-ti18$q65_5
ti18$wissol[ti18$wissol==-3]<-NA
ti18$wissol[ti18$wissol==-7]<-NA
prop.table(xtabs(indwt~wissol, data=ti18, na.action = "na.omit"))
table(ti18$wissol) #overall 637 case

ti18$diff<-ti18$q65_6
ti18$diff[ti18$diff==-3]<-NA
ti18$diff[ti18$diff==-7]<-NA
prop.table(xtabs(indwt~diff, data=ti18, na.action = "na.omit"))
table(ti18$diff) #overall 637 case


ti18$other<-ti18$q65_7
ti18$other[ti18$other==-3]<-NA
ti18$other[ti18$other==-7]<-NA
prop.table(xtabs(indwt~other, data=ti18, na.action = "na.omit"))
table(ti18$other) #overall 637 case




## Weighted proportional tables 2019:
ti19$lukoil<-ti19$q71_1
ti19$lukoil[ti19$lukoil==-3]<-NA
ti19$lukoil[ti19$lukoil==-7]<-NA
ti19$lukoil[ti19$lukoil==-9]<-NA
prop.table(xtabs(indwt~lukoil, data=ti19, na.action = "na.omit"))
table(ti19$lukoil) #overall 801 case

ti19$rompetrol<-ti19$q71_2
ti19$rompetrol[ti19$rompetrol==-3]<-NA
ti19$rompetrol[ti19$rompetrol==-7]<-NA
ti19$rompetrol[ti19$rompetrol==-9]<-NA
prop.table(xtabs(indwt~rompetrol, data=ti19, na.action = "na.omit"))
table(ti19$rompetrol) #overall 801 case

ti19$gulf<-ti19$q71_3
ti19$gulf[ti19$gulf==-3]<-NA
ti19$gulf[ti19$gulf==-7]<-NA
ti19$gulf[ti19$gulf==-9]<-NA
prop.table(xtabs(indwt~gulf, data=ti19, na.action = "na.omit"))
table(ti19$gulf) #overall 801 case

ti19$socar<-ti19$q71_4
ti19$socar[ti19$socar==-3]<-NA
ti19$socar[ti19$socar==-7]<-NA
ti19$socar[ti19$socar==-9]<-NA
prop.table(xtabs(indwt~socar, data=ti19, na.action = "na.omit"))
table(ti19$socar) #overall 801 case

ti19$wissol<-ti19$q71_5
ti19$wissol[ti19$wissol==-3]<-NA
ti19$wissol[ti19$wissol==-7]<-NA
ti19$wissol[ti19$wissol==-9]<-NA
prop.table(xtabs(indwt~wissol, data=ti19, na.action = "na.omit"))
table(ti19$wissol) #overall 801 case

ti19$diff<-ti19$q71_6
ti19$diff[ti19$diff==-3]<-NA
ti19$diff[ti19$diff==-7]<-NA
ti19$diff[ti19$diff==-9]<-NA
prop.table(xtabs(indwt~diff, data=ti19, na.action = "na.omit"))
table(ti19$diff) #overall 801 case

ti19$other<-ti19$q71_7
ti19$other[ti19$other==-3]<-NA
ti19$other[ti19$other==-7]<-NA
ti19$other[ti19$other==-9]<-NA
prop.table(xtabs(indwt~other, data=ti19, na.action = "na.omit"))
table(ti19$other) #overall 801 case

### Data transformation for model
table(ti19$wissol)
ti19$wissol_r<-ti19$wissol
ti19$wissol_r[ti19$wissol_r<0]<-NA
table(ti19$wissol_r)

table(ti19$wissol)
ti19$wissol_only<-ti19$wissol
ti19$wissol_only[ti19$wissol_only<0]<-NA
ti19$wissol_only[ti19$wissol_only==1 & ti19$rompetrol==1]<-0
ti19$wissol_only[ti19$wissol_only==1 & ti19$gulf==1]<-0
ti19$wissol_only[ti19$wissol_only==1 & ti19$lukoil==1]<-0
ti19$wissol_only[ti19$wissol_only==1 & ti19$socar==1]<-0
ti19$wissol_only[ti19$wissol_only==1 & ti19$other==1]<-0
table(ti19$wissol_only)




ti19$socar_r<-ti19$socar
ti19$socar_r[ti19$socar_r<0]<-NA
table(ti19$socar_r)

ti19$lukoil_r<-ti19$lukoil
ti19$lukoil_r[ti19$lukoil_r<0]<-NA
table(ti19$lukoil_r)

ti19$rompetrol_r<-ti19$rompetrol
ti19$rompetrol_r[ti19$rompetrol_r<0]<-NA
table(ti19$rompetrol_r)

ti19$gulf_r<-ti19$gulf
ti19$gulf_r[ti19$gulf_r<0]<-NA
table(ti19$gulf_r)

ti19$diff_r<-ti19$diff
ti19$diff_r[ti19$diff_r<0]<-NA
table(ti19$diff_r)

ti19$other_r<-ti19$other
ti19$other_r[ti19$other_r<0]<-NA
table(ti19$other_r)



table(ti19$stratum)
ti19$stratum_r<-ti19$stratum
ti19$stratum_r<-factor(ti19$stratum_r, levels = c(1,2,3), labels = c("Capital", "Urban", "Rural"))
table(ti19$stratum_r)


table(ti19$sex)
ti19$sex_r<-ti19$sex
ti19$sex_r<-factor(ti19$sex_r, levels = c(1,2), labels = c("Male", "Female"))
table(ti19$sex_r)


table(ti19$Age3)
ti19$age_r<-ti19$Age3
ti19$age_r<-factor(ti19$age_r, levels = c(1,2,3), labels = c("18-34", "35-54", "55+"))
table(ti19$age_r)



table(ti19$q1)
ti19$q1_r<-ti19$q1
ti19$q1_r[ti19$q1_r<0]<-NA
ti19$q1_r[ti19$q1_r<4]<-1
ti19$q1_r[ti19$q1_r==4]<-2
ti19$q1_r[ti19$q1_r>4]<-3
ti19$q1_r<-factor(ti19$q1_r, levels = c(1,2,3), labels = c("Secondary or lower", "Technical", "Incomplete or complete tertiary"))
table(ti19$q1_r)


table(ti19$q72_1)
table(ti19$q72_2)
table(ti19$q72_3)
table(ti19$q72_4)
table(ti19$q72_5)
table(ti19$q72_6)
table(ti19$q72_7)
table(ti19$q72_8)

ti19$quality<-ti19$q72_1
ti19$quality[ti19$quality<0]<-0
ti19$quality<-factor(ti19$quality, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$quality)
prop.table(xtabs(indwt~quality, data=ti19, na.action = "na.omit"))

ti19$prices<-ti19$q72_2
ti19$prices[ti19$prices<0]<-0
ti19$prices<-factor(ti19$prices, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$prices)
prop.table(xtabs(indwt~prices, data=ti19, na.action = "na.omit"))

ti19$contests_bonus<-ti19$q72_3
ti19$contests_bonus[ti19$contests_bonus<0]<-0
ti19$contests_bonus<-factor(ti19$contests_bonus, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$contests_bonus)
prop.table(xtabs(indwt~contests_bonus, data=ti19, na.action = "na.omit"))

ti19$territory<-ti19$q72_4
ti19$territory[ti19$territory<0]<-0
ti19$territory<-factor(ti19$territory, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$territory)
prop.table(xtabs(indwt~territory, data=ti19, na.action = "na.omit"))

ti19$not_cheating<-ti19$q72_5
ti19$not_cheating[ti19$not_cheating<0]<-0
ti19$not_cheating<-factor(ti19$not_cheating, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$not_cheating)
prop.table(xtabs(indwt~not_cheating, data=ti19, na.action = "na.omit"))

ti19$not_falsified<-ti19$q72_6
ti19$not_falsified[ti19$not_falsified<0]<-0
ti19$not_falsified<-factor(ti19$not_falsified, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$not_falsified)
prop.table(xtabs(indwt~not_falsified, data=ti19, na.action = "na.omit"))

ti19$has_this_fuel<-ti19$q72_7
ti19$has_this_fuel[ti19$has_this_fuel<0]<-0
ti19$has_this_fuel<-factor(ti19$has_this_fuel, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$has_this_fuel)
prop.table(xtabs(indwt~has_this_fuel, data=ti19, na.action = "na.omit"))

ti19$other_reason<-ti19$q72_8
ti19$other_reason[ti19$other_reason<0]<-0
ti19$other_reason<-factor(ti19$other_reason, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$other_reason)
prop.table(xtabs(indwt~other_reason, data=ti19, na.action = "na.omit"))

table(ti19$q3)
ti19$job<-ti19$q3
ti19$job[ti19$job<1]<-0
ti19$job<-factor(ti19$job, levels=c(0,1), labels = c("Did not interest in finding a job", "Interested in finding a job"))
table(ti19$job)




table(ti19$q2_1)
ti19$contract<-ti19$q2_1
ti19$contract[ti19$contract<0]<-NA
#ti19$contract<-factor(ti19$contract, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$contract)

table(ti19$q2_2)
ti19$without_contract<-ti19$q2_2
ti19$without_contract[ti19$without_contract<0]<-NA
#ti19$without_contract<-factor(ti19$without_contract, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$without_contract)


table(ti19$q2_3)
ti19$self_empl_registred<-ti19$q2_3
ti19$self_empl_registred[ti19$self_empl_registred<0]<-NA
#ti19$self_empl_registred<-factor(ti19$self_empl_registred, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$self_empl_registred)


table(ti19$q2_4)
ti19$self_empl_not_registred<-ti19$q2_4
ti19$self_empl_not_registred[ti19$self_empl_not_registred<0]<-NA
#ti19$self_empl_not_registred<-factor(ti19$self_empl_not_registred, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$self_empl_not_registred)


table(ti19$q2_5)
ti19$unemployed<-ti19$q2_5
ti19$unemployed[ti19$unemployed<0]<-NA
#ti19$unemployed<-factor(ti19$unemployed, levels = c(0,1), labels = c("Not mentioned", "Mentioned"))
table(ti19$unemployed)


ti19$emp_stat<-ti19$unemployed
table(ti19$emp_stat)
ti19$emp_stat[ti19$contract==1 | ti19$without_contract==1 | ti19$self_empl_registred==1 | ti19$self_empl_not_registred==1]<-2
ti19$emp_stat<-factor(ti19$emp_stat, levels = c(1,2), labels = c("Unemployed", "Employed"))


names(ti19)

#survey settings
ti_svy<-svydesign(~psu, strata = ti19$substratum, weights = ti19$indwt, data = ti19)
#Wissol model__________________________________________________________________________________________________----
model_ti19<-svyglm(wissol_r~stratum_r+
                    age_r+
                    q1_r+
                    emp_stat+                  
                    sex_r+
                    quality+
                    prices+
                    contests_bonus+
                    territory+
                    not_cheating+
                    not_falsified, design = ti_svy, family="binomial")
summary(model_ti19)

print(ggemmeans(model_ti19, terms = "contests_bonus"))
print(ggemmeans(model_ti19, terms = "prices")) #%>% plot()

wiss1<-ggemmeans(model_ti19, terms = "contests_bonus")
wiss2<-ggemmeans(model_ti19, terms = "prices")


openxlsx::write.xlsx(wiss1, "wissol1_tabs.xlsx")
openxlsx::write.xlsx(wiss2, "wissol2_tabs.xlsx")
#check interrelation between stratum and prices
model_ti19_in<-svyglm(wissol_r~stratum_r*prices+
                     age_r+
                     q1_r+
                     emp_stat+                  
                     sex_r+
                     quality+
                     contests_bonus+
                     territory+
                     not_cheating+
                     not_falsified, design = ti_svy)
summary(model_ti19_in)

print(ggemmeans(model_ti19_in, terms = c("stratum_r","prices")))
tablepred<-ggemmeans(model_ti19_in, terms = c("stratum_r","prices")) #%>% plot()
print(tablepred)
openxlsx::write.xlsx(tablepred, "interactiontabl.xlsx", overwrite = TRUE)



#model on prices
model_ti19_price<-svyglm(prices~wissol_r+
                    gulf_r+
                    lukoil_r+
                    rompetrol_r+
                    socar_r+
                    diff_r+
                    other_r+
                    age_r+
                     q1_r+
                     emp_stat+                  
                      stratum_r+ sex_r, design = ti_svy, family="binomial")

summary(model_ti19_price)



##_________New data_____________________________________________________________________________________________________________



recode_data <- function(x){
  x = as_factor(x)
  x[str_detect(x, regex("Break(.*)off|Breakoff|Legal skip|Interviewer error", ignore_case = T))] = NA
  x = fct_relevel(x, c("Other"), after=0)
  x = fct_relevel(x, c("Don't know", "Don t know", "Don't know"), after=0)
  x = fct_relevel(x, c("Refuse to answer", "Don t know", "Don't know"), after=0)
  x = fct_relevel(x, c("Not applicable"), after=0)
  x = droplevels(x)
}


### Fonts


loadfonts(device = "win")

### Themes

theme_crrc <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(family = "Lato", face="bold", size = 14),
      text = element_text(family= "Lato"),
      plot.title = element_text(size=14, face="bold", family="Lato", hjust = 0),
      plot.subtitle = element_text(size=12, family="Lato", hjust=0),
      axis.text = element_text(size=12, family="Lato", color = "black"),
      legend.position = "none"
    )
}


### Get data

### same but also accesses labels

get_freq <- function(data, var) {
  
  label <- attr(data$variables[[var]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    labelled::set_variable_labels(
      !!as.name(var) := label,
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    )
}


### get multiple frequency

get_multi_freq <- function(data, var, strip_part) {
  
  label <- attr(data$variables[[var]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var)
    ) %>%
    mutate(
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    )
}

### Get a crosstab

get_cross <- function(data, var, cross, strip_part) {
  
  label <- attr(data$variables[[var]], "label")
  
  cross_label <- attr(data$variables[[cross]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var)),
      !!as.name(cross) := recode_data(!!as.name(cross))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(cross), !!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var),
      by_var = !!as.name(cross)
    ) %>%
    mutate(
      x_lab = as.character(cross_label),
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    )
}



### CHARTING

##### Base color for categories: #0092A7

base_color <- "#0092A7"

color_general_categories <- function(data, var) {
  
  get_freq(data, {{var}}) -> freq
  
  list(c("#999999", "#444444"), rep(base_color, unlist(nrow(freq)-2))) %>%unlist()
}


color_yesno <- c("#444444", "#999999", "#3EBCD2",  "#DB444C")

color_three <- c("#444444", "#999999", "#3EBCD2", "#FACA58",  "#DB444C")

color_five_cat <- c("#d7263d", "#f46036", "#2e294e", "#1b998b", "#c5d86d")

color_nine_cat <- c("#C7303C", "#1270A8", "#00788D", "#00786B", "#C28812", "#667100", "#925977", "#826636", "#576E79")

color_intensity <- c("#444444","#999999", "#00786B", "#2E9284", "#69C9B9", "#FFDA82", "#FACA58", "#E7B030", "#C28812")

color_intensity2 <- c("#444444","#999999", "#00786B", "#2E9284", "#FACA58", "#E7B030", "#C28812")

##### Define chart making functions
### TODO: (1) soft-code export directory, (2) different sizes for PPT and publications

### single_categorical_variable, unsorted

single_cat_unsorted <- function(data, var, palette, limit) {
  get_freq(data, var) %>%
    data.frame()%>%
    ggplot(aes(!!as.name(var), proportion, fill=!!as.name(var), label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))))+
    geom_text(hjust = -1.1, nudge_x = 0.1, family="Lato", size=4, fontface = "bold")+
    scale_fill_manual(values=palette(data, var))+
    ylim(0, limit)+
    geom_col()+
    coord_flip()+
    theme_crrc()
  # ggsave(paste0("D://", var, ".pdf"), width=13, height=5)
  
}

### single_categorical_variable, sorted

single_cat_sorted <- function(data, var, palette, limit) {
  get_freq(data, var) %>%
    data.frame()%>%
    mutate(
      !!as.name(var) := fct_reorder(!!as.name(var), proportion),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Other"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Don't know", "Don t know", "Don't know"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Refuse to answer", "Don t know", "Don't know"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Not applicable"), after=0),
    ) %>%
    ggplot(aes(!!as.name(var), proportion, fill=!!as.name(var), label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))))+
    geom_text(hjust = -1.1, nudge_x = 0.1, family="Lato", size=4, fontface = "bold")+
    scale_fill_manual(values=palette(data, var))+
    ylim(0, limit)+
    #  labs(
    #   title = attr(data$variables[[var]], "label")
    #  )+
    geom_col()+
    coord_flip()+
    theme_crrc()
  # ggsave(paste0("D://", var, ".pdf"), width=13, height=5)
}






### Recode data

omjul22<-read_dta("omnibus_CRRC_Omnibus_Public_Wave13.dta")


omjul22 %>%
  mutate(
    ## Covariates
    nato_supp = recode_data(e1_1),
    eu_supp = recode_data(e1_2),
    eeu_supp = recode_data(e1_3),
    party = case_when(
      m9 == 1 ~ 1, # government
      m9 %in% c(2:999) ~ 2, # opposition
      m9 == -5 ~ 3, # no party
      m9 %in% c(-1, -2) ~ 4, # DK/RA
      T ~ NA_real_
    ),
    party = factor(party, levels =c(1:4), labels = c("Government", "Opposition", "No party", "DK/RA")),
    displaced = case_when(
      d8 %in% c(-2, -1) ~ 0,
      d8 %in% c(-9, -3) ~ NA_real_,
      T ~ as.numeric(d8)
    ),
    ethnic = case_when(
      d4 == 3 ~ 1,
      T ~ 0
    ),
    employed = case_when(
      d2 == 1 ~ 1, # Private sector
      d2 == 2 ~ 2, # Public sector
      T ~ 3 # Not working
    ),
    agegroup = case_when(
      age < 35 ~ 1,
      age >= 35 & age < 55 ~ 2,
      T ~ 3
    ),
    across(starts_with("d7_"), ~case_when(
      .x %in% c(-2, -7, -1, -5, -9) ~ NA_real_,
      .x == 1 ~ 1,
      T ~ 0,
    )),
    wealth = rowSums(across(starts_with("d7_"))),
    internet = case_when(
      m2 %in% c(1:4) ~ 1,
      m2 %in% c(-9, -3) ~ NA_real_,
      T ~ 0
    ),
    e2_rec = case_when(
      e2 == 1 ~ 1,
      T ~ 0
    ),
    e3_rec = as.numeric(e3),
    e3_rec = case_when(
      e3_oth == 1 ~ 14,
      e3_oth == 2 ~ 7,
      e3_oth == 9 ~ 11,
      T ~ as.numeric(e3_rec)
    ),
    education = case_when(
      d3 %in% c(1, 2) ~ 1,
      d3 == 3 ~ 2,
      d3 %in% c(4, 5) ~ 3,
      T ~ NA_real_
    ),
    e4_rec = case_when(
      e4 %in% c(1, 2) ~ 1,
      e4 %in% c(3, 4) ~ 3,
      e4 %in% c(-2, -1) ~ 2,
      T ~ NA_real_
    ),
    e7_rec = case_when(
      e7 %in% c(1, 2) ~ 1,
      e7 %in% c(3, 4) ~ 3,
      e7 %in% c(-2, -1) ~ 2,
      T ~ NA_real_
    ),
    education = factor(education, levels =c(1, 2, 3), labels = c("Secondary or lower", "Vocational", "Higher")),
    agegroup = factor(agegroup, levels = c(1, 2, 3), labels = c("18-34", "35-54", "55+")),
    e4_rec = factor(e4_rec, levels = c(1, 2, 3), labels = c("False", "DK/RA", "True")),
    e7_rec = factor(e7_rec, levels = c(1, 2, 3), labels = c("Not acceptable", "DK/RA", "Acceptable")),
    employed = factor(employed, levels = c(1:3), labels = c("Private", "Public", "Not working")),
    settlement = as_factor(stratum),
    ethnic = factor(ethnic, levels = c(0, 1), labels = c("Other", "Georgian")),
    sex = as_factor(sex),
    ## Substantive variables
    across(starts_with("m8_"), ~case_when(
      .x %in% c(-9, -3) ~ NA_real_,
      .x %in% c(-1, -2) ~ 98,
      .x %in% c(-5) ~ 98,
      .x %in% c(0) ~ 2,
      T ~ as.numeric(.x)
    )),
  ) %>%
  labelled::add_value_labels(
    e3_rec = c(
      labelled::val_labels(omjul22$e3),
      "EU does not want/need Georgia" = 14
    )
  ) %>%
  labelled::set_variable_labels(
    party = "Party affiliation",
    agegroup = "Age groups",
    wealth = "Wealth index",
    employed = "Employment status",
    education = "Education",
    ethnic = "Ethnic ID",
    e3_rec = "The main reason why Georgia was not granted an EU candidate status?",
    e4_rec = "True that Georgia would gain candidate status if engaged in war against Russia?",
    e7_rec = attr(omjul22$e7, "label"),
    eu_supp = "Support or oppose EU membership?"
  )-> om_coded

om_coded$f2r<-om_coded$f2
table(om_coded$f2r)
om_coded$f2r[om_coded$f2r<0]<-NA
om_coded$f2r<-factor(om_coded$f2r, levels = c(0,1), label=c("No", "Yes"))

om_coded$f3r<-om_coded$f3
table(om_coded$f3r)
om_coded$f3r[om_coded$f3r<0]<-NA

#### Make survey objects
om_coded %>%
  as_survey_design(ids = 1,
                   weights = weight, data = om_coded) -> om_svy


names(om_svy$variables)
get_freq(om_svy, "f2r")
get_freq(om_svy, "employed")

f2_use <- svyglm(f2r~sex+stratum+ethnic+agegroup+wealth+employed+education+internet+party,
                       design = om_svy, family = "binomial")

summary(f2_use)
broom::tidy(f2_use)


ggeffects::ggpredict(f2_use, c( "employed"))
ggemmeans(f2_use, terms = "employed")->f2r_pred
openxlsx::write.xlsx(f2r_pred, "employed.xlsx", overwrite = FALSE)


get_freq(om_svy,"f3")
table(om_svy$variables$f3)

om_svy$variables$f3r
mean(om_svy$variables$f3r)










####

price1<-ggemmeans(model_ti19_price, terms = "wissol_r")
price2<-ggemmeans(model_ti19_price, terms = "diff_r")
price3<-ggemmeans(model_ti19_price, terms = "other_r")
price4<-ggemmeans(model_ti19_price, terms = "rompetrol_r")
price5<-ggemmeans(model_ti19_price, terms = "gulf_r")
price6<-ggemmeans(model_ti19_price, terms = "lukoil_r")
price7<-ggemmeans(model_ti19_price, terms = "socar_r")



openxlsx::write.xlsx(price1, "wissol_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price2, "diff_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price3, "other_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price4, "rompetrol_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price5, "gulf_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price6, "lukoil_main1.xlsx", overwrite = FALSE)
openxlsx::write.xlsx(price7, "socar_main1.xlsx", overwrite = FALSE)


#socar consumers

summary(svyglm(socar_r~stratum_r+
                     age_r+
                     q1_r+
                     emp_stat+                  
                     sex_r+
                     quality+
                     prices+
                     contests_bonus+
                     territory+
                     not_cheating+
                     not_falsified, design = ti_svy, family="binomial"))

# employment status was significantly associated
print(ggemmeans(svyglm(socar_r~stratum_r+
                         age_r+
                         q1_r+
                         emp_stat+                  
                         sex_r+
                         quality+
                         prices+
                         contests_bonus+
                         territory+
                         not_cheating+
                         not_falsified, design = ti_svy, family="binomial"), terms = "emp_stat")) #%>% plot()

openxlsx::write.xlsx(ggemmeans(svyglm(socar_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "emp_stat"), "socar_tabs.xlsx", sheetName=1)
#lukoil


summary(svyglm(lukoil_r~stratum_r+
                 age_r+
                 q1_r+
                 emp_stat+                  
                 sex_r+
                 quality+
                 prices+
                 contests_bonus+
                 territory+
                 not_cheating+
                 not_falsified, design = ti_svy, family="binomial"))

openxlsx::write.xlsx(ggemmeans(svyglm(lukoil_r~stratum_r+
                   age_r+
                   q1_r+
                   emp_stat+                  
                   sex_r+
                   quality+
                   prices+
                   contests_bonus+
                   territory+
                   not_cheating+
                   not_falsified, design = ti_svy, family="binomial"), terms = "stratum_r"), "luko1_tabs.xlsx", sheetName="luk1")

openxlsx::write.xlsx(ggemmeans(svyglm(lukoil_r~stratum_r+
                   age_r+
                   q1_r+
                   emp_stat+                  
                   sex_r+
                   quality+
                   prices+
                   contests_bonus+
                   territory+
                   not_cheating+
                   not_falsified, design = ti_svy, family="binomial"), terms = "q1_r"), file="luko2_tabs.xlsx")

#openxlsx::write.xlsx(ggemmeans(svyglm(lukoil_r~stratum_r+
#                   age_r+
#                   q1_r+
#                   emp_stat+                  
#                   sex_r+
#                   quality+
#                   prices+
#                   contests_bonus+
#                   territory+
#                   not_cheating+
#                   not_falsified, design = ti_svy, family="binomial"), terms = "contests_bonus"), file="luko3_tabs.xlsx")

openxlsx::write.xlsx(ggemmeans(svyglm(lukoil_r~stratum_r+
                   age_r+
                   q1_r+
                   emp_stat+                  
                   sex_r+
                   quality+
                   prices+
                   contests_bonus+
                   territory+
                   not_cheating+
                   not_falsified, design = ti_svy, family="binomial"), terms = "not_falsified"), file="luko4_tabs.xlsx")
#rompetrol

summary(svyglm(rompetrol_r~stratum_r+
                 age_r+
                 q1_r+
                 emp_stat+                  
                 sex_r+
                 quality+
                 prices+
                 contests_bonus+
                 territory+
                 not_cheating+
                 not_falsified, design = ti_svy, family="binomial"))

openxlsx::write.xlsx(ggemmeans(svyglm(rompetrol_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy,  family="binomial"), terms = "quality"), "romp1_tabs.xlsx", sheetName="romp1")

openxlsx::write.xlsx(ggemmeans(svyglm(rompetrol_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "q1_r"), file="romp2_tabs.xlsx")

openxlsx::write.xlsx(ggemmeans(svyglm(rompetrol_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "prices"), file="romp3_tabs.xlsx")


#GUlf
summary(svyglm(gulf_r~stratum_r+
                 age_r+
                 q1_r+
                 emp_stat+                  
                 sex_r+
                 quality+
                 prices+
                 contests_bonus+
                 territory+
                 not_cheating+
                 not_falsified, design = ti_svy, family="binomial"))

openxlsx::write.xlsx(ggemmeans(svyglm(gulf_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "contests_bonus"), "gulf1_tabs.xlsx", sheetName="romp1")

openxlsx::write.xlsx(ggemmeans(svyglm(gulf_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "q1_r"), file="gulf2_tabs.xlsx")



#different company fuel
summary(svyglm(diff_r~stratum_r+
                 age_r+
                 q1_r+
                 emp_stat+                  
                 sex_r+
                 quality+
                 prices+
                 contests_bonus+
                 territory+
                 not_cheating+
                 not_falsified, design = ti_svy, family="binomial"))

openxlsx::write.xlsx(ggemmeans(svyglm(diff_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "stratum_r"), "diff1_tabs.xlsx", sheetName="luk1")

openxlsx::write.xlsx(ggemmeans(svyglm(diff_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "quality"), file="diff2_tabs.xlsx")

openxlsx::write.xlsx(ggemmeans(svyglm(diff_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "prices"), file="diff3_tabs.xlsx")

openxlsx::write.xlsx(ggemmeans(svyglm(diff_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "territory"), file="diff4_tabs.xlsx")


#other company fuel
summary(svyglm(other_r~stratum_r+
                 age_r+
                 q1_r+
                 emp_stat+                  
                 sex_r+
                 quality+
                 prices+
                 contests_bonus+
                 territory+
                 not_cheating+
                 not_falsified, design = ti_svy, family="binomial"))


openxlsx::write.xlsx(ggemmeans(svyglm(other_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "stratum_r"), "oth1_tabs.xlsx", sheetName="luk1")

openxlsx::write.xlsx(ggemmeans(svyglm(other_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "contests_bonus"), file="oth2_tabs.xlsx")

openxlsx::write.xlsx(ggemmeans(svyglm(other_r~stratum_r+
                                        age_r+
                                        q1_r+
                                        emp_stat+                  
                                        sex_r+
                                        quality+
                                        prices+
                                        contests_bonus+
                                        territory+
                                        not_cheating+
                                        not_falsified, design = ti_svy, family="binomial"), terms = "prices"), file="oth3_tabs.xlsx")
