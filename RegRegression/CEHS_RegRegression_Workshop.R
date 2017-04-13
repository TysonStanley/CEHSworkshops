## CEHS Workshop
## Lasso, Ridge, Elastic Net
## Tyson S. Barrett

## Libraries
library(tidyverse)
library(furniture)
library(glmnet)
library(anteo)
library(ggthemes)
library(magrittr)

## Set Seed
set.seed(84322)

## Youth Risk Behavior Survey 2015
d = read.csv("~/Dropbox/1 Dissertation/ElasticNet/YRBS_Dichotomous.csv") %>% 
  filter(complete.cases(record)) %>%
  select(-site, -record, -psu, -weight, -stratum) %>%
  mutate(mari   = washer(qn47, 2, value = 0)) %>%    ## ever used
  mutate(mari_c = washer(qn49, 2, value = 0)) %>%    ## current
  mutate(mari_s = washer(qn55, 2, value = 0)) %>%    ## synthetic used
  mutate(asthma = washer(qn87, 2, value = 0)) %>%    ## doctor diagnosed with asthma
  select(-qn47, -qn48, -qn49, -qn55, -qn87, -q5, -q7orig, -q6orig,
         -qn67, -qn33, -qn38, - qn68, -raceeth, -qn85) %>%
  dmap(~washer(.x, 2, value = 0)) %>%
  data.frame
d2 = list()

## If missing > 10% remove variable
for (i in names(d)){
  if (sum(is.na(d[[i]])) < 1700){
    d2[[i]] = d[,i]
  } else {
    d2[[i]] = NULL
  }
}
d2 = data.frame(d2) %>%
  dmap(~factor(.x)) %>%
  mutate(bmi = as.numeric(as.character(BMIPCT))/100,
         q7  = as.numeric(as.character(q7)),
         q6  = as.numeric(as.character(q6))) %>%
  select(-BMIPCT)       

## Only those with Asthma
d_nm = d2 %>% 
  filter(asthma == 1) %>%
  na.omit

## Data Exploration
d2 %>%
  table1(qndaycig, 
         splitby = ~asthma,
         test = TRUE,
         format_output = "full")
d_nm %>%
  select(-qntob4, -qntob3, -qntob2, -qnnotob4, -qnnotob3, qnnotob2, -asthma) %>%
  table1(type = "condensed",
         splitby = ~qndaycig,
         test = TRUE)
d_nm %>%
  group_by(qndaycig) %>%
  summarise(mari_use       = mean(as.numeric(as.character(mari_c)), na.rm=TRUE),
            obese          = mean(as.numeric(as.character(qnobese)), na.rm=TRUE),
            carried_weapon = mean(as.numeric(as.character(qn13)), na.rm=TRUE),
            phys_fight     = mean(as.numeric(as.character(qn20)), na.rm=TRUE)) %>%
  gather("var", "value", 2:5) %>%
  mutate(var = factor(var, labels = c("Carried\nWeapon", "Currently\nUse\nMarijuana", "Obese", "Physical\nFight"))) %>%
  ggplot(aes(x = var, y = value, group = qndaycig, fill = qndaycig)) +
    geom_bar(stat = "identity", position = "dodge",
             alpha = .75) +
    labs(y = "Proportion",
         x = "") +
    annotate("text", x = 2.65, y = .6, label = "Daily Smoker",
             color = "firebrick4", hjust = 0) +
    annotate("segment", y = .6, yend = .60, x = 2.6, xend = 2.4, 
            color = "firebrick4") +
    annotate("text", x = 2.6, y = .3, label = "Not a\nDaily Smoker", 
             color = "dodgerblue4", hjust = 0) + 
    annotate("segment", y = .255, yend = .18, x = 2.75, xend = 2.8, 
             color = "dodgerblue4") +
    anteo::theme_anteo_wh() +
    theme(legend.position ="none") +
    scale_fill_manual(values = c("dodgerblue4", "firebrick4"))

## Models
X = d_nm %>%
  select(-mari, -mari_c, -mari_s, -asthma, -qnnotob4, -qnnotob2, -qnnotob3) %>%
  data.frame
X = model.matrix( ~ ., X)[, -1]
fit.cv = cv.glmnet(X, d_nm$mari, alpha = .8, family = "binomial", standardize = TRUE)
fit.cv
coef(fit.cv, s = "lambda.1se")
fit = glmnet(X, d_nm$mari, alpha = .8, family = "binomial", standardize = TRUE)
fit
coef(fit)


## Figure 1
par(mfrow = c(3,1))
plot(fit)

vars1 = as.matrix(cbind(coef(fit, s="lambda.1se"), coef(fit2, s="lambda.1se"), coef(fit3, s="lambda.1se")))
vars1

d_nm = d_nm %>%
  mutate(q3 = factor(ifelse(q3 == 5, NA, q3)))
## Final Model
fit_glm = glm(mari ~ q1 + q3 + qn10 + qn20 + qn26 + qn32 + qn39 +
                qn41 + qn42 + qn43 + qn44 + qn50 + qn54 + qn57 + 
                qn59 + qn60 + qn75 + qn80 + qn81 + qn83 + qn89 + 
                qntob4 + qntob2 +
                bmi, data = d_nm, family = "binomial")
summary(fit_glm)  ## 24 vars
fit_glm2 = glm(mari_c ~ qn39 + qn40 + qn43 + qn44 + qn54 + qn57 + 
                 qn59 + qn60 + qntob4 + qntob2, 
               data = d_nm, family = "binomial")
summary(fit_glm2)  ## 10 vars
fit_glm3 = glm(mari_s ~ qn13 + qn15 + qn32 + qn39 + qn40 + qn44 +
                 qn50 + qn51 + qn53 + qn54 + qn57 + qndaycig +
                 qn60 + qntob4 + qntob3 + qntob2, 
               data = d_nm, family = "binomial")
summary(fit_glm3)  ## 16 vars

ors = rbind(cbind(exp(coef(fit_glm)),  exp(confint(fit_glm))),
            cbind(exp(coef(fit_glm2)), exp(confint(fit_glm2))),
            cbind(exp(coef(fit_glm3)), exp(confint(fit_glm3))))
ors2 = data.frame(ors, row.names = NULL)
ors2$vars = row.names(ors)
names(ors2) = c("Estimate", "Lower", "Upper", "Variable")
ors2$outcome = c(rep("Ever", 30), rep("Currently", 11), rep("Synthetic", 17))
ors3 = ors2 %>%
  filter(!Variable %in% c("(Intercept)", "qn541")) %>%
  mutate(Variable = factor(Variable, labels = c("BMI", "Age: 15", "Age: 16", "Age: 17",
                                                "Age: 18", "9th Grade", "11th Grade",
                                                "12th Grade", "Rode with Drinker",
                                                "Carried Weapon", "Weapon on School",
                                                "Fight at School", "Unsafe at School",
                                                "Cigarette Before 13", "Vaped", 
                                                "Currently Vape", "Ever Drank", 
                                                "Drank Before 13", "Currently Drink",
                                                "Drank 5+ Conseq", "Cocaine",
                                                "Inhalants", "Meth",
                                                "Prescription", "Drugs at School",
                                                "Had Sex", "No Eat Carrots", 
                                                "Active 60", "TV 3+ Hours Daily", 
                                                "Attend PE Class", 
                                                "Mostly A's or B's", "Cigarettes Daily",
                                                "Current Cigar(ette)", "Current Tobacco", 
                                                "Current Any Tobacco"))) %>%
  mutate(class1 = factor(ifelse(Variable %in% c("BMI", "No Eat Carrots", "TV 3+ Hours Daily", "Attend PE Class", "Active 60"), "Health",
                                ifelse(Variable %in% c("Carried Weapon", "Weapon on School", "Fight at School"), "Violence",
                                       ifelse(Variable %in% c("Cigarette Before 13", "Vaped", "Currently Vape", "Ever Drank", "Drank Before 13", 
                                                              "Currently Drink", "Drank 5+ Conseq", "Cocaine", "Inhalants", "Meth", "Ecstacy", 
                                                              "Prescription", "Drugs at School", "Cigarettes Daily", "Current Cigar(ette)", 
                                                              "Current Tobacco", "Current Any Tobacco"), "Drug Use",
                                              ifelse(Variable %in% c("Had Sex", "Rode with Drinker"), "Other\nRisk", "Maturity"))))))
ggplot(ors3, aes(x = outcome, y = Variable, 
                 color = factor(ifelse(Lower > 1 | Upper < 1, 1, 0), 
                                labels = c("No", "Yes")),
                 fill  = factor(ifelse(Lower > 1 | Upper < 1, 1, 0), 
                                labels = c("No", "Yes")))) +
  geom_tile(color = "grey50", alpha = .85) +
  geom_text(aes(label = paste0(round(Estimate, 3), 
                               " (", round(Lower, 2), ", ",
                               round(Upper, 2), ")")),
            size = 2.5) +
  scale_color_manual(guide = FALSE,
                     values = c("grey10", "white")) +
  scale_fill_manual(name = "Significant\nat p < .05",
                    values = c("white","slategray4")) +
  theme_tufte(base_family="Helvetica") +
  labs(y = "",
       x = "") +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white", color = "grey90"),
        axis.line = element_line(color = "grey80"),
        axis.ticks = element_line(color = "grey70"),
        axis.text.x = element_text(face = "bold"),
        strip.text = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"),
        legend.title.align = 0,
        legend.key = element_rect(color = "darkgrey")) +
  facet_grid(class1~., scales = "free", space = "free")


## qn10 = rode with driver who had been drinking alcohol
## qn13 = carried a weapon last 30
## qn15 = carried weapon on school last 30
## qn20 = phys fight at school
## qn26 = Unsafe at School
## qn32 = smoked whole cigarette before age 13
## qn39 = vaped last 30
## qn40 = current vape
## qn41 = ever drank alcohol
## qn42 = drank before age 13
## qn43 = current drink
## qn44 = drank 5 or more drinks in a row last 30
## qn50 = ever cocaine
## qn51 = ever inhalents
## qn53 = ever meth
## qn54 = ever ecstacy
## qn57 = used prescription without prescription
## qn59 = illegal drug on school last year
## qn60 = ever sex
## qn75 = no eat carrots
## qn80 = active 60 min 5+ days
## qn81 = tv 3+ hours daily
## qn83 = attend PE class
## qn89 = mostly A's or B's
## qndaycig = smoke cigs daily
## qntob2 = currently cigs or cigars
## qntob3 = currently use tobacco last 30
## qntob4 = currently any tobacco last 30
## raceth = race/ethnicity: 
##      1 = Am Indian, 2 = Asian, 3 = Black, 4 = Pacific I, 5 = White, 
##      6 = Hispanic/Latino, 7 = Multiple Hisp, 8 = Multiple Non