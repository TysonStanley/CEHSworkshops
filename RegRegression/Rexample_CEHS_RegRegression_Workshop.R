## -----
## Code for CEHS Workshop R Example
## Tyson S. Barrett
## April 2017
## -----

setwd("~/Box Sync/GitHub/CEHSworkshops/RegRegression/")

## Data (data object called "df")
load("WideFormat_TheOffice.RData")

##------------------------------------##
## STEP 1. Explore and Clean the Data ##
##------------------------------------##
library(furniture)
table1(df,
       type = c("condense", "simple"))
table1(df %>% select(-Name),
       splitby = ~Sex,
       type = c("condense", "simple"))

ggplot(df, aes(x = Income)) +
  geom_histogram(binwidth = 5)

df %>%
  mutate(Sex = factor(Sex, labels = c("Male", "Female"))) %>%
  group_by(Sex) %>%
  summarise(income = mean(Income),
            incomeSE = sd(Income)/sqrt(n())) %>%
  ggplot(aes(x = Sex, y = income, group = Sex, fill = Sex, color = Sex)) +
    geom_bar(stat = "identity", alpha = .5) +
    geom_errorbar(aes(ymin = income - incomeSE, ymax = income + incomeSE),
                  width = .3) +
    scale_color_manual(values = c("chartreuse4", "coral2")) +
    scale_fill_manual(values = c("chartreuse4", "coral2")) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "",
         y = "Income (Thousands of Dollars)")

## Clean the Data
## 1. Missingness?
## 2. Redundancies?
## Here the only thing that needs help is the missing value in Depr1
df = df %>%
  filter(complete.cases(Depr1))

##-----------------------##
## STEP 2. Specify Model ##
##-----------------------##

## Research Question: Which of the various indicators is most predictive of income?
## Income is approximately continuous (not normal) but we'd need to see if the residuals showed a problem.


##--------------------##
## STEP 3. Dummy Code ##
##--------------------##

Y = df$Income
X = df %>%
  select(-Income, -Name) %>%
  data.frame
X = model.matrix(~ ., X)[,-1]

##-------------------------------##
## STEP 4. Number of Folds of CV ##
##-------------------------------##

## Let's choose 3 (since we have a small sample)

##-------------------##
## STEP 5. Fit Model ##
##-------------------##

library(glmnet)
fit.cv = cv.glmnet(X, Y, alpha = .8, nfolds = 3, standardize = TRUE)
fit.cv
coef(fit.cv, s = "lambda.min")
plot(fit.cv)
fit = glmnet(X, Y, alpha = .8, standardize = TRUE)
fit
coef(fit)

##----------------------------------------------##
## STEP 6. Select Model (fit unpenalized model) ##
##----------------------------------------------##

fit_un = lm(Income ~ Sex + Married + Prod2, data = df)
summary(fit_un)



