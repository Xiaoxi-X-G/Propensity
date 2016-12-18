rm(list = ls())
# Ref: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html#exercise

RDataPath<-"C:/gxx/Database/ecls/data-processed"
require(MatchIt)
library(dplyr)
library(ggplot2)


ecls <- read.csv(paste(RDataPath, "/ecls.csv", sep=''), stringsAsFactors = F)

str(ecls)

summary(ecls)

#####1.1: Difference-in-means: outcome variable  #####
#### %>%: pass the LHS to the first argumant of the RHS
#### summarise: perform group-wise summaries

# Compared with c5r2mtsc, the outcome variable, c5r2mtsc_std,
# is a standardized (mean = 0, sd = 1).
ecls %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std),
            std_error = sd(c5r2mtsc_std) / sqrt(n_students))

# The difference-in-means is statistically significant at conventional 
# levels of confidence 
with(ecls, t.test(c5r2mtsc_std ~ catholic))

####1.2: Difference-in-means: pre-treatment covariates #####
# race_white: Is the student white (1) or not (0)?
# p5hmage: Mother's age
# w3income: Family income
# p5numpla: Number of places the student has lived for at least 4 months
# w3momed_hsb: Is the mother's education level high-school or below (1) or some college or more (0)?
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%  #select: choose interested columns;  
                      # one_of() might be useful, When a column is named as character string
  summarise_all(funs(mean(., na.rm = T))) # apply the functions to all (non-grouping) columns

## We can carry out t-tests to evaluate whether these 
# means are statistically distinguish from 'catholic'
lapply(ecls_cov, function(v){
      t.test(ecls[, v] ~ ecls[, 'catholic'])
}) # lhs is a numeric array that can be factorized using rhs


####Propensity score estimation####
# We estimate the propensity score by running a logit model (probit also works) 
# where the outcome variable is a binary variable indicating treatment status.

# What covariates should you include? For the matching to give you a 
# causal estimate in the end, you need to include any covariate that 
# is related to both the treatment assignment and potential outcomes.
# I choose just a few covariates below-they are unlikely to capture 
# all covariates that should be included. You'll be asked to come up 
# with a potentially better model on your own later.
ecls <- mutate(ecls, w3income_3k = w3income/1000)
m_ps <- glm(catholic ~ race_white + w3income_3k + p5hmage + p5numpla + w3momed_hsb,
            data = ecls,
            family = binomial())
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type="response"), #the fitted linear predictors are used.
                     catholic = m_ps$model$catholic)


#### 2.1 Examining the region of common support####
labs <- paste("Actual School Attened:",c("Catholic", "Public"))
prs_df %>%
  mutate(Catholic = ifelse(catholic==1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~Catholic)+
  xlab("Probability of going to Catholic School")

####3 Executing a matching algorithm####
# The method we use below is to find pairs of observations 
# that have very similar propensity scores, but that differ 
# in their treatment status. We use the package MatchIt for this. 
# This package estimates the propensity score in the background and 
# then matches observations based on the method of choice ("nearest" in this case).

ecls_nomiss <- ecls %>%
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
  na.omit() # delete rows with NAs, as MatchIt doesn't accept NAs

mod_match <- matchit(catholic ~ race_white + w3income + p5hmage +
                       p5numpla + w3momed_hsb,
                     method = "nearest", data = ecls_nomiss)

summary(mod_match)
plot(mod_match)
# Also note that the final dataset contains a variable 
# called distance, which is the propensity score.

# To create a dataframe containing only the matched observations, 
# use the match.data() function:
dta_m <- match.data(mod_match)
dim(dta_m)



##### 4 Examining covariate balance in the matched sample #####
# We'll do three things to assess covariate balance in the matched sample:
#1. visual inspection
#2. t-tests of difference-in-means
#3. computation of the average absolute standardized difference ("standardized imbalance")


####4.1 Visual inspection#####
# It is useful to plot the mean of each covariate against the estimated 
# propensity score, separately by treatment status. If matching is done 
# well, the treatment and control groups will have (near) identical 
# means of each covariate at each value of the propensity score.

# Below is an example using the four covariates in our model. 
# Here I use a loess smoother to estimate the mean of each covariate,
# by treatment status, at each value of the propensity score.

fn_bal <- function(dta, variable){
  dta$variable <- dta[, variable]
  if (variable == 'w3income') dta$variable <- dta$variable / 1000
  dta$catholic <- as.factor(dta$catholic)
  
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = catholic)) + 
    geom_point(alpha = 0.5, size = 1.3) + # alpha: color saturation
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    ylim(support)
}

library(gridExtra)

grid.arrange(
  fn_bal(dta_m, 'w3income') + theme(legend.position = "none"),
  fn_bal(dta_m, 'p5numpla') + theme(legend.position = "none"),
  fn_bal(dta_m, 'p5hmage')  + theme(legend.position = "none"),
  fn_bal(dta_m, 'w3momed_hsb') + theme(legend.position = "none"),
  fn_bal(dta_m, 'race_white'),
  nrow = 2, widths = c(1, 0.8, 0.8)
)


##### 4.2 Difference in mean #####
dta_m %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean)) # almost same mean for all covariate

# peform t-test to double confirm: Hyperthsis: 2 dataset are different
# t-test on the Null Hyperthesis: 2 dataset are similar
# Reject Null Hyperthesis if P-value < 0.05
lapply(ecls_cov, function(v){
        t.test(dta_m[,v] ~ dta_m$catholic)
})


#######4.3 Average absolute standardized difference#####
##1. Find \beta_i: for each covariate, it is the normalized absolute (absolute over std)
# difference in the treated and control groups in the matched samples
##2. take average of sum-up, and check if it's close to 0. 

## A close to 0 results small differences between the control 
# and treatment groups in the matched sample.
##

matchMatrix <- mod_match$match.matrix
matchitInputData <- ecls_nomiss
fn_imbal <- function(matchMatrix, matchitInputData, variable){
  temp <- data.frame(treated = matchitInputData[rownames(matchMatrix), variable],
                    matched = matchitInputData[unname(matchMatrix[,1]), variable],
                    stringsAsFactors = F)
  beta <- abs(temp[,1] - temp[,2])
  return(mean(beta, na.rm = T)/sd(beta, na.rm = T)) # normalized difference
}

imbalAvg <- 0
for (covName in ecls_cov){
  imbalAvg <- imbalAvg + fn_imbal(mod_match$match.matrix, ecls_nomiss, covName)/length(ecls_cov)
}



####5 Estimating treatment effects####
#t.test 
with(dta_m, t.test(c5r2mtsc_std ~ catholic)) # results are different in 2 groups

# Or we can use OLS with or without covariates
lm_treat1 <- lm(c5r2mtsc_std ~ catholic, data = dta_m)
summary(lm_treat1)

lm_treat2 <- lm(c5r2mtsc_std ~ catholic + race_white + p5hmage +
                  I(w3income / 10^3) + p5numpla + w3momed_hsb, data = dta_m)
summary(lm_treat2)

##Interpret these estimates!