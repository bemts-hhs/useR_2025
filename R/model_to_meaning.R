###______________________________________________
### Load data
###______________________________________________

# Install
# renv::install(c("tidyverse", "marginaleffects", "modelsummary", "tinytable"))

# data
dat <- marginaleffects::get_dataset("thornton")

# examine the data
head(dat)

# model
mod <- glm(
  outcome ~ incentive + distance + agecat,
  family = binomial,
  data = dat
)

# get a summary
summary(mod)

# get marginal effects
h <- marginaleffects::hypotheses(mod)

# print
print(h)

# Is the incentive coefficient different from 1.9?
marginaleffects::hypotheses(mod, hypothesis = 1.9)

# coefficients
coef(mod)

# is the agecat 18 to 35 different from agecat > 35?
coef(mod)[4] - coef(mod)[5]

# use marginaleffects to compute
# flexible formula format using coefficient positions
# danger! need to check your code to make sure your coefficient positions have not changed
marginaleffects::hypotheses(mod, hypothesis = "b4 - b5 = 0")

# get the same using a formula, safer
marginaleffects::hypotheses(mod, hypothesis = difference ~ sequential)

# a new model
mod2 <- lm(outcome ~ incentive + distance, data = dat)

# summary of new model
summary(mod)

# beta (coefficients)
beta <- coef(mod)

# print
print(beta)

# get a prediction
# because the response variable is 0 for no testing and 1 for getting tested
# the linear model will give us something like a probability, or the prediction
# will be on the scale from [0,1]
nd <- data.frame(incentive = 1, distance = 1)
print(nd)

# standard predictions
predict(mod2, newdata = nd) # 0.8215 - the person was likely to get tested, the incentive worked

# marginaleffects predictions
p <- marginaleffects::predictions(mod2, newdata = nd)
print(p)

# let's do a new model, get the link function on the logit scale
mod3 <- glm(outcome ~ incentive + distance, data = dat, family = binomial)

summary(mod3)

# get predictions
p2 <- marginaleffects::predictions(mod3, newdata = nd, type = "response")
print(p2)

# make predictions easy with a grid
# basically just dataframe of multiple values to predict on
marginaleffects::predictions(
  mod3,
  newdata = marginaleffects::datagrid(incentive = unique, distance = c(1, 2))
)

# aggregate predictions
# empirical distribution from the observed data
marginaleffects::avg_predictions(mod3, type = "response")

# aggregate by groups
# 'response' is default for binomial error term
marginaleffects::avg_predictions(mod3, by = "agecat")

# are the average predictions equal among groups?
# use the linear model, not the glm
marginaleffects::avg_predictions(
  mod,
  by = "agecat",
  hypothesis = difference ~ pairwise
)

# plotting function!
marginaleffects::plot_predictions(mod, condition = "distance") + 
  ggplot2::theme_minimal()

# get new data
titanic <- marginaleffects::get_dataset("Titanic", package = "Stat2Data")

# specify a linear model
m1 <- glm(Survived ~ PClass + Age, family = binomial, data = titanic)

# Discrete bins
m2 <- glm(Survived ~ PClass + Age + I(Age < 20))