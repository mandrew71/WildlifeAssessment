
#if(!require(corrr)){install.packages("corrr"); library(corrr)} # for correlations
if(!require(brms)){install.packages("brms"); library(brms)} # for modelling
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # for modelling

#get the data
humanData <- readRDS("Data/humanData.rds")

# Null value
modNull <- brm(occ ~ 1 + offset(log(Effort)),
               data = humanData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)


# Elevation
modElev <- brm(occ ~ scaleElevmean + offset(log(Effort)),
               data = humanData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
conditional_effects(modElev)


# Ruggedness
modRugg <- brm(occ ~ scaleRuggmean + offset(log(Effort)),
               data = humanData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# Conditional effects test

conditional_effects(modRugg)


# Elevation plus ruggedness
modElevRugg <- brm(occ ~ scaleElevmean + scaleRuggmean + offset(log(Effort)),
                   data = humanData,
                   family = bernoulli(link="logit"),
                   warmup = 2000, iter = 4000, chains = 4)
# CI 
conditional_effects(modElevRugg)


# Add criteria for comparison
mElev <- add_criterion(modElev, "loo")
m0 <- add_criterion(modNull, "loo")
mElevRugg <- add_criterion(modElevRugg, "loo")

# Compare the models with loo_comapre
loo_compare(m0, mElev, mElevRugg, criterion = "loo")

