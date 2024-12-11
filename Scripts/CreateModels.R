# Create the models for analysis
# 10 December 2024

# library(magrittr)
# library(dplyr)
# library(purrr)
# library(forcats)
# library(tidyr)
# library(modelr)
# library(ggdist)
# library(tidybayes)
# library(ggplot2)
# library(cowplot)
# library(rstan)
# library(brms)
# library(ggrepel)
# library(RColorBrewer)
# library(gganimate)
# library(posterior)
# library(distributional)
# library(marginaleffects)

#if(!require(corrr)){install.packages("corrr"); library(corrr)} # for correlations
if(!require(brms)){install.packages("brms"); library(brms)} # for modelling

# Get the data
monkeyData <- readRDS("Data/monkeyData.rds")

# Scale the covariates
   ### Already done in GetData.R

# Examine correlations and don't combine if over 0.7
cor(dplyr::select(monkeyData,Effort:Ruggmean), method='s', use='complete.obs')
  # Road distance and village distance are highly correlated

# Add new boolean dat
### Already done in GetData.R


###-------------------------
### Create the models
###------------------------

# Null value
modNull <- brm(occ ~ 1 + offset(log(Effort)),
            data = monkeyData,
            family = bernoulli(link="logit"),
            warmup = 2000, iter = 4000, chains = 4)



# scaleEffort = scale(Effort),           # Scale values: Effort
# scaleElevmean = scale(Elevmean),       # Scale values: Elevmean
# scaleRiverDist = scale(RiverDist),     # Scale values: RiverDist
# scaleRoadDist = scale(RoadDist),       # Scale values: RoadDist
# scaleVillDist = scale(VillDist),       # Scale values: VillDist
# scaleRuggmean = scale(Ruggmean))       # Scale values: Ruggmean

# Elevation
modElev <- brm(occ ~ scaleElevmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)

conditional_effects(modElev)

# River distance
modRiver <- brm(occ ~ scaleRiverDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)

conditional_effects(modRiver)


# Road distance
modRoad <- brm(occ ~ scaleRoadDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)

conditional_effects(modRoad)


# Village distance
modVill <- brm(occ ~ scaleVillDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)

conditional_effects(modVill)


# Ruggedness
modRugg <- brm(occ ~ scaleRuggmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)

conditional_effects(modRugg)


### Add criterion??
m0 <- add_criterion(modNull, "loo")
ma <- add_criterion(modElev, "loo")
mb <- add_criterion(modRiver, "loo")
mc <- add_criterion(modRoad, "loo")
md <- add_criterion(modVill, "loo")
me <- add_criterion(modRugg, "loo")


loo_compare(m0, ma, mb, mc, md, me, criterion = "loo")


#compare the models with loo_compare

# repeat

