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

#get the data
monkeyData <- readRDS("Data/monkeyData.rds")

#scale the covariates
monkeyData[12 : 16] <- as.data.frame(scale(monkeyData[12 : 16]))
    
    # Elevation
    ##### Altitude can be removed as using mean variables for grid
    # dist vill
    # dist road
    # river dist
    # ruggedness
    # canopy
    # year?date? -  leave for now

#examine correlations
# and remove if over 0.7
cor(monkeyData[12:16], method='s', use='complete.obs')
  # Road distance and vilalge distance are highly correlated


# Add new boolean dat




### Create the models
### - single FE for all covars
# Null value
mod0 <- brm(monkeyData ~  1+offset(log(Effort))+(1|Site),
            data = monkeyData,
            family = bernoulli(link="logit"),
            warmup = 1000,iter = 2000, chains = 4,control = list(adapt_delta = 0.99, 
                                                                 max_treedepth = 16), cores = 20)
# Elevation

### Add criterion??

#compare the models with loo_compare

# repeat

