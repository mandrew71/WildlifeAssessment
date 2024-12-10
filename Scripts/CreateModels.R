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

if(!require(corrr)){install.packages("corrr"); library(corrr)} # for correlations

# Get the data
monkeyData <- readRDS("Data/monkeyData.rds")

# Scale the covariates
   ### Already done in GetData.R

# Examine correlations and don't combine if over 0.7
cor(dplyr::select(monkeyData,Effort:Ruggmean), method='s', use='complete.obs')
  # Road distance and village distance are highly correlated

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

