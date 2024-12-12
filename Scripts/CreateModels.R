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
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # for modelling

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
# CI 0.27     0.79  - significant

conditional_effects(modElev)

# River distance
modRiver <- brm(occ ~ scaleRiverDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
#CI -0.11     0.39 - not significant

conditional_effects(modRiver)


# Road distance
modRoad <- brm(occ ~ scaleRoadDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# CI -0.09     0.41 - not significant

conditional_effects(modRoad)


# Village distance
modVill <- brm(occ ~ scaleVillDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# CI -0.05     0.49 - not significant

conditional_effects(modVill)


# Ruggedness
modRugg <- brm(occ ~ scaleRuggmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# CI -0.14     0.37 - not significant

conditional_effects(modRugg)


# Add criteria for comparison
m0 <- add_criterion(modNull, "loo")
ma <- add_criterion(modElev, "loo")
mb <- add_criterion(modRiver, "loo")
mc <- add_criterion(modRoad, "loo")
md <- add_criterion(modVill, "loo")
me <- add_criterion(modRugg, "loo")

# Compare the models with loo_comapre
loo_compare(m0, ma, mb, mc, md, me, criterion = "loo")

### ma (Elevation) is the best model
### md (Village distance is next best)


# Create models with elevation plus another covariate
# Ignore road distance, just use village distance.

# Elevation plus river distance
modElevRiver <- brm(occ ~ scaleElevmean + scaleRiverDist + offset(log(Effort)),
                data = monkeyData,
                family = bernoulli(link="logit"),
                warmup = 2000, iter = 4000, chains = 4)
#CI 
conditional_effects(modElevRiver)


# Elevation plus village distance
modElevVill <- brm(occ ~ scaleElevmean + scaleVillDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# CI 

conditional_effects(modElevVill)


# Elevation plus ruggedness
modElevRugg <- brm(occ ~ scaleElevmean + scaleRuggmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# CI 
conditional_effects(modElevRugg)

### This is a good model. Let's save it
saveRDS(modElevRugg, file="Data/modElevRugg.rds") 



# Add criteria for comparison
maa <- add_criterion(modElevRiver, "loo")
mab <- add_criterion(modElevVill, "loo")
mac <- add_criterion(modElevRugg, "loo")

# Compare the models with loo_comapre
loo_compare(m0, ma, maa, mab, mac, criterion = "loo")

# Elevation plus ruggedness is the better model


#-------------------------------------
# Elevation plus ruggedness plus river
modElevRuggRiver <- brm(occ ~ scaleRuggmean + scaleElevmean + scaleRiverDist + offset(log(Effort)),
                    data = monkeyData,
                    family = bernoulli(link="logit"),
                    warmup = 2000, iter = 4000, chains = 4)
#CI 
conditional_effects(modElevRuggRiver)


# Elevation plus village distance
modElevRuggVill <- brm(occ ~ scaleRuggmean + scaleElevmean + scaleVillDist + offset(log(Effort)),
                   data = monkeyData,
                   family = bernoulli(link="logit"),
                   warmup = 2000, iter = 4000, chains = 4)
# CI 
conditional_effects(modElevRuggVill)

# Add criteria for comparison
maca <- add_criterion(modElevRuggRiver, "loo")
macb <- add_criterion(modElevRuggVill, "loo")


loo_compare(m0, ma, mac, maca, macb, criterion = "loo")
# elpd_diff se_diff
# mac   0.0       0.0   - still best model
# maca -0.1       1.2   
# macb -0.8       0.8   
# ma   -1.8       2.3   
# m0   -8.8       4.4





#################################
# Extract conditional effects
effects <- conditional_effects(modElevRugg)

# create a plotting dataset
plotData <- effects[[1]]
summary(plotData)

max(monkeyData$Elevmean)

M  <- mean(monkeyData$Elevmean)   # Mean of original elevation data
SD  <- sd(monkeyData$Elevmean)    # SD of original elevation data

# Back transform the sclaed elevation values to natural values
plotData$Elevation <- M + (plotData$scaleElevmean * SD)

# Plot the data
(trendPlot <- ggplot(plotData, aes(Elevation)) +                             # Create the plot
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightcoral") +  # Add the confidence interval ribbon
    geom_line(aes(y = estimate__), colour="firebrick1", linewidth=1) +       # Add the trend line 
    scale_x_continuous(limits=c(0,1200), breaks=seq(0,1200,200)) +           # Set the x-axis
    labs(x="Elevation (m)",                                                  # Label the x-axis
         y="Occurrence probability",                                         # Label the y-axis 
         title="Occurrence probability of Red eared monkey")                 # Add a title
  )
#Lovely. Let's save that graph
ggsave(trendPlot, file="Plots/ElevRuggTrend.png", width=8, height=8)

