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


### Add criterion??
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




### Add criterion??
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


##################### TO HERE

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

M  <- mean(monkeyData$Elevmean)   # Mean of original elevation data
SD  <- sd(monkeyData$Elevmean)    # SD of original elevation data

# Back transform the sclaed elevation values to natural values
plotData$Elevation <- M + (plotData$scaleElevmean * SD)


(trendPlot <- ggplot(plotData, aes(Elevation)) +
 geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "slategrey") +
 geom_line(aes(y = estimate__), colour="gray0", linewidth=1) +
    labs(y="Occurrence probability", title="Occurrence probability of Red eared monkey")

  )

# #Plot with the original values on the x axis
# 
# h <- ggplot(dt, aes(Rugge)) +
#   geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey70") +
#   geom_line(aes(y = estimate__), colour="gray0", linewidth=1)+
#   theme_bw()+
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank())+
#   labs(x = "Terrain ruggednex index", y = "Occurrence probability")+
#   theme(axis.text.y = element_text(size = 18, colour="black"), 
#         axis.text.x = element_text(size = 18, colour="black"), 
#         axis.title = element_text(face = "plain", size = 18), 
#         plot.title = element_text(face = "bold", size = 18)) +
#   scale_y_continuous(limits= c(0.0,0.11,0.030), breaks = seq(0.0,0.11,0.030)) +
#   #scale_x_continuous(limits= c(0,1100,300), breaks = seq(0,1100,300)) +
#   facet_wrap(~"Putty-nosed monkey")+
#   theme(strip.background =element_rect(fill="grey50"))+
#   theme(strip.text = element_text(colour = 'white'))+
#   theme(strip.text.x = element_text(size = 18, face="bold"))
# 
# h
# 
# ggsave(h, file="ElephRugg.png", width = 5, height = 4, dpi=600)




