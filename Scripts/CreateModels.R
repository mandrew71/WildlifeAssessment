# Create the models for analysis
# 10 December 2024


# Libraries
if(!require(brms)){install.packages("brms"); library(brms)} # for modelling
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # for graphs

# Get the data
monkeyData <- readRDS("Data/monkeyData.rds")

# Examine correlations and don't combine if over 0.7
cor(dplyr::select(monkeyData,Effort:Ruggmean), method='s', use='complete.obs')
    #   # Road distance and village distance are highly correlated
    # Effort  Elevmean  RiverDist   RoadDist   VillDist    Ruggmean
    # Effort     1.00000000 0.1267722 0.03422987 0.37338213 0.31167998 -0.01223895
    # Elevmean   0.12677216 1.0000000 0.14994518 0.46210619 0.30916823  0.69200916
    # RiverDist  0.03422987 0.1499452 1.00000000 0.04547842 0.04292708  0.18220390
    # RoadDist   0.37338213 0.4621062 0.04547842 1.00000000 0.78592755  0.33160146
    # VillDist   0.31167998 0.3091682 0.04292708 0.78592755 1.00000000  0.17107447
    # Ruggmean  -0.01223895 0.6920092 0.18220390 0.33160146 0.17107447  1.00000000


###-------------------------
### Create the models
###------------------------

# Null value
modNull <- brm(occ ~ 1 + offset(log(Effort)),
            data = monkeyData,
            family = bernoulli(link="logit"),
            warmup = 2000, iter = 4000, chains = 4)
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept    -9.35      0.13    -9.60    -9.11 1.00     2704     3852


# Elevation
modElev <- brm(occ ~ scaleElevmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # CI 0.27     0.79  - significant
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.45      0.13    -9.71    -9.19 1.00     5913     5071
    # scaleElevmean     0.53      0.13     0.28     0.79 1.00     6095     5202

conditional_effects(modElev)

# River distance
modRiver <- brm(occ ~ scaleRiverDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    #CI -0.11     0.39 - not significant
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept         -9.36      0.13    -9.61    -9.11 1.00     7141     5606
    # scaleRiverDist     0.14      0.13    -0.11     0.39 1.00     7722     5228
conditional_effects(modRiver)


# Road distance
modRoad <- brm(occ ~ scaleRoadDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # CI -0.09     0.41 - not significant
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.39      0.13    -9.65    -9.15 1.00     5503     5329
    # scaleRoadDist     0.16      0.13    -0.09     0.41 1.00     5279     4749
conditional_effects(modRoad)


# Village distance
modVill <- brm(occ ~ scaleVillDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # CI -0.05     0.49 - not significant
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.40      0.13    -9.66    -9.14 1.00     4894     5353
    # scaleVillDist     0.22      0.14    -0.05     0.48 1.00     4862     5058
conditional_effects(modVill)


# Ruggedness
modRugg <- brm(occ ~ scaleRuggmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # CI -0.14     0.37 - not significant
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.36      0.13    -9.61    -9.11 1.00     7089     5145
    # scaleRuggmean     0.12      0.13    -0.14     0.37 1.00     7024     5419
conditional_effects(modRugg)


# Add criteria for comparison
m0  <- add_criterion(modNull, "loo")
mEl <- add_criterion(modElev, "loo")
mRi <- add_criterion(modRiver, "loo")
mRo <- add_criterion(modRoad, "loo")
mVi <- add_criterion(modVill, "loo")
mRu <- add_criterion(modRugg, "loo")

# Compare the models with loo_compare
loo_compare(m0, mEl, mRi, mRo, mVi, mRu, criterion = "loo")
    # elpd_diff se_diff
    # mEl  0.0       0.0   
    # mVi -7.0       4.2   
    # m0  -7.2       4.0   
    # mRo -7.3       3.9   
    # mRi -7.6       4.1   
    # mRu -7.8       3.5 

### mEl (Elevation) is the best model
### mVi (Village distance is next best)


# Create models with elevation plus another covariate
# Ignore road distance, just use village distance.

# Elevation plus river distance
modElevRiver <- brm(occ ~ scaleElevmean + scaleRiverDist + offset(log(Effort)),
                data = monkeyData,
                family = bernoulli(link="logit"),
                warmup = 2000, iter = 4000, chains = 4)
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept         -9.46      0.13    -9.73    -9.20 1.00     6326     5542
    # scaleElevmean      0.52      0.13     0.26     0.78 1.00     6486     6121
    # scaleRiverDist     0.10      0.13    -0.16     0.36 1.00     7120     5905
conditional_effects(modElevRiver)


# Elevation plus village distance
modElevVill <- brm(occ ~ scaleElevmean + scaleVillDist + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # # CI 
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.47      0.14    -9.74    -9.20 1.00     6342     5880
    # scaleElevmean     0.51      0.14     0.24     0.78 1.00     6070     4812
    # scaleVillDist     0.10      0.14    -0.18     0.38 1.00     6994     6045 
conditional_effects(modElevVill)


# Elevation plus ruggedness
modElevRugg <- brm(occ ~ scaleElevmean + scaleRuggmean + offset(log(Effort)),
               data = monkeyData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
    # # CI 
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.48      0.14    -9.76    -9.21 1.00     4581     4812
    # scaleElevmean     0.79      0.18     0.46     1.14 1.00     4087     4882
    # scaleRuggmean    -0.44      0.19    -0.82    -0.08 1.00     4318     4761
    # conditional_effects(modElevRugg)

### This is a good model. Let's save it
saveRDS(modElevRugg, file="Data/modElevRugg.rds") 


# Add criteria for comparison
mElRi<- add_criterion(modElevRiver, "loo")
mElVi <- add_criterion(modElevVill, "loo")
mElRu<- add_criterion(modElevRugg, "loo")

# Compare the models with loo_compare
loo_compare(m0, mEl, mElRi, mElVi, mElRu, criterion = "loo")
    # elpd_diff se_diff
    # mElRu  0.0       0.0   
    # mEl   -1.7       2.3   
    # mElRi -2.4       2.6   
    # mElVi -2.6       2.5   
    # m0    -8.9       4.4   
    ### Elevation plus ruggedness is the better model


#-------------------------------------
# Elevation plus ruggedness plus river
modElevRuggRiver <- brm(occ ~ scaleElevmean + scaleRuggmean + scaleRiverDist + offset(log(Effort)),
                    data = monkeyData,
                    family = bernoulli(link="logit"),
                    warmup = 2000, iter = 4000, chains = 4)
    # #CI 
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept         -9.50      0.14    -9.77    -9.24 1.00     7195     5607
    # scaleElevmean      0.81      0.18     0.46     1.16 1.00     5618     5134
    # scaleRuggmean     -0.47      0.19    -0.85    -0.11 1.00     5796     6169
    # scaleRiverDist     0.16      0.13    -0.10     0.42 1.00     7308     5638
conditional_effects(modElevRuggRiver)


# Elevation plus village distance
modElevRuggVill <- brm(occ ~ scaleElevmean + scaleRuggmean + scaleVillDist + offset(log(Effort)),
                   data = monkeyData,
                   family = bernoulli(link="logit"),
                   warmup = 2000, iter = 4000, chains = 4)
    # # CI 
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept        -9.51      0.14    -9.79    -9.24 1.00     6726     5334
    # scaleElevmean     0.78      0.18     0.43     1.13 1.00     5546     5445
    # scaleRuggmean    -0.45      0.19    -0.83    -0.07 1.00     5721     5285
    # scaleVillDist     0.11      0.15    -0.17     0.40 1.00     6459     5190
conditional_effects(modElevRuggVill)

# Add criteria for comparison
mElRuRi <- add_criterion(modElevRuggRiver, "loo")
mElRuVi <- add_criterion(modElevRuggVill, "loo")

# Compare the models with loo_compare
loo_compare(m0, mEl, mElRu, mElRuRi, mElRuVi, criterion = "loo")
    # elpd_diff se_diff
    # mElRu    0.0       0.0   - still best model
    # mElRuRi -0.1       1.2   
    # mElRuVi -0.8       0.8   
    # mEl     -1.8       2.3   
    # m0      -8.8       4.4


### save the models. Give Kate Moss a sandwich
save(modNull,
     modElev,
     modRiver,
     modRoad,
     modVill,
     modRugg,
     modElevRiver,
     modElevVill,
     modElevRugg,
     modElevRuggRiver,
     modElevRuggVill,
     file="Data/monkeyModels.rdata") 



### Add in area as an interaction to see differences between the different forests
modElevRugg_Area <- brm(occ ~ Area*(scaleElevmean + scaleRuggmean) + offset(log(Effort)),
                      data = monkeyData,
                      family = bernoulli(link="logit"),
                      warmup = 2000, iter = 4000, chains = 4)
    # Regression Coefficients:
    #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    # Intercept                    -9.52      0.19    -9.90    -9.17 1.00     7673     6258
    # AreaMakombe                  -9.34      4.33   -18.55    -1.55 1.00     3986     4094
    # AreaNdokbou                   0.54      0.47    -0.38     1.45 1.00     5894     5284
    # scaleElevmean                 0.91      0.26     0.40     1.43 1.00     4444     4966
    # scaleRuggmean                -0.47      0.23    -0.94    -0.02 1.00     4593     5584
    # AreaMakombe:scaleElevmean    -7.08      3.69   -14.70    -0.01 1.00     4067     4062
    # AreaNdokbou:scaleElevmean    -0.67      0.48    -1.62     0.27 1.00     4191     4922
    # AreaMakombe:scaleRuggmean    -1.54      1.11    -3.81     0.56 1.00     6431     5352
    # AreaNdokbou:scaleRuggmean     0.23      0.49    -0.73     1.19 1.00     5647     5425
conditional_effects(modElevRugg_Area) ### Cool plots!

### Interesting! Save the model
saveRDS(modElevRugg_Area, file="Data/modElevRugg_Area.rds") 

# Add criteria for comparison
mArea <- add_criterion(modElevRugg_Area, "loo")

# Compare the models with loo_compare
loo_compare(m0, mEl, mElRu, mElRuRi, mElRuVi, mArea, criterion = "loo")
    # elpd_diff se_diff
    # mElRu    0.0       0.0   
    # mElRuRi -0.2       1.2   
    # mElRuVi -0.9       0.8   
    # mEl     -1.8       2.3   
    # mArea   -2.1       3.4   
    # m0      -8.9       4.4 
### Area doesn't add much to the models but the graphs show the different condtioins in the areas
