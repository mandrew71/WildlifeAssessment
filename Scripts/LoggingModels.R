#investigating logging activities 
# Create the core dataset


# Libraries
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} # for data manipulation


# Grab the data
all_species <- read.csv("Data/YKBA_All_species_data.csv")

# view the data
str(all_species)

# Check species observation counts
speciesSummary <- all_species %>%          # create a new object to see the summaries
  group_by(Species) %>%                    # group by species
  summarise(spCount = length(Month)) %>%   # get the count of rows for each species
  ungroup()                                # ungroup

# view the data
speciesSummary

#----------------------------------
# select the species
ourSpecies <- "Human"
#----------------------------------

!!!!!!
# Get an occurrence dataset
occSummary <- all_species %>%           # create a new object to hold the summary
  filter(Species == ourSpecies,
         Sign %in% c("Logging site", "Logging road", "Logging transect", "Planks")) %>%     # Only our hunters activities
  group_by(Site) %>%                    # group by grid cell
  summarise(occ = 1) %>%                # Set occurrence = 1 for all rows
  ungroup()                             # ungroup




#Get the site data
all_sites <- read.csv("Data/YKBA_Site_characteristics.csv")

# Let's have a look
head(all_sites)
str(all_sites)
# trim the site data
siteData <- all_sites %>%
  select(Site = id,
         Area,
         Effort = TotalEffor,
         Elevmean,
         RiverDist,
         RoadDist,
         VillDist,
         Ruggmean
  )


# Join the two datasets
LoggingData <- siteData %>%
  left_join(occSummary, by=c("Site"="Site"))

LoggingData <- LoggingData %>%
  mutate(occ = if_else(is.na(occ),0,1),    # all grid cells show presence/absence
         scaleEffort = scale(Effort),           # Scale values: Effort
         scaleElevmean = scale(Elevmean),       # Scale values: Elevmean
         scaleRiverDist = scale(RiverDist),     # Scale values: RiverDist
         scaleRoadDist = scale(RoadDist),       # Scale values: RoadDist
         scaleVillDist = scale(VillDist),       # Scale values: VillDist
         scaleRuggmean = scale(Ruggmean))       # Scale values: Ruggmean

# Check the data structure
str(LoggingData)

saveRDS(LoggingData, file="Data/LoggingData.rds") 



##if(!require(corrr)){install.packages("corrr"); library(corrr)} # for correlations
if(!require(brms)){install.packages("brms"); library(brms)} # for modelling
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # for modelling

#get the data
LoggingData <- readRDS("Data/LoggingData.rds")

# Null value
modNull <- brm(occ ~ 1 + offset(log(Effort)),
               data = LoggingData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)



# Elevation
modElev <- brm(occ ~ scaleElevmean + offset(log(Effort)),
               data = LoggingData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
conditional_effects(modElev)


# Ruggedness
modRugg <- brm(occ ~ scaleRuggmean + offset(log(Effort)),
               data = LoggingData,
               family = bernoulli(link="logit"),
               warmup = 2000, iter = 4000, chains = 4)
# Conditional effects test

conditional_effects(modRugg)


# Elevation plus ruggedness
modElevRugg <- brm(occ ~ scaleElevmean + scaleRuggmean + offset(log(Effort)),
                   data = LoggingData,
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

#plot effects

# Extract conditional effects
effects <- conditional_effects(modElev)

plotData <- effects[[1]]
summary(plotData)

max(LoggingData$Elevmean)

M  <- mean(LoggingData$Elevmean)   # Mean of original elevation data
SD  <- sd(LoggingData$Elevmean)    # SD of original elevation data

# Back transform the sclaed elevation values to natural values
plotData$Elevmean <- M + (plotData$scaleElevmean * SD)

# Plot the data
(trendPlot <- ggplot(plotData, aes(Elevmean)) +                             # Create the plot
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightcoral") +  # Add the confidence interval ribbon
    geom_line(aes(y = estimate__), colour="firebrick1", linewidth=1) +       # Add the trend line 
    scale_x_continuous(limits=c(0,1200), breaks=seq(0,1200,200)) +          # Set the x-axis
    #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +           # Set the x-axis
    labs(x="Elevation (m a.s.l.)",                                                  # Label the x-axis
         y="Occurrence probability",                                         # Label the y-axis 
         title="Occurrence probability of Logging activities")) 
#Lovely. Let's save that graph
ggsave(trendPlot, file="Plots/ElevTrendLogging.png", width=8, height=8)














