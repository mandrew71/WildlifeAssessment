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
ourSpecies <- "Red-eared monkey"
#----------------------------------


# Get an occurrence dataset
occSummary <- all_species %>%           # create a new object to hold the summary
  filter(Species == ourSpecies) %>%     # Only our monkey
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
monkeyData <- siteData %>%
  left_join(occSummary, by=c("Site"="Site"))

# Prepare values for analysis
monkeyData <- monkeyData %>%
  mutate(occ = if_else(is.na(occ),0,1),    # all grid cells show presence/absence
         scaleEffort = scale(Effort),           # Scale values: Effort
         scaleElevmean = scale(Elevmean),       # Scale values: Elevmean
         scaleRiverDist = scale(RiverDist),     # Scale values: RiverDist
         scaleRoadDist = scale(RoadDist),       # Scale values: RoadDist
         scaleVillDist = scale(VillDist),       # Scale values: VillDist
         scaleRuggmean = scale(Ruggmean))       # Scale values: Ruggmean

# Check the data structure
str(monkeyData)


#Save the data file
saveRDS(monkeyData, file="Data/monkeyData.rds") 

  



