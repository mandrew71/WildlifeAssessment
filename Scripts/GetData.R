# Create the core dataset


# Libraries
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} # for data manipulation

# Grab the data
all_species <- read.csv("Data/YKBA_All_species_data.csv")

str(all_species) # look at the data


speciesSummary <- all_species %>%          # create a new object to see the summaries
  group_by(Species) %>%                    # group by species
  summarise(spCount = length(Month)) %>%   # get the count of rows for weach species
  ungroup()                                # ungroup

# view the data
speciesSummary

# select the species
ourSpecies <- "Red-eared monkey"


# Trim the dataframe
monkeyData <- all_species %>%         # Clean dataframe for our monkey
  filter(Species == ourSpecies,       # Only our monkey
         !(is.na(Observed))) %>%      # No empty observation cells
  dplyr::select(Site,                 # Grid site
                Species,              # Species
                Effort = TotalEffor,  # Total effort
                Area = Area_2,        # Area (e.g. Ebo forest)
                Month,                # Month observed
                Date,                 # Date observed
                Observed,             # Number observed
                Estimated,            # Estimated number of individuals 
                Position,             # Height in canopy
                Long_E,               # Longitude
                Lat_N,                # Latitude
                Altitude = Alt.m.)    # Altitude


# Get the site data
all_sites <- read.csv("Data/YKBA_Site_characteristics.csv")

# Let's have a look
head(all_sites)
str(all_sites)


# trim the site data
siteData <- all_sites %>%
  select(id,
         Elevmean,
         RiverDist,
         RoadDist,
         VillDist,
         Ruggmean
         )

# add the site values onto our monkey dataset
monkeyData <- monkeyData %>%
  left_join(siteData, by=c("Site"="id"))


#Save the data file
saveRDS(monkeyData, file="Data/monkeyData.rds") 

  



