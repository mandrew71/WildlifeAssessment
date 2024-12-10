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
  filter(Species == ourSpecies) %>%   # Only our monkey
  dplyr::select(Site,                 # Grab the columns we need
                TotalEffor,
                Area_2,
                Month,
                Date,
                Observed,
                Estimated,
                Position,
                Long_E,
                Lat_N,
                Alt.m.)



