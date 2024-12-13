#investigating multispecies groups
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
  ungroup()       

# view the data
speciesSummary

#----------------------------------
# select the species
ourSpecies <- "Red-eared monkey"
#----------------------------------

!!!!!!
  # Get an occurrence dataset
  occSummary <- all_species %>%           # create a new object to hold the summary
  filter(!is.na(Group.ID)) %>%           # Only our multispecies groups
  #group_by(Site) %>%                    # group by grid cell
  summarise(occ = 1) %>%                # Set occurrence = 1 for all rows
  ungroup()                             # ungroup

cleaned_specieslist <- all_species %>% filter(!is.na(Group.ID))

print(cleaned_specieslist)


# trim the site dataset to only multispecies groups
multispecies<- cleaned_specieslist %>%
  select(Group.ID, Species, Observed, Estimated)
  )
#filter for multispiecies groups that inclucde red eared monkeys

multiRedeared <- multispecies %>%
  filter(Group.ID %in% Group.ID[Species == "Red-eared monkey"])

print(multiRedeared)


# Calculate Number of other Species per Group ID
other_species_count <- multiRedeared %>%
  group_by(Group.ID) %>%
  summarize(OtherSpeciesCount = sum(Species != "Red-eared monkey")) 




# plot Histogramm, but this one is with absolute group numbers  
ggplot(other_species_count, aes(x = OtherSpeciesCount)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Frequency of species combinations with Red-eared Monkey",
    x = "Number of other species",
    y = "Frequency "
  ) +
  theme_minimal()

#plotted in percentage to compair with reference data

ggplot(other_species_count, aes(x = OtherSpeciesCount)) +
  geom_histogram(
    binwidth = 1,
    fill = "violet",
    color = "black",
    alpha = 0.7,
    aes(y = ..count../sum(..count..)),
    width = 0.7
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(
    breaks = seq(0, max(other_species_count$OtherSpeciesCount)),
    labels = function(x) paste0("+", x, ifelse(x == 1, " sp", " spp"))
  ) +
  labs(
    title = "Frequency of Species Combinations with Red-eared Monkey",
    x = "Number of Other Species",
    y = "Percentage"
  ) +
  theme_minimal()


#find top three associated species 

associated_species <- multiRedeared %>%
  filter(Species != "Red-eared monkey") %>%  # remove red eared monkey
  count(Species, sort = TRUE)            # counting frequency of associated monkeys

# Top 3 assocaiated species
top_3_species <- head(associated_species, 3)

print(top_3_species)
#1. Putty-nosed monkey
#2. Crowned monkey
#3. Mona monkey 





















