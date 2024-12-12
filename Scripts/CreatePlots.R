

# Libraries
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # for graphs

#Load our model the easy way
modElevRugg <- readRDS("Data/modElevRugg.rds")


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
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +           # Set the y-axis
    labs(x="Elevation (m asl)",                                                  # Label the x-axis
         y="Occurrence probability",                                         # Label the y-axis 
         title="Occurrence probability of Red eared monkey")                 # Add a title
)
#Lovely. Let's save that graph
ggsave(trendPlot, file="Plots/ER_ElevationTrend.png", width=8, height=6)




# Plot against ruggedness
plotData <- effects[[2]]
max(monkeyData$Ruggmean)

M  <- mean(monkeyData$Ruggmean)   # Mean of original elevation data
SD  <- sd(monkeyData$Ruggmean)    # SD of original elevation data

# Back transform the sclaed elevation values to natural values
plotData$Ruggedness <- M + (plotData$scaleRuggmean * SD)

# Plot the data
(trendPlot <- ggplot(plotData, aes(Ruggedness)) +                           # Create the plot
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "lightblue1") + # Add the confidence interval ribbon
    geom_line(aes(y = estimate__), colour="skyblue4", linewidth=1) +        # Add the trend line 
    scale_x_continuous(limits=c(0,50), breaks=seq(0,50,10)) +               # Set the x-axis
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +           # Set the y-axis
    labs(x="Ruggedness index",                                              # Label the x-axis
         y="Occurrence probability",                                        # Label the y-axis 
         title="Occurrence probability of Red eared monkey")                # Add a title
)
#Lovely. Let's save that graph
ggsave(trendPlot, file="Plots/ER_RuggednessTrend.png", width=8, height=6)

