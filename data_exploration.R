###
# This file is for data exploration and plotting functions
###
library(ggplot2)
library(corrplot)
library(dplyr)

#load the full file
cbb_full <- read.csv('cbb_full.csv')

cbb_full$YEAR <- as.factor(cbb_full$YEAR)

unique(cbb_full$TEAM) #353 unique teams
unique(cbb_full$CONF) #33 unique conferences
dim(cbb_full) #1757 observations, 27 columns

#correlation matrix of quantitative variables
corrplot::corrplot(cor(cbb_full[,c(5:21,25:27)]), type="upper")

#Wins Above Bubble (WAB) strong positive relationship with ADJOE and BARTHAG. 
#strong negative relationship with ADJDE (less points allowed, more wins above bubble).

#Function to plot boxplots. Required inputs: the x and y axis you choose to plot.
plot_boxplot = function(x_axis, y_axis){
  q <- ggplot(data = cbb_full) + geom_boxplot(mapping=aes(x=x_axis, y = y_axis)) + coord_flip()
  return(q)
}

plot_boxplot(cbb_full$CONF, cbb_full$BARTHAG)

#Stats for teams with 0 postseason wins

stats_by_postseason_wins <- function(num_wins){
  a <- cbb_full %>%
    filter(POSTSEASON == num_wins) %>%
    group_by(TEAM) %>%
    summarise_if(is.numeric, mean)
  
  return(a)
}

zero_win_teams <- stats_by_postseason_wins(0)
one_win_teams <- stats_by_postseason_wins(1)
two_win_teams <- stats_by_postseason_wins(2)
three_win_teams <- stats_by_postseason_wins(3)
four_win_teams <- stats_by_postseason_wins(4)
five_win_teams <- stats_by_postseason_wins(5)
six_win_teams <- stats_by_postseason_wins(6)



# Dummy function to remove outliers.  For now this function just returns
#   whatever dataframe was provided as an argument.  Someone will need
#   to fill this out later.
remove_outliers = function(df){
  return(df)
}

# Function to normalize columns of numeric features
normalize_numerics = function(df, feature_cols="all"){
  df$YEAR = as.factor(df$YEAR)    # protects year column from scaling
  if(feature_cols=="all"){
    # Find all numerica columns and normalize them
    df[,-c(1,2,22,23,24)] = scale(df[,-c(1,2,22,23,24)]) # does not attempt to scale factor columns
  }
  else{
    for(col in feature_cols){
      df[col] = scale[col]
    }
  }
  return(df)
}