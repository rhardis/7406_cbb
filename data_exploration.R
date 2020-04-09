###
# This file is for data exploration and plotting functions
###
library(ggplot2)
library(corrplot)
library(dplyr)

#load the full file
cbb_full <- read.csv('cbb_full.csv')

cbb_full$YEAR <- as.numeric(cbb_full$YEAR)

unique(cbb_full$TEAM) #353 unique teams
unique(cbb_full$CONF) #33 unique conferences
dim(cbb_full) #1757 observations, 27 columns

#filter to exclude teams that don't make the round of 64
cbb_tourney <- cbb_full %>%
  filter(!is.na(SEED)) #leaves you with 340 observations

#offensive/defensive percentage stats
percentage_stats_plots <- function(){
  par(mfrow=c(1,2))
  boxplot(cbb_tourney[c(8,10,12,14,16,18)], las=2, main = "Offensive Stats", col="orange")
  boxplot(cbb_tourney[c(9,11,13,15,17,19)], las=2, main = "Defensive Stats", col="orange")
}


adjusted_wins_plots <- function(){
  par(mfrow=c(1,3))
  #adjusted offensive/defensive efficiency/tempo
  boxplot(cbb_tourney[c(5:6, 20)], las=2, main="Adjusted Efficiency and Tempo", col="orange")
  #BARTHAG/Wins above Bubble
  boxplot(cbb_tourney[7], las=2, main="BARTHAG", col="orange")
  boxplot(cbb_tourney[21], las=2, main="Wins Above Bubble", col="orange")
  
}

games_wins_plots <- function(){
  par(mfrow=c(1,2))
  #Games/Wins/Coach's Previous Tourney Wins
  boxplot(cbb_tourney[c(3:4, 25,26)], las=2, main="Games, Wins, 
        Coach's Tourney Wins", col="orange")
  #Coach's Previous Regular Season Wins, 
  boxplot(cbb_tourney[27], las=2, main="Coach's Previous Regular 
        Season Wins", col="orange")
}



#No need to plot year, seed, and postseason for outliers

corr_plot <- function(){
  #correlation matrix of quantitative variables
  corrplot::corrplot(cor(cbb_tourney[,c(5:21,25:27)]), type="upper")
  
  
}


# Dummy function to remove outliers.  For now this function just returns
#   whatever dataframe was provided as an argument.  Someone will need
#   to fill this out later.
remove_outliers = function(df){
  return(df)
}

# Function to normalize columns of numeric features
normalize_numerics = function(df, feature_cols="all"){
  #df$YEAR = as.factor(df$YEAR)    # protects year column from scaling
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