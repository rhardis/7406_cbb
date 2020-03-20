###
# This file is for creating the baseline dataset
###

# Load in libraries
library(plyr)

# Read in the data as a dataframe of all years
data = read.csv("college-basketball-dataset/cbb.csv")

# Change NA in postseason column that represents was not in tournament to 0
facna <- addNA(data$POSTSEASON)
levels(facna) <- c(levels(data$POSTSEASON), "DNA")
data$POSTSEASON = facna

# Change Round of Exit to numeric number of wins
data$POSTSEASON = revalue(data$POSTSEASON, c("DNA"=0, "R68"=0, "R64"=0, "R32"=1, "S16"=2, "E8"=3, "F4"=4, "2ND"=5, "Champions"=6))

##################################################################
##################################################################

# Add in any extra data features here

##################################################################
##################################################################

# Save to csv for use in other files
write.csv(data, "cbb_full.csv", row.names = F)
