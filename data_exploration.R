###
# This file is for data exploration and plotting functions
###

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