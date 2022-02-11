
# Assign each point to a HORIZONTAL grid line

###### Instructions ########

# The input for this script is the file with the word "PreProc" that was produced in the previous step.

# 0. This script does batch processing of all files in the same folder. 
# 1. All files should be in the same folder. 
# 2. Each file should have a unique name. No other files should be in this folder. 


# The input file should have a column named x that contains the x-coordinates of a point.
# The input file should have a column named y that contains the y-coordinate of a point. 

# NOTE: 
#  1. This script assumes that the horizontal lines are spaced 5 units apart. Thus in Step 1, the 
#     for-loop rounds to 5.
#  2. This script defines the origin of a horizontal line as 10 pixels left (closer to 0) of the 
#     left-most x-coordinate in the data (Step 2). This is just a convention that objectively measures 
#     objects from the same starting point. 
#############################


#############################
# Required packages

#install.packages('stringr')
library(stringr)
#install.packages('dplyr')
library(dplyr)
#install.packages('plyr')
library(plyr)
#install.packages('tidyr')
library(tidyr)
#############################




# This gets the file path to the folder containing all the files to be analyzed by this script
path = getwd()

# This creates a vector of all files in the folder
contents = list.files(path)
contents



for (i in contents){

  ####################################
  #  Step 0 - Load the data
  
  df = read.csv(i)
  # head(df)
  # Prepare the files name for adding to the output file in Step 5
  filename = basename(i)
  stripped.name = stringr::str_remove(filename, ".csv")
  
  
  ####################################
  ####################################
  ####################################
  
  # Step 1 - Assign each row to an angle
  
  #  The y-coordinate of the first object in the data from bottom up will be the 
  #    first line of the horizontal grid system. This line is defined as line 0.
  
  # Find the bottom-most object as defined by the y coordinate
  # The bottom most object will become line 0, which is like "angle 0" of a radial grid system
  bottom.most = min(df$y)
  # bottom.most 
  
  # Create new column that contains the subtracts of bottom.most from every y-coordinate in y
  registered.angle = c()
  
  for (i in df$y){
    subtraction = i - bottom.most
    registered.angle = c(registered.angle, subtraction)  
  }
  
  registered.angle = as.data.frame(registered.angle) 
  
  angle = c()
  
  for (i in registered.angle){
    rnd = plyr::round_any(i, 5)
    angle = c(angle, rnd)
  }
  
  # angle
  
  
  df = cbind(df, angle)
  
  
  ####################################
  ####################################
  ####################################
  
  
  # Step 2 - Add x origin coordinate for each point/row
  
  #   For a HORIZONTAL grid system, origin of each line is on the LEFT side of the image.
  #     The x-coordinate of the origin is defined as 10 pixels left of the left-most coordinate of the object.  
  
  
  left.most = min(df$x); left.most
  leftMost.minus10 = left.most-10; leftMost.minus10
  
  df = mutate(df, Xo = rep(leftMost.minus10, times = dim(df)[1]))
  
  
  ####################################
  ####################################
  ####################################
  # Step 4 - Add y origin coordinates for each point/row 
  
  df = mutate(df, Yo = y)
  
  
  ####################################
  ####################################
  ####################################
  
  # Save the result as a csv file
  write.csv(df, paste0(stripped.name,"_angles_origins-added.csv"), row.names = F)
  
  # This erases all variables in the R environment
  rm(list = ls())
  
}


