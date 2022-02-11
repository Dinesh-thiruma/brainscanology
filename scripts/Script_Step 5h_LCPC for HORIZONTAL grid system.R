# Script for LCPC Algorithm - HORIZONTAL Grid
# (Calculating Distances Between Points and Summing All Distances That 
# Are on the Same HORIZONTAL Line)

# By David H. Nguyen, PhD www.TSG-Lab.org


# Start of ReadMe Info

######### HOW TO FORMAT THE DATA FILE FOR THIS SCRIPT ###########
# The input for this script is the file with the words "origins-added" that was produced from the previous step.
#
# 1. The column containing x-coordinates must be titled "x"
# 2. The column containing y-coordinates must be titled "y"
# 3. The column containing the measure of the angle must be titled "angle"
# 4. The column containing the x-coordinate of the origin must be titled "Xo"
# 5. The column containing the y-coordinate of the origin must be titled "Yo"
# 6. There should be no missing items in the columns x, y, and angle.
#
# NOTE: This script assumes that the horizontal grid lines are spaced 5 pixels apart. 
#      Modify all.possible.angles in Appendix A if you use a different nspacing.
#
# This script will produce two files.
#   1. The first file is called "_HORIZONTAL-grid_LCPC-data.csv". This is the file you will use
#      for the FFT script.
#   2. The second file is called "_horiz-grid_distance-calculation.csv" and is the result 
#      the calculations done by this script, EXCEPT for adding all 
#      distances of the same angle together.
#
#   
#
#
# End of ReadMe Info

#####


####################
# Required packages

#install.packages('dplyr')
#library(dplyr)
# #install.packages('tidyr')
#library(tidyr)
####################





# This gets the file path to the folder containing all the files to be analyzed by this script
path = getwd()

# This creates a vector of all files in the folder
contents = list.files(path)
contents


################################################
################################################
# This for-loop does the work on each file in the designated file path


for (i in contents) {
  
  df = read.csv(i)
  
  # This modifies the name of the loaded file for the purposes of naming the 
  #   image that is saved at the end of this script
  filename = basename(i)
  stripped.name = stringr::str_remove(filename, ".csv")
  
  
  ###### Doing the distance formula for two points
  
  # Calculate distance formula for each coordinate pair in the columns 'x' and 'y' 
  #    to the first row of columns 'Xo' and 'Yo' 
  #    This is the (x-Xo)^2 term
  
  df = mutate(df, x.part = x - Xo)
  df = mutate(df, x.partSq = x.part * x.part)
  
  df = mutate(df, y.part = y - Yo)
  df = mutate(df, y.partSq = y.part * y.part)
  
  # Sum each row of columns called x.partSq and y.partSq
  df = mutate(df, sum.of.sq = x.partSq + y.partSq)
  
  # This function takes the square root of its input
  take.sqrt = function (n){
    sqrt(n)
  }
  
  # This takes the square root of each row in column df$sum.of.sq
  sqrt.of.sum = sapply(df$sum.of.sq, take.sqrt)
  
  # Add sqrt.of.sum as new column to df
  df = cbind(df, sqrt.of.sum)
  
  
  
  #########
  #########
  # Isolate each distance in sqrt.of.sum according to it's value in the column 'angle'
  
  # Identify all unique values in df$angle
  uniq.angles = unique(df$angle)
  
  
  write.commands = function (n){
    paste0("group", n, "= filter(df, angle==", n,")")
  }
  
  execute.me = lapply(uniq.angles, write.commands)
  
  final.run = for (i in execute.me){
    eval(parse(text=i))
  }
  
  
  part1 = rep("group", times = length(uniq.angles))
  part2 = uniq.angles
  uniq.names = paste0(part1,part2)
  
  
  ### The following lines sum all distances in each "groupXX" data frame
  
  summing = function(n){
    paste0(n, " = summarise(", n, ", sum(sqrt.of.sum))")
  }
  # Example output: "group135 = summarise(group135, sum(sqrt.of.sum))" 
  
  # "container" contains the commands that need to be executed
  container = sapply(uniq.names, summing)
  
  for (i in container){
    eval(parse(text=i))
  }
  
  ###
  
  
  ##########
  # create function that rbinds all groups into one data frame
  
  basket = c()
  
  for (i in uniq.names){
    basket = append(basket, i)
  }
  
  basket = as.data.frame(basket) # turn header into a data frame
  basket = t(basket) # transpose this data frame into one horizontal row
  basket = as.data.frame(basket) # make basket into a data frame again b/c t() turned it into a matrix
  
  
  # merge all items in basket into one cell, separating each item by a comma
  combined = unite(basket, "groups", c(colnames(basket)), sep = ",")
  
  header = "bind.them = rbind("
  tailer = ")"
  
  text2execute = paste0(header, combined, tailer)
  
  bound.stuff = eval(parse(text = text2execute))
  
  named.bound = cbind(uniq.angles, bound.stuff) # this adds the name of the angles to bound.stuff
  names(named.bound) = c("angle", "summedDistance") # rename the column headers
  #View(named.bound)
  
  
  
  #########################
  
  # Appendix A
  
  
  largest.angle = max(df$angle) # find the largest angle 
  
  # This object is for adding  in angle groups that are not in bound.stuff.  
  all.possible.angles = seq(0, largest.angle, by = 5)
  
  diff.angles = is.element(all.possible.angles, uniq.angles)
  # View(diff.angles)
  
  absent.angles.marked = cbind(diff.angles, all.possible.angles)
  absent.angles.marked = as.data.frame(absent.angles.marked)
  
  isolated.absents = filter(absent.angles.marked, grepl(0, diff.angles))
  #View(isolated.absents)
  
  isolated.absents = isolated.absents[c(2,1)] # rearrange the order of columns
  names(isolated.absents) = c("angle", "summedDistance") 
  
  final.df = rbind(isolated.absents, named.bound) # combine
  
  final.df = arrange(final.df, angle)
  
  #View(final.df)
  
  
  
  ##################################
  # Saving the data as files
  
  write.csv(df, paste0(stripped.name, "_HORIZ-grid_distance-calculation.csv"), row.names = F)
  
  write.csv(final.df, paste0(stripped.name, "_HORIZONTAL-grid_LCPC-data.csv"), row.names = F)
  
  
  
  rm(list = ls())

}




