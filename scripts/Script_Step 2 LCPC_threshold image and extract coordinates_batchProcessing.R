
# By David H. Nguyen, PhD. BrainScanology, Inc.

# The input for this script should come from the stencil images produced from the previous step, 
#    which were produced by the script called "Script_automated LCPC_crop and create stencil with horizontal grid_batchProcessing"

###### Instructions ###### 
# 0. This script does batch processing of all files in the same folder. 
# 1. All files should be in the same folder. 
# 2. Each file should have a unique name. No other files should be in this folder. 
##########################


##########################
# Required packages
#library(dplyr)
#library(tidyr)
##########################


# This gets the file path to the folder containing all the files to be analyzed by this script
path = getwd()
print(path)

# This creates a vector of all files in the folder
contents = list.files(path)
print(contents)


################################################
################################################
# This for-loop does the work on each file in the designated file path


for (i in contents) {
    print(i)

    focus = load.image(i)
    # plot(focus)
    
    # This modifies the name of the loaded file for the purposes of naming the 
    #   image that is saved at the end of this script
    filename = basename(i)
    stripped.name = stringr::str_remove(filename, ".jpeg")
    
    
    #########################################################
    #########################################################
    
    # Step 1 - Threshold the Image
    
    # Turn image to grayscale
    gray.focus = grayscale(focus, method = "Luma", drop = TRUE)
    
    plot(gray.focus)
    
    # Remove noise from image background
    cleaned = threshold(gray.focus, thr = 0.5, approx = TRUE, adjust = 1)
    
    plot(cleaned)
    
    
    #########################################################
    #########################################################
    
    # Step 2 - Segment the Image
    
    
    # Highlight objects in red
    edges = highlight(cleaned, col = "red")
    
    
    # Save image as "_mask" 
    # dev.copy(png, paste0(stripped.name, "_mask.jpg"))
    # dev.off()

    #########################################################
    #########################################################
    
    # Step 3 - Extract Information of Objects in Image
    
    # Turn each sub-list in "edges" into a separate data frame
    
    counter = seq(1:length(edges)) 
    counter
    
    # Script to Automate
    # item_1 = as.data.frame(edges[1])
    # item_2 = as.data.frame(edges[2])
    # item_3 ...
    
    funky = function(n1){
      paste0("item_", n1, " = as.data.frame(edges[", n1, "])")
    }
    
    basket = lapply(counter, funky)
    # basket
    
    for (i in basket){
      eval(parse(text=i))
    }
    
    
    #########################################################
    #########################################################
    
    # Step 4a -  calculate average of x coordinates
    
    
    printFunk.x = function(n){
      paste0("x.item_", n, " = summarise(item_", n, ", mean(x))")
    }
    
    x.item_basket = lapply(counter, printFunk.x)
    x.item_basket
    
    for (i in x.item_basket){
      eval(parse(text=i))
    }
    
    
    # The below code isolates the last object that is highlighted red in the image.
    #   This will be important for Step 5b, which only selects items that are specific to this image
    #   and not objects left over from previous images run through this script. 
    
    bushy.tail = tail(x.item_basket) # isolate last 6 items in holder
    # bushy.tail
    string.tail = substr(bushy.tail,1,9) # extract the first 9 characters
    # string.tail
    final.tail = last(string.tail) # isolate the last item
    # final.tail
    
    
    ###################
    # Step 4b - calculate average of y coordinates
    
    y.item_1 = summarise(item_1, mean(y))
    y.item_1
    
    printFunk.y = function(n){
      paste0("y.item_", n, " = summarise(item_", n, ", mean(y))")
    }
    
    y.item_basket = lapply(counter, printFunk.y)
    # y.item_basket
    
    for (i in y.item_basket){
      eval(parse(text=i))
    }
    
    
    
    ###################
    # Step 4c - calculate length of item along x-axis
    
    length.along.x = summarise(item_1, max(x)) - summarise(item_1, min(x)) 
    
    findLength.x = function(n){
      paste0("length.along.x_", n, " = summarise(item_", n, ", max(x)) - summarise(item_", n, ", min(x))")
    }
    
    length.x.basket = lapply(counter, findLength.x)
    # length.x.basket
    
    for (i in length.x.basket){
      eval(parse(text=i))
    }
    
    
    
    ##########################
    
    #############
    # Step 5a - Identify all objects in R environment 
    
    show.all.objects = objects()
    # show.all.objects
    
    #############
    # Step 5b - Find all objects that have the pattern "x.item_" 
    named_x.item = grep("x.item_", show.all.objects, value = TRUE) # find objects that contain pattern "line."
    # named_x.item
    
    named_x.item = grep("\\d", named_x.item, value = TRUE) # find objects that have integers in their name
    named_x.item = as.data.frame(named_x.item) 
    # named_x.item
    
    sorted_x.item = mutate(named_x.item, new_col = named_x.item)
    # head(sorted_x.item)
    sorted_x.item = separate(sorted_x.item, new_col, c("name", "number.as.string"), sep = '_')
    to.numeric = as.numeric(sorted_x.item$number.as.string)
    sorted_x.item = cbind(sorted_x.item, to.numeric)
    sorted_x.item = arrange(sorted_x.item, to.numeric)
    sorted_x.item = sorted_x.item[-c(2:4)]
    # sorted_x.item
    
    
    cutoff.point = grep(final.tail, sorted_x.item[,1])
    # cutoff.point
    biggest = last(cutoff.point)
    # biggest
    
    correct.items = sorted_x.item[1:biggest,1] 
    # correct.items
    
    
    #################################33
    
    # Step 5c - bind each x-coordinate, y-coordinate, and length.along.x into a row
    
    funk.combiner = function (n){
      paste0("row_", n, " = cbind(x.item_", n, ", y.item_", n, ", length.along.x_", n, ")")
    }
    
    iter.me = seq(1:length(correct.items))
    iter.me
      
    result.combiner = sapply(iter.me, funk.combiner)
    # result.combiner
    
    eval(parse(text=result.combiner))
    
    
    ##########################################
    # Step 5d - bind each row from 5b into a data frame
    
    all.things = ls()
    named_row = grep("row_", all.things, value = TRUE) # find objects that contain pattern "row_"
    # named_row
    
    named_row = as.data.frame(named_row)
    
    # Sort the data frame by numerical order
    bowl = mutate(named_row, new_col = named_row)
    # bowl
    bowl = separate(bowl, new_col, c("name", "number.as.string"), sep = '_')
    to.numeric = as.numeric(bowl$number.as.string)
    bowl = cbind(bowl, to.numeric)
    bowl = arrange(bowl, to.numeric)
    bowl = bowl[c(1)]
    
    real.McCoy = bowl[1:biggest,1]
    # real.McCoy
    
    real.McCoy.df = as.data.frame(real.McCoy)
    # real.McCoy.df
    
    # rbind all objects called in real.McCoy
    real.McCoy.asChar = as.character(real.McCoy)
    # real.McCoy.asChar
    
    united = do.call("rbind", lapply(real.McCoy.asChar, get))
    # united 
          
    # Sort the data frame by numerical order
    nearFinal = cbind(real.McCoy.df, united)
    # nearFinal
    
    
    # Change column names
    colnames(nearFinal)[colnames(nearFinal) %in% c("real.McCoy", "mean(x)", "mean(y)", "max(x)")] = c("order", "x", "raw.y", "length")
    
    # Invert y coordinates so that origin of image (0,0) is at bottom left-hand corner
    max.y = dim(focus)[2]
    # max.y
    
    # Invert Y coordinate by subtracting each value from max.y
    ycoord.corrected = mutate(nearFinal, y = max.y-raw.y)
    
    
    right_order = c("order", "x", "y", "length", "raw.y")
    ycoord.corrected = ycoord.corrected[, right_order]
    
    # ycoord.corrected
    
    ##############################3
    
    # Save the data frame
    
    write.csv(ycoord.corrected, paste0(stripped.name, "_intersections.csv"), row.names = F)
    
    
    # clear all variables in the environment for clean slate for next sample
    rm(list=ls())

}

