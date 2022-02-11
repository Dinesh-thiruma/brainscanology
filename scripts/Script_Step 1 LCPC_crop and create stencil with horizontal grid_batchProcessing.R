# David H. Nguyen, PhD. Tissue Spatial Geometrics Lab (www.TSG-Lab.org)


########### ReadMe ############

###### What does this script do? ######
# 1. Resize the image so the width is 600 pixels and the height is constrained to correlate 
#       with the width (we do this by convention; no significance).
# 2. Crop the shape outline within the image such that there is a 10-pixel margin 
#       on all four sides of the shape outline.
# 3a. Create a stencil of the shape outline in the form of horizontal grid lines. 
#       This stencil is a crude approximation of locations where the grid lines 
#       intersect with the shape's edge. Why? Because calculating the mid-point 
#       of each intersection gives us the x & y coordinates of each intersection.
# 3b. Info on Horizontal Grid Lines: The first line starts at the top of the cropped image 
#       and then the rest are spaced 5 pixels along the height of the image. 
#       The default spacing is 5 pixels, but you can change this in Step 5b. 


# Note: For each image inputted into this script, it will produce 3 output files.
#    Example: 
#       If the input file is named: "test ROI.jpg"
#       1st output file will be called: "test ROI_resized.jpg"
#       2nd output file will be called: "test ROI_resized_crop.jpg"
#       3rd output file will be called: "test ROI_resized_crop_stencil.jpg"


###### Instructions ###### 
# 0. This script does batch processing of all files in the same folder. All files should be in the same folder. 
#    Each file should have a unique name. No other files should be in this folder. 
# 1. *******The input file must consist of a single-color line object that is a dark color 
#    (like an empty blue circle) on a COMPLETELY white canvas. **********
# 2. Each file must have a unique name. 
# 3. Specify how much space you want between each line of the horizontal grid system (See Step 5b; object called distance.bt.lines). 
#       The default is 5 pixels between each line.
# 4. You must specific the file path in Step 6c to say where you want the final 
#       output to be saved on your computer
#   

# End of ReadMe

################################################
################################################


# Required Packages

# install.packages("magick")
library(magick)

# install.packages("rsvg")
library(rsvg)

# install.packages("imager")
library(imager)

# install.packages("OpenImageR")
library(OpenImageR)

# install.packages("plyr")
# no need to call this library b/c I use "plyr::" shortcut

# install.packages("data.table")
library(data.table)

# install.packages("dplyr")
library(dplyr)

# install.packages("tidyr")
library(tidyr)




##########################3

# This gets the file path to the folder containing all the files to be analyzed by this script
path = getwd()

# This creates a vector of all files in the folder
contents = list.files(path)
contents


################################################
################################################
# This for-loop does the work on each file in the designated file path



for (i in contents){
  
    ################################################
    ################################################
    
    # STEP 1 - Load the image
  
  
    # image_read() is a function in magick
    im = image_read(i)
    # plot(im)
    

    # This modifies the name of the loaded file for the purposes of naming the 
    #   image that is saved at the end of this script
    filename = basename(i)
    stripped.name = stringr::str_remove(filename, ".jpeg")

    
    ################################################
    ################################################
    
    # STEP 2 - Resize the image to a width of 600 and height of ~900 [due to constrained ratio] (this is by convention from when we first started doing this)
    
    # resize image so that 
    im.resized = image_scale(im, "600") # resize proportionally to width: 200px
    image_info(im.resized)
    # plot(im.resized)
    
    # save the image as a jpg file
    im.renamed = image_write(im.resized, path = paste0(stripped.name, "_resized.jpg"), format = "jpg")
    
    # im.renamed
    
    ################################################
    ################################################
    
    # STEP 3 - Crop the image
    
    
    # Step 3a - Load the file from Step 2 for imager package
    
    im.4imager = load.image(im.renamed)
    
    # plot(im.4imager) 
    
    
    #############
    
    # Step 3b - Find the two X boundaries (left, right) of the object
    
    # imsplit() separates pixels into vertical columns. 
    #    Then laply(mean) calculates the average value in eacn column
    eachLine.x = imsplit(im.4imager, "x") %>% plyr::laply(mean)  
    # eachLine.x
    
    # Find the first and last column along x-axis that has a pixel of the object
    
    basket.4x = c()
    
    the.index = 1:length(eachLine.x)
    
    for (i in the.index) {
      if (eachLine.x[i] < 1){
        basket.4x = c(basket.4x, i)
      }
    }
    
    left.edge = basket.4x[1] # this is the left-most x-coordinate of the object
    left.edge.minus10 = left.edge-10 
    left.edge.minus10
    
    right.edge = data.table::last(basket.4x) # this is the right-most x-coordinate of the object
    right.edge.plus10 = right.edge+10
    right.edge.plus10
    
    
    #############
    
    # Step 3c - Find the two y boundaries (top, bottom) of the ojbect
    
    
    # imsplit() separates pixels into vertical columns. Then laply(mean) calculates the average value in eacn column
    eachLine.y = imsplit(im.4imager,"y") %>% plyr::laply(mean)  
    # View(eachLine.y)
    
    # Find the first and last column along x-axis that has a pixel of the object
    
    basket.4y = c()
    
    the.index4y = 1:length(eachLine.y)
    
    for (i in the.index4y) {
      if (eachLine.y[i] < 1){
        basket.4y = c(basket.4y, i)
      }
    }
    
    top.edge = basket.4y[1] # this is the top-most y-coordinate of the object (origin is at upper left)
    top.edge.minus10 = top.edge-10
    top.edge.minus10
    
    bottom.edge = data.table::last(basket.4y) # this is the bottom-most y-coordinate of the object (origin is at upper left)
    bottom.edge.plus10 = bottom.edge+10
    bottom.edge.plus10
    
    
    
    ################################################
    ################################################
    
    # STEP 4 - Crop the image
    
    
    ##########
    # Step 4a - Find width of desired crop area
    
    width = right.edge.plus10 - left.edge.minus10
    width
    
    height = bottom.edge.plus10 - top.edge.minus10
    height
    
    
    ##########
    # Step 4b - Find distance from left edge of image to left.edge.minus10
    
    distance.from.left = left.edge.minus10
    
    # Step 4c - Find the distance from the top edge of the image to top.edge.minus10
    
    distance.from.top = top.edge.minus10
    
    ##########
    # Step 4d - Crop the image based on steps 4a and 4b
    
    # image_crop() is a function in magick. 
    #Ex: im.crop = image_crop(im, "100x150+50+50") # crop out width:100px and height:150px starting +50px from the left, and +50px from top
    
    im.crop = image_crop(im.resized, paste0(width, "x", height,"+", distance.from.left, "+", distance.from.top))
    
    # plot(im.crop)
    # image_info(im.crop)
    
    
    im.saved.crop = image_write(im.crop, path = paste0(stripped.name, "_resized_crop.jpg"), format = "jpg")
    
    
    #########################################################
    #########################################################
    
    # STEP 5 - Overlay thick white lines to create a thin stencil of the image
    
    # Step 5a
    # load image from Step 4d
    im.cropped = load.image(im.saved.crop)
    # plot(im.cropped)
    
    # Step 5b
    # Define the pixel distance between the lines your grid system of horizontal lines
    distance.bt.lines = 5
    
    height.im.cropped = dim(im.cropped)[2] # this is the height of your cropped image
    height.im.cropped
    
    width.im.cropped = dim(im.cropped)[1] # this is the width of your cropped image
    width.im.cropped
    
    # Calculate how many lines will be in your horizontal grid system based on distance.bt.lines
    num.of.horiz.lines = round(height.im.cropped/distance.bt.lines)
    num.of.horiz.lines
    
    vector.of.lines = seq(1, num.of.horiz.lines, by = 1)
    vector.of.lines
    
    line.0 = im.cropped #rename the cropped image as line.0 to make the automation easier
    # plot(line.0)
    
    
    # Sample Code to Automate
    #     n1                  n2               n3                              n4                                                            n5
    # line.1 = draw_rect(line.0,  x0 = 0, y0 = 1 , x1 = width.im.cropped, y1 = 4,  color = "white", opacity = 1, filled = TRUE) %>% plot(line.1)
    # line.2 = draw_rect(line.1,  x0 = 0, y0 = 6,  x1 = width.im.cropped, y1 = 9,  color = "white", opacity = 1, filled = TRUE) %>% plot(line.2)
    # line.3 = draw_rect(line.2,  x0 = 0, y0 = 11, x1 = width.im.cropped, y1 = 14, color = "white", opacity = 1, filled = TRUE) %>% plot(line.3)
    # line.4 = draw_rect(line.3,  x0 = 0, y0 = 16, x1 = width.im.cropped, y1 = 19, color = "white", opacity = 1, filled = TRUE) %>% plot(line.4)
    # line.5 = draw_rect(line.4,  x0 = 0, y0 = 21, x1 = width.im.cropped, y1 = 24, color = "white", opacity = 1, filled = TRUE) %>% plot(line.5)
    # line.6 = draw_rect(line.5,  x0 = 0, y0 = 26, x1 = width.im.cropped, y1 = 29, color = "white", opacity = 1, filled = TRUE) %>% plot(line.6)
    # line.7 = draw_rect(line.6,  x0 = 0, y0 = 31, x1 = width.im.cropped, y1 = 34, color = "white", opacity = 1, filled = TRUE) %>% plot(line.7)
    
    
    #### 
    # The following are the vector inputs that will go into the mapply() function called holder
    
    # 1st ad 5th inputs for holder = mapply()
    vector.of.lines
    
    # 2nd input for holder = mapply()
    b4.one.less = length(vector.of.lines)-1
    b4.one.less
    one.less = seq(0,b4.one.less)
    one.less
    
    
    # 3rd input for holder = mapply()
    y0.input = seq(1, height.im.cropped, by = distance.bt.lines)
    y0.input
    
    if (length(y0.input > length(vector.of.lines))){
      y0.input = head(y0.input, -1)
    }
    
    # 4th Input holder = mapply()
    y1.input = y0.input+3
    y1.input
    
    ###
    
    # This function pastes command lines that will draw lines on the image when the command is evaluated. It is for the mapply function coming next
    line.funk = function (n1, n2, n3, n4, n5) {
      print(paste0("line.", n1, " = draw_rect(line.", n2, ", x0 = 0, y0 = ", n3, ", x1 = width.im.cropped, y1 = ", n4, ", color = 'white', opacity = 1, filled = TRUE) %>% plot(line.", n5, ")"))
    }
    
    holder = mapply(line.funk, vector.of.lines, one.less, y0.input, y1.input, vector.of.lines)
    # head(holder)
    
    
    last6 = tail(holder) # isolate last 6 items in holder
    last6
    string.extract = sub("\\ .*", "", last6) # extract all chraracters up to and except the first space
    string.extract
    end.last6 = last(string.extract) # isolate the last item
    end.last6
    
    
    
    
    # This function evaluates and executes the printed commands stored in holder
    for (i in holder){
      eval(parse(text=i))
    }
    
    # plot(get(end.last6))
    
    #########################################################
    #########################################################
    
    # STEP 6 - Save the Stencil
    
    # Need to type "imager::save.image(" b/c R-base has its own save.image() function
    execute.this = paste0("imager::save.image(", end.last6, ", paste0('/Users/davidnguyen/Documents/', stripped.name, '_resized_crop_stencil.jpg'))")
    # execute.this
    
    eval(parse(text=execute.this))
    
    
    # clear all variables in the environment for clean slate for next sample
    rm(list=ls())
    
    writeLines("NOTE: \n \n This script will produce warnings messages that say \n 'In mapply(line.funk, vector.of.lines, one.less, y0.input,  ... :
  longer argument not a multiple of length of shorter'  \n  You can ignore them because the issue does not effect the result.")

}

