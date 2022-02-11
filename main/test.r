library(shiny)
library(shinythemes)
library(magick)
library(rsvg)
library("imager")
library(OpenImageR)
library(shinyFiles)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)

  ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(
      "BrainScanology",
      tabPanel("Script 1",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                           width = '100%',
                           accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_1", "Output Download Path", ""),
                 actionButton("analyzeButton", "Analyze"),
                 tags$head(tags$style("#download_text_1{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_1")
               ), 
               mainPanel(
                            h3("Instructions:"),
                            p("This is step 1 of the seven step process.\n
                               The ratio of input files to output files should be 1:3.\n
                               Use the practice files below to understand the correct input/output."),
                            
                            h3("Example Input:"),
                            actionButton("downloadPractice1", "Download Example Input File"),
                            #plotOutput("step1input1"),
                            #div(style = "margin-top: -150px"),
                            p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                            
                            h3("Example Output:"),
                            actionButton("downloadstep1output", "Download Example Output Files"),

                            #plotOutput("step1output1"),
                            #div(style = "margin-top: -150px"),
                            p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
                            
                            #plotOutput("step1output2"),
                            #div(style = "margin-top: -150px"),
                            p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized_crop.jpg"),
                            
                            #plotOutput("step1output3"),
                            #div(style = "margin-top: -150px"),
                            p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized_crop_stencil.jpg"),
                            
                        ) 
               
      ), 
      tabPanel("Script 2", 
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input2", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                           width = '100%',
                           accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_2", "Output Download Path", ""),
                 actionButton("analyzeButton2", "Analyze"),
                 tags$head(tags$style("#download_text_2{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_2")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 2 of the seven step process.\n
                               The ratio of input files to output files should be 1:1.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
               )
               ),
      tabPanel("Script 3",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input3", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                           width = '100%',
                           accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_3", "Output Download Path", ""),
                 actionButton("analyzeButton3", "Analyze"),
                 tags$head(tags$style("#download_text_3{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_3")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 3 of the seven step process.\n
                               The ratio of input files to output files should be 1:1.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
               )
               ),
      tabPanel("Script 4",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input4", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                 width = '100%',
                 accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_4", "Output Download Path", ""),
                 actionButton("analyzeButton4", "Analyze"),
                 tags$head(tags$style("#download_text_4{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_4")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 4 of the seven step process.\n
                               The ratio of input files to output files should be 1:1.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
               )
      ),
      tabPanel("Script 5",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input5", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                 width = '100%',
                 accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_5", "Output Download Path", ""),
                 actionButton("analyzeButton5", "Analyze"),
                 tags$head(tags$style("#download_text_5{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_5")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 5 of the seven step process.\n
                               The ratio of input files to output files should be 1:2.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
                 
                 #plotOutput("step1output2"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized_crop.jpg"),
               )
      ),
      tabPanel("Script 6",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input6", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                 width = '100%',
                 accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_6", "Output Download Path", ""),
                 actionButton("analyzeButton6", "Analyze"),
                 tags$head(tags$style("#download_text_6{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_6")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 6 of the seven step process.\n
                               The ratio of input files to output files should be 1:1.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
               )
      ),
      tabPanel("Script 7",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput(inputId = "image_input7", 
                           label = h5("Drag and drop files:"),
                           multiple = TRUE),
                 width = '100%',
                 accept=c("txt/csv", ".png", ".jpg"),
                 textInput("download_path_7", "Output Download Path", ""),
                 actionButton("analyzeButton7", "Analyze"),
                 tags$head(tags$style("#download_text_7{color: green;}")),
                 div(style = "margin-top: 15px"),
                 textOutput("download_text_7")
               ), 
               mainPanel(
                 h3("Instructions:"),
                 p("This is step 7 of the seven step process.\n
                               There should only be one output file.\n
                               Use the practice files below to understand the correct input/output."),
                 
                 h3("Example Input:"),
                 actionButton("downloadPractice1", "Download Example Input File"),
                 #plotOutput("step1input1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.jpg"),
                 
                 h3("Example Output:"),
                 actionButton("downloadstep1output", "Download Example Output Files"),
                 
                 #plotOutput("step1output1"),
                 #div(style = "margin-top: -150px"),
                 p("File name: 002_S_0816_age72 resized L rot mask rot90R.tif_resized.jpg"),
               )
      )
    ) 
  ) 
  
  server <- function(input, output, session) {
    
    output$step1input1 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images/step1input1.jpg'))
      
      # Return a list containing the filename
      list(src = filename,
           width=227,
           height=227)
    }, deleteFile = FALSE)
    
    output$step1output1 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images/step1output1.jpg'))
      
      # Return a list containing the filename
      list(src = filename,
           width=227,
           height=227)
    }, deleteFile = FALSE)
    
    output$step1output2 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images/step1output2.jpg'))
      
      # Return a list containing the filename
      list(src = filename,
           width=99,
           height=227)
    }, deleteFile = FALSE)
    
    output$step1output3 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images/step1output3.jpg'))
      
      # Return a list containing the filename
      list(src = filename,
           width=99,
           height=227)
    }, deleteFile = FALSE)
    
    observeEvent(input$analyzeButton, {
      for (row in 1:nrow(input$image_input)) {
        file_path <- input$image_input[row, "datapath"]
        output_file_path <- paste0(input$download_path_1, "/")
        im = image_read(file_path)
        
        filename = basename(input$image_input[row, "name"])
        stripped.name = stringr::str_remove(filename, ".jpeg")
        
        im.resized = image_scale(im, "600") 
        image_info(im.resized)
        
        print(paste0("test/step2/", stripped.name, "_resized.jpg"))
        im.renamed = image_write(im.resized, path = paste0("test/step2/", stripped.name, "_resized.jpg"), format = "jpg")
        im.4imager = load.image(im.renamed)
        
        eachLine.x = imsplit(im.4imager, "x") %>% plyr::laply(mean) 
        
        basket.4x = c()
        
        the.index = 1:length(eachLine.x)
        
        for (i in the.index) {
          if (eachLine.x[i] < 1){
            basket.4x = c(basket.4x, i)
          }
        }
        
        left.edge = basket.4x[1] # this is the left-most x-coordinate of the object
        left.edge.minus10 = left.edge-10 
        
        right.edge = data.table::last(basket.4x) # this is the right-most x-coordinate of the object
        right.edge.plus10 = right.edge+10
        
        eachLine.y = imsplit(im.4imager,"y") %>% plyr::laply(mean)
        
        basket.4y = c()
        
        the.index4y = 1:length(eachLine.y)
        
        for (i in the.index4y) {
          if (eachLine.y[i] < 1){
            basket.4y = c(basket.4y, i)
          }
        }
        
        top.edge = basket.4y[1] # this is the top-most y-coordinate of the object (origin is at upper left)
        top.edge.minus10 = top.edge-10
        
        bottom.edge = data.table::last(basket.4y) # this is the bottom-most y-coordinate of the object (origin is at upper left)
        bottom.edge.plus10 = bottom.edge+10
        
        width = right.edge.plus10 - left.edge.minus10
        
        height = bottom.edge.plus10 - top.edge.minus10
        
        distance.from.left = left.edge.minus10
        
        distance.from.top = top.edge.minus10
        
        im.crop = image_crop(im.resized, paste0(width, "x", height,"+", distance.from.left, "+", distance.from.top))
        
        im.saved.crop = image_write(im.crop, path = paste0("test/step2/", stripped.name, "_resized_crop.jpg"), format = "jpg")
        
        im.cropped = load.image(im.saved.crop)
        
        distance.bt.lines = 5
        
        height.im.cropped = dim(im.cropped)[2]
        
        width.im.cropped = dim(im.cropped)[1]
        
        num.of.horiz.lines = round(height.im.cropped/distance.bt.lines)
        
        vector.of.lines = seq(1, num.of.horiz.lines, by = 1)
        
        line.0 = im.cropped
        
        # The following are the vector inputs that will go into the mapply() function called holder
        
        # 1st ad 5th inputs for holder = mapply()
        
        # 2nd input for holder = mapply()
        b4.one.less = length(vector.of.lines)-1
        one.less = seq(0,b4.one.less)
        
        
        # 3rd input for holder = mapply()
        y0.input = seq(1, height.im.cropped, by = distance.bt.lines)
        
        if (length(y0.input > length(vector.of.lines))){
          y0.input = head(y0.input, -1)
        }
        
        # 4th Input holder = mapply()
        y1.input = y0.input+3
        
        ###
        
        # This function pastes command lines that will draw lines on the image when the command is evaluated. It is for the mapply function coming next
        line.funk = function (n1, n2, n3, n4, n5) {
          paste0("line.", n1, " = draw_rect(line.", n2, ", x0 = 0, y0 = ", n3, ", x1 = width.im.cropped, y1 = ", n4, ", color = 'white', opacity = 1, filled = TRUE) %>% plot(line.", n5, ")")
        }
        
        holder = mapply(line.funk, vector.of.lines, one.less, y0.input, y1.input, vector.of.lines)
        # head(holder)
        
        
        last6 = tail(holder) # isolate last 6 items in holder
        string.extract = sub("\\ .*", "", last6) # extract all chraracters up to and except the first space
        end.last6 = last(string.extract) # isolate the last item
        
        
        
        
        # This function evaluates and executes the printed commands stored in holder
        for (i in holder){
          eval(parse(text=i))
        }
        
        # plot(get(end.last6))
        
        #########################################################
        #########################################################
        
        # STEP 6 - Save the Stencil
        
        # Need to type "imager::save.image(" b/c R-base has its own save.image() function
        final_file_name = input$image_input[row, "name"] 
        execute.this = paste0("imager::save.image(", end.last6, ", paste0('test/step2/', final_file_name, '_resized_crop_stencil.jpg'))")
        # execute.this
        
        eval(parse(text=execute.this))
        
        
        # clear all variables in the environment for clean slate for next sample
        rm(list=ls())
        
        output$download_text_1 <- renderText({ "Download successful." })
        
    }})
    
    observeEvent(input$analyzeButton2, {
      
      # This gets the file path to the folder containing all the files to be analyzed by this script
      #setwd("/Users/dinesh/Desktop/brainscanology/test/step2")
      # This gets the file path to the folder containing all the files to be analyzed by this script
      #path = getwd()
      # This creates a vector of all files in the folder
      #contents = list.files(path)
      
      ################################################
      ################################################
      # This for-loop does the work on each file in the designated file path
      
      
      for (row in 1:nrow(input$image_input2)) {
        file_path <- input$image_input2[row, "datapath"]
        output_file_path <- paste0(input$download_path_2, "/")
        
        filename = basename(input$image_input2[row, "name"])
        stripped.name = stringr::str_remove(filename, ".jpeg")
        focus = load.image(file_path)
        
        
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
        #print(real.McCoy.asChar)
        
        united = do.call("rbind", lapply(real.McCoy.asChar, dynGet))
        
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
        
        write.csv(ycoord.corrected, paste0(output_file_path, stripped.name, "_intersections.csv"), row.names = F)
        
        
        # clear all variables in the environment for clean slate for next sample
        rm(list=ls())
        
        output$download_text_2 <- renderText({ "Download successful." })
        
      }
    })
    
    observeEvent(input$analyzeButton3, {
      for (row in 1:nrow(input$image_input3)) {
        file_path <- input$image_input3[row, "datapath"]
        output_file_path <- paste0(input$download_path_3, "/")
        
        filename = basename(input$image_input3[row, "name"])
        stripped.name = stringr::str_remove(filename, ".jpeg")
        
        df_copy = df
        df = arrange(df, length)
        
        ##########
        # Extract rows in which Feret is >=7
        
        greaterThan7 = filter(df, length>=7)
        View(greaterThan7)
        if(dim(greaterThan7)[1] == 0){
          print("Duplicating file")
          write.csv(df_copy, paste0(stripped.name, "_preProc.csv"), row.names = F)
          break 
        }
        
        numRows = seq(1:dim(greaterThan7)[1])
        
        
        #############
        
        
        container = list()
        
        for (item in numRows) {
          thing = paste0("rowStuff", item," = greaterThan7[",item, ",]")
          container = append(container, thing)
        }
        
        for (item in container){
          eval(parse(text=item))
        }
        
        ######## 
        # The following renames the first item in objects called rowStuffN (N=integer)
        
        basket = list()
        
        for (item in numRows){
          cup = paste0("rowStuff",item,"[1] = paste('duplic of', rowStuff",item,"[1])")
          basket = append(basket, cup)
        }
        
        for (item in basket){
          eval(parse(text=item))
        }
        
        
        #######
        # The following rowbinds each object called rowstuffN (N=integers)
        #   to the original data frame called df. Names the new data frame df.final
        
        
        df.final = df
        
        net = list()
        
        for (item in numRows){
          bowl = paste0("df.final = rbind(df.final,rowStuff",item,")")
          net = append(net, bowl)
        }
        
        for (item in net){
          eval(parse(text=item))
        }
        
        #View(df.final)
        
        ######
        # Check to see if the number of rows in df.final make sense.
        # Create a new csv file out of df.final
        
        correct_length = dim(df)[1] + dim(greaterThan7)[1]
        correct_length
        
        if (dim(df.final)[1] == correct_length){
          print("Congratulations! 'df.final' has the correct number of rows.")
          write.csv(df.final, paste0(output_file_path, stripped.name, "_preProc.csv"), row.names = F)
        } else {
          print(rep("ERROR", times = 200))
        }
      }
      output$download_text_3 <- renderText({ "Download successful." })
    })
    
    observeEvent(input$analyzeButton4, {
      for (row in 1:nrow(input$image_input4)) {
        file_path <- input$image_input4[row, "datapath"]
        output_file_path <- paste0(input$download_path_4, "/")
        
        filename = basename(input$image_input4[row, "name"])
        stripped.name = stringr::str_remove(filename, ".csv")
        
        df = read.csv(file_path)
        
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
        write.csv(df, paste0(output_file_path, stripped.name,"_angles_origins-added.csv"), row.names = F)
        
        # This erases all variables in the R environment
        rm(list = ls())
        
        output$download_text_4 <- renderText({ "Download successful." })
      }
    })
    
    observeEvent(input$analyzeButton5, {
      for (row in 1:nrow(input$image_input5)) {
        file_path <- input$image_input5[row, "datapath"]
        output_file_path <- paste0(input$download_path_5, "/")
        
        filename = basename(input$image_input5[row, "name"])
        stripped.name = stringr::str_remove(filename, ".csv")
        output$downloadText5 <- renderText(paste("\n\n", nrow(input$image_input5), " file(s) ready to download."))
        
        df = read.csv(file_path)

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
        
        write.csv(df, paste0(output_file_path, stripped.name, "_HORIZ-grid_distance-calculation.csv"), row.names = F)
        
        write.csv(final.df, paste0(output_file_path, stripped.name, "_HORIZONTAL-grid_LCPC-data.csv"), row.names = F)
        
        rm(list = ls())
        
        output$download_text_5 <- renderText({ "Download successful." })
      }
    })
    
    observeEvent(input$analyzeButton6, {
      for (row in 1:nrow(input$image_input6)) {
        file_path <- input$image_input6[row, "datapath"]
        output_file_path <- paste0(input$download_path_6, "/")
        
        filename = basename(input$image_input6[row, "name"])
        stripped.name = stringr::str_remove(filename, ".csv")
        output$downloadText6 <- renderText(paste("\n\n", nrow(input$image_input6), " file(s) ready to download."))
        
        df = read.csv(file_path)
        
        ###########################
        ###########################
        ###########################
        # Step 1a - Calculating the Sampling Frequency
        # The following calculates the sampling frequency, which is (number of rows/total time of sampling)
        
        
        # this extracts the number of rows
        num.of.rows = dim(df)[1]
        
        # This is sampling time. This is preset as 200 units of time based on a horizontal grid that 
        #    covers objects no taller than 200 pixels
        total.time = 200 
        
        # This is the sampling frequency
        sampling.frequency = num.of.rows/total.time
        
        # sampling.frequency
        
        sampFreq.numSamps = sampling.frequency/num.of.rows
        
        
        ###########################
        ###########################
        ###########################
        # Step 1b
        
        how.many.rows = 40 
        
        order.of.sampling = seq(0:(how.many.rows-1))
        # order.of.sampling
        
        calc.fs = function (i){
          result = i * sampFreq.numSamps
        }
        
        freq = lapply(order.of.sampling, calc.fs)
        # freq
        
        
        
        ###########################
        ###########################
        ###########################
        
        # Step 2 - Calculating the Magnitudes
        
        the.dist = df$summedDistance
        
        
        # This does the FFT on your data, which should be a vector 
        result = fft(the.dist, inverse = FALSE)
        
        
        num.of.samples = seq(1:length(result))
        
        # this extracts the numeric value of the real numbers
        realNums = list()
        extractor.real = for (i in num.of.samples){
          thing = Re(result)[i]
          realNums = append(realNums, thing)
        }
        # realNums
        
        
        # this extracts the numeric value of the complex numbers
        imagNums = list()
        extractor.real = for (i in num.of.samples){
          thing = Im(result)[i]
          imagNums = append(imagNums, thing)
        }
        # imagNums
        
        
        
        # This function calculates the magnitude of a complex number
        calc.mag = function (i, j) {
          mag = sqrt(i^2 + j^2)
        }
        
        
        # This function compiles the calculated magnitudes, from two sources, into a list
        magnitudes = mapply(calc.mag, realNums, imagNums)
        # magnitudes
        
        
        
        ###########################
        ###########################
        ###########################
        
        # Step 3 - Compile the Frequency Bins and Magnitudes
        
        
        final.data = cbind(freq, magnitudes)
        
        final.data.filtered = final.data[1:19,]
        
        # This multiples each magnitude by 2 to produce a single-sided spectrogram
        single.side.mags = c()
        for (i in final.data.filtered[,2]){
          result = 2 * i
          single.side.mags = c(single.side.mags, result)
        }
        # single.side.mags
        
        # Export the file as a .csv
        export.me = cbind(final.data.filtered[,1],single.side.mags)
        colnames(export.me)[colnames(export.me) == ""] = "freq"

        write.csv(export.me, paste0(output_file_path, filename, "_fft-data.csv"), row.names = F)
        
        
        rm(list = ls())
        
        output$download_text_6 <- renderText({ "Download successful." })
      }
    })
    
    observeEvent(input$analyzeButton7, {
      # This is a sequence of numbers that will be iterated over by the  below for-loop
      output_file_path <- paste0(input$download_path_7, "/")
      file_path <- input$image_input7[1, "datapath"]

      # This helps determine how many rows should be in the blank data frame that will collect the results
      open.first.file = read.csv(file_path)
      length.1st.file = dim(open.first.file)[1]
      
      # this blank data frame will collect the results
      basket = data.frame(open.first.file[,1])
      colnames(basket)[1] = "frequency"
      
      for (i in 1:nrow(input$image_input7)) {
        file_path <- input$image_input7[1, "datapath"]
        output$downloadText7 <- renderText(paste("\n\n1 file(s) ready to download."))
        
        df2 = read.csv(file_path)
        nameFile = input$image_input7[i, "name"]
        extracted = df2[,2]
        extracted = as.data.frame(extracted)
        colnames(extracted)[1] = nameFile
        basket = cbind(basket, extracted)
      }
      write.csv(basket, paste0(output_file_path, "compiled fft data.csv"), row.names = F)
      output$download_text_7 <- renderText({ "Download successful." })
    })

  }
