# Script to Calculate FFT on data outputted by LCPC algorithm.
# David H. Nguyen, PhD, www.TSG-Lab.org


#######
# Start of Readme Info

###### How to Format the Data #######

# The input file for this script should have the "LCPC-data" in its name. 
#    This file was produced in the previous step.

# 1. The input file should have a column called "summedDistance", which is
#    the column that contains the lcpc distances. 
# 2. You need to type in the sampling time in Step 1. The default is 200 units of time for a HORIZONTAL 
#    grid system on objects whose vertical height is within 200 pixels.  
#    If you used a 360 degree RADIAL grid, then the sampling time should be 360. 
# 3. This script was designed for a table/vector of 40 rows (see "how.many.rows" in Step 1b). 
#    The 40 is from the sampling of 0 to 200 degrees/pixels by 5 degrees/pixels (200/5 = 40).
#    You should change the value of "how.many.rows" if your input data has less than or more than 40 rows.
#    
# 4. There should be no missing items in the "summedDistance" column.
# 5. There should only be numbers in the summedDistance" column.
# 6. The script produces a .csv file called "fft_data.csv".
#
# End of ReadMe Info

#######







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
  
  write.csv(export.me, paste0(filename, "_fft-data.csv"), row.names = F)
  
  
  rm(list = ls())
  
  writeLines("NOTE: \n \n You can ignore the warning  messages that say: \n 'In cbind(freq, magnitudes) :
  number of rows of result is not a multiple of vector length (arg 2)' \n ")
  

}

