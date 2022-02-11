


# First, set the working directory to the folder that contains the files

path = getwd()

# This creates a vector of all files in the folder
contents = list.files(path)
contents



# This is a sequence of numbers that will be iterated over by the  below for-loop
counter = seq(1:length(contents))
counter


# This helps determine how many rows should be in the blank data frame that will collect the results
open.first.file = read.csv(contents[1])
length.1st.file = dim(open.first.file)[1]
length.1st.file



# this blank data frame will collect the results
basket = data.frame(open.first.file[,1])
colnames(basket)[1] = "frequency"
basket

# This opens each file, copies the second column, changes the name of the column, and then binds that column
# to a storage data frame called basket
for (i in counter){
  df2 = read.csv(contents[i])
  nameFile = contents[i]
  extracted = df2[,2]
  extracted = as.data.frame(extracted)
  colnames(extracted)[1] = nameFile
  basket = cbind(basket, extracted)
}

# basket

# Save the results as a csv file

write.csv(basket, "compiled fft data.csv", row.names = F)

