#################################################################################################################################
# Part I
# There are 20 files with .dat extention. You have to read all the files in to single dataframe.

setwd("Target directory")                   # ("C:/Users/-----/iris")  set the directory for iris folder(which contain .dat files)
file_list <- list.files()                   # getting the list of files in the working directory(in iris folder)


var_names <- c("SepalLength","SepalWidth", "PetalLength", "PetalWidth", "Class")                 
var_names                                   # Creating the variabe names(column Header)


  # forming a loop function to append all .dat files...
   single_df <- data.frame()                   # creating a empty data frame 
   
   for(file in file_list) {
       temp_single_df <- read.delim(file ,  sep = "," ,stringsAsFactors = FALSE,header = FALSE ,skip = 9, col.names =var_names)
       single_df <-rbind(single_df, temp_single_df) 
       rm(temp_single_df)
    }

  # replacing null values with NA's
  single_df[single_df=="<null>"] <- NA              
  
  # replacing NA values with COLUMN MEAN value
  for(i in 1:ncol(single_df)) {                    
  single_df[is.na(single_df[,i]) ,  i] <- round(mean(as.numeric(single_df[ , i]), na.rm = TRUE), 1)
  }
  
  nrow(single_df)
  head(single_df)
  tail(single_df)
  class(single_df)


#################################################################################################################################
# Part II
# The data is present in xml format, with file name, iris.xml. 
# Your task is to read the XML data and store it in the data frame df.

# Ans.....
install.packages("XML")
library("XML")

 setwd("target directory")     # set the directory for iris.xml file location

 xml_data <- xmlToDataFrame("iris.xml",stringsAsFactors = F,collectNames = TRUE)
 df <- xml_data
 df
 
#################################################################################################################################
# Part III
# Convert the iris data into the JSON format and read the data in JSON format 
# and convert it into dataframe "iris_data".

# Ans..
 
 # Install and call the following libraries
 install.packages("RJSONIO")     
 library(RJSONIO)
 
 install.packages("rjson")
 library(rjson)
 
 install.packages(jsonlite)
 library(jsonlite)
 
 # as we get only 3 rows of data out of iris.xml file
 # reading the inbuilt iris data 
 irisdata <- iris
 
 # reading irisdata into json format
 data_json <- toJSON(irisdata)
 data_json
 class(data_json)
 
 # exporting json format data into a external file 
 write_json(data_json,"iris_json.json")

 # reading json formatted external file....
 ext_data <- read_json("iris_json.json",simplifyVector = TRUE)
 
 # reading json format data into dataframe format 
 iris_data <- fromJSON(ext_data)
 iris_data
 
 class(iris_data)
 class(iris_data$Sepal.Length)
 class(iris_data$Petal.Length)
 class(iris_data$Species)
 
 
#################################################################################################################################
# Part IV
# Use dplyr function on the data iris_data. Implement select, match, filter, 
# arrange, rename, and mutate function on the iris_data.
 
 library(dplyr)
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
 
 # (1) FILTER
 # filter() the data for species virginica
 virginica <- filter(iris_data, Species == "virginica")
 head(virginica) # This dispalys the first six rows
 
 # filter the dataset where Species = versicolor and > 5.8
 sepalLength6 <- filter(iris_data, Species == "versicolor", Sepal.Length > 5.8)
 tail(sepalLength6) # compare this to head()s
 
 # (2) SELECT
 # select() the specified columns
 data1 <- select(iris_data, Sepal.Length, Sepal.Width, Petal.Length)
 
 # select all columns from sepal.Length to Petal.Length
 data2 <- select(iris_data, Sepal.Length:Petal.Length)
 head(data1, 3)
 
 # selected and selected2 are exactly the same
 identical(data1, data2)
 
 
 # (3) MUTATE
 # create a new column that stores logical values for Sepal.Width greater than half of Sepal.Length
 newCol <- mutate(iris_data, greater.half = Sepal.Width > 0.5 * Sepal.Length)
 tail(newCol)
 
 
 # (4) ARRANGE
 # arrange()
 newCol <- arrange(newCol, Petal.Width)
 head(newCol)
 
 
 # (5) MATCH
 # match the  Sepal.Length and Petal.Length of iris_data 
 match(iris_data$Sepal.Length,iris_data$Petal.Length)
 

 # (6) RENAME
 ?rename
 # Rename the columns of iris dataset to Sep.L Sep.W Pet.L Pet.W
 iris_renamed <- rename(iris_data,"Sep.L"="Sepal.Length","Sep.W"="Sepal.Width",
        "Pet.L"="Petal.Length","Pet.W"="Petal.Width")
 iris_renamed
 
 
 # (7) The chain operator, or the pipeline %>%
 # This will first filter, and then arrange our data. 
 # Note that here the order in which you call functions does not matter, but in other cases it might
 arr.virginica <- newCol %>% filter(Species == "virginica") %>% 
   arrange(Sepal.Length)
 arr.virginica
 
#################################################################################################################################
# Part V
 # Print the summary of iris_data
  str(iris_data)
  
 # type convert the "Species" column into Factor to get better summary results
 iris_data$Species <- as.factor(iris_data$Species)
 
  str(iris_data)
  summary(iris_data)
 
 # print the summary of all variables...
  summary(iris_data$Sepal.Length)
  summary(iris_data$Sepal.Width)
  summary(iris_data$Petal.Length)
  summary(iris_data$Petal.Width)
   
 # example for for summary of linear model..
  library(VIF)
  require(biglm)
  library(psych)
 
  model1 <- lm(Sepal.Length ~ Species, data = iris_data)
  summary(model1)
  
  
 # PLOT
 # ploting the iris dataset with the help of plot & ggplot
 install.packages("ggplot2") 
 library("ggplot2")
 iris_data
 
 plot(iris_data)
 plot(iris_data$Sepal.Length,iris_data$Sepal.Width)

 ggplot(iris_data, aes(y = Sepal.Length, x = Petal.Length, colour = Species)) + 
   geom_point() +
   ggtitle('Iris Species by Sepal and Petal Length')
 
 ?ggplot

################################################################################################################################# 
 
 