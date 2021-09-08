library(dplyr)
library(tidyr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(lubridate)
library(viridis)
library(Plotly)
RStudio.Version()


# CAPSTONE PROJECT: FOUNDATIONS OF DATA SCIENCE 2019
# TITLE: "Atmospheric Nitrogen Deposition in Rural Areas of Western Region of USA"
getwd() # getting working directory

Field_Gas_Data <- read.table("/Users/Biljana/Field_Gas_Densities.csv", header = TRUE, sep = ",", quote = "\"'", dec = ".",
                     fill = TRUE, comment.char = "", na.strings =  "?") # reads a file in table format and creates data frame 
#from it named “Field_Gas_Data”
View(Field_Gas_Data) # check the data is loaded correctly 
dim(Field_Gas_Data) # check dimensions of the data frame
length(Field_Gas_Data) # number of columns/length in a data frame
ncol(Field_Gas_Data) # check number of columns
nrow(Field_Gas_Data) # check number of rows

# Type conversion

Field_Gas_Data$SITE_ID <- as.factor(Field_Gas_Data$SITE_ID) # encoding SITE_ID as a factor
levels(Field_Gas_Data$SITE_ID) # display levels of the factor
Field_Gas_Data$SITE_ID <- factor((Field_Gas_Data$SITE_ID), levels = c("CAN407", "CON186", "DIN431", "JOT403", "LAV410", "PIN414" ,"SEK430", "YOS404")) 
#assigning as a factor and display levels
Field_Gas_Data$SITE_ID <- as.numeric(Field_Gas_Data$SITE_ID) # convert ordinal to numerical
SITE_ID <- Field_Gas_Data$SITE_ID # assign value to a name
head(SITE_ID) # first 6 row display

Field_Gas_Data$YEAR <- as.integer(Field_Gas_Data$YEAR) # encoding YEAR as integer
Field_Gas_Data$YEAR <- as.factor((Field_Gas_Data$YEAR)) # coercion from nominal to ordinal variable
levels(Field_Gas_Data$YEAR) # checking levels

Field_Gas_Data$WEEK <- as.integer(Field_Gas_Data$WEEK) # encoding WEEK as integer
Field_Gas_Data$WEEK <- as.factor((Field_Gas_Data$WEEK)) # coercion from nominal to ordinal variable
WEEK <- Field_Gas_Data$WEEK # assigning value to a name
levels(Field_Gas_Data$WEEK) # checking levels


Field_Gas_Data$SO2_CONC <- as.numeric(Field_Gas_Data$SO2_CONC) # encoding SO2_CONC as numeric
SO2_CONC <- Field_Gas_Data$SO2_CONC # assigning value to a name
Field_Gas_Data$SO4_CONC <- as.numeric(Field_Gas_Data$SO4_CONC) # encoding SO4_CONC as numeric
SO4_CONC <- Field_Gas_Data$SO4_CONC # assigning value to a name
Field_Gas_Data$NO3_CONC <- as.numeric(Field_Gas_Data$NO3_CONC) #encoding NO3_CONC as numeric
NO3_CONC <- Field_Gas_Data$NO3_CONC # assigning value to a name
Field_Gas_Data$HNO3_CONC <- as.numeric(Field_Gas_Data$HNO3_CONC) # encoding HNO3_CONC as numeric
HNO3_CONC <- Field_Gas_Data$HNO3_CONC # assigning value to a name

# Re-casting the “DATEON” variable to the “POSIXct data type 
Field_Gas_Data$DATEON <- as.POSIXct(Field_Gas_Data$DATEON, format = "%m/%d/%Y") # re-casting as POSIxt data type
DATEON <- Field_Gas_Data$DATEON # assigning value to aname
class(DATEON) # checking the data type
Field_Gas_Data$DATEON <- format(as.POSIXct(Field_Gas_Data$DATEON,
                                            format = "%m/%d/%Y %H:%M:S"), "%Y-%m-%d") # reformating from "%m/%d/%Y %H:%M:S" to "%Y-%m-%d"
Field_Gas <- Field_Gas_Data %>% mutate(DATEON = strftime(Field_Gas_Data$DATEON, "%m")) %>% rename(MONTH = DATEON) # re-formatting  DATEON with strftime() from 
#lubridate package nto "%m" format and renaming it MONTH

Field_Gas$MONTH <- as.factor(Field_Gas$MONTH) # coercion into 
  #factor with 12 levels 
MONTH <- Field_Gas$MONTH # assigning value to a name
 sapply(Field_Gas, class) # checking the data types of variables
head(MONTH)
#Data Cleaning: structure, check for missing values and for outliers

str(Field_Gas) # data structure compact display
sapply(Field_Gas, class) # high-order function returns vector as an output and it is a version of lapply()
summary(Field_Gas) # summary statistics of "Field_Gas_Data"
head(Field_Gas) # first 15 rows display
typeof(Field_Gas) # determines what type of object is "Field_Gas_Data"
class(Field_Gas) # check class of the object "Field_Gas_Data"
#Checking for missing values

any(is.na(Field_Gas)) # checking for missing values in a data table
any(is.na(Field_Gas$NO3_CONC)) # checking for missing values in a variable
any(is.na(Field_Gas$HNO3_CONC))# checking for missing values in a variable
any(is.na(Field_Gas$NH4_CONC))# checking for missing values in a variable
any(is.na(Field_Gas$SO2_CONC))# checking for missing values in a variable
any(is.na(Field_Gas$SO4_CONC)) #checking for missing values in a variable

Rows <- unique(unlist(lapply(Field_Gas, function(Field_Gas) which(is.na(Field_Gas))))) # number of missing values by rows 
print(Rows) # print the rows containing NA’s 
length(Rows) # number of rows with missing values

Columns <- sapply(Field_Gas, function(Field_Gas) sum(is.na(Field_Gas))) # number of missing values by column 
print(Columns)
sum(is.na(Field_Gas)) # adding up elements that are missing from Field_Gas_Data data table. 
#Is.na () is a generic function that indicates which records are missing and sum() adds them together. 

#Checking for outliers with ggboxplot

NitratePlot1 <- ggplot(data = Field_Gas) + aes(x= HNO3_CONC, y= NO3_CONC) + aes(color = YEAR) + geom_boxplot(outlier.size = 2,
  outlier.color = "red", outlier.shape = 4) + geom_jitter(width = 0.1, alpha = 0.05, color = "blue") # ggboxplot of NO3_CONC before filtration

Nitrateplottheme1 <- theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Times New Roman"), 
                      legend.text = element_text(face = "italic", colour="steelblue",family = "Times New Roman"), 
                      axis.title = element_text(family = "Times New Roman", size = (10), colour = "steelblue4"),
                      axis.text = element_text(family = "Times New Roman", colour = "steelblue", size = (10)))

print(NitratePlot1 + Nitrateplottheme1 + labs( title= "Distribution of Outliers in Nitrate Variable (NO3_CONC) before Filtration", y="Concentration of Nitrate,NO3_CONC,(μg/m3)", 
                                      x = "Concentration of Nitric Acid, HNO3_CONC (μg/m3)")) # theme, tittle and axis labelling


NitratePlot2 <- ggplot(data = Field_Gas_Data_Filtered) + aes(x = HNO3_CONC, y = NO3_CONC) + aes(color = YEAR) + geom_boxplot(outlier.size = 2,
 outlier.color = "red", outlier.shape = 3) + geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
Nitrateplottheme2 <- theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (15)), 
                           legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Times New Roman"), 
                           legend.text = element_text(face = "italic", colour="steelblue",family = "Times New Roman"), 
                           axis.title = element_text(family = "Times New Roman", size = (10), colour = "steelblue4"),
                           axis.text = element_text(family = "Times New Roman", colour = "steelblue", size = (10)))

print(NitratePlot2 + Nitrateplottheme2 + labs( title= "Distribution of Outliers in Nitrate Variable (NO3_CONC) after Filtration", y="Concentration of Nitrate,NO3_CONC,(μg/m3)", 
                                                 x = "Concentration of Nitric Acid, HNO3_CONC (μg/m3)")) # theme, tittle and axis labelling
NitratePlot3 <- ggplot(data = Field_Gas_Data_Transformed) + aes(x = TNO3_CONC, y = NH4_CONC) + aes(color = YEAR) + geom_boxplot(outlier.size = 2,
                                                                                                                             outlier.color = "red", outlier.shape = 3) + geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
Nitrateplottheme3 <- theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = (15)), 
                           legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Times New Roman"), 
                           legend.text = element_text(face = "italic", colour="steelblue",family = "Times New Roman"), 
                           axis.title = element_text(family = "Times New Roman", size = (10), colour = "steelblue4"),
                           axis.text = element_text(family = "Times New Roman", colour = "steelblue", size = (10)))

print(NitratePlot3 + Nitrateplottheme3 + labs( title= "Distribution of Outliers in Total Nitrate Variable (TNO3_CONC) after Transformation", y="Concentration of Nitrate,NO3_CONC,(μg/m3)", 
                                               x = "Concentration of Total Nitrate, TNO3_CONC (μg/m3)")) # theme, tittle and axis labelling

ggarrange(NitratePlot1, NitratePlot2, NitratePlot3, ncol = 3) # arranging two plots 
                                                                                  
# Assessing Normality

require(gridExtra)
plot1 <- ggqqplot(Field_Gas_Data, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC") # ggqqplot before filtration 
print(plot1) # print the plot
plot2 <- ggqqplot(Field_Gas_Data_Filtered, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC") # ggqqplot after filatration
print(plot2) # print the plot
grid.arrange(plot1, plot2, ncol=2) # side-by-side printing of plots


# Threshold min and max values of the variables as conditions for filtration
Thresh.NH4_CONC.min <- 0.1
Thresh.NH4_CONC.max <- 0.4

Thresh.NO3_CONC.min <- 0.1
Thresh.NO3_CONC.max <- 0.7

Thresh.HNO3_CONC.min <- 0.1
Thresh.HNO3_CONC.max <- 1

Thresh.SO2_CONC.min <- 0.1
Thresh.SO2_CONC.max <- 0.5


Thresh.SO4_CONC.min <- 0.1
Thresh.SO4_CONC.max <- 1

NH4_CONC <- Field_Gas$NH4_CONC # assigning value to a name
NO3_CONC <- Field_Gas$NO3_CONC #assigning value to a name
HNO3_CONC <- Field_Gas$HNO3_CONC # assigning value to a name
SO2_CONC <- Field_Gas$SO2_CONC # assigning value to a name
SO4_CONC <- Field_Gas$SO4_CONC # assigning value to a name

Field_Gas_Data_Filtered <- Field_Gas %>% filter(complete.cases(.)) %>% filter(NH4_CONC > 0.1) %>%
  filter(NH4_CONC < 0.4) %>% filter(NO3_CONC > 0.1) %>% filter(NO3_CONC < 0.7) %>% filter(HNO3_CONC > 0.1) %>%
  filter(HNO3_CONC < 1) %>% filter(SO2_CONC > 0.1) %>% filter(SO2_CONC < 0.5) %>% filter(SO4_CONC > 0.1) %>%
  filter(SO4_CONC < 1)
dim(Field_Gas_Data_Filtered) # check the dimesnions after filtration of data frame

#
#Checking that the missing value have been filtered

Rows <- unique(unlist(lapply(Field_Gas_Data_Filtered, function(Field_Gas_Data_Filtered) which(is.na(Field_Gas_Data_Filtered))))) # number of
print(Rows)
#missing values in Field_Gas_Data_Filtered
sum(is.na(Field_Gas_Data_Filtered))
Columns <- sapply(Field_Gas_Data_Filtered, function(Field_Gas_Data_Filtered) sum(is.na(Field_Gas_Data_Filtered))) # number of missing values by
#column with Field_Gas_Data_Filtered after removal of NA’s values. The sapply () function output confirms
#that the missing values are removed from the dataset.
print(Columns)
sapply(Field_Gas_Data_Filtered, class)
print(Field_Gas_Data_Filtered$SITE_ID)
levels(Field_Gas_Data_Filtered$SITE_ID)
View(Field_Gas_Data_Filtered)
#Side-by-side QQplots and box plots
require(gridExtra)
plot1 <- ggqqplot(Field_Gas_Data, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC") 
plot2 <- ggqqplot(Field_Gas_Data_Filtered, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC") 
grid.arrange(plot1, plot2, ncol=2)

plot3 <- ggplot(data = Field_Gas_Data) + aes(x = HNO3_CONC, y = NO3_CONC) + aes(color = NO3_CONC) + geom_boxplot(outlier.size = 2,
                                                                                                                 outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
plot4 <- ggplot(data = Field_Gas_Data_Filtered) + aes(x = HNO3_CONC, y = NO3_CONC) + aes(color = NO3_CONC)) + geom_boxplot(outlier.size = 2,
                                                                                                                 outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")
grid.arrange(plot3, plot4, ncol=2)


# group base data summarization : 
SITE_ID <- Field_Gas_Data_Filtered$SITE_ID
YEAR <- Field_Gas_Data_Filtered$SITE_ID
WEEK <- Field_Gas_Data_Filtered$WEEK
MONTH <-Field_Gas_Data_Filtered$MONTH
Field_Gas_Data_Trans <- Field_Gas_Data_Filtered %>%
  mutate(TNO3_CONC = NO3_CONC + HNO3_CONC) %>% mutate(TS_CONC = SO4_CONC + SO2_CONC) %>%
  mutate(INDEX = abs(TNO3_CONC - TS_CONC))
View(Field_Gas_Data_Trans) 


#Field_Gas_Data_Trans <- Field_Gas_Data_Filtered %>% 
 # select(SITE_ID, YEAR, WEEK, MONTH, SO2_CONC, SO4_CONC, NO3_CONC, HNO3_CONC, NH4_CONC) %>% 
 # mutate(TNO3_CONC = NO3_CONC + HNO3_CONC) %>% mutate(TS_CONC = SO4_CONC + SO2_CONC)
 #View(Field_Gas_Data_Trans) 
  
  
TS_CONC <- Field_Gas_Data_Trans$TS_CONC
TNO3_CONC <- Field_Gas_Data_Trans$TNO3_CONC
INDEX <- Field_Gas_Data_Trans$INDEX
Field_Gas_Data_Final <-  Field_Gas_Data_Trans %>% group_by(YEAR, MONTH, WEEK) %>% 
  summarise(TS_CONC = mean(TS_CONC), TNO3_CONC = mean(TNO3_CONC), 
             NH4_CONC = mean(TNO3_CONC), INDEX = mean(INDEX))

View(Field_Gas_Data_Final)
  
dim(Field_Gas_Data_Final)
hist(TNO3_CONC)
hist(TS_CONC)
hist(NH4_CONC)
hist(INDEX)
hist

#Transformation



T_cub <- sign(x)*abs(x)^(1/3)
Field_Gas_Data_Transformed <- Field_Gas_Data_Final %>% 
  transform(NH4_CONC = sign(NH4_CONC)*abs(NH4_CONC)^(1/3)) %>% 
  transform(TNO3_CONC = sign(TNO3_CONC)*abs(NH4_CONC)^(1/3)) %>% 
  transform(TS_CONC = sign(TS_CONC)*abs(TS_CONC)^(1/3)) %>% 
  transform(INDEX = sign(INDEX)*abs(INDEX)^(1/3))
  
View(Field_Gas_Data_Transformed)
with(Field_Gas_Data_Transformed, shapiro.test(Field_Gas_Data_Transformed$TNO3_CONC)) # Shapiro-Wilk test for the “post” variable
View(Field_Gas_Data_Transformed)
3Q-Q pLota
plot1 <- ggqqplot(Field_Gas_Data, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC") 
plot2 <- ggqqplot(Field_Gas_Data_Filtered, title = "NORMAL Q-Q PLOT", x = "NO3_CONC", color = c("#00AFBB"), y = "NO3_CONC")
plot3 <- ggqqplot(Field_Gas_Data_Transformed, title = "NORMAL Q-Q PLOT", x = "TNO3_CONC", color = c("#00AFBB"), y = "TNO3_CONC")
grid.arrange(plot1, plot2, plot3, ncol=3)

Scatterplot with marginal histogram
p <- ggplot(Field_Gas_Data_Transformed, aes(TNO3_CONC, TS_CONC, color = TNO3_CONC)) + geom_point( size = 2) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50') + theme_classic()
ggExtra::ggMarginal(p, type = "histogram", fill = c("lightgreen"), xparams = list(fill = "purple", bins=10))

library(viridis)

# LINES PLOT: Discrete color. use the argument discrete = TRUE need categorical data
ggplot(Field_Gas_Data_Transformed, aes(TNO3_CONC, YEAR))+
  geom_point(aes(color = MONTH)) +
  geom_smooth(aes(color = YEAR, fill = YEAR), method = "lm") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + ggtitle(lab = "Distribution of Total Nitrate Deposition (μg/m3), from 2011 - 2019")

ggplot(Field_Gas_Data_Transformed, aes(TS_CONC, YEAR))+
  geom_point(aes(color = MONTH)) +
  geom_smooth(aes(color = YEAR, fill = YEAR), method = "lm") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + ggtitle(lab = "Distribution of Total Sulphur Deposition (μg/m3), from 2011 - 2019")


library(plotly)
# Kernel Density
p <- ggplot(Field_Gas_Data_Transformed, aes(x = TS_CONC) + 
  geom_density(aes(fill = YEAR), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")
p <- ggplotly(p)
p <- ggplot(Field_Gas_Data_Transformed, aes(x = TS_CONC)) + 
  geom_density(aes(fill = YEAR), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")

p <- ggplotly(p)
p
#Pearson correlation coefficient
cor.test(TNO3_CONC, TS_CONC, method=c("pearson"))