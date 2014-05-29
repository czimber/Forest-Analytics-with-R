require(FAwR)
data(herbdata)

herbdata$date <- as.POSIXct(strptime(herbdata$date,"%m/%d/%Y"))

coplot(height ~ dia | treat * rep, type = "p",
               data = herbdata[herbdata$isalive == 1,],
       ylab = "Height (cm)", xlab = "Basal Diameter (mm)")

summary(herbdata)

#We see that some of the observations contain NA values, 
#which signify missing values. Other than that, nothing immediately appears 
#to be wrong with the data. We display the first few observations to 
#assess whether or not the other observations in those rows that include 
#NA entries are correct:

head(herbdata[is.na(herbdata$height),])

#We suspect that the trees with missing measurements are those trees 
#that are not alive (isalive = 0). This conjecture can be verified by 
#counting the rows that have missing values anywhere for each value of 
#isalive, as follows.

table(complete.cases(herbdata), herbdata$isalive)

#As we suspected, the rows that contain missing values, which correspond 
#to the complete.cases function returning FALSE, are all dead trees. 
#To complete our scrutiny, in the following section we will visually 
#inspect the relationships among the variables.

#We created a conditioning plot by plotting the height–diameter scatterplots, 
#conditioned on treatment and replication (Figure 2.4). Inspection of 
#Figure 2.4 reveals outliers in the B replication of the control group. 
#In order to focus on these outliers we now need to produce a conditioning 
#plot by measurement date (Figure 2.5):

coplot(height ~ dia | treat * factor(date),
       data = herbdata[herbdata$isalive == 1,],
       type = "p",
       ylab = "Height (cm)",
       xlab = "Basal Diameter (mm)",)

levels(herbdata$treat)
levels(herbdata$rep)

#The date variable is not a factor, so we handle it differently
sort(unique(herbdata$date))

#so we can use the values to index those problematic observations,

bad.index <- herbdata$treat == levels(herbdata$treat)[1] & 
        herbdata$rep == levels(herbdata$rep)[2] & 
        herbdata$date == sort(unique(herbdata$date))[7]
bad.index

bad.data <- herbdata[bad.index,]

bad.data

#A subsequent telephone call revealed the error in our data handling. 
#In this case, the field crew had used a different device to measure 
#the stems on 2004-04-26. During our unit conversion process, the value 
#was incorrectly converted from imperial units to metric units. 
#The values should have been divided by 2.54. We can use the index 
#values again to correct the problem with the following commands:

herbdata$dia[bad.index] <- herbdata$dia[bad.index] / 2.54

#Note that we have to correct the breast-height diameter as well:

herbdata$dbh[bad.index] <- herbdata$dbh[bad.index] / 2.54

#These data-cleaning commands represent important intelligence about the 
#data handling. It is very important that the newly cleaned data object 
#not be saved back over the original dataset. Instead, either the data 
#should be kept as they are and the code that was used to clean the data 
#should be run routinely when the data are used, or a new version of the 
#data, time- stamped, should be saved, and the differences between the 
#original and the cleaned data should be carefully documented in a data 
#dictionary or journal, which should be readily available with the dataset.

#Now that the data have been corrected, we may want to revisit our original 
#plots using coplot or Deepayan Sarkar’s lattice functions to generate 
#the conditioning plot again, verify our results, and continue with our 
#analysis.

#Imagine that we want to find out how many trees we have in each of the 
#treatments of the herbdata object. Using the split function, we can 
#split the data frame into a list of data frames where the name of each 
#treatment is now the item in the list,

split.herb <- split(herbdata, herbdata$treat)
class(split.herb)

names(split.herb)

nrow(split.herb$CONTROL)

nrow(split.herb$OUST)

#We can also check the names of the members,
names(split.herb$CONTROL)

#A more compact way to obtain the row count information is to use the 
#powerful lapply function, which applies its second argument as a 
#function to each of the items in the list that is identified as its 
#first argument; for example

lapply(split.herb, nrow)

#The lapply(X, FUN, ...) function returns a list of the same length as X. 
#Using the newly split data frame, which is stored as a list, we can write 
#for loops to process our data or continue to manipulate our data frame 
#for processing using the core data management functions.

#For simplicity’s sake, we examine a reduced data frame, including only 
#columns 1 (treatment), 2 (replication), 6 (height), and 7 (basal diameter), 
#and print some basic summaries using the lapply function:

herbdata.short <- herbdata[,c(1,2,6,7)]
plot(herbdata.short[,3],herbdata.short[,4])
split.herb.short <- split(herbdata.short, herbdata$treat)
split.herb.short
lapply(split.herb.short, summary)














