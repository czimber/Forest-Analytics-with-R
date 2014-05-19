require(FAwR)
data(herbdata)

herbdata$date <- as.POSIXct(strptime(herbdata$date,"%m/%d/%Y"))

coplot(height ~ dia | treat * rep, type = "p",
               data = herbdata[herbdata$isalive == 1,],
       ylab = "Height (cm)", xlab = "Basal Diameter (mm)")

summary(herbdata)
