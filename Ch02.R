require(FAwR)
require(Hmisc)
data(herbdata)

herbdata$date <- as.POSIXct(strptime(herbdata$date,"%m/%d/%Y"))

coplot(height ~ dia | treat * rep, type = "p",
               data = herbdata[herbdata$isalive == 1,],
       ylab = "Height (cm)", xlab = "Basal Diameter (mm)")

summary(herbdata)

head(herbdata[is.na(herbdata$height),])

table(complete.cases(herbdata), herbdata$isalive)

coplot(height ~ dia | treat * factor(date),
       data = herbdata[herbdata$isalive == 1,],
       type = "p",
       ylab = "Height (cm)",
       xlab = "Basal Diameter (mm)",)

levels(herbdata$treat)
levels(herbdata$rep)

sort(unique(herbdata$date))

bad.index <- herbdata$treat == levels(herbdata$treat)[1] & 
        herbdata$rep == levels(herbdata$rep)[2] & 
        herbdata$date == sort(unique(herbdata$date))[7]
bad.index

bad.data <- herbdata[bad.index,]

bad.data

herbdata$dia[bad.index] <- herbdata$dia[bad.index] / 2.54

herbdata$dbh[bad.index] <- herbdata$dbh[bad.index] / 2.54

split.herb <- split(herbdata, herbdata$treat)
class(split.herb)

names(split.herb)

nrow(split.herb$CONTROL)
nrow(split.herb$OUST)

names(split.herb$CONTROL)

lapply(split.herb, nrow)

herbdata.short <- herbdata[,c(1,2,6,7)]
plot(herbdata.short[,3],herbdata.short[,4])
split.herb.short <- split(herbdata.short, herbdata$treat)
split.herb.short
lapply(split.herb.short, summary)
summary(split.herb.short$CONTROL)

sort(unique(herbdata$date))

herbdata.shorter <-herbdata[herbdata$date == max(herbdata$date), c(1,2,6,8)]
split.herb.shorter <-split(herbdata.shorter, herbdata.shorter$treat)

rt <- cbind(herbdata.shorter,
            dc = cut(herbdata.shorter$dbh,
                     breaks = c(0, 50, 100, 150, 200, 300, 400, 999),
                     labels = c("000--050", "050--100", "100--150",
                                "150--200", "200--300", "300--400","400+")))

st <- aggregate(x = list(basal.area = pi/(4*10^2) * rt$dbh^2,tht = rt$height,
                         stems = rep(1, nrow(rt))),
                by = list(treat = rt$treat,
                          diac = rt$dc),
                FUN = sum)
                                                  
st

st$tht <- st$tht / st$stems / 100
st

cap <- "OUST herbicide trials."
st <- st[order(st$treat, st$diac),]
st$treat <- as.character(st$treat)
st$diac <- as.character(st$diac)
names(st) <- c("Treatment", "Dia. Class (mm)",
               "Basal Area ($\\mbox{mm}^2$)",
               "Mean Total Height (m)", "Stems")
latex(st, rowlabel = NULL, rowname = NULL, file="",
      caption = cap, label = "tab:herbdata_results",
      digits = 5, booktabs = TRUE,
      cjust = c("l","c","r","r","r"))

names(split.herb)

areas <- with(herbdata,
             aggregate(x = list(plot.bh.area = pi/400 * dbh^2,
                                plot.bas.area = pi/400 * dia^2),
                       by = list(treat = treat,
                                 rep = rep,
                                 date = date),
                       FUN = sum))
areas[1:10,]

areas <- with(herbdata,
     aggregate(x = list(plot.bh.area = pi/400 * dbh^2,
                        plot.bas.area = pi/400 * dia^2),
               by = list(treat = treat,
                         rep = rep,
                         date = date),
               FUN = sum,
               na.rm = TRUE))

areas[1:10,]

final.data <- merge(herbdata, areas)
names(final.data)
head(final.data[,c(1,2,3,4,7,10)])

show.cols.with.na <- function(x) {
           ## First, check that object is a data frame
                   if (class(x) != "data.frame")
                             stop("x must be a data frame.\n")
           ## Count the missing values by column.
                   missing.by.column <- colSums(is.na(x))
           ## Are any missing?
                   if (sum(missing.by.column) == 0) {
                             cat("No missing values.\n")
                         } else {
                                 # Only return columns with missing values.
                                 missing <- which(missing.by.column > 0)
                                 return(missing.by.column[missing]) 
                                        
                                        } }

#2.4.1 Upper Flat Creek in the UIEF
#2.4.1.1 Tree Data
ufc.tree <- read.csv("/Users/craigzimber/Documents/R/Forest-Analytics-with-R/ufc.csv")
str(ufc.tree)
show.cols.with.na(ufc.tree)

names(ufc.tree) <-
           c("point","tree","species","dbh.mm","ht.dm")

ufc.tree$dbh.cm <- ufc.tree$dbh.mm / 10
ufc.tree$ba.m2 <- ufc.tree$dbh.cm^2 / 40000 * pi
ufc.tree$height.m <- ufc.tree$ht.dm / 10

height.hat <- ht.fvs.ni.m(ufc.tree$species, ufc.tree$dbh.cm)
missing.hts <- is.na(ufc.tree$height.m)
ufc.tree$height.m[missing.hts] <- height.hat[missing.hts]

ufc.tree$vol.m3 <-
             with(ufc.tree, vol.fvs.ni.m3(species, dbh.cm, height.m))

#2.4.1.2 Plot-Level Data
ufc.baf.met <- 7
ufc.tree$tf.ha <- ufc.baf.met / ufc.tree$ba.m2
ufc.tree$vol.m3.ha <- ufc.tree$vol.m3 * ufc.tree$tf.ha
ufc.SyRS.data <-
        aggregate(x = list(vol.m3.ha = ufc.tree$vol.m3.ha),
                  by = list(point = ufc.tree$point),
                  FUN = sum,
                  na.rm = TRUE)
        
ufc.SyRS.data$weight <- 1
str(ufc.SyRS.data)

#2.4.1.3 Spatial Data
locations <- data.frame(point = 1:144,
                        north.n = rep(c(12:1),12),
                        east.n = rep(c(1:12), rep(12,12)))
locations$north <- (locations$north.n - 0.5) * 134.11
locations$east <- (locations$east.n - 0.5) * 167.64

ufc.SyRS.data <- merge(ufc.SyRS.data, locations)

#Figure 2.6 is then obtained by the following code:
         opar <- par(las=1, pty="s")
plot(ufc.SyRS.data$east, ufc.SyRS.data$north,
             type = "n", axes = F,
             xlim = c(0,max(ufc.SyRS.data$east)+167.64/2),
             ylim = c(0,max(ufc.SyRS.data$north)+134.11/2),
             xlab = "West-East (m)", ylab = "South-North (m)",
             main = expression(paste("Units of 50", m^3, "/ha")))
axis(1); axis(2)
grayrange <- range(ufc.SyRS.data$vol.m3.ha)
text(formatC(ufc.SyRS.data$vol.m3.ha/50,
                             format = "f", digits = 0),
             x = ufc.SyRS.data$east,
             y = ufc.SyRS.data$north, cex=1.5,
             col = gray(1 - (ufc.SyRS.data$vol.m3.ha+200)/800))
par(opar)


#2.4.2 Sweetgum Stem Profiles
raw.data <- scan("FAwR/inst/resources/data/TX_SGUM2.DAT", 
                 what = "", sep = "\n")
length(raw.data)

raw.data <- raw.data[-c(1:26, 1101)]

metadata <- grep("SWEETGUM", raw.data)
cbind(metadata, raw.data[metadata])

substr(raw.data[627], 1, 1) <- "4"
substr(raw.data[910], 1, 1) <- "5"

for (i in 1:length(raw.data)) {
        if(substr(raw.data[i], 57, 64) != "SWEETGUM")
                raw.data[i] <- paste(substr(raw.data[i - 1], 1, 10),
                                     raw.data[i], sep="")
                 }

tree.data <- raw.data[metadata]
length(tree.data)

sections.data <- raw.data[-metadata]
length(sections.data)

sweetgum <-
        data.frame(plot = factor(substr(tree.data, 1, 5)),
                   tree = substr(tree.data, 6, 10),
                   dbh.in = substr(tree.data, 21, 26),
                   stump.ht.ft = substr(tree.data, 27, 32),
                   height.ft = substr(tree.data, 39, 44))
sections <-
        data.frame(plot = factor(substr(sections.data, 1, 5)),
                   tree = substr(sections.data, 6, 10),
                   meas.ln.ft = substr(sections.data, 11, 16),
                   meas.dob.in = substr(sections.data, 20, 25),
                   meas.dib.in = substr(sections.data, 26, 31))

sapply(sweetgum, class)
sapply(sections, class)

#The data are not yet of the appropriate class. We have to convert them. 
#A single loop will suffice.
for (i in 3:5) {
            sweetgum[,i] <- as.numeric(as.character(sweetgum[,i]))
         sections[,i] <- as.numeric(as.character(sections[,i])) }

#We next merge the two data frame objects. R will automatically use the 
#variables that are common to the data frames.
all.meas <- merge(sweetgum, sections, all = TRUE)
dim(all.meas)
names(all.meas)

#We now need to convert the data to metric measures for the section data 
#and the tree-level data.
all.meas$meas.ht.ft <- with(all.meas,meas.ln.ft + stump.ht.ft)
all.meas$meas.ht.m <- all.meas$meas.ht.ft / 3.2808399
all.meas$meas.dob.cm <- all.meas$meas.dob.in * 2.54
sweetgum$height.m <- sweetgum$height.ft / 3.2808399
sweetgum$dbh.cm <- sweetgum$dbh.in * 2.54


spline.vol.m3 <- function(hts.m,
                          ds.cm,
                          max.ht.m,
                          min.ht.m = 0) { 
        rs.cm <- c(ds.cm[order(hts.m)] / 2, 0)
        hts.m <- c(hts.m[order(hts.m)], max.ht.m)
        taper <- splinefun(hts.m, rs.cm)
        volume <- integrate(f = function(x)
                pi * (taper(pmax(x,0))/100)^2,
                lower = min.ht.m,
                upper = max.ht.m)$value
        return(volume)
                          }
#We apply this function to the section data and the tree data using the pow- 
#erful mapply function, along with split.
sweetgum$vol.m3 <- mapply(spline.vol.m3,
                       hts.m = split(all.meas$meas.ht.m, all.meas$tree),
                       ds.cm = split(all.meas$meas.dob.cm, all.meas$tree),
                       max.ht.m = as.list(sweetgum$height.m),
                       min.ht.m = 0.3)

par(las = 1)
plot(sweetgum$vol.m3,
     (sweetgum$dbh.cm/200)^2 * pi * sweetgum$height.m / 2,
     ylab = expression(paste("Second-degree paraboloid volume (",
                             m^3, ")", sep="")),
     xlab = expression(paste("Integrated spline volume (",
                             m^3, ")", sep="")))
    
abline(0, 1, col="darkgrey")
               
#2.4.3 FIA Data
fia.plots <- read.csv("FAwR/inst/resources/data/fia_plots.csv")
fia.plots$forest <- factor(fia.plots$forest)
        
fia.plots$ba.m2.ha <- fia.plots$ba * 2.47105381 / 10.7639104
fia.plots$ht.m <- fia.plots$ht * 0.3048

#2.4.4 Norway Spruce Profiles
gutten <- read.csv("FAwR/inst/resources/data/gutten.csv")
names(gutten) <- tolower(names(gutten))
names(gutten)[names(gutten)=="diameter"] <- "dbh.cm"
str(gutten)

#It would be useful to ensure that we have a unique tree identifier.
gutten$site <- factor(gutten$site)
gutten$location <- factor(gutten$location)
gutten$tree.ID <- with(gutten, interaction(location, tree))
        
with(unique(gutten[,c("site","location","tree.ID")]),
               table(location))

with(unique(gutten[,c("site","location","tree.ID")]),
                     table(location, site))

show.cols.with.na(gutten)

max(gutten$height[is.na(gutten$dbh.cm)])

gutten <- gutten[!is.na(gutten$dbh.cm),]

#2.4.5 Grand Fir Profiles
stage <- read.csv("FAwR/inst/resources/data/stage.csv")
str(stage)

#Some cleaning and manipulation will be necessary. We start by defining 
#the factors.
stage$Tree.ID <- factor(stage$Tree.ID)
stage$Forest.ID <- factor(stage$Forest, 
                          labels = c("Kaniksu", "Coeur d'Alene", "St. Joe", 
                                     "Clearwater", "Nez Perce","Clark Fork",
                                     "Umatilla", "Wallowa", "Payette"))

stage$Hab.ID <- factor(stage$HabType, labels = c("Ts/Pac","Ts/Op", 
                                                 "Th/Pach", "AG/Pach", 
                                                 "PA/Pach"))

#The measurements are all imperial (this was about 1960, after all). We com- 
#pute metric measures.
stage$dbhib.cm <- stage$Dbhib * 2.54
stage$height.m <- stage$Height / 3.2808399
        
#A final check for missing values shows us that there are
show.cols.with.na(stage)

#2.4.6 McDonald–Dunn Research Forest
#2.4.6.1 Stand Data
require(maptools)
stands <- readShapePoly("FAwR/inst/resources/data/stands.shp",verbose=FALSE)
names(stands)
                                  
#2 Available at http://www.cof.orst.edu/cf/forests/mcdonald/. 

#2.4.6.2 Plot Data
#The plot data, read in using the readShapePoints function,
plots <- readShapePoints("FAwR/inst/resources/data/plots.shp")
#contains a plot identifier (plots$UNIPLOT) and plot locations stored as co- 
#ordinates (plots$coords.x1 and plots$coords.x2). The plot locations can be 
#easily added to the existing plot using the plot function with the add = 
#TRUE argument,
plot(plots, add=TRUE, pch=46)
lev <- as.numeric(stands$ALLOCATION)
fgs <- gray(length(levels(stands$ALLOCATION)):1 / 3)
plot(stands,col=fgs[lev],add=FALSE,axes=TRUE)
title(paste("McDonald-Dunn Research Forest",
                      "Stand Boundaries and Plot Locations",
                      sep = "\n"))
legend(1280000, 365000,
                 levels(stands$ALLOCATION)[3:1],
                 fill = fgs[3:1],
                 cex = 0.7,
                 title = "Land Allocations")
plot(plots, add=TRUE, pch=46)

#2.4.6.3 Tree Data
##mdtrees <- read.dbf("FAwR/inst/resources/data/mdtrees.dbf")
##        > head(mdtrees)
##> mdtrees$EXPF <- NA
##> mdtrees$EXPF[mdtrees$SUBPLOT == 1] <-
##        +      20.0 / (0.0054541539 *
##                               +              mdtrees$DBH[mdtrees$SUBPLOT == 1] ^2)
##> mdtrees$EXPF[mdtrees$SUBPLOT == 2] <- 43560 / (pi * 7.78^2)
##> mdtrees$EXPF[mdtrees$SUBPLOT == 3] <- 43560 / (pi * 15.56^2)
##> head(mdtrees[, 3:11])
##> trees.by.plot <- split(mdtrees, mdtrees$PLOT)
##> get.plot.sums <- function(trs) {
##        +
##                + # /******************************************************/
##                + # /* Bruce, D.  1981.  Consistent height-growth and     */
##                + # /*    growth-rate estimates for remeasured plots.     */
##                + # /*    Forest Science 27:711-725.                      */
##                + # /******************************************************/
##                +   site.index.bruce.1981 <- function(tht, bha) {
##                        + tht * exp(-21.663 * (3.744e-2 - (bha + 8.0)^ -0.809)) +}
##        +
##                +   not.missing.dbh <- !is.na(trs$DBH)
##        +   bh.idx <- not.missing.dbh & trs$THT > 4.5
##        +   expf.tot <- sum(trs$EXPF)
##        +   expf.bh <- sum(trs$EXPF[not.missing.dbh])
##        60
##        2 Forest Data Management
##        +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                +
##                + +}
##ba <- sum(0.0054541539 * trs$DBH[not.missing.dbh] ^ 2 *
##                  trs$EXPF[not.missing.dbh])
##qmd <- sqrt(ba / expf.bh / 0.0054541539)
##s.trs <- trs[trs$SITETREE == 1 & trs$SPCODE == "DF" &
##                     !is.na(trs$THT),]
##nst <- nrow(s.trs)
##site.bar <-
##        ifelse(nst > 0,
##               weighted.mean(site.index.bruce.1981(s.trs$THT,
##                                                   s.trs$AGE),
##                             s.trs$EXPF),
##               NA)
##return(c(nrow(trs), expf.bh, expf.tot,
##         ba, qmd, nst, site.bar))
##To generate the summaries, call the sapply function, and transpose and coerce the results from sapply call
##> plot.sums  <-
##        +   data.frame(t(sapply(trees.by.plot, get.plot.sums)))
##so that each row in plot.sums contains the plot-level summaries, computed in the get.plot.sums function.
##The current column names are meaningless, as they have been automat- ically generated. Also, the plot identifier needs to be appended to our data frame. Therefore we use the names from the split operation with the names of the variables from the get.plot.sums function,
##> plot.sums$id <- as.numeric(names(trees.by.plot))
##> names(plot.sums) <- c("trees","expf.bh","expf.tot",
##                        +                       "ba","qmd","nst","site","id")
##> print(head(plot.sums), digits=3)
##> plot.id <- as.numeric(as.character(plots$UNIPLOT))
##> plot.centers <- data.frame(cbind(coordinates(plots), plot.id))
##> names(plot.centers) <- c("x","y","id")
##> final.plots <- merge(plot.centers, plot.sums, all = TRUE)
##> print(head(final.plots[,c(1:3,5:10)]), digits = 3)
##> write.csv( final.plots, "../../data/final-plots.csv")
##2.4.7 Priest River Experimental Forest

#2.4.7.1 Ground Data
pref.tree.all <- read.csv("FAwR/inst/resources/data/pref_trees.csv")

#After importing the data, we take a snapshot of the data frame and check 
#it for missing values.
names(pref.tree.all)
dim(pref.tree.all)

show.cols.with.na(pref.tree.all)

sapply(pref.tree.all, class)
##########################
#ht.m
##########################

table(pref.tree.all$stratum)

#However, making the decision about data type explicit provides us with a 
#layer of error checking. R will warn us if we try to do something that does 
#not make sense, like adding labels together. A one-line for loop takes care 
#of the conversion from integers to factors.
for (i in 1:4)
        pref.tree.all[,i] <- factor(pref.tree.all[,i])

#R will then object, correctly, to the illegal operation of trying to 
#average the stratum labels.
mean(pref.tree.all$stratum)

table(pref.tree.all$point)

#The conversion proceeds as follows.
levels(pref.tree.all$point) <- c("1","3","5","7","9")

#We then check the new values of the variable.
table(pref.tree.all$point)

pref.tree <- subset(pref.tree.all, dbh.cm > 25.4)
dim(pref.tree)

levels(pref.tree$species)
pref.tree$species <- factor(pref.tree$species)
levels(pref.tree$species)


#We will convert this volume measure from board feet to cubic meters. Also, 
#the tree factor must be converted from trees per acre to trees per hectare. 
#Note that the tree factor is the sampling weight for each tree, not the 
#factor that identifies the tree identity!
pref.tree$vol.m3 <- pref.tree$vol.bf / 12 * 0.0283168466
pref.tree$tf.ha <- pref.tree$tf.ac * 2.47105381
pref.tree$vol.m3.ha <- pref.tree$vol.m3 * pref.tree$tf.ha
pref.tree$baf.m2.ha <-
           pref.tree$baf.ft2.ac / 3.2808399^2 / 0.404685642

pref.tree$weight <- 1/pref.tree$baf.ft2.ac

pref.point <- with(pref.tree,
                   aggregate(x = list(ba.m2.ha = baf.m2.ha,
                                      vol.m3.ha = vol.m3.ha),          
                                      by = list(stratum = stratum,
                                                cluster = cluster,
                                                point = point),
                                                FUN = sum))
                   
#We should examine the new object to ensure that it matches our expecta- 
#tions. For example, we expect each of the nine strata to contain 20 points.
table(pref.point$stratum)

design.point <-
        expand.grid(cluster = levels(pref.tree.all$cluster),
        point = levels(pref.tree.all$point))
str(design.point)

design.cluster <-
        unique(pref.tree.all[, c("cluster","stratum")])
str(design.cluster)

dim(design.point)

dim(design.cluster)

test <- merge(x = design.point,
              y = design.cluster,
              all = TRUE)
dim(test)
head(test)

#This seems to have worked just fine. We can now merge the volumes.
design.point <- test
dim(design.point)

dim(pref.point)
test <- merge(x = design.point,
              y = pref.point,
              all = TRUE)
dim(test)

head(test)

#Again, this is successful. Three last steps complete the cleaning.
test$vol.m3.ha[is.na(test$vol.m3.ha)] <- 0
test$ba.m2.ha[is.na(test$ba.m2.ha)] <- 0
pref.point <- test
rm(test)

#Our final step is to append the survey weights. In this case, the weights 
#are equal, which simplifies the process.
pref.point$weight <- 1


#2.4.7.2 Remotely Sensed Data
##pref.pixel <- read.csv("FAwR/inst/resources/data/pref_pixels.csv")

##names(pref.pixel)
##dim(pref.pixel)
##pref.pixel[1:5, c(1:5,11)]
##show.cols.with.na(pref.pixel)
##sapply(pref.pixel, class)
##pref.subplot <- read.csv("../../data/pref_subplots.csv")
##names(pref.subplot)
##dim(pref.subplot)
##pref.subplot[1:5, c(1:3, 5:6, 24)]
##show.cols.with.na(pref.subplot)
##sapply(pref.subplot, class)[1:4]
##pref.subplot$plot <- factor(pref.subplot$plot)
##pref.subplot$subplot <- factor(pref.subplot$subplot)
##dim(pref.point)
##dim(pref.subplot[,c("plot","subplot","acndviC")])
##pref.point.cov <-
##        +
##        +
##        +
##        +
##        +
##        > head(pref.point.cov)
##merge(x = pref.point,
##      y = pref.subplot[,c("plot","subplot","acndviC")],
##      all.x = TRUE, all.y = FALSE,
##      by.x = c("cluster","point"),
##      by.y = c("plot","subplot"))
##dim(pref.point.cov)
##show.cols.with.na(pref.point.cov)


#2.4.8 Leuschner
##leusch.ylds <- read.table("../../data/leuschner.txt",
##                            +                           header = TRUE)

##2.5 Summary

