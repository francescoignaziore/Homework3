#R code used for the analysis


#Setting up the 2010 and 2017 Data Sets 

#read in the 2010 Appreh.
ap10 <- read.csv("BP apprehensions 2010.csv")
#Creating a row-names vector
cities <- ap10[,1]
#Deleting the column containing the row-names
ap10 <- ap10[-1]
rownames(ap10) <- cities
#reordering the columns starting from January
ap10 <- cbind(ap10[4:12],ap10[1:3])
SumCol10 <- colSums(Filter(is.numeric, ap10))

ap17 <- read.csv("BP Apprehensions 2017_Windows.csv")
ap17 <- ap17[-c(1,2,15)]
ap17 <- cbind(ap17[4:12],ap17[1:3])
rownames(ap17) <- cities
SumCol17 <- colSums(Filter(is.numeric, ap17))

#Creating a matrix containing the Sum of the Columns of each data set
CmbColS <- rbind((SumCol10),(SumCol17))

#Creating a matrix containing the Sum of the Rows of each data set 
CmbRowS <- rbind(rowSums(ap10), rowSums(ap17))




#Making the bar-plots

#Bar plot by month
bp<-barplot(CmbColS,
        beside=TRUE, 
        las=2, 
        main = "Apprehensions by month",
        cex.lab =1.5,
        col = c("orange","red") )
legend("topright",
       c("Apprehensions in 2010","Apprehensions in 2017"),
       cex=0.8,
       bty="n",
       fill=c("orange","red") )


#Bar plot by sector

barplot(CmbRowS,
        beside=TRUE, 
        las=2, main = "Apprehensions by sector", 
        cex.lab =1.5, 
        col = c("orange","red") )
legend("topleft",
       c("Apprehensions in 2010","Apprehensions in 2017"),
       cex=0.8,
       bty="n",
       fill=c("orange","red") )




#Time series code


msum  <- read.csv("BP monthly summaries.csv", stringsAsFactors = FALSE)
years <- msum[c(1)]
msum  <- msum[-c(1)]
rownames(msum) <- as.vector(t(years))
msum <- cbind(msum[4:12], msum[1:3])
msum<-msum[18:1,]
ts3 <- ts(as.vector(unlist(t(msum))),frequency = 12, start=c(2000,1))

#time series plot 
ts.plot(ts3, gpars=list(xlab="year",ylab="Apprehensions", lty=c(1:3)))

# time series - box plot
boxplot(ts3~cycle(ts3))


#Time series plot with yearly means 



#Calculating X-squared test 
chisq.test(ap10) 
chisq.test(ap17)


#function to get t-tests for by-sector comparison
by_sector.t <- function(y){
  
  t.test(as.numeric(ap10[y,]), as.numeric(ap17[y,]), paired = TRUE)
}
# T-test on the sector 8 Tucson (The most trafficated sector in 2010)
by_sector.t(8)
# T-test on the sector 6 Rio Grande Valley (The most trafficated sector in 2017)
by_sector.t(6)



# 3 MONTHS PEAK 2010
#Testing the hyphotesis for which the three most trafficated months'observations in 2010 are equal in mean to the 
#same months in 2017
x17 <- msum[1,]
x10 <- msum[8,]
x17 <- x17[6:8]
x10 <- x10[6:8]
t.test(as.numeric(x10), as.numeric(x17), paired = TRUE)

# 3 MONTHS PEAK 2017
#Testing the hyphotesis for which the three most trafficated months'observations in 2017 are equal in mean to the 
#same months in 2010
x17 <- msum[1,]
x10 <- msum[8,]
x17 <- x17[2:4]
x10 <- x10[2:4]
t.test(as.numeric(x17), as.numeric(x10), paired = TRUE)


ts3 <- ts(as.vector(unlist(t(msum))),frequency = 12, start=c(2000,1))

ts4 <- ts.plot(ts3,main="Time Series from 2010 to 2017", gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))

#create data for calculating the annual means and draw the means line
ts3.1<-as.vector(t(msum))


for (i in 1:18 ){
  if (i == 1){
    from <- i
    to <- 12
    annualMeans <- c(mean(ts3.1[from :to]))
  }  
  else{
    from <- 1 + to
    to <- i*12
  }  
  annualMean<- mean(ts3.1[from: to])
  #print(annualMean)
  annualMeans <- append(annualMeans, annualMean)
  segments(1999+i,annualMean,2000+i,annualMean, col = rgb( 0, .7, .9, .5),lwd=2)
}
annualMeans <- annualMeans[-1]  
text(0.5+seq(from = 2000, to = 2017, by = 1),annualMeans[1:18]+6000,labels=2000:2017, cex=0.7, col = rgb(.9,  0, .7, .5) , font=2)
## add legend
legend('topright', col=c(rgb(0,0,0),  rgb( 0, .7, .9, .5)), lty=1, lwd=2, 
       legend=c("Monthly apprehensions", "Annual averages"), bg='white')






#Plot the time Series Graph
ts4 <- ts.plot(ts3,main="Time Series from 2010 to 2017", gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))

#create data for calculating the annual means and draw the means line
ts3.1<-as.vector(ts3)

for (i in 1:18 ){
  if (i == 1){
    from <- i
    to <- 12
    annualMeans <- c(mean(ts3.1[from :to]))
  }  
  else{
    from <- 1 + to
    to <- i*12
  }  
  annualMean<- mean(ts3.1[from: to])
  #print(annualMean)
  annualMeans <- append(annualMeans, annualMean)
  segments(1999+i,annualMean,2000+i,annualMean, col = "blue" ,lwd=2)
}
annualMeans <- annualMeans[-1]  

## add text labels (year)
text(0.5+seq(from = 2000, to = 2017, by = 1),annualMeans[1:18]+6000,labels=2000:2017, cex=0.7, col = rgb(.9,  0, .7, .5) , font=2)
## add legend
legend('topright', col=c(rgb(0,0,0),  "blue"), lty=1, lwd=2, 
       legend=c("Monthly apprehensions", "Annual averages"), bg='white')


