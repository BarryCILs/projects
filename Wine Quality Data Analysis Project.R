##################################
#Question 1 - Understand the Data
##################################

#install.packages("e1071")
library("e1071")

#Q1.(ii) assign the data to a matrix
wine_data<-as.matrix(read.table("RedWine.txt"))

# using my student ID number for reproducible sampling with the seed function
set.seed(220358865) 


#Q2.(iii) Generate a subset of 450 data with added quality variable
#quality is score between 0 and 10
wine_data<-wine_data[sample(1:1599,450),c(1:6)]

#rename columns of the data set
colnames(wine_data)<-c("citric_acid","chlorides","total_sulfur_dioxide","pH","alcohol","quality")

#check dataset dimension in order to see number of rows and columns of the dataset
dim(wine_data)

# Create 5 scatterplots function (for each X variable against the variable of interest Y) 

#create function to print out pearsoncorrelation relationship
cor_func<-function(a,b){
  cor=cor(a,b)
  if(cor==0){return("No Correlation")}
  else if(0<cor && cor<=0.5){return("Low Positive Correlation")}
  else if(0.5<cor && cor<=0.9){return("High Positive Correlation")}
  else if(0.9<cor && cor<=1){return("Perfect Positive Correlation")}
  else if(0>cor && cor>=-0.5){return("Low Negative Correlation")}
  else if(-0.5>cor && cor>=-0.9){return("High Negative Correlation")}
  else if(-0.9>cor && cor>=-1){return("Perfect Negative Correlation")}
  else{return("Error")}
}

par(mfrow=c(2,3)) #set 2x3 subplots
#plot the Scatter plot of Citric Acid vs Quality
plot(wine_data[,1],wine_data[,6],col="green",main="Scatter plot of Citric Acid vs Quality", xlab="Citric_acid",ylab="Quality")
#print out the correlation 
cor.test(wine_data[,1],wine_data[,6])
cat("The correlation between Citric Acid and Quality is", cor_func(wine_data[,1],wine_data[,6]))

#plot the Scatter plot of chlorides vs Quality
plot(wine_data[,2],wine_data[,6],col="darkblue",main="Scatter plot of Chlorides vs Quality", xlab="chlorides",ylab="Quality")
#print out the correlation 
cor.test(wine_data[,2],wine_data[,6])
cat("The correlation between Chlorides and Quality is", cor_func(wine_data[,2],wine_data[,6]))

#plot the Scatter plot of total_sulfur_dioxide vs Quality
plot(wine_data[,3],wine_data[,6],col="lightblue",main="Scatter plot of Total_sulfur_dioxide vs Quality", xlab="total_sulfur_dioxide",ylab="Quality")
#print out the correlation 
cor.test(wine_data[,3],wine_data[,6])
cat("The correlation between Total_sulfur_dioxide and Quality is", cor_func(wine_data[,3],wine_data[,6]))

#plot the Scatter plot of pH vs Quality
plot(wine_data[,4],wine_data[,6],col="pink",main="Scatter plot of pH vs Quality", xlab="pH",ylab="Quality")
#print out the correlation 
cor.test(wine_data[,4],wine_data[,6])
cat("The correlation between pH and Quality is", cor_func(wine_data[,4],wine_data[,6]))

#plot the Scatter plot of alcohol vs Quality
plot(wine_data[,5],wine_data[,6],col="grey",main="Scatter plot of Alcohol vs Quality", xlab="alcohol",ylab="Quality")
#print out the correlation 
cor.test(wine_data[,5],wine_data[,6])
cat("The correlation between Alcohol and Quality is", cor_func(wine_data[,5],wine_data[,6]))

# Create 6 histograms for each X variable and Y
par(mfrow=c(2,3)) #create 2x3 subplots
#plot histograms for each columns
hist(wine_data[,1],col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level", ylab="Numbers of sample")
hist(wine_data[,2],col=4, main="Histogram of Chlorides", xlab="Chlorides Level", ylab="Numbers of sample")
hist(wine_data[,3], col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel", ylab="Numbers of sample")
hist(wine_data[,4], col=6, main="Histogram of pH", xlab="pH Level", ylab="Numbers of sample")
hist(wine_data[,5], col=8, main="Histogram of Alcohol", xlab="Alcohol Level", ylab="Numbers of sample")
hist(wine_data[,6], col=7, main="Histogram of Quality", xlab="Quality Level", ylab="Numbers of sample")

#plot histograms for each variable to show whether they are normally distributed
hist(wine_data[,1], freq = FALSE,col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level")
curve(dnorm(x, mean=mean(wine_data[,1]), sd=sd(wine_data[,1])), add=TRUE, col = "red")
hist(wine_data[,2], freq = FALSE,col=4, main="Histogram of Chlorides", xlab="Chlorides Level")
curve(dnorm(x, mean=mean(wine_data[,2]), sd=sd(wine_data[,2])), add=TRUE, col = "red")
hist(wine_data[,3], freq = FALSE,col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel")
curve(dnorm(x, mean=mean(wine_data[,3]), sd=sd(wine_data[,3])), add=TRUE, col = "red")
hist(wine_data[,4], freq = FALSE,col=6, main="Histogram of pH", xlab="pH Level")
curve(dnorm(x, mean=mean(wine_data[,4]), sd=sd(wine_data[,4])), add=TRUE, col = "red")
hist(wine_data[,5], freq = FALSE,col=8, main="Histogram of Alcohol", xlab="Alcohol Level")
curve(dnorm(x, mean=mean(wine_data[,5]), sd=sd(wine_data[,5])), add=TRUE, col = "red")
hist(wine_data[,6], freq = FALSE,col=7, main="Histogram of Quality", xlab="Quality Level")
curve(dnorm(x, mean=mean(wine_data[,6]), sd=sd(wine_data[,6])), add=TRUE, col = "red")
#reset back to 1x1 plot
par(mfrow=c(1,1))

#check skewness of columns, figure>1 has greater skewness
#display skewness of each histogram
cat("The skewness of Histogram of Citric Acid is",round(skewness(wine_data[,1]),2))
cat("The skewness of Histogram of Chlorides is",round(skewness(wine_data[,2]),2))
cat("The skewness of Histogram of Total Sulfur Dioxide is",round(skewness(wine_data[,3]),2))
cat("The skewness of Histogram of pH is",round(skewness(wine_data[,4]),2))
cat("The skewness of Histogram of Alcohol is",round(skewness(wine_data[,5]),2))
cat("The skewness of Histogram of Quality is",round(skewness(wine_data[,6]),2))


################################
#Question 2 - Transform the Data
################################

# Choose any four X variables and Y
index <- c("citric_acid","chlorides","total_sulfur_dioxide","alcohol","quality") 

wine_dt <- subset(wine_data[,index])  # obtain a 450 by 5 matrix

wine_d <- wine_dt #make a copy of the original data before data transformation


#identify and remove outliers
#https://statsandr.com/blog/outliers-detection-in-r/
#use boxplot to visualize potential outliers of each variable
par(mfrow=c(2,3)) #create 2x3 subplots
boxplot(wine_dt[,1], col="green",ylab="Citric Acid", main="boxplot of Citric Acid")
boxplot(wine_dt[,2], col="darkblue", ylab="Chlorides", main="boxplot of Chlorides")
boxplot(wine_dt[,3], col="lightblue", ylab="Total Sulfur Dioxide", main="boxplot of Total Sulfur Dioxide")
boxplot(wine_dt[,4], col="grey", ylab="Alcohol", main="boxplot of Alcohol")
boxplot(wine_dt[,5], col="yellow", ylab="Quality", main="boxplot of Quality")

#isolate potential outliers of each variable base on the interquartile range (IQR) criterion
#https://towardsdatascience.com/outliers-detection-in-r-6c835f14e554
out1<-boxplot.stats(wine_dt[,1])$out
out2<-boxplot.stats(wine_dt[,2])$out
out3<-boxplot.stats(wine_dt[,3])$out
out4<-boxplot.stats(wine_dt[,4])$out
out5<-boxplot.stats(wine_dt[,5])$out

cat("Citric Acid has",length(out1),"outliers.")
cat("Chlorides has",length(out2),"outliers.")
cat("Total Sulfur Dioxide has",length(out3),"outliers.")
cat("Alcohol has",length(out4),"outliers.")
cat("Quality has",length(out5),"outliers.")

#Citric Acid variable has no outlier, so just focus on the rest of the 4 variables
out_ind2 <- which(wine_dt[,2] %in% c(out2))
out_ind3 <- which(wine_dt[,3] %in% c(out3))
out_ind4 <- which(wine_dt[,4] %in% c(out4))
out_ind5 <- which(wine_dt[,5] %in% c(out5))

#calculate total number of outliers
out_total=sum(length(out2),length(out3),length(out4),length(out5))
cat("Total",out_total,"outliers, which is around",out_total/dim(wine_dt)[1]*100,
    "% samples from the dataset have identified as outliers.")

#https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
#list out the row number of potential outliers of each variables
cat("Chlorides has outliers in row",out_ind2)
cat("Total Sulfur Dioxide has outliers in row",out_ind3)
cat("Alcohol has outliers in row",out_ind4)
cat("Quality has outliers in row",out_ind5)

#create WinsorizationW function to convert outliers 
#https://towardsdatascience.com/why-1-5-in-iqr-method-of-outlier-detection-5d07fdc82097
#https://m-nayoumi.medium.com/scaling-outliers-using-winsorizing-ecd6901187cb
winsor <-function(column,outliers){
  #set Q3 of IQR
  Q3<-quantile(column, prob = c(0.75))[["75%"]]
  #set Q1 of IQR
  Q1<-quantile(column, prob = c(0.25))[["25%"]]
  upQ <- Q3 + (1.5*IQR(column))#set upper bound 
  lowQ <- Q1 - (1.5*IQR(column))#set lower bound
  if(lowQ<0){lowQ<-0}#cannot let variable turn in negative number
  print(upQ)
  print(lowQ)
  #Winsorization - assign Q1 & Q3 values to outliers
  for (i in 1:length(outliers)) {
    if (column[outliers][i]>upQ){
      column[outliers][i]<-Q3
    } else if (column[outliers][i]<lowQ){
      column[outliers][i]<-Q1
    }
  }
  print(column[c(outliers)])
}

wine_dt3 <- wine_dt #create dataset for Winsorization 

#Execute Winsorization 
wine_dt3[,2][out_ind2]<-winsor(wine_dt3[,2],out_ind2)
wine_dt3[,3][out_ind3]<-winsor(wine_dt3[,3],out_ind3)
wine_dt3[,4][out_ind4]<-winsor(wine_dt3[,4],out_ind4)
wine_dt3[,5][out_ind5]<-winsor(wine_dt3[,5],out_ind5)

#check if removing all outliers of each variable affect the its relationship with quality
#https://humansofdata.atlan.com/2018/03/when-delete-outliers-dataset/
#check the effect on removing all outliers vs Winsorization
wine_dt2 <- wine_dt[-c(out_ind2,out_ind3,out_ind4,out_ind5),]
par(mfrow=c(1,3))
#use Citric Acid to check Quality variable after or prior outlier treatment
scatter.smooth(wine_dt[,1],wine_dt[,5],xlab='Citric Acid',ylab='Quality',main='Kept Outliers')
scatter.smooth(wine_dt2[,1],wine_dt2[,5],xlab='Citric Acid',ylab='Quality',main='Remove Outliers')
scatter.smooth(wine_dt3[,1],wine_dt3[,5],xlab='Citric Acid',ylab='Quality',main='Winsorization')

scatter.smooth(wine_dt[,2],wine_dt[,5],xlab='Chlorides',ylab='Quality',main='Kept Outliers')
scatter.smooth(wine_dt2[,2],wine_dt2[,5],xlab='Chlorides',ylab='Quality',main='Remove Outliers')
scatter.smooth(wine_dt3[,2],wine_dt3[,5],xlab='Chlorides',ylab='Quality',main='Winsorization')


scatter.smooth(wine_dt[,3],wine_dt[,5],xlab='Total Sulfur Dioxide',ylab='Quality',main='Kept Outliers')
scatter.smooth(wine_dt2[,3],wine_dt2[,5],xlab='Total Sulfur Dioxide',ylab='Quality',main='Remove Outliers')
scatter.smooth(wine_dt3[,3],wine_dt3[,5],xlab='Total Sulfur Dioxide',ylab='Quality',main='Winsorization')

scatter.smooth(wine_dt[,4],wine_dt[,5],xlab='Alcohol',ylab='Quality',main='Kept Outliers')
scatter.smooth(wine_dt2[,4],wine_dt2[,5],xlab='Alcohol',ylab='Quality',main='Remove Outliers')
scatter.smooth(wine_dt3[,4],wine_dt3[,5],xlab='Alcohol',ylab='Quality',main='Winsorization')


#use box plot to check if there is any outliers after outlier treatment
par(mfrow=c(2,3)) #create 2x3 subplots
boxplot(wine_dt3[,1], col="green",ylab="Citric Acid", main="boxplot of Citric Acid")
boxplot(wine_dt3[,2], col="darkblue", ylab="Chlorides", main="boxplot of Chlorides")
boxplot(wine_dt3[,3], col="lightblue", ylab="Total Sulfur Dioxide", main="boxplot of Total Sulfur Dioxide")
boxplot(wine_dt3[,4], col="grey", ylab="Alcohol", main="boxplot of Alcohol")
boxplot(wine_dt3[,5], col="yellow", ylab="Quality", main="boxplot of Quality")

#check if the correlation with quality changed
cor_func(wine_dt3[,1],wine_dt3[,5])#Citric Acid
cor_func(wine_dt3[,2],wine_dt3[,5])#Chlorides
cor_func(wine_dt3[,3],wine_dt3[,5])#Total Sulfur Dioxide
cor_func(wine_dt3[,4],wine_dt3[,5])#Alcohol
#correlation with quality has not changed, go as planned.

wine_d2 <- wine_dt2 #make another copy before transformation
wine_d3 <- wine_dt3 #make another copy before transformation

#now we plot histogram of each variable then design what method of data transformation
par(mfrow=c(2,3)) #create 2x3 subplots
#plot histograms for each columns
hist(wine_dt3[,1], col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level", ylab="Numbers of sample")
hist(wine_dt3[,2], col=4, main="Histogram of Chlorides", xlab="Chlorides Level", ylab="Numbers of sample")
hist(wine_dt3[,3], col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel", ylab="Numbers of sample")
hist(wine_dt3[,4], col=8, main="Histogram of Alcohol", xlab="Alcohol Level", ylab="Numbers of sample")
hist(wine_dt3[,5], col=7, main="Histogram of Quality", xlab="Quality Level", ylab="Numbers of sample")


#negation function 
negation<-function(column){
  min(column)+max(column)-column
}

# min_max normalization function
min_max <- function(column){
  (column-min(column))/(max(column)-min(column))
}

# z-score standardisation and scaling to unit interval
unit_z <- function(column){
  0.15*((column-mean(column))/sd(column)) + 0.5
}


par(mfrow=c(1,1))
#Citric Acid
skewness(wine_dt3[,1])
#since citric acid already has 0.3401517 skewness 
#we can use z-score standardisation and scaling to unit interval
wine_dt3[,1] <- unit_z(wine_dt3[,1])
hist(wine_dt3[,1], col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level", ylab="Numbers of sample")
#we can perform log tranformations since data has a few 
p1<-0.2
wine_dt3[,1]<-(wine_dt3[,1])^p1
skewness(wine_dt3[,1])#skewness is -0.0002284564, happy


#Chlorides
#since Chlorides up as quality down
#apply negation function to convert the data
wine_dt3[,2] <- negation(wine_dt3[,2])
skewness(wine_dt3[,2])
#since chlorides has 0.0367359 skewness, we would just use z-score standardisation and scaling to unit interval
wine_dt3[,2]<-unit_z(wine_dt3[,2])
hist(wine_dt3[,2], col=4, main="Histogram of Chlorides", xlab="Chlorides Level", ylab="Numbers of sample")
skewness(wine_dt3[,2])#skewness is 0.0367359, happy
#we can perform polynomial transformation to further reduce the skewness
p2<-0.964
wine_dt3[,2]<-wine_dt3[,2]^p2
skewness(wine_dt3[,2])#skewness is 0.0005962648, very happy

#Total Sulfur Dioxide
par(mfrow=c(1,2))
#since Sulfur dioxide up as quality down
#apply negation function to convert the data
wine_dt3[,3] <- negation(wine_dt3[,3])
skewness(wine_dt3[,3])
hist(wine_dt2[,3], col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel", ylab="Numbers of sample")
#since chlorides has -0.8201569 skewness, we would use linear feature scaling to convert to unit interval
wine_dt3[,3] <- min_max(wine_dt3[,3])
hist(wine_dt3[,3], col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel", ylab="Numbers of sample")
#we can perform polynomial transformation to further reduce the skewness 
p3<-2.2
wine_dt3[,3]<-wine_dt3[,3]^p3
skewness(wine_dt3[,3])#skewness is 0.001498176, happy

#Alcohol
par(mfrow=c(1,1))
skewness(wine_dt3[,4])
#since chlorides has 0.7199423 skewness, we would use linear feature scaling to convert to unit interval
wine_dt3[,4] <- min_max(wine_dt3[,4])
#we can perform polynomial transformation to further reduce the skewness
p4<-0.375
wine_dt3[,4]<-wine_dt3[,4]^p4
skewness(wine_dt3[,4])#skewness is -0.0009473035 happy


#Quality
par(mfrow=c(1,1))
skewness(wine_dt3[,5])
#since chlorides has 0.2437681skewness, we would first use Z-scores Standardization method
wine_dt3[,5]<-unit_z(wine_dt3[,5])
hist(wine_dt3[,5], col=7, main="Histogram of Quality", xlab="Quality Level", ylab="Numbers of sample")
#we can perform polynomial transformation to further reduce the skewness
p5<-0.65
wine_dt3[,5]<-wine_dt3[,5]^p5
skewness(wine_dt3[,5])#skewness is -0.00357144, happy

#check if all variables have unit interval
summary(wine_dt3)

#display skewness of each histogram
print("Prior to the data tansformation")
cat("The skewness of Histogram of Citric Acid is",round(skewness(wine_d3[,1]),2))
cat("The skewness of Histogram of Chlorides is",round(skewness(wine_d3[,2]),2))
cat("The skewness of Histogram of Total Sulfur Dioxide is",round(skewness(wine_d3[,3]),2))
cat("The skewness of Histogram of Alcohol is",round(skewness(wine_d3[,4]),2))
cat("The skewness of Histogram of Quality is",round(skewness(wine_d3[,5]),2))
print("After the data tansformation")
cat("The skewness of Histogram of Citric Acid is",round(skewness(wine_dt3[,1]),2))
cat("The skewness of Histogram of Chlorides is",round(skewness(wine_dt3[,2]),2))
cat("The skewness of Histogram of Total Sulfur Dioxide is",round(skewness(wine_dt3[,3]),2))
cat("The skewness of Histogram of Alcohol is",round(skewness(wine_dt3[,4]),2))
cat("The skewness of Histogram of Quality is",round(skewness(wine_dt3[,5]),2))

#plot data set after data transformation
par(mfrow=c(2,3)) #create 2x3 subplots
#plot histograms for each columns
hist(wine_dt3[,1], col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level", ylab="Numbers of sample")
hist(wine_dt3[,2], col=4, main="Histogram of Chlorides", xlab="Chlorides Level", ylab="Numbers of sample")
hist(wine_dt3[,3], col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel", ylab="Numbers of sample")
hist(wine_dt3[,4], col=8, main="Histogram of Alcohol", xlab="Alcohol Level", ylab="Numbers of sample")
hist(wine_dt3[,5], col=7, main="Histogram of Quality", xlab="Quality Level", ylab="Numbers of sample")
par(mfrow=c(1,1))
#plot histograms for each columns
par(mfrow=c(2,3)) #create 2x3 subplots
hist(wine_dt3[,1], freq = FALSE,col=3, main="Histogram of Citric Acid", xlab="Citric Acid Level")
curve(dnorm(x, mean=mean(wine_dt3[,1]), sd=sd(wine_dt3[,1])), add=TRUE, col = "red")
hist(wine_dt3[,2], freq = FALSE, col=4, main="Histogram of Chlorides", xlab="Chlorides Level")
curve(dnorm(x, mean=mean(wine_dt3[,2]), sd=sd(wine_dt3[,2])), add=TRUE, col = "red")
hist(wine_dt3[,3], freq = FALSE,col=5, main="Histogram of Total Sulfur Dioxide", xlab="Total Sulfur DioxideLevel")
curve(dnorm(x, mean=mean(wine_dt3[,3]), sd=sd(wine_dt3[,3])), add=TRUE, col = "red")
hist(wine_dt3[,4], freq = FALSE,col=8, main="Histogram of Alcohol", xlab="Alcohol Level")
curve(dnorm(x, mean=mean(wine_dt3[,4]), sd=sd(wine_dt3[,4])), add=TRUE, col = "red")
hist(wine_dt3[,5], freq = FALSE,col=7, main="Histogram of Quality", xlab="Quality Level")
curve(dnorm(x, mean=mean(wine_dt3[,5]), sd=sd(wine_dt3[,5])), add=TRUE, col = "red")


# Save this transformed data to a text file
write.table(wine_dt3, "barry-transformed.txt")  # replace ??name?? with either your surname or first name.


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

wine_dt4 <- as.matrix(read.table("barry-transformed.txt"))  # import your saved data

dim(wine_dt4)
# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(wine_dt4,output.1="WAMoutputB.txt",stats.1="WAMstatsB.txt",g=AM,g.inv = invAM) 

# Get weights for Power Mean p=0.1 and p=6 with fit.QAM()
PM01 <- function(x) {x^0.1}
invPM01 <-function(x) {x^(1/0.1)}
PM6 <- function(x) {x^6}
invPM6 <-function(x) {x^(1/6)}

#  Weighted power means with p=0.1, the outputs files are PM01outputB.txt and PM01statsB.txt
fit.QAM(wine_dt4,output.1="PM01outputB.txt",stats.1="PM01statsB.txt",g=PM01,g.inv = invPM01) 
#  Weighted power means with p=6, the outputs files are PM6outputB.txt and PM6statsB.txt
fit.QAM(wine_dt4,output.1="PM6outputB.txt",stats.1="PM6statsB.txt",g=PM6,g.inv = invPM6) 

# Get weights for Ordered Weighted Average with fit.OWA()
#  OWA, the outputs files are OWAoutputB.txt and OWAstatsB.txt 
fit.OWA(wine_dt4,"OWAoutputB.txt","OWAstatsB.txt") 

# Get weights for Choquet Integral with fit.choquet()
# identify the fuzzy measures, The outputs files are ChoutputB.txt and ChstatsB.txt 
fit.choquet(wine_dt4,output.1="ChoutputB.txt",stats.1="ChstatsB.txt")

#read the ChstatsB txt file to extract the fuzzy measure values
chstats<-read.table("ChstatsB.txt",fill = TRUE , header = FALSE )
#convert characters to numeric and round up to 4 decimal points 
chstatsv<-round(as.numeric(chstats[12:26,2]),digits = 4)

#Since we have fuzzy measure value for v1-4 from the Choquet model report
#we can build a function to test the variable interaction
fuzzy_measure <- function(num,array){
  v1<-chstatsv[1]
  v2<-chstatsv[2]
  v3<-chstatsv[4]
  v4<-chstatsv[8]
  #create a list of fuzzy measure value for v1-4
  fuzzynumber<-c(v1,v2,v3,v4)
  #sum up the value of selected input
  sum<-sum(fuzzynumber[array])
  if (num>sum){cat("Variable set is Complementary, since set value",num,"is more than sum of the variable set",sum)}
  else if (num<sum){cat("Variable set is Redundant, since set value",num,"is less than sum of the variable set",sum)}
  else if (num==sum){cat("Variable set is Additive or no interaction, since set value",num,"is equal to sum of the variable set",sum)}
  else{print("Error")}
}
#set v1,2
fuzzy_measure(chstatsv[3],c(1,2))
#set v1,3
fuzzy_measure(chstatsv[5],c(1,3))
#set v2,3
fuzzy_measure(chstatsv[6],c(2,3))
#set v1,2,3
fuzzy_measure(chstatsv[7],c(1,2,3))
#set v1,4
fuzzy_measure(chstatsv[9],c(1,4))
#set v2,4
fuzzy_measure(chstatsv[10],c(2,4))
#set v1,2,4
fuzzy_measure(chstatsv[11],c(1,2,4))
#set v3,4
fuzzy_measure(chstatsv[12],c(3,4))
#set v1,3,4
fuzzy_measure(chstatsv[13],c(1,3,4))
#set v2,3,4
fuzzy_measure(chstatsv[14],c(2,3,4))
#set v1,2,3,4
fuzzy_measure(chstatsv[15],c(1,2,3,4))



#######################################
#Question 4 - Use Model for Prediction
#######################################

#assign given input
inputs <- c(0, 0.075, 41, 3.53, 9.3) 

# choose the same four X variables as in Q2 
inputf<- inputs[c(1:3,5)] # choose the same four X variables as in Q2 

inputform<-inputf #make a copy of the inputs

wine_dt5<-wine_d3#copy the dataset prior to data transformation in Q2


#Apply the same data transformation process for all 4 variables
#Citric Acid 
#use z-score standardisation and scaling to unit interval
inputform[1]<-0.15*((inputform[1]-mean(wine_dt5[,1]))/sd(wine_dt5[,1])) + 0.5
#polynomial transformation
p1<-0.2
inputform[1]<-inputform[1]^p1

#Chlorides
#since Chlorides up as quality down
#apply negation function to convert the data
inputform[2] <- min(wine_dt5[,2])+max(wine_dt5[,2])-inputform[2]
#use z-score standardisation and scaling to unit interval
inputform[2]<-0.15*((inputform[2]-mean(wine_dt5[,2]))/sd(wine_dt5[,2])) + 0.5
#wpolynomial transformation
p2<-0.964
inputform[2]<-inputform[2]^p2

#Total Sulfur Dioxide
#since Sulfur dioxide up as quality down
#apply negation function to convert the data
inputform[3] <- min(wine_dt5[,3])+max(wine_dt5[,3])-inputform[3]
#use linear feature scaling to convert to unit interval
inputform[3] <- (inputform[3]-min(wine_dt5[,3]))/(max(wine_dt5[,3])-min(wine_dt5[,3]))
#we can perform polynomial transformation to further reduce the skewness
p3<-2.2
inputform[3]<-inputform[3]^p3

#Alcohol
#use linear feature scaling to convert to unit interval
inputform[4] <- (inputform[4]-min(wine_dt5[,4]))/(max(wine_dt5[,4])-min(wine_dt5[,4]))
#we can perform polynomial transformation to further reduce the skewness
p4<-0.375
inputform[4]<-inputform[4]^p4

#check the transformed inputs
inputform

# applying the transformed variables to the best model selected from Q3 for Y prediction
x<-inputform #assign the input 
#assign the weight
v<-c(0.275791421477913,0.275791421477914,0.275791421477914,0,0.275791421477913,
     0.412719111328928,0.412719111328928,0,0.707199864044463,0.819184434882208,
     0.819184434882206,0.495304918386218,0.707199864043667,1.0000000000001,0.999999999999296)
#apply choquet model to predict the quality of the wine
quality_predict<-choquet(x,v)


# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
#first, reverse polynomial transformation
p5<-0.65
quality_predict<-quality_predict^(1/p5)
#2nd, reverse z-score standardisation 
quality_predict<-(quality_predict-0.5)/0.15*sd(wine_dt5[,5])+mean(wine_dt5[,5])
#check the ouput
quality_predict
#round up predicted value
quality_predict<-round(quality_predict)
cat("The predicted quality of wine from given inputs by our model is",quality_predict)

cat("Since the predicted value equals to 1st quartile of wine quality in our 
    original dataset",summary(wine_data)[2,6],"therefore the result is reasonable.")

#The subset below contained data that has highest quality (dataset with outliers handled)
#we can see these wines all contains very low chlorides (mean 0.07) and very high alcohol (mean 11.44).
wine_sub<- subset(wine_d3, wine_d3[,5] == max(wine_d3[,5]))
summary(wine_sub)
#therefore in the condition that the wine contain low chlorides (negative correlation with quality) 
#and high alcohol, higher quality wine would be occurred.


######################################
#Question 5 - Linear Regression Model
######################################
# Load transformed data
# import your saved data
wine_dt6 <- as.matrix(read.table("barry-transformed.txt")) 
#check dimension of the dataset
dim(wine_dt6)

# Apply linear regression with lm() function
lr_fit <- lm(wine_dt6[,5]~wine_dt6[,1:4])
summary(lr_fit)

#check the residuals of our linear regression model
residuals<-data.frame(summary(lr_fit)[[3]])
par(mfrow=c(1,1))
hist(residuals[,1], freq = FALSE,col=14, main="Histogram of Residuals value from linear regression model ", xlab="residuals")
curve(dnorm(x, mean=mean(residuals[,1]), sd=sd(residuals[,1])), add=TRUE, col = "blue")
#if residual histogram is normally distributed then the model fit well on our data
#https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/
cat("The Histogram show the residuals value seems to be symmetrical around 0, 
    also it has skewness of",skewness(residuals[,1]),"indicate our model fit the data well.")

#extract predict values from linear model 
lr_quality<-predict(lr_fit,data.frame(wine_dt6[,1:4]))
#create dataframe
lr_quality_df<-data.frame(lr_quality)
colnames(lr_quality_df)<-"Predicted_quality"
#add the predicted value back to the dataset
wine_dt6p<-cbind(wine_dt6,lr_quality_df)

#################################################################

# Visualizing the predicted values and actual values for both the models on the transformed data

#extract data from the Choquet model and create a data frame
choutput<-data.frame(read.table("ChoutputB.txt"))

colnames(choutput)<-c("citric_acid","chlorides",
                      "total_sulfur_dioxide","alcohol",
                      "quality","predicted_quality")
summary(choutput)

#use scatter plots to show difference between true value and predicted value
#use regression line to show when has better fit
par(mfrow=c(1,2))
#plot for linear model
plot(wine_dt6p[,5],wine_dt6p[,6],col=c("green","purple"),xlab="Quality",ylab="Predicted_Quality",main="Quality difference of Linear model")
abline(lm(wine_dt6p[,5]~wine_dt6p[,6]), col="red")#add regression line
#plot for choque model
plot(choutput[,5],choutput[,6],col=c("green","purple"),xlab="Quality",ylab="Predicted_Quality",main="Quality difference of Choquet model")
abline(lm(choutput[,5]~choutput[,6]), col="blue")#add regression line
par(mfrow=c(1,1))

#check correlation of both models
#linear model
cor.test(wine_dt6p[,5],wine_dt6p[,6])#0.5088649 Pearson correlation
#choque model
cor.test(choutput[,5],choutput[,6])#0.4774369 Pearson correlation

cat("Our Linear model histogram has 0.5088649 Pearson correlation measure, 
    it is higher than the Choquet mode which scored 0.4774369")

#calculate the difference between predicted and true value from both models
lr_diff<-(wine_dt6p[,5]-wine_dt6p[,6])#linear model
cho_diff<-(choutput[,5]-choutput[,6])#choque model

#scatter plot the quality of both models into 1 plot,
par(mfrow=c(1,1))
plot(lr_diff,cho_diff,col=c("red","blue"),xlab="Linear Model",ylab="Choquet Model",main="Quality difference between 2 models")
abline(lm(lr_diff~cho_diff), col="green")#add regression line
legend("topright", legend=c("Linear model","Choquet model"), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), pt.cex=2, pch=1)
cor.test(lr_diff,cho_diff)#0.9012929 Pearson correlation between both model

#plot quality difference histogram of linear model
par(mfrow=c(1,2))
hist(lr_diff,xlab="Quality",ylab="Predicted_Quality",main="Quality difference of Linear model",col=rgb(1,0,0,0.5))
curve(dnorm(x, mean=mean(lr_diff), sd=sd(lr_diff)), add=TRUE, col = "red")
#plot quality difference histogram of choquet model
hist(cho_diff,xlab="Quality",ylab="Predicted_Quality",main="Quality difference of Choquet model",col=rgb(0,0,1,0.5))
curve(dnorm(x, mean=mean(cho_diff), sd=sd(cho_diff)), add=TRUE, col = "blue")
#check histogram skewness of both models
skewness(lr_diff) #-0.3710673 skewness linear model
skewness(cho_diff) #-0.4820037 skewness choque model

cat("Our Linear model histogram has",skewness(lr_diff),"skewness which more asymmetry 
than our Choquet mode which has",skewness(cho_diff),"skewness.")

#plot the quality difference histograms of both models into 1 plot, 
#code sourced from website below
#"Two Histograms with melt colors",https://www.r-graph-gallery.com/2-two-histograms-with-melt-colors.html
par(mfrow=c(1,1))
hist(lr_diff, breaks=30,col=rgb(1,0,0,0.5),xlab="Quality",ylab="Predicted_Quality",main="Quality difference between 2 models")
curve(dnorm(x, mean=mean(lr_diff), sd=sd(lr_diff)), add=TRUE, col = "red")
hist(cho_diff, breaks=30, col=rgb(0,0,1,0.5), add=TRUE)
curve(dnorm(x, mean=mean(cho_diff), sd=sd(cho_diff)), add=TRUE, col = "blue")
legend("topright", legend=c("Linear model","Choquet model"), col=c(rgb(1,0,0,0.5), 
      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )



#############################################################################################
#############################################################################################



