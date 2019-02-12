#import dataset
data<-read.csv(file="kc_house_data.csv", header=TRUE, sep=",")

#Question-1
# (b)

#simple random samples
sampling_fraction = 0.75
sample_index = sample(nrow(data), round(sampling_fraction * nrow(data)))
sample_df = data[sample_index,]


#systematic sampling
sys.sample= function(N,k){
  sys.samp = data[seq(1, nrow(data), k),]
  sys.samp <<- sys.samp
}
sys.sample(nrow(data),4)


#clustered sampling
install.packages("sampling")
library(sampling)
clusterSampling=cluster(data,clustername=c("bedrooms"),size=13*0.6,method="srswor")
clustered_data =getdata(data,clusterSampling)[c(-22,-23)]

#stratified sampling
install.packages("splitstackshape")
library(splitstackshape)
stratifiedSampling = stratified(data, c("floors"), 0.7)

#plots of different samples
hist(sample_df$price,xlab="Price",ylim=c(0,10000),breaks=20,right=FALSE,main = "Simple Random Sampling Histogram")

systemPrice = subset(sys.samp,select = c("price"))
hist(systemPrice$price,xlab="Price",breaks=20,ylim=c(0,3500),right=FALSE,main = "Systematic Sampling Histogram")

hist(clustered_data$price,xlab="Price",ylim=c(0,10000),breaks=20,right=FALSE,main = "Clustered Sampling Histogram")

stratifiedPrice = subset(stratifiedSampling,select = c("price"))
hist(stratifiedPrice$price,xlab="Price",breaks=20,ylim=c(0,11000),right=FALSE,main = "Stratified Sampling Histogram")

#Question-2

# (a) scaled grade
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 10
scaled_grade <- setNames(data.frame(rescale(data[['grade']])),c("Scaled_grade"))

# (b)
interested_data <- subset(data,select = c('price','bedrooms','sqft_living','sqft_lot','grade','sqft_above','sqft_basement'))
scaled_data <- data.frame(scale(interested_data))

hist(scaled_data$price,xlab="Price",xlim=c(-2,8),ylim=c(0,15000),breaks=20,right=FALSE,main = "Scaled Price Histogram")

hist(scaled_data$bedrooms,xlab="bedrooms",xlim=c(-4,8),ylim=c(0,15000),breaks=20,right=FALSE,main = "Scaled Bedrooms Histogram")

hist(scaled_data$sqft_living,xlab="sqft_living",xlim=c(-2,8),ylim=c(0,10000),breaks=20,right=FALSE,main = "Scaled Sqft_living Histogram")

hist(scaled_data$sqft_lot,xlab="sqft_lot",xlim=c(-2,10),ylim=c(0,20000),breaks=20,right=FALSE,main = "Scaled Sqft_lot Histogram")

hist(scaled_data$grade,xlab="Price",ylim=c(0,15000),breaks=20,right=FALSE,main = "Scaled Grade Histogram")

hist(scaled_data$sqft_above,xlab="sqft_above",xlim=c(-2,8),ylim=c(0,15000),breaks=20,right=FALSE,main = "Scaled Sqft_above Histogram")

hist(scaled_data$sqft_basement,xlab="Price",xlim=c(-2,8),ylim=c(0,15000),breaks=20,right=FALSE,main = "Scaled Sqft_basement Histogram")

# (c) Skewness and Kurtosis
install.packages("moments")
library(moments)

print("Skewness")
print(apply(scaled_data,2,skewness))
print("Kurtosis")
print(apply(scaled_data,2,kurtosis))


# (d) mean distances
print("Euclidean Distance between original attributes")
print(mean(data$sqft_lot)-mean(data$sqft_living))
print(mean(data$sqft_lot)-mean(data$sqft_above))
print(mean(data$sqft_living)-mean(data$sqft_above))

print("Euclidean Distance between scaled attributes")
print(mean(scaled_data$sqft_lot)-mean(scaled_data$sqft_living))
print(mean(scaled_data$sqft_lot)-mean(scaled_data$sqft_above))
print(mean(scaled_data$sqft_living)-mean(scaled_data$sqft_above))


#Question-3

#(a) correlation matrix and plot to visualize
cor_mat = cor(data[,-2])
print(cor_mat)
install.packages("corrplot")
library(corrplot)
corrplot(cor_mat, method="circle")

# (b) variance of  the  attributes  in  the  dataset
print(sort(apply(data[,-2],2,sd)**2))
print("5 attributes with lowest SD")
print(sort(apply(data[,-2],2,sd))[1:5])


# (c)
# c(i) Principal Component Analysis 
PCA_data <- princomp(scale(data[,-2],scale=FALSE) ,cor=TRUE)
var <- setNames(data.frame(PCA_data$sdev**2),c("Var"))
rownames(var)<-c()
summary(PCA_data)

print("variances of 2 PC's")
print(var[c(1,2),])

print("score")
percent_var <- var*100/sum(var)
#extent to which 2 principal components explain the dataset
print(sum(percent_var[c(1,2),]))

# c(ii) get original data using reverse PCA
reconstructed_data= t(t(PCA_data$scores %*% t(PCA_data$loadings))* PCA_data$scale + PCA_data$center)
summary(reconstructed_data)

