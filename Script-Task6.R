library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)
library(randomForest)
library(plotly)
library(data.table)

#import existing a new products dataset
existingproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 06 - Multiple Regression in R\\existingproductattributes2017.2.csv")
newproducts <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 06 - Multiple Regression in R\\newproductattributes2017.2.csv")

#first look to the existing products dataset
head(existingproducts)
str(existingproducts)
summary(existingproducts)
attributes(existingproducts)
table(existingproducts$ProductType)

#first look to the new products dataset
head(newproducts)
str(newproducts)
summary(newproducts)
table(newproducts$ProductType)

#remove the column BestSellerRank  - it has 15 missing values and the values makes no sense whatosever.
existingproducts$BestSellersRank <- NULL
newproducts$BestSellersRank <- NULL

#remove the column ProductNum  - because it's just an identifier.
existingproducts$ProductNum <- NULL
newproducts$ProductNum <- NULL

#random forest - to check meaninful realationships between variables
set.seed(123)
rf <- train(Volume ~ ., data = existingproducts, method = "rf", importance = TRUE)

#save the results of the weighting after the first randomforest
varweights <- varImp(rf) 
varweights

#extract the dataframe with the weights
dfweights <- data.frame(varweights$importance) 
dfweights

#linearmodel of x5StarsRating against Volume
lm <- lm(x5StarReviews ~ Volume, existingproducts)
summary(lm)
#In summary.lm(lm) : essentially perfect fit: summary may be unreliable#
#remove x5StarReviews#
existingproducts$x5StarReviews <- NULL
newproducts$x5StarReviews <- NULL

#run a second random forest without the variables removed to check the new weights
set.seed(123)
rf <- train(Volume ~ ., data = existingproducts, method = "rf", importance = TRUE)
varweights <- varImp(rf) 

#save the results of the weighting after the first randomforest
varweights <- varImp(rf) 
varweights

#extract the dataframe with the weights
dfweights <- data.frame(varweights$importance) 
dfweights

#order the index of each variable according the relevance - decreasing
indicesorden <- order(dfweights$Overall, decreasing = TRUE)

#remove the variables with less than 2% of appeareance
for (i in indicesorden){
  if (dfweights$Overall[i] > 101){
    deletevariable <- (rownames(dfweights)[i])
    n <- names(existingproducts)
    nameindex <- which(n == deletevariable)
    existingproducts[,nameindex] <- NULL
    
  } else if (dfweights$Overall[i] < 2){
    deletevariable <- (rownames(dfweights)[i])
    n <- names(existingproducts)
    nameindex <- which(n == deletevariable)
    existingproducts[,nameindex] <- NULL
  }
}

indicesorden

dfinicial <- data.frame(existingproducts$indices)
dfagregado

dfweights$Overall[17]
dfweights$Overall[13]
dfweights$Overall[15]



view(existingproducts)

names(existingproducts)



existingproducts$"x5StarReviews"

existingprod

deletevariable

dfweights



plyr::arrange(dfweights, desc(Overall) )



?arrange


dfweights[order(dfweights$Overall)]


dfweights




?order



sort(dfweights
rownames(dfweights)[1]

print(rf)



#desicion tree
set.seed(123)
rp <- rpart(Volume ~ .-x5StarReviews-ProfitMargin-ProductType, existingproducts)
summary(rp)
rpart.plot(rp, type = 1)





#END OF CODE
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################


#checking the data distribution# Basic histogram
for (i in 1:ncol(existingproducts)) { #each columns index
  if is.numeric(existingproducts[,i]){ #si la variable es numerica qqplot y histogram
    
    ggplot(existingproducts, aes(x=Volume)) + 
      geom_histogram(binwidth = 200, color="black", fill="white") + 
      geom_vline(aes(xintercept=mean(Volume)),color="red", linetype="dashed", size=1) +
      geom_density(alpha=.2, fill="#FF6666")
    
    
    
    
    ggplot(existingproducts, aes(x=Volume)) + 
      geom_histogram(binwidth = 200, color="black", fill="white") + 
      geom_vline(aes(xintercept=mean(Volume)),color="red", linetype="dashed", size=1) +
      geom_density(alpha=.2, fill="#FF6666") 
    
    
    # Change the width of bins
    ggplot(existingproducts, aes(x=volume)) + 
      geom_histogram(binwidth=1)
    # Change colors
    p<-ggplot(existingproducts, aes(x=volume)) + 
      geom_histogram(color="black", fill="white")
    p
    
    
    
    # Color and shape depend on factor (categorical variable)
    ggplot(surveydata, aes(x=age, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=age, y=credit, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=car, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=zipcode, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    ggplot(surveydata, aes(x=elevel, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)
    
    
    ggplot(surveydata, aes(x=salary)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=age)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=credit)) + 
      geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))
    
    ggplot(surveydata, aes(x=elevel)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    
    ggplot(surveydata, aes(x=car)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" ) 
    
    ggplot(surveydata, aes(x=zipcode)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    
    ggplot(surveydata, aes(x=brand)) + 
      geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )
    #checking the data with a decisiton tree and a randomforest
    
    #Since we are doing a regression and we have a categorical variable (ProductType) we have to "dummify the data" to make our factors/levels a numeric dummy.
    
    #ProductType is a factor with 12 levels
    str(existingproducts$ProductType)
    summary(existingproducts$ProductType)
    
    #create list selecting our categorical variables and make a new data.frame joining the dummy features with our existing product dataset. is a factor with 12 levels
    dummylist <- dummyVars("~ .",data = existingproducts)
    duexistingproducts <- data.frame(predict(dummylist, newdata = existingproducts))
    
    str(duexistingproducts)
    summary(duexistingproducts)
    
    
    #correlation matrix 
    
    
    corexisting_dummy <- cor(duexistingproducts) 
    
    corexisting_dummy
    corrplot(corexisting_dummy, method = "square", order="hclust", col = brewer.pal(n = 8, name = "RdYlBu"),cl.ratio = 0.2, cl.align = "r")
    