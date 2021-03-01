#AMALIA GEORGOUDI 3006
#---- READ DATA----
GroceriesInitial <- read.csv("~/GroceriesInitial.csv", header=FALSE)
attach(GroceriesInitial)
str(GroceriesInitial)
summary(GroceriesInitial)

#----CREATE THE BINARY FORM OF THE DATA----
product_names <- levels(unlist(GroceriesInitial[,4:35])) 
blank <- which(product_names == "")
product_names <- product_names[-c(blank)]
print(product_names)
products <- as.data.frame(t(apply(GroceriesInitial[,4:35],1, function(x) (product_names) %in% as.character(unlist(x)))))
names(products) <- product_names 
groceries_binary <- cbind(GroceriesInitial[,1:3],products)
str(groceries_binary)
summary(groceries_binary)
View(groceries_binary)

#---- INSTALL ----
if (!require('dplyr')) install.packages('dplyr')
library('dplyr')
library(tidyverse)
if (!require('arules')) install.packages('arules') 
library(arules)
if (!require('arulesViz')) install.packages("arulesViz") 
library(arulesViz)
if (!require('factoextra')) install.packages("factoextra")
library(factoextra)
library(ggplot2)
if (!require("RColorBrewer")) install.packages("RColorBrewer")
library("RColorBrewer")
coul <- brewer.pal(6, "Set2")

# ---- GROCERIES ----
groceries= data.frame('citrus fruit'= groceries_binary$`citrus fruit`, ' tropical fruit' = groceries_binary$`tropical fruit`,' whole milk'=groceries_binary$`whole milk`,' other vegetables'=groceries_binary$`other vegetables`,'rolls/buns'=groceries_binary$`rolls/buns`, chocolate=groceries_binary$chocolate,'bottled water'=groceries_binary$`bottled water`,yogurt=groceries_binary$yogurt,sausage=groceries_binary$sausage,' root vegetables'=groceries_binary$`root vegetables`, pastry=groceries_binary$pastry,soda=groceries_binary$soda,cream=groceries_binary$cream)
summary(groceries)

#RENAME THE  COLUMNS AT GROCERIES
names(groceries)[names(groceries) == "X.tropical.fruit"] <- "tropical.fruit"
names(groceries)[names(groceries) == "X.whole.milk"] <- "whole.milk"
names(groceries)[names(groceries) == "X.other.vegetables"] <- "other.vegetables"
names(groceries)[names(groceries) == "X.root.vegetables"] <- "root.vegetables"

View(groceries)
# ---- DF1 ----
df1=data.frame(	id=groceries_binary$V1, basket_value=groceries_binary$V2 , 	recency_days=groceries_binary$V3)

# ---- CBIND ----
groceries2<-cbind(groceries,df1)

#---- DELETE INVALID ROWS----
groceries3<-groceries2[-c(1),]
View(groceries3)

#---- CREATE VALUE_OF_BASKET----
groceriesfinal<-groceries3
str(groceriesfinal$basket_value)
groceriesfinal$basket_value<-as.numeric(as.character(groceries3$basket_value))
cut_points <- quantile(groceriesfinal$basket_value ,probs = c(0,0.33, 0.66, 1) ,na.rm = TRUE ,names = FALSE)
groceriesfinal$value_of_basket <- cut(groceriesfinal$basket_value ,breaks = cut_points,labels=c("Low basket value","Medium basket value","High basket value"),include.lowest = TRUE)
table(groceriesfinal$value_of_basket)
View(groceriesfinal)
head(groceriesfinal)

#plot for basket value
hist(groceriesfinal$basket_value,xlab="basket value")
ggplot(groceriesfinal, aes(basket_value)) + geom_histogram()
ggplot(groceriesfinal, aes(basket_value)) + geom_histogram(binwidth = 0.05)
#KOINI KATANOMI
p <- ggplot(groceriesfinal, aes(x=recency_days, y=basket_value)) + geom_point(shape=3)
print(p)

#---- PRODUCTS/BASKET_VALUE BOXPLOT----
boxplot(basket_value~chocolate, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Chocolate", xlab="Chocolate" ,ylab="Basket Value")
boxplot(basket_value~pastry, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Pastry", xlab="pastry" ,ylab="Basket Value")
boxplot(basket_value~citrus.fruit, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying citrus fruit", xlab="citrus.fruit" ,ylab="Basket Value")
boxplot(basket_value~sausage, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Sausage", xlab="sausage" ,ylab="Basket Value")
boxplot(basket_value~soda, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Soda", xlab="soda" ,ylab="Basket Value")
boxplot(basket_value~cream, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying cream", xlab="cream" ,ylab="Basket Value")
boxplot(basket_value~whole.milk, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying whole milk", xlab="whole.milk" ,ylab="Basket Value")
boxplot(basket_value~bottled.water, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying bottled water", xlab="bottled water" ,ylab="Basket Value")
boxplot(basket_value~yogurt, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying yogurt", xlab="yogurt" ,ylab="Basket Value")
boxplot(basket_value~other.vegetables, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying other vegetables", xlab="other vegetables" ,ylab="Basket Value")
boxplot(basket_value~root.vegetables, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying root vegetables", xlab="root vegetables" ,ylab="Basket Value")
boxplot(basket_value~tropical.fruit, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying tropical fruit", xlab="tropical fruit" ,ylab="Basket Value")
boxplot(basket_value~rolls.buns, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying rolls.buns", xlab="rolls.buns" ,ylab="Basket Value")

#---- PRODUCTS/RECENCY_DAYS BOXPLOT----
groceriesfinal$recency_days<-as.numeric(groceriesfinal$recency_days)

boxplot(recency_days~chocolate, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Chocolate", xlab="Chocolate" ,ylab="Recency_days")
boxplot(recency_days~pastry, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Pastry", xlab="pastry" ,ylab="Recency_days")
boxplot(recency_days~citrus.fruit, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying citrus fruit", xlab="citrus.fruit" ,ylab="Recency_days")
boxplot(recency_days~sausage, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Sausage", xlab="sausage" ,ylab="Recency_days")
boxplot(recency_days~soda, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying Soda", xlab="soda" ,ylab="Recency_days")
boxplot(recency_days~cream, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying cream", xlab="cream" ,ylab="Recency_days")
boxplot(recency_days~whole.milk, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying whole milk", xlab="whole.milk" ,ylab="Recency_days")
boxplot(recency_days~bottled.water, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying bottled water", xlab="bottled water" ,ylab="Recency_days")
boxplot(recency_days~yogurt, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying yogurt", xlab="yogurt" ,ylab="Recency_days")
boxplot(recency_days~other.vegetables, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying other vegetables", xlab="other vegetables" ,ylab="Recency_days")
boxplot(recency_days~root.vegetables, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying root vegetables", xlab="root vegetables" ,ylab="Recency_days")
boxplot(recency_days~tropical.fruit, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying tropical fruit", xlab="tropical fruit" ,ylab="Recency_days")
boxplot(recency_days~rolls.buns, data=groceriesfinal, notch=TRUE ,col=coul,main="Buying rolls.buns", xlab="rolls.buns" ,ylab="Recency_days")


#----EXERCISE 2----
rules <- apriori(groceries[,1:ncol(groceries)] ,parameter = list(minlen=2, supp=0.001, conf=1) ,control = list(verbose=TRUE))

#---- APRIORI RULE FOR PRODUCTS----
rules <- apriori(groceries[,1:ncol(groceries)] ,parameter = list(minlen=2, supp=0.001) ,control = list(verbose=FALSE))
rules_conf<- sort (rules, by='confidence', decreasing=TRUE)
inspect(rules_conf)
#PLOT
plot(rules[1:20], method="graph", control=list(type="items"))
plot(rules[1:20], method="paracoord", control=list(reorder=TRUE))
plot(rules, method="grouped")

#---- FUNCTION BINARIZE----
binarize <-function(data_columns,extra_columns=NULL){
  column_names <- levels(unlist(data_columns)) 
  blank <- which(column_names == "") 
  if (length(blank) !=0) column_names <- column_names[-c(blank)]
  binary_result <- as.data.frame(t(apply(data_columns,1 ,function(x) column_names %in% as.character(unlist(x))))) 
  names(binary_result) <- column_names 
  if (is.null(extra_columns)==FALSE) binary_result<- cbind(extra_columns,binary_result) 
  return(binary_result)
}
#----CREATE THE 3 COLUMNS OF HIGH, MEDIUM, LOW BASKET VALUE----
groceriesfinal <-binarize(as.data.frame(groceriesfinal$value_of_basket),groceriesfinal)
View(groceriesfinal)

#---- CREATE A NEW DATA FRAME WITH ONLY PRODUCTS AND THE HIGH, MEDIUM, LOW COLUMNS----
basketvalue=data.frame( 'High basket value'=groceriesfinal$`High basket value` , 'Medium basket value'=groceriesfinal$`Medium basket value`,'Low basket value'=groceriesfinal$`Low basket value`)
View(basketvalue)
groceries<-groceries[-c(1),]
groceries_with_values<-cbind(groceries,basketvalue)
View(groceries_with_values)

#----APRIORI RULE FOR PRODUCTS AND VALUE----
rules <- apriori(groceries_with_values[,1:ncol(groceries_with_values)] ,parameter = list(minlen=2, supp=0.0175) ,control = list(verbose=FALSE))
rules_conf<- sort (rules, by='confidence', decreasing=TRUE)
inspect(rules_conf)
# PLOT
plot(rules, method="grouped")
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

#----EXERCISE 3----
groceriesbr=data.frame( basket_value=groceriesfinal$basket_value , 	recency_days=groceriesfinal$recency_days)
groceriescale=data.frame( basket_value=groceriesfinal$basket_value , 	recency_days=groceriesfinal$recency_days)
groceriescale<- na.omit(groceriescale)

groceriescale$basket_value<-scale(groceriescale$basket_value)
groceriescale$recency_days<-scale(groceriescale$recency_days)
View(groceriescale)

summary(groceriescale)

#---- K-MEANS CLUSTERING----
set.seed(123)
km.res <- kmeans(groceriescale, 5, nstart = 25)
print(km.res)
str(km.res)

fviz_cluster(km.res, data = groceriescale)

km.res$size
km.res$centers
km.res$withinss

groceriesbr %>%
  mutate(Cluster = km.res$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

str(km.res$cluster)
pie_data <- table(km.res$cluster) 
pie(pie_data,labels = paste(names(pie_data), "\n", pie_data, sep=""))

#---- 3C ----
#---- CREATE A COLUMN FOR CLUSTER----
grocerieswithclusters = cbind(groceriesfinal, cluster = km.res$cluster)

# CONVERT CLUSTER COLUMN FROM NUMERIC TO CHARACTER
grocerieswithclusters$cluster<-as.character(grocerieswithclusters$cluster)
str(grocerieswithclusters$cluster)

#CALL BINARIZE FUCTION TO CREATE THE NEW COLUMNS AND CONVERT TO BINARY
grocerieswithclusters<-binarize(as.data.frame(grocerieswithclusters$cluster),grocerieswithclusters)

#RENAME THE NEW COLUMNS
names(grocerieswithclusters)[names(grocerieswithclusters) == "1"] <- "cluster0"
names(grocerieswithclusters)[names(grocerieswithclusters) == "2"] <- "cluster1"
names(grocerieswithclusters)[names(grocerieswithclusters) == "3"] <- "cluster2"
names(grocerieswithclusters)[names(grocerieswithclusters) == "4"] <- "cluster3"
names(grocerieswithclusters)[names(grocerieswithclusters) == "5"] <- "cluster4"

View(grocerieswithclusters)
head(grocerieswithclusters)

#---- 4 -----

#A data frame only with the 5 clusters
clusters=data.frame(cluster0=grocerieswithclusters$cluster0,cluster1=grocerieswithclusters$cluster1,cluster2=grocerieswithclusters$cluster2,
                    cluster3=grocerieswithclusters$cluster3,cluster4=grocerieswithclusters$cluster4)

#A data frame with the products and the clusters
groceries_clustered<-cbind(groceries,clusters)
View(groceries_clustered)
summary(groceries_clustered)

#---- APRIORI RULE FOR PRODUCTS AND CLUSTERS----
rules_c <- apriori(groceries_clustered[,1:ncol(groceries_clustered)] ,parameter = list(minlen=2, supp=0.001) ,control = list(verbose=FALSE))
rules_cconf<- sort (rules_c, by='confidence', decreasing=TRUE)
inspect(rules_cconf[1:22])

#PLOT
plot(rules_c[1:20], method="graph", control=list(type="items"))
plot(rules_c[1:20], method="paracoord", control=list(reorder=TRUE))
plot(rules_c[1:20], method="grouped")
plot(rules_c[1:20], method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL)

#---- Table with results apriori----
inspectDT(rules_c[1:24])