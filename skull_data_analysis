library(ggplot2)
library(tidyverse)
library(ggdendro)
library(cluster)
library(corrplot)
library(SciViews)
library(MASS)
library(klaR)

Worldskulls_edit <- read.csv("C:/Users/moharahman/Downloads/Worldskulls_edit (2).csv")
View(Worldskulls_edit)

head(Worldskulls_edit)     
summary(Worldskulls_edit)


#A: means & correlation
skull_means <- colMeans(Worldskulls_edit[3:8], na.rm = T)
round(skull_means, digits = 3)

skull_corr <- cor(Worldskulls_edit[3:8], use = "complete.obs")
round(skull_corr, digits = 3)
corrplot(skull_corr, method="color")


#B: means & correlations for each group
kenya_means <- colMeans(Worldskulls_edit[Worldskulls_edit[,2]=="Kenya", 3:8], na.rm = T)
round(kenya_means, digits = 3)

brazil_means <- colMeans(Worldskulls_edit[Worldskulls_edit[,2]=="Brazil", 3:8], na.rm = T)
round(brazil_means, digits = 3)

kenya_corr <- cor(Worldskulls_edit[Worldskulls_edit[,2]=="Kenya", 3:8], use = "complete.obs")
round(kenya_corr, digits = 3)
corrplot(kenya_corr, method = "color")

brazil_corr <- cor(Worldskulls_edit[Worldskulls_edit[,2]=="Brazil", 3:8], use = "complete.obs")
round(brazil_corr, digits = 3)
corrplot(brazil_corr, method = "color")


#C: Hierarchical Clustering for the variables 
skull_corr
dis_skull_corr <- 1- skull_corr 
dis_skull_corr   # shouldn't the correlation (r) be between -1 and 1? 

as_dis_skull_corr <- as.dist(dis_skull_corr)
skull_clust <- hclust(as_dis_skull_corr, method= "average")
skull_clust
plot(skull_clust) #or,
ggdendrogram(skull_clust, rotate = F, size=10)

plot(hclust(as.dist(1-kenya_corr), method= "average")) #for Kenya
plot(hclust(as.dist(1-brazil_corr), method= "average")) #for Brazil

#D: H. clustering for the observations  
new_dist <- dist(Worldskulls_edit)
new_clust <- hclust(new_dist, method = "average")
plot(new_clust) #or, 
ggdendrogram(new_clust, rotate = F, size=20)

x <- cutree(new_clust, 2)
x
plot(silhouette(x,new_dist))

y <- cutree(new_clust, 3)
y
plot(silhouette(y,new_dist))


#E: K means for 4 groups of observations 
is.na(Worldskulls_edit)
skull_omit <- na.omit(Worldskulls_edit)
View(skull_omit)

w_skulls <- kmeans(skull_omit[3:8], 4, iter.max = 30, algorithm = "MacQueen")
w_skulls

w_skulls$cluster
w_skulls$size
w_skulls$iter
w_skulls$centers

wc <- w_skulls$cluster
slt <- silhouette(w_skulls$cluster,dist(wc))
plot(slt)

#F: Linear regression with all variables on weight

str(Worldskulls_edit)
plot(Worldskulls_edit)
attach(Worldskulls_edit)


hist(breadth)
hist(ln(breadth))

hist(length_u)
hist(ln(length_u))

hist(height)
hist(ln(height))

hist(iron)
hist(ln(iron))

hist(density)
hist(ln(density))

hist(weight)
hist(ln(weight))

skull_weight_len <- lm(weight~breadth+length_u+height+iron+density)
skull_weight_len
summary(skull_weight_len)
plot(skull_weight_len)

len_log <- lm(weight~breadth+length_u+height+iron+density)
len_log
summary(len_log)
plot(len_log)

# why error?
# skull_country_len <- lm(country~breadth+length_u+height+iron+density+breadth, data = skull_omit)


#E: LDA
#how to determine ind & depn variable? Can it be 2+ independent variables? 
lda_skull <- lda(country~iron+density, data=skull_omit) 
lda_skull

#Predicting classification
Skull_class <- predict(lda_skull, method="plug-in")$class
Skull_class

#confusion matrix to see the accuracy of the prediction
attach(skull_omit)
table(country,Skull_class)


#APER (apparent error rate)

aper <- (16+15)/(18+15+16+17)
aper

#Predicting new point

new_point <- data.frame(iron=100, density=120)
predict(lda_skull, new_point)

ggplot(data=skull_omit, aes(x=iron, y=density, color=country))+
  geom_point(size=3)

ggplot(data=skull_omit, aes(x=iron, y=density, color=country))+
  geom_point(size=3)+
  geom_point(aes(x=100, y=120, color="new_point", size=5))+
  labs(title = "LDA plot",
       x="Iron",
       y="Density",
       caption = "R Practice Quiz 2")

#LDA with holdout method

lda_skull_hm <- lda(country~iron+density, data=skull_omit, cv=T) 
table(country, lda_skull_hm$class) #why error ????

new_aper <- 

#using partimat

f_country <- as.factor(skull_omit$country)
partimat(f_country ~ iron+density, method= "lda")



