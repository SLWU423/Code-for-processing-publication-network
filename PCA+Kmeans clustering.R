library(readxl)
library(tidyverse) #data manipulation
library(cluster) #clustering algorithms
library(factoextra) #clustering visualization
library(dendextend) #compare twd dendrograms
library(ggpubr)
library(FactoMineR)
library(corrplot)


setwd("")

#create data frame for first period 1900-1987
df1 <- read_excel("", col_names = TRUE)
mat1<- as.matrix(df1)
rownames(mat1)<- mat1[,1] 
revmat1<- mat1[,-1]
mode(revmat1)<- "numeric"

#create data frame for second period 1988-2007
df2 <- read_excel("", col_names = TRUE)
mat2<- as.matrix(df2)
rownames(mat2)<- mat2[,1] 
revmat2<- mat2[,-1]
mode(revmat2)<- "numeric"

#create data frame for third period 2008-2017
df3 <- read_excel("", col_names = TRUE)
mat3<- as.matrix(df3)
rownames(mat3)<- mat3[,1] 
revmat3<- mat3[,-1]
mode(revmat3)<- "numeric"



set.seed(1333)
######Principal component analysis######
#https://www.datacamp.com/community/tutorials/pca-analysis-r
res.pca1<- PCA(revmat1, graph = FALSE)
res.pca2<- PCA(revmat2, graph = FALSE)
res.pca3<- PCA(revmat3, graph = FALSE)

#get the contributions etc. from pca results
##https://rpkgs.datanovia.com/factoextra/index.html
var1<- get_pca_var(res.pca1)
var2<- get_pca_var(res.pca2)
var3<- get_pca_var(res.pca3)

fviz_pca_var(res.pca1, col.var = "contrib", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_var(res.pca2, col.var = "contrib", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_var(res.pca3, col.var = "contrib", repel = TRUE, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
dev.off()

fviz_pca_ind(res.pca1, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_ind(res.pca2, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_ind(res.pca3, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

contri1<- var1$contrib
contri2<- var2$contrib
contri3<- var3$contrib
comb.cont<- cbind(contri1, contri2, contri3)
write.csv(comb.cont, file = "")

#Get the eigenvalues for each PC
eg1<- get_eig(res.pca1)
eg2<- get_eig(res.pca2)
eg3<- get_eig(res.pca3)
comb.eg<- cbind(eg1, eg2, eg3)
write.csv(comb.eg, file = "Eigenvalues in 3 periods_COV-DEN.csv")


#extract the coordinates of the principal components
pca1<- prcomp(revmat1, scale=T, center = T)
coord1<- pca1$x
write.csv(coord1, file = "")

pca2<- prcomp(revmat2, scale=T, center = T)
coord2<- pca2$x
write.csv(coord2, file = "")

pca3<- prcomp(revmat3, scale=T, center = T)
coord3<- pca3$x
write.csv(coord3, file = "")


#determine the contributions of the original variables to the principal components
pdf(file = "")
par(mfrow=c(3,1))
par(mar=c(8,2,2,2), new=TRUE)
corrplot(res.pca1$var$contrib, add = FALSE, is.corr = FALSE, method="circle", col=colorRampPalette(c("gray100", "palegreen", "palegreen4"))(20), cl.lim=c(0,60), tl.col="black", tl.cex = 0.8, cl.cex=1, cl.align.text = "l")
corrplot(res.pca2$var$contrib, add = FALSE, is.corr = FALSE, method="circle", col=colorRampPalette(c("gray100", "palegreen", "palegreen4"))(20), cl.lim=c(0,70), tl.col="black", tl.cex = 0.8, cl.cex=1, cl.align.text = "l")
corrplot(res.pca3$var$contrib, add = FALSE, is.corr = FALSE, method="circle", col=colorRampPalette(c("gray100", "palegreen", "palegreen4"))(20), cl.lim=c(0,80), tl.col="black", tl.cex = 0.8, cl.cex=1, cl.align.text = "l")
dev.off()

fviz_pca_var(res.pca1, col.var = "contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), ggtheme = classic2(), labelsize = 6)

#contributions of variables to PC1
fviz_contrib(res.pca1, choice="var", axes=1)
#contribitions to PC2
fviz_contrib(res.pca1, choice="var", axes=2)
#total contribuitons to PC1 and PC2
pcaf1<- fviz_contrib(res.pca1, choice="var", axes=1:2, ggtheme = theme_gray())
pcaf2<- fviz_contrib(res.pca2, choice="var", axes=1:2, ggtheme = theme_gray())
pcaf3<- fviz_contrib(res.pca3, choice="var", axes=1:2, ggtheme = theme_gray())
fig.pca<- ggarrange(pcaf1, pcaf2, pcaf3, ncol=1, nrow = 3)
ggexport(fig.pca, filename = "", width = 5, height=15)
dev.off()



######k-means clustering######
#Determine optimum number of k-means clusters
#Elbow method: minimise within-cluster sum of square
set.seed(2019)
#function to compute total wihitn-cluster sum of square
wss<- function(k){
  kmeans(revmat3, k, nstart = 10)$tot.withinss
}
#compute and plot wss for k=1 to k=15
k.values<- 1:9
#extract wss for 2-15 clusters
wss_values<- map_dbl(k.values, wss)
plot(k.values, wss_values, 
     type = "b", pch=19, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#OR: in a single function
set.seed(2019)
fviz_nbclust(revmat3, kmeans, method = "wss")


#Average Silhouette Method: measure the quality of a clustering
#a high average silhouette width indicates a good clustering
#the optimal number of clusters k is the one that maximizes the average silhouette over a range of possible k
set.seed(2019)
fviz_nbclust(revmat3, kmeans, method = "silhouette")


#Gap Statistic Method: compares the total intra-cluster variation for different k with their expected values under null reference distribution of the data
#i.e. a distribution with no obvious clustering
#the reference dataset is generated using Monte Carlo simulations of the sampling process
#can be applied to any clustering method (kmeans, hierarchical)
set.seed(2019)
gap_stat<- clusGap(revmat3, FUN=kmeans, nstart=25, K.max = 10, B=50)
#print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)



#######Implement k-means clustering######
set.seed(201999)  #111 for disciplines
k1<- kmeans(revmat1, centers = 3, nstart = 30)
x1<- k1$cluster
write.csv(x1, file = "")

k2<- kmeans(revmat2, centers = 3, nstart = 25)
x2<- k2$cluster
write.csv(x2, file = "")

k3<- kmeans(revmat3, centers = 3, nstart = 25)
x3<- k3$cluster
write.csv(x3, file = "")

comb.river<- cbind(x1, x2, x3)
write.csv(comb.river, file = "")


#Visualization
cluf1<- fviz_cluster(k1, data = revmat1,  labelsize = 20, main = "1900-1987", ggtheme = theme_gray()) +
  scale_color_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_fill_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_shape_manual("Cluster", values = c(22, 23, 24))

cluf2<- fviz_cluster(k2, data = revmat2, labelsize = 20, main = "1988-2007", ggtheme = theme_gray())+
  scale_color_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_fill_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_shape_manual("Cluster", values = c(22, 23, 24))

cluf3<- fviz_cluster(k3, data = revmat3,  labelsize = 20, main = "2008-2017", ggtheme = theme_gray())+
  scale_color_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_fill_manual("Cluster", values  = c("#2C4A52", "#537072", "#8E9B97")) +
  scale_shape_manual("Cluster", values = c(22, 23, 24))

figure<- ggarrange(cluf1, cluf2, cluf3, ncol=1, nrow = 3)
ggexport(figure, filename = "", width = 15, height=30)
dev.off()