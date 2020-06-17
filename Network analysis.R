library(readxl)
library(dplyr)
library(igraph)
library(tidyverse)
library(splitstackshape)
library(data.table)


setwd("")

#network analysis by rivers
tab_names<- excel_sheets(path = ".xlsx")
lst<- lapply(tab_names, function(i) read_excel(".xlsx", sheet = i, col_names=c("From", "TO", "YEAR","3 Periods", "10-yr Periods","77-17 Periods", "WEIGHTS")))

#to split the list by 3 periods:1900-1987, 1988-2007, 2008-2017
split.byperiod<- function(df){
  g<- split(df, df$"3 Periods")
  return(g)
}
lst.byperiod<- lapply(lst, split.byperiod)


#network analysis when all rivers are combined
lst.merge <- read_excel(".xlsx", col_names = TRUE)
lst.merge.byyear<- split(lst.merge, lst.merge$"Year")


#function to calculate the required network measures
cal.measures<- function(df){
  deg<- degree(df)
  clos<- closeness(df)
  betw<- betweenness(df)
  clus<- transitivity(df, type = c("local"))
  
  deg.avg<- mean(degree(df))
  clos.avg<- mean(closeness(df))
  betw.avg<- mean(betweenness(df))
  clus.avg<- mean(transitivity(df, type = c("local")))
  
  measures<- cbind(deg, betw, clos, clus, deg.avg, clos.avg, betw.avg, clus.avg)
  
  return(measures)}


#function to transform the two-mode edglist to 1-mode igraph, and calculate the required network measures
get.1modegraph.kw<- function(df){
  
#need to separate overlapping kw and disp: ecology, remote sensing, management
  df[which(df[,1]=="Ecology"), 1]<- "Ecology-kw"
  df[which(df[,2]=="Ecology"), 2]<- "Ecology-dis"
  df[which(df[,1]=="remote sensing"), 1]<- "remote sensing-kw"
  df[which(df[,2]=="Remote Sensing"), 2]<- "Remote Sensing-dis"
  df[which(df[,1]=="Management"), 1]<- "Management-kw"
  df[which(df[,2]=="Management"), 2]<- "Management-dis"
  
  g<- graph_from_data_frame(df)
  g_el<- as_edgelist(g)
  colnames(g_el)<- c("kw", "disp")
  V(g)$type<- ifelse(V(g)$name %in% g_el[,"kw"], TRUE, FALSE)
  E(g)$weight<- E(g)$"WEIGHTS"
  
  gproj<- bipartite.projection(g, V(g)$type, multiplicity = TRUE)
  measures.kw<- cal.measures(gproj$proj2)
  measures.cat<- cal.measures(gproj$proj1)
  
  return(measures.kw)
}

get.1modegraph.cat<- function(df){
  
  df[which(df[,1]=="Ecology"), 1]<- "Ecology-kw"
  df[which(df[,2]=="Ecology"), 2]<- "Ecology-dis"
  df[which(df[,1]=="remote sensing"), 1]<- "remote sensing-kw"
  df[which(df[,2]=="Remote Sensing"), 2]<- "Remote Sensing-dis"
  df[which(df[,1]=="Management"), 1]<- "Management-kw"
  df[which(df[,2]=="Management"), 2]<- "Management-dis"
  
  g<- graph_from_data_frame(df, vertices = NULL)
  g_el<- as_edgelist(g)
  colnames(g_el)<- c("kw", "disp")
  V(g)$type<- ifelse(V(g)$name %in% g_el[,"kw"], TRUE, FALSE)
  E(g)$weight<- E(g)$"WEIGHTS"
  
  gproj<- bipartite.projection(g, V(g)$type, multiplicity = TRUE)
  measures.kw<- cal.measures(gproj$proj2)
  measures.cat<- cal.measures(gproj$proj1)
  
  return(measures.cat)
}


#Export results
for (i in 1:95){
  lst.byperiod[[i]]<- lapply(lst.byperiod[[i]], get.1modegraph.kw)
}
for (i in 1:95){
  lapply(lst.byperiod[[i]], function(x) write.table(data.frame(x),file= paste("1 mode-kw-by 87-07-17",i,".csv"), append= T, sep=',', row.names=TRUE, col.names = TRUE))
}

for (i in 1:95){
  lst.byperiod[[i]]<- lapply(lst.byperiod[[i]], get.1modegraph.cat)
}
for (i in 1:95){
  lapply(lst.byperiod[[i]], function(x) write.table(data.frame(x),file= paste("1 mode-cat-by 87-07-17",i,".csv"), append= T, sep=',', row.names=TRUE, col.names = TRUE))
}


lapply(lst.merge.kw, function(x) write.table(x,"1-mode all rivers by 87-07-17-kw.csv", append= T, sep=',', row.names=TRUE, col.names = TRUE))
lapply(lst.merge.cat, function(x) write.table(x,"1-mode all rivers by year-cat-AVG.csv", append= T, sep=',', row.names=TRUE, col.names = TRUE))



#Visualization: heatmap
col.1mode <- colorRampPalette(c("gold", "dark orange"))
plot.1mode<- function(df){
  g<- as_adjacency_matrix(df, attr = "weight", sparse = F)
  heatmap(g, Rowv = NA, Colv = NA, col = col.1mode(1000), scale = "none", margins = c(10,10), cex.lab=3)
}

png("cat1-2000.png", width = 1500, height = 1500)
plot.1mode(lst.byperiod[[1]]$`2000`)
dev.off()
