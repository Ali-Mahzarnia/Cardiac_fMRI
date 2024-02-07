library(ggplot2)
library(pROC)
library(grpnet)
library(glmnet)
library(xlsx)
library('igraph');

options(warn=-1)



v2m = function(v){
  x=length(v)
  n=(1+sqrt(1+8*x))/2
  mat <- matrix(0, nrow = n, ncol =  n)
  mat[upper.tri(mat, diag = FALSE)] <- v
  mat = mat + t(mat)
  return(mat)
}


m2v = function(m){
  x=dim(m)[1]
  n=(x-1)*x/2
  v <- vector("double", n)
  v = m[upper.tri(m, diag = FALSE)] 
  return(v)
}


library(R.matlab)
read_vec = readMat("vector.mat")

read_vec[1]


## edges 
edges = as.numeric(read_vec[[1]])
connectivitvals = v2m(edges)
##### 324 to 332 

chass324_path = "Chass324index2.csv"
chass324 = read.csv(chass324_path, header = F)
index2_1000 = chass324[chass324>166]
chass324[chass324>166] = index2_1000 - 1000 + 166
seq = seq(1,332)
index_nonexists_regions  = which(!seq %in% chass324$V1 )
### add these non zero 
for (i in 1:dim(connectivitvals)) {
  
  if (i%in% index_nonexists_regions){
    connectivitvals = cbind(connectivitvals[,1:(i-1)], rep(0 ,  dim(connectivitvals)[1]) , connectivitvals[,i:dim(connectivitvals)[2]])
    connectivitvals = rbind(connectivitvals[1:(i-1),], rep(0 ,  dim(connectivitvals)[2]) , connectivitvals[i:dim(connectivitvals)[1],])
  }
}

########




datalabels <- read.csv(paste0(input_path, "mouse_anatomy.csv"), header = TRUE, sep = ",", quote = "")

# Extract graph structure
Adjmat[lower.tri(Adjmat)] = 0
t <- which(Adjmat!=0, arr.ind=TRUE)
t <- cbind(t, Adjmat[which(Adjmat!=0,arr.ind=TRUE)]) 
t.graph <- graph.data.frame(t,directed=F)
E(t.graph)$color <- ifelse(E(t.graph)$V3 > 0,'blue','red')
minC <- rep(-Inf, vcount(t.graph))
maxC <- rep(Inf, vcount(t.graph))
minC[1] <- maxC[1] <- 0
l <- layout_with_fr(t.graph, minx=minC, maxx=maxC,
                    miny=minC, maxy=maxC)      

# Plot flattened network
par(mfrow=c(1,1))
jpeg("nets.jpeg", units="in", width=10, height=5, res=300)  
plot(t.graph, layout=l, 
     rescale=T,
     asp=0,
     edge.arrow.size=0.1, 
     vertex.label.cex=0.8, 
     vertex.label.family="Helvetica",
     vertex.label.font=4,
     vertex.shape="circle", 
     vertex.size=5, 
     vertex.color="white",
     vertex.label.color="black", 
     edge.width=as.integer(cut(abs(E(t.graph)$V3), breaks = 5)))
dev.off()

# Summarize connected components
Adjmat <- Adjmat+t(Adjmat)
subnets <- groups(components(t.graph))
subnetsresults <- vector(mode = "list", length = length(subnets))
colsumabs <- colSums(abs(Adjmat))
colsum <- colSums(Adjmat)
leftright <- datalabels$Hemisphere
for (i in 1:length(subnets)) {
  temp <- subnets[[i]]
  temp <- as.numeric(temp)
  net <- matrix(NA,8,length(temp) )
  net[1,] <- as.numeric(temp)
  net[2,] <- datalabels$ROI[temp]
  net[5,] <- leftright[temp]
  net[1,] <- paste(net[1,],net[5,])
  net[3,] <- as.numeric( colsum[temp]   )
  net[4,] <- as.numeric( colsumabs[temp]   )
  net[6,] <- sum(as.numeric(net[4,]))
  net[7,] <- sum(as.numeric(net[3,]))
  for (j in 1:length( net[8,])) {
    tempindex <- which(datalabels$ROI %in% net[2,j]  )
    if (net[5,j]=="Right" ) {net[8,j]= max(tempindex) } else { net[8,j]=min(tempindex) }
  }
  subnetsresults[[i]] <- net 
}
subnetsresults_sorted <- vector(mode = "list", length = length(subnets))
sorted_weights <- matrix(NA, 1, length(subnetsresults))
for (i in 1:length(subnetsresults)) {temp =subnetsresults[[i]]; sorted_weights[1,i] <- abs(as.numeric(temp[6,1]))   }
index_of_nets <- order(sorted_weights, decreasing = T)
for (i in 1:length(subnetsresults)) {subnetsresults_sorted[[i]] <- subnetsresults [[index_of_nets[i]]]}
subnetsresults <- subnetsresults_sorted

# Save connected component information to spreadsheet
net_new=matrix(NA, length(subnetsresults),5)
for (j in 1:dim(net_new)[1]) {
  temps=subnetsresults[[j]]
  net_new[j,1] = j
  net_new[j,2] = paste(temps[8,], collapse = ", ")
  net_new[j,3] = paste(paste(temps[5,],temps[2,]), collapse = ", ")
  net_new[j,4] = paste(temps[7,1])
  net_new[j,5] = paste(abs(as.numeric(temps[6,1])))
}
colnames(net_new)=c("Sub-Network", "Region Number", "Region Name", "Sub-Network Weight", "Sub-Network abs Weight")
write.xlsx2(net_new, "net_new.xlsx" )

# Plot glass brains
brainconn(atlas ="CHASS", conmat=Adjmat, 
          view="left", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=9, background.alpha=1, 
          label.edge.weight=F, background = "Chass")
ggsave("glassleft.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)

brainconn(atlas ="CHASS", conmat=Adjmat, 
          view="top", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=9, background.alpha=1, 
          label.edge.weight=F, background = "Chass")  
ggsave("glasstop.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)

brainconn(atlas ="CHASS", conmat=Adjmat, 
          view="front", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=9, background.alpha=1, 
          label.edge.weight=F, background = "Chass")  
ggsave("glassfront.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)