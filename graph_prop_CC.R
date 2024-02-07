
library(readxl)
library(dplyr)
library(DirectedClustering)

load("connectivity_plain.rda")
load("response.rda")

# aaa= connectivity[,,1]
# isSymmetric(connectivity[,,1])
# which(connectivity[,,1] != t(connectivity[,,1]), arr.ind = TRUE)

response$CC = NA
for (i in 1:dim(connectivity)[3]) {
  if (isSymmetric(connectivity[,,i])){
  response$CC[i] = ClustF(connectivity[,,i])$GlobalCC
  }
  else {response$CC[i] = NA}
}

path_data='Full_Cardiac_Metrics_01082024_Exercise.csv'
data=read.csv(path_data )
data = as.data.frame(data)
data = as.data.frame(t(na.omit(t(data))))

results1 = aggregate(as.numeric(Ejection_Fraction) ~  Genotype + Diet , data = data, FUN= "mean" )
results2 = aggregate(as.numeric(CC) ~ Genotype + Diet , data = response, FUN= "mean" )

aaaa = results2[7,]
aaaa[,2] = "HFD"
results2 =  rbind(results2, aaaa)



 plot_data = cbind(results2$`as.numeric(CC)`, results1$`as.numeric(Ejection_Fraction)`, paste0(results1$Genotype,"_", results1$Diet))

 # plot(plot_data)
 
 library(gcookbook) # Load gcookbook for the countries data set
 
 p=ggplot(data=as.data.frame(plot_data), aes(x = as.numeric(plot_data[,1]), y = as.numeric(plot_data[,2]))) +
   geom_point()+   labs( x ="Clustering Coefitients" ,y = "Ejection Fraction"  )+theme(text=element_text(size=15))+
   geom_text(aes(label = plot_data[,3]), size = 4)+ theme_bw()
 ggsave( filename = 'CC_VS_EF_diet_genotype.PNG'  , p  )
 
