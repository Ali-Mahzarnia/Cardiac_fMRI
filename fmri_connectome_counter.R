
library(readxl)
library(dplyr)


master_path = 'MasterSheet_Experiments2021.xlsx'
master = read_xlsx(master_path, sheet = "18ABB11_readable02.22.22_BJ_Cor")



# datatemp=data%>%dplyr::select(PTID , Sex, Age, Clinical.Dx, APOE)#subselect
datatemp = master[!is.na(master$ARunno),]

####plains
path_connec="/Users/ali/Desktop/Feb24/fmri_cardiac/fmri_FC/"
file_list=list.files(path_connec)
plain_index = grep("FC", file_list)
# sft_node_index = grep("sift_node", file_list)
# sft_index = grep("sift.csv", file_list)
# dst_index = grep("distances.csv", file_list)
# mean_FA_index = grep("mean_FA", file_list)

file_list = file_list[plain_index]
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
connectivity =connectivity
dim(connectivity)

notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  name= datatemp$ARunno[i]
  # if (is.na(name)) {name = datatemp$ID[i]}
  # name = gsub("-","", name)
  # if (nchar(name)!=8){ name = gsub('^(.{6})(.*)$', '\\10\\2', name)}
  temp_index=which(name==(substr(file_list,4,12)))
  if (length(temp_index)>0) 
  { #print(temp_index)
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
    temp_connec = temp_connec + t(temp_connec)
    temp_connec = matrix(rank(temp_connec), nrow(temp_connec) , ncol(temp_connec))
    print(isSymmetric(temp_connec))
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    #colnames(temp_connec)=NA
    connectivity[,,i]=as.matrix(temp_connec)
    #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
    #connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$ARunno[i])
  
}

notfound=notfound[2:length(notfound)]
not_found_index=which( datatemp$ARunno  %in%  notfound )

datatemp=datatemp[-not_found_index,]
connectivity=connectivity[,,-not_found_index]
sum(is.na(connectivity))
aaa = which(is.na(connectivity), arr.ind = T)
#connectivity=connectivity[,,-not_found_index]
#sum(is.na(connectivity))
connectivity[is.na(connectivity)]=0
 save(connectivity, file="connectivity_plain.rda")


# 
# 
# 








response=datatemp

# write.csv(file = "fmri_connectome_sheet.csv", response)
# response$agecat =response$Age_Months
# response$agecat[response$Age_Months>median(response$Age_Months)] ="OLD" 
# response$agecat[response$Age_Months<=median(response$Age_Months)] ="YOUNG" 

# table(response$Sex, response$Diet, response$agecat)

#
# RNA=read.delim('/Users/ali/Desktop/may23/risk/code/humanwithsymbol.txt',sep="\t", header = T)
# RNA=t(RNA)
# RNA_rows=rownames(RNA)
# 
# #RNA_rows= as.numeric(gsub("\\D", "", RNA_rows))
# #RNA_rows=RNA_rows[2:(length(RNA_rows)-1)]
# 
# RNA_data=RNA[2:(dim(RNA)[1]-1),]
# colnames(RNA_data)=RNA[dim(RNA)[1],]
# rownames(RNA_data)=RNA_rows[2:(length(RNA_rows)-1)]
# #rownames(RNA_data)=NULL
# sum(is.na(RNA_data))
# #RNA_data[!is.na(RNA_data)]=NA
# sum(is.na(RNA_data))
# 
# 
# #exist_response_ID=sub("\\s*\\:.*$", "",  response$Subject)
# #response$Subject=as.numeric(gsub("\\D", "", exist_response_ID ))
# #as.numeric(rownames(RNA_data))
# 
# 
# found=0
# notfound=0
# for (jj in 1:dim(RNA_data)[1]) {
# 
#   index=which( row.names( RNA_data)[jj]== response$ Subject )
#   if (length(index)>0){
#         found=c(found,index)
#   }
# 
#   else notfound=c(notfound,jj)
# 
#   }
# found=found[2:length(found)]
# 
# response=response[found,]
# dim(response)
# 
# # connectivity=connectivity[,,found]
# dim(connectivity)
# 
# notfound=notfound[2:length(notfound)]
# RNA_data=RNA_data[-notfound,]
# 
# dim(RNA_data)
# 
# rownames(RNA_data)=NULL


# 
# save(RNA_data, file="RNA_data.rda")
# save(connectivity, file="connectivity_plain.rda")
# 
save(response, file="response.rda")

library(reticulate)
np = import("numpy")
np$save("connectivity.npy",r_to_py(connectivity))
# 
# 

# 
# ###### sift
# file_list=list.files(path_connec)
# file_list = file_list[sft_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
#   
#   temp_index=which(datatemp$MRI_Exam[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0) 
#   { #print(temp_index)
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$MRI_Exam[i])
#   
# }
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_sift.rda")
# 
# 
# 
# 
# ###### sift_node
# file_list=list.files(path_connec)
# file_list = file_list[sft_node_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
#   
#   temp_index=which(datatemp$MRI_Exam[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0) 
#   { #print(temp_index)
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$MRI_Exam[i])
#   
# }
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_sift_node.rda")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# ###### dst_index
# file_list=list.files(path_connec)
# file_list = file_list[dst_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
# 
#   temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0)
#   {
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$DWI[i])
# 
# }
# 
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_dst.rda")
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# ###### mean_FA_index
# file_list=list.files(path_connec)
# file_list = file_list[mean_FA_index]
# temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# #temp_conn=temp_conn[,2: dim(temp_conn)[2]]
# connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
# connectivity =connectivity
# dim(connectivity)
# 
# notfound=0
# ##read connec
# for (i in 1:dim(connectivity)[3]) {
# 
#   temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
#   if (length(temp_index)>0)
#   {
#     temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F )
#     #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
#     #colnames(temp_connec)=NA
#     connectivity[,,i]=as.matrix(temp_connec)
#     cat ( sum( is.na(as.matrix(temp_connec)  )  ) , "\n")
#     #temp_connec = temp_connec /sum(temp_connec[lower.tri(temp_connec, diag=FALSE)])
#     #connectivity[,,i]=as.matrix(temp_connec)
#   }
#   else
#     notfound=c(notfound, datatemp$DWI[i])
# 
# }
# 
# #notfound=notfound[2:length(notfound)]
# #not_found_index=which( datatemp$DWI  %in%  notfound )
# 
# #datatemp=datatemp[-not_found_index,]
# #connectivity=connectivity[,,-not_found_index]
# #sum(is.na(connectivity))
# 
# #connectivity=connectivity[,,-not_found_index]
# sum(is.na(connectivity))
# dim(connectivity)
# # connectivity=connectivity[,,found]
# connectivity [is.na(connectivity)] = 0
# dim(connectivity)
# #response=datatemp
# #save(response, file="response.rda")
# save(connectivity, file="connectivity_mean_FA_index.rda")
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# creating a boxplot
library(ggplot2)
p = ggplot(response, aes(x = Clinical.Dx, y = as.numeric(Age), fill = as.factor(Clinical.Dx))) +
  geom_violin(alpha = 0.5) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5)  
ggsave( filename = paste0('age.PNG' )  , p  )

by(as.numeric(response$Age),as.factor(response$Clinical.Dx), sd)
