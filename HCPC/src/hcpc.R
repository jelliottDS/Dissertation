#Required packages FactoMineR, factoextra, dplyr,plyr, ggpubr

files = list.files(path="./data", pattern= "*data.csv", full.names = TRUE)

for (i in 1:length(files)) {
  
  filename=files[i]
  month= substr(files[i], start = 8, 10)
  print(month)
  savefile= substr(files[i], start = 8, stop = nchar(files[i]))
  print(savefile)
  
  
  data= read.csv(files[i])
  location= data$city
  rownames(data) = location
  data = data[2:7]
  cols=c("T", "FOW", "NaCl", "P", "UVR", "TOW")
  colnames(data)=cols
  
  #pca on data
  res.pca = prcomp(data, retx=TRUE, center=TRUE, scale=FALSE)
  
  res.pca2= PCA(data, scale.unit = FALSE, graph = TRUE)
  #% explained variance of each component
  expl.var <- round(res.pca$sdev^2/sum(res.pca$sdev^2)*100)
  #visualise explained variance 
  scree= fviz_screeplot(res.pca,
                 repel = TRUE,            # Avoid label overlapping
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "jco",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 #title = paste(month, "Original Scree Plot", sep=" ")
  )
  ggpar(scree,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_scree.png", sep="")))
  # 
  # #scatter plot cities pca
  # fviz_pca_ind(res.pca,
  #              #col.ind = "cos2", # Color by the quality of representation
  #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #              repel = TRUE,     # Avoid text overlapping,
  #              title = paste(month, "Original Scatter Plot", sep=" "),
  #              
  # )
  # ggsave(file.path('graphs', filename=paste(month, "_scatter.pdf", sep="")))
  
  #calculate the best number of clusters by ward linkage
  number.cluster <- NbClust(res.pca$x, distance = "euclidean", min.nc = 1, max.nc = 7, method = "average", index = "ch")
  #print(number.cluster$All.index)
  best.cluster <- as.numeric(number.cluster$Best.nc[1])
  
  # Compute hierarchical clustering and cut into n clusters
  res.hcut <- hcut(res.pca$x, k = best.cluster)
  # Visualize
  dend= fviz_dend(res.hcut,
            cex = 0.7,                     # Label size
            palette = "jco",               # Color palette see ?ggpubr::ggpar
            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
            rect_border = "jco",           # Rectangle color
            labels_track_height = 0.3,      # Augment the room for labels
            main= paste(month, "Original Dendrogram", sep=" "),
            #horiz-TRUE,
            lower_rect= -0.3
  )
  ggpar(dend,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_dend.png", sep="")))
  
  #create matrix of cluster id for each location
  clust_id= res.hcut$cluster
  assign(paste0(month, "_clustID"), clust_id)
  
  
  #fviz_pca_biplot(res.pca, repel = TRUE)
  
  #compute PAM clustering and visualise 
  res.pam <- pam(res.pca$x, 
                 best.cluster,
                 diss=FALSE,
                 stand=FALSE)
  #create matrix of cluster id for each location
  clust_id_pam= res.pam$clustering
  assign(paste0(month, "_clustID_pam"), clust_id)
  
  clust= fviz_cluster(res.pam,
                      stand= FALSE,
                      data = data,
                      repel = TRUE,            # Avoid label overlapping
                      show.clust.cent = TRUE, # Show cluster centers
                      palette = "jco",         # Color palette see ?ggpubr::ggpar
                      ggtheme = theme_minimal(),
                      main = paste(month, "PAM Clustering", sep=" "),
                      labels_track_height = 0.3,
                      lower_rect= -0.3
                      
)
  ggpar(clust,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_cluster.png", sep="")))
  assign(paste0(month, "_clust"), clust)

# fviz_pca_var(res.pca,
#              repel = TRUE,            # Avoid label overlapping
#              show.clust.cent = TRUE, # Show cluster centers
#              palette = "jco",         # Color palette see ?ggpubr::ggpar
#              ggtheme = theme_minimal(),
#              title = paste(month, "Original Variable Direction", sep=" ")
# )
# ggsave(file.path('graphs', filename=paste(month, "_var_direction.pdf", sep="")))
# 
pc1= fviz_contrib(res.pca,
             choice = "var",
             axes = 1,
             top = 10,
             palette = "jco",
             axis.text= 12,
             title= NULL,
             ggtheme = theme_minimal(),
             )

ggpar(pc1,
      font.main = 0,
      font.y = c(20, "red"),
      font.tickslab = c(20, "bold", "red"))

ggsave(file.path('graphs', filename=paste(month, "_pc1.png", sep="")))

pc2= fviz_contrib(res.pca,
             choice = "var",
             axes = 2,
             top = 10,
             palette = "jco",
             title= NULL,
             ggtheme = theme_minimal(),
             )

ggpar(pc2,
      font.main = 0,
      font.y = c(20, "red"),
      font.tickslab = c(20, "bold", "red"))

ggsave(file.path('graphs', filename=paste(month, "_pc2.png", sep="")))
# 

}

# res.hcpc= HCPC(res.pca2, nb.clust = best.cluster, iter.max = 10, method = 'average', graph = TRUE, description=TRUE, consol = FALSE)
# 
# 
# cluster_vars=res.hcpc$desc.var$quanti
# for(i in 1:length(cluster_vars)){
#   df= data.frame(cluster_vars[[i]])
#   print(df)
#   capture.output(df, file = paste(names(cluster_vars)[i], paste(month, "_cluster_variables.csv", sep=""), sep = "_"))
# }


# vi_df = ldply (var_importance, data.frame)
# vi_df=subset(vi_df, select=c(".id", "Mean.in.category", "Overall.mean", "p.value"))
# vi_df=rename(vi_df, c(".id"="Cluster", "Mean.in.category"="Mean In Cluster", "Overall.mean" = "Overall Mean", "p.value"= "p Value"))
# vi_df =vi_df %>% mutate_at(vars("Mean In Cluster", "Overall Mean", "p Value"), funs(round(., 2)))
# # # 
# data.frame(matrix(unlist(var_importance), nrow=length(var_importance), byrow=T))
# res.hcpc$desc.axes$quanti
# capture.output(res.hcpc$desc.axes$quanti, file = "dim_importance jan.txt")
# # 
# # }
# # 




# Times in same cluster PAM
#create matrix of cluster id numbers
clust_matrix_pam= cbind(matrix(Jan_clustID_pam), matrix(Feb_clustID_pam), matrix(Mar_clustID_pam), matrix(Apr_clustID_pam), matrix(May_clustID_pam), matrix(Jun_clustID_pam), matrix(Jul_clustID_pam), matrix(Aug_clustID_pam), matrix(Sep_clustID_pam), matrix(Oct_clustID_pam), matrix(Nov_clustID_pam), matrix(Dec_clustID_pam))

#create blank matrix to assign match numbers to
match_matrix_pam= matrix(nrow = 17, ncol = 17)
rownames(match_matrix_pam) = location
colnames(match_matrix_pam)= location

#for loop comparing all rows of matrix and counting matches
for (i in 1:nrow(clust_matrix_pam)) for(j in 1:nrow(clust_matrix_pam)){
  matches= length(which(clust_matrix_pam[i,]== clust_matrix_pam[j,]))
  match_matrix_pam[i,j]= matches 
} 
png(file = "./graphs/match_matrix_ffp_pam.png")
corrplot(match_matrix_pam, is.corr=FALSE, method='square', order='alphabet', diag=FALSE, type='lower', number.digits = 0,col.lim=c(0,12), col=colorRampPalette(c("white","red"))(200), tl.col = "blue", cl.length = 7, title= "Number of months two locations share the same cluster", mar=c(1, 1, 1, 1))
dev.off() 

