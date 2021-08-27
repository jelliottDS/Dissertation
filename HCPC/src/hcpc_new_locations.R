#read in data files that include data for suggested ETS locations

files = list.files(path="./data", pattern= "*_lowest_cities.csv", full.names = TRUE)

#function for clustering and creating visualisations
for (i in 1:length(files)) {
  
  #to enable naming of plots with month
  filename=files[i]
  month= substr(files[i], start = 8, 10)
  print(month)
  savefile= substr(files[i], start = 8, stop = nchar(files[i]))
  print(savefile)
  
  data= read.csv("./data/Nov_lowest_cities.csv")
  #format data set
  data= read.csv(files[i])
  location= data$city
  rownames(data) = location
  data = data[2:7]
  rownames(data) = location
  cols=c("T", "FOW", "NaCl", "P", "UVR", "TOW")
  colnames(data)=cols
  
  #pca on data
  res.pca = prcomp(data, retx=TRUE, center=TRUE, scale=FALSE)
  
  #% explained variance of each component
  expl.var <- round(res.pca$sdev^2/sum(res.pca$sdev^2)*100)
  #visualise explained variance 
  scree= fviz_screeplot(res.pca,
                 repel = TRUE,            # Avoid label overlapping
                 show.clust.cent = TRUE, # Show cluster centers
                 palette = "jco",         # Color palette see ?ggpubr::ggpar
                 ggtheme = theme_minimal(),
                 mian = paste(month, "Additional Locations Scree Plot", sep=" ")
  )
  ggpar(scree,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_new_scree.png", sep="")))


  
  #calculate the best number of clusters by ward linkage
  number.cluster <- NbClust(res.pca$x, distance = "euclidean", min.nc = 1, max.nc = 10, method = "ward.D", index = "ch")
  best.cluster <- as.numeric(number.cluster$Best.nc[1])
  
  # Compute hierarchical clustering and cut into n clusters
  res.hcut <- hcut(res.pca$x, k = best.cluster)
  # Visualize
  dend_new= fviz_dend(res.hcut,
            cex = 0.7,                     # Label size
            palette = "jco",               # Color palette see ?ggpubr::ggpar
            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
            rect_border = "jco",           # Rectangle color
            labels_track_height = 0.8,      # Augment the room for labels
            main = paste(month, "Additional Locations Dendrogram", sep=" ")
  )
  ggpar(dend_new,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_new_dend.png", sep="")))
  
  #create matrix of cluster id for each location
  clust_id= res.hcut$cluster
  assign(paste0(month, "_clustID"), clust_id)
  
  
  #compute PAM clustering and visualise 
  res.pam <- pam(res.pca$x, 
                 best.cluster,
                 diss=FALSE,
                 stand=FALSE)
  #create matrix of cluster id for each location(needed for count of same times in each cluster)
  clust_id= res.pam$clustering
  assign(paste0(month, "_clustID_pam"), clust_id)
  
  clust_new=fviz_cluster(res.pam,
               stand= FALSE,
               data = data,
               repel = TRUE,            # Avoid label overlapping
               show.clust.cent = TRUE, # Show cluster centers
               palette = "jco",         # Color palette see ?ggpubr::ggpar
               ggtheme = theme_minimal(),
               main = paste(month, "PAM Clustering Including Additional Locations", sep=" ")
  )
  ggpar(clust_new,
        font.main = 0,
        font.x = c(14, "red"),
        font.y = c(14, "red"),
        font.tickslab = c(14, "bold", "red"))
  ggsave(file.path('graphs', filename=paste(month, "_new_cluster.png", sep="")))
  
  
#plot variable contributions to PC1 
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
  ggsave(file.path('./graphs/', filename=paste(month, "_new_pc1.png", sep="")))

#plot variable contributions to PC2 
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
  
  ggsave(file.path('./graphs/', filename=paste(month, "_new_pc2.png", sep="")))
 
}

#Code for counting and plotting the number of times two locations share a cluster

#create matrix of cluster id numbers
clust_matrix_new_pam= cbind(matrix(Jan_clustID_pam), matrix(Feb_clustID_pam), matrix(Mar_clustID_pam), matrix(Apr_clustID_pam), matrix(May_clustID_pam), matrix(Jun_clustID_pam), matrix(Jul_clustID_pam), matrix(Aug_clustID_pam), matrix(Sep_clustID_pam), matrix(Oct_clustID_pam), matrix(Nov_clustID_pam), matrix(Dec_clustID_pam))

#create blank matrix to assign match numbers to
match_matrix_new_pam= matrix(nrow = 27, ncol = 27)
rownames(match_matrix_new_pam) = location
colnames(match_matrix_new_pam)= location

#for loop comparing all rows of matrix and counting matches
for (i in 1:nrow(clust_matrix_new_pam)) for(j in 1:nrow(clust_matrix_new_pam)){
  matches= length(which(clust_matrix_new_pam[i,]== clust_matrix_new_pam[j,]))
  match_matrix_new_pam[i,j]= matches 
} 

#save plot of number of times two locations share the same cluster 
png(file = "./graphs/match_matrix_new_pam.png")
corrplot(match_matrix_new_pam, is.corr=FALSE, method='square', order='alphabet', diag=FALSE, type='lower', number.digits = 0,col.lim=c(0,12), col=colorRampPalette(c("white","red"))(200), tl.col = "blue", cl.length = 7, title= "Number of months two locations share the same cluster", mar=c(1, 1, 1, 1))
dev.off()



