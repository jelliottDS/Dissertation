#Required packages FactoMineR, factoextra, dplyr,plyr, cluster, NbClust

files = list.files(path="./data", pattern= "*_lowest_cities.csv", full.names = TRUE)

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
  
  #pca on data
  res.pca = prcomp(data, retx=TRUE, center=TRUE, scale=FALSE)
  
  #% explained variance of each component
  expl.var <- round(res.pca$sdev^2/sum(res.pca$sdev^2)*100)
  #visualise explained variance 
  # fviz_screeplot(res.pca,
  #                repel = TRUE,            # Avoid label overlapping
  #                show.clust.cent = TRUE, # Show cluster centers
  #                palette = "jco",         # Color palette see ?ggpubr::ggpar
  #                ggtheme = theme_minimal(),
  #                mian = paste(month, "Additional Locations Scree Plot", sep=" ")
  # )
  # ggsave(file.path('graphs', filename=paste(month, "_new_scree.png", sep="")))
  # 
  # #scatter plot cities pca
  # fviz_pca_ind(res.pca,
  #              #col.ind = "cos2", # Color by the quality of representation
  #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #              repel = TRUE,     # Avoid text overlapping,
  #              main = paste(month, "Additional Locations Scatter Plot", sep=" ")
  # )
  # ggsave(file.path('graphs', filename=paste(month, "_new_scatter.png", sep="")))
  
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
  
  #fviz_pca_biplot(res.pca, repel = TRUE)
  
  #compute PAM clustering and visualise 
  res.pam <- pam(res.pca$x, 
                 best.cluster,
                 diss=FALSE,
                 stand=FALSE)
  #create matrix of cluster id for each location
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
  
  
  # fviz_pca_var(res.pca,
  #              repel = TRUE,            # Avoid label overlapping
  #              show.clust.cent = TRUE, # Show cluster centers
  #              palette = "jco",         # Color palette see ?ggpubr::ggpar
  #              ggtheme = theme_minimal(),
  #              title = paste(month, "Additional Locations Variable Direction", sep=" ")
  # )
  # ggsave(file.path('graphs', filename=paste(month, "_new_var_direction.pdf", sep="")))
  # 
  fviz_contrib(res.pca,
               choice = "var",
               axes = 1,
               top = 10,
               palette = "jco",
               ggtheme = theme_minimal(),
               title = paste(month, "Additional Locations PC1 Contributions", sep=" "))

  ggsave(file.path('./graphs/', filename=paste(month, "_new_pc1.png", sep="")))

  fviz_contrib(res.pca,
               choice = "var",
               axes = 2,
               top = 10,
               palette = "jco",
               ggtheme = theme_minimal(),
               title = paste(month, "Additional Locations PC2 Contributions", sep=" "))

  ggsave(file.path('./graphs/', filename=paste(month, "_new_pc2.png", sep="")))
  # 
  # head(res.hcpc$data.clust, 10)
  # res.hcpc$desc.var$quanti
  # var_importance= res.hcpc$desc.var$quanti
  # capture.output(res.hcpc$desc.var$quanti, file = "Var_importance jul.txt")
  # 
  # vi_df = ldply (var_importance, data.frame)
  # vi_df=subset(vi_df, select=c(".id", "Mean.in.category", "Overall.mean", "p.value"))
  # vi_df=rename(vi_df, c(".id"="Cluster", "Mean.in.category"="Mean In Cluster", "Overall.mean" = "Overall Mean", "p.value"= "p Value"))
  # vi_df =vi_df %>% mutate_at(vars("Mean In Cluster", "Overall Mean", "p Value"), funs(round(., 2)))
  # 
  # data.frame(matrix(unlist(var_importance), nrow=length(var_importance), byrow=T))
  # res.hcpc$desc.axes$quanti
  # capture.output(res.hcpc$desc.axes$quanti, file = "dim_importance jan.txt")
  
}

#create matrix of cluster id numbers
clust_matrix_new= cbind(matrix(Jan_clustID), matrix(Feb_clustID), matrix(Mar_clustID), matrix(Apr_clustID), matrix(May_clustID), matrix(Jun_clustID), matrix(Jul_clustID), matrix(Aug_clustID), matrix(Sep_clustID), matrix(Oct_clustID), matrix(Nov_clustID), matrix(Dec_clustID))

#create blank matrix to assign match numbers to
match_matrix_new= matrix(nrow = 27, ncol = 27)
rownames(match_matrix_new) = location
colnames(match_matrix_new)= location

#for loop comparing all rows of matrix and counting matches
for (i in 1:nrow(clust_matrix_new)) for(j in 1:nrow(clust_matrix_new)){
  matches= length(which(clust_matrix_new[i,]== clust_matrix_new[j,]))
  match_matrix_new[i,j]= matches 
} 
png(file = "./graphs/match_matrix_new.png")
heatmap.2( match_matrix_new, Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=match_matrix_new, notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()


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


png(file = "./graphs/match_matrix_new_pam.png")
heatmap.2( match_matrix_new_pam, Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=match_matrix_new_pam, notecol="black", trace='none', key=FALSE,lwid = c(.01,.99),lhei = c(.01,.99),margins = c(5,15 ))
dev.off()
