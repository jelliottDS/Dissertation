#compare annual means of each location data to mean of whole data set

#ffp location data
ffp_df= read.csv("./data/ffp_variable_means.csv")
rownames(ffp_df)= ffp_df$city
ffp_df= apply(ffp_df[2:7],2, function(x) (x/mean(x)*100))

#lowest similarities cities data
lowest_df= read.csv("./data/lowest_sim_variable_means.csv")
rownames(lowest_df)= lowest_df$city
lowest_df=apply(lowest_df[2:7],2, function(x) (x/mean(x)*100))
