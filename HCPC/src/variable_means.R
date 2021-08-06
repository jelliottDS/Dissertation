#pacakages gridextra

#compare annual means of each location data to mean of whole data set

#only show 2 d.p.
options(digits=2)
#ffp location data
ffp_df= read.csv("./data/ffp_variable_means.csv")
rownames(ffp_df)= ffp_df$city
ffp_df= apply(ffp_df[2:7],2, function(x)(x/mean(x)*100))
columns= c("TOW", "UV Radiation", "Temperature", "Total Precipitation", "Electrolyte Concentration", "FOW")
colnames(ffp_df)= columns


pdf("./graphs/variable_means_ffp.pdf", height=11, width=14)
grid.table(format(ffp_df, decimal.mark = "."))
dev.off()



#lowest similarities cities data
lowest_df= read.csv("./data/lowest_sim_variable_means.csv")
rownames(lowest_df)= lowest_df$city
lowest_df=apply(lowest_df[2:7],2, function(x) (x/mean(x)*100))
columns= c("TOW", "UV Radiation", "Temperature", "Total Precipitation", "Electrolyte Concentration", "FOW")
colnames(lowest_df)= columns


pdf("./graphs/variable_means_lowest.pdf", height=11, width=14)
grid.table(format(lowest_df, decimal.mark = "."))
dev.off()
