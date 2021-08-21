#pacakages gridextra

#compare annual means of each location data to mean of whole data set

#only show 2 d.p.
options(digits=2)
#ffp location data
ffp_df= read.csv("./data/ffp_variable_means.csv")
city= ffp_df$city
ffp_df= apply(ffp_df[2:7],2, function(x)(x/mean(x)*100))
ffp_df= data.frame(ffp_df)
ffp_df=cbind(City=city, ffp_df)
columns= c("city","TOW", "UVR", "T", "P", "NaCl", "FOW")# "City")
colnames(ffp_df)= columns



mins= c(8.7,	135,	112,	177.2,	245,	153)
maxs=c(186,	62,	88,	8,	19,	18)
ffp_df2= ffp_df[2:7] 
rownames(ffp_df2)= city


for(i in 1:nrow(ffp_df)){
  city= ffp_df[i,1]
  df=data.frame(mins=mins,
             maxs=maxs,
             actual= t(ffp_df2[i,]))
  assign(paste(city, "df", sep="_"), df)
} 
 
  

png(file = "./graphs/bangalore_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Bangalore_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/dammam_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Dammam_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/felling_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Felling_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/florida1_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(`Florida 1_df`)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/florida2_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(`Florida 2_df`)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/geoje_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Geoje_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/houston_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Houston_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/melbourne_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Melbourne_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/phoenix_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Phoenix_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/plymouth_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Plymouth_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/pudong_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Pudong_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/runco_grande_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(`Ruco Grande_df`)) , pfcol="darkorchid3", vlcex = 1.75 )
dev.off()

png(file = "./graphs/sanary_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Sanary_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/songjiang_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Songjiang_df)) , pfcol="darkorchid3", vlcex = 1.75 )
dev.off()

png(file = "./graphs/sunderland_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Sunderland_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/suzhou_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Suzhou_df)) , pfcol="darkorchid3", vlcex = 1.75)
dev.off()

png(file = "./graphs/willawong_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Willawong_df)), pfcol="darkorchid3", vlcex = 1.75)
dev.off()


#lowest similarities cities data
lowest_df= read.csv("./data/lowest_sim_variable_means.csv")
lowest_city= lowest_df$city
lowest_df=apply(lowest_df[2:7],2, function(x) (x/mean(x)*100))
lowest_df=data.frame(lowest_df)
lowest_df=cbind(City=lowest_city, lowest_df)
colnames(lowest_df)= columns


mins_new= c(2.2,57,86,195.8,294.1,182.1)
maxs_new=c(242.1,	132,	112,	2.5,	5.5,	6.3)
lowest_df2= lowest_df[2:7] 
rownames(lowest_df2)= lowest_city


for(i in 1:nrow(lowest_df)){
  city= lowest_df[i,1]
  df=data.frame(mins=mins_new,
                maxs=maxs_new,
                actual= t(lowest_df2[i,]))
  assign(paste(city, "df_new", sep="_"), df)
} 


png(file = "./graphs/lima_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Lima_df_new)), pfcol="darkorange1", vlcex = 1.75)
dev.off()

png(file = "./graphs/kinshasa_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Kinshasa_df_new)), pfcol="darkorange1", vlcex = 1.75)
dev.off()

png(file = "./graphs/jakarta_var_means.png")
par(mar =c(0,0,0,0))
radarchart(data.frame(t(Jakarta_df_new)), pfcol="darkorange1", vlcex = 1.75)
dev.off()

