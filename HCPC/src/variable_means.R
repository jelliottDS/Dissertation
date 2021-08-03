df= read.csv("./data/ffp_variable_means.csv")


apply(df[2:7],2, function(x) (x/mean(x)*100))
