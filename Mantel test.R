########################################## Run mantel test #####################################################################

# run the test for all dataframes manually, replace the data name (in mantel_stats as well) and run the script

data=all
#load dataframe, read first two roas as longitude and latitude and remove the first column with OTU ids
x_coordinates= data.frame(Long=as.numeric( t(data[2,])), Lat= as.numeric( t(data[1,])), row.names = colnames(data))
x_coordinates=x_coordinates[-1,]

x_geodist=geodist::geodist(x_coordinates, measure = "haversine" ) #translate coordinates into geospatial distance matrix

#assign row names and colnames
rownames(x_geodist)=rownames(x_coordinates)
colnames(x_geodist)=rownames(x_coordinates)


#vegdist on community data
ss=vegdist(t(data[-c(1,2),-1]), method = "bray")

# convert to matrix to use in "ready2plot_dist" function
ss=as.matrix(ss)
# confirm if rows and columns are identical for geodist data and bray-curtis data
identical(rownames(x_geodist), rownames(ss))
mod=mantel(ss, x_geodist,  permutations=9999)


mantel_stats<- rbind(mantel_stats, data.frame(plants="all", y="Spatial Distance", p=mod$signif, r.value=mod$statistic, x="Bray-Curtis dist"))



write.table(mantel_stats, "mantel_stats.xls", sep = "\t")


#Plot the r statistics
ggplot(mantel_stats, aes(x=reorder( plants,r.value),y=r.value, group=1))+geom_line(size=1)+geom_point(size=4)+theme_minimal()+
  theme(axis.text = element_text(size = 14, color = 'black'), axis.title = element_text(size = 16))+labs(x="", y="r-value", caption = "Mantel test statistics (Bray-curtis X Spatial distance). p-values are written with each point.")+ 
  scale_y_continuous(limits=c(0.02,0.3), breaks=seq(0.05,0.28, by = 0.05))+geom_text(aes(label=p, hjust=-0.1,vjust=1.5))

