

Ready2plot_distData=function(dist1, dist2){
  library(tidyr)
  library(gdata)
  dist1<-as.matrix(dist1)
  dist1<-as.data.frame(dist1)
  upperTriangle(dist1) <- NA
  pcm <- gather(dist1, Env, Eluc, D10:P33,
                factor_key=TRUE)
  pcm$sample2 <- rep(colnames(dist1),len=nrow(pcm)) 
  colnames(pcm)<-c("Sample1","bray_curtis","Sample2" )#changethe colnames according to dataframes
  
  pcm<-pcm[order(pcm$Sample2),]
  
  dist2<-as.matrix(dist2)
  dist2<-as.data.frame(dist2)
  upperTriangle(dist2) <- NA
  pcm2 <- gather(dist2, Env, Eluc, D10:P33,
                 factor_key=TRUE)
  
  pcm2$sample2 <- rep(colnames(dist2),len=nrow(pcm2)) 
  
  colnames(pcm2)<-c("Sample1","phylo_dist","Sample2" )
  pcm2<-pcm2[order(pcm2$Sample2),]
  if (identical(pcm2$Sample2, pcm$Sample2)==TRUE & identical(pcm2$Sample, pcm$Sample)==TRUE ){
    final_long_data<-cbind(pcm,pcm2)
    final_long_data<-na.omit(final_long_data)
    final_long_data<-final_long_data[which(final_long_data$bray_curtis!=0 &final_long_data$phylo_dist!=0),-c(4,6)]
    return(as.data.frame(final_long_data))
    
  }
  else print("sample names aren't identical")
}

