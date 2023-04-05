PCA_analysis <-function(matxData,metadata){
  if (
    identical(rownames(matxData),rownames(metadata))== TRUE) {
    
    pca<-prcomp(matxData, scale=TRUE)
    
    #calculate variation percentages
    pca.var<-pca$sdev^2
    pca.var.per<-round(pca.var/sum(pca.var)*100,1)
    # print variance explained for first 2 components
    print(c("Variance explained by PC1 =", pca.var.per[1]) )
    print(c("Variance explained by PC2 =", pca.var.per[2]) )
    
    #extract scores for ggplot
    pca.data<-data.frame(PC1=pca$x[,1], PC2=pca$x[,2])
    
    metadata_with_scores<- cbind(metadata, pca.data)
    
    return(metadata_with_scores)
  }
  else{
    print("Sample order is not matching")
  }
} 