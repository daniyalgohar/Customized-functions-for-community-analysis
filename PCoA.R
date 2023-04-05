#function returns axis variance percentage(PCoA, from eig values) and beta diversity scores based on bray curtis distances combbinded in metadata provided
#inputs are (x,y,z) where x=otu data(columns as samples, best is to use same data as used for alpha diversity function)
#			  y=metadata$cloumnName( against which you want to use calculate scores)
#			z=  metada(ful data0)
# make sure samples names are identical	before using it

#################################################################################################################
calc_betaDiv <-function(x,y,z){
  library(vegan)
  dist_PCOA<- vegdist(t(x), method = "bray") ## Bray curtis distance measures
  
  ##multivariate homogeneity of group dispersions (variances) ,(PCoA)
  PCOA_d<- betadisper(dist_PCOA, y)
  #variance explained for axis 1
  axis1<- as.data.frame(PCOA_d$eig)[1,]/sum(PCOA_d$eig)*100
  print(c("Variance explained for axis 1 =", axis1) )
  #variance explained for axis 2
  axis2<- as.data.frame(PCOA_d$eig)[2,]/sum(PCOA_d$eig)*100
  print(c("Variance explained for axis 2 =", axis2) )
  
  ##extract scores
  Score_PCOA<- as.data.frame(scores(PCOA_d, display= c("sites"), choices=c(1,2)))
  metadata_with_scores<- cbind(z, Score_PCOA)
  
  return(metadata_with_scores)
  
}