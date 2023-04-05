#. function returns the  alpha diversity index, both richness and shannon binded with metadata. inputs are  #(otu,metadata).
#otu table has columns as samples function will transpose and calculate the index
################################################################################################################
calc_alphadiv<-function(OTU_data,Metadata){
  richness<-as.data.frame(specnumber(t(OTU_data)))
  colnames(richness)<-"Richness"
  if(identical(rownames(richness), rownames(Metadata))==TRUE){
    metadata<- cbind(Metadata,richness)
    
    shannon<-as.data.frame(diversity(t(OTU_data)), index="shannon")  
    colnames(shannon)<-"Shannon"
    
    invsimpson<-as.data.frame(diversity(t(OTU_data), index="invsimpson"))  
    colnames(invsimpson)<-"invsimpson"
    metadata<- cbind(metadata,shannon,invsimpson)
    return(metadata)
  }
  else
  {print("Row names of metadata and OTU are not same")}
}


#function returns raw and normalized alpha diversity indices both shannon and richness , 
#x= raw abudnance data where y= metadata, function expects no out name column and sample names are identical

###########################################

calc_Norm_alphadiv<-function(x,y){
  richness<-as.data.frame(specnumber(t(x)))
  colnames(richness)<-"Richness"
  
  shannon<-as.data.frame(diversity(t(x)), index="shannon")  
  colnames(shannon)<-"Shannon"
  AlphaDiv<- cbind(richness,shannon)
  
  normalized_richness<-resid(lm(AlphaDiv$Richness~sqrt(colSums(x)))) 
  normalized_shannon<-resid(lm(AlphaDiv$Shannon~sqrt(colSums(x)))) 
  AlphaDiv$norm_richnes<-normalized_richness
  AlphaDiv$norm_shannon<-normalized_shannon
  AlphaDiv<-cbind(y,AlphaDiv)
  return(AlphaDiv)
  
}
