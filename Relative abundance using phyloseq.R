#calculate abundance at genera level, change the taxa level if necessary

#function expects OTU table column as samples, rownames as otu names, no sample id column should be included, also taxa as rows
#function will return the  long dataframe with relative abundnace
# w is metadata, make sure rownames of metadata and col names of OTU table, samples, are matching
# y = OTU
# z = taxonomy
# w =metadata
###############################################################################################################
calc_abund<- function (y, z,w){
  
  
  if(identical(rownames(z),rownames(y)==TRUE)){
    otu_p= otu_table(y, taxa_are_rows = T)
    tax_p=tax_table(z)
    meta_phy=sample_data(w)
    top10_phyloseq<- phyloseq(otu_p,tax_p,meta_phy)
    return(top10_phyloseq)
  } 
  else {
    taxdata= z[ order(row.names(z)), ]
    otudata= y[ order(row.names(y)), ]
    otu_p= otu_table(y, taxa_are_rows = T)
    tax_p=tax_table(z)
    meta_phy=sample_data(w)
    
    taxa_names(tax_p)<-rownames(z)
    
    
    top_phyloseq<- phyloseq(otu_p,tax_p,meta_phy)
  }
  colnames(tax_table(top_phyloseq))<-c("Phylum","Class","Order","Family","Genus","Species")
  
  
  GP4_tax = transform_sample_counts(top_phyloseq, function(x) x/sum(x))
  
  glom_genus <- tax_glom(GP4_tax, taxrank = 'Genus')
  
  data_glom_genus<- psmelt(glom_genus) # create dataframe from phyloseq object
  
  data_glom_genus$Genus <- as.character(data_glom_genus$Genus) #convert to character
  
  
  
  return(data_glom_genus)
  
}