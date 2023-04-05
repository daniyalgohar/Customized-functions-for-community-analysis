#cooccurance analysis, returns a igraph object

Coocc_sparcc<-
  function(matrix,cor.cutoff,p.cutoff){
    
    matrix1<-matrix
    matrix1[matrix1>0]<-1
    
    #correlation analysis based on spearman's co-efficient
    matrix.dist<-rcorr(t(matrix),type="spearman")
    ###matrix.dist<-rcorr(t(matrix),type="pearson")
    matrix.cor<-matrix.dist$r
    matrix.cor.p<-matrix.dist$P
    
    #Multiple testing correction using Benjamini-Hochberg standard false discovery rate correction ("FDR-BH")
    matrix.cor.p <- p.adjust(matrix.cor.p, method="BH")
    
    
    #3.Consider both positive and netagive cooccurence at given coefficient (cor.cutoff) and p-value cutoffs
    matrix.cor3<-matrix.cor
    matrix.cor3.p<-matrix.cor.p
    matrix.cor3[which(matrix.cor3>=(-cor.cutoff) & matrix.cor3 <= cor.cutoff)]=0
    matrix.cor3[which(matrix.cor3.p>p.cutoff)]=0
    
    # delete those rows and columns with sum = 0
    matrix.cor3[is.nan(matrix.cor3)] <- 0
    matrix.cor3<-matrix.cor3[which(rowSums(matrix.cor3)!=1),]
    matrix.cor3<-matrix.cor3[,which(colSums(matrix.cor3)!=0)]
    
    
    
    ###g2<-graph.adjacency(matrix.cor2,weight=T,mode="undirected")
    ###g2<-simplify(g2)
    ###V(g2)$label <- V(g2)$name
    ###V(g2)$degree <- degree(g2)
    
    g3<-graph.adjacency(matrix.cor3,weight=T,mode="undirected")
    g3<-simplify(g3)
    V(g3)$label <- V(g3)$name
    V(g3)$degree <- degree(g3)
    
    # append the output into results
    result<-list()
    result$matrix.cor<-matrix.cor
    result$matrix.cor.p<-matrix.cor.p
    
    result$matrix.cor3<-matrix.cor3
    result$graph3<-g3
    return(result)
  }
#wirte gml formati for giphy

write.graph(ig.sparcc,'PosNeg0.6-NW.sparcc.gml',format='gml')