#glm()dependetvariable- 1, data) try for overdisperssion in data. See GLM course by method
#overdispersion function
#help
# Ben Bolker's GLMM FAQ webpage



overdisp_fun <- function(model) {
  
  rdf <- df.residual(model)
  
  rp <- residuals(model,type="pearson")
  
  Pearson.chisq <- sum(rp^2)
  
  prat <- Pearson.chisq/rdf
  
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  
  c(chisq=Pearson.chisq,dispersion=prat,rdf=rdf,p=pval)
  
}


overdisp_fun(mod) #insert your model name here once you've loaded the function into R