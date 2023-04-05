
library(lavaan)
library(semPlot)
library(piecewiseSEM)
library(nlme)
install.packages("lavaanPlot")
library(lavaanPlot)
### Read data

leaf_path_data2<-read.table("clipboard", header = T, sep = "\t", dec = ",")
#rerun all the models, previous ones are with old version of piecewisesem

####################################################################################################################

#SEM for Leaf_path data

colnames(leaf_path_data)



leaf_mod888<- psem(gls(Mort._LR ~Asc_mold._LR+Picea.abies+LR_ECM_BA+pH+LEAF_PATH_OTUs+longitude, leaf_path_data2),
                 gls(Asc_mold._LR~pH +Picea.abies+longitude, leaf_path_data2),
                 gls(LEAF_PATH_OTUs~Asc_mold._LR+pH+Picea.abies+LR_ECM_BA+longitude, leaf_path_data2),
                 gls(pH~LR_ECM_BA+longitude+Picea.abies, leaf_path_data2))

summary(leaf_mod888)
#

leaf_mod4<- psem(gls(Mort._LR ~Asc_mold._LR+Picea.abies+LR_ECM_BA+pH+LEAF_PATH_OTUs, leaf_path_data2),
                 gls(Asc_mold._LR~Picea.abies, leaf_path_data2),
                 gls(LEAF_PATH_OTUs~Asc_mold._LR+pH+Picea.abies+LR_ECM_BA, leaf_path_data2),
                 gls(pH~LR_ECM_BA+Picea.abies, leaf_path_data2))
summary(leaf_mod4)
leaf_mod5<- psem(gls(Mort._LR ~Picea.abies+LR_ECM_BA+pH, leaf_path_data2),
                 gls(Asc_mold._LR~Picea.abies, leaf_path_data2),
                 gls(LEAF_PATH_OTUs~LR_ECM_BA+pH+Picea.abies, leaf_path_data2),
                 gls(pH~LR_ECM_BA+Picea.abies, leaf_path_data2),
                 LEAF_PATH_OTUs%~~%Asc_mold._LR,
                 LEAF_PATH_OTUs%~~% Mort._LR,
                 Asc_mold._LR%~~% Mort._LR)
summary(leaf_mod5)

plot(
  leaf_mod5,
  return = FALSE,
  node_attrs = data.frame(shape = "rectangle", color = "black", fillcolor = "white"),
  edge_attrs = data.frame(style = "lisrel", color = "black"),
  ns_dashed = T,
  alpha = 0.05,
  show = "std",
  digits = 3,
  add_edge_label_spaces = TRUE
)
#################################################
#SEM for Leaf_path data in lavaan

lav_mod1<- 'Mort._LR ~Picea.abies+LR_ECM_BA+pH+LEAF_PATH._LR+longitude
            Asc_mold._LR~Mort._LR +Picea.abies+pH+LEAF_PATH._LR+longitude'

fit.lav_mod1<-sem(lav_mod1, data = leaf_path_data)

summary(fit.lav_mod1, fit.measures=T, standardized=T, rsq=T)

semPaths(object = fit.lav_mod1,
         layout = "spring",
         rotation = 1,
         whatLabels = "std",
         edge.label.cex = 1,
         what = "std",
         edge.color = "navy",
         sizeMan = 10, nCharNodes=8,title=T)



semPaths(fit.lav_mod1, whatLabels="std", style="lisrel", exoCov = T, curvePivot = TRUE, sizeMan = 8, sizeInt = 12, 
         residuals=F,nCharNodes=8) 
#, residuals = T
modificationindices(fit.lav_mod1, sort = T)
#########################################################################
#labels <- list(pH = "pH", Longitude = "Longitude", LR_ECM_BA = "Basal area", Picea.abies = "P.abies", LEAF_PATH._LR = "Leaf path", Mort._LR = "Mort._LR",
#               Asc_mold._LR="Asc mold LR")
#argument lables=labels
lavaanPlot(model = fit.lav_mod1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = T,covs = TRUE)
######################################################



