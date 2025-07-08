####################################
#### Deforestation analysis LUP ####
####################################

# Code written by: Diana María Gutiérrez-Zapata
# Mail: d.m.gutierrez@cgiar.org
# Date: May 2024

## Note ##

#Please be advised that updates to libraries and functions may affect the results. If you find differences in some of them, please note that the results found on the LUP manuscript were obtained in late 2021.

#### LOAD LIBRARIES AND DATA ###

library(readxl)
library(plyr)
library(doBy)
library(dplyr)
library(tidyr)
library(mgcv)
library(sf)
library(spdep)
library(ape)
library(emmeans)
library(gratia)
library(ggplot2)
library(mgcViz)
library(ggspatial)
library(cowplot)


# PDET pre-processed data

pdet=read_excel("C:/My_Folder/PDET_Data.xlsx")

# Deforestation analysis data

p1019 <- read_excel("C:/My_Folder/p1019_Data.xlsx")
p1019=dplyr::mutate_if(p1019,is.character,as.factor)

# Study area map 

nucleos=st_read('C:/My_Folder/nucleos_completo.shp')



####################################
#### PDET data weighting system ####
###################################


### OPPORTUNITIES ###

opdet=subset(pdet,Tipo=="Oportunidad")
opdet$MN=paste(opdet$Municipio,opdet$`Nucleo Veredal`,sep="/")

# Frequency of each category by 'Nucelo veredal' (i.e., cluster)

opdet.resu=data.frame(table(opdet$MN,opdet$Categoria))
colnames(opdet.resu)=c("MN","Categoria","FOP")

# Adding the pillar and the frequencies by category and pillar

nrow(plyr::count(opdet,vars=c("Categoria")))# should be 52 
opdet.resu=merge(opdet.resu,data.frame(plyr::count(opdet,vars=c("Categoria","Pilar"))),all.x=T)
opdet.resu=opdet.resu[do.call(order,opdet.resu[c("Pilar","Categoria","MN")]),]
colnames(opdet.resu)=c("Categoria","MN","FOP","Pilar","f_cat_op")
opdet.resu=merge(opdet.resu,plyr::count(opdet,vars="Pilar"),all.x = T)
colnames(opdet.resu)=c("Pilar","Categoria","MN","FOP","f_cat_op","f_pil_op")

# Average of mentions

f_op=subset(opdet.resu,opdet.resu$FOP!=0)
cmean_op=summaryBy(FOP~Categoria,FUN=c(mean),data=f_op)
cmean_op$FOP.mean=round(cmean_op$FOP.mean,0)
opdet.resu=merge(opdet.resu,cmean_op)

# Weight of each category within a pillar

opdet.resu$por_cat_op=round((opdet.resu$f_cat_op*100)/opdet.resu$f_pil_op,2)
summaryBy(por_cat_op~Pilar+MN,FUN=sum,data=opdet.resu)

# Total points by cluster and pillar

opdet.resu$puntos_cat_op=ifelse(opdet.resu$FOP.mean>opdet.resu$FOP,(opdet.resu$por_cat_op*opdet.resu$FOP)/opdet.resu$FOP.mean,opdet.resu$por_cat_op)
tot_op=summaryBy(puntos_cat_op~MN+Pilar,FUN=sum,data=opdet.resu)


### PROBLEMS ###

prpdet=subset(pdet,Tipo=="Problemática")
prpdet$MN=paste(prpdet$Municipio,prpdet$`Nucleo Veredal`,sep="/")

# Frequency of each category by 'Nucelo veredal' (i.e., cluster)

prpdet.resu=data.frame(table(prpdet$MN,prpdet$Categoria))
colnames(prpdet.resu)=c("MN","Categoria","FPR")

# Adding the pillar and the frequencies by category and pillar

nrow(plyr::count(prpdet,vars=c("Categoria")))# should be 52 
prpdet.resu=merge(prpdet.resu,data.frame(plyr::count(prpdet,vars=c("Categoria","Pilar"))),all.x=T)
prpdet.resu=prpdet.resu[do.call(order,prpdet.resu[c("Pilar","Categoria","MN")]),]
colnames(prpdet.resu)=c("Categoria","MN","FPR","Pilar","f_cat_pr")
prpdet.resu=merge(prpdet.resu,plyr::count(prpdet,vars="Pilar"),all.x = T)
colnames(prpdet.resu)=c("Pilar","Categoria","MN","FPR","f_cat_pr","f_pil_pr")

# Average of mentions

f_pr=subset(prpdet.resu,prpdet.resu$FPR!=0)
cmean_pr=summaryBy(FPR~Categoria,FUN=c(mean),data=f_pr)
cmean_pr$FPR.mean=round(cmean_pr$FPR.mean,0)
prpdet.resu=merge(prpdet.resu,cmean_pr)

# Weight of each category within a pillar

prpdet.resu$por_cat_pr=round((prpdet.resu$f_cat_pr*100)/prpdet.resu$f_pil_pr,2)
summaryBy(por_cat_pr~Pilar+MN,FUN=sum,data=prpdet.resu)

# Total points by cluster and pillar

prpdet.resu$puntos_cat_pr=ifelse(prpdet.resu$FPR.mean>prpdet.resu$FPR,(prpdet.resu$por_cat_pr*prpdet.resu$FPR)/prpdet.resu$FPR.mean,prpdet.resu$por_cat_pr)
tot_pr=summaryBy(puntos_cat_pr~MN+Pilar,FUN=sum,data=prpdet.resu)


#### NOTES ###

#1. The maximum score for a pillar is 100 points. The points earned depend on the frequency of the categories in the pillars.
#2. Each category can reach a maximum of X points, contributing to the total of 100 points for each pillar.
#3. The number of points of a category for a cluster X depends on the frequencies of this category in the cluster.
#4. If the frequency of a category is below the average frequency of that category (number of mentions / number of clusters that mentioned it), points will be awarded proportionally.
#5. If the frequency of a category is equal to or higher than the average frequency of that category, it will receive the maximum points that category can receive in Pillar X.
#6. In the case of opportunities, 100 points is the best scenario, in the case of problems it would be the worst.



####################################
#### Perception-based scenarios ####
###################################


total=merge(tot_pr,tot_op)
which(total$puntos_cat_op.sum>100)
which(total$puntos_cat_op.sum<0)
which(total$puntos_cat_pr.sum>100)
which(total$puntos_cat_pr.sum<0)
total$puntos_cat_op.sum=ifelse(total$puntos_cat_op.sum>100.00,100,total$puntos_cat_op.sum)
total$puntos_cat_pr.sum=ifelse(total$puntos_cat_pr.sum>100.00,100,total$puntos_cat_pr.sum)

total$clase_3l=ifelse(total$puntos_cat_pr.sum>=0 & total$puntos_cat_pr.sum<=20 &
                        total$puntos_cat_op.sum>=0 & total$puntos_cat_op.sum<=20,"Nini",
                      ifelse(total$puntos_cat_pr.sum>=0 & total$puntos_cat_pr.sum<=40 &
                               total$puntos_cat_op.sum>40 & total$puntos_cat_op.sum<=100,"OP",
                             ifelse(total$puntos_cat_pr.sum>=0 & total$puntos_cat_pr.sum<=20 &
                                      total$puntos_cat_op.sum>20 & total$puntos_cat_op.sum<=40,"OP",
                                    ifelse(total$puntos_cat_pr.sum>20 & total$puntos_cat_pr.sum<=40 &
                                             total$puntos_cat_op.sum>20 & total$puntos_cat_op.sum<=40,"Nini",
                                           ifelse(total$puntos_cat_pr.sum>20 & total$puntos_cat_pr.sum<=40 &
                                                    total$puntos_cat_op.sum>=0 & total$puntos_cat_op.sum<=20,"PE",
                                                  ifelse(total$puntos_cat_pr.sum>60 & total$puntos_cat_pr.sum<=100 &
                                                           total$puntos_cat_op.sum>=0 & total$puntos_cat_op.sum<=60,"PE",
                                                         ifelse(total$puntos_cat_pr.sum>40 & total$puntos_cat_pr.sum<=60 &
                                                                  total$puntos_cat_op.sum>40 & total$puntos_cat_op.sum<=60,"Nini",
                                                                ifelse(total$puntos_cat_pr.sum>40 & total$puntos_cat_pr.sum<=60 &
                                                                         total$puntos_cat_op.sum>60 & total$puntos_cat_op.sum<=100,"OP",
                                                                       ifelse(total$puntos_cat_pr.sum>40 & total$puntos_cat_pr.sum<=60 &
                                                                                total$puntos_cat_op.sum>=0 & total$puntos_cat_op.sum<=40,"PE",
                                                                              ifelse(total$puntos_cat_pr.sum>60 & total$puntos_cat_pr.sum<=80 &
                                                                                       total$puntos_cat_op.sum>60 & total$puntos_cat_op.sum<=80,"Nini",
                                                                                     ifelse(total$puntos_cat_pr.sum>60 & total$puntos_cat_pr.sum<=80 &
                                                                                              total$puntos_cat_op.sum>80 & total$puntos_cat_op.sum<=100,"OP",
                                                                                            ifelse(total$puntos_cat_pr.sum>80 & total$puntos_cat_pr.sum<=100 &
                                                                                                     total$puntos_cat_op.sum>60 & total$puntos_cat_op.sum<=80,"PE",
                                                                                                   ifelse(total$puntos_cat_pr.sum>80 & total$puntos_cat_pr.sum<=100 &
                                                                                                            total$puntos_cat_op.sum>80 & total$puntos_cat_op.sum<=100,"Nini",
                                                                                                          NA)))))))))))))

table(total$clase_3l)
clase1=total%>%pivot_wider(id_cols= MN, names_from = Pilar, values_from = clase_3l, names_prefix = "calse_")



#############################################
#### Generalized Additive Models (GAM)  ####
############################################


#### ENVRIRONMENTAL PRESSURE ###

# mpa=gam(porc_def_anual~NOMB_MPIO+s(mean_pamb),data=p1019,method = "REML",
#         select = TRUE,family = quasi(variance = "mu^2", link = "log"))
# summary(mpa)
# concurvity(mpa)
# appraise(mpa)
# par(mfrow=c(2,2))
# gam.check(mpa)
# plot(mpa,seWithMean = TRUE,shift = coef(mpa)[1],scheme = 2,scale=0,all.terms = T)
# 
# # Testing normality
# 
# shapiro.test(residuals(mpa));shapiro.test(residuals(mpa,type="pearson"))

# Testing spatial autocorrelation

inv_dist = with(p1019, 1/dist(cbind(x, y), diag = T, upper = T))
inv_dist = as.matrix(inv_dist)
#Moran.I(residuals(mpa), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.

p=merge(nucleos, p1019,by="mun_nv")
p=subset(p,is.na(p$porc_def_anual)=="FALSE")
p$ID=seq(1,nrow(p))
w <- poly2nb(p, row.names=p$ID)
class(w)
summary(w)
str(w)
ww <-  nb2listw(w, style='B')
ww
# moran.test(residuals(mpa), ww, randomisation=FALSE)
# moran.mc(residuals(mpa), ww, nsim=99)
# 
# Testing homoscedasticity
# 
# bartlett.test(residuals(mpa) ~ NOMB_MPIO, data = p1019)


#### DISTANCE TO TERTIARY ROADS ###

mvter=gam(porc_def_anual~NOMB_MPIO+s(mean_vter,bs="cr"),data=p1019,method = "REML",
          select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mvter)
concurvity(mvter)
appraise(mvter)
par(mfrow=c(2,2))
gam.check(mvter)
plot(mvter,seWithMean = TRUE,shift = coef(mvter)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mvter));shapiro.test(residuals(mvter,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mvter), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mvter), ww, randomisation=FALSE)
moran.mc(residuals(mvter), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mvter) ~ NOMB_MPIO, data = p1019)


#### DISTANCE TO EXTENSIVE LIVESTOCK PRODUCTION AREAS WITH A HIGH LEVEL OF TECHNOLOGY ###

mgextc=gam(porc_def_anual~s(mean_dgextn,bs="cr")+s(x,bs="cr"),data=p1019,method = "REML",
           select=TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mgextc)
concurvity(mgextc)
appraise(mgextc)
par(mfrow=c(2,2))
gam.check(mgextc)
plot(mgextc,seWithMean = TRUE,shift = coef(mgextc)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mgextc));shapiro.test(residuals(mgextc,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mgextc), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mgextc), ww, randomisation=FALSE)
moran.mc(residuals(mgextc), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mgextc) ~ NOMB_MPIO, data = p1019)


#### DIFFERENCES IN TEMPERATURE AND PRECIPITATION BETWEEN 2010 AND 2017 ###

mpyt=gam(porc_def_anual~te(dtp,dpp),data=p1019,method = "REML",
         select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mpyt)
concurvity(mpyt)
appraise(mpyt)
par(mfrow=c(2,2))
gam.check(mpyt)
plot(mpyt,seWithMean = TRUE,shift = coef(mpyt)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mpyt));shapiro.test(residuals(mpyt,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mpyt), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mpyt), ww, randomisation=FALSE)
moran.mc(residuals(mpyt), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mpyt) ~ NOMB_MPIO, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: EDUCATION AND RURAL EARLY CHILDHOOD (EREC) ###

mxy_erpi3=gam(porc_def_anual~clase3l_erpi+s(x,y,bs="ds"),data=p1019,method = "REML",
              select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_erpi3)
concurvity(mxy_erpi3)
appraise(mxy_erpi3)
par(mfrow=c(2,2))
gam.check(mxy_erpi3)
plot(mxy_erpi3,seWithMean = TRUE,shift = coef(mxy_erpi3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_erpi3));shapiro.test(residuals(mxy_erpi3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_erpi3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_erpi3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_erpi3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_erpi3) ~ clase3l_erpi, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: INFRASTRUCTURE AND LAND ADAPTATION (ILA) ###

mxy_iat3=gam(porc_def_anual~clase3l_iat+s(x,y,bs="ds"),data=p1019,method = "REML",
             select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_iat3)
concurvity(mxy_iat3)
appraise(mxy_iat3)
par(mfrow=c(2,2))
gam.check(mxy_iat3)
plot(mxy_iat3,seWithMean = TRUE,shift = coef(mxy_iat3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_iat3));shapiro.test(residuals(mxy_iat3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_iat3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_iat3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_iat3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_iat3) ~ clase3l_iat, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: SOCIALLY-ORIENTED ZONING OF RURAL PROPERTY AND LAND USE (SOZ) ###

mxy_otus3=gam(porc_def_anual~clase3l_otus+s(x,y,bs="ds"),data=p1019,method = "REML",
              select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_otus3)
concurvity(mxy_otus3)
appraise(mxy_otus3)
par(mfrow=c(2,2))
gam.check(mxy_otus3)
plot(mxy_otus3,seWithMean = TRUE,shift = coef(mxy_otus3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_otus3));shapiro.test(residuals(mxy_otus3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_otus3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_otus3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_otus3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_otus3) ~ clase3l_otus, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: RECONCILITATION, COEXISTENCE AND PEACE (RCP) ###

mxy_rccp3=gam(porc_def_anual~clase3l_rccp+s(x,y,bs="ds"),data=p1019,method = "REML",
              select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_rccp3)
concurvity(mxy_rccp3)
appraise(mxy_rccp3)
par(mfrow=c(2,2))
gam.check(mxy_rccp3)
plot(mxy_rccp3,seWithMean = TRUE,shift = coef(mxy_rccp3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_rccp3));shapiro.test(residuals(mxy_rccp3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_rccp3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_rccp3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_rccp3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_rccp3) ~ clase3l_rccp, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: ECONOMIC RECOVERY AND AGRICULTURAL PRODUCTION (ERAP) ###

mxy_repa3=gam(porc_def_anual~clase3l_repa+s(x,y,bs="ds"),data=p1019,method = "REML",
              select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_repa3)
concurvity(mxy_repa3)
appraise(mxy_repa3)
par(mfrow=c(2,2))
gam.check(mxy_repa3)
plot(mxy_repa3,seWithMean = TRUE,shift = coef(mxy_repa3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_repa3));shapiro.test(residuals(mxy_repa3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_repa3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_repa3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_repa3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_repa3) ~ clase3l_repa, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: RURAL HEALTH (RH) ###

mxy_sr3=gam(porc_def_anual~clase3l_sr+s(x,y,bs="ds"),data=p1019,method = "REML",
            select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_sr3)
concurvity(mxy_sr3)
appraise(mxy_sr3)
par(mfrow=c(2,2))
gam.check(mxy_sr3)
plot(mxy_sr3,seWithMean = TRUE,shift = coef(mxy_sr3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_sr3));shapiro.test(residuals(mxy_sr3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_sr3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_sr3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_sr3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_sr3) ~ clase3l_sr, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: A SYSTEM TO GUARANTEE THE RIGHT TO FOOD (SGRF) ###

mxy_sgda3=gam(porc_def_anual~clase3l_sgda+s(x,y,bs="ds"),data=p1019,method = "REML",
              select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_sgda3)
concurvity(mxy_sgda3)
appraise(mxy_sgda3)
par(mfrow=c(2,2))
gam.check(mxy_sgda3)
plot(mxy_sgda3,seWithMean = TRUE,shift = coef(mxy_sgda3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_sgda3));shapiro.test(residuals(mxy_sgda3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_sgda3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_sgda3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_sgda3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_sgda3) ~ clase3l_sgda, data = p1019)


####  PERCEPTION-BASED SCENARIO PILLAR: HOUSING, DRINKING WATER AND SANITATION (HWS) ###

mxy_vas3=gam(porc_def_anual~clase3l_vas+s(x,y,bs="ds"),data=p1019,method = "REML",
             select = TRUE,family = quasi(variance = "mu^2", link = "log"))
summary(mxy_vas3)
concurvity(mxy_vas3)
appraise(mxy_vas3)
par(mfrow=c(2,2))
gam.check(mxy_vas3)
plot(mxy_vas3,seWithMean = TRUE,shift = coef(mxy_vas3)[1],scheme = 2,scale=0,all.terms = T)

# Testing normality

shapiro.test(residuals(mxy_vas3));shapiro.test(residuals(mxy_vas3,type="pearson"))

# Testing spatial autocorrelation

Moran.I(residuals(mxy_vas3), weight = inv_dist, scaled=T,na.rm = TRUE)#Using scaled=T results in a correlation metric that goes from -1 to 1.
moran.test(residuals(mxy_vas3), ww, randomisation=FALSE)
moran.mc(residuals(mxy_vas3), ww, nsim=99)

# Testing homoscedasticity

bartlett.test(residuals(mxy_vas3) ~ clase3l_vas, data = p1019)


####################################
####  Means and comparisons     ####
####################################


#mean_mpa=emmeans(mpa,specs = "NOMB_MPIO")
mean_mvter=emmeans(mvter,specs = "NOMB_MPIO")

mean_erpi=emmeans(mxy_erpi3,specs = "clase3l_erpi")
mean_iat=emmeans(mxy_iat3,specs = "clase3l_iat")
mean_otus=emmeans(mxy_otus3,specs = "clase3l_otus")
mean_rccp=emmeans(mxy_rccp3,specs = "clase3l_rccp")
mean_repa=emmeans(mxy_repa3,specs = "clase3l_repa")
mean_sr=emmeans(mxy_sr3,specs = "clase3l_sr")
mean_sgda=emmeans(mxy_sgda3,specs = "clase3l_sgda")
mean_vas=emmeans(mxy_vas3,specs = "clase3l_vas")

### Pairwise comparisons ###

#emmeans(mpa,pairwise~NOMB_MPIO,type="response")
emmeans(mvter,pairwise~NOMB_MPIO,type="response")

emmeans(mxy_erpi3,pairwise~clase3l_erpi,type="response")
emmeans(mxy_iat3,pairwise~clase3l_iat,type="response")
emmeans(mxy_otus3,pairwise~clase3l_otus,type="response")
emmeans(mxy_rccp3,pairwise~clase3l_rccp,type="response")
emmeans(mxy_repa3,pairwise~clase3l_repa,type="response")
emmeans(mxy_sr3,pairwise~clase3l_sr,type="response")
emmeans(mxy_sgda3,pairwise~clase3l_sgda,type="response")
emmeans(mxy_vas3,pairwise~clase3l_vas,type="response")

### standardized effect-size measures ###

#Cohen effect sizes are close cousins of pairwise differences

#eff_size(mean_mpa, sigma = sqrt(mean(sigma(mpa)^2)), edf = df.residual(mpa))
eff_size(mean_mvter, sigma = sqrt(mean(sigma(mvter)^2)), edf = df.residual(mvter))

eff_size(mean_erpi, sigma = sqrt(mean(sigma(mxy_erpi3)^2)), edf = df.residual(mxy_erpi3))
eff_size(mean_iat, sigma = sqrt(mean(sigma(mxy_iat3)^2)), edf = df.residual(mxy_iat3))
eff_size(mean_otus, sigma = sqrt(mean(sigma(mxy_otus3)^2)), edf = df.residual(mxy_otus3))
eff_size(mean_rccp, sigma = sqrt(mean(sigma(mxy_rccp3)^2)), edf = df.residual(mxy_rccp3))
eff_size(mean_repa, sigma = sqrt(mean(sigma(mxy_repa3)^2)), edf = df.residual(mxy_repa3))
eff_size(mean_sr, sigma = sqrt(mean(sigma(mxy_sr3)^2)), edf = df.residual(mxy_sr3))
eff_size(mean_sgda, sigma = sqrt(mean(sigma(mxy_sgda3)^2)), edf = df.residual(mxy_sgda3))
eff_size(mean_vas, sigma = sqrt(mean(sigma(mxy_vas3)^2)), edf = df.residual(mxy_vas3))


########################
#### Visualization ####
#######################


### Annual rate of deforestation (%) ###

mapa_ver2=merge(nucleos,p1019,all=TRUE)

jpeg(file = "C:/My_Folder/fig4_LUP.jpeg", width = 189, height = 100, units = "mm", res =500)

ggplot(data=mapa_ver2)+
  geom_sf(aes(fill=porc_def_anual)) +
  labs(x=NULL,y=NULL,fill="Annual rate of\ndeforestation (%)")+
  scale_fill_viridis_c(limits = c(0,12), breaks = c(0,3,6,9,12),na.value = "transparent")+
  guides (fill = guide_colourbar(barwidth = 0.7, barheight = 5))+
  theme_light()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="sans"))+
  annotation_scale(location = "bl", line_width = 2,text_cex = 0.7,text_family="sans")+  
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering, 
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))
dev.off()

### Annual mean rate of deforestation by municipality (%) ###

md_vt=plot(mean_mvter, comparisons = TRUE,type="response")+ 
  theme_light()+
  theme(
    axis.text.y = element_text(size = 16),
    axis.text.x= element_text(size=16),
    axis.title = element_text(size=20))+#,
  scale_x_continuous(limits = c(0.5,6.5), breaks = c(0.5,2,3.5,5,6.5))+
  labs(y="",x="Annual mean rate of deforestation (%)")
ggsave(filename = "C:/My_Folder/fig5_LUP.jpeg", plot=md_vt,width = 420, height = 297, units = "mm", dpi =500)


####  Perception-based scenarios ###

md_iat=plot(mean_iat, comparisons = TRUE,type="response",alpha=0.1)+ 
  theme_light()+
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x= element_blank(),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))+
  scale_x_continuous(limits = c(1.2,4.2), breaks = c(1.2,1.8,2.4,3,3.6,4.2))+
  scale_y_discrete(labels=c("PE"="Pessimistic","OP"="Optimistic","Nini"="Neutral"))+
  labs(y="Perception-based scenario",x="",subtitle="Pillar: Ifrastructure and land adaptation (ILA)")

md_rccp=plot(mean_rccp, comparisons = TRUE,type="response")+ 
  theme_light()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x= element_text(size=12),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))+
  scale_x_continuous(limits = c(1.2,4.2), breaks = c(1.2,1.8,2.4,3,3.6,4.2))+
  labs(y="",x="Average annual deforestation rates (%)",subtitle="Pillar: Reconciliation, coexistence, and peace (RCP)")

md_vas=plot(mean_vas, comparisons = TRUE,type="response",alpha=0.1)+ 
  theme_light()+
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x= element_text(size=12),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))+
  scale_x_continuous(limits = c(1.2,4.2), breaks = c(1.2,1.8,2.4,3,3.6,4.2))+
  scale_y_discrete(labels=c("PE"="Pessimistic","OP"="Optimistic","Nini"="Neutral"))+
  labs(y="Perception-based scenario",x="Average annual deforestation rates (%)",subtitle="Pillar: Housing, drinking water and sanitation (HWS)")

pilmd=plot_grid(md_iat,md_rccp,md_vas,align = "hv",nrow=2,ncol=2,labels = "AUTO")
ggsave(filename = "C:/My_Folder/fig6_LUP.jpeg", plot=pilmd,width = 420, height = 297, units = "mm", dpi =500)


####  Smooth terms ###

#p_mpa <- getViz(mpa)
p_mvt <- getViz(mvter)
p_dgx <- getViz(mgextc)

# pa=plot( sm(p_mpa, 1),seWithMean = TRUE,trans=exp)+
#   l_rug() + l_ciPoly(level=0.95,alpha=0.5) +
#   l_fitLine()+
#   labs(x="Environmental pressure",y="Annual rate of deforestation (%)",subtitle = "Deviance explained = 59.7%; p<0.01")+#labs(y=Log(E(Y))) #subtitle = "Devianza explicada = 59.7%; p = 5.58e-04; edf = 1.80"
#   theme_bw()+
#   theme(
#     axis.title = element_text(size=12),
#     axis.text.x = element_text(size=10),
#     axis.text.y = element_text(size=10),
#     plot.subtitle = element_text(size=12))

vt=plot( sm(p_mvt, 1),seWithMean = TRUE,trans=exp)+
  l_rug() + l_ciPoly(level=0.95,alpha=0.5) +
  l_fitLine()+
  labs(x="Distance to tertiary roads (km)",y="Annual rate of deforestation (%)")+#,subtitle = "Deviance explained = 64.5%; p<0.01"
  theme_bw()+
  theme(
    axis.title = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    plot.subtitle = element_text(size=12))

dg=plot( sm(p_dgx, 1),seWithMean = TRUE,trans=exp)+
  l_rug() + l_ciPoly(level=0.95,alpha=0.5) +
  l_fitLine()+
  labs(x="Distance to extensive livestock production areas with a high level of technology (km)",
       y="Annual rate of deforestation (%)")+#,subtitle = "Deviance explained = 57.7%; p<0.01"
  theme_bw()+
  theme(
    axis.title = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    plot.subtitle = element_text(size=12))

#pa_gg=pa$ggObj
vt_gg=vt$ggObj
dg_gg=dg$ggObj
pmod=plot_grid(vt_gg,dg_gg,align = "hv",nrow=2,ncol=1,labels="AUTO")
ggsave(filename = "C:/My_Folder/fig8_LUP.jpeg", plot=pmod,width = 270, height = 180, units = "mm", dpi =500)


### Bi-dimensional smooth terms ###

p_pyt <- getViz(mpyt)

jpeg(file = "C:/My_Folder/fig7_LUP.jpeg", width = 300, height = 183, units = "mm", res =500)

sb_pyt=plot(sm(p_pyt, 1),seWithMean = TRUE,too.far = 0.1,trans=exp)+
  l_fitRaster(na.rm = TRUE) +
  scale_fill_viridis_c(option="D",na.value = NA)+
  l_fitContour()+
  labs(title="",x="Difference in temperature between 2010 and 2017 (°C)",y="Difference in precipitation between 2010 and 2017 (mm)",
       fill="Annual rate of \ndeforestation (%)")+#,subtitle = "Deviance explained = 58.1%; p<0.01"
  theme_bw()+
  theme(
    axis.title = element_text(size=12),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    legend.title = element_text(hjust=0.5,size=12),
    legend.text = element_text(size=10))

sb_pyt
dev.off()

# sbpyt_gg=sb_pyt$ggObj
# 
# sbmod2=plot_grid(pa_gg,sbpyt_gg,align = "hv",nrow=2,ncol=1,labels = "AUTO")
# ggsave(filename = "C:/My_Folder/fig7_LUP.jpeg", plot=sbmod2,width = 300, height = 183, units = "mm", dpi =500)


####  Parametric terms ###

p_iat <- getViz(mxy_iat3)
mp_iat=plot(pterm(p_iat, 1))+l_ciBar(level=0.95)+l_rug()+l_fitPoints(size = 2, col = 2)+
  #+l_points(shape = 19, size = 1, alpha = 0.1)+ 
  labs(x="",y="Partial effect on log\n(Annual rate of deforestation)",subtitle = "Pillar: Ifrastructure and land adaptation (ILA)")+
  scale_y_continuous(limits = c(-0.5,0.3), breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3))+
  theme_light()+
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x= element_blank(),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))

p_rccp <- getViz(mxy_rccp3)
mp_rccp=plot(pterm(p_rccp, 1))+l_ciBar(level=0.95)+l_rug()+l_fitPoints(size = 2, col = 2)+
  labs(x="Perception-based outlook",y="",subtitle = "Pillar: Reconciliation, coexistence, and peace (RCP)")+
  scale_y_continuous(limits = c(-0.5,0.3), breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3))+
  theme_light()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x= element_text(size = 12),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))+
  scale_x_discrete(labels = c(
    "Nini" = "Neutral",
    "OP" = "Optimistic",
    "PE" = "Pessimistic"))

p_vas<- getViz(mxy_vas3)
mp_vas=plot(pterm(p_vas, 1))+l_ciBar(level=0.90)+l_rug()+l_fitPoints(size = 2, col = 2)+
  labs(x="Perception-based outlook",y="Partial effect on log\n(Annual rate of deforestation)",subtitle = "Pillar: Housing, drinking water and sanitation (HWS)")+
  theme_light()+
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x= element_text(size = 12),
    axis.title = element_text(size=16),
    plot.subtitle = element_text(size=16))+
  scale_y_continuous(limits = c(-0.5,0.3), breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3))+
  scale_x_discrete(labels = c(
    "Nini" = "Neutral",
    "OP" = "Optimistic",
    "PE" = "Pessimistic"))

iat_gg=mp_iat$ggObj
rccp_gg=mp_rccp$ggObj
vas_gg=mp_vas$ggObj

p=plot_grid(iat_gg,rccp_gg,vas_gg,align = "hv",nrow=2,ncol=2,labels = "AUTO")
p
ggsave(filename = "C:/My_Folder/partialef_PDET.png", plot=p,width = 420, height = 297, units = "mm", dpi =500)



####  Model predictions and confidence intervals  ###

add_predictions_to_df <- function(df, model_list, suffixes = NULL) {
  if (is.null(suffixes)) {
    suffixes <- names(model_list)
    if (is.null(suffixes)) stop("Please provide suffixes or name the models in the list.")
  }
  
  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    suf <- suffixes[i]
    
    preds <- predict(model, newdata = df, se.fit = TRUE)
    
    df[[paste0("fit_", suf)]] <- preds$fit
    df[[paste0("se_", suf)]] <- preds$se.fit
    df[[paste0("yt_", suf)]]  <- exp(preds$fit)
    df[[paste0("cil_", suf)]] <- exp(preds$fit - 1.96 * preds$se.fit)
    df[[paste0("ciu_", suf)]] <- exp(preds$fit + 1.96 * preds$se.fit)
  }
  
  return(df)
}

# List of GAM models
models <- list(iat = mxy_iat3, rcp = mxy_rccp3, hws = mxy_vas3,
               mvt = mvter, dgex = mgextc, pyt = mpyt)

p1019_updated <- add_predictions_to_df(p1019, model_list = models)

# fit_iat <- predict(mxy_iat3, se.fit=T)
# p1019$fit_iat=fit_iat[['fit']]
# p1019$fit_iat=fit_iat[['se.fit']]
# p1019$fit_iat=fit_iat[['fit']]
# p1019$se_iat=fit_iat[['se.fit']]
# p1019$yt_iat=exp(p1019$fit_iat)
# p1019$cil_iat=exp(p1019$fit_iat-1.96*p1019$se_iat)
# p1019$ciu_iat=exp(p1019$fit_iat+1.96*p1019$se_iat)

mapa_joined <- nucleos %>%
  left_join(p1019_updated %>% select(mun_nv, fit_iat:ciu_pyt), by = "mun_nv")

mapa_long <- mapa_joined %>%
  pivot_longer(
    cols = matches("^(fit|cil|ciu)_(iat|rcp|hws|mvt|dgex|pyt)$"),
    names_to = c("type", "model"),
    names_pattern = "(fit|cil|ciu)_(.*)"
  )

# Define model labels and order
model_labels <- c(mvt = "Model 1",
                  dgex = "Model 2",
                  pyt = "Model 3",
                  iat = "Model 4 - ILA", 
                  hws = "Model 4 - HWS", 
                  rcp = "Model 5 - RCP")

# Order rows: fit, cil, ciu
mapa_long <- mapa_long %>%
  mutate(bound = factor(case_when(
    type == "fit" ~ "Fitted",
    type == "cil" ~ "Lower 95% CI",
    type == "ciu" ~ "Upper 95% CI"
  ), levels = c("Lower 95% CI", "Fitted", "Upper 95% CI")),
  model = factor(model, levels = names(model_labels), labels = model_labels))

plot_model_maps <- function(models_to_show, data = mapa_long) {
  ggplot(data %>% filter(model %in% model_labels[models_to_show])) +
    geom_sf(aes(fill = value), color = "white", lwd = 0.1) +
    facet_grid(bound ~ model) +
    scale_fill_viridis_c(na.value = "transparent", name = "Annual rate of\ndeforestation (%)") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12),
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}

page1 <- plot_model_maps(c("iat", "rcp", "hws"))
page2 <- plot_model_maps(c("mvt", "dgex", "pyt"))

# View or save each
print(page1)
#ggsave("C:/My_Folder/fit_ci_maps1.png", page1, width = 10, height = 8, dpi =500)

print(page2)
#ggsave("C:/My_Folder/fit_ci_maps2.png", page2, width = 10, height = 8, dpi =500)
