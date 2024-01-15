#########################################################################
############## Analisis conservacion macros definitivo ##################
#########################################################################



#### Importacion de datos y creacion de matrices de macros ####

#setwd("C:/Users/josem/Desktop/Doctorado/UC Berkeley/Datos MECODISPER/R") ## Portatill
#setwd("C:/Users/Jose Fernandez/Desktop/Datos MECODISPER/R") ## Ordenador facultad


# setwd(choose.dir())

#install.packages("purrr")

library("readr")
library(vegan)
#library(iNEXT)
#library(adespatial)
library(ggplot2)
library(corrplot)
library(patchwork)
library(dplyr)
library(GGally)
library(Hmisc) # Esto es para hacer las correlaciones
library(corrplot)
library(PerformanceAnalytics)

##### Importacion de datos y creacion de matrices

## Macros

datosmacros <- read_csv2("Matriz_macros.csv", skip=0, col_names=TRUE)
datosmacros<-as.data.frame(datosmacros)
macros_total<-datosmacros[,-1]
rownames(macros_total)<-datosmacros[,1]
dimnames(macros_total)

#View(datosmacros)
#View(macros_total)





#


#### Transformacion de datos de macros ####

###Transformacion con log. A partir de aqui todos mis datos son con la transformacion de log

## Macros

macros_total_indval <- macros_total

macros_total <- log10(macros_total[,-c(1:4)] + 1)


#

#### Importacion de variables ambientales macros (FQ, IHF e hidrologicas) ####

### Fisico quimica


datosFQ <- read_csv2("FQ_macros.csv", skip=0, col_names=TRUE)
datosFQ <-as.data.frame(datosFQ)
FQ_total <-datosFQ[,-1]
rownames(FQ_total)<-datosFQ[,1]
dimnames(FQ_total)
#View(datosFQ)
#View(FQ_total)


## Aqui voy a recuperar las columnas categoricas para repescarlas mas adelante

categoricas_total <- FQ_total [1:4]
categoricas_total



# Aqui hago la estandarizacion de los datos con la funcion scale


FQ_total = scale(FQ_total [,-c(1:4)])
FQ_total





### IHF & CC

datosIHF <- read_csv2("IHF_CC.csv", skip=0, col_names=TRUE)
datosIHF <-as.data.frame(datosIHF)
IHF_total <-datosIHF[,-1]
rownames(IHF_total)<-datosIHF[,1]
dimnames(IHF_total)

#View(datosIHF)
#View(IHF_total)

## Elimino la columna del IHF y la de CC

IHF_total <- (select(IHF_total, -c(IHF, CC)))



# Aqui hago la estandarizacion de los datos con la funcion scale


IHF_total = scale(IHF_total [,-c(1:4)])
IHF_total



### Flow permanence

## FP por cada estacion en la ventana de 100 y 50 dias

datosFP_100_50 <- read.csv2("FP_100_50.csv")
datosFP_100_50
class(datosFP_100_50)



# Datos FP_100_50 total

FP_100_50_total <- read.csv2("FP_100_50_total.csv")
FP_100_50_total


# Estandarizo con scale




FP_100_50_total = scale(FP_100_50_total [, -c(1:3)])
FP_100_50_total



### ST con ### CUIDADO, QUE TIENES QUE METER EL ARCHIVO CADA VEZ QUE ABRES EL r, CREO)



#STcon_winter_bruto <- STcon_out
#STcon_winter_bruto
## Cambio el nombre a dos columnas (A2 es B1 y B1 es A2 --> además, actualizo los nombres a los nuevos)
#colnames (STcon_winter_bruto) <- c ("Riera", "Codi_HOBO", "Latitud", "Longitud", "ID", "DtoU", "ID_UpDo", "Samp_ID", "Samp_UD_Let", "Samp_UtoD", "DirBin", "UndBin", "DirWei", "UndWei")
#STcon_winter_bruto

#write.csv2(STcon_winter_bruto, file = "STcon_winter_bruto.csv")


#STcon_spring_bruto <- STcon_out
#STcon_spring_bruto
## Cambio el nombre a dos columnas (A2 es B1 y B1 es A2 --> además, actualizo los nombres a los nuevos)
#colnames (STcon_spring_bruto) <- c ("Riera", "Codi_HOBO", "Latitud", "Longitud", "ID", "DtoU", "ID_UpDo", "Samp_ID", "Samp_UD_Let", "Samp_UtoD", "DirBin", "UndBin", "DirWei", "UndWei")
#STcon_spring_bruto
#write.csv2(STcon_spring_bruto, file = "STcon_spring_bruto.csv")


#STcon_summer_bruto <- STcon_out
#STcon_summer_bruto
## Cambio el nombre a dos columnas (A2 es B1 y B1 es A2 --> además, actualizo los nombres a los nuevos)
#colnames (STcon_summer_bruto) <- c ("Riera", "Codi_HOBO", "Latitud", "Longitud", "ID", "DtoU", "ID_UpDo", "Samp_ID", "Samp_UD_Let", "Samp_UtoD", "DirBin", "UndBin", "DirWei", "UndWei")
#STcon_summer_bruto
#write.csv2(STcon_summer_bruto, file = "STcon_summer_bruto.csv")


#STcon_autumn_bruto <- STcon_out
#STcon_autumn_bruto
## Cambio el nombre a dos columnas (A2 es B1 y B1 es A2 --> además, actualizo los nombres a los nuevos)
#colnames (STcon_autumn_bruto) <- c ("Riera", "Codi_HOBO", "Latitud", "Longitud", "ID", "DtoU", "ID_UpDo", "Samp_ID", "Samp_UD_Let", "Samp_UtoD", "DirBin", "UndBin", "DirWei", "UndWei")
#STcon_autumn_bruto
#write.csv2(STcon_autumn_bruto, file = "STcon_autumn_bruto.csv")


STcon_totalB <- read.csv2("STcon_total.csv")
STcon_total <- STcon_totalB [, -c (1:4)]
STcon_total



STcon_total = scale(STcon_total)





#

### TotDur y TotNum

## Importante --> El TotNum tengo que dividirlo por el número de días que llevaban los hobos puestos en cada muestreo.
## Invierno == 173; primavera == 298; verano == 348; otoño == 480.

datostots <- read_csv2("Tots.csv", skip=0, col_names=TRUE)
datostots <-as.data.frame(datostots)
tots_total <-datostots[,-1]
rownames(tots_total)<-datostots[,1]
dimnames(tots_total)
tots_total




# Vamos a estandarizar las variables


tots_total = scale(tots_total [, -c(1:3)])
tots_total



#

#### Correlaciones de variables ####

### Uno las matrices de los diferentes datos por estaciones


variables_total <- cbind(FQ_total, IHF_total, FP_100_50_total, STcon_total, tots_total)
variables_total


### Correlaciones



chart.Correlation(variables_total, histogram = T, pch = 19) # Saca directamente el grafico de la matriz con los significativos
rcorr(as.matrix(variables_total)) # matriz de correlaciones y los p valores


#



## Eliminacion de variables redundantes (#criterio: que tengan una R > 0.70)
# https://www.maximaformacion.es/blog-ciencia-datos/analizando-la-correlacion-con-la-libreria-caret/


variables_total <- as.data.frame(variables_total)

str(variables_total); pairs(variables_total %>% dplyr::select_if(is.numeric))
library(caret)
corr <-cor(variables_total %>% dplyr::select_if(is.numeric), use="complete.obs", method="spearman")
var_eliminar <- (findCorrelation(corr, cutoff = 0.7, names = T))
var_eliminar




# Elimino variables


variables_totalB <- (select (variables_total, -c(TotDur, TotNum, FP100, EC, SPC, DO_por,)))
variables_totalB

chart.Correlation(variables_totalB, histogram = T, pch = 19) # Correlaciones despues del cribado


#

#### LCBD de macros #####

library(adespatial)

## Macros

beta_total_macros <- beta.div(macros_total, method = "hellinger", nperm = 9999)
beta_total_macros
summary(beta_total_macros)
beta_total_macros$beta # SSTotal and BDTotal

# Which species have a SCBD larger than the mean SCBD?
beta_total_macros$SCBD[beta_total_macros$SCBD >= mean(beta_total_macros$SCBD)]

#SCBD valores
beta_total_macros$SCBD
# LCBD values
beta_total_macros$LCBD
# p-values
beta_total_macros$p.LCBD
# Holm correction
p.adjust(beta_total_macros$p.LCBD, "holm")
# Sites with significant Holm-corrected LCBD value
row.names(macros_total[which(p.adjust(beta_total_macros$p.LCBD, "holm") <= 0.05),])

summary(beta_total_macros$SCBD)

beta_total_macros$LCBD
LCBD_total <- beta_total_macros$LCBD
LCBD_total

SCBD_total <- beta_total_macros$SCBD
SCBD_total

# Que sitios tienen un LCBD por encima y por debajo de la media

beta_total_macros$LCBD[beta_total_macros$LCBD >= mean(beta_total_macros$LCBD)] # Por encima de la media
beta_total_macros$LCBD[beta_total_macros$LCBD <= mean(beta_total_macros$LCBD)] # Por debajo de la media





# Exportar los datos
#write.csv2(LCBDHigh_autumn_macros, file = "LCBD_High")
#write.csv2(LCBDLow_autumn_macros, file = "LCBD_Low")


#### Adjunto la LCBD a todas las variables ####

## Macros

variables_totalC <- cbind(categoricas_total,variables_totalB, LCBD_total)
variables_totalC



#




#### Frecuencia de seleccion de Marxan ####

### Importar datos

## Macros


Fre_sel_marxan <- read.csv2("Marxan_frecuencia_seleccion.csv")
class(Fre_sel_marxan)



#

#### Adjunto la frecuencia de seleccion a todas las variables ####

## Macros

#Fre_sel_marxan <- as.numeric(Fre_sel_marxan)

Fre_sel_marxan <- Fre_sel_marxan[, -c(1:4)]
Fre_sel_marxan <- as.numeric(Fre_sel_marxan)

variables_totalC_frecuencia <- cbind(categoricas_total, variables_totalB, Fre_sel_marxan)
variables_totalC_frecuencia



#




#### H1 macros:¿Que tiene más peso para expliar la distribucion de las especies de macros? ####

## Utilizamos datos totales de macros y hacemos análisis con Shannon?, LCBD y marxan

## variables de diversidad (Shannon?, LCBD y marxan)

Selection_frequency <- Fre_sel_marxan
#Selection_frequency <- as.numeric(Fre_sel_marxan$Frequency)

class(Selection_frequency)


variables_total_h1 <- cbind(categoricas_total,variables_totalB, LCBD_total, Selection_frequency)
variables_total_h1

colnames(variables_total_h1) <- c("Reach", "Stream", "Season", "AE", "pH", "DO_mgL", "T", "TDS", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "FP50", "DirBin", "UndBin", "DirWei", "UndWei",  "TotLeng", "LCBD", "MSF")

# We scale all the variables except those that are categorical and LCBD & Freq.Selection
## WARNING: Freq.Selextion appears as a character variable
variables_total_h1[,7:(ncol(variables_total_h1)-2)] <- scale(variables_total_h1[,7:(ncol(variables_total_h1)-2)], center = T, scale = T)


## Analisis con LCBD (beta regression)

library(glmmTMB)

# We run the model and will obtain a result without warnings
## Take a look at how the random is built, we are considering the two elements together in the same random.
## I also saw that it is better to write the family as "beta_family"
## I also found that the problem could come from values not being standarized and leading to below 0 estimations that
##fuck the model and make it appar with NA (https://cran.r-project.org/web/packages/glmmTMB/vignettes/troubleshooting.html)




ph_total_ani <- glmmTMB(LCBD_total~pH + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(ph_total_ani)

oxi_total_ani <- glmmTMB(LCBD_total~DO_mgL + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(oxi_total_ani)

t_total_ani <- glmmTMB(LCBD_total~T + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(t_total_ani)

tds_total_ani <- glmmTMB(LCBD_total~TDS + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(tds_total_ani)

b1_total_ani <- glmmTMB(LCBD_total ~ B1 + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b1_total_ani) # Strong evidence #

b2_total_ani <- glmmTMB(LCBD_total~B2 + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b2_total_ani) # Strong evidence #

b3_total_ani <- glmmTMB(LCBD_total~B3+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b3_total_ani) # Strong evidence #

b4_total_ani <- glmmTMB(LCBD_total~B4+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b4_total_ani) # Weak evidence #

b5_total_ani <- glmmTMB(LCBD_total~B5+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b5_total_ani)

b6_total_ani <- glmmTMB(LCBD_total~B6+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b6_total_ani)

b7_total_ani <- glmmTMB(LCBD_total~B7+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(b7_total_ani) # Weak evidence #

fp_50_total_ani <- glmmTMB(LCBD_total~FP50 + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(fp_50_total_ani)

DirBin_total_ani <- glmmTMB(LCBD_total~DirBin+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(DirBin_total_ani)

DirWei_total_ani <- glmmTMB(LCBD_total~DirWei+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(DirWei_total_ani) # Moderate evidence #

UndBin_total_ani <- glmmTMB(LCBD_total~UndBin+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(UndBin_total_ani) # Moderate evidence #

UndWei_total_ani <- glmmTMB(LCBD_total~UndWei+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(UndWei_total_ani)

totleng_total_ani <- glmmTMB(LCBD_total~TotLeng+ (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(totleng_total_ani)




# Graficos ggplot anidado macroshttp://127.0.0.1:35729/graphics/plot_zoom_png?width=1920&height=1017


plot1_total_ani <- ggplot(variables_total_h1, aes(B1, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
                       theme_bw() + 
 labs(x = "B1", y = "LCBD", subtitle = "A") + 
  theme(axis.title = element_text(size = 20,
                                   color = "black",
                                   face = "bold")) + # esto es el tipo de letra
theme(legend.position = "none")
  
plot1_total_ani



plot2_total_ani <- ggplot(variables_total_h1, aes(B2, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "B") + 
  xlab("B2") + 
  ylab("LCBD") + 
  theme(axis.title.y =  element_blank())+ 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
 theme(legend.position = "none")


plot2_total_ani



plot3_total_ani <- ggplot(variables_total_h1, aes(B3, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "C")+ 
  xlab("B3") + 
  ylab("LCBD") + 
  theme(axis.title.y =  element_blank()) + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"))
        

plot3_total_ani



plot4_total_ani <- ggplot(variables_total_h1, aes(B4, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(x = "B4", y = "LCBD", subtitle = "D") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) # esto es el tipo de letra
  

plot4_total_ani



plot5_total_ani <- ggplot(variables_total_h1, aes(B7, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "E")+ 
  xlab("B7") + 
  ylab("LCBD") + 
  theme(axis.title.y =  element_blank()) + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) # esto es el tipo de letra

plot5_total_ani



plot6_total_ani <- ggplot(variables_total_h1, aes(DirWei, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(x = "Dispersal resistance (DirWei)", y = "LCBD", subtitle = "D") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
theme(legend.position = "none")

plot6_total_ani


plot7_total_ani <- ggplot(variables_total_h1, aes(UndBin, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "E")+ 
  xlab("Connectivity (UndBin)") + 
  ylab("LCBD") + 
  theme(axis.title.y =  element_blank()) + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) +  # esto es el tipo de letra
theme(legend.position = "none")

plot7_total_ani






### Analisis con marxan (frecuencia de selección)

library(nlme)
#library(lme4)
#library(lmerTest)
library(MuMIn)

variables_total_h1

variables_total_h1$Selection_frequency


ph_total_ani_marxan <- lme(MSF~pH, random = ~ 1|(Stream/Reach),data=variables_total_h1)
anova(ph_total_ani_marxan)
#summary(ph_total_ani_marxan)
#r.squaredGLMM(ph_total_ani_marxan)


oxi_total_ani_marxan <- lme(MSF~DO_mgL, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(oxi_total_ani_marxan)


t_total_ani_marxan <- lme(MSF~T , random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(t_total_ani_marxan)


tds_total_ani_marxan <- lme(MSF~TDS, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(tds_total_ani_marxan)


b1_total_ani_marxan <- lme(MSF ~ B1 , random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b1_total_ani_marxan)


b2_total_ani_marxan <- lme(MSF~B2 , random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b2_total_ani_marxan)


b3_total_ani_marxan <- lme(MSF~B3, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b3_total_ani_marxan)


b4_total_ani_marxan <- lme(MSF~B4, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b4_total_ani_marxan)


b5_total_ani_marxan <- lme(MSF~B5, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b5_total_ani_marxan)


b6_total_ani_marxan <- lme(MSF~B6, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b6_total_ani_marxan)


b7_total_ani_marxan <- lme(MSF~B7, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(b7_total_ani_marxan) # Strong evidence #
#summary(b7_total_ani_marxan)
r.squaredGLMM(b7_total_ani_marxan)

fp50_total_ani_marxan <- lme(MSF~FP50, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(fp50_total_ani_marxan)

DirBin_total_ani_marxan <- lme(MSF~DirBin, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(DirBin_total_ani_marxan)


DirWei_total_ani_marxan <- lme(MSF~DirWei, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(DirWei_total_ani_marxan)


UndBin_total_ani_marxan <- lme(MSF~UndBin, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(UndBin_total_ani_marxan)


UndWei_total_ani_marxan <- lme(MSF~UndWei, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(UndWei_total_ani_marxan)


totleng_total_ani_marxan <- lme(MSF~TotLeng, random = ~ 1| (Stream/Reach),data=variables_total_h1)
anova(totleng_total_ani_marxan)




# Graficos ggplot anidado marxan macros




plot3_total_ani_marxan <- ggplot(variables_total_h1, aes(B7, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw()+ 
  labs(x = "B7", y = "MSF", subtitle = "F") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) # esto es el tipo de letra

plot3_total_ani_marxan




# Uno los graficos

plot_total_LCBD_MSF <- plot1_total_ani + plot2_total_ani + plot3_total_ani  + plot6_total_ani + plot7_total_ani + plot3_total_ani_marxan
plot_total_LCBD_MSF

## Esto es para cambiar los subtitulos de cada uno de los gráficos (el A,B,C...de los gráficos)  
## plot.subtitle =element_text(size = 20,
##  color = "black",
##  face = "bold") ) 


#

#### H2 macros: ¿Hay variabilidad estacional entre los sitios seleccionados por marxan  LCBD? ####


## Graficos


## Preparo datos

# Macros

Fre_sel_marxan
Fre_sel_macros_total <- Fre_sel_marxan#$Frequency
class(Fre_sel_marxan)
#Fre_sel_macros_total <- as.numeric(Fre_sel_macros_total)


categoricas_total <- datosFQ[1:5]
categoricas_total



STcon_480_code <- read.csv2("STcon_480.csv")
STcon_480 = scale (STcon_480_code[, -c(1:2)])
STcon_480

Flow_permanence_code <- read.csv2 ("Flow_permanence_total.csv")
Flow_permanence = scale(Flow_permanence_code[, -c(1,2,3,5) ] )
Flow_permanence

LCBD_marxan <- cbind(categoricas_total, LCBD_total, Fre_sel_macros_total, STcon_total, STcon_480) 
# Esto hay que quitarlo si este codigo va bien -> colnames(LCBD_marxan) <- c("Code", "Reach", "Stream", "Season", "AE", "LCBD_total", "Fre_sel_macros_total", "DirBin", "UndBin", "DirWei", "UndWei", "DirBin_480", "DirWei_480", "UndBin_480", "UndWei_480")
LCBD_marxan

LCBD_marxan_fp <- cbind(categoricas_total, LCBD_total, Fre_sel_macros_total, STcon_total, Flow_permanence) 
# Esto hay que quitarlo si este codigo va bien -> colnames(LCBD_marxan) <- c("Code", "Reach", "Stream", "Season", "AE", "LCBD_total", "Fre_sel_macros_total", "DirBin", "UndBin", "DirWei", "UndWei", "DirBin_480", "DirWei_480", "UndBin_480", "UndWei_480")
LCBD_marxan_fp


df_orden_ST480 <- LCBD_marxan[order (LCBD_marxan$UndWei_480),]
df_orden_ST480 <- mutate(df_orden_ST480, Code=factor(Code, levels = df_orden_ST480$Code))

df_orden_FP <- LCBD_marxan[order (LCBD_marxan_fp$Flow_permanence),]
df_orden_FP <- mutate(df_orden_FP, Code=factor(Code, levels = df_orden_FP$Code))

#

## Graficos

# Macros

### Version requete buena by cunillera-miguel-yo y tol mundo :)


df_orden_ST480$UndWei_480

unique(df_orden_ST480$Reach) # saca el orden sin duplicar de los reach
       
       
LCBD_macros_plot <- ggplot(df_orden_ST480, aes(Reach, Season, col = UndWei)) + 
  ggtitle("C) LCBD macroinvertebrates") + theme_bw() +
  geom_point(aes(size = LCBD_total)) +
  scale_size(range = c(.1, 5), name="LCBD")+
  theme(legend.text=element_text(size=10)) + # esto es para el size de las etiquetas de rios
  scale_color_gradient(low="blue", high="yellow")+
  scale_x_discrete(limits=unique(df_orden_ST480$Reach), breaks=unique(df_orden_ST480$Reach))+
  scale_y_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) + # ordenar las estaciones de arriba a abajo
theme(axis.title = element_text(size = 20,
                                  color = "black")) +
  theme(axis.title.x =  element_blank()) +
  theme (legend.key.size = unit (3, 'mm')) #cambiar el tamaño de la clave de la leyenda 


LCBD_macros_plot 



## Plot con el orden de Flow Permanence

df_orden_ST480$UndWei_480

unique(df_orden_FP$Reach) # saca el orden sin duplicar de los reach


LCBD_macros_plot_2 <- ggplot(df_orden_FP, aes(Reach, Season, col = Flow_permanence))+ ggtitle("C) Uniqueness macroinvertebrates") + theme_bw() +
  geom_point(aes(size = LCBD_total)) +
  scale_size(range = c(.1, 10), name="LCBD")+
  theme(legend.text=element_text(size=10)) + # esto es para el size de las etiquetas de rios
  scale_color_gradient(low="yellow", high="blue")+
  scale_x_discrete(limits=unique(df_orden_FP$Reach), breaks=unique(df_orden_FP$Reach))+
  scale_y_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) + # ordenar las estaciones de arriba a abajo
  theme(axis.title = element_text(size = 20,
                                  color = "black"))
  

LCBD_macros_plot_2




marxan_macros_plot <- ggplot(df_orden_ST480, aes(Reach, Season, col = UndWei)) + 
  ggtitle("D) MSF macroinvertebrates") + theme_bw() +
  geom_point(aes(size = Fre_sel_macros_total)) +
  scale_size(range = c(.1, 5), name="MSF")+
  theme(legend.text=element_text(size=10)) + # esto es para el size de las etiquetas de rios
  scale_color_gradient(low="blue", high="yellow")+
  scale_x_discrete(limits=unique(df_orden_ST480$Reach), breaks=unique(df_orden_ST480$Reach))+
  scale_y_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) + # ordenar las estaciones de arriba a abajo
  theme(axis.title = element_text(size = 20,
                                  color = "black")) +
  theme (legend.key.size = unit (3, 'mm'))

marxan_macros_plot



plots_macros <- LCBD_macros_plot + marxan_macros_plot +
  plot_layout(ncol = 1) # Pongo una cola columna y así pongo uno bajo el otro
plots_macros




#

# Gráfico hidrológicas puntos ordenados. ## EL DIOOOOOOOOOOS DAVIIIDDDDD HA HABLADDOOOOOOO ##

#df_orden_UB_winter <- filter(df_orden_UB, Season=="Winter")

#LCBD_macros_plot_UB_winter <- ggplot(df_orden_UB_winter, aes(x=Code, y = 1, col = UndBin))+ ggtitle("LCBD macroinvertebrates by UB") + theme_bw() +
#  labs(y="Winter") +
 # geom_point(aes(size = LCBD_total)) +
  #scale_size(range = c(.1, 10), name="LCBD")+
#  theme(legend.text=element_text(size=10), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
 # scale_color_gradient(low="blue", high="red")



#df_orden_UB_spring <- filter(df_orden_UB, Season=="Spring")

#LCBD_macros_plot_UB_spring <- ggplot(df_orden_UB_spring, aes(x=Code, y = 1, col = UndBin))+ ggtitle("LCBD macroinvertebrates by UB") + theme_bw() +
#labs(y="Spring") +
# geom_point(aes(size = LCBD_total)) +
#  scale_size(range = c(.1, 10), name="LCBD")+
#  theme(legend.text=element_text(size=10), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
#  scale_color_gradient(low="blue", high="red")



#df_orden_UB_summer <- filter(df_orden_UB, Season=="Summer")

#LCBD_macros_plot_UB_summer <- ggplot(df_orden_UB_summer, aes(x=Code, y = 1, col = UndBin))+ ggtitle("LCBD macroinvertebrates by UB") + theme_bw() +
# labs(y="Summer") +
# geom_point(aes(size = LCBD_total)) +
# scale_size(range = c(.1, 10), name="LCBD")+
# theme(legend.text=element_text(size=10), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
# scale_color_gradient(low="blue", high="red") 



#df_orden_UB_autumn <- filter(df_orden_UB, Season=="Autumn")

#LCBD_macros_plot_UB_autumn <- ggplot(df_orden_UB_autumn, aes(x=Code, y = 1, col = UndBin))+ ggtitle("LCBD macroinvertebrates by UB") + theme_bw() +
# labs(y="Autumn") +
# geom_point(aes(size = LCBD_total)) +
# scale_size(range = c(.1, 10), name="LCBD")+
# theme(legend.text=element_text(size=10), legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
# scale_color_gradient(low="blue", high="red")
 


#Uno encima de otros
#x <- LCBD_macros_plot_UB_winter / LCBD_macros_plot_UB_spring / LCBD_macros_plot_UB_summer / LCBD_macros_plot_UB_autumn
#x

#Uno al lado del otro 2x2
#b <- LCBD_macros_plot_UB_winter + LCBD_macros_plot_UB_spring + LCBD_macros_plot_UB_summer + LCBD_macros_plot_UB_autumn
#b


## Legendas del gráfico

library(cowplot)
library(gridExtra)

#legend_plots <- get_legend(ggplot(df_orden_UB, aes(x=Code, y = 1, col = UndBin ))+ ggtitle("LCBD macroinvertebrates by UB") + theme_bw() +
#labs(y="Winter") +
# geom_point(aes(size = LCBD_total)) +
# scale_size(range = c(.1, 10), name="LCBD")+
# theme(legend.text=element_text(size=10), 
#      legend.box = "horizontal") + scale_color_gradient(low="blue", high="red"))





#a <- grid.arrange(LCBD_macros_plot_UB_winter, LCBD_macros_plot_UB_spring, LCBD_macros_plot_UB_summer, LCBD_macros_plot_UB_autumn, legend_plots, nrow=3, ncol=2)


#grid.arrange(a) # esto es para exportar la imagen

#

#### H2 macros continuacion: Boxplot de la LCBD por season y comparaciones ####



LCBD_total_df <- as.data.frame(LCBD_total)
class(LCBD_total_df)


categoricas_total

datos_corr_total


## Boxplot con ggplot (https://rpubs.com/aafernandez1976/boxplot)



# Macros
##


plot_LCBD_macros_vio <- ggplot(LCBD_marxan, aes(x=Season, y=LCBD_total, fill = Season)) +
  geom_violin() +
  geom_boxplot(width=0.2, color="black", alpha=0.2) +
  geom_jitter() + # para que se vean los puntos en el gráfico
  theme_bw() + ggtitle("A) LCBD macroinvertebrates") + 
  scale_fill_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  scale_x_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) # ordenar las estaciones de arriba a abajo


# aquí ponemos las letras por grupos significativos

plot_LCBD_macros_vio <- plot_LCBD_macros_vio + geom_text(aes(x = 1, y = 0.0055,
                                     label = "a"),
                                 stat = "unique") +
  geom_text(aes(x = 2, y = 0.0055,
                label = "b"),
            stat = "unique") +
  geom_text(aes(x = 3, y = 0.0055,
                label = "b"),
            stat = "unique") +
  geom_text(aes(x = 4, y = 0.0055,
                label = "b"),
            stat = "unique")
  

plot_LCBD_macros_vio





plot_marxan_macros_vio <- ggplot(LCBD_marxan, aes(x=Season, y=Fre_sel_macros_total, fill = Season)) +
  geom_violin() +
  geom_boxplot(width=0.2, color="black", alpha=0.2) +
  geom_jitter() + # para que se vean los puntos en el gráfico
  theme_bw() + ggtitle("Frequency selection macroinvertebrates") +
  scale_fill_manual(values = c("Winter" = "Blue",
                               "Spring" = "green4",
                               "Summer" = "Yellow",
                               "Autumn" = "brown")) +
  scale_x_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) # ordenar las estaciones de arriba a abajo


# aquí ponemos las letras por grupos significativos

plot_marxan_macros_vio <- plot_marxan_macros_vio +
  geom_text(aes(x = 1, y = -0.05,
                label = "a"),
            stat = "unique") +
  geom_text(aes(x = 2, y = -0.05,
                label = "a"),
            stat = "unique") +
  geom_text(aes(x = 3, y = -0.05,
                label = "b"),
            stat = "unique") +
  geom_text(aes(x = 4, y = -0.05,
                label = "a"),
            stat = "unique")


plot_marxan_macros_vio




## Shannon lo vamos a quitar

#plot_alfa_macros_vio <- ggplot(datos_corr_total, aes(x=Season, y=shannon_total, fill = Season)) +
#  geom_violin() +
#  geom_jitter() + # para que se vean los puntos en el gráfico
#  theme_bw() + ggtitle("Shannon diversity macroinvertebrates") +
#  scale_fill_manual(values = c("Winter" = "Blue",
#                               "Spring" = "green4",
#                               "Summer" = "Yellow",
#                              "Autumn" = "brown")) +
  #  scale_x_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) # ordenar las estaciones de arriba a abajo

#plot_alfa_macros_vio



plot_LCBD_macros_vio + plot_marxan_macros_vio



## Hacemos la comparación entre estaciones con el modelo mixto


# Para LCBD
LCBD_seasons <- glmmTMB(LCBD_total~Season + (1| Stream*Reach),data=variables_total_h1, family=beta_family(link="logit"))
summary(LCBD_seasons)


## Explicación LCBD: Esto compara el otoño con las otras estaciones. Vemos que todas las estaciones son iguales, es decir, que 
## no se diferencian entre ellas. Si pensamos en el violin plot el verano tiene menos variación, por lo que todos 
## los sitios tienen la misma importancia, pero a nivel de comparaión de las medias es casi igual que las otras
## estaciones. En cambio, es el otoño el que tiene un pico más alto, en la que nos muestra que hay sitios con una 
## LCBD mucho más alta que otros.


# Para la frecuencia de seleccion

selection_frequency <- as.numeric(variables_total_h1$Selection_frequency)

marxan_seasons <- lme(selection_frequency ~Season, random = ~ 1|(Stream/Reach),data=variables_total_h1)
anova(marxan_seasons)
summary(marxan_seasons)





## Esto es como una especie de test de Tukey. (https://glmmtmb.github.io/glmmTMB/articles/model_evaluation.html)

library(DHARMa)
library(emmeans)


emmeans(LCBD_seasons, ~Season)



#



### Pruebo a transformar las variables para que sean normales

LCBD_marxan
class(LCBD_marxan)

LCBD_total_log <- log(LCBD_marxan$LCBD_total)
Fre_sel_log <- log(LCBD_marxan$Fre_sel_macros_total) + 1

LCBD_marxan_log <- cbind(LCBD_marxan, LCBD_total_log, Fre_sel_log)
LCBD_marxan_log



hist(LCBD_total_log)
hist(Fre_sel_log)
hist(LCBD_marxan$Fre_sel_macros_total)


## Test de normalidad

#Shapiro-wilk
shapiro.test(LCBD_total_log)# Casi normal
shapiro.test(LCBD_marxan$Fre_sel_macros_total) # no es normal

#Kolmogorov-smirnov
ks.test(LCBD_total_log, "pnorm", mean = mean(LCBD_total_log), sd = sd(LCBD_total_log)) # Normal
ks.test(LCBD_marxan$Fre_sel_macros_total, "pnorm", mean = mean(LCBD_marxan$Fre_sel_macros_total, sd = sd(LCBD_marxan$Fre_sel_macros_total)))

qqnorm(LCBD_total_log) 
qqline(LCBD_total_log)


## Homogeneidad de varianza (homocedasticidad)

# Test de Bartlett se utiliza cuando los datos son normales. Es muy sensible si no lo son.

bartlett.test(LCBD_total_log, LCBD_marxan$Season)# No cumple el supuesto
bartlett.test(LCBD_marxan$Fre_sel_macros_total, LCBD_marxan$Season) # Cumple el supuesto la frecuencia # 

# Test de Levene, cuando las muestras no son del todo normales

library(car)

leveneTest(LCBD_total_log, group = LCBD_marxan$Season)
leveneTest(LCBD_marxan$Fre_sel_macros_total, group = LCBD_marxan$Season)


# Test de Fligner-Killeen

fligner.test(LCBD_total_log, LCBD_marxan$Season) # no hay homocedasticidad
fligner.test(LCBD_marxan$Fre_sel_macros_total,LCBD_marxan$Season) # Hay homocedasticidad



## Tests Kruskal-Wallis para comparar medias (si no cumple la homocedasticidad....no puedo hacer esto :( )


kruskal.test(LCBD_total_log ~ Season, data = LCBD_marxan_log) # Supuestemente no puedo hacerlo, porla no homocedasticidad

kruskal.test(Fre_sel_macros_total ~ Season, data = LCBD_marxan) # No es significativo y no hay diferencias


## ANOVA para comparar los grupos

anova_LCBD <- aov(LCBD_total_log ~ LCBD_marxan_log$Season, LCBD_marxan_log )
summary(anova_LCBD)

anova_Fre <- aov(Fre_sel_macros_total ~ LCBD_marxan_log$Season, LCBD_marxan_log )
summary(anova_Fre)


# Test de Tukey

TukeyHSD(anova_LCBD)
TukeyHSD(anova_Fre)


#### H2 macros continuacion: Qué variables explican marxan (refugios) en verano ####



variables_summerC_frecuencia



ph_summer <- lme(Fre_sel_summer_macros~pH,random=~1|Stream,data=variables_summerC_frecuencia)
anova(ph_summer)
#summary(ph_summer)

oxi_summer <- lme(Fre_sel_summer_macros~DO_por,random=~1|Stream,data=variables_summerC_frecuencia)
anova(oxi_summer)
#summary(oxi_summer)

spc_summer <- lme(Fre_sel_summer_macros~SPC,random=~1|Stream,data=variables_summerC_frecuencia)
anova(spc_summer)
#summary(spc_summer)

b2_summer <- lme(Fre_sel_summer_macros~B2,random=~1|Stream,data=variables_summerC_frecuencia)
anova(b2_summer) # significativo #
#summary(b2_summer)

b3_summer <- lme(Fre_sel_summer_macros~B3,random=~1|Stream,data=variables_summerC_frecuencia)
anova(b3_summer)
#summary(b3_summer)

b4_summer <- lme(Fre_sel_summer_macros~B4,random=~1|Stream,data=variables_summerC_frecuencia)
anova(b4_summer)
#summary(b4_summer)

b6_summer <- lme(Fre_sel_summer_macros~B6,random=~1|Stream,data=variables_summerC_frecuencia)
anova(b6_summer)
#summary(b6_summer)

b7_summer <- lme(Fre_sel_summer_macros~B7,random=~1|Stream,data=variables_summerC_frecuencia)
anova(b7_summer)
#summary(b7_summer)

DirBin_summer <- lme(Fre_sel_summer_macros~DirBin,random=~1|Stream,data=variables_summerC_frecuencia)
anova(DirBin_summer)
#summary(DirBin_summer)

DirWei_summer <- lme(Fre_sel_summer_macros~DirWei,random=~1|Stream,data=variables_summerC_frecuencia)
anova(DirWei_summer)
#summary(DirWei_summer)

UndBin_summer <- lme(Fre_sel_summer_macros~UndBin,random=~1|Stream,data=variables_summerC_frecuencia)
anova(UndBin_summer)
#summary(UndBin_summer)

UndWei_summer <- lme(Fre_sel_summer_macros~UndWei,random=~1|Stream,data=variables_summerC_frecuencia)
anova(UndWei_summer)
#summary(UndWei_summer)

totdur_summer <- lme(Fre_sel_summer_macros~TotDur,random=~1|Stream,data=variables_summerC_frecuencia)
anova(totdur_summer)
#summary(totdur_summer)

totnum_summer <- lme(Fre_sel_summer_macros~TotNum,random=~1|Stream,data=variables_summerC_frecuencia)
anova(totnum_summer)
#summary(totnum_summer)

totleng_summer <- lme(Fre_sel_summer_macros~TotLeng,random=~1|Stream,data=variables_summerC_frecuencia)
anova(totleng_summer)
#summary(totleng_summer)

betweenness_summer <- lme(Fre_sel_summer_macros~Betweenness,random=~1|Stream,data=variables_summerC_frecuencia)
anova(betweenness_summer)
#summary(betweenness_summer)

closeness_summer <- lme(Fre_sel_summer_macros~Closeness, random=~1|Stream,data=variables_summerC_frecuencia)
anova(closeness_summer)
#summary(closeness_summer)

bmeridionalis_summer <- lme(Fre_sel_summer_macros~B.meridionalis, random=~1|Stream,data=variables_summerC_frecuencia)
anova(bmeridionalis_summer)
#summary(bmeridionalis_summer)

bhaasi_summer <- lme(Fre_sel_summer_macros~B.haasi, random=~1|Stream,data=variables_summerC_frecuencia)
anova(bhaasi_summer)
#summary(bhaasi_summer)

pphoxinus_summer <- lme(Fre_sel_summer_macros~P.phoxinus, random=~1|Stream,data=variables_summerC_frecuencia)
anova(pphoxinus_summer)
#summary(pphoxinus_summer)


## Plot

plot <- ggplot(variables_summerC_frecuencia, aes(B2, Fre_sel_summer_macros)) + geom_point() +
  geom_smooth(method = "lm", se = T)
plot



#


#### H3 macros: ¿Qué especies son indicadoras de cada estación? ####




#### SCBD


# Por estacion


scbd_total <- beta_total_macros$SCBD
scbd_total



SCBD_total <- read_csv2("SCBD_seasons_total.csv", col_names = T)
SCBD_total <- as.data.frame(SCBD_total)
SCBD_total



SCBD_winter <- read_csv2("SCBD_winter.csv", col_names = T)
SCBD_winter <- as.data.frame(SCBD_winter)
SCBD_winter

SCBD_spring <- read_csv2("SCBD_spring.csv", col_names = T)
SCBD_spring <- as.data.frame(SCBD_spring)
SCBD_spring

SCBD_summer <- read_csv2("SCBD_summer.csv", col_names = T)
SCBD_summer <- as.data.frame(SCBD_summer)
SCBD_summer

SCBD_autumn <- read_csv2("SCBD_autumn.csv", col_names = T)
SCBD_autumn <- as.data.frame(SCBD_autumn)
SCBD_autumn


## Seleccionamos los 30 primeros valores

SCBD_total_30h <- SCBD_total[order(SCBD_total$SCBD, decreasing = T),]
SCBD_total_30h

SCBD_winter_30h <- SCBD_winter[order(SCBD_winter$SCBD, decreasing = T),]
SCBD_winter_30h

SCBD_spring_30h <- SCBD_spring[order(SCBD_spring$SCBD, decreasing = T),]
SCBD_spring_30h

SCBD_summer_30h <- SCBD_summer[order(SCBD_summer$SCBD, decreasing = T),]
SCBD_summer_30h

SCBD_autumn_30h <- SCBD_autumn[order(SCBD_autumn$SCBD, decreasing = T),]
SCBD_autumn_30h

### Ordeno los datos para coger las primeras 20 especies por cada estación y hacer el plot general.

LGSCBD_total <- bind_rows(
SCBD_total%>% filter(Season == "Winter") %>% arrange(desc(SCBD)) %>% slice(1:10),
SCBD_total%>% filter(Season == "Spring") %>% arrange(desc(SCBD)) %>% slice(1:10),
SCBD_total%>% filter(Season == "Summer") %>% arrange(desc(SCBD)) %>% slice(1:10),
SCBD_total%>% filter(Season == "Autumn") %>% arrange(desc(SCBD)) %>% slice(1:10))



## Gráficos Rose plots

library(radiant.data)

## Rose plot por estaciones


LGSCBD_winter = data.frame(SCBD_winter_30h)
# LGSCBD <- as_tibble(rownames_to_column(LGSCBD, var = "Taxa"))
LGSCBD_winter_s <- LGSCBD_winter[c(1:20),] # Select the most abundant species


rose_winter <- ggplot(LGSCBD_winter_s, aes(x=Taxa, y=SCBD)) +
  ggtitle("SCBD Winter macros") +
  geom_bar(aes(fill=Taxa), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  coord_polar() + labs(x = "", y = "")+
  theme_bw()

rose_winter



LGSCBD_spring = data.frame(SCBD_spring_30h)
# LGSCBD <- as_tibble(rownames_to_column(LGSCBD, var = "Taxa"))
LGSCBD_spring_s <- LGSCBD_spring[c(1:20),] # Select the most abundant species


rose_spring <- ggplot(LGSCBD_spring_s, aes(x=Taxa, y=SCBD)) +
  geom_bar(aes(fill=SCBD), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  coord_polar() + labs(x = "", y = "")



LGSCBD_summer = data.frame(SCBD_summer_30h)
# LGSCBD <- as_tibble(rownames_to_column(LGSCBD, var = "Taxa"))
LGSCBD_summer_s <- LGSCBD_summer[c(1:20),] # Select the most abundant species


rose_summer <- ggplot(LGSCBD_summer_s, aes(x=Taxa, y=SCBD)) +
  geom_bar(aes(fill=SCBD), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  coord_polar() + labs(x = "", y = "")



LGSCBD_autumn = data.frame(SCBD_autumn_30h)
# LGSCBD <- as_tibble(rownames_to_column(LGSCBD, var = "Taxa"))
LGSCBD_autumn_s <- LGSCBD_autumn[c(1:20),] # Select the most abundant species


rose_autumn <- ggplot(LGSCBD_autumn_s, aes(x=Taxa, y=SCBD)) +
  geom_bar(aes(fill=SCBD), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  coord_polar() + labs(x = "", y = "")


rose_winter + rose_spring + rose_summer + rose_autumn



## Rose plot total

## ¿Qué es este plot?
# Se cogen las 15 especies con valores más altos de SCBD en CADA estación (esto nos da 60 especies con sus SCBDs).Después se
# representan en el box plot, pero no saldrá 60 especies porque algunas están repetidas en las estaciones. Así que habrá
# un número de especies en el plot que está entre 15 (si todas las especies coinciden en las estaciones) y 60 (si ninguna coincide).



# ROSE PLOT
rose_plot_macros <- ggplot(LGSCBD_total, aes(x=Taxa, y=SCBD)) +
  ggtitle("SCBD macroinvertebrates") +
  geom_bar(aes(fill=Season), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  scale_fill_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() +
  coord_polar() + labs(x = "", y = "")  # + theme(axis.text.x = element_text(angle = 45)) # esto es para poner angulo a las etiquetas
  
rose_plot_macros

#


#### H3 Gráficos de SCBD curvas presencia/ausencia y abundancia ####

## Calculo la occupancy (Nº de sitios en los que está cada especie)

oc.macros <- macros_total

oc.macros.t <- t(oc.macros) # transpones para que las filas sean especies

ps.oc.macros <- ifelse(oc.macros.t > 0,1,0) # pongo en cada site la presencia-ausencia

occupancy <- rowSums(ps.oc.macros) # sumas los valores de cada fila (spp)
occupancy <- as.data.frame(occupancy)
occupancy

per.occupancy <- (occupancy/101) # occupancy relativa por el numero de sites (entre 0 y 1)





## datos presencia auseancia


ps_macros_total <- ifelse(macros_total > 0,1,0)


ps_beta_total_macros <- beta.div(ps_macros_total, method = "hellinger", nperm = 9999)

SCBD_total_ps <- ps_beta_total_macros$SCBD
summary(SCBD_total_ps)

oc_SCBD_ps <- cbind(per.occupancy, SCBD_total_ps)


lm_oc.scbd.ps <- lm(occupancy ~ SCBD_total_ps , data = oc_SCBD_ps)
summary(lm_oc.scbd.ps)


oc.ps.scbd_macros <- ggplot(oc_SCBD_ps, aes(x=occupancy, y=SCBD_total_ps)) +
  geom_point() +    # genera circulos en el grafico
  geom_smooth(method = "loess") + # adjunta la linea de regresion por defecto es al 95% de confianza, de forma suavizada
  theme_bw() + 
  labs(x = "Occupancy", y = "SCBD Pres/abs", subtitle = "D") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) # esto es el tipo de letra

  
oc.ps.scbd_macros


## Datos abundancia sin transformación

ab_macros_total <- datosmacros[, -c(1:5)]

ab_beta_total_macros <- beta.div(ab_macros_total, method = "hellinger", nperm = 9999)


SCBD_total_ab <- ab_beta_total_macros$SCBD
summary(SCBD_total_ab)


oc_SCBD_ab <- cbind(per.occupancy, SCBD_total_ab)


lm_oc.scbd.ab <- lm(occupancy ~ SCBD_total_ab , data = oc_SCBD_ab)
summary(lm_oc.scbd.ab)


oc.ab.scbd_macros <- ggplot(oc_SCBD_ab, aes(x=occupancy, y=SCBD_total_ab)) +
  ggtitle("Macroinvertebrates") +
  geom_point() +    # genera circulos en el grafico
  geom_smooth(method = "loess") + # adjunta la linea de regresion por defecto es al 95% de confianza, de forma suavizada 
  theme_bw() + 
  labs(x = "Occupancy", y = "SCBD Abund", subtitle = "C") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20)) 

oc.ab.scbd_macros




# Uno plots SCBD

SCBD_plot_macros <- oc.ab.scbd_macros + oc.ps.scbd_macros
SCBD_plot_macros


##


#### H4 Relacion LCBD y Marxan con alfa y beta ####

## Total

#Indices alpha
shannon_total <- diversity(macros_total, index = "shannon")
shannon_total

richness_total <- specnumber(macros_total)
richness_total
summary(richness_total)


#Indices beta
jaccard <- vegdist(macros_total, index = "Jaccard") # calculo la matriz de disimilaridad de jaccard
jaccard

jaccard_total<-apply(jaccard,2,mean, na.rm=T) # hago la media de cada sitio de jaccard
jaccard_total
summary(jaccard_total)

# Correlación directa con la sequía

TotDur_diver <- cbind(categoricas_total, variables_total, richness_total, jaccard_total)

rich_inter <- lme(richness_total ~ TotDur, random = ~ 1|(Stream/Reach), data = TotDur_diver)
anova(rich_inter)
r.squaredGLMM(rich_inter)

jacc_inter <- lme(jaccard_total ~ TotDur, random = ~ 1|(Stream/Reach), data = TotDur_diver)
anova(jacc_inter)
r.squaredGLMM(jacc_inter)


plot1_rich_inter <- ggplot(TotDur_diver, aes(TotDur, richness_total)) + geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() + 
  labs(subtitle = "A")

plot1_rich_inter


plot2_jacc_inter <- ggplot(TotDur_diver, aes(TotDur, jaccard_total)) + geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() + 
  labs(subtitle = "A")

plot2_jacc_inter


#

#conservacion variables
Fre_sel_marxan <- as.numeric(Fre_sel_marxan)
Fre_sel_marxan

LCBD_total



datos_corr_total <- cbind(categoricas_total, shannon_total, richness_total, jaccard_total, LCBD_total, Fre_sel_marxan)
datos_corr_total <- as.data.frame(datos_corr_total)
datos_corr_total

colnames(datos_corr_total) <- c( "Reach", "Stream", "Season", "AE", "Shannon_index", "Richness", "Jaccard_index", "LCBD", "MSF" )



#

#### Diferencias entre estaciones para la diversidad ####


## Homogeneidad de varianza (p>0.05 para que haya homogeneidad)

datos_corr_total

library(car)

# Riqueza
leveneTest(Richness ~ Season, data = datos_corr_total, center = "median")
bartlett.test(datos_corr_total$Richness, datos_corr_total$Season)

# Jaccard
leveneTest(Jaccard_index ~ Season, data = datos_corr_total, center = "median")
bartlett.test(datos_corr_total$Jaccard_index, datos_corr_total$Season)


## Tests Kruskal-Wallis (p<0.05 es que hay diferencia entre al menos dos grupos)

kruskal.test(Richness ~ Season, data = datos_corr_total)
kruskal.test(Jaccard_index ~ Season, data = datos_corr_total)



## test pos-hoc de Mann-Whitney-wilcox para ver diferencias entre estaciones

# Riqueza

pairwise.wilcox.test(x = datos_corr_total$Richness, g = datos_corr_total$Season, p.adjust.method = "holm")

# Jaccard

pairwise.wilcox.test(x = datos_corr_total$Jaccard_index, g = datos_corr_total$Season, p.adjust.method = "holm")



### para pasar resultados a una tabla

#results_table <- (xx$p.value) # xx es el nombre que le di al test de wilcox

#results_table <- as.data.frame(results_table)

#library(officer)
# Create a Word document object
#doc <- read_docx()
# Add a table to the document
#doc <- body_add_table(doc, results_table)
# Save the Word document
# print(doc, target = "Jupuuuuuuttuuuuuu.docx")


#

#### Relaciones y modelos con LCBD ####


## Riqueza


LCBD_rich_beta <- glmmTMB(LCBD~Richness + (1| Stream*Reach),data=datos_corr_total, family=beta_family(link="logit"))
summary(LCBD_rich_beta)
r.squaredGLMM(LCBD_rich_beta)


# ANCOVA

LCBD_rich_anc <- aov(LCBD ~ Richness + Season, data=datos_corr_total)
summary(LCBD_rich_anc)

# ver si los datos son normales
qqnorm(resid(LCBD_rich_anc), main="Normal Q-Q Plot")
qqline(resid(LCBD_rich_anc), col="red")


# plot

plot1_LCBD_rich_sea <- ggplot(datos_corr_total, aes(Richness, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "A")

plot1_LCBD_rich_sea

# plot sin seasons

plot1_LCBD_rich_macros <- ggplot(datos_corr_total, aes(Richness, LCBD)) +
  ggtitle("Macroinvertebrates") +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20)) 
  

plot1_LCBD_rich_macros


  


## Jaccard 

LCBD_jacc <- lme(LCBD~Jaccard_index, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(LCBD_jacc)
#summary(LCBD_jacc)
r.squaredGLMM(LCBD_jacc)

# ANCOVA

LCBD_jacc_anc <- aov(LCBD ~ Jaccard_index + Season, data=datos_corr_total)
summary(LCBD_jacc_anc)

# ver si los datos son normales
qqnorm(resid(LCBD_jacc_anc), main="Normal Q-Q Plot")
qqline(resid(LCBD_jacc_anc), col="red")


# plot

plot2_LCBD_jacc <- ggplot(datos_corr_total, aes(Jaccard_index, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "A")

plot2_LCBD_jacc


# plots sin seasons

plot2_LCBD_jacc <- ggplot(datos_corr_total, aes(Jaccard_index, LCBD)) + geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() + 
  labs(subtitle = "A")

plot2_LCBD_jacc



#

#### Relaciones y modelos con Marxan ####


## Riqueza


MSF_rich <- lme(MSF~Richness, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_rich)
#summary(LCBD_rich)
r.squaredGLMM(MSF_rich)

# ANCOVA

MSF_rich_anc <- aov(MSF ~ Richness + Season, data=datos_corr_total)
summary(MSF_rich_anc)

# ver si los datos son normales
qqnorm(resid(MSF_rich_anc), main="Normal Q-Q Plot")
qqline(resid(MSF_rich_anc), col="red")


# plot

plot3_MSF_rich_sea <- ggplot(datos_corr_total, aes(Richness, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "A")

plot3_MSF_rich_sea


# plots sin seasos

plot3_MSF_rich_macros <- ggplot(datos_corr_total, aes(Richness, MSF)) + 
  ggtitle("") +
  geom_point() +
  geom_smooth(method = "lm", se = T) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"))
  

plot3_MSF_rich_macros



## Jaccard 

MSF_jacc <- lme(MSF~Jaccard_index, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_jacc)
#summary(MSF_jacc)
r.squaredGLMM(MSF_jacc)

# ANCOVA

MSF_jacc_anc <- aov(MSF ~ Jaccard_index + Season, data=datos_corr_total)
summary(MSF_jacc_anc)

# ver si los datos son normales
qqnorm(resid(MSF_jacc_anc), main="Normal Q-Q Plot")
qqline(resid(MSF_jacc_anc), col="red")


# plot 

plot4_MSF_jacc <- ggplot(datos_corr_total, aes(Jaccard_index, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "A")

plot4_MSF_jacc


# plot sin seasons

plot4_MSF_jacc <- ggplot(datos_corr_total, aes(Jaccard_index, MSF)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  theme_bw() + 
  labs(subtitle = "A")

plot4_MSF_jacc


## LCBD

MSF_LCBD <- lme(MSF~LCBD, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_LCBD)
#summary(MSF_MSF)
r.squaredGLMM(MSF_LCBD)


# ANCOVA

MSF_LCBD_anc <- aov(MSF ~ LCBD + Season, data=datos_corr_total)
summary(MSF_LCBD_anc)

# ver si los datos son normales
qqnorm(resid(MSF_LCBD_anc), main="Normal Q-Q Plot")
qqline(resid(MSF_LCBD_anc), col="red")



# Plot


plot5_MSF_LCBD_sea <- ggplot(datos_corr_total, aes(MSF, LCBD, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) +
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() + 
  labs(subtitle = "A")

plot5_MSF_LCBD_sea


#plot sin seasons

plot5_MSF_LCBD_macros <- ggplot(datos_corr_total, aes(MSF, LCBD)) + 
  ggtitle("") +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20)) +
  theme(axis.title.y =  element_blank())

plot5_MSF_LCBD_macros



# Uno los plots


LCBD_MSF_rich_macros <- grid.arrange(plot1_LCBD_rich_macros, plot5_MSF_LCBD_macros, plot3_MSF_rich_macros, nrow = 1,   ncol = 3)


#


