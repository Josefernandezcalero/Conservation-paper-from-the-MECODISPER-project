#########################################################################################################
###### Linear mixed-effect models analysis for Fernández-Calero et al., 2024. Aquatic conservation ######
#########################################################################################################



#### Relationships between LCBD, MSF and predictor variables ####
#### Macroinvertebrates ####


### LCBD models (beta regression)

library(glmmTMB)


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


## Graphs (Figure 6)

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




### MSF models


library(nlme)
library(MuMIn)

variables_total_h1



ph_total_ani_marxan <- lme(MSF~pH, random = ~ 1|(Stream/Reach),data=variables_total_h1)
anova(ph_total_ani_marxan)



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


## Graphs (Figure 6)

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




#### Diatoms ####

### LCBD models (beta regression)

ph_total_ani <- glmmTMB(LCBD_total_diatos~pH + (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(ph_total_ani)

oxi_total_ani <- glmmTMB(LCBD_total_diatos~DO_mgL + (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(oxi_total_ani)

t_total_ani <- glmmTMB(LCBD_total_diatos~T + (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(t_total_ani)

tds_total_ani <- glmmTMB(LCBD_total_diatos~TDS+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(tds_total_ani)

b1_total_ani <- glmmTMB(LCBD_total_diatos ~ B1 + (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b1_total_ani)

b2_total_ani <- glmmTMB(LCBD_total_diatos~B2 + (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b2_total_ani)

b3_total_ani <- glmmTMB(LCBD_total_diatos~B3+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b3_total_ani)

b4_total_ani <- glmmTMB(LCBD_total_diatos~B4+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b4_total_ani)

b5_total_ani <- glmmTMB(LCBD_total_diatos~B5+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b5_total_ani)

b7_total_ani <- glmmTMB(LCBD_total_diatos~B7+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(b7_total_ani)

FP50_total_ani <- glmmTMB(LCBD_total_diatos~FP50+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(FP50_total_ani)

DirBin_total_ani <- glmmTMB(LCBD_total_diatos~DirBin+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(DirBin_total_ani)

DirWei_total_ani <- glmmTMB(LCBD_total_diatos~DirWei+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(DirWei_total_ani)

UndBin_total_ani <- glmmTMB(LCBD_total_diatos~UndBin+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(UndBin_total_ani)

UndWei_total_ani <- glmmTMB(LCBD_total_diatos~UndWei+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(UndWei_total_ani)

totleng_total_ani <- glmmTMB(LCBD_total_diatos~TotLeng+ (1| Stream*Reach),data=variables_total_h1_diatos, family=beta_family(link="logit"))
summary(totleng_total_ani)




### MSF models


ph_total_ani_marxan <- lme(MSF~pH, random = ~ 1|(Stream/Reach),data=variables_total_h1_diatos)
anova(ph_total_ani_marxan)
#summary(ph_total_ani_marxan)

oxi_total_ani_marxan <- lme(MSF~DO_mgL, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(oxi_total_ani_marxan) # Strong evidence #
#r.squaredGLMM(oxi_total_ani_marxan)

t_total_ani_marxan <- lme(MSF~T , random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(t_total_ani_marxan) # Strong evidence #


tds_total_ani_marxan <- lme(MSF~TDS, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(tds_total_ani_marxan)


b1_total_ani_marxan <- lme(MSF ~ B1 , random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b1_total_ani_marxan)


b2_total_ani_marxan <- lme(MSF~B2 , random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b2_total_ani_marxan)


b3_total_ani_marxan <- lme(MSF~B3, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b3_total_ani_marxan)


b4_total_ani_marxan <- lme(MSF~B4, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b4_total_ani_marxan)


b5_total_ani_marxan <- lme(MSF~B5, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b5_total_ani_marxan)


b7_total_ani_marxan <- lme(MSF~B7, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(b7_total_ani_marxan)


FP50_total_ani_marxan <- lme(MSF~FP50, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(FP50_total_ani_marxan)


DirBin_total_ani_marxan <- lme(MSF~DirBin, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(DirBin_total_ani_marxan)


DirWei_total_ani_marxan <- lme(MSF~DirWei, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(DirWei_total_ani_marxan)


UndBin_total_ani_marxan <- lme(MSF~UndBin, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(UndBin_total_ani_marxan) # weak evidence #


UndWei_total_ani_marxan <- lme(MSF~UndWei, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(UndWei_total_ani_marxan) # weak evidence #


totleng_total_ani_marxan <- lme(MSF~TotLeng, random = ~ 1| (Stream/Reach),data=variables_total_h1_diatos)
anova(totleng_total_ani_marxan)



## Graphs (Figure 5)


plot1_total_ani_marxan <- ggplot(variables_total_h1_diatos, aes(DO_mgL, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() +
  labs(x = "DO mg/L", y = "MSF", subtitle = "A") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
  theme(legend.position = "none")

plot1_total_ani_marxan




plot2_total_ani_marxan <- ggplot(variables_total_h1_diatos, aes(T, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() +
  labs(x = "Temperature", y = "MSF", subtitle = "B") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
  theme(axis.title.y = element_blank())



plot2_total_ani_marxan



plot3_total_ani_marxan <- ggplot(variables_total_h1_diatos, aes(UndBin, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() +
  labs(x = "Connectivity (UndBin)", y = "MSF", subtitle = "C") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
  theme(legend.position = "none")


plot3_total_ani_marxan




plot4_total_ani_marxan <- ggplot(variables_total_h1_diatos, aes(UndWei, MSF, color = Season)) + geom_point() +
  geom_smooth(method = "lm", se = T) + 
  scale_color_manual(values = c("Winter" = "Blue",
                                "Spring" = "green4",
                                "Summer" = "Yellow",
                                "Autumn" = "brown")) +
  theme_bw() +
  labs(x = "Dispersal resistance (UndWei)", y = "MSF", subtitle = "D") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold",)) + # esto es el tipo de letra
  theme(axis.title.y = element_blank())



plot4_total_ani_marxan





#### Relationships between LCBD, MSF and Richness ####
##### Macroinvertebrates ####

## Richness VS LCBD

LCBD_rich_beta <- glmmTMB(LCBD~Richness + (1| Stream*Reach),data=datos_corr_total, family=beta_family(link="logit"))
summary(LCBD_rich_beta)
r.squaredGLMM(LCBD_rich_beta)


# plot (Figure 2)

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


## Richness VS MSF

MSF_rich <- lme(MSF~Richness, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_rich)
r.squaredGLMM(MSF_rich)


# Plot (Figure 2)


plot3_MSF_rich_macros <- ggplot(datos_corr_total, aes(Richness, MSF)) + 
  ggtitle("") +
  geom_point() +
  geom_smooth(method = "lm", se = T) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"))


plot3_MSF_rich_macros



## LCBD vs MSF

MSF_LCBD <- lme(MSF~LCBD, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_LCBD)
r.squaredGLMM(MSF_LCBD)


# plot (Figure 2)

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




##### Diatoms ####

## Richness VS LCBD

LCBD_rich_beta <- glmmTMB(LCBD~Richness + (1| Stream*Reach),data=datos_corr_total, family=beta_family(link="logit"))
summary(LCBD_rich_beta)
r.squaredGLMM(LCBD_rich_beta)




# plot (Figure 2)


plot1_LCBD_rich <- ggplot(datos_corr_total, aes(Richness, LCBD)) +
  ggtitle("Diatoms") +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() +
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20))

plot1_LCBD_rich


## Richness vs MSF

MSF_rich <- lme(MSF~Richness, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_rich)
#summary(LCBD_rich)
r.squaredGLMM(MSF_rich)


# plot (Figure 2)


plot3_MSF_rich <- ggplot(datos_corr_total, aes(Richness, MSF)) +
  ggtitle("") +
  geom_point() +
  geom_smooth(method = "lm", se = T) + 
  theme_bw() +
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20)) 

plot3_MSF_rich


## LCBD vs MSF

MSF_LCBD <- lme(MSF~LCBD, random = ~ 1|(Stream/Reach),data=datos_corr_total)
anova(MSF_LCBD)
#summary(MSF_MSF)
r.squaredGLMM(MSF_LCBD)




## Plot (Figure 2)

plot5_MSF_LCBD <- ggplot(datos_corr_total, aes(MSF, LCBD)) + 
  ggtitle("") +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_bw() +
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) + # esto es el tipo de letra
  theme(axis.title.y =  element_blank())


plot5_MSF_LCBD





