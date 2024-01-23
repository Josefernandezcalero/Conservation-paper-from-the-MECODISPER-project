########################################################################################################
############ LCBD and SCBD analysis for Fernández-Calero et al., 2024. Aquatic conservation ############
########################################################################################################


#### Macroinvertebrates ####

library(adespatial)

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

SCBD_total <- beta_total_macros$SCBD



### Graphics

# Figure 1 (LCBD)

df_orden_ST480$UndWei_480

unique(df_orden_ST480$Reach) 


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



#### SCBD graphs presence/absence and abundance curves (Figure 3) ####

## occupancy

oc.macros <- macros_total

oc.macros.t <- t(oc.macros) 

ps.oc.macros <- ifelse(oc.macros.t > 0,1,0) 

occupancy <- rowSums(ps.oc.macros) 
occupancy <- as.data.frame(occupancy)
occupancy

per.occupancy <- (occupancy/101)


## presencia auseancia


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


## abundancia

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



#### SCBD rose plots graphs (Figure 4) ####


# Select the first 30 values

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



LGSCBD_total <- bind_rows(
  SCBD_total%>% filter(Season == "Winter") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total%>% filter(Season == "Spring") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total%>% filter(Season == "Summer") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total%>% filter(Season == "Autumn") %>% arrange(desc(SCBD)) %>% slice(1:10))



## Rose plot

library(radiant.data)


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



#### Diatoms ####


beta_total_diatos <- beta.div(diatos_total, method = "hellinger", nperm = 9999)
beta_total_diatos
summary(beta_total_diatos)
beta_total_diatos$beta # SSTotal and BDTotal

# Which species have a SCBD larger than the mean SCBD?
beta_total_diatos$SCBD[beta_total_diatos$SCBD >= mean(beta_total_diatos$SCBD)]

#SCBD valores
beta_total_diatos$SCBD
# LCBD values
beta_total_diatos$LCBD
# p-values
beta_total_diatos$p.LCBD
# Holm correction
p.adjust(beta_total_diatos$p.LCBD, "holm")
# Sites with significant Holm-corrected LCBD value
row.names(diatos_total[,-c(1:3)][which(p.adjust(beta_total_diatos$p.LCBD, "holm") <= 0.05),])


summary(beta_total_diatos$SCBD)

beta_total_diatos$LCBD
LCBD_total_diatos <- beta_total_diatos$LCBD


### Graphics

# Figure 1 (LCBD)


unique(df_orden_ST480$Reach) # saca el orden sin duplicar de los reach

orden_UndWei480_macros <- c("R4","SA2","T1","MU3","MU4","SA3","SA5","T3","T4","CA2","T2","R1","R2","R3","H2","SC4","MU2","CA4","H3",
                            "SA4","SA1","CA1","H1","H5","MU1","CA3","H4","SC3","SC1","SC2") # esto es para que tengan el mismo orden las tablas de diatos y macros

LCBD_diatos_plot <- ggplot(df_orden_ST480, aes(Reach, Season, col = UndWei))+ 
  ggtitle("A) LCBD diatoms") + theme_bw() +
  geom_point(aes(size = LCBD_total)) +
  scale_size(range = c(.1, 5), name="LCBD")+
  theme(legend.text=element_text(size=10)) + # esto es para el size de las etiquetas de rios
  scale_color_gradient(low="blue", high="yellow")+
  scale_x_discrete(limits=orden_UndWei480_macros, breaks=orden_UndWei480_macros)+
  scale_y_discrete(limits=c("Autumn", "Summer", "Spring", "Winter")) + # ordenar las estaciones de arriba a abajo
  theme(axis.title = element_text(size = 20,
                                  color = "black")) +
  theme(axis.title.x =  element_blank()) +
  theme (legend.key.size = unit (3, 'mm'))





#### SCBD graphs presence/absence and abundance curves (Figure 3) ####

## occupancy

oc.diatos <- diatos_total

oc.diatos.t <- t(oc.diatos) # transpones para que las filas sean especies

ps.oc.diatos <- ifelse(oc.diatos.t > 0,1,0) # pongo en cada site la presencia-ausencia

occupancy <- rowSums(ps.oc.diatos) # sumas los valores de cada fila (spp)
occupancy <- as.data.frame(occupancy)
occupancy

per.occupancy <- (occupancy/96) # occupancy relativa por el numero de sites (entre 0 y 1)


## presencia auseancia


ps_diatos_total <- ifelse(diatos_total > 0,1,0)


ps_beta_total_diatos <- beta.div(ps_diatos_total, method = "hellinger", nperm = 9999)

SCBD_total_ps <- ps_beta_total_diatos$SCBD
summary(SCBD_total_ps)

oc_SCBD_ps <- cbind(per.occupancy, SCBD_total_ps)


lm_oc.scbd.ps <- lm(occupancy ~ SCBD_total_ps , data = oc_SCBD_ps)
summary(lm_oc.scbd.ps)


oc.ps.scbd <- ggplot(oc_SCBD_ps, aes(x=occupancy, y=SCBD_total_ps)) + 
  geom_point() +    # genera circulos en el grafico
  geom_smooth(method = "loess") + # adjunta la linea de regresion por defecto es al 95% de confianza, de forma suavizada
  theme_bw() + 
  labs(x = "Occupancy", y = "SCBD Pres/Abs", subtitle = "B") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold")) # esto es el tipo de letra

oc.ps.scbd



## abundancia

ab_diatos_total <- datosdiatos[, -c(1:4)]

ab_beta_total_diatos <- beta.div(ab_diatos_total, method = "hellinger", nperm = 9999)


SCBD_total_ab <- ab_beta_total_diatos$SCBD
summary(SCBD_total_ab)


oc_SCBD_ab <- cbind(per.occupancy, SCBD_total_ab)


lm_oc.scbd.ab <- lm(occupancy ~ SCBD_total_ab , data = oc_SCBD_ab)
summary(lm_oc.scbd.ab)


oc.ab.scbd <- ggplot(oc_SCBD_ab, aes(x=occupancy, y=SCBD_total_ab)) +
  ggtitle("Diatoms") +
  geom_point() +    # genera circulos en el grafico
  geom_smooth(method = "loess") + # adjunta la linea de regresion por defecto es al 95% de confianza, de forma suavizada 
  theme_bw() +
  labs(x = "Occupancy", y = "SCBD Abund", subtitle = "A") + 
  theme(axis.title = element_text(size = 20,
                                  color = "black",
                                  face = "bold"), # esto es el tipo de letra
        plot.title=element_text(size=20))

oc.ab.scbd


# Uno plots SCBD

SCBD_plot <- oc.ab.scbd + oc.ps.scbd
SCBD_plot

#### SCBD rose plots graphs (Figure 4) ####

## Seleccionamos los 30 primeros valores

SCBD_total_diatos_30h <- SCBD_total_diatos[order(SCBD_total_diatos$SCBD, decreasing = T),]
SCBD_total_diatos_30h



### Ordeno los datos para coger las primeras 15 especies por cada estación y hacer el plot general.

LGSCBD_total_diatos <- bind_rows(
  SCBD_total_diatos%>% filter(Season == "Winter") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total_diatos%>% filter(Season == "Spring") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total_diatos%>% filter(Season == "Summer") %>% arrange(desc(SCBD)) %>% slice(1:10),
  SCBD_total_diatos%>% filter(Season == "Autumn") %>% arrange(desc(SCBD)) %>% slice(1:10))



## Rose plot

rose_plot_diatos <- ggplot(LGSCBD_total_diatos, aes(x=Taxa, y=SCBD)) +
  ggtitle("SCBD diatoms") +
  geom_bar(aes(fill=Season), stat="identity") +
  scale_y_continuous(breaks = 0:10) +
  scale_fill_manual(values = c("Winter" = "Blue",
                               "Spring" = "green4",
                               "Summer" = "Yellow",
                               "Autumn" = "brown")) +
  theme_bw() +
  theme(legend.position = "none") + # Quito la layenda porque la lleva pegada la misma figura de los macros.
  coord_polar() + labs(x = "", y = "") + theme(axis.text.x = element_text(angle = 0))

rose_plot_diatos


