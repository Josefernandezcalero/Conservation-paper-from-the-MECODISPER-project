#########################################################################################################
############## Marxan Code for Fernández-Calero et al., 2024. Aquatic conservation ######################
#########################################################################################################




##### Creating the puvpsr2.dat file #####

#### Macroinvertebrates ####

## Winter

library(readr)
wide <- read.csv("Matriz_macros_marxan_winter.csv", sep=";", header=TRUE, check.names=FALSE)

# Presence/absence data

wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)



# Creation of puvpsr2.dat file

library(tidyr)
library(dplyr)

long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/puvspr2.csv", row.names = FALSE)



## Spring

wide <- read.csv("Matriz_macros_marxan_spring.csv", sep=";", header=TRUE, check.names=FALSE) # Portatil


# Presence/absence data

wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


# Creation of puvpsr2.dat file

long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/puvspr2.csv", row.names = FALSE)


## Summer



wide <- read.csv("Matriz_macros_marxan_summer.csv", sep = ";", header = TRUE, check.names = FALSE)

# Presence/absence data

wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


# Creation of puvpsr2.dat file

long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, file = "puvspr2.csv", row.names = FALSE)


## Autumn

wide <- read.csv("Matriz_macros_marxan_autumn.csv", sep = ";", header = TRUE, check.names = FALSE)

# Presence absence data

wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)

# Creation of puvpsr2.dat file

long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, file = "puvspr2.csv", row.names = FALSE)


#### Diatoms ####

## Winter


wide <- read.csv2("Matriz_diatos_marxan_winter.csv", sep=",", header=TRUE, check.names=FALSE)



wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/puvspr2.csv", row.names = FALSE)


## Spring


wide <- read.csv2("Matriz_diatos_marxan_spring.csv", sep=",", header=TRUE, check.names=FALSE)


wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/puvspr2.csv", row.names = FALSE)


## Summer


wide <- read.csv2("Matriz_diatos_marxan_summer.csv", sep=",", header=TRUE, check.names=FALSE)


wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/puvspr2.csv", row.names = FALSE)



## Autumn


wide <- read.csv2("Matriz_diatos_marxan_autumn.csv", sep=",", header=TRUE, check.names=FALSE)

wide
wide_preau <- ifelse(wide>0,1,0)
wide_preau <- as.data.frame(wide_preau)


long <- gather(wide, "species", "amount", -code)
long2 <- filter(long, amount > 0)
names(long2)[1] <- "pu"
long3 <- long2[c(2,1,3)]
long3 <- long3[  order(long3[,2], long3[,1] ),]
long3
write.csv(long3, "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/puvspr2.csv", row.names = FALSE)






##### Bootstrap code #####

#### Macroinvertebrates ####

## Winter

library(dplyr)

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/input/pu.csv", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/input/puvspr2.csv", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/input/spec.csv", sep=";") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/input/pu.csv", sep=";")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)

# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/input/spec.csv", sep=",") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  
  
  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion invierno/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Spring

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  



# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/input/spec.csv", sep=";") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/input/pu.csv", sep=";")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}


write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)


# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/input/spec.csv", sep=";") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  
  
  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion primavera/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Summer

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/input/spec.csv", sep=";") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/input/pu.csv", sep=";")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)



# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/input/spec.csv", sep=";") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  
 
  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion verano/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Autumn

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/input")
  input[29] <- paste("SPECNAME", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/input/spec.csv", sep=";") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/input/pu.csv", sep=";")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)


# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/input/spec.csv", sep=";") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  
  
  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan conservacion otono/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)



#### Diatoms ####

## Winter

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/input/spec.csv", sep=",") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/input/pu.csv", sep=";")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)



# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/input/spec.csv", sep=",") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  
 
  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas invierno/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Spring

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/input/spec.csv", sep=",") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/input/pu.csv", sep=",")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)



# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/input/spec.csv", sep=",") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  

  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas primavera/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Summer

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/input/spec.csv", sep=",") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/input/pu.csv", sep=",")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)


# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/input/spec.csv", sep=",") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  

  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas verano/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


## Autumn

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/spec.csv")

for(i in 1:1000) {    
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input"))
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i)) #create input folder
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/input")) #create input folder
  
  
  aa <- spec %>% sample_frac(.5)
  write.csv(aa, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/input/spec_random.csv"), row.names= FALSE)
  
  input.file <- "C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input.dat"
  input <- readLines(input.file[1], n = -1)
  
  dir.create(path= paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/", i, "/output")) #create output folder
  
  input[28] <- paste0("INPUTDIR C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/input")
  input[29] <- paste0("SPECNAME ", "spec_random.csv", sep = " ") # rewrite the name of the spec file
  input[43] <- paste0("OUTPUTDIR  C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/", i, "/output")
  input[35] <- paste0("SCENNAME output", i)
  
  write(input, paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i,"/input.dat")) # Re-write input file at each run with the corresponding parameters changed
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/input/pu.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/input"))
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/input/puvspr2.dat", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/input"))
  
  file.copy("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Marxan_x64.exe", paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i))
  setwd(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/", i))
  system("Marxan_x64.exe", wait = T, invisible = T) # Call Marxan to execute
}  


#

# collect solutions automatically

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/input/spec.csv", sep=",") #leo el archivo de especies y pu para tenerlo como base para después ir pegando las soluciones (igual que crearme una lista de sitios y especies)
pu <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/input/pu.csv", sep=",")
df <-data.frame(pu=pu[,1])
df2 <- data.frame(spec=spec[,1])

for(i in 1:1000) #ajustar al número de bootstraps que tengas. En mi caso las carpetas de salida tenían un numero como nombre -numero de bootstrap- por eso pego DIRECTORIO/output[i]/...
  
{
  solutions <- read.csv (paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/output/output",i,"_best.txt"))
  df <- cbind(df, solutions[, 2])
  names(df)[dim(df)[2]]<- paste('sol', i)
  
  
}

write.csv(df, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/mvbest_50%spp.csv' , row.names = FALSE)


# recopilar el % de target conseguido (MPM) de forma automatica

spec <- read.csv("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/input/spec.csv", sep=",") #lees tu archivo de especies

df2 <- data.frame(spec=spec[,1]) #te quedas solo con la columna de los ids de las especies


for(i in 1:1000)
  
{
  
  species <- read.csv(paste0("C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/input/",i, "/output/output",i,"_mvbest.txt")) #lees tus archivos de mvbest
  
  species2 <- species[c(1,10)] #te quedas con la primera y ultima columna (tienen los id de las especies y el MPM)
  

  df2 <- df2 %>% left_join(species2,  by = c("spec" = "Conservation.Feature")) #pegas los valores de MPM
  
}

write.csv(df2, 'C:/Users/Jose Fernandez/Desktop/Marxan/Marxan diatomeas/Diatomeas otono/Bootstrap/mvbest_MPM.csv' , row.names = FALSE)


