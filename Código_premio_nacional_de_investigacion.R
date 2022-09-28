#Librerías a utilizar
library(haven)
library(dplyr)
library(knitr)

#Importando las bases
Adolescentes <- read_sav("Adolescentes_ENH2018.sav")
Personas <- read_sav("Personas_ENH18.sav")
Vivienda_Hogares <- read_sav("Hogares_ENH18.sav")

#Unificando las bases
Adolescentes <- mutate(Adolescentes, key = paste(Adolescentes$Region, #Construyendo una variable llave para hacer la unión con la tabla Personas
                                                 Adolescentes$HPROVI,
                                                 Adolescentes$UPM,
                                                 Adolescentes$HVIVIEN,
                                                 Adolescentes$HHOGAR,
                                                 Adolescentes$HLINEA,
                                                 sep = "-"))

Personas <- mutate(Personas, key = paste(Personas$Region, #Construyendo una variable llave para hacer la unión con la tabla Adolescentes
                                         Personas$HPROVI,
                                         Personas$UPM,
                                         Personas$HVIVIEN,
                                         Personas$HHOGAR,
                                         Personas$HLINEA,
                                         sep = "-"))

Base <- left_join(x = Adolescentes, y = Personas, by = "key") #Uniendo la tabla Personas con la tabla Adolescentes


Base <- mutate(Base, key = paste(Base$CUEST_HOGAR, #Construyendo una variable llave para hacer la unión con Vivienda_Hogares
                                 Base$HPROVI,
                                 Base$UPM,
                                 sep = "-"))





Base <- mutate(Base,Key = paste( Base$CUEST_HOGAR + Base$HVIVI, sep = "-" ) )
Vivienda_Hogares <- mutate(Vivienda_Hogares,Key = paste( Vivienda_Hogares$CUEST_HOGAR + Vivienda_Hogares$HVIVI, sep = "-" ) )


Base_2 <- left_join(x = Base, y = Vivienda_Hogares, "CUEST_HOGAR" )


Adolescentes <- Adolescentes_ENH2018 %>% filter(AD102 < 20)
Adolescentes %>% group_by() %>% 
  summarise(n()) %>% 
  kable()

