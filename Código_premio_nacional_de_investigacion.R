#Librerías a utilizar-----------------------------------------------------------
library(haven)
library(dplyr)
library(knitr)

#Importando las bases-----------------------------------------------------------
Adolescentes <- read_sav("Adolescentes_ENH2018.sav")
Personas <- read_sav("Personas_ENH18.sav")
Vivienda_Hogares <- read_sav("Hogares_ENH18.sav")

#Unificando las bases-----------------------------------------------------------
Adolescentes <- mutate(Adolescentes, key = paste(Adolescentes$Region, #Construyendo una variable llave para hacer la unión de adolescentes con la tabla Personas
                                                 Adolescentes$HPROVI,
                                                 Adolescentes$UPM,
                                                 Adolescentes$HVIVIEN,
                                                 Adolescentes$HHOGAR,
                                                 Adolescentes$HLINEA,
                                                 sep = "-"))

Personas <- mutate(Personas, key = paste(Personas$Region, #Construyendo una variable llave para hacer la unión de personas con la tabla Adolescentes
                                         Personas$HPROVI,
                                         Personas$UPM,
                                         Personas$HVIVIEN,
                                         Personas$HHOGAR,
                                         Personas$HLINEA,
                                         sep = "-"))

Base <- left_join(x = Adolescentes, y = Personas, by = "key") #Uniendo la tabla Personas con la tabla Adolescentes


Vivienda_Hogares <- mutate(Vivienda_Hogares, key_2 = paste(Vivienda_Hogares$Region, #Construyendo una variable llave para hacer la unión de hogares con la base unificada
                                                       Vivienda_Hogares$HPROVI,
                                                       Vivienda_Hogares$UPM,
                                                       Vivienda_Hogares$HVIVIEN,
                                                       Vivienda_Hogares$HHOGAR,
                                                       Vivienda_Hogares$HVIVI,
                                                       sep = "-"))

Base <- mutate(Base, key_2 = paste(Base$Region.x, #Construyendo una variable llave para hacer la unión de la base unificada con Vivienda_Hogares
                                 Base$HPROVI.x,
                                 Base$UPM.x,
                                 Base$HVIVIEN.x,
                                 Base$HHOGAR.x,
                                 Base$HVIVI,
                                 sep = "-"))



Base <- left_join(x = Base, y = Vivienda_Hogares, by = "key_2") #añadiendo la tabla hogares a la base unificada

#Recodificando y construyendo las variables de la OMS en base a la ENHOGAR-----

