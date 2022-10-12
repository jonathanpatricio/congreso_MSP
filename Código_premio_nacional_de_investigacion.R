#Librerías a utilizar-----------------------------------------------------------
library(haven)
library(dplyr)
library(knitr)

#Importando las bases-----------------------------------------------------------
Adolescentes <- read_sav("Adolescentes_ENH2018.sav")
Personas <- read_sav("Personas_ENH18.sav")
Vivienda_Hogares <- read_sav("Hogares_ENH18.sav")

#Unificando las bases-----------------------------------------------------------
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


Vivienda_Hogares <- mutate(Vivienda_Hogares, key_2 = paste(Vivienda_Hogares$Region, #Construyendo una variable llave para hacer la unión con Vivienda_Hogares
                                                       Vivienda_Hogares$HPROVI,
                                                       Vivienda_Hogares$UPM,
                                                       Vivienda_Hogares$HVIVIEN,
                                                       Vivienda_Hogares$HHOGAR,
                                                       Vivienda_Hogares$HVIVI,
                                                       sep = "-"))

Base <- mutate(Base, key_2 = paste(Base$Region.x, #Construyendo una variable llave para hacer la unión con Vivienda_Hogares
                                 Base$HPROVI.x,
                                 Base$UPM.x,
                                 Base$HVIVIEN.x,
                                 Base$HHOGAR.x,
                                 Base$HVIVI,
                                 sep = "-"))



Base <- left_join(x = Base, y = Vivienda_Hogares, by = "key_2") #añadiendo la tabla hogares

#Recodificando y construyendo las variables de la OMS en base a la ENHOGAR-----

Base_2 <- Base %>%  select(AD201, AD203, AD702, AD701, AD502, AD504, AD506, AD508, AD510, # Seleccionando las variables a utilizar
                           AD512, AD514, AD516, AD518, AD520, AD522, AD710, AD1001, AD1004,
                           AD113, AD114, AD115, AD117, AD118, AD112, H202, H205, AD301, 
                           AD312, AD703, AD705, AD718_c, AD112, AD216_f, AD524_h, H205, AD202_a,
                           AD202_b, AD202_c, AD202_d, AD202_e, AD202_f, AD202_g, AD202_h, 
                           AD202_x, AD204_a, AD204_b, AD204_c, AD204_d, AD204_e, AD204_f,
                           AD204_g, AD204_h, AD204_x, AD216_h, AD524_j, AD713, AD1006,
                           AD102, AD114, AD115, AD119_a, AD119_b, AD119_c, AD119_d, AD119_e,
                           AD119_f, AD119_g, AD119_h, AD119_i, AD119_j, AD119_k, AD119_l, 
                           AD119_m, AD712, AD307, AD309, AD312, HESTRAT.x) %>% filter(AD102 < 20)

Base_2$Escuchado_pubertad <- if_else(condition = Base_2$AD201 == 1, true = 1, false = 0)
Base_2$Conocimientos_antomia_repro <- if_else(condition = Base_2$AD203 == 1, true = 1, false = 0)
Base_2$Inicio_relaciones <- as.ordered(cut(x = Base_2$AD702, breaks = c(-Inf,0,15,Inf), labels = c("0", "2", "1")))
Base_2 <- mutate(Base_2, # Construyendo la variable sobre uniones tempranas
                 Union_temprana = if_else(condition = Base_2$AD701 == 7, true = 0, 
                                            false = if_else(condition = Base_2$AD701 == 9, true = 0, 
                                                            false = 1)))
Base_2 <- mutate(Base_2, # Constuyendo la variables sobre embarazo--------------
                 Embarazo = if_else(condition = Base_2$AD301 == 1, true = 1, 
                                    false = if_else(condition = Base_2$AD307 == 1, true = 1, 
                                                    false = if_else(condition = Base_2$AD309 == 1, true = 1, 
                                                                    false = if_else(condition = Base_2$AD312 == 1, true = 1, 
                                                                                    false = 0)))))
Base_2 <- mutate(Base_2, 
                 Acceso_salud = if_else(condition = Base_2$AD1004 == 3, true = 1,
                                        false = if_else(condition = Base_2$AD1004 == 4, true = 1, 
                                                        false = if_else(condition = is.na(Base_2$AD1004) == TRUE, true = 0, 
                                                                        false = 0)))) ; Base_2$Acceso_salud[is.na(Base_2$Acceso_salud)] <- 0
