# Graficos para el hilo 

# Info de la pc y R version ####

# R version 4.0.2 (2020-06-22) -- "Taking Off Again"
# Copyright (C) 2020 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# Paquetes ####

library(ggplot2)

library(ggrepel)

library(dplyr)

library(readxl)

# Datos ####

iaps_parte2 <- read_excel("datos/datos_paper_IAPS2.xlsx")
iaps_parte2$set <- as.factor(iaps_parte2$set)

# Para el grafico general distinguiendo puntuaciones entre hombres y mujeres
mujeres <- iaps_parte2 %>% select(valence = VAM_m, arousal = AAM_m) %>% mutate(sex = "mujeres")
hombres <- iaps_parte2 %>% select(valence = VAM_h, arousal = AAM_h) %>% mutate(sex = "hombres")
datos_grafico_hym <- bind_rows(mujeres, hombres)
# Graficos ####

grafico_general <- ggplot(data = iaps_parte2, aes(x = VAM, y = AAM)) +
  geom_point(colour = "darkgreen") +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en población general", 
       x = "Valencia", y = "Activación") +
  geom_label_repel(aes(label = ifelse(VAM < 2 & AAM > 7.5, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # desagradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM > 6 & AAM > 6, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # agradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM < 5 & AAM < 3.5, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # neutras
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  coord_flip() +
  theme_minimal()

grafico_general_sets <- ggplot(data = iaps_parte2, aes(x = VAM, y = AAM, colour = set)) +
  geom_point() +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en población general",
       subtitle = "Representacion en funcion a cada set de estimulos",
       x = "Valencia", y = "Activación") +
  coord_flip() +
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  theme_minimal()


grafico_general_hym <- ggplot(data = datos_grafico_hym, aes(x = valence, y = arousal, colour = sex)) +
  geom_point() +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en población general",
       x = "Valencia", y = "Activación") +
  coord_flip() +
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  labs(colour = " ")
  theme_minimal()



grafico_mujeres <- ggplot(data = iaps_parte2, aes(x = VAM_m, y = AAM_m)) +
  geom_point(colour = "darkgreen") +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en mujeres", 
       x = "Valencia", y = "Activación") +
  geom_label_repel(aes(label = ifelse(VAM_m < 2 & AAM_m > 7.9, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) +# desagradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM_m > 6.5 & AAM_m > 6.5, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) +# agradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM_m < 4.5 & AAM_m < 3.5, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) +# neutras
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  coord_flip() +
  theme_minimal()

grafico_mujeres_sets <- ggplot(data = iaps_parte2, aes(x = VAM_m, y = AAM_m, colour = set)) +
  geom_point() +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en mujeres",
       subtitle = "Representacion en funcion a cada set de estimulos",
       x = "Valencia", y = "Activación") +
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  coord_flip() +
  theme_minimal()


grafico_hombres <- ggplot(data = iaps_parte2, aes(x = VAM_h, y = AAM_h)) +
  geom_point(colour = "darkgreen") +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en hombres", 
       x = "Valencia", y = "Activación") +
  geom_label_repel(aes(label = ifelse(VAM_h < 2.5 & AAM_h > 7.2, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # desagradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM_h > 6 & AAM_h > 6, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # agradables y activadoras
  geom_label_repel(aes(label = ifelse(VAM_h < 5.5 & AAM_h < 3, as.character(descripcion),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5) + # neutras
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  coord_flip() +
  theme_minimal() 

grafico_hombres_sets <- ggplot(data = iaps_parte2, aes(x = VAM_h, y = AAM_h, colour = set)) +
  geom_point() +
  labs(title = "Dispersión de las puntuaciones según su Valencia y Activación en hombres",
       subtitle = "Representacion en funcion a cada set de estimulos",
       x = "Valencia", y = "Activación") +
  xlim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  ylim("1","2", "3", "4", "5", "6", "7", "8", "9") +
  coord_flip() +
  theme_minimal()
