#LIBRERÍAS
library(sf)
library(maps)
library(terra)
library(dplyr)
library(tidyr)
library(readxl)
library(spData)
library(geosphere)
library(spDataLarge)   

#DATOS MUNDO
data("world")

#DATOS TRATA DE PERSONAS
base <- read_excel("base_final.xlsx") 

#summary(world)
#names(world)

world |>
      select(iso_a2) |>
      plot()

base_2 <- base %>% 
  dplyr::select(citizenship,
                LAT_STAR,
                LONG_STAR,  
                CountryOfExploitation,
                LAT_FIN,
                LONG_FIN)

names(base_2)[1] = "iso_a2"

base_3 <- base_2 %>% 
  group_by(iso_a2)  %>% 
  summarise(n=n())

world[,1] |>
  left_join(base_3) |>
  select(n) |> plot()

base_4 <- base %>% 
  dplyr::select(CountryOfExploitation,
                LAT_FIN,
                LONG_FIN)

names(base_4)[1] = "iso_a2"

base_4 <- base_4 %>% 
  group_by(iso_a2)  %>% 
  summarise(n=n())

world[,1] |>
  left_join(base_4) |>
  select(n) |> plot()


########################REDES##################################
base <- read_excel("base_final.xlsx") 
#coordenadas_m <- read_excel("coordenadas.xlsx") 
#str(coordenadas_m)
base_1 <- base %>% 
          dplyr::select(citizenship,
                        LAT_STAR,
                        LONG_STAR,  
                        CountryOfExploitation,
                        LAT_FIN,
                        LONG_FIN)


#dat_1 <- graph.data.frame(base_1)

map("world", col="darkblue")

base_2 <- base_1

for(i in 1:nrow(base_2)) {
  arc <- gcIntermediate( c(base_2[i,]$LONG_STAR, base_2[i,]$LAT_STAR),
                         c(base_2[i,]$LONG_FIN, base_2[i,]$LAT_FIN),
                         n=1000, addStartEnd=TRUE )
  lines(arc, 
        #arrows=TRUE, 
        col="#95c799")
}
