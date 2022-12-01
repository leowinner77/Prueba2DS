library( readxl)
library(tidyverse)
library(ggrepel)
library(dslabs)
library(stringr)
library(zoo)
library(lubridate)
library(rvest)
library(dplyr)
library(gridExtra)
library(rmarkdown)
#Leemos los archivos descargados en excel y creamos las variables para la cantidad de años y el pais

Observaciones <- c(2000:2021)
Pais1 <- "United States"
Pais2 <- "France"
Nuclear_Share <- read_csv("share-electricity-nuclear.csv") 
EmisionesCO2 <- read_csv("annual-co2-emissions-per-country.csv") %>%
 
#Cambiamos los nombres de las variables al dataframe de EmisionesCO2
    
  set_names("Country", "Code", "Year", "Emmisions") %>%

#Filtramos por la variable de pais y de observaciones anuales  
    
  filter(Country %in% Pais1| Country %in% Pais2) %>%
  filter(Year %in% Observaciones) %>%

#Escogemos las variables a usar para tener una data limpia    
  
  select(Country, Year, Emmisions)

#Una vez limpiada este dataframe, pasamos al siguiente y limpiamos todos los NA  
   
Nuclear_ShareUSA <- Nuclear_Share %>%
  
#Ponemos los nombres a las variables para que sea mas facil trabajarlos dentro de este dataframe
    
 set_names("Country", "Code", "Year", "Energy_Share") %>%

#Filtramos por las observaciones necesarias, y nos quedamos con el Share de Energia Nuclear   
  
  filter(Year %in% Observaciones) %>%
  filter(Country %in% Pais1) %>%
  select(Country, Year, Energy_Share) 

#Ahora unimos ambos dataframes en 1, por su año y pais, y lo guardamos en una nueva dataframe

Relation_USA <- merge(EmisionesCO2, Nuclear_ShareUSA, by=c("Year", "Country"))

#Cambiamos los nombres de las variables al dataframe de NuclearProdFR

  
Nuclear_ShareFR <- set_names(Nuclear_Share, "Country", "Code", "Year", "Energy_Share") %>%
  
#Filtramos por la variable de pais y de observaciones anuales  
  
  filter(Country %in% Pais2) %>%
  filter(Year %in% Observaciones) %>%
  
#Escogemos las variables a usar para tener una data limpia    
  
  select(Country, Year, Energy_Share)

#Ahora unimos ambos dataframes en 1, por su año y pais, y lo guardamos en una nueva dataframe
  
Relation_FR <- merge(EmisionesCO2, Nuclear_ShareFR, by=c("Year", "Country"))

#Empezamos por graficar el share de la energia nuclear en el sector energico en los estados unidos

Graph1_ShareUSA <- Nuclear_ShareUSA %>%
  ggplot(aes(x = Year, y =Energy_Share)) +
  labs(title = "Share of Nuclear Energy in the USA", x = "Year", y = "Nuclear Share") +
  geom_line(size = 2)+
  ylim(c(0, 80))

#Ahora graficamos el share de la energia nuclear en el sector energico en Francia

Graph2_ShareFR <- Nuclear_ShareFR %>%
  ggplot(aes(x = Year, y =Energy_Share)) +
  labs(title = "Share of Nuclear Energy in France", x = "Year", y = "Nuclear Share") +
  geom_line(size= 2)+
  ylim(c(0, 80))

#Realizamos la comparacion del share de la energia nuclear en ambos paises

Comparison_Share <- grid.arrange(Graph1_ShareUSA, Graph2_ShareFR, ncol=2)

#Dado que los datos de CO2 son muy altos, reducimos su escala

EmisionesCO2$EmmisionsReduced = EmisionesCO2$Emmisions/100000
Relation_USA$EmmisionsReduced = Relation_USA$Emmisions/100000000
Relation_FR$EmmisionsReduced = Relation_FR$Emmisions/10000000

#Realizamos el grafico de emisiones de CO2 de USA

Graph1_CO2 <- EmisionesCO2 %>%
  filter(Country %in% Pais1) %>%
  ggplot(aes(x = Year, y = EmmisionsReduced)) +
  labs(title = "CO2 Emmisions in the USA (hundred thousands)", x = "Year", y = "CO2 Emmisions") +
  geom_col()+
  ylim(0,80000)

#Realizamos el grafico de emisiones de CO2 de Francia

Graph2_CO2 <- EmisionesCO2 %>%
  filter(Country %in% Pais2) %>%
  ggplot(aes(x = Year, y = EmmisionsReduced)) +
  labs(title = "CO2 Emmisions in France (hundred thousands)", x = "Year", y = "CO2 Emmisions") +
  geom_col()+
  ylim(0,80000)
  
#Realizamos la comparacion entre las emisiones de CO2 de ambos paises

Comparison_CO2 <- grid.arrange(Graph1_CO2, Graph2_CO2, ncol = 2)

#Comparacion entre CO2 y nuclear Share USA

Comparison_USA <- ggplot(Relation_USA)  + 
  geom_bar(aes(x = Year, y = EmmisionsReduced), stat ="identity")+
  geom_line(aes(x = Year, y = Energy_Share), stat = "identity",color = "red",size = 2)+
  labs(title= "Comparison between Nuclear Share and CO2 Emmisions (Hundred millions)",
       x="Year",y="CO2 Emmisions")+
  scale_y_continuous(sec.axis=sec_axis(~.*0.01, name="Nuclear Share", labels=scales::percent))

#Comparacion entre CO2 y nuclear Share France, hay que destacar que la escala del CO2 esta en Ten millions, a comparacion de los hundred millions de USA.

Comparison_FR <- ggplot(Relation_FR)  + 
  geom_bar(aes(x = Year, y = EmmisionsReduced), stat = "identity")+
  geom_line(aes(x = Year, y = Energy_Share), stat ="identity", color = "red", size = 2)+
  labs(title= "Comparison between Nuclear Share and CO2 Emmisions (Ten millions)",
       x="Year",y="CO2 Emmisions")+
  scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Nuclear Share", labels=scales::percent))


Comparison_USA_FR <- grid.arrange(Comparison_USA, Comparison_FR, ncol = 2)