setwd("C:/Users/hp/OneDrive/Economia/8vo Semestre/Electiva - Introduccion a la Ciencia de Datos/Prof. Ricardo Andres Benzecry Mancin/Semestre Abr-Ago 2022/Proyecto Final/Proyecto-Final/Data")

getwd()

#Cargar librearias 


library(usmap)
library(readr)
library(plyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(maps)
library(ggplot2)
library(scales)
library(forcats)
library(stargazer)
library(knitr)
library(here)
library(ggthemes)
library(readxl)
library(stringi)
library(stringr)
library(plm)


# Como se que voy a trabajar con data hasta el desde el 2014 al 2019,
# procedo a desprenderme de la data fuera de ese periodo temporal

# Creo un vector de anos validos

anos_validos <- 2014:2019

# lo vuelvo character porque asi estan los datos en gun_rights

anos_validos <- as.character(anos_validos) 


# Lo anterior me servira mas tarde

# Cargar todos los archivos de Gun Violence Archive de un solo golpe,
# toda la data estara en all_gva.

all_files <- list.files()
all_files

all_files <- all_files[all_files != "Subdata"]

all_gva <- ldply(all_files, read_csv)
all_gva

#Cambio los nombres de las columnas a unos que se me haran mas facil trabajar

colnames(all_gva) <- c("Incident_ID", "Incident_Date", "State", "City/County",
                       "Address", "Killed", "Injured","Operations")

#Un vistazo.
#View(all_gva)
#str(all_gva)
#names(all_gva)

#Eliminar unas filas que que estorban.

all_gva <- all_gva %>% 
  filter(`Incident_ID` != "Incident ID") 

#View(all_gva)

#Convertir esto la culumna que tiene las fehcas a una fecha de verdad, ya que
#antes de los siguientes pasos es un character.

all_gva$`Incident_Date` <- as.Date(all_gva$`Incident_Date`,
                                   format = "%B %d,%Y")


#Creando las columnas Year y Month
all_gva <- all_gva %>% 
  mutate(Year = year(all_gva$`Incident_Date`),
         Month = month(all_gva$`Incident_Date`))

#Creando la columna Year_Month
all_gva$Year_Month <- paste(all_gva$Year, all_gva$Month, sep = "-")
#all_gva$Year_Month <- as.Date(all_gva$Year_Month,
#                              format = "%Y-%m")        


#View(all_gva)
#str(all_gva)
#Reorganizando las columnas y quitamos el aC1o 2022 porque es
#un ano incompleto

all_gva <- all_gva %>%
  select(Incident_ID, Incident_Date, Year_Month, Year, Month,State,
         `City/County`, Killed, Injured) %>% 
  filter(Year != 2022)

#Organizamos alfabeticamente

all_gva <- all_gva[order(all_gva$State),]

#Voy a usar los datos que vienen de la tabla statepop, para lo cual
#procedo a hacer unas modificaciones que me permitiran trabajar comodo

colnames(statepop) <- c("fips", "abbr", "State", "Pop_2015")

#Hago un leftjoin para obtener la data que necesito

all_gva <-  left_join(all_gva, statepop , by = "State")


#Realizar una estadisticas descriptivas initiales
#Me gusta bastante usar stargazer, creo que tiene un nombre muy poetico.
#Siempre me imagino que es como mirar a la profundidad de estrellas
#y ver que hay, peroen lugar de estrella es la data.

stargazer(all_gva, type = "text")

#Apartir de aqui deberian poder hacerse los graficos ya que la data esta limpia



#View(all_gva)

# Falta agregar unos labels a los datos mas importantes y quitar
# de los States a Disctric of Columbia porque no todos mis otras fuentes
# de datos tienen District of Columbia 

all_gva_summarised <- all_gva %>%
  group_by(Year, State, abbr, fips ) %>% 
  filter(State != "District of Columbia") %>% 
  filter(Year %in%  anos_validos) %>% 
  summarise(Incident_Count = length(Incident_ID)) %>% 
  ungroup() %>% 
  mutate(Year = as.character(Year))




# Aqui comienzo a cargar y limpiar mis datos de gun rights

# Cargado


gun_rights <- read_excel("Subdata/gun_rights.xlsx", 
                         sheet = "Personal")
View(gun_rights)

# Me percato del nombre de las columnas

colnames(gun_rights)

# Me desprendo de columnas  filas que me estorban

gun_rights <- gun_rights %>% 
  filter(! ...1 %in% c("Mean", "Constitutional weight?", "St. dev."),
          ...4 != "standardized") %>% 
  select( ...1, ...2, `Gun Rights`)

# Cambio las columnas a un nombre mas manejable

colnames(gun_rights) <- c("State", "Year", "Gun_Rights")

# Hago vistazos a la data        
         
#View(gun_rights)

#str(gun_rights)

# Enmarco la data en los a;os para los que tengo data

gun_rights <- gun_rights %>% 
  filter(Year %in% anos_validos)

#View(gun_rights)


# Cargo los datos de median household income

household_income <- read_excel("Subdata/median_household_income.xls", 
                                      skip = 2)
View(household_income)

# Quito columnas y filas que estorban. Ademas, hago la data longer

household_income <- household_income %>% 
  filter(State != c("1", "United States")) %>% 
  select(State ,anos_validos) %>% 
  pivot_longer( cols = anos_validos,
                names_to = "Year",
                values_to = "Median_Household_Income")

# Vistazo a la data

# View(household_income)


# Cargar y limpiar data de incarceration. El primero modelo que voy a 
# hacer no va a tomar en cuenta esta variable porque solo conseguimos data
# para 2014-2016, siendo nuestro timeframe 2014-2019


incarceration <- read_csv("Subdata/incarceration.csv")
View(incarceration)

# Agarro las columnas que me sirven

incarceration <- incarceration %>% 
  select(jurisdiction, year, prisoner_count) %>% 
  filter(year %in% anos_validos,
         jurisdiction != "FEDERAL")

# Cambio los nombres de las colunmnas a unos que me sean utiles
View(incarceration)

colnames(incarceration) <- c("State", "Year", "Prisoners_Count")

incarceration$State <- stri_trans_totitle(incarceration$State)


# Cargar porcentaje de poblacion pobre

poverty <- read_excel("Subdata/poverty.xlsx")

# Year la vuelvo character

poverty$Year <- as.character(poverty$Year)


# Cargar Unemplayment

unemployment <- read_excel("Subdata/unemployment.xlsx")
View(unemployment)


# Year la vuelvo character 

unemployment$Year <- as.character(unemployment$Year)


# Cargo data de la poblacion de USA

population <- read_csv("Subdata/usa_state_population.csv", 
                                 skip = 1)
View(population)



# Realizo esta limpieza de la tabla population, no se si es de buenas practicas
# limpiar tanto de un solo golpe




population <- population %>% 
  separate( "Estimate Date", 
          into = c("Year", NA),
          sep = ' ') %>% 
  filter(id != "0100000US") %>% 
  select(- id) 
  
# Despues de muchas hora, hago lo siguiente para que me funcione el 
# formato de la fecha, las fechas siempre se me hacen mas dificil
# de lo esperado.

# Me desprendo de todo lo que no sea el ano, total, solo voy a usar ano

population<- population %>% 
  separate(Year,
           into = c(NA, NA, "Year"),
           sep = "/") %>% 
  filter( Year %in% anos_validos)

colnames(population) <- c("State", "Year", "Population")
                      
  
View(population)

# Cargo data de Gun Ownership, usamos como proxy rate de suicidios 
# cometidos con armas de fuego

# Describri que puedes quitar las columnas que no quieres desde que
# cargas la data

suicide_firearm <- read_excel("Subdata/suicide.xlsx", 
                      col_types = c("skip", "text", "skip", 
                                    "text", "skip", "text", "skip", "text", 
                                    "text", "numeric", "skip", "text"))

# Agrupo la data 

suicide_firearm <-suicide_firearm %>% 
  group_by(Year , State) %>% 
  summarise(Deaths = sum (Deaths)) %>% 
  ungroup() %>% 
  filter(Year %in% anos_validos)

View(suicide_firearm)

# Cambio el nombre de columnas para hacerlo mas entendible

colnames(suicide_firearm) <- c("Year", "State", "Firearm_Suicide")

# La data de suicidios totales esta en el siguiente archivo. Lo anterior era solo
# los suicidios cometidos con armas de fuego.


total_suicides <- read_csv("Subdata/total_suicides.csv", 
                           col_types = cols(YEAR = col_character(), 
                                            RATE = col_skip(), URL = col_skip()))
View(total_suicides)

# Cambio nombre de columnas

colnames(total_suicides) <- c("Year","abbr", "Suicides")

# Junto tablas para poder cambiar eventualmente la abreviacion de los Estados
# por el Estado

total_suicides <- left_join(total_suicides, statepop, by = "abbr")

# Me desprendo de columnas que no sirven y le pego la columna de 
# suicidios por firearm

total_suicides <- total_suicides %>% 
  select(! c(Pop_2015, fips, abbr)) %>% 
  select( Year, State, Suicides) %>% 
  filter( Year != "2020")




total_suicides <- left_join(total_suicides ,suicide_firearm,
                            by = c("Year", "State")) %>%
  mutate(firearm_Rate = Firearm_Suicide/Suicides*100) %>% 
  select(! c("Suicides", "Firearm_Suicide"))
  

# Vamos a dejar solo los anos que nos importan

total_suicides <- total_suicides %>% 
  filter(Year %in% anos_validos)





# Comienzo a pegar todas las tablas en una sola gran tabla

consolidado <- left_join(all_gva_summarised,gun_rights,
                              by = c("Year", "State"))

consolidado <- left_join(consolidado, household_income, 
                              by = c("Year", "State"))

consolidado <- left_join(consolidado, poverty, 
                              by = c("Year", "State"))

consolidado <- left_join(consolidado, total_suicides, 
                              by = c("Year", "State"))

consolidado <- left_join(consolidado, unemployment, 
                              by = c("Year", "State"))

consolidado <-  left_join(consolidado, population, 
                               by = c("Year", "State"))



# Cambios esteticos en Poverty y Unemployment


consolidado <- consolidado %>% 
  mutate(Over_All_Poverty = Over_All_Poverty*100,
         Unemployment_Rate = Unemployment_Rate*100)



stargazer(as.data.frame(consolidado), type = "text")

colnames(consolidado)

# Aqui comienzo a ejecutar el modelo de panel (el momento mas esperado
# por toda latinoamerica unida)

# Primero voy a correr un modelo que busque explicar el impacto del derecho al
# porte de armas en el numero de tiroteos

Y <- cbind(consolidado$Incident_Count)

X <- cbind(consolidado$Gun_Rights, consolidado$Median_Household_Income,
           consolidado$Over_All_Poverty, consolidado$Unemployment_Rate
           )

# Aqui le digo al R que mi tabla consolidado es para hacer un modelo de panel

pdata <- pdata.frame(consolidado, index = c("State", "Year"))

# Unas estadisticas descriptivas

summary(Y)
summary(X)

# Tenemos entendido de que hay diferentes tipos de regresiones panel
# de acuerdo a las caracteristicas de la data, primero sacarare todos los modelos 
# que se pueden sacar y luego decidire cual es conveniente


# Primero sacaremos el modelo que tiene como principal objetivo evaluar el impacto 
# que tiene las leyes relacionadas con el porte de armas en los tiroteos masivos

# Aclaro aca que las lineas del codigo de stargazer tienen 2 objetivos: 1) imprimir
# la data para verla en el R y 2) imprimir la regresion en formato html para su uso
# en otros lados

# Pooled OLS estimator

grpooling <- plm(pdata$Incident_Count ~ pdata$Gun_Rights + pdata$Median_Household_Income +
                pdata$Over_All_Poverty + pdata$Unemployment_Rate,
                data = pdata, model = "pooling")


stargazer(grpooling ,  type = "text")

#stargazer(grpooling ,  type = "html", out = "grpooling.html")

# Between estimator

grbetween <- plm(pdata$Incident_Count ~ pdata$Gun_Rights + pdata$Median_Household_Income +
                 pdata$Over_All_Poverty + pdata$Unemployment_Rate,
               data = pdata, model = "between")

stargazer(grbetween ,  type = "text")

#stargazer(grbetween ,  type = "html", out = "grbetween.html")


# First differences estimator

grfirstdiff <- plm(pdata$Incident_Count ~ pdata$Gun_Rights + pdata$Median_Household_Income +
                 pdata$Over_All_Poverty + pdata$Unemployment_Rate,
               data = pdata, model = "fd")

stargazer(grfirstdiff ,  type = "text")

#stargazer(grfirstdiff ,  type = "html", out = "grfirstdiff.html")

# Fixed effects or within estimator

grfixed <- plm(pdata$Incident_Count ~ pdata$Gun_Rights + pdata$Median_Household_Income +
                   pdata$Over_All_Poverty + pdata$Unemployment_Rate,
                 data = pdata, model = "within")


stargazer(grfixed ,  type = "text")

#stargazer(grfixed ,  type = "html", out = "grfixed.html")


# Random effects estimator

grrandom <- plm(pdata$Incident_Count ~ pdata$Gun_Rights + pdata$Median_Household_Income +
               pdata$Over_All_Poverty + pdata$Unemployment_Rate,
             data = pdata, model = "random")


stargazer(grrandom ,  type = "text")

#stargazer(grrandom ,  type = "html", out = "grrandom.html")




# Realizamos la prueba Hausman que nos sirve para determinar si debemos usar el modelo
# de fixed effects o el de random effects


phtest(grrandom, grfixed)


# La prueba nos dice que debemos usar el modelo de efectos random


# Ahora, realizaremos el mismo modelo peroo en lugar de tener una metrica del derecho
# al porte de armas, tendremos una que nos sirve de proxy a cantidad de armas en la poblacion
# como lo es el porcentaje de suicidios cometidos con armas de fuego



# Pooled OLS estimator

frpooling <- plm(pdata$Incident_Count ~ pdata$firearm_Rate + pdata$Median_Household_Income +
                 pdata$Over_All_Poverty + pdata$Unemployment_Rate,
               data = pdata, model = "pooling")


stargazer(frpooling ,  type = "text")

#stargazer(frpooling ,  type = "html", out = "frpooling.html")

# Between estimator

frbetween <- plm(pdata$Incident_Count ~ pdata$firearm_Rate + pdata$Median_Household_Income +
                 pdata$Over_All_Poverty + pdata$Unemployment_Rate,
               data = pdata, model = "between")

stargazer(frbetween ,  type = "text")

#stargazer(frbetween ,  type = "html", out = "frbetween.html")


# First differences estimator

frfirstdiff <- plm(pdata$Incident_Count ~ pdata$firearm_Rate + pdata$Median_Household_Income +
                   pdata$Over_All_Poverty + pdata$Unemployment_Rate,
                 data = pdata, model = "fd")

stargazer(frfirstdiff ,  type = "text")

#stargazer(frfirstdiff ,  type = "html", out = "frfirstdiff.html")

# Fixed effects or within estimator

frfixed <- plm(pdata$Incident_Count ~ pdata$firearm_Rate + pdata$Median_Household_Income +
               pdata$Over_All_Poverty + pdata$Unemployment_Rate,
             data = pdata, model = "within")

summary(random)

stargazer(frfixed ,  type = "text")

#stargazer(frfixed ,  type = "html", out = "frfixed.html")


# Random effects estimator

frrandom <- plm(pdata$Incident_Count ~ pdata$firearm_Rate + pdata$Median_Household_Income +
                pdata$Over_All_Poverty + pdata$Unemployment_Rate,
              data = pdata, model = "random")


stargazer(frrandom ,  type = "text")

#stargazer(frrandom ,  type = "html", out = "frrandom.html")




# Realizamos la prueba Hausman que nos sirve para determinar si debemos usar el modelo
# de fixed effects o el de random effects


phtest(random, fixed)



# De nuevo, la prueba hausman nos dice que usemos el modelo de efectos random



chart1 <- all_gva %>%
  group_by(Year) %>% 
  summarise(Incident_Count = length(Incident_ID)) %>% 
  ggplot() +
  geom_col(aes(Year,Incident_Count, fill = Incident_Count)) +
  scale_fill_gradient2( low = "white", high = "red" )+
  theme_classic() +
  theme(legend.position = "none")+
  labs(title="Number of Mass Shootings in USA",
       subtitle = "From 2014 to 2021",
       x="Year", y = "Number of Incidents")


chart1
#Debido a que aparentemente una funcion que necesito no es pipefrieldly
#creo proto_chart2


proto_chart2 <- all_gva %>% 
  group_by(State, fips, abbr) %>% 
  summarise(Incident_Count = length(Incident_ID)) %>% 
  ungroup()
chart2<-  plot_usmap( data = proto_chart2,
                      values = "Incident_Count",
                      labels = T) +
  scale_fill_gradient(low = "white",
                      high = "red",
                      name = NULL) +
  theme_classic()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank())+
  
  labs(title="Mass Shooting in USA divided by State",
       subtitle = "From 2014 to 2021")

chart2

#Quiero crear un chart que me muestre la distribucion de los incidentes de 
#mass shooting por numero de personas asesinadas

chart3 <- all_gva %>%
  ggplot(aes(x = Killed)) +
  geom_bar(fill = "red")+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title="Number of Mass Shootings by Amount of People Killed",
       subtitle = "Between 2014 and 2021 in USA",
       x="People Killed", y = "Number of Incidents")

chart3

#El chart 3 tiene unos valores outliers muy extremos. Asi que, para
#tener una foto mas cercana de los datos proceso a acortar el axis x


chart4 <- chart3 +
  scale_x_continuous(limits = c(-1,11),
                     breaks = seq(0,10, by =1),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  scale_y_continuous(limits = c(-0,1750),
                     breaks = seq(0,1750, by =250),
                     minor_breaks = NULL,
                     expand = c(0,0))

chart4

#El granfico anterior nos dio una magnitud del valor absoluto de los 
#datos en la variable Killed. Ahora,veremos cual es la representacion
#relativa de los mismos. 


chart5 <- all_gva %>%
  group_by(Killed) %>% 
  summarise(Incident_Count = length(Incident_ID)) %>%
  mutate(Killed_Percentage = Incident_Count / sum(Incident_Count) *100,
         Killed = ifelse(Killed > 4,
                         yes = "+4",
                         no = Killed),
         Killed = paste(Killed, "People Killed", sep = " " ),
         Killed = fct_reorder(Killed, Killed_Percentage)) %>% 
  ggplot() +
  geom_col(aes(
    x =Killed_Percentage,
    y =Killed,
    fill = "red")) +
  theme_classic() +
  theme(legend.position = "none")+
  labs(title="Mass Shootings by Number of People Killed",
       subtitle = "Between 2014 and 2021 in USA",
       x="Percentage of Total Mass Shootings", y = "Number of People Killed")

chart5

#Ahora haremos lo mismo con los heridos, a ver que conseguimos

chart6 <- all_gva %>%
  ggplot(aes(x = Injured)) +
  geom_bar(fill = "red")+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title="Number of Mass Shootings by Amount of People Injured",
       subtitle = "Between 2014 and 2021 in USA",
       x="People Injured", y = "Number of Incidents")

chart6

#Pasa lo mismo que en el caso de Killed, vamos a acotar los datos 
# a ver que conseguimos

chart7 <- chart6 +
  scale_x_continuous(limits = c(-1,30),
                     breaks = seq(0,30, by =1),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  scale_y_continuous(limits = c(-0,1750),
                     breaks = seq(0,1750, by =250),
                     minor_breaks = NULL,
                     expand = c(0,0))

chart7

#Como en Killed, vamos a sacar la cantidad relativa de las personas que 
#fueron injured a lo largo de los a;os de data

chart8 <- all_gva %>%
  group_by(Injured) %>% 
  summarise(Incident_Count = length(Incident_ID)) %>%
  mutate(Injured_Percentage = Incident_Count / sum(Incident_Count) *100,
         Injured = ifelse(Injured > 5,
                          yes = "+6",
                          no = Injured),
         Injured = paste(Injured, "People Injured", sep = " " ),
         Injured = fct_reorder(Injured, Injured_Percentage)) %>% 
  ggplot() +
  geom_col(aes(
    x = Injured_Percentage,
    y = Injured,
    fill = "red")) +
  theme_classic() +
  theme(legend.position = "none")+
  labs(title="Mass Shootings by Number of People Injured",
       subtitle = "Between 2014 and 2021 in USA",
       x="Percentage of Total Mass Shootings", y = "Number of People Injured")

chart8


View(consolidado)

chart9 <- consolidado %>% 
  mutate(Gun_Rights = ifelse(Gun_Rights > 0,
                             yes = ">0",
                             no = "<0")) %>% 
  group_by(Year, Gun_Rights) %>% 
  summarise(Incident_Count = sum(Incident_Count)) %>% 
  ggplot(aes(x = Year, y = Incident_Count, color = Gun_Rights)) +
  geom_point()+
  theme_clean()+
  scale_y_continuous(limits = c(0,250),
                     breaks = seq(0,250, by =50),
                     minor_breaks = NULL,
                     expand = c(0,0))+
  labs(title="Conteo de Tiroteos Masivos por Año",
       subtitle = "Divididos por Leyes al Porte de Arma",
       x="Año", y = "Número de Tiroteos Masivos")

chart9


ownership_chart_2<- consolidado %>%
  filter(Year %in% c(2014, 2019)) %>%
  ggplot() +
  geom_jitter(position = position_jitter(seed=1), aes((firearm_Rate), Incident_Count, color = c(Year)))+
  geom_text(position = position_jitter(seed=1), aes((firearm_Rate), Incident_Count, label = paste(abbr, Year))) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Tasa de porte de armas por estado (log scale)") +
  ylab("Incidentes (log scale)") +
  ggtitle("Porte de armas en EEUU durante 2014", ) +
  geom_abline(intercept = log10(r), lty = 5, color = "red")+
  theme_classic() +
  scale_x_continuous(limits = c(0,80),
                     breaks = seq(0,80, by =5),
                     minor_breaks = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, by =1),
                     minor_breaks = NULL,
                     expand = c(0,0))

chart_shootings_by_state <- data_arm_rights %>%
  ggplot() +
  geom_line(aes(Year, mean_total, col=leyes))+
  theme_classic()

# FIN