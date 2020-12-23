rm(list=ls())

library(jsonlite) #for å importere data fra SSR
library(gganimate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gifski)

json <- "https://ssr.finanstilsynet.no/api/Issuers?extension=json" #Importere data fra SSR
SSRD <- fromJSON(json, flatten = TRUE) #Assign json data til en ny DF_SSRD

top_shorted <- SSRD[SSRD$ShortPercent > 1, ] #Filtrere SSRD-datasettet så den viser selskaper som er shortet over 1%
top_shorted <- top_shorted[-c(1,5:6)] #renser datasettet (fjerner informasjon som ikke trengs, bl.a. ISIN, last change)

mean(top_shorted$ShortPercent)
mean(top_shorted$ShortedSum)

#Skal nå skille ut selskapet som er mest shortet (prosentvis) og deretter plotte selskapets shorthistorie.
summary(top_shorted) #selskapet som er mest shortet står under ShortPercent, Max:

TS_Stock <- top_shorted[which.max(top_shorted$ShortPercent),] #Filtrerer ut det mest shortede selskapet (x).


TS_Stock$Name  #Se om koden returnerer riktig selskap.
#View(top_shorted)

TS_Pos <- as.data.frame(TS_Stock$Positions) #Oppretter en ny DF til posisjonene innad i selskap x.
TS_Pos <- TS_Pos[-c(1,2,7:8)] #Renser opp datasettet til posisjonene i selskap x.
TS_Pos$ShortingDate <- as.POSIXct(TS_Pos$ShortingDate) #Gjør om kolonne 1 til datoformat

summary(TS_Pos)
str(TS_Pos)

colnames(TS_Pos)
names(TS_Pos)[1] <- "Date"


#plotter selskapets shorthistorie
ggplot(data = TS_Pos, aes(x=Date, y=ShortPercent, colour=PositionHolder)) + 
  geom_point() + 
  ggtitle(TS_Stock) +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black")) +
  labs(y = "Short %", color = "Position Holder")

# https://www.youtube.com/watch?v=SnCi0s0e4Io


