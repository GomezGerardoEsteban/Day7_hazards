

# limpieza del ambiente ---------------------------------------------------

rm(list = ls())


# librerias ---------------------------------------------------------------

library(tidyverse)
library(ggtext)


setwd("E:\\Documents\\Escritorio\\FLACSO\\Tesis\\petroleo\\rmd\\resultados\\graficos\\30DayChartChallenge")

baseWho_ <- read.csv2("WHOMortalityDatabase_Road_traffic_accidents.csv")

baseWho_ %>% 
  filter(Region.Name %in% c("North America and the Caribbean", "Central and South America") & Year == 2020) %>% 
  select(Country.Name) %>% 
  pull() 


vector_filtro <- c("ARG", "BRA", "CHL", "COL", )

baseWho_ %>% filter(Country.Name == "Colombia",
                    Age.group.code == "Age_all",
                    Sex == "All") %>% 
  ggplot(mapping = aes(x = Year, y = Number)) +
  geom_line() +
  labs(title = "Gráfico 1: Número de muertes por accidentes de tránsito en Colombia",
       x = NULL,
       y = "Personas fallecidas",
       caption = "Fuente: elaboración propia en base a OMS") +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black"))


baseWho_R <- baseWho_ %>% 
  filter(Age.group.code == "Age_all",
         Sex == "All") %>% 
  group_by(Region.Code, Region.Name, Year) %>% 
  summarise(promedioTasa = mean(Death.rate.per.100.000.population))

EU <- baseWho_ %>% filter(Age.group.code == "Age_all",
                          Sex == "All",
                          Country.Name == "United States of America") %>% 
  group_by(Country.Code, Year) %>% 
  summarise(promedioTas = Death.rate.per.100.000.population )

baseWho_R %>%
  left_join(y = EU, by = c("Year" = "Year")) %>% 
  filter(Region.Code == "EU" | Region.Code == "CSA",
         Year < 2021) %>% 
  ggplot(mapping = aes(x = Year, y = promedioTasa, color = Region.Code)) +
  geom_line(show.legend = F) +
  geom_line(aes(x = Year, y = promedioTas), color = "#D0BE38") +
  scale_color_manual(values = c("#1A6D0B", "#0B426D")) +
  labs(title = "Gráfico 2: Evolución del número de fallecidos por cada 100.000 personas \n en accidentes de tránsito",
       x = NULL,
       y = "Fallecidos por cada 100.000 personas",
       caption = "Fuente: elaboración propia en base a OMS") +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20) +
  annotate(geom = "text",
           x = c(2015, 2015, 1980),
           y = c(8, 15, 25),
           label = c("Europa",
                     "Latinoamérica",
                     "USA"),
           size = 2,
           color = c("#0B426D", "#1A6D0B", "#D0BE38")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black"))


baseWho_ %>% filter(Country.Name == "Colombia",
                    Age.group.code == "Age_all",
                    Sex == "All",
                    Year > 1999 & Year < 2020) %>%
  mutate(estrellasNegras = ifelse(Year <= 2005, 13.1, NA)) %>% 
  ggplot(mapping = aes(x = Year, y = Death.rate.per.100.000.population)) +
  geom_line() + 
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(breaks = c(2000, 2005, 2013, 2015, 2019)) +
  labs(title = "Gráfico 3: Evolución del número de fallecidos por cada 100.000 personas \n en accidentes de tránsito en Colombia",
       x = NULL,
       y = "Fallecidos por cada 100.000 personas",
       caption = "Fuente: elaboración propia en base a OMS") +
  geom_vline(xintercept = c(2013), color = c("#8B1F04"), linetype = "longdash", size = 0.5)+
  geom_line(mapping = aes(x = Year, y = estrellasNegras), linetype = "longdash", color = "#4F4708") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black")) +
  annotate(geom = "text",
           x = c(2013.1, 2002.5),
           y = c(16, 13.3),
           label = c("Entrada en vigencia \n Ley 1696",
                     "Campaña Estrellas Negras"),
           size = 2,
           color = c("#8B1F04", "#4F4708"))


basePol <- read.csv2("dataPolicia.csv")

nPolicia <- basePol %>% group_by(year, tipo) %>% 
  summarise(totalYear = sum(CANTIDAD)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(prop = totalYear/sum(totalYear))


nPolicia %>% group_by(year) %>% 
  summarise(total = sum(totalYear)) %>% 
  filter(year < 2016) %>% 
  ggplot(mapping = aes(x = year, y = total)) +
  geom_line() + 
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(breaks = c(2010:2016)) +
  labs(title = "Gráfico 4: Cantidad de accidentes de tránsito con fallecidos o lesionados \n por año en Colombia",
       x = NULL,
       y = "Número de accidentes",
       caption = "Fuente: elaboración propia en base a Policia Nacional de Colombia") +
  geom_vline(xintercept = c(2013), color = c("#8B1F04"), linetype = "longdash", size = 0.5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black")) +
  annotate(geom = "text",
           x = c(2013, 2013, 2014),
           y = c(42000, 46400, 45000),
           label = c("Entrada en vigencia \n Ley 1696",
                     "46202",
                     "45267"),
           size = 2,
           color = c("#8B1F04", "black", "black"))


baseWho_ %>% filter(Country.Name == "Chile",
                    Age.group.code == "Age_all",
                    Sex == "All",
                    Year > 1999 & Year < 2020) %>%
  ggplot(mapping = aes(x = Year, y = Death.rate.per.100.000.population)) +
  geom_line() + 
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(breaks = c(2000, 2003, 2016, 2019)) +
  labs(title = "Gráfico 5: Evolución del número de fallecidos por cada 100.000 personas \n en accidentes de tránsito en Chile",
       x = NULL,
       y = "Fallecidos por cada 100.000 personas",
       caption = "Fuente: elaboración propia en base a OMS") +
  geom_vline(xintercept = c(2016), color = c("#0A26B2"), linetype = "longdash", size = 0.5)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black")) +
  annotate(geom = "text",
           x = c(2015.9),
           y = c(13),
           label = c("Campaña Dile Tú \n Conaset - Chile"),
           size = 2,
           color = c("#0A26B2"))



baseWho_ %>% filter(Country.Name == "Japan" | Country.Name == "Colombia" | Country.Name == "Chile",
                    Age.group.code == "Age_all",
                    Sex == "All",
                    Year > 1985 & Year < 2020) %>% 
  ggplot(mapping = aes(x = Year, y = Death.rate.per.100.000.population, color = Country.Code)) +
  geom_line(show.legend = F) +
  scale_color_manual(values = c("#B20A12", "#69771C", "#040E7C")) +
  labs(title = "Gráfico 6: Evolución del número de fallecidos por cada 100.000 personas \n en accidentes de tránsito",
       x = NULL,
       y = "Personas fallecidas",
       caption = "Fuente: elaboración propia en base a OMS") +
  annotate(geom = "text",
           x = c(2008, 2008, 2008.4),
           y = c(12, 15, 6),
           label = c("Chile",
                     "Colombia",
                     "Japón"),
           size = 3,
           color = c("#B20A12", "#69771C", "#040E7C"))+
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 20) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size=8, hjust=0.5, face="italic", color="black"))
