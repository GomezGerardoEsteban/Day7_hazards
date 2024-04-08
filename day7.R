

# limpieza del ambiente ---------------------------------------------------

rm(list = ls())


# librerias ---------------------------------------------------------------

library(tidyverse)
library(ggtext)


baseWho_ <- read.csv2("WHOMortalityDatabase_Road_traffic_accidents.csv")

vector_filtro <- c("ARG", "BRA", "CAN", "CHL", "COL", "CRI",
                   "ECU", "GTM", "MEX",
                   "PER", "PRY", "URY", "USA")

unique(baseWho_$Year)

grafico <- baseWho_ %>% filter(Country.Code %in% vector_filtro,
                    Age.group.code == "Age_all",
                    Sex == "All") %>% 
  group_by(Country.Code) %>% 
  mutate(mediana = median(Death.rate.per.100.000.population, na.rm = T),
         media = mean(Death.rate.per.100.000.population, na.rm = T)) %>% 
  ggplot(mapping = aes(x = reorder(Country.Code, desc(mediana)), y = Death.rate.per.100.000.population)) +
  geom_boxplot(width = 0.5) +
  geom_point(mapping = aes(x = reorder(Country.Code, desc(mediana)), y = media), shape = 23, fill = "#B22525", size = 2) +
  geom_point(mapping = aes(x = 12, y = 29), shape = 23, fill = "#B22525", size = 4) +
  annotate(geom = "text",
           x = 10.8,
           y = 29,
           label = "Media del periodo",
           color = "#B22525",
           size = 4) +
  labs(title = "Distribución del número de muertes por 100 mil habitantes en **Accidentes de Transito** (1987 - 2021)",
       subtitle = "<span style='color:#4B5253;'>Los accidentes de transito son la **principal causa de muerte en niños y adultos jovenes** de entre 5 y 29 años</span>",
       x = NULL,
       y = "Personas fallecidas por 100 mil habitantes",
       caption = "Fuente: elaboración propia en base a WHO road traffic injuries<br>**#30DayChartChallenge #Day7**<br>@GEstebanGomez") +
  scale_y_continuous(n.breaks = 20) +
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.0, size = 13),
        plot.subtitle = element_markdown(hjust = 0.0, size = 11),
        axis.text.x = element_text(size = 9), 
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 8),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"))


ggsave(filename = "../rmd/resultados/graficos/30DayChartChallenge/7Day_hazards.png",
       plot = grafico,
       dpi = 500,
       width = 8.66,
       height = 4.6
)
