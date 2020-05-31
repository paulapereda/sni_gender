library(hrbrthemes)
library(tidyverse)
library(readr)

sni_2020 <- read_rds("data/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación" ~ valor/607,
      nivel == "Nivel I" ~ valor/994,
      nivel == "Nivel II" ~ valor/382,
      nivel == "Nivel III" ~ valor/121
    ),
    label = paste0(round((porcentaje)*100), "%"))

ggplot(sni_2020, aes(nivel, porcentaje, group = sexo, color = sexo, label = label)) + 
  geom_line() +
  geom_point() +
  geom_text(data = sni_2020[sni_2020$sexo == "Mujer",], aes(vjust = 1.4)) + 
  geom_text(data = sni_2020[sni_2020$sexo == "Varón",], aes(vjust = -.7)) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  labs(title = "Composición del Sistema Nacioneal de Investigadores según nivel y sexo: mujeres y varones. Año 2020.", 
       caption = "Fuente: elaboración propia en base a datos del SNI.
                 Paula Pereda - @paubgood") +
  theme_ipsum() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(angle = 0)) +
  ggsave("plots/sni_2020.png", dpi = 550, width = 11)

sni_area <- read_rds("data/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel, area) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación" ~ valor/607,
      nivel == "Nivel I" ~ valor/994,
      nivel == "Nivel II" ~ valor/382,
      nivel == "Nivel III" ~ valor/121
    ),
    label = paste0(round((porcentaje)*100), "%"))
  