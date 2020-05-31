library(hrbrthemes)
library(tidyverse)
library(readr)

read_rds("data/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación" ~ valor/559,
      nivel == "Nivel I" ~ valor/924,
      nivel == "Nivel II" ~ valor/345,
      nivel == "Nivel III" ~ valor/113
    ),
    label = paste0(round((porcentaje)*100), "%")) %>%
  ggplot(aes(nivel, porcentaje, group = sexo, color = sexo, label = label)) +
  geom_line() +
  geom_point() +
  geom_text(data = sni_2020[sni_2020$sexo == "Mujer", ], aes(vjust = 1.4), size = 3.5) +
  geom_text(data = sni_2020[sni_2020$sexo == "Varón", ], aes(vjust = -.7), size = 3.5) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = seq(0, 0.85, by = .1),) +
  labs(title = "Composición del Sistema Nacional de Investigadores según nivel y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.
                  Paula Pereda - @paubgood") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  ggsave("plots/sni_2020.png", dpi = 550, width = 11, height = 6)

sni_area <- read_rds("data/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, area) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      area == "Ciencias Agrícolas" ~ valor/236,
      area == "Ciencias Médicas y de la Salud" ~ valor/239,
      area == "Ciencias Naturales y Exactas" ~ valor/689,
      area == "Ciencias Sociales"	 ~ valor/416,
      area == "Humanidades" ~ valor/168,
      area == "Ingeniería y Tecnología" ~ valor/193),
    label = paste0(round((porcentaje)*100), "%"),
    area = factor(area, levels = c("Humanidades", "Ciencias Médicas y de la Salud",
                                   "Ciencias Sociales", "Ciencias Agrícolas",
                                   "Ciencias Naturales y Exactas", "Ingeniería y Tecnología" )))

ggplot(sni_area, aes(area, porcentaje, fill = sexo, label = label)) +
  geom_bar(position = "fill", stat = "identity", alpha = .7) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  geom_text(aes(label = label), position = position_fill(vjust = .5), family = "Arial Narrow") +
  labs(title = "Investigadores del SNI según área de conocimiento y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.
                  Paula Pereda - @paubgood") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  ggsave("plots/sni_area_2020.png", dpi = 550, width = 12, height = 6)

sni_area_nivel <- read_rds("data/total_sni.rds") %>%
  filter(!is.na(sexo)) %>%
  group_by(sexo, nivel, area) %>%
  summarise(valor = n()) %>%
  ungroup() %>%
  mutate(
    porcentaje = case_when(
      nivel == "Iniciación"	& area == "Ciencias Agrícolas" ~ valor/92,
      nivel == "Iniciación"	& area == "Ciencias Médicas y de la Salud" ~ valor/75,
      nivel == "Iniciación"	& area == "Ciencias Naturales y Exactas" ~ valor/141,
      nivel == "Iniciación"	& area == "Ciencias Sociales"	 ~ valor/146,
      nivel == "Iniciación"	& area == "Humanidades" ~ valor/50,
      nivel == "Iniciación"	& area == "Ingeniería y Tecnología" ~ valor/55,
      nivel == "Nivel I" & area == "Ciencias Agrícolas" ~ valor/92,
      nivel == "Nivel I" & area == "Ciencias Médicas y de la Salud" ~ valor/116,
      nivel == "Nivel I" & area == "Ciencias Naturales y Exactas" ~ valor/351,
      nivel == "Nivel I" & area == "Ciencias Sociales" ~ valor/202,
      nivel == "Nivel I" & area == "Humanidades" ~ valor/80,
      nivel == "Nivel I" & area == "Ingeniería y Tecnología" ~ valor/83,
      nivel == "Nivel II" & area == "Ciencias Agrícolas" ~ valor/40,
      nivel == "Nivel II"	& area == "Ciencias Médicas y de la Salud" ~ valor/30,
      nivel == "Nivel II" & area == "Ciencias Naturales y Exactas" ~ valor/148,
      nivel == "Nivel II" & area == "Ciencias Sociales" ~ valor/53,
      nivel == "Nivel II" & area == 	"Humanidades"	 ~ valor/27,
      nivel == "Nivel II" & area == "Ingeniería y Tecnología" ~ valor/47,
      nivel == "Nivel III" & area == 	"Ciencias Agrícolas" ~ valor/12,
      nivel == "Nivel III" & area == 	"Ciencias Médicas y de la Salud" ~ valor/18,
      nivel == "Nivel III" & area == 	"Ciencias Naturales y Exactas" ~ valor/49,
      nivel == "Nivel III" & area == 	"Ciencias Sociales" ~ valor/15,
      nivel == "Nivel III" & area == 	"Humanidades"	 ~ valor/11,
      nivel == "Nivel III" & area == 	"Ingeniería y Tecnología"	 ~ valor/8
    ),
    label = paste0(round((porcentaje)*100), "%")) 

ggplot(sni_area_nivel, aes(nivel, porcentaje, group = sexo, color = sexo, label = label)) +
  geom_line() +
  geom_point() +
  geom_text(data = sni_area_nivel[sni_area_nivel$sexo == "Mujer", ], aes(vjust = 1.4), size = 2.5) +
  geom_text(data = sni_area_nivel[sni_area_nivel$sexo == "Varón", ], aes(vjust = -.7), size = 2.5) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Varón" = "#58c1aa", "Mujer" = "#7c2ef0")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = seq(0, .95, by = .1),
                     expand = c(.1, .1)) +
  labs(title = "Composición del Sistema Nacional de Investigadores según nivel y sexo: mujeres y varones. Año 2020.",
       caption = "Fuente: elaboración propia en base a datos del SNI.
                 Paula Pereda - @paubgood") +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 8, vjust = 3, angle = 0),
        axis.text.y = element_text(size = 11),
        axis.line.x = element_line(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "grey30", hjust = 0.5),
        plot.caption = element_text(colour = "grey50"),
        text = element_text(family = "Arial Narrow"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(~ area) +
  ggsave("plots/sni_area_nivel_2020.png", dpi = 550, width = 11, height = 7)