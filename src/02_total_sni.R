library(tidyverse)
library(readr)

sni_permanencias <- read_rds("data/sni_permanencias.rds") %>% 
  select(nombre, apellido, primer_nombre, nivel, area, sexo)

total_sni <- read_rds("data/sni.rds") %>% 
  select(nombre, apellido, primer_nombre, nivel, area, sexo) %>% 
  bind_rows(sni_permanencias) %>% 
  mutate(area = case_when(
    area == "Ciencias Médicas y de la\r\nSalud" ~ "Ciencias Médicas y de la Salud",
    T ~ area
  ),
  nombre = str_trim(nombre, side = "right"),
  apellido = str_trim(apellido, side = "right")) %>% 
  distinct(nombre, apellido, primer_nombre, nivel, area, sexo) %>% 
  group_by(apellido, nombre) %>% 
  summarise(primer_nombre = first(primer_nombre),
            nivel = last(nivel),
            area = first(area),
            sexo = first(sexo)) %>% 
  ungroup() %>% 
  filter(nombre != "álvaro Gustavo") %>% 
  filter(nombre != "Bernardo Rómulo") %>% 
  filter(nombre != "César Augusto") %>% 
  filter(apellido != "Abreu") %>% 
  filter(nombre != "álvaro Hugo") %>% 
  filter(nombre != "Verónica Lucy") %>% 
  filter(nombre != "Sergio Alexandre") %>% 
  filter(nombre != "Carlos Tabaré Fidel") %>% 
  filter(nombre != "Federico Jesãºs") %>% 
  filter(apellido != "Fraiman") %>% 
  filter(apellido != "Fornaro") %>% 
  filter(apellido != "Varela Pensado,") %>% 
  filter(apellido != "Perdomo") %>% 
  filter(apellido != "Perciante") %>%
  filter(apellido != "Peluffo Bossio,") %>% 
  filter(apellido != "Paulino") %>%
  filter(apellido != "Otero") %>%
  filter(apellido != "Olivera Cajiga,") %>%
  filter(apellido != "Olazabal") %>%
  filter(apellido != "Vanrell Majó,") %>% 
  filter(apellido != "Achigar Pereira") %>% 
  filter(apellido != "álvarez Pedrosian") %>%
  filter(apellido != "Trías Tejería") %>% 
  filter(apellido != "Tancredi Machado") %>% 
  filter(apellido != "Yannicelli") %>% 
  filter(apellido != "Viola") %>% 
  filter(apellido != "Souza Antognazza") %>% 
  filter(apellido != "Soust-verdaguer") %>% 
  filter(apellido != "Slomovitz") %>% 
  filter(apellido != "Sensale") %>% 
  filter(apellido != "Schelotto Guillamon") %>% 
  filter(apellido != "Sanchez Tellechea") %>% 
  filter(apellido != "Sanabria") %>% 
  filter(apellido != "Romero Rodriguez") %>% 
  filter(apellido != "Romano Granito,") %>%
  filter(apellido != "Rittatore Calvo,") %>% 
  filter(apellido != "Risso Ferrand,") %>% 
  filter(apellido != "Pouso") %>% 
  filter(apellido != "Potrie") %>% 
  filter(apellido != "Pizarro") %>% 
  filter(apellido != "Méndez") %>% 
  filter(apellido != "Mazzilli Vanzini,") %>% 
  filter(apellido != "Martínez D´alto") %>% 
  filter(apellido != "Piñeiro Guerra,") %>% 
  filter(apellido != "Perez Miles") %>%
  filter(apellido != "Perez De Sierra") %>% 
  filter(apellido != "Markarian Abrahamian") %>% 
  filter(apellido != "Marizcurrena") %>% 
  filter(apellido != "Lanzaro Peyre,") %>% 
  filter(apellido != "Irisarri") %>% 
  filter(apellido != "Langone") %>% 
  filter(apellido != "Gonzalez Guyer") %>%
  filter(apellido != "Enciso Deleon") %>% 
  filter(apellido != "Dominguez") %>% 
  filter(apellido != "De Cores") %>% 
  filter(apellido != "D´anatro Gómez") %>% 
  filter(apellido != "Crosignani") %>% 
  filter(apellido != "Cristiani Labat,") %>% 
  filter(apellido != "Chiappe") %>% 
  filter(apellido != "Celentano Campodonico") %>% 
  filter(apellido != "Cavestany  Böcking,") %>% 
  filter(apellido != "Broquetas") %>% 
  filter(apellido != "Brito Diaz") %>% 
  filter(apellido != "Brida") %>% 
  filter(apellido != "Block Teper") %>% 
  filter(!(nombre == "Camou Soliño" & apellido == "Maria Teresa")) %>% 
  filter(!(nombre == "Paula Maria" & apellido == "Cardellino Alvarez")) %>% 
  filter(!(nombre == "Adriana María" & apellido == "Cassina Gomez")) %>% 
  filter(!(nombre == "Amílcar" & apellido == "Davyt")) %>% 
  filter(!(nombre == "Andrea" & apellido == "Delgado Cavaliere")) %>% 
  filter(!(nombre == "Pablo Andres" & apellido == "Denis Marinoni")) %>% 
  filter(!(nombre == "Laura Pilar" & apellido == "Domínguez Llera")) %>% 
  filter(!(nombre == "Maria Ines" & apellido == "Fariello Rico")) %>% 
  filter(!(nombre == "Guzman" & apellido == "Favre Silva")) %>% 
  filter(!(nombre == "Gustavo" & apellido == "Vazquez")) %>% 
  filter(!(nombre == "álvaro" & apellido == "Rovella Osores")) %>% 
  filter(!(nombre == "Pablo" & apellido == "Rodríguez Bocca")) %>% 
  filter(!(nombre == "Mariela" & apellido == "Quiñones Montoro")) %>% 
  filter(!(nombre == "Diego" & apellido == "Queirolo"))  %>% 
  filter(!(nombre == "álvaro" & apellido == "Martín Menoni")) %>% 
  write_rds("data/total_sni.rds")
