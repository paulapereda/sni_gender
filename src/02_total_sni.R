library(tidyverse)
library(readr)

sni_permanencias <- read_rds("data/sni_permanencias.rds") %>% 
  select(nombre, apellido, primer_nombre, nivel, area, sexo)

total_sni <- read_rds("data/sni.rds") %>% 
  select(nombre, apellido, primer_nombre, nivel, area, sexo) %>% 
  bind_rows(sni_permanencias) %>% 
  distinct(nombre, apellido, primer_nombre, nivel, area, sexo) %>% 
  filter(nombre != "álvaro Gustavo") %>% 
  filter(nombre != "álvaro Hugo") %>% 
  #filter(!(nombre == "Andrés Ezequiel" & nivel == "Nivel II"))
  write_rds("data/total_sni.rds")

