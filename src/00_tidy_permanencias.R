library(genderizeR)
library(tidyverse)
library(stringi)
library(readr)

sni_permanencias <- readxl::read_xlsx("data/sni_permanencias.xlsx") %>% 
  mutate(primer_nombre = stri_trans_general(str_to_lower(str_extract(Nombre, "\\w+")), "Latin-ASCII"),
         segundo_nombre = stri_trans_general(str_to_lower(str_extract(Nombre, "\\w+$")), "Latin-ASCII"),
         segundo_nombre = case_when(
           primer_nombre == segundo_nombre ~ NA_character_,
           T ~ segundo_nombre),
         nivel_2 = case_when(
           Nivel == "Iniciación dos\r\naños" ~ "Iniciación",
           Nivel == "Iniciación dos años" ~ "Iniciación",         
           Nivel == "Iniciación tres" ~ "Iniciación",
           Nivel == "Iniciación tres\r\naños" ~ "Iniciación",     
           Nivel == "Iniciación tres años" ~ "Iniciación",
           Nivel == "Iniciación un año" ~ "Iniciación",           
           Nivel == "Nivel I dos años" ~ "Nivel I",
           Nivel == "Nivel I tres años" ~ "Nivel I",            
           Nivel == "Nivel I un año" ~ "Nivel I",
           Nivel == "Nivel II cuatro" ~ "Nivel II",              
           Nivel == "Nivel II cuatro\r\naños" ~ "Nivel II", 
           Nivel == "Nivel II cuatro años" ~ "Nivel II",          
           Nivel == "Nivel II dos años" ~ "Nivel II", 
           Nivel == "Nivel II tres años" ~ "Nivel II",           
           Nivel == "Nivel II un año" ~ "Nivel II", 
           Nivel == "Nivel III cuatro\r\naños" ~ "Nivel III",      
           Nivel == "Nivel III cuatro años" ~ "Nivel III",      
           Nivel == "Nivel III cuatro años\r\naños" ~ "Nivel III",      
           Nivel == "Nivel III tres años" ~ "Nivel III" 
         ))

primer_nombre <- sni_permanencias %>% 
  pull(primer_nombre)

segundo_nombre <- sni_permanencias %>% 
  pull(segundo_nombre)

gender_pn <- genderizeR::findGivenNames(primer_nombre, country = "UY", language = "es") %>% 
  rename(primer_nombre = name) %>% 
  distinct(primer_nombre, gender) 

sni_permanencias <- sni_permanencias %>%
  left_join(gender_pn) %>% 
  janitor::clean_names() %>% 
  transmute(nombre, 
            apellido,
            primer_nombre,
            gender,
            area,
            nivel = nivel_2,
            categoria)

faltantes <- sni_permanencias %>% 
  filter(is.na(gender)) %>% 
  pull(nombre)

gender_aux <- genderizeR::findGivenNames(faltantes,  language = "es") %>% 
  rename(primer_nombre = name,
         gender_aux = gender) %>% 
  distinct(primer_nombre, gender_aux) 

sni_permanencias <- sni_permanencias %>% 
  left_join(gender_aux) %>% 
  mutate(gender = coalesce(gender, gender_aux),
         sexo = case_when(
           gender == "male" ~ "Varón",
           gender == "female" ~ "Mujer"),
         nivel = factor(nivel, levels = c("Iniciación", "Nivel I", "Nivel II", "Nivel III"))) %>% 
  select(- starts_with("gender")) %>% 
  write_rds("data/sni_permanencias.rds")
  
  
