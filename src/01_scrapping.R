library(dplyr)
library(rvest)
library(stringi)
library(stringr)
library(janitor)

html_sni <- "https://sni.org.uy/buscador/" %>%
  read_html()

tabla_investigadores <- html_sni %>%
  html_nodes(".vc_table.tabla_buscador") %>%
  html_table() %>%
  .[[1]] %>%
  clean_names() %>%
  transmute(categoria, area, sub_area, nivel, investigador,
            nombre   = str_extract_all(investigador, pattern = "\\,.+"),
            apellido = str_extract_all(investigador, pattern = ".+\\,"),
            nombre   = str_replace_all(nombre, pattern = "^, ", replace = ""),
            apellido = str_replace_all(apellido, pattern = " ,$", replace = ""),
            nombre   = str_to_title(nombre),
            apellido = str_to_title(apellido)) %>%
  arrange(categoria, area, sub_area, nivel, apellido) %>% 
  readr::write_rds("data/tabla_investigadores.rds")

sni <- tabla_investigadores %>% 
  mutate(primer_nombre = stri_trans_general(str_to_lower(str_extract(nombre, "\\w+")), "Latin-ASCII"),
         segundo_nombre = stri_trans_general(str_to_lower(str_extract(nombre, "\\w+$")), "Latin-ASCII"),
         segundo_nombre = case_when(
           primer_nombre == segundo_nombre ~ NA_character_,
           T ~ segundo_nombre),
         nivel = factor(nivel, levels = c("Iniciación", "Nivel I", "Nivel II", "Nivel III")))

primer_nombre <- sni %>% 
  pull(primer_nombre)

segundo_nombre <- sni %>% 
  pull(segundo_nombre)

gender_pn <- genderizeR::findGivenNames(primer_nombre, country = "UY", language = "es") %>% 
  rename(primer_nombre = name) %>% 
  distinct(primer_nombre, gender) 

sni <- sni %>%
  left_join(gender_pn) %>% 
  transmute(categoria, 
            area,
            sub_area,
            nivel, 
            investigador,
            nombre,
            apellido,
            primer_nombre,
            gender,
            categoria)

faltantes <- sni %>% 
  filter(is.na(gender)) %>% 
  pull(primer_nombre)

gender_aux <- genderizeR::findGivenNames(faltantes,  language = "es") %>% 
  rename(primer_nombre = name,
         gender_aux = gender) %>% 
  distinct(primer_nombre, gender_aux) 

sni <- sni %>% 
  left_join(gender_aux) %>% 
  mutate(gender = coalesce(gender, gender_aux),
         sexo = case_when(
           gender == "male" ~ "Varón",
           gender == "female" ~ "Mujer"),
         area = ifelse(area == "Ciencias Médicas y de la\r\nSalud", "Ciencias Médicas y de la Salud", area)) %>% 
  select(- gender, - gender_aux) %>% 
  readr::write_rds("data/sni.rds")



