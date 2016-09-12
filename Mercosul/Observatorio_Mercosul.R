# BIBLIOTECA --------------------------------------------------------------

library(bit64) # leitura de dados numericos mais rapida
library(descr) # transformar microdados em uma base csv
library(survey) # amostragem complexa
library(data.table) # manipulacao de dados
library(dplyr) # manipulacao de dados
library(magrittr) # pacote adicional ao dplyr
library(tidyr) # agrupar colunas de variaveis
library(stringr)


rm(list = ls())
gc()
options(scipen=999) # disable scientific notation

# LEITURA -----------------------------------------------------------------

# Ler todos os anos da PNAD
pnad_read <- function() {
  
  pnad = list()
  # Diretorio dos dados e do dicionario
  pnad_path <- 'C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD/'
  
  year.list = list.files(path = pnad_path,
                         pattern = "[0-9]{4}")

  for(ano in year.list){
    
    print(paste("Lendo arquivo do ano de ", ano))
    
    temp_data <- fread(paste0(pnad_path,ano,'/PES',ano,'.csv'), 
                       sep = "\t", integer64 = 'double', 
                       select= c('v0401', # id e filtro
                                 'v8005', 'v0302', 'v4729',  # idade, sexo, peso e rural
                                 'v4704', 'v4745', 'v4805', # PEA ocupa educacao
                                 'v9058', 'v9101', 'v9105', # tempo trabalho
                                 'v4706', 'v4718', 'v9907' # assalariado   renda  cnae-dom
                       ))
    temp_data$ano = ano
    pnad[[ano]] <- temp_data
  }
  return(rbindlist(pnad))
}

pnad = pnad_read()

# Excluir pensionista, empregado doméstico e parentes do empregado
pnad[ v0401 %in% 1:5 ] -> pnad

# Divindo a populacao em intervalo de 10 anos
pnad[, idadeF10 := cut(pnad$v8005, 
                         breaks = c(-1, seq(14, 64, by = 10), 150 ))]
pnad[, idadeF10 := as.character(idadeF10)]

pnad[ , one := 1]


# PEA ---------------------------------------------------------------------

# Por idade, por sexo e escolaridade
pnad[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(ano, idadeF10, v0302, v4745)] %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745', 'V1'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO', 'TOTAL')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# OCUPADO -----------------------------------------------------------------

# Por idade, por sexo e escolaridade
pnad[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(ano, idadeF10, v0302, v4745)] %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745', 'V1'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO', 'TOTAL')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# DESOCUPADO --------------------------------------------------------------

# Por idade, por sexo e escolaridade
pnad[v8005 >= 15 & v4805 %in% 2, sum(v4729), .(ano, idadeF10, v0302, v4745)] %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745', 'V1'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO', 'TOTAL')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# TAXA DE ATIVIDADE -------------------------------------------------------
# PEA / Populacao total
pnad[v8005 >= 15] %>% 
  group_by(ano, v0302, idadeF10, v4745) %>% 
  summarise(pea = sum(v4704 %in% 1*v4729, na.rm = T),
            pop = sum(one*v4729, na.rm = T)) %>% 
  mutate(`Tasa Actividad` = round(pea/pop*100, 2)) %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  ungroup() %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  select(ano:v4745, starts_with('Tasa')) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# TAXA DE EMPREGO ---------------------------------------------------------
# Ocupado / Populacao total
pnad[v8005 >= 15] %>% 
  group_by(ano, v0302, idadeF10, v4745) %>% 
  summarise(ocupado = sum(v4805 %in% 1*v4729, na.rm = T),
            pop = sum(one*v4729, na.rm = T)) %>% 
  mutate(`Tasa Empleo` = round(ocupado/pop*100, 2)) %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  ungroup() %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
        ) %>% 
  select(ano:v4745, starts_with('Tasa')) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)



# TAXA DE DESEMPREGO -------------------------------------------------------
# Desocupado / PEA

pnad[v8005 >= 15] %>% 
  group_by(ano, v0302, idadeF10, v4745) %>% 
  summarise(ocupado = sum(v4805 %in% 1*v4729, na.rm = T),
            desocupado = sum(v4805 %in% 2*v4729, na.rm = T)) %>% 
  mutate(`Tasa Desocupación` = round(desocupado/(ocupado+desocupado)*100, 2)) %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  ungroup() %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  select(ano:v4745, starts_with('Tasa')) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# TAXA SUBEMPREGO ---------------------------------------------------------

# Ocupado em menos de 35h semanais dispostos a trabalhar mais horas / PEA

# Tenho quantidade de horas semanais 
# Tenho se o cara procura emprego 
# Não tenho disposicao de trabalhar mais horas

# Não é possível

# pnad[is.na(v9058), v9058 := 0]
# pnad[is.na(v9101), v9101 := 0]
# pnad[is.na(v9105), v9105 := 0]
# 
# pnad %>% 
#   mutate(horas_trabalho = v9058 + v9101 + v9105) -> pnad
# 
# 
# pnad[, horas_trabalho := ifelse(v4805 %in% 1, horas_trabalho, NA)]
# pnad[, trab_parcial := as.numeric(horas_trabalho <= 35)]


# TAXA EMPREGO SEM REGISTRO - ASSALARIADO ---------------------------------

# assalariado sem registro / assalariado total
# Assalariado
pnad[v8005 >= 15 & v4706 %in% c(1:7), sum(v4729), .(ano, idadeF10, v0302, v4745)] %>% 
  arrange(ano, v0302, idadeF10, v4745)

# Assalariado sem registro
pnad[v8005 >= 15 & v4706 %in% c(4,7), sum(v4729), .(ano, idadeF10, v0302, v4745)] %>% 
  arrange(ano, v0302, idadeF10, v4745)




pnad[v8005 >= 15] %>% 
  group_by(ano, v0302, idadeF10, v4745) %>% 
  summarise(sem_registro = sum(v4706 %in% c(4,7)*v4729, na.rm = T),
            assalariado = sum(v4706 %in% c(1:7)*v4729, na.rm = T)) %>% 
  mutate(`TASA DE EMPLEO NO REGISTRADO` = round(sem_registro/(assalariado)*100, 2)) %>% 
  arrange(ano, v0302, idadeF10, v4745) %>% 
  ungroup() %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         idadeF10 = ifelse(idadeF10 == '(14,24]','15 a 24 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(24,34]','25 a 34 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(34,44]','35 a 44 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(44,54]','45 a 54 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(54,64]','55 a 64 años', idadeF10),
         idadeF10 = ifelse(idadeF10 == '(64,150]','65 más', idadeF10),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745)
  ) %>% 
  setnames(., c('ano', 'idadeF10', 'v0302', 'v4745'),
           c('AÑO', 'GRUPOS DE EDAD', 'SEXO', 'NIVEL EDUCATIVO')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)


# RENDA MÉDIA -------------------------------------------------------------


# v4706
# 1- Patrão ou empregador
# 2- Empregado (inclui empregado doméstico)
# 3- Conta própria
# 4- Trabalhador auxiliar familia
# 5 - Trabalhador próprio consumo

pnad[v8005 >= 15 & v4805 %in% 1 & v4718 != 999999999999, 
       weighted.mean(v4718, v4729, na.rm = T), 
       .(ano, v0401, v4706, v0302, v4745)] %>% 
  arrange(ano, v0302, v4706, v4745, v0401) %>% 
  mutate(v0302 = ifelse(v0302 == 2, 'Varón', 'Mujer'),
         v4745 = ifelse(v4745 == 1, 'Sin Instrución / Nunca Asistió', v4745),
         v4745 = ifelse(v4745 == 2, 'Primaria Incompleta', v4745),
         v4745 = ifelse(v4745 == 3, 'Primaria Completa', v4745),
         v4745 = ifelse(v4745 == 4, 'Secundario Incompleto', v4745),
         v4745 = ifelse(v4745 == 5, 'Secundario Completo', v4745),
         v4745 = ifelse(v4745 == 6, 'Terciario / Universitario Incompleto', v4745),
         v4745 = ifelse(v4745 == 7, 'Terciario / Universitario Completo', v4745),
         v4745 = ifelse(v4745 == 8, 'No Determinado', v4745),
         v4706 = ifelse(v4706 == 10, 'Patrón o empleador', v4706),
         v4706 = ifelse(v4706 %in% 1:7, 'Obrero o empleado', v4706),
         v4706 = ifelse(v4706 == 9, 'Cuenta propria', v4706),
         v4706 = ifelse(v4706 == 13, 'Trabajador familiar sin remuneracion', v4706),
         v4706 = ifelse(v4706 %in% 11:12, 'Trabajador para consumo proprio', v4706),
         v0401 = ifelse(v0401 == 1, 'Jefe de hogar', v0401),
         v0401 = ifelse(v0401 == 2, 'Cónyuge', v0401),
         v0401 = ifelse(v0401 == 3, 'Hijo', v0401),
         v0401 = ifelse(v0401 == 4, 'Otro familiar', v0401),
         v0401 = ifelse(v0401 == 5, 'Agregado', v0401)
  ) %>% 
  setnames(., c('ano', 'v0302', 'v4745', 'V1', 'v0401', 'v4706'),
           c('AÑO', 'SEXO', 'NIVEL EDUCATIVO', 'INGRESO',
             'POSICIÓN HOGAR', 'CATEGORÍA OCUPACIONAL')) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)

# SETOR DE ATIVIDADE ------------------------------------------------------


# Variavel da cnae domiciliar com apenas os dois primeiros digitos
pnad %<>% 
  mutate(cnae = str_pad(v9907, width = 5, side = 'left', pad = 0)) %>% 
  mutate(cnae = substr(cnae, 1, 3)) %>% # pegar o 990 e o 980
  mutate(cnae = ifelse(cnae %in% '998', 'XX', cnae)) %>%  # o 980 virou x
  mutate(cnae = substr(cnae, 1, 2)) # usar os dois digitos

# Ler o arquivo de correspondencia à secao da atividade e os dois primeiros dig da cnae
sec_2dig <- 
  read.csv('C:/Users/matheus.rabetti/Documents/Base de Dados/Correspondencias/cnae_dom_2dig_secao.csv',
           colClasses = rep('character', 2))

merge(pnad, sec_2dig, by.x = 'cnae', by.y = 'dois_dig', all.x = T) -> pnad
rm(sec_2dig)
setDT(pnad)

pnad[secao %in% c('A','B','C'), ramo_atividad := 1]
pnad[secao %in% c('D'), ramo_atividad := 2]
pnad[secao %in% c('F'), ramo_atividad := 3]
pnad[secao %in% c('G'), ramo_atividad := 4]
pnad[secao %in% c('H'), ramo_atividad := 5]
pnad[secao %in% c('I'), ramo_atividad := 6]
pnad[secao %in% c('J','K'), ramo_atividad := 7]
pnad[secao %in% c('M'), ramo_atividad := 8]
pnad[secao %in% c('N'), ramo_atividad := 9]
pnad[secao %in% c('P'), ramo_atividad := 10]
pnad[secao %in% c('O'), ramo_atividad := 11]
pnad[secao %in% c('Q','E','L'), ramo_atividad := 12]

pnad[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(ano, ramo_atividad)] %>% 
  arrange(ano, ramo_atividad) %>% 
  group_by(ano) %>% 
  mutate(porcent = V1/sum(V1)) %>%
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv', row.names = F)







