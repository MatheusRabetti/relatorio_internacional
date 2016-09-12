# Autor Matheus Rabetti

# Relatorio da OIT
# Preenchimento do questionario da OIT com base nos dados da PNAD de 2014

# Data att: 28/07/2016
# Ultima att: 


# BIBLIOTECA --------------------------------------------------------------

library(bit64) # leitura de dados numericos mais rapida
library(descr) # transformar microdados em uma base csv
library(survey) # amostragem complexa
library(data.table) # manipulacao de dados
library(dplyr) # manipulacao de dados
library(magrittr) # pacote adicional ao dplyr
library(tidyr) # agrupar colunas de variaveis
library(stringr) # manipulacao de textos

rm(list = ls())
gc()
# LEITURA -----------------------------------------------------------------


# Diretorio dos dados e do dicionario
pnad14_path <- 'C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD/2014'


pes <- fread(paste0(pnad14_path, '/pes2014.csv'), 
             sep = "\t", integer64 = 'double', 
             select= c('v0102', 'v0103', 'v0301', 'v0401', # id e filtro
                       'v8005', 'v0302', 'v4729', 'v4728', # idade, sexo, peso e rural
                       'v4805', 'v4711', # ocupado  contribuinte
                       'v9058', 'v9101', 'v9105',  # tempo habitualmente trabalhando +3
                       'v4706', # posicao/tipo ocupacao // outra agreg
                       'v9907', # cnae dom
                       'v9032', # publico ou privado
                       'v4808', # agricola/n
                       'v4809', # grupamento da atividade
                       'v9008', 'v9029',# auxiliar na familia
                       'v9906' #cbo dom
             ))


dom <- fread(paste0(pnad14_path, '/dom2014.csv'), 
             sep = "\t", integer64 = 'double',
             select= c('V0102', 'V0103', # id
                       'V4617', 'V4618')) # amostragem complexa
names(dom) <- tolower(names(dom))

pnad14 <- merge(pes, dom, by = c('v0102', 'v0103'))
rm(pes, dom)

# Excluir pensionista, empregado doméstico e parentes do empregado
pnad14[ v0401 %in% 1:5 ] -> pnad14

pnad14[ , one := 1]

# Divindo a populacao em intervalo de 10 anos
pnad14[, idadeF10 := cut(pnad14$v8005, 
                         breaks = c(-1, seq(14, 64, by = 10), 150 ))]


# Dividindo de 5 em 5 a pop
pnad14[, idadeF5 := cut(pnad14$v8005, breaks = c(-1, seq(9, 64, by = 5), 150 ))]

# PUBLICO / PRIVADO -------------------------------------------------------


# Privado
pnad14[v8005 >= 15 & 
         ((is.na(v9032) & v4805 %in% 1 & v4706 %in% c(1,4:13)) |
            v9032 %in% 2), setor := 1] 

# Publico
pnad14[v8005 >= 15 & v9032 %in% 4, setor := 2]

# Assalariado
pnad14[v8005 >= 15 & v4706 %in% c(1:7), sum(v4729), .(setor,v0302)] %>% 
  arrange(v0302, setor) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# SETOR ATIVIDADE ---------------------------------------------------------

# CIUU3
# Nao classificados muda para XX - seguindo arquivo de correspondencia

# Variavel da cnae domiciliar com apenas os dois primeiros digitos
pnad14 %<>% 
  mutate(cnae = str_pad(v9907, width = 5, side = 'left', pad = 0)) %>% 
  mutate(cnae = substr(cnae, 1, 3)) %>% # pegar o 990 e o 980
  mutate(cnae = ifelse(cnae %in% '998', 'XX', cnae)) %>%  # o 980 virou x
  mutate(cnae = substr(cnae, 1, 2)) # usar os dois digitos

# Ler o arquivo de correspondencia à secao da atividade e os dois primeiros dig da cnae
sec_2dig <- 
  read.csv('C:/Users/matheus.rabetti/Documents/Base de Dados/Correspondencias/cnae_dom_2dig_secao.csv',
           colClasses = rep('character', 2))

merge(pnad14, sec_2dig, by.x = 'cnae', by.y = 'dois_dig', all.x = T) -> pnad14
rm(sec_2dig)

pnad14[v8005 >= 15 & v4706 %in% c(1:7), sum(v4729), .(v0302, secao)] %>% 
  arrange(v0302, secao) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# SUBSETOR ATIVIDADE ------------------------------------------------------

# Usar as variaveis cnae (2 digitos) ou v9907

pnad14[cnae %in% '01', sub_setor := 1]
pnad14[cnae %in% '05', sub_setor := 2]
pnad14[v9907 %in% c(15010, 15021, 15022), sub_setor := 3]
pnad14[v9907 %in% c(15050, 23400), sub_setor := 4]
pnad14[cnae %in% '17', sub_setor := 5]
pnad14[cnae %in% '18', sub_setor := 6]
pnad14[cnae %in% '24', sub_setor := 7]
pnad14[cnae %in% '36', sub_setor := 8]
pnad14[cnae %in% '41', sub_setor := 9]
pnad14[v9907 %in% 45999, sub_setor := 10]
pnad14[v9907 %in% c(53010,53020,53030, 53041,53042,53050,53061:53068),
       sub_setor := 11]
pnad14[v9907 %in% c(53070,53080,53090,53101,53111:53113),
       sub_setor := 12]
pnad14[cnae %in% '60', sub_setor := 13]
pnad14[v9907 %in% 55010, sub_setor := 14]
pnad14[v9907 %in% 55020, sub_setor := 15]
pnad14[v9907 %in% 64020, sub_setor := 16]
pnad14[cnae %in% c('65','67'), sub_setor := 17]
pnad14[cnae %in% '66', sub_setor := 18]
pnad14[v9907 %in% c(74040,74050,74060,74090), sub_setor := 19]
pnad14[v9907 %in% c(85011:85013), sub_setor := 20]

pnad14[v8005 >= 15 & v4706 %in% c(1:7), sum(v4729), .(v0302, sub_setor)] %>% 
  arrange(v0302, sub_setor) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# GRANDE GRUPO OCUPACAO ---------------------------------------------------

# 9 é reparação e mantimento e não ocupações elementares
pnad14 %<>% 
  mutate(cbo = str_pad(v9906, width = 4, side = 'left', pad = 0)) %>% 
  mutate(cbo_gg = substr(cbo, 1, 1)) %>% 
  mutate(cbo_2dig = substr(cbo, 1, 2)) 

pnad14[v8005 >= 15 & v4706 %in% c(1:7), sum(v4729), .(v0302, cbo_gg)] %>% 
  arrange(v0302, cbo_gg) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# ATIVIDADE ECON E OCUP ---------------------------------------------------

# 9 é reparação e mantimento e não ocupações elementares

# Rodar setor atividade e grande grupo ocupaca
pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(secao, cbo_gg)] %>% 
  arrange(secao, cbo_gg) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

  