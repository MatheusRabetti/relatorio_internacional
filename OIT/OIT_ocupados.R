# Autor Matheus Rabetti

# Relatorio da OIT
# Preenchimento do questionario da OIT com base nos dados da PNAD de 2014

# Data att: 20/07/2016
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
                       'v6007', 'v6070', 'v0610', 'v0611', 'v0606', # nao frequentavam escola
                       'v6003', 'v6030', 'v0605', # frequentavam escola
                       'v4805', # ocupado
                       'v9058', 'v9101', 'v9105',  # tempo habitualmente trabalhando +3
                       'v4706', 'v0711', # posicao/tipo ocupacao // outra agreg
                       'v9032', # publico / privado
                       'v9906','v9907' # cbp_dom, setor atividade - cnae dom
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

# POPULACAO ---------------------------------------------------------------


# PEA total
pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729)]
# PEA por sexo
pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729), v0302]

# PEA por idade
pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729), idadeF5] %>% 
  arrange(idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# Por idade e por sexo
pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# EDUCACAO ----------------------------------------------------------------


pnad14[, isced0 := as.numeric((v6007 %in% 11:13) | # ca, creche ou jardim de infancia
                                (v6007 %in% 6 & v0610 <= 3) | # começo do supletivo - EJA
                                (v6007 %in% 4 & v6070 %in% 1 & v0610 %in% 1:3) | # começo do fundamental
                                (v6007 %in% 4 & v6070 %in% 3 & v0610 %in% 1:4) | # começo do fundamental
                                (v6007 %in% 1 & v0611 %in% 3) | # primario incompleto
                                (is.na(v0611) & v6007 %in% 1) | # primario incompleto
                                (v6007 %in% 10) | # alfabetizacao de adultos
                                
                                (v6003 %in% 6:9) | 
                                (v6003 %in% 1 & v6030 %in% 1 & v0605 %in% 1:4) | # cursando da quarta pra baixo
                                (v6003 %in% 1 & v6030 %in% 3 & v0605 %in% 1:5) | # cursando do quinto pra baixo
                                (v6003 %in% 3 & v0605 %in% 1:4) # cursando comeco do supletivo
)]                                

pnad14[, isced1 := as.numeric((v6007 %in% 1 & v0611 %in% 1) | # primario completo
                                (v6007 %in% 2 & v0611 %in% 3) | # ginasio incompleto  
                                (v6007 %in% 4 & v6070 %in% 1 & v0610 %in% 4:7) | # fundamental quarta a setima serie
                                (v6007 %in% 4 & v6070 %in% 3 & v0610 %in% 6:8) | # fundamental quinto ao oitavo ano 
                                (v6007 %in% 6 & v0610 %in% 4:7) | # supletivo fundamental quarta a setima
                                (is.na(v0611) & v6007 %in% 2) | # ginasio incompleto
                                (is.na(v0611) & v6007 %in% 4) | # fundamental incompleto
                                
                                (v6003 %in% 1 & v6030 %in% 1 & v0605 %in% 5:8) | # cursando da quinta pra cima
                                (v6003 %in% 1 & v6030 %in% 3 & v0605 %in% c(0,6:8)) | # cursando do sexto pra cima
                                (v6003 %in% 3 & v0605 %in% 5:8) # cursando o segundo ciclo do supletivo fundamental
)] 


pnad14[, isced2 := as.numeric((v6007 %in% 2 & v0611 %in% 1) | # ginasio completo  
                                (v6007 %in% 3 & v0611 %in% 3) | # colegio incompleto
                                (v6007 %in% 4 & v0611 %in% 1) | # fundamental completo
                                (v6007 %in% 5 & v0611 %in% 3) | # medio incompleto
                                (v6007 %in% 6 & v0611 %in% 1) | # supletivo fundamental completo
                                (v6007 %in% 7 & v0611 %in% 3) | # supletivo medio incompleto
                                (is.na(v0611) & v6007 %in% 5) | # medio incompleto
                                (is.na(v0611) & v6007 %in% 3) | # colegio incompleto
                                (is.na(v0611) & v6007 %in% 7) | # supl. medio incompleto
                                
                                (v6003 %in% 2) | # cursando ensino medio
                                (v6003 %in% 4) # cursando supletivo ensino medio
)]

pnad14[, isced3 := as.numeric((v6007 %in% 3 & v0611 %in% 1) | # colegio completo  
                                (v6007 %in% 5 & v0611 %in% 1) | # medio completo
                                (v6007 %in% 8 & v0611 %in% 3) | # graduacao incompleto
                                (v6007 %in% 7 & v0611 %in% 1) | # supletivo medio completo
                                (is.na(v0611) & v6007 %in% 8) | # fundamental incompleto
                                
                                (v6003 %in% 5)  # cursando faculdade
                              
)]

pnad14[, isced4 := as.numeric((v6003 %in% 10))] # cursando pre vestibular

pnad14[, isced6 := as.numeric((v6007 %in% 8 & v0611 %in% 1) # faculdade completa  
                              
)]


pnad14[, isced7 := as.numeric((v6007 %in% 9 & v0611 %in% 1) | # mestrado/dr completa  
                                (v6007 %in% 9 & v0611 %in% 3) | # mest/dr incompleto
                                (is.na(v0611) & v6007 %in% 9) | # mestr/dr incompleto
                                
                                (v6003 %in% 11)
)]


# Agrupar as colunas de ISCED em apenas uma
pnad14 %>% 
  gather(isced_cod, isced, -(v0102:idadeF10)) %>% 
  filter(v8005 >= 10 & isced == 1) -> educacao
# Nunca frequentaram a escola
pnad14 %>% 
  filter(v8005 >= 10 & v0606 == 4) -> nunca_frequentou


########## CALCULANDO
setDT(educacao, nunca_frequentou)

# Nunca frequentou >= 10
nunca_frequentou[v8005 >= 15 & v4805 %in% 1, sum(v4729)]

# PEA por ISCED
educacao[v8005 >= 15 & v4805 %in% 1, sum(v4729), isced_cod] %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# PEA por idade
nunca_frequentou[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(idadeF10)] %>%
  arrange(idadeF10)

# PEA por idade
educacao[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(isced_cod, idadeF10)] %>%
  arrange(idadeF10, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# Por sexo
educacao[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(isced_cod, v0302)] %>% 
  arrange(v0302, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

nunca_frequentou[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(v0302)]

# Por idade e por sexo
educacao[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(idadeF10, isced_cod, v0302)] %>% 
  arrange(v0302, idadeF10, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

nunca_frequentou[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(idadeF10, v0302)] %>% 
  arrange(v0302, idadeF10) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# RURAL / URBANA ----------------------------------------------------------
pnad14[, urbano := as.numeric(v4728 %in% 1:3)]

# Total
pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(urbano, v0302)] %>% 
  arrange(v0302, urbano)


pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(urbano)] %>% 
  arrange(urbano)

pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(urbano, v0302, idadeF10)] %>% 
  arrange(v0302, idadeF10, urbano) %>%
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')



# Trabalho Parcial  -------------------------------------------------------

# Trabalho parcial <= 25h semanais


pnad14[is.na(v9058), v9058 := 0]
pnad14[is.na(v9101), v9101 := 0]
pnad14[is.na(v9105), v9105 := 0]

pnad14 %>% 
  mutate(horas_trabalho = v9058 + v9101 + v9105) -> pnad14


pnad14[, horas_trabalho := ifelse(v4805 %in% 1, horas_trabalho, NA)]

pnad14[, trab_parcial := as.numeric(horas_trabalho <= 25)]

pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(idadeF10, v0302, trab_parcial)] %>% 
  arrange(v0302, idadeF10, desc(trab_parcial)) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# TIPO DE EMPREGO ---------------------------------------------------------

# nao remunerado seria o trabalhador familiar auxiliar
# as variaveis v9008 e v9029 classificam os nao remunerados em 
# trabalhadores familiares e outros nao remunerados

# assalariado
pnad14[v4706 %in% 1:7, tipo_emprego := 1]

# empregador
pnad14[v4706 %in% 10, tipo_emprego := 2]

#conta propria 
pnad14[v4706 %in% 9, tipo_emprego := 3]

# nao remunerados
pnad14[v4706 %in% 13, tipo_emprego := 5]

# Consumo proprio
pnad14[v4706 %in% 11:12, tipo_emprego := 6]

pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(v0302, tipo_emprego)] %>% 
  arrange(v0302, tipo_emprego) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# PUBLICO / PRIVADO -------------------------------------------------------

# Existem casos de ocupado, mas NA no setor em que trabalha pela respondencia
pnad14[is.na(v9032) & v4805 %in% 1, table(v4706)]

# Mesmo trabalhando aparentemente no setor privado - alguns são do setor público
pnad14[v4805 %in% 1 & v4706 %in% c(1,4:13), table(v9032)]

# Publico
pnad14[v8005 >= 15 & v9032 %in% 4, sum(v4729), v0302] %>% 
  arrange(v0302)

# Privado
pnad14[v8005 >= 15 & 
         ((is.na(v9032) & v4805 %in% 1 & v4706 %in% c(1,4:13)) |
         v9032 %in% 2), sum(v4729), v0302] %>% 
  arrange(v0302)


# SETOR ATIVIDADE ---------------------------------------------------------

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

pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(v0302, secao)] %>% 
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

pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(v0302, sub_setor)] %>% 
  arrange(v0302, sub_setor) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# GRANDE GRUPO OCUPACAO ---------------------------------------------------

# 9 é reparação e mantimento e não ocupações elementares
pnad14 %<>% 
  mutate(cbo = str_pad(v9906, width = 4, side = 'left', pad = 0)) %>% 
  mutate(cbo_gg = substr(cbo, 1, 1)) %>% 
  mutate(cbo_2dig = substr(cbo, 1, 2)) 
  
pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729), .(cbo_gg)] %>% 
  arrange(cbo_gg) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# GRUPO OCUPACAO ----------------------------------------------------------

pnad14[cbo_2dig %in% '1' | cbo_3dig %in% '121', cbo_grupo := 1]
pnad14[cbo_3dig %in% c('123','132'), cbo_grupo := 2]
pnad14[cbo_3dig %in% c('122','131'), cbo_grupo := 3]
pnad14[cbo_2dig %in% c('23', '33'), cbo_grupo := 4]
pnad14[cbo_3dig %in% '212', cbo_grupo := 5]
pnad14[cbo_3dig %in% c('322','323', '324'), cbo_grupo := 6]
pnad14[cbo_3dig %in% c('351', '424') | cbo %in% c('3711', '2523'), cbo_grupo := 7]
pnad14[cbo_2dig %in% '41' | cbo %in% c('3532', '3423', '3426', '5101'),
       cbo_grupo := 8]
pnad14[cbo_2dig %in% '42' & (cbo_3dig != '424'), cbo_grupo := 8]

# a terminar....

# ATIVIDADE ECON E OCUP ---------------------------------------------------

# 9 é reparação e mantimento e não ocupações elementares

# Rodar setor atividade e grande grupo ocupaca
pnad14[v8005 >= 15 & v4805 %in% 1, sum(v4729), .(secao, cbo_gg)] %>% 
  arrange(secao, cbo_gg) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# TAXA DE PARTICIPACAO ----------------------------------------------------

pnad14[v8005 >= 10 & v4805 %in% 1, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) -> ocup

pnad14[v8005 >= 10, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) -> pia

setnames(ocup, old = 'V1', 'ocup') 
setnames(pia, old = 'V1', 'pia') 

# Trazer a coluna pia
cbind(ocup, select(pia, pia)) -> ocup

ocup %<>% # %<>% singnifca usar e sobrepor a base
  mutate(taxa_partcip = round(ocup/pia*100, 2)) 

write.csv2(ocup, 'C:/Users/matheus.rabetti/Desktop/temp.csv')


