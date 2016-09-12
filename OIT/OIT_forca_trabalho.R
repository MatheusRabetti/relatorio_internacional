# Autor Matheus Rabetti

# Relatorio da OIT
# Preenchimento do questionario da OIT com base nos dados da PNAD de 2014

# Data att: 08/07/2016
# Ultima att: 


# BIBLIOTECA --------------------------------------------------------------

library(bit64) # leitura de dados numericos mais rapida
library(descr) # transformar microdados em uma base csv
library(survey) # amostragem complexa
library(data.table) # manipulacao de dados
library(dplyr) # manipulacao de dados
library(magrittr) # pacote adicional ao dplyr
library(tidyr) # agrupar colunas de variaveis


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
                       'v4704')) # PEA

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
pnad14[v8005 >= 10 & v4704 %in% 1, sum(v4729)]
# PEA por sexo
pnad14[v8005 >= 10 & v4704 %in% 1, sum(v4729), v0302]

# PEA por idade
pnad14[v8005 >= 10 & v4704 %in% 1, sum(v4729), idadeF5] %>% 
  arrange(idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# Por idade e por sexo
pnad14[v8005 >= 10 & v4704 %in% 1, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# EDUCACAO ----------------------------------------------------------------

# Divindo a populacao em intervalo de 10 anos
pnad14[, idadeF10 := cut(pnad14$v8005, 
                         breaks = c(-1, seq(14, 64, by = 10), 150 ))]

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
nunca_frequentou[v8005 >= 15 & v4704 %in% 1, sum(v4729)]

# PEA por ISCED
educacao[v8005 >= 15 & v4704 %in% 1, sum(v4729), isced_cod] %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# PEA por idade
nunca_frequentou[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(idadeF10)] %>%
  arrange(idadeF10)

# PEA por idade
educacao[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(isced_cod, idadeF10)] %>%
  arrange(idadeF10, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# Por sexo
educacao[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(isced_cod, v0302)] %>% 
  arrange(v0302, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

nunca_frequentou[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(v0302)]

# Por idade e por sexo
educacao[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(idadeF10, isced_cod, v0302)] %>% 
  arrange(v0302, idadeF10, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

nunca_frequentou[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(idadeF10, v0302)] %>% 
  arrange(v0302, idadeF10) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# RURAL / URBANA ----------------------------------------------------------
pnad14[, urbano := as.numeric(v4728 %in% 1:3)]

# Total
pnad14[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(urbano, v0302)] %>% 
  arrange(v0302, urbano)


pnad14[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(urbano)] %>% 
  arrange(urbano)

pnad14[v8005 >= 15 & v4704 %in% 1, sum(v4729), .(urbano, v0302, idadeF10)] %>% 
  arrange(v0302, idadeF10, urbano) %>%
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')



# TAXA DE PARTICIPACAO ----------------------------------------------------

pnad14[v8005 >= 10 & v4704 %in% 1, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) -> pea

pnad14[v8005 >= 10, sum(v4729), .(idadeF5, v0302)] %>% 
  arrange(v0302, idadeF5) -> pia

setnames(pea, old = 'V1', 'pea') 
setnames(pia, old = 'V1', 'pia') 

# Trazer a coluna pia
cbind(pea, select(pia, pia)) -> pea

pea %<>% # %<>% singnifca usar e sobrepor a base
  mutate(taxa_partcip = round(pea/pia*100, 2)) 

write.csv2(pea, 'C:/Users/matheus.rabetti/Desktop/temp.csv')
