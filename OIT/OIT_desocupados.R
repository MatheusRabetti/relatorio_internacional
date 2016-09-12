# Autor Matheus Rabetti

# Relatorio da OIT
# Preenchimento do questionario da OIT com base nos dados da PNAD de 2014

# Data att: 10/08/2016
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
                       'v9906', #cbo dom
                       'v6007', 'v6070', 'v0610', 'v0611', 'v0606', 'v0608',  # nao frequentavam escola
                       'v6003', 'v6030', 'v0605', # frequentavam escola
                       'v9106', 'v9067', #já trabalhou na vida?
                       'v0602', 'v9001' # nenem
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


# IDADE -------------------------------------------------------------------

# Populacao por idade
pnad14[v8005 >= 10 & v4805 %in% 2, sum(v4729), .(v0302, idadeF5)] %>% 
  arrange(v0302, idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')



# INCAPACIDADE ------------------------------------------------------------

dir_pnadc <- "C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD Continua/Trimestral/"
dir_document <- "C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD Continua/Documentacao/"
files_name <- list.files(dir_pnadc, pattern = "rda")

# Posicao no vetor segundo o ano
grep("2015", files_name)

# Ler e arquivar nome da variavel
for(i in 14:15){
  assign(x = paste0("pnadc", gsub(".*[0-9]{2}([0-9]{2}).*[0-9]([0-9]).*", "\\1_\\2", 
                                  files_name[i])),
         value = get(load(paste0(dir_pnadc,files_name[i]))))
}
rm(x)

pnadc15_3 %<>% 
  select(vd4002,v2005,v1028,v2009,v4074# ocupado, domestico etc, peso, idade, incapaz
         )
pnadc15_2 %<>% 
  select(vd4002,v2005,v1028,v2009,v4074# ocupado, domestico etc, peso, idade, incapaz
         )

setDT(pnadc15_3)
pnadc15_3[v2005 %in% 1:16] -> pnadc15_3

pnadc15_3[v2009 >= 15 & vd4002 %in% 2, sum(v1028)]

pnadc15_3[v4074 %in% 7, sum(vd4002, na.rm = T)]


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
  gather(isced_cod, isced, -(v0102:idadeF5)) %>% 
  filter(v8005 >= 10 & isced == 1) -> educacao
# Nunca frequentaram a escola
pnad14 %>% 
  filter(v8005 >= 10 & v0606 == 4) -> nunca_frequentou

setDT(educacao)
setDT(nunca_frequentou)


# Por idade e por sexo
educacao[v4805 %in% 2, sum(v4729), .(isced_cod, v0302, idadeF10)] %>% 
  arrange(v0302, idadeF10, isced_cod) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# Populacao por idade
nunca_frequentou[v4805 %in% 2, sum(v4729), .(v0302, idadeF10)] %>% 
  arrange(v0302, idadeF10) 

rm(educacao)
rm(nunca_frequentou)

# RURAL / URBANA ----------------------------------------------------------
pnad14[, urbano := as.numeric(v4728 %in% 1:3)]

# Total
pnad14[v8005 >= 15 & v4805 %in% 2, sum(v4729), .(urbano, idadeF10, v0302)] %>% 
  arrange(v0302, idadeF10, urbano) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# SETOR ATIVIDADE ---------------------------------------------------------

# Ocupado que tornou-se desocupado - mobilidade
# Usar a pnad continua

# CIUU4

# SETOR OCUPACIONAL -------------------------------------------------------

# Ocupado que tornou-se desocupado - mobilidade
# Usar a pnad continua


# PRIMEIRO EMPREGO --------------------------------------------------------

# Desempregado procurando primeiro emprego
pnad14[v8005 >= 10 & v4805 %in% 2 & v9106 %in% 4, sum(v4729), v0302]

# Desempregado que já trabalhou na vida
pnad14[v8005 >= 10 & v4805 %in% 2 & 
         (v9106 %in% 2 | v9067 %in% 1), sum(v4729), v0302]


# TAXA DE DESEMPREGO ------------------------------------------------------

pnad14[v4805 %in% 1, ocupado := 1]
pnad14[v4805 %in% 2, desocupado := 1]

pnad14[v8005 >= 10] %>% 
  group_by(v0302, idadeF5) %>% 
  summarise(ocupado = sum(ocupado*v4729, na.rm = T),
            desocupado = sum(desocupado*v4729, na.rm = T)) %>% 
  mutate(taxa_desemprego = round(desocupado/(ocupado+desocupado)*100, 2)) %>% 
  arrange(v0302, idadeF5) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# NENEM -------------------------------------------------------------------

# Não trabalha e não estuda
pnad14[v8005 >= 15 & v8005 <= 24 & v0602 %in% 4 & v9001 %in% 3, 
       sum(v4729), v0302]


# NAO PROCURA EMPREGO -----------------------------------------------------

dir_pnadc <- "C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD Continua/Trimestral/"
dir_document <- "C:/Users/matheus.rabetti/Documents/Base de Dados/PNAD Continua/Documentacao/"
files_name <- list.files(dir_pnadc, pattern = "rda")

# Posicao no vetor segundo o ano
grep("2015", files_name)

# Ler e arquivar nome da variavel
# Selecao temporal 2014_4 e 2015_3
for(i in 15){
  assign(x = paste0("pnadc", gsub(".*[0-9]{2}([0-9]{2}).*[0-9]([0-9]).*", "\\1_\\2", 
                                  files_name[i])),
         value = get(load(paste0(dir_pnadc,files_name[i]))))
}
rm(x)
# PEso = v1027

pnadc15_3 %<>% select(vd4002, v1027, #  desocupada, peso
                     v4073, v4074, # quer trabalhar  , motivo nao procurar
                     v2009, v2007, v2005) # idade, sexo, pensionista etc

setDT(pnadc15_3)

# Faixa idade 10 anos
# Dividindo a populacao em intervalo de 10 anos
pnadc15_3[, idadeF10 := cut(pnadc15_3$v2009, 
                         breaks = c(-1, seq(14, 64, by = 10), 150 ))]

pnadc15_3[v2005 %in% 1:16] -> pnadc15_3

# Não trabalhava e não procurou emprego
pnadc15_3[, procura := 0]
pnadc15_3[v4073 %in% 1 & v4074 %in% 3:8, procura := 1]


pnadc15_3[v4073 %in% 1 & procura == 1, sum(v1027), .(idadeF10, v2007)] %>% 
  mutate(V1 = round(V1, digits = 0)) %>% 
  arrange(v2007, idadeF10) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')
  


