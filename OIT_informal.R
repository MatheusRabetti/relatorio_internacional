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

`%ni%` <- Negate(`%in%`)
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
                       'v4808', # agricola/n
                       'v4809', # grupamento da atividade
                       'v9008', 'v9029', # auxiliar na familia
                       'v9048', # nº empregados
                       'v90531', 'v90532', 'v90533' # cnpj, contabildiade
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


# AGRICOLA ----------------------------------------------------------------

# Ocupacao informal
pnad14[, informal := as.numeric(v4706 %in% c(4,7,11:13) | 
                                  (v4706 %in% 9 & v4711 %in% 2) |
                                  (v4706 %in% 9 & v90531 %in% 3) |
                                  (v4706 %in% 10 & v90531 %in% 3) |
                                  (v4706 %in% 10 & v90532 %in% 3) |
                                  (v4706 %in% 10 & v90533 %in% 3) |
                                  (v4706 %in% 1 & v90531 %in% 3) |
                                  (v4706 %in% 1 & v90532 %in% 3) |
                                  (v4706 %in% 1 & v90533 %in% 3))]

pnad14[v8005 >= 10 & informal %in% 1, sum(v4729), .(v0302, v4808)] %>% 
  arrange(v4808, v0302) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


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

# AGREGAR SEÇÕES
pnad14[secao %in% c('A', 'B'), atividade := 1]
pnad14[secao %in% LETTERS[3:6], atividade := 2]
pnad14[secao %in% LETTERS[7:11], atividade := 3]
pnad14[secao %in% LETTERS[12:17], atividade := 4]
pnad14[secao %in% 'X', atividade := 5]


pnad14[v8005 >= 10 & informal %in% 1 & secao %in% 'D', sum(v4729)]
pnad14[v8005 >= 10 & informal %in% 1 & secao %in% 'F', sum(v4729)]
pnad14[v8005 >= 10 & informal %in% 1 & secao %in% 'G', sum(v4729)]
pnad14[v8005 >= 10 & informal %in% 1 & secao %in% 'I', sum(v4729)]


pnad14[v8005 >= 10 & informal %in% 1, sum(v4729), .(atividade)] %>% 
  arrange(atividade) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')


# POSICAO OCUPACAO --------------------------------------------------------

# Ocupacao informal
pnad14[, informal := as.numeric(v4706 %in% c(4,7,11:13) | 
                                  (v4706 %in% 9 & v4711 %in% 2))]

# Empregadores e conta própria
pnad14[(v4706 %in% 9 & v4711 %in% 2) |
         (v4706 %in% 9 & v90531 %in% 3) |
         (v4706 %in% 10 & v90531 %in% 3) |
         (v4706 %in% 10 & v90532 %in% 3) |
         (v4706 %in% 10 & v90533 %in% 3) & v8005 >= 10, situ := 1]
# Assalariado informal - sem carteira e com carteira em setor informal
pnad14[((v4706 %in% c(4,7))|
         (v4706 %in% 1 & v90531 %in% 3) |
         (v4706 %in% 1 & v90532 %in% 3) |
         (v4706 %in% 1 & v90533 %in% 3)) & v8005 >= 10, situ := 2]
pnad14[(v9008 %in% 11 | v9029 %in% 5) & informal %in% 1 & v8005 >= 10, situ := 3]
pnad14[v4706 %in% c(11:12) & v8005 >= 10, situ := 4]
pnad14[(v9008 %in% 12 | v9029 %in% 6) & informal %in% 1 & v8005 >= 10, situ := 5]

pnad14[v8005 >= 10 & informal %in% 1, sum(v4729), .(situ, v0302, v4808)] %>% 
  arrange(v4808, v0302, situ) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')



# CARACTERISTICA TRABALHO -------------------------------------------------

# ASSALARIADO INFORMAL
# Com carteira no setor informal
pnad14[((v4706 %in% 1 & v90531 %in% 3) |
          (v4706 %in% 1 & v90532 %in% 3) |
          (v4706 %in% 1 & v90533 %in% 3)) & v8005 >= 10, caract := 1]
# Sem carteira
pnad14[v4706 %in% 4 & v8005 >= 10, caract := 2]
# domestico informal
pnad14[v4706 %in% 7 & v8005 >= 10, caract := 3]


# ASSALARIADO FORMAL
# Com Carteira no setor formal
pnad14[((v4706 %in% 1 & v90531 %ni% 3) |
          (v4706 %in% 1 & v90532 %ni% 3) |
          (v4706 %in% 1 & v90533 %ni% 3)) & v8005 >= 10, caract := 4]
# domestico formal
pnad14[v4706 %in% 6 & v8005 >= 10, caract := 5]


pnad14[v8005 >= 10, sum(v4729), .(caract, v4808)] %>% 
  arrange(v4808, caract) %>% 
  write.csv2(., 'C:/Users/matheus.rabetti/Desktop/temp.csv')

# TAXA INFORMALIDADE ------------------------------------------------------

pnad14[v4805 %in% 1, ocupado := 1]

# Taxa de informalidade por sexo e emprego agricola ou n
pnad14[v8005 >= 10] %>% 
  group_by(v0302, v4808) %>% 
  summarise(ocupado = sum(ocupado*v4729),
            informal = sum(informal*v4729)) %>% 
  mutate(taxa_inform = round(informal/ocupado*100, 2)) %>% 
  arrange(v4808, v0302)

# Taxa informalidade por sexo
pnad14[v8005 >= 10] %>% 
  group_by(v0302) %>% 
  summarise(ocupado = sum(ocupado*v4729, na.rm = T),
            informal = sum(informal*v4729)) %>% 
  mutate(taxa_inform = round(informal/ocupado*100, 2)) %>% 
  arrange(v0302)

# Informalidade para total agricola ou n
pnad14[v8005 >= 10] %>% 
  group_by(v4808) %>% 
  summarise(ocupado = sum(ocupado*v4729, na.rm = T),
            informal = sum(informal*v4729)) %>% 
  mutate(taxa_inform = round(informal/ocupado*100, 2)) %>% 
  arrange(v4808)

# Taxa informalidade total
pnad14[v8005 >= 10] %>% 
  summarise(ocupado = sum(ocupado*v4729, na.rm = T),
            informal = sum(informal*v4729)) %>% 
  mutate(taxa_inform = round(informal/ocupado*100, 2)) %>% 
  print(.)






