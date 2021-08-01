
############ Produto 1 ###########################
##################################################
###### script desenvolvido por Mikael Lemos ######
###### versão 1.0 - 21.06.2019 ##################
##################################################

######
### Loading / installing packages
######

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

# install.packages("tidyverse")
library("tidyverse")

# install.packages("lubridate")
library("lubridate")

#install.packages("Amelia")
library("Amelia")

#install.packages('Rtools')
#library('Rtools')

#install.packages('microbenchmark')
library("microbenchmark")

#install.packages('ggplot2movies')
library("ggplot2movies")

#install.packages('profvis')
library("profvis")

#install.packages('Rcpp')
library("Rcpp")

#install.packages('compiler')
library("compiler")

#install.packages('memoise')
library("memoise")

#install.packages('DiagrammeR')
library("DiagrammeR")

#install.packages('rio')
library("rio")

#install.packages('readr')
library("readr")

#install.packages('data.table')
library("data.table")

#install.packages('feather')
library("feather")

#install.packages('WDI')
library("WDI")


######
### Loading DATA FRAME
######

##### APAC ####
#### MAC mikael
df.APAC_procedimento <- read.csv("/Users/mikaellemos/Produtos/produto1/produto1_APAC_UF_procedimento.csv")

df.APAC_CID <- read.csv("/Users/mikaellemos/Produtos/produto1/produto1_APAC_UF_CID.csv")


## Datas de procedimentos

df.APAC_procedimento$DATA_INICIO <- as.character(df.APAC_procedimento$DATA_INICIO)
df.APAC_procedimento$DATA_FIM <- as.character(df.APAC_procedimento$DATA_FIM)

df.APAC_procedimento$DATA_INICIO <- dmy(df.APAC_procedimento$DATA_INICIO)
df.APAC_procedimento$DATA_FIM <- dmy(df.APAC_procedimento$DATA_FIM)

df.APAC_procedimento$DATA_SOLIC <- as.character(df.APAC_procedimento$DATA_SOLIC)
df.APAC_procedimento$DATA_GERACAO <- as.character(df.APAC_procedimento$DATA_GERACAO)

df.APAC_procedimento$DATA_SOLIC <- dmy(df.APAC_procedimento$DATA_SOLIC)
df.APAC_procedimento$DATA_GERACAO <- dmy(df.APAC_procedimento$DATA_GERACAO)

## Separar data - dia, mês, ano

df.APAC_procedimento <- df.APAC_procedimento %>%
  separate(DATA_INICIO, sep="-", into = c("ano_inicio", "mes_inicio", "dia_inicio"))

df.APAC_procedimento <- df.APAC_procedimento %>%
  separate(DATA_FIM, sep="-", into = c("ano_fim", "mes_fim", "dia_fim"))

df.APAC_procedimento <- df.APAC_procedimento %>%
  separate(DATA_SOLIC, sep="-", into = c("ano_solic", "mes_solic", "dia_solic"))

df.APAC_procedimento <- df.APAC_procedimento %>%
  separate(DATA_GERACAO, sep="-", into = c("ano_gera", "mes_gera", "dia_gera")) 


table(df.APAC_procedimento$ano_solic, useNA="always")

table(df.APAC_procedimento$ano_gera, useNA="always")

table(df.APAC_procedimento$ano_inicio, useNA="always")


df.APAC_procedimento_2015 <- filter(df.APAC_procedimento, df.APAC_procedimento$ano_inicio == 2015)

df.APAC_procedimento_2016 <- filter(df.APAC_procedimento, df.APAC_procedimento$ano_inicio == 2016)

df.APAC_procedimento_2017 <- filter(df.APAC_procedimento, df.APAC_procedimento$ano_inicio == 2017)

df.APAC_procedimento_2018 <- filter(df.APAC_procedimento, df.APAC_procedimento$ano_inicio == 2018)

# Procedimento 0604640030

df.APAC_procedimento_60464003 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_PRINCIPAL == 60464003)

df.APAC_procedimento_sec_604640030 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_SECUNDARIO == 604640030)

# Procedimento 0604760019

df.APAC_procedimento_60476001 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_PRINCIPAL == 60476001)

df.APAC_procedimento_sec_604760019 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_SECUNDARIO == 604760019)

# Procedimento 0604760027

df.APAC_procedimento_60476002 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_PRINCIPAL == 60476002)

df.APAC_procedimento_sec_604760027 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_SECUNDARIO == 604760027)

# Procedimento 0604760035

df.APAC_procedimento_60476003 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_PRINCIPAL == 60476003)

df.APAC_procedimento_sec_604760035 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_SECUNDARIO == 604760035)

# Procedimento 0604760043

df.APAC_procedimento_60476004 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_PRINCIPAL == 60476004)

df.APAC_procedimento_sec_604760043 <- filter(df.APAC_procedimento, df.APAC_procedimento$CO_PROCEDIMENTO_SECUNDARIO == 604760043)

# isolando NAs e não NAs

ano_gera <- as.data.frame(df.APAC_procedimento$ano_gera)
setnames(ano_gera, "df.APAC_procedimento$ano_gera", "ano_geração")
missmap(ano_gera, y.at = c(1) , y.labels = c(''), col = c('yellow' , 'black'), main = "Mapa de dados faltantes")

ano_inicio <- as.data.frame(df.APAC_procedimento$ano_inicio)
setnames(ano_inicio, "df.APAC_procedimento$ano_inicio", "ano_início")
missmap(ano_inicio, y.at = c(1) , y.labels = c(''), col = c('yellow' , 'black'), main = "Mapa de dados faltantes")

ano_solic <- as.data.frame(df.APAC_procedimento$ano_solic)
setnames(ano_solic, "df.APAC_procedimento$ano_solic", "ano_solicitação")
missmap(ano_solic, y.at = c(1) , y.labels = c(''),col = c('yellow' , 'black'), main = "Mapa de dados faltantes")

ano_missmap <- as.data.frame(c(ano_inicio,ano_solic))

colnames(ano_missmap)

missmap(ano_missmap, y.at = c(1) , y.labels = c(''), col = c('yellow' , 'black'), main = "Mapa de dados faltantes")

co_municipio_hospital_miss <- as.data.frame(df.APAC_procedimento$CO_MUNICIPIO_HOSPITAL)
setnames(co_municipio_hospital_miss, "df.APAC_procedimento$CO_MUNICIPIO_HOSPITAL", "co_municipio_hospital")
missmap(co_municipio_hospital_miss, y.at = c(1) , y.labels = c(''), col = c('yellow' , 'black'), main = "Mapa de dados faltantes")

# ano de início - separação por ano para todos os tratamentos

#2015
df.APAC_procedimento_2015_mun_br <- select(df.APAC_procedimento_2015, CO_MUNICIPIO_HOSPITAL)
df.APAC_procedimento_2015_mun <- table(df.APAC_procedimento_2015_mun_br)
df.APAC_procedimento_2015_mun <- as.data.frame(df.APAC_procedimento_2015_mun)
write.csv(df.APAC_procedimento_2015_mun_br, file = '/Users/mikaellemos/Produtos/produto1/df.APAC_procedimento_2015_mun.csv', row.names=FALSE)

#2016
df.APAC_procedimento_2016_mun_br <- select(df.APAC_procedimento_2016, CO_MUNICIPIO_HOSPITAL)
df.APAC_procedimento_2016_mun <- table(df.APAC_procedimento_2016_mun_br)
df.APAC_procedimento_2016_mun <- as.data.frame(df.APAC_procedimento_2016_mun)
write.csv(df.APAC_procedimento_2016_mun_br, file = '/Users/mikaellemos/Produtos/produto1/df.APAC_procedimento_2016_mun.csv', row.names=FALSE)

#2017
df.APAC_procedimento_2017_mun_br <- select(df.APAC_procedimento_2017, CO_MUNICIPIO_HOSPITAL)
df.APAC_procedimento_2017_mun <- table(df.APAC_procedimento_2017_mun_br)
df.APAC_procedimento_2017_mun <- as.data.frame(df.APAC_procedimento_2017_mun)
write.csv(df.APAC_procedimento_2017_mun_br, file = '/Users/mikaellemos/Produtos/produto1/df.APAC_procedimento_2017_mun.csv', row.names=FALSE)

#2018
df.APAC_procedimento_2018_mun_br <- select(df.APAC_procedimento_2018, CO_MUNICIPIO_HOSPITAL)
df.APAC_procedimento_2018_mun <- table(df.APAC_procedimento_2018_mun_br)
df.APAC_procedimento_2018_mun <- as.data.frame(df.APAC_procedimento_2018_mun)
write.csv(df.APAC_procedimento_2018_mun_br, file = '/Users/mikaellemos/Produtos/produto1/df.APAC_procedimento_2018_mun.csv', row.names=FALSE)

# Extrair código IBGE completo
cod_mun_IBGE <- df.APAC_CID <- readxl::read_xls("/Users/mikaellemos/Downloads/DTB_2018/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

 ############# tabela completa ano 2015 - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2015 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
cod_mun_IBGE_6d <- cbind(cod_mun_completo, cod_mun_IBGE)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
df1 <- as.data.frame(cod_mun_IBGE_6d)
 
# tabela contendo todos os municipios (IBGE 6 digitos) que receberam trtatamento de Hep C em 2015
df_15 <- df.APAC_procedimento_2015_mun_br 

# tabela completa contendo todos os múnicipios que receberam tratamento de Hep C em 2015 com cod IBGE 6 e 7 dígitos
df.hepC.2015.mun  <- merge(df1,df_15, by.x = "cod_mun_completo", by.y = "CO_MUNICIPIO_HOSPITAL" , all.y = TRUE)

# Contagem de tratamentos por UF

cont_2015_hepC_UF <- table(df.hepC.2015.mun$UF)
cont_2015_hepC_UF <- as.data.frame(cont_2015_hepC_UF)

# merge contagem com a tabela UF 2015
contagem_hepC_2015_UF  <- merge(df.hepC.2015.mun,cont_2015_hepC_UF, by.x = "UF", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2015 <- distinct(contagem_hepC_2015_UF, UF, .keep_all = TRUE)

# salvar tabela de med para Hep C do ano de 2015
write.csv(df.hepC.2015.mun, file = '/Users/mikaellemos/Produtos/produto1/df.hepC.2015.mun.csv', row.names=FALSE)

# salvar tabela de med para Hep C por UF do ano de 2015
write.csv(UF_hepC_2015, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2015.csv', row.names=FALSE)

### Municipios 2015 ####

# Contagem de tratamentos por MUN

cont_2015_hepC_MUN <- table(df.hepC.2015.mun$`Código Município Completo`)
cont_2015_hepC_MUN <- as.data.frame(cont_2015_hepC_MUN)

# merge contagem com a tabela MUN 2015
contagem_hepC_2015_MUN  <- merge(df.hepC.2015.mun,cont_2015_hepC_MUN, by.x = "Código Município Completo", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2015_MUN <- distinct(contagem_hepC_2015_MUN, Nome_Município, .keep_all = TRUE)

# salvar tabela de med para Hep C por MUN do ano de 2015
write.csv(UF_hepC_2015_MUN, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2015_MUN.csv', row.names=FALSE)


############ tabela completa ano 2016 - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2015 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
cod_mun_IBGE_6d <- cbind(cod_mun_completo, cod_mun_IBGE)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
df1 <- as.data.frame(cod_mun_IBGE_6d)

# tabela contendo todos os municipios (IBGE 6 digitos) que receberam trtatamento de Hep C em 2016
df_16 <- df.APAC_procedimento_2016_mun_br 

# tabela completa contendo todos os múnicipios que receberam tratamento de Hep C em 2016 com cod IBGE 6 e 7 dígitos
df.hepC.2016.mun  <- merge(df1,df_16, by.x = "cod_mun_completo", by.y = "CO_MUNICIPIO_HOSPITAL" , all.y = TRUE)

# Contagem de tratamentos por UF

cont_2016_hepC_UF <- table(df.hepC.2016.mun$UF)
cont_2016_hepC_UF <- as.data.frame(cont_2016_hepC_UF)

# merge contagem com a tabela UF 2016
contagem_hepC_2016_UF  <- merge(df.hepC.2016.mun,cont_2016_hepC_UF, by.x = "UF", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2016 <- distinct(contagem_hepC_2016_UF, UF, .keep_all = TRUE)

# salvar tabela de med para Hep C do ano de 2016
write.csv(df.hepC.2016.mun, file = '/Users/mikaellemos/Produtos/produto1/df.hepC.2016.mun.csv', row.names=FALSE)

# salvar tabela de med para Hep C por UF do ano de 2016
write.csv(UF_hepC_2016, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2016.csv', row.names=FALSE)

### Municipios 2016 ####

# Contagem de tratamentos por MUN

cont_2016_hepC_MUN <- table(df.hepC.2016.mun$`Código Município Completo`)
cont_2016_hepC_MUN <- as.data.frame(cont_2016_hepC_MUN)

# merge contagem com a tabela MUN 2016
contagem_hepC_2016_MUN  <- merge(df.hepC.2016.mun,cont_2016_hepC_MUN, by.x = "Código Município Completo", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2016_MUN <- distinct(contagem_hepC_2016_MUN, Nome_Município, .keep_all = TRUE)

# salvar tabela de med para Hep C por MUN do ano de 2016
write.csv(UF_hepC_2016_MUN, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2016_MUN.csv', row.names=FALSE)

############ tabela completa ano 2017 - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2017 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
cod_mun_IBGE_6d <- cbind(cod_mun_completo, cod_mun_IBGE)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
df1 <- as.data.frame(cod_mun_IBGE_6d)

# tabela contendo todos os municipios (IBGE 6 digitos) que receberam trtatamento de Hep C em 2017
df_17 <- df.APAC_procedimento_2017_mun_br 

# tabela completa contendo todos os múnicipios que receberam tratamento de Hep C em 2017 com cod IBGE 6 e 7 dígitos
df.hepC.2017.mun  <- merge(df1,df_17, by.x = "cod_mun_completo", by.y = "CO_MUNICIPIO_HOSPITAL" , all.y = TRUE)

# Remover NAs
df.hepC.2017.mun <- na.exclude(df.hepC.2017.mun)

# Contagem de tratamentos por UF

cont_2017_hepC_UF <- table(df.hepC.2017.mun$UF)
cont_2017_hepC_UF <- as.data.frame(cont_2017_hepC_UF)

# merge contagem com a tabela UF 2017
contagem_hepC_2017_UF  <- merge(df.hepC.2017.mun,cont_2017_hepC_UF, by.x = "UF", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2017 <- distinct(contagem_hepC_2017_UF, UF, .keep_all = TRUE)

# salvar tabela de med para Hep C do ano de 2017
write.csv(df.hepC.2017.mun, file = '/Users/mikaellemos/Produtos/produto1/df.hepC.2017.mun.csv', row.names=FALSE)

# salvar tabela de med para Hep C por UF do ano de 2017
write.csv(UF_hepC_2017, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2017.csv', row.names=FALSE)

### Municipios 2017 ####

# Contagem de tratamentos por MUN

cont_2017_hepC_MUN <- table(df.hepC.2017.mun$`Código Município Completo`)
cont_2017_hepC_MUN <- as.data.frame(cont_2017_hepC_MUN)

# merge contagem com a tabela MUN 2017
contagem_hepC_2017_MUN  <- merge(df.hepC.2017.mun,cont_2017_hepC_MUN, by.x = "Código Município Completo", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2017_MUN <- distinct(contagem_hepC_2017_MUN, Nome_Município, .keep_all = TRUE)

# salvar tabela de med para Hep C por MUN do ano de 2017
write.csv(UF_hepC_2017_MUN, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2017_MUN.csv', row.names=FALSE)

############ tabela completa ano 2018 - COD IBGE 7 digitos ################

# retirar úktimo digito do código completo 7 dígitos 
cod_mun_completo <- substr(cod_mun_IBGE$`Código Município Completo`, 1, 6)

# transformar lista de cidades 2018 com 6 digitos em df
cod_mun_completo <- as.data.frame(cod_mun_completo)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
cod_mun_IBGE_6d <- cbind(cod_mun_completo, cod_mun_IBGE)

# tabela completa IBGE contendo todos os municipios em 2018 (5570)
df1 <- as.data.frame(cod_mun_IBGE_6d)

# tabela contendo todos os municipios (IBGE 6 digitos) que receberam trtatamento de Hep C em 2018
df_18 <- df.APAC_procedimento_2018_mun_br 

# tabela completa contendo todos os múnicipios que receberam tratamento de Hep C em 2017 com cod IBGE 6 e 7 dígitos
df.hepC.2018.mun  <- merge(df1,df_18, by.x = "cod_mun_completo", by.y = "CO_MUNICIPIO_HOSPITAL" , all.y = TRUE)

# Contagem de tratamentos por UF

cont_2018_hepC_UF <- table(df.hepC.2018.mun$UF)
cont_2018_hepC_UF <- as.data.frame(cont_2018_hepC_UF)

# merge contagem com a tabela UF 2018
contagem_hepC_2018_UF  <- merge(df.hepC.2018.mun,cont_2018_hepC_UF, by.x = "UF", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2018 <- distinct(contagem_hepC_2018_UF, UF, .keep_all = TRUE)

# salvar tabela de med para Hep C do ano de 2018
write.csv(df.hepC.2018.mun, file = '/Users/mikaellemos/Produtos/produto1/df.hepC.2018.mun.csv', row.names=FALSE)

# salvar tabela de med para Hep C por UF do ano de 2018
write.csv(UF_hepC_2018, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2018.csv', row.names=FALSE)

### Municipios 2018 ####

# Contagem de tratamentos por MUN

cont_2018_hepC_MUN <- table(df.hepC.2018.mun$`Código Município Completo`)
cont_2018_hepC_MUN <- as.data.frame(cont_2018_hepC_MUN)

# merge contagem com a tabela MUN 2018
contagem_hepC_2018_MUN  <- merge(df.hepC.2018.mun,cont_2018_hepC_MUN, by.x = "Código Município Completo", by.y = "Var1" , all.x  = TRUE)

UF_hepC_2018_MUN <- distinct(contagem_hepC_2018_MUN, Nome_Município, .keep_all = TRUE)

# salvar tabela de med para Hep C por MUN do ano de 2018
write.csv(UF_hepC_2018_MUN, file = '/Users/mikaellemos/Produtos/produto1/UF_hepC_2018_MUN.csv', row.names=FALSE)

# 2015 por UF

UF_2015 <- sort(table(df.hepC.2015.mun$Nome_UF, useNA = "always"), decreasing = TRUE)

UF_2015 <- as.data.frame(UF_2015)

UF_2015$Porcentagem <- UF_2015$Freq / sum(UF_2015$Freq) * 100

sum(UF_2015$Freq)

write.csv(UF_2015, file = '/Users/mikaellemos/Produtos/produto1/UF_2015.csv', row.names=FALSE)


# 2016 por UF

UF_2016<- sort(table(df.hepC.2016.mun$Nome_UF, useNA = "always"), decreasing = TRUE)

UF_2016 <- as.data.frame(UF_2016)

UF_2016$Porcentagem <- UF_2016$Freq / sum(UF_2016$Freq) * 100

sum(UF_2016$Freq)

write.csv(UF_2016, file = '/Users/mikaellemos/Produtos/produto1/UF_2016.csv', row.names=FALSE)

# 2017 por UF

UF_2017<- sort(table(df.hepC.2017.mun$Nome_UF, useNA = "always"),decreasing = TRUE)

UF_2017 <- as.data.frame(UF_2017)

UF_2017$Porcentagem <- UF_2017$Freq / sum(UF_2017$Freq) * 100

sum(UF_2017$Freq)

write.csv(UF_2017, file = '/Users/mikaellemos/Produtos/produto1/UF_2017.csv', row.names=FALSE)

# 2018 por UF

UF_2018<- sort(table(df.hepC.2018.mun$Nome_UF, useNA = "always"), decreasing = TRUE)

UF_2018 <- as.data.frame(UF_2018)

UF_2018$Porcentagem <- UF_2018$Freq / sum(UF_2018$Freq) * 100

sum(UF_2018$Freq)

write.csv(UF_2018, file = '/Users/mikaellemos/Produtos/produto1/UF_2018.csv', row.names=FALSE)



# 2015 por MUN

MUN_2015 <- sort(table(df.hepC.2015.mun$Nome_Município, useNA = "always"), decreasing = TRUE)

MUN_2015 <- as.data.frame(MUN_2015)

MUN_2015$Porcentagem <- MUN_2015$Freq / sum(MUN_2015$Freq) * 100

sum(MUN_2015$Freq)

write.csv(MUN_2015, file = '/Users/mikaellemos/Produtos/produto1/MUN_2015.csv', row.names=FALSE)

# 2016 por MUN

MUN_2016 <- sort(table(df.hepC.2016.mun$Nome_Município, useNA = "always"), decreasing = TRUE)

MUN_2016 <- as.data.frame(MUN_2016)

MUN_2016$Porcentagem <- MUN_2016$Freq / sum(MUN_2016$Freq) * 100

sum(MUN_2016$Freq)

write.csv(MUN_2016, file = '/Users/mikaellemos/Produtos/produto1/MUN_2016.csv', row.names=FALSE)

# 2017 por MUN

MUN_2017 <- sort(table(df.hepC.2017.mun$Nome_Município, useNA = "always"), decreasing = TRUE)

MUN_2017 <- as.data.frame(MUN_2017)

MUN_2017$Porcentagem <- MUN_2017$Freq / sum(MUN_2017$Freq) * 100

sum(MUN_2015$Freq)

write.csv(MUN_2017, file = '/Users/mikaellemos/Produtos/produto1/MUN_2017.csv', row.names=FALSE)

# 2018 por MUN

MUN_2018 <- sort(table(df.hepC.2018.mun$Nome_Município, useNA = "always"), decreasing = TRUE)

MUN_2018 <- as.data.frame(MUN_2018)

MUN_2018$Porcentagem <- MUN_2018$Freq / sum(MUN_2018$Freq) * 100

sum(MUN_2018$Freq)

write.csv(MUN_2018, file = '/Users/mikaellemos/Produtos/produto1/MUN_2018.csv', row.names=FALSE)

# Pie chart % distrtibuição por UF por ano
UF_2015 <- na.exclude(UF_2015)
UF_2015["regiao"] <- c("sudeste", "sudeste", "sul", "centro oeste", "norte", "nordeste", "nordeste")



# Pie Chart with Percentages 2015
slices <- c(10317, 2506, 2554, 2036, 2614) 
lbls <- c("Sudeste", "Sul", "Centro Oeste", "Norte", "Nordeste")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Porcentagem de dispensação por região")

# Pie Chart with Percentages 2016
slices <- c(134974, 20384, 10333, 11526, 12899) 
lbls <- c("Sudeste", "Sul", "Centro Oeste", "Norte", "Nordeste")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Porcentagem de dispensação por região")

# Pie Chart with Percentages 2017
slices <- c(94831, 36539, 3377, 5925, 8973) 
lbls <- c("Sudeste", "Sul", "Centro Oeste", "Norte", "Nordeste")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Porcentagem de dispensação por região")


# Pie Chart with Percentages 2018
slices <- c(48644, 26862, 3677, 3559, 5700) 
lbls <- c("Sudeste", "Sul", "Centro Oeste", "Norte", "Nordeste")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Porcentagem de dispensação por região")