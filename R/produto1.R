######################################################################
############ Script de produto I do contrato 2021 ####################
####### Medicamentos e tratamentos  2007 - 2018 - hepatite C #########
########### V.1.0 - Desenvolvido por Mikael Lemos ####################
######################################################################

#### Carregando bibliotecas ####

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

#install.packages('Amelia')
library('Amelia')

# install.packages("tidyverse")
library(tidyverse)

# install.packages("lubridate")
library(lubridate)

# install.packages("ggplot2")
library(ggplot2)

#library(xlsx)

library(rJava)

#install.packages("read.dbc")

library(read.dbc)

#install.packages("forcats")

#library(forcats)

library("foreign")

#install.packages("foreign")

#install.packages("openxlsx")

library("openxlsx")

library(RColorBrewer)


###################
### Carregando bancos para os cruzamentos
##################

## SIH/AIH

AIH_SIH <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/AIH_PR_BDcompleto.csv")

AIH_SIH$DT_OCOR <- as.Date(AIH_SIH$DT_OCOR)

AIH_SIH$ano <- ymd(AIH_SIH$DT_OCOR)

AIH_SIH$ano <- year(AIH_SIH$ano)

## SIA

APAC_SIA_trat_hepc <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/TB_ESPELHO_APAC_202107281009.csv")

SIA_APAC <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/APAC_PR_BDcompleto.csv")

SIA_APAC$DT_OCOR <- as.Date(SIA_APAC$DT_OCOR)

SIA_APAC$ano <- ymd(SIA_APAC$DT_OCOR)

SIA_APAC$ano <- year(SIA_APAC$ano)

## BPAI

BPAI <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/BPAI_PR_BDcompleto.csv")

BPAI$DT_OCOR <- as.Date(BPAI$DT_OCOR)

BPAI$ano <- ymd(BPAI$DT_OCOR)

BPAI$ano <- year(BPAI$ano)

## GAL

GAL <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/GAL_hepc.csv")

## SINAN

SINAN <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SINAN.csv")

SINAN$DT_OCOR <- as.Date(SINAN$DT_OCOR)

SINAN$ano <- ymd(SINAN$DT_OCOR)

SINAN$ano <- year(SINAN$ano)

############
#### Filtro para hepatite C
###########


## AIH - SIH

AIH_SIH_hepc <- AIH_SIH %>% filter(grepl('C', HEPATITE))

## SIA

SIA_APAC_hepc <- SIA_APAC %>% filter(grepl('C', HEPATITE))

## BPAI

BPAI_hepc <- BPAI %>% filter(grepl('C', HEPATITE))

## SINAN

#SINAN_hepc <- SINAN %>% filter(grepl('C', HEPATITE))

## GAL

GAL_agravo <- table(GAL$AGRAVO_REQUISICAO)

GAL_agravo <- as.data.frame(GAL_agravo)

GAL_hepatite <- GAL %>% filter(grepl('HEPATITES VIRAIS', AGRAVO_REQUISICAO))

GAL_hepatite <- GAL %>% filter(grepl('Resultado: Detectavel|Resultado: Reagente|Resultado: Acima do limite de quantificacao', RESULTADO))

GAL_hepatite_c <- GAL_hepatite %>% filter(grepl('Hepatite C', NO_EXAME))

GAL_hepatite_c$CID <- "B182/B171"

GAL_hepatite_c$DB_ORIGEM <- "GAL"

GAL_hepatite_c$HEPATITE <- "c"

GAL_hepatite_c$IDADE <- " "

GAL_hepatite_c$ano <- " "

GAL_hepatite_c_filtro <- select(GAL_hepatite_c, "ID_PACIENTE", "CID", "PROCEDIMENTO" = "NO_EXAME", "SEXO" = "DS_SEXO", "MUN_RES" = "CO_MUNICIPIO_RESIDENCIA", "MUN_OCOR" = "CO_MUNICIPIO_RESIDENCIA", "DT_NASC" = "DT_NASCIMENTO",  "DT_OCOR" = "DT_COLETA", "DB_ORIGEM", "HEPATITE", "IDADE", "UF_RES" = "SG_UF_RESIDENCIA",  "UF_OCOR" = "CO_UF_REQUISICAO", "ano")

GAL_datas <- select(GAL_hepatite_c, "DT_SOLICITACAO", "DT_SINTOMAS", "DT_CADASTRO", "DT_COLETA", "DT_ENCAMINHADO", "DT_RECEBIMENTO", "DT_PROCESSAMENTO" , "DT_LIBERACAO" )

GAL_datas <- as.data.frame(GAL_datas)

GAL_hepatite_c_filtro$DT_NASC <- as.Date(GAL_hepatite_c_filtro$DT_NASC)
GAL_hepatite_c_filtro$DT_OCOR <- as.Date(GAL_hepatite_c_filtro$DT_OCOR)

##### Column "IDADE"
YEAR1 <- data_frame(year  = ymd(GAL_hepatite_c_filtro$DT_NASC))
YEAR2 <- data_frame(year = ymd(GAL_hepatite_c_filtro$DT_OCOR))

GAL_hepatite_c_filtro$IDADE <-  as.period(year(YEAR2$year) - year(YEAR1$year), units = "year")

GAL_hepatite_c_filtro$IDADE <- gsub("[S]", "", GAL_hepatite_c_filtro$IDADE)

GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "11"] <- "RO"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "12"] <- "AC"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "13"] <- "AM"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "14"] <- "RR"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "15"] <- "PA"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "16"] <- "AP"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "17"] <- "TO"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "21"] <- "MA"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "22"] <- "PI"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "23"] <- "CE"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "24"] <- "RN"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "25"] <- "PB"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "26"] <- "PE"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "27"] <- "AL"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "28"] <- "SE"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "29"] <- "BA"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "31"] <- "MG"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "32"] <- "ES"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "33"] <- "RJ"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "35"] <- "SP"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "41"] <- "PR"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "42"] <- "SC"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "43"] <- "RS"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "50"] <- "MS"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "51"] <- "MT"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "52"] <- "GO"
GAL_hepatite_c_filtro$UF_OCOR[GAL_hepatite_c_filtro$UF_OCOR == "53"] <- "DF"

#GAL - NAs 
is.na(GAL_hepatite_c) <- GAL_hepatite_c==''  
is.na(GAL_hepatite_c) <- GAL_hepatite_c=='*' 
is.na(GAL_hepatite_c) <- GAL_hepatite_c=='//'

is.na(GAL_datas) <- GAL_datas==''  
is.na(GAL_datas) <- GAL_datas=='*' 
is.na(GAL_datas) <- GAL_datas=='//'


## Divisão por ano - DT_OCOR  2007 - 2018

GAL_hepatite_c_filtro$DT_OCOR <- as.Date(GAL_hepatite_c_filtro$DT_OCOR)

GAL_hepatite_c_filtro$ano <- ymd(GAL_hepatite_c_filtro$DT_OCOR)

GAL_hepatite_c_filtro$ano <- year(GAL_hepatite_c_filtro$ano)


#####
## Filtro de procedimentos
####

#######
### UNIÃO DE BANCOS - Identificação de pacientes com hepatite C
#######

## AIH - SIH/ SIA - APAC/ BPAI / GAL

SIH_APAC_BPAI_GAL_hepc <- do.call("rbind", list( SIA_APAC_hepc, AIH_SIH_hepc, GAL_hepatite_c_filtro, BPAI_hepc))

SIH_APAC_BPAI_GAL_hepc_un <- distinct(SIH_APAC_BPAI_GAL_hepc, ID_PACIENTE , .keep_all = TRUE)


#SIH_APAC_BPAI_GAL_hepc - NAs 
is.na(SIH_APAC_BPAI_GAL_hepc_un) <- SIH_APAC_BPAI_GAL_hepc_un==''  

######
## Correção de datas - SIH_APAC_BPAI_GAL_hepc_un
######

SIH_APAC_BPAI_GAL_hepc_un_corre_datas <- filter(SIH_APAC_BPAI_GAL_hepc_un, ano == 1601 | ano == 1940 | ano == 1946 | ano == 1950 | ano == 1954 | ano == 1955 | ano == 1961 | ano == 1964 | ano == 1965 | ano == 1968 | ano == 1971 | ano == 1974 | ano == 1977 | ano == 1983 | ano == 1984 | ano == 1988 | ano == 1990 | ano == 2002 | ano == 2003 | ano == 2004 | ano == 2006 | ano == 2019)

SIH_APAC_BPAI_GAL_hepc_un_corre_datas_ij <- inner_join(SIH_APAC_BPAI_GAL_hepc_un_corre_datas, GAL_hepatite_c, by="ID_PACIENTE")

SIH_APAC_BPAI_GAL_hepc_un_corre_datas_ij_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_corre_datas_ij, ID_PACIENTE , .keep_all = TRUE)


SIH_APAC_BPAI_GAL_hepc_un$ano[is.na(SIH_APAC_BPAI_GAL_hepc_un$ano)] <- 2018

SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1968 ] <- 2014
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 2019 ] <- 2018
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1601 ] <- 2010
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1940 ] <- 2015
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1946 ] <- 2012
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1950 ] <- 2012
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1954 ] <- 2017
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1955 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1961 ] <- 2018
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1964 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1965 ] <- 2014
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1971 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1974 ] <- 2014
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1977 ] <- 2018
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1983 ] <- 2014
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1984 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1988 ] <- 2012
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 1990 ] <- 2016
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 2002 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 2003 ] <- 2013
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 2004 ] <- 2015
SIH_APAC_BPAI_GAL_hepc_un$ano[SIH_APAC_BPAI_GAL_hepc_un$ano == 2006 ] <- 2016



#####
##  Pacientes notificados no SINAN ou Não
#####

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_ij <- inner_join(SIH_APAC_BPAI_GAL_hepc_un, SINAN, by="ID_PACIENTE")

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_ij, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_ij.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj <- anti_join(SIH_APAC_BPAI_GAL_hepc_un, SINAN, by="ID_PACIENTE")

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj2 <- anti_join(SINAN, SIH_APAC_BPAI_GAL_hepc_un, by="ID_PACIENTE")

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj2, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj2.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj <- do.call("rbind", list( SIH_APAC_BPAI_GAL_hepc_un, SINAN))

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj, ID_PACIENTE , .keep_all = TRUE)

#####
##  Tratamentos - Tratados ou não - Todos tratamentos
#####

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij <- inner_join(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep, APAC_SIA_trat_hepc, by="ID_PACIENTE")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij, ID_PACIENTE , .keep_all = TRUE)

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj <- anti_join(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep, APAC_SIA_trat_hepc, by="ID_PACIENTE")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj, ID_PACIENTE , .keep_all = TRUE)

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj_un.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2 <- anti_join(APAC_SIA_trat_hepc, SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep, by="ID_PACIENTE")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un  <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2, ID_PACIENTE , .keep_all = TRUE)

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj <- full_join(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep, APAC_SIA_trat_hepc)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj, ID_PACIENTE , .keep_all = TRUE)

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un.csv")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60445001] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439001] <- "ALFAINTERFERONA 2B 3.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439002] <- "ALFAINTERFERONA 2B 5.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439004] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439005] <- "ALFAPEGINTERFERONA 2A 80MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439006] <- "ALFAPEGINTERFERONA 2A 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439007] <- "ALFAPEGINTERFERONA 2A 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60425001] <- "FILGRASTIM 300MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60447005] <- "ALFAEPOETINA 10.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60464001] <- "BOCEPREVIR 200 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60464002] <- "TELAPREVIR 375 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60464003] <- "SIMEPREVIR 150 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60476001] <- "SOFOSBUVIR 400 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60476002] <- "DACLATASVIR 60 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60476003] <- "DACLATASVIR 30 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60476004] <- "OMBITASVIR - 12,5MG/VERUPREVIR 75 MG/ RITONAVIR 50MG+DASABUVIR 250MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60439003] <- "ALFAINTERFERONA 2B 10.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60434007] <- "TACROLIMO 5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60446005] <- "TENOFOVIR 300MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60446004] <- "LAMIVUDINA 150MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60401002] <- "MESALAZINA 500MG "
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60404005] <- "ORMOTEROL 12MCG+BUDESONIDA 400 MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60428008] <- "BUDESONIDA 200MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60442001] <- "FLUDROCORTISONA 0.1MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60446002] <- "ENTECAVIR 0.5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60447004] <- "ALFAEPOETINA 4.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60432005] <- "MICOFENOLATO DE MOFETILA 500MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60446003] <- "LAMIVUDINA 10 MG/ML"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_PRINCIPAL == 60446006] <- "ENTECAVIR 1.0 MG"


SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604450010] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 601120035] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390017] <- "ALFAINTERFERONA 2B 3.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390025] <- "ALFAINTERFERONA 2B 5.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390033] <- "ALFAINTERFERONA 2B 10.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 601190114] <- "ALFAINTERFERONA 2B 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 601190122] <- "ALFAINTERFERONA 2B 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 601190050] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390041] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390050] <- "ALFAPEGINTERFERONA 2A 80MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390068] <- "ALFAPEGINTERFERONA 2A 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604390076] <- "ALFAPEGINTERFERONA 2A 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604250010] <- "FILGRASTIM 300MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604470053] <- "ALFAEPOETINA 10.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604640013] <- "BOCEPREVIR 200 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604640021] <- "TELAPREVIR 375 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604640030] <- "SIMEPREVIR 150 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604760019] <- "SOFOSBUVIR 400 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604760027] <- "DACLATASVIR 60 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604760035] <- "DACLATASVIR 30 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604760043] <- "OMBITASVIR - 12,5MG/VERUPREVIR 75 MG/ RITONAVIR 50MG+DASABUVIR 250MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604460023] <- "ENTECAVIR 0.5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604460040] <- "TENOFOVIR 300MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un$CO_PROCEDIMENTO_SECUNDARIO == 604460058] <- "TENOFOVIR 300MG"

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60445001] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439001] <- "ALFAINTERFERONA 2B 3.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439002] <- "ALFAINTERFERONA 2B 5.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439004] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439005] <- "ALFAPEGINTERFERONA 2A 80MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439006] <- "ALFAPEGINTERFERONA 2A 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439007] <- "ALFAPEGINTERFERONA 2A 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60425001] <- "FILGRASTIM 300MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60447005] <- "ALFAEPOETINA 10.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60464001] <- "BOCEPREVIR 200 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60464002] <- "TELAPREVIR 375 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60464003] <- "SIMEPREVIR 150 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60476001] <- "SOFOSBUVIR 400 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60476002] <- "DACLATASVIR 60 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60476003] <- "DACLATASVIR 30 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60476004] <- "OMBITASVIR - 12,5MG/VERUPREVIR 75 MG/ RITONAVIR 50MG+DASABUVIR 250MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60439003] <- "ALFAINTERFERONA 2B 10.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60434007] <- "TACROLIMO 5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60446005] <- "TENOFOVIR 300MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60446004] <- "LAMIVUDINA 150MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60401002] <- "MESALAZINA 500MG "
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60404005] <- "ORMOTEROL 12MCG+BUDESONIDA 400 MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60428008] <- "BUDESONIDA 200MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60442001] <- "FLUDROCORTISONA 0.1MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60446002] <- "ENTECAVIR 0.5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60447004] <- "ALFAEPOETINA 4.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60432005] <- "MICOFENOLATO DE MOFETILA 500MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60446003] <- "LAMIVUDINA 10 MG/ML"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60446006] <- "ENTECAVIR 1.0 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_PRINCIPAL == 60401009] <- "SULFASSALAZINA 500MG"


SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604450010] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601120035] <- "RIBAVIRINA 250 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390017] <- "ALFAINTERFERONA 2B 3.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390025] <- "ALFAINTERFERONA 2B 5.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390033] <- "ALFAINTERFERONA 2B 10.000.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601190114] <- "ALFAINTERFERONA 2B 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601190122] <- "ALFAINTERFERONA 2B 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601190050] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390041] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390050] <- "ALFAPEGINTERFERONA 2A 80MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390068] <- "ALFAPEGINTERFERONA 2A 100MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604390076] <- "ALFAPEGINTERFERONA 2A 120MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604250010] <- "FILGRASTIM 300MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604470053] <- "ALFAEPOETINA 10.000 UI"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604640013] <- "BOCEPREVIR 200 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604640021] <- "TELAPREVIR 375 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604640030] <- "SIMEPREVIR 150 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604760019] <- "SOFOSBUVIR 400 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604760027] <- "DACLATASVIR 60 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604760035] <- "DACLATASVIR 30 MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604760043] <- "OMBITASVIR - 12,5MG/VERUPREVIR 75 MG/ RITONAVIR 50MG+DASABUVIR 250MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604460023] <- "ENTECAVIR 0.5MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604460040] <- "TENOFOVIR 300MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604460058] <- "TENOFOVIR 300MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601160010] <- "FILGRASTIM 300MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 601190041] <- "ALFAPEGINTERFERONA 2A 180MCG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604460015] <- "ADEFOVIR 10MG"
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO[SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un$CO_PROCEDIMENTO_SECUNDARIO == 604460031] <- "LAMIVUDINA 10MG/ML"


#SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_tratados <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un %>% filter(grepl('604450010|604390017|604390025|0604390041|604390050|604390068|604390076|604250010|604470053|604640013|604640021|604640030|604760019|604760027|604760035|604760043', PROCEDIMENTO))

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un %>% separate(PROCEDIMENTO, c("PROCEDIMENTO1","PROCEDIMENTO2", "PROCEDIMENTO3"), "_", extra = "merge") 

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un_sep.csv")

write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_fj_un.csv")


##############
##### Gráficos
##############

### tabela plot - frequência de pacientes com hep c/tratamentos e porcentagem pacientes hep c/tratamentos por ano

table(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano, exclude = NULL)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_OCORR <- as.Date(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_OCORR, format = "%d/%m/%Y")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_INICIO <- as.Date(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_INICIO, format = "%d/%m/%Y")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_SOLIC <- as.Date(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_SOLIC, format = "%d/%m/%Y")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_FIM <- as.Date(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_FIM, format = "%d/%m/%Y")
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_GERACAO <- as.Date(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_GERACAO, format = "%d/%m/%Y")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano2 <- ymd(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_OCORR)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano2 <- year(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano2)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano3 <- ymd(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_INICIO)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano3 <- year(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano3)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano4 <- ymd(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_SOLIC)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano4 <- year(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano4)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano5 <- ymd(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_FIM)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano5 <- year(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano5)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano6 <- ymd(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$DATA_GERACAO)
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano6 <- year(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano6)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un %>% mutate(ano7 = coalesce(ano3, ano6, ano5, ano4, ano2))

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un %>% mutate(ano8 = coalesce(ano, ano7))

### Correção de datas

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un_datas <- filter(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un, ano8 == 1952 | ano8 == 1977 | ano8 == 1979 | ano8 == 1982 | ano8 == 1983 | ano8 == 1984 | ano8 == 1987 | ano8 == 1990 | ano8 == 1992 | ano8 == 1993 | ano8 == 1994 | ano8 == 1995 | ano8 == 1996 | ano8 == 1997 | ano8 == 1998 | ano8 == 1999 | ano8 == 2000 | ano8 == 2001 | ano8 == 2002 | ano8 == 2003 | ano8 == 2004 | ano8 == 2005 | ano8 == 2006)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un_datas_un <- distinct(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un_datas, ID_PACIENTE , .keep_all = TRUE)

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un_datas_un <- select(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un_datas_un, "DT_OCOR", "ano", "ano8")

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano8[is.na(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un$ano8)] <- 2007

plot_hep_tratamentos <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un %>% group_by(ID_PACIENTE, ano8 )
plot_hep_tratamentos_n <- plot_hep_tratamentos %>% summarise(n = n())

plot_hep_tratamentos_n$CO_PROCEDIMENTO_PRINCIPAL <- ""

plot_hep_tratamentos_n <- select(plot_hep_tratamentos_n, "ID_PACIENTE", "ano" = "ano8", "CO_PROCEDIMENTO_PRINCIPAL", "n")

#### tabela Plot tratamentos - todos os tratamentos

plot_todos_tratamentos <- full_join(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_aj2_un, SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_ij_un)
plot_todos_tratamentos_un <-  distinct(plot_todos_tratamentos, ID_PACIENTE , .keep_all = TRUE)

plot_todos_tratamentos_un$COMPETENCIA <- ym(plot_todos_tratamentos_un$COMPETENCIA)
plot_todos_tratamentos_un$ano <- year(plot_todos_tratamentos_un$COMPETENCIA)

plot_todos_tratamentos_un <- plot_todos_tratamentos_un %>% group_by(ID_PACIENTE, ano, CO_PROCEDIMENTO_PRINCIPAL )
plot_todos_tratamentos_un_n <- plot_todos_tratamentos_un %>% summarise(n = n())

plot_todos_tratamentos__hepc_un_n <- do.call("rbind", list(plot_hep_tratamentos_n, plot_todos_tratamentos_un_n))

plot_todos_tratamentos__hepc_un_n <- full_join(plot_todos_tratamentos_un_n, plot_hep_tratamentos_n )

plot_todos_tratamentos__hepc_un_n_ij <- inner_join(plot_todos_tratamentos_un_n, plot_hep_tratamentos_n, by = "ID_PACIENTE")

plot_todos_tratamentos__hepc_un_n_aj2 <- anti_join(plot_hep_tratamentos_n, plot_todos_tratamentos_un_n , by = "ID_PACIENTE")

plot_todos_tratamentos__hepc_un_n_aj2$tratamento <- "Diagnosticados"
plot_todos_tratamentos__hepc_un_n_ij$tratamento <- "Tratados"

plot_todos_tratamentos__hepc_un_n_ij <- select(plot_todos_tratamentos__hepc_un_n_ij, "ID_PACIENTE", "ano" = "ano.x",  "CO_PROCEDIMENTO_PRINCIPAL" = "CO_PROCEDIMENTO_PRINCIPAL.x" , "n" = "n.x" )

plot_todos_tratamentos__hepc_un_n <- do.call("rbind", list(plot_todos_tratamentos__hepc_un_n_ij, plot_todos_tratamentos__hepc_un_n_aj2))

is.na(plot_todos_tratamentos__hepc_un_n) <- plot_todos_tratamentos__hepc_un_n==''  

plot_todos_tratamentos__hepc_un_n <- plot_todos_tratamentos__hepc_un_n %>% group_by(CO_PROCEDIMENTO_PRINCIPAL, tratamento, ano )
plot_todos_tratamentos__hepc_un_n <- plot_todos_tratamentos__hepc_un_n %>% summarise(n = n())

plot_todos_tratamentos__hepc_un_n_ano <- aggregate(plot_todos_tratamentos__hepc_un_n['n'], by=c(plot_todos_tratamentos__hepc_un_n['ano'],plot_todos_tratamentos__hepc_un_n['tratamento']), sum)


plot_todos_tratamentos__hepc_un_n_ano_2007_2018 <- filter(plot_todos_tratamentos__hepc_un_n_ano, ano == 2007 | ano == 2008 |  ano == 2009|  ano == 2010 |  ano == 2011 |  ano == 2012 |  ano == 2013 |  ano == 2014 |  ano == 2015 |  ano == 2016|  ano == 2017 |  ano == 2018 )

#plot_todos_tratamentos__hepc_un_n_ano_2007_2018 = mutate(plot_todos_tratamentos__hepc_un_n_ano_2007_2018, 
 #               n_pct = n / sum(n))
    

#plot_todos_tratamentos__hepc_un_n_ano_2007_2018 = mutate(plot_todos_tratamentos__hepc_un_n_ano_2007_2018, pct=paste(round(n_pct*100,1)))


# total de Tratamentos/pacientes com diagnóstico de hepatite C por ano

### Frequência

ggplot(data=plot_todos_tratamentos__hepc_un_n_ano_2007_2018, aes(x=ano, y=n, fill=tratamento)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() +  scale_x_continuous(name ="",breaks= plot_todos_tratamentos__hepc_un_n_ano_2007_2018$ano)+
  scale_y_continuous(name="Frequência") + theme(legend.text = element_text(colour="black", size=12, face="bold") , legend.title = element_blank(),axis.text.y =element_text(hjust=7,size=14, face = "bold" ), axis.text.x=element_text(vjust=7,size=14, face = "bold" ) ,axis.title.x = element_blank(),
axis.title.y = element_text(size = 14, face = "bold")
  )


### Porcentagem

ggplot(plot_todos_tratamentos__hepc_un_n_2007_2018, aes(x=as.factor(ano), fill=as.factor(tratamento)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" , size=5) + scale_fill_brewer(palette="Paired") + theme_minimal()+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('% de pessoas diagnosticadas/tratadas') + scale_y_continuous(labels = scales::percent) + theme(legend.text = element_text(colour="black", size=12, face="bold") , legend.title = element_blank(),axis.text.y =element_text(size=14, face = "bold" ), axis.text.x=element_text(vjust=7,size=14, face = "bold" ) ,axis.title.x = element_blank(),
                                                                                                         axis.title.y = element_text(size = 14, face = "bold")) 

plot_todos_tratamentos__hepc_un_n_2007_2018 <- filter(plot_todos_tratamentos__hepc_un_n, ano == 2007 | ano == 2008 |  ano == 2009|  ano == 2010 |  ano == 2011 |  ano == 2012 |  ano == 2013 |  ano == 2014 |  ano == 2015 |  ano == 2016|  ano == 2017 |  ano == 2018 )

### Venn diagram - pacientes com diagnóstico de hep c x SINAN - subnotificação/porcentagem subnotificação por banco de dados

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj %>% group_by(ID_PACIENTE,DB_ORIGEM, ano )
SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb_n <- SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb %>% summarise(n = n())

SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb_n_2007_2018 <- filter(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb_n, ano == 2007 | ano == 2008 |  ano == 2009|  ano == 2010 |  ano == 2011 |  ano == 2012 |  ano == 2013 |  ano == 2014 |  ano == 2015 |  ano == 2016|  ano == 2017 |  ano == 2018 )

ggplot(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_aj_tb_n, aes(x=as.factor(ano), fill=as.factor(DB_ORIGEM)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" , size=5)  + theme_minimal()+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], angle=90,hjust=-0.1, label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('% dos bancos nos casos subnotificados') + scale_y_continuous(labels = scales::percent) + theme(legend.text = element_text(colour="black", size=12, face="bold") , legend.title = element_blank(),axis.text.y =element_text(size=14, face = "bold" ), axis.text.x=element_text(vjust=7,size=14, face = "bold" ) ,axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_text(size = 14, face = "bold")) 

# distribuição de tratamentos por ano 

#plot_todos_tratamentos__hepc_un_n_tratamento <- aggregate(plot_todos_tratamentos__hepc_un_n['n'], by=c(plot_todos_tratamentos__hepc_un_n['ano'],plot_todos_tratamentos__hepc_un_n['CO_PROCEDIMENTO_PRINCIPAL']), sum)

#plot_todos_tratamentos__hepc_un_n_tratamento_2007_2018 <- filter(plot_todos_tratamentos__hepc_un_n_tratamento, ano == 2007 | ano == 2008 |  ano == 2009|  ano == 2010 |  ano == 2011 |  ano == 2012 |  ano == 2013 |  ano == 2014 |  ano == 2015 |  ano == 2016|  ano == 2017 |  ano == 2018 )

plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "RIBAVIRINA 250 MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAINTERFERONA 2B 3.000.000 UI"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL ==  "ALFAINTERFERONA 2B 5.000.000 UI"] <-"OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAPEGINTERFERONA 2A 180MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAPEGINTERFERONA 2A 80MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAPEGINTERFERONA 2A 100MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAPEGINTERFERONA 2A 120MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "FILGRASTIM 300MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAEPOETINA 10.000 UI"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "BOCEPREVIR 200 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "TELAPREVIR 375 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "SIMEPREVIR 150 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "SOFOSBUVIR 400 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL ==  "DACLATASVIR 60 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "DACLATASVIR 30 MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL ==  "OMBITASVIR - 12,5MG/VERUPREVIR 75 MG/ RITONAVIR 50MG+DASABUVIR 250MG"] <- "DAA"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAINTERFERONA 2B 10.000.000 UI"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL ==  "TACROLIMO 5MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "TENOFOVIR 300MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "LAMIVUDINA 150MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "MESALAZINA 500MG "] <-  "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ORMOTEROL 12MCG+BUDESONIDA 400 MCG"] <-  "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "BUDESONIDA 200MCG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "FLUDROCORTISONA 0.1MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ENTECAVIR 0.5MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ALFAEPOETINA 4.000 UI"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "MICOFENOLATO DE MOFETILA 500MG"] <-  "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "LAMIVUDINA 10 MG/ML"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "ENTECAVIR 1.0 MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat[plot_todos_tratamentos__hepc_un_n_2007_2018$CO_PROCEDIMENTO_PRINCIPAL == "SULFASSALAZINA 500MG"] <- "OUTROS TRATAMENTOS"
plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat <- replace_na(plot_todos_tratamentos__hepc_un_n_2007_2018$ttrat, "NÃO TRATADO")


plot_todos_tratamentos__hepc_un_n_2007_2018gb <- plot_todos_tratamentos__hepc_un_n_2007_2018 %>% group_by( ttrat, ano )
plot_todos_tratamentos__hepc_un_n_2007_2018gbn <- plot_todos_tratamentos__hepc_un_n_2007_2018gb %>% summarise(n = n())

plot_todos_tratamentos__hepc_un_n_2007_2018gbn <- aggregate(plot_todos_tratamentos__hepc_un_n_2007_2018gbn['n'], by=c(plot_todos_tratamentos__hepc_un_n_2007_2018gbn['ano'],plot_todos_tratamentos__hepc_un_n_2007_2018gbn['ttrat']), sum)



## Freq

ggplot(data=plot_todos_tratamentos__hepc_un_n_2007_2018gbn, aes(x=ano, y=n, fill=ttrat)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  geom_text(aes(label=n), vjust=-0.3, position = position_dodge(0.9), size=5)+
  theme_minimal() +  scale_x_continuous(name ="",breaks= plot_todos_tratamentos__hepc_un_n_2007_2018gbn$ano)+
  scale_y_continuous(name="Frequência") + theme(legend.text = element_text(colour="black", size=12, face="bold") , legend.title = element_blank(),axis.text.y =element_text(hjust=7,size=14, face = "bold" ), axis.text.x=element_text(vjust=7,size=14, face = "bold" ) ,axis.title.x = element_blank(),
                                                axis.title.y = element_text(size = 14, face = "bold")
  )

# porcentagem tratamentos x DAAs 

ggplot(plot_todos_tratamentos__hepc_un_n_2007_2018gbn, aes(x=as.factor(ano), fill=as.factor(ttrat)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" , size=5) + scale_fill_brewer(palette="Paired") + theme_minimal()+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('% de pessoas diagnosticadas/tratadas') + scale_y_continuous(labels = scales::percent) + theme(legend.text = element_text(colour="black", size=12, face="bold") , legend.title = element_blank(),axis.text.y =element_text(size=14, face = "bold" ), axis.text.x=element_text(vjust=7,size=14, face = "bold" ) ,axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_text(size = 14, face = "bold")) 


##############
 


write.csv(SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/SIH_APAC_BPAI_GAL_hepc_un_notif_sinan_APAC_medicamentos_fj_un.csv")
write.csv(plot_todos_tratamentos_un_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos_un_n.csv")
write.csv(plot_hep_tratamentos_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_hep_tratamentos_n.csv")
write.csv(plot_todos_tratamentos__hepc_un_n, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n.csv")
write.csv(plot_todos_tratamentos__hepc_un_n_ano, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_ano.csv")
write.csv(plot_todos_tratamentos__hepc_un_n_tratamento, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_tratamento.csv")
write.csv(plot_todos_tratamentos__hepc_un_n_2007_2018, "C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_2007_2018.csv")


plot_todos_tratamentos__hepc_un_n <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n.csv")
plot_todos_tratamentos__hepc_un_n_2007_2018 <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_2007_2018.csv")
plot_todos_tratamentos__hepc_un_n_ano <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_ano.csv")
plot_todos_tratamentos__hepc_un_n_tratamento <- read.csv("C:/Users/lemos/Downloads/HEPATITES/produtos_opas/contrato_2021/produto1/BANCOS/plot_todos_tratamentos__hepc_un_n_tratamento.csv")


