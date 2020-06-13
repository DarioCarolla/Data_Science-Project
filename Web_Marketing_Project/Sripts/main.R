# Progetto Web Marketing di:
#     DARIO CAROLLA mat. 807547
#     MATTEO LICCIARDELLO mat. 799368
#     FEDERICO DA RONCH mat. 807918

# Caricamento librerie utilizzate
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(pander)
library(randomForest)
library(e1071)
library(caret)
library(sjmisc)
library(BBmisc)
library(ROCR)
library(readxl)
library(corrplot)
library(nnet)
library(NeuralNetTools)

# set working directory
setwd("~/Desktop/Progetto_def")

# set seed
set.seed(12345)

# caricamento dei dati
source("import_ds.R")

#### DESCRIIONE DATAFRAME ####

# df_1_cli_fid -> clienti fidelizzati
# ID_CLI -> id cliente
# ID_FID -> id fidelizzazione
# ID_NEG -> id negozio di riferimento
# TYP_CLI_FID -> main account binatia (0/1)
# COD_FID -> tipo di programma fedeltà:
#                 PREMIUM 
#                 PREMIUM BIZ 
#                 STANDARD 
#                 STANDARD BIZ
# STATUS_FID -> stato fidelizzazione (0/1)
# DT_ACTIVE -> data attivazione fidelizzazione


# df_2_cli_account -> account cliente
# ID_CLI -> id cliente
# EMAIL_PROVIDER -> tipo di email 
# W_PHONE -> numero di telefono aggiunto (0/1)
# ID_ADDRESS -> id address
# TYP_CLI_ACCOUNT -> tipo di cliente (4 o 2) ??
# TYP_JOB -> lavoro cliente


# df_3_cli_address
# ID_ADDRESS -> id address
# CAP -> codice postale
# PRV -> provincia 
# REGION -> regione


# df_4_cli_privacy
# ID_CLI -> id cliente
# FLAG_PRIVACY_1 -> ?? (0/1)
# FLAG_PRIVACY_2 -> ?? (0/1)
# FLAG_DIRECT_MKT -> ?? (0/1)


# df_5_camp_cat
# ID_CAMP ->id campagna
# TYP_CAMP -> tipo di campagna email
#             tipi:
#                   PRODUCT            372 
#                   PERSONALIZED       169 
#                   NATIONAL           150 
#                   NEWSLETTER         109 
#                   LOCAL              48


#df_6_camp_event
# ID_EVENT -> id evento feedback
# ID_CLI -> id cliente
# ID_CAMP -> id campagna
# ID_DELIVERY -> id consegna email
# TYP_EVENT -> tipo di evento:
               # tipo:
               #      S = send
               #      V = open
               #      C = click
               #      B = bounce
               #      E = error
# EVENT_DATE -> datetime evento

# df_7_tic -> scontrino
# ID_SCONTRINO -> id scontrino
# ID_CLI -> id cliente
# ID_NEG -> id negozio
# ID_ARTICOLO -> id articolo acquistato o rimborsato
# COD_REPARTO -> identifica il reparto a cui corrisponde l'articolo
# DIREZIONE -> acquistato (1) rimborsato (-1)
# IMPORTO_LORDO -> importo lordo = importo netto + sconto applicato 
#                  (negativo se rimborsato)
# SCONTO -> sconto applicato
# DATETIME -> datatime dell'acquisto


#### CLEANING DF ####
# Di seguito vengono eseguite diverse operazioni sui data frame forniti
# sia per pulire i dati sia per fornire una prima analisi di questi ultimi.
source("cleaning_df1.R", echo = FALSE)
source("cleaning_df2.R", echo = FALSE)
source("cleaning_df3.R", echo = FALSE)
source("cleaning_df4.R", echo = FALSE)
source("cleaning_df5.R", echo = FALSE)
source("cleaning_df6.R", echo = FALSE)
source("cleaning_df7.R", echo = FALSE)

#### DATA PREPARATION EMAIL ####
# Di seguito vengono effettuate varie operazione per la creazione di un dataframe
# che verra utilizzato per prevedere la propensione di un cliente ad aprire una email
source("preparation_email.R")

#### PREVISIONI EMAIL ####
# Di seguito vengono testati tre diversi modelli per la "propensity of email engagement"
source("ML_mail.R")

#### DATA PREPARATION CHURN ####
# Di seguito vengono effettuate varie operazione per la creazione di un dataframe
# che verra utilizzato per prevedere la propensione di un cliente ad essere un churn
source("churn.R")

#### PREVISIONI CHURN ####
# Di seguito vengono testati tre diversi modelli per la "propensity to churn"
source("ML_churn.R")


