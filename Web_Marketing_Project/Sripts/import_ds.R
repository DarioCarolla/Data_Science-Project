#### IMPORTING DATA SETS ####
setwd("~/Desktop/Università/Web Marketing/Lab_digital_marketing/Progetto")

### clients fidelity subscriptions ###
df_1_cli_fid <- read.csv2("dataset/raw_1_cli_fid.csv", na.strings = c("NA", ""))

### clients accounts details ###
df_2_cli_account <- read.csv2("dataset/raw_2_cli_account.csv", na.strings = c("NA", ""))

### clients addresses ###
df_3_cli_address <- read.csv2("dataset/raw_3_cli_address.csv", na.strings = c(""), stringsAsFactors = F)

### clients privacy ###
df_4_cli_privacy <- read.csv2("dataset/raw_4_cli_privacy.csv" , na.strings = c("NA", ""))

### email campaign characterization ###
df_5_camp_cat <- read.csv2("dataset/raw_5_camp_cat.csv" , na.strings = c("NA", ""))

### email event ###
df_6_camp_event <- read.csv2("dataset/raw_6_camp_event.csv" , na.strings = c("NA", ""))

### scontrino ###
df_7_tic <- read.csv2("dataset/raw_7_tic.csv" , na.strings = c("NA", ""))
