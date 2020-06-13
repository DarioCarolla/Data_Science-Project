### df_3_cli_address ###

# Summary df_3_cli_address
str(df_3_cli_address)
summary(df_3_cli_address)

# La variabile REGION ha numerosi valori missing, quindi è stata applicata una funzione 
# per inserire i valori mancanti utilizzando il valore CAP. Da 23.894 NA si ottengono 1.328 NA. 
df_3_cli_address_clean <- df_3_cli_address %>%
  mutate(CAP = as.numeric(CAP)) %>%
  mutate(REGN = case_when(
    CAP >= 66010 & CAP <= 66100 ~ "ABRUZZO",
    CAP >= 67010 & CAP <= 67100 ~ "ABRUZZO",
    CAP >= 65010 & CAP <= 65029 ~ "ABRUZZO", 
    CAP >= 65121 & CAP <= 65129 ~ "ABRUZZO",
    CAP >= 64010 & CAP <= 64100 ~ "ABRUZZO",
    CAP >= 75010 & CAP <= 75100 ~ "BASILICATA",
    CAP >= 85010 & CAP <= 85100 ~ "BASILICATA",
    CAP >= 88020 & CAP <= 88100 ~ "CALABRIA",
    CAP >= 87010 & CAP <= 87100 ~ "CALABRIA",
    CAP >= 88811 & CAP <= 88900 ~ "CALABRIA",
    CAP >= 89010 & CAP <= 89069 ~ "CALABRIA",
    CAP >= 89121 & CAP <= 89135 ~ "CALABRIA",
    CAP >= 89812 & CAP <= 89900 ~ "CALABRIA",
    CAP >= 83010 & CAP <= 83100 ~ "CAMPANIA",
    CAP >= 82010 & CAP <= 82100 ~ "CAMPANIA",
    CAP >= 81010 & CAP <= 81100 ~ "CAMPANIA",
    CAP >= 80010 & CAP <= 80081 ~ "CAMPANIA",
    CAP >= 80121 & CAP <= 80147 ~ "CAMPANIA",
    CAP >= 84010 & CAP <= 84099 ~ "CAMPANIA",
    CAP >= 84121 & CAP <= 84135 ~ "CAMPANIA",
    CAP >= 40010 & CAP <= 40069 ~ "EMILIA ROMAGNA",
    CAP >= 40121 & CAP <= 40141 ~ "EMILIA ROMAGNA",
    CAP >= 44011 & CAP <= 44049 ~ "EMILIA ROMAGNA",
    CAP >= 44121 & CAP <= 44124 ~ "EMILIA ROMAGNA",
    CAP >= 47010 & CAP <= 47043 ~ "EMILIA ROMAGNA",
    CAP >= 47521 & CAP <= 47522 ~ "EMILIA ROMAGNA",
    CAP >= 47121 & CAP <= 47122 ~ "EMILIA ROMAGNA",
    CAP >= 41011 & CAP <= 41059 ~ "EMILIA ROMAGNA",
    CAP >= 41121 & CAP <= 41126 ~ "EMILIA ROMAGNA",
    CAP >= 43010 & CAP <= 43059 ~ "EMILIA ROMAGNA",
    CAP >= 43121 & CAP <= 43126 ~ "EMILIA ROMAGNA",
    CAP >= 29010 & CAP <= 29031 ~ "EMILIA ROMAGNA",
    CAP >= 29121 & CAP <= 29122 ~ "EMILIA ROMAGNA",
    CAP >= 48011 & CAP <= 48034 ~ "EMILIA ROMAGNA",
    CAP >= 48121 & CAP <= 48125 ~ "EMILIA ROMAGNA",
    CAP >= 42010 & CAP <= 42049 ~ "EMILIA ROMAGNA",
    CAP >= 42121 & CAP <= 42124 ~ "EMILIA ROMAGNA",
    CAP >= 47814 & CAP <= 47867 ~ "EMILIA ROMAGNA",
    CAP >= 47921 & CAP <= 47924 ~ "EMILIA ROMAGNA",
    CAP >= 34070 & CAP <= 34170 ~ "FRIULI VENEZIA GIULIA",
    CAP >= 33070 & CAP <= 33170 ~ "FRIULI VENEZIA GIULIA",
    CAP >= 34010 & CAP <= 34018 ~ "FRIULI VENEZIA GIULIA",
    CAP >= 34121 & CAP <= 34151 ~ "FRIULI VENEZIA GIULIA",
    CAP >= 33010 & CAP <= 33100 ~ "FRIULI VENEZIA GIULIA",
    CAP >= 03010 & CAP <= 03100 ~ "LAZIO",
    CAP >= 04010 & CAP <= 04100 ~ "LAZIO",
    CAP >= 02010 & CAP <= 02100 ~ "LAZIO",
    CAP >= 00010 & CAP <= 00079 ~ "LAZIO",
    CAP >= 00118 & CAP <= 00199 ~ "LAZIO",
    CAP >= 01010 & CAP <= 01100 ~ "LAZIO",
    CAP >= 16010 & CAP <= 16049 ~ "LIGURIA",
    CAP >= 16121 & CAP <= 16167 ~ "LIGURIA",
    CAP >= 18010 & CAP <= 18100 ~ "LIGURIA",
    CAP >= 19010 & CAP <= 19038 ~ "LIGURIA",
    CAP >= 19121 & CAP <= 19137 ~ "LIGURIA",
    CAP >= 17010 & CAP <= 17100 ~ "LIGURIA",
    CAP >= 24010 & CAP <= 24069 ~ "LOMBARDIA",
    CAP >= 24121 & CAP <= 24129 ~ "LOMBARDIA",
    CAP >= 25010 & CAP <= 25089 ~ "LOMBARDIA",
    CAP >= 25121 & CAP <= 25136 ~ "LOMBARDIA",
    CAP >= 22010 & CAP <= 22100 ~ "LOMBARDIA",
    CAP >= 26010 & CAP <= 26100 ~ "LOMBARDIA",
    CAP >= 23801 & CAP <= 23900 ~ "LOMBARDIA",
    CAP >= 26811 & CAP <= 26900 ~ "LOMBARDIA",
    CAP >= 46010 & CAP <= 46100 ~ "LOMBARDIA",
    CAP >= 20010 & CAP <= 20099 ~ "LOMBARDIA",
    CAP >= 20121 & CAP <= 20162 ~ "LOMBARDIA",
    CAP >= 20811 & CAP <= 20900 ~ "LOMBARDIA",
    CAP >= 27010 & CAP <= 27100 ~ "LOMBARDIA",
    CAP >= 23010 & CAP <= 23100 ~ "LOMBARDIA",
    CAP >= 21010 & CAP <= 21100 ~ "LOMBARDIA",
    CAP >= 60010 & CAP <= 60048 ~ "MARCHE",
    CAP >= 63061 & CAP <= 63100 ~ "MARCHE",
    CAP >= 63811 & CAP <= 63900 ~ "MARCHE",
    CAP >= 62010 & CAP <= 62100 ~ "MARCHE",
    CAP >= 61010 & CAP <= 61049 ~ "MARCHE",
    CAP >= 61121 & CAP <= 61122 ~ "MARCHE",
    CAP >= 86010 & CAP <= 86100 ~ "MOLISE",
    CAP >= 86070 & CAP <= 86170 ~ "MOLISE",
    CAP >= 15010 & CAP <= 15079 ~ "PIEMONTE",
    CAP >= 15121 & CAP <= 15122 ~ "PIEMONTE",
    CAP >= 14010 & CAP <= 14100 ~ "PIEMONTE",
    CAP >= 13811 & CAP <= 13900 ~ "PIEMONTE",
    CAP >= 12010 & CAP <= 12100 ~ "PIEMONTE",
    CAP >= 28010 & CAP <= 28100 ~ "PIEMONTE",
    CAP >= 10010 & CAP <= 10099 ~ "PIEMONTE",
    CAP >= 10121 & CAP <= 10156 ~ "PIEMONTE",
    CAP >= 28801 & CAP <= 28899 ~ "PIEMONTE",
    CAP >= 28921 & CAP <= 28925 ~ "PIEMONTE",
    CAP >= 13010 & CAP <= 13100 ~ "PIEMONTE",
    CAP >= 70010 & CAP <= 70056 ~ "PUGLIA",
    CAP >= 70121 & CAP <= 70132 ~ "PUGLIA",
    CAP >= 76011 & CAP <= 76125 ~ "PUGLIA",
    CAP >= 72012 & CAP <= 72100 ~ "PUGLIA",
    CAP >= 71010 & CAP <= 71051 ~ "PUGLIA",
    CAP >= 71121 & CAP <= 71122 ~ "PUGLIA",
    CAP >= 73010 & CAP <= 73100 ~ "PUGLIA",
    CAP >= 74010 & CAP <= 74028 ~ "PUGLIA",
    CAP >= 74121 & CAP <= 74123 ~ "PUGLIA",
    CAP >= 09012 & CAP <= 09069 ~ "SARDEGNA",
    CAP >= 09121 & CAP <= 09134 ~ "SARDEGNA",
    CAP >= 08010 & CAP <= 08100 ~ "SARDEGNA",
    CAP >= 09070 & CAP <= 09170 ~ "SARDEGNA",
    CAP >= 07010 & CAP <= 07100 ~ "SARDEGNA",
    CAP >= 09010 & CAP <= 09066 ~ "SARDEGNA",
    CAP >= 92010 & CAP <= 92100 ~ "SICILIA",
    CAP >= 93010 & CAP <= 93100 ~ "SICILIA",
    CAP >= 95010 & CAP <= 95049 ~ "SICILIA",
    CAP >= 95121 & CAP <= 95131 ~ "SICILIA",
    CAP >= 94010 & CAP <= 94100 ~ "SICILIA",
    CAP >= 98020 & CAP <= 98079 ~ "SICILIA",
    CAP >= 98121 & CAP <= 98168 ~ "SICILIA",
    CAP >= 90010 & CAP <= 90051 ~ "SICILIA",
    CAP >= 90121 & CAP <= 90151 ~ "SICILIA",
    CAP >= 97010 & CAP <= 97100 ~ "SICILIA",
    CAP >= 96010 & CAP <= 96100 ~ "SICILIA",
    CAP >= 91010 & CAP <= 91100 ~ "SICILIA",
    CAP >= 52010 & CAP <= 52100 ~ "TOSCANA",
    CAP >= 50012 & CAP <= 50068 ~ "TOSCANA",
    CAP >= 50121 & CAP <= 50145 ~ "TOSCANA",
    CAP >= 58010 & CAP <= 58100 ~ "TOSCANA",
    CAP >= 57014 & CAP <= 57038 ~ "TOSCANA",
    CAP >= 57121 & CAP <= 57128 ~ "TOSCANA",
    CAP >= 55011 & CAP <= 55100 ~ "TOSCANA",
    CAP >= 54010 & CAP <= 54100 ~ "TOSCANA",
    CAP >= 56010 & CAP <= 56048 ~ "TOSCANA",
    CAP >= 56121 & CAP <= 56128 ~ "TOSCANA",
    CAP >= 51010 & CAP <= 51100 ~ "TOSCANA",
    CAP >= 59013 & CAP <= 59100 ~ "TOSCANA",
    CAP >= 53011 & CAP <= 53100 ~ "TOSCANA",
    CAP >= 39010 & CAP <= 39100 ~ "TRENTINO ALTO ADIGE",
    CAP >= 38010 & CAP <= 38096 ~ "TRENTINO ALTO ADIGE",
    CAP >= 38121 & CAP <= 38123 ~ "TRENTINO ALTO ADIGE",
    CAP >= 06010 & CAP <= 06089 ~ "UMBRIA",
    CAP >= 06121 & CAP <= 06135 ~ "UMBRIA",
    CAP >= 05010 & CAP <= 05100 ~ "UMBRIA",
    CAP >= 11010 & CAP <= 11100 ~ "VALLE D'AOSTA",
    CAP >= 32010 & CAP <= 32100 ~ "VENETO",
    CAP >= 35010 & CAP <= 35048 ~ "VENETO",
    CAP >= 35121 & CAP <= 35143 ~ "VENETO",
    CAP >= 45010 & CAP <= 45100 ~ "VENETO",
    CAP >= 31010 & CAP <= 31100 ~ "VENETO",
    CAP >= 30010 & CAP <= 30039 ~ "VENETO",
    CAP >= 30121 & CAP <= 30176 ~ "VENETO",
    CAP >= 37121 & CAP <= 37142 ~ "VENETO",
    CAP >= 36010 & CAP <= 36100 ~ "VENETO",
    CAP == 18025 ~ "PIEMONTE",
    CAP == 12071 ~ "LIGURIA"
  )) %>%
  #se ci sono NA su REGN, inserisco il valore di REGION se presente. 
  mutate(REGN = if_else(is.na(REGN), REGION, REGN)) %>%
  select("ID_ADDRESS", "CAP", "PRV", "REGN") %>%
  rename(REGION = REGN)
  
head(df_3_cli_address_clean)

# Trasformo le variabili numeriche in factor (PRV, REGION)
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(PRV = as.factor(PRV)) %>%
  mutate(REGION = as.factor(REGION)) %>%
  distinct()

# Controllo quanti clienti contengono le variabili w_CAP, w_PRV e w_REGION. Nel caso in cui non 
# dovessero contenere uno dei campi vengono eliminati. 
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP), w_PRV = !is.na(PRV), w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

# Esplorazione della variabile PRV
df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=PRV)) +
  geom_bar(width = 0.05, fill = 'darkblue', color = 'black') +
  coord_flip() +
  labs(title = "Province") +
  theme_bw()

# Esplorazione della variabile REGION
df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))

ggplot(df_3_cli_address_clean, aes(x=REGION))  +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  coord_flip() +
  labs(title = "Regioni") +
  theme_bw()

# Summary df_3_cli_address_clean
str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)
head(df_3_cli_address_clean)
