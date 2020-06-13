library(dplyr)
set.seed(12345)

############## PREPARAZIONE DATI ##############
# I dataframe considerati per la valutazione dei churn sono df_7_tic
# e df_1_cli. All'interno dei quali, rispettivamente, sono contenuti
# i dati degli scontrini e dei clienti.
scontrino <- df_7_tic_clean
client <- df_1_cli_fid_clean %>%
  select(c(ID_CLI, COD_FID, STATUS_FID, DT_ACTIVE))

#### AGGIUNTA DATI CLIENTE ####
# unione dati scontrino e clienti
scontrino_w_cliente <- scontrino %>%
  left_join(client, by = "ID_CLI")

#### CALCOLO QUANTITA PRODOTTI PER ACQUISTO E RISPETTIVI DATI ####
# La colonna id_scontrino non è univoca, dunque, ogni scontrino è ripetuto più
# volte ed ogni apparizione rappresenta un prodotto acquistato quel giorno da quel
# determinato cliete. Di seguito è stata effettuata una group by sugli scontrini
# calcolando per ognuno di essi l'importo totale, la quantità di articoli totali ecc..
df_clienti <- scontrino_w_cliente %>%
  mutate(IMPORTO_NETTO = IMPORTO_LORDO - SCONTO) %>% # calcolo impoto netto
  group_by(ID_SCONTRINO) %>% # group by per scontrino (per ogni articolo acquistato  viene ripetuto ripetuto lo stesso scontrino)
  mutate(QUANTITA_ARTICOLI = n()) %>% # calcolo la quantità di articoli per ogni scontrino
  mutate(IMPORTO_NETTO = sum(IMPORTO_NETTO)) %>% # calcolo l'importo netto per ogni scontrino
  mutate(IMPORTO_LORDO = sum(IMPORTO_LORDO)) %>% # calcolo l'importo lordo per ogni scontrino
  mutate(SCONTO = sum(SCONTO)) %>% # calcolo lo sconto per ogni scontrino
  distinct(ID_SCONTRINO, .keep_all = TRUE) %>% # rimuovo le ripetizioni degli scontrini
  select(-c(ID_ARTICOLO, COD_REPARTO)) %>% # rimuovo le colonne inutilizzate
  arrange(ID_CLI, DATETIME) %>%
  as.data.frame()

#### OPERAZIONI SU CLIENTI ####
# E' stata effettuata una group by sui clienti per clacolare tutti i loro acquisti
# in modo tale da avere una visione generale della loro storia. Sono stati calcolati
# il numero totale di acquisti, l'importo totale speso ma in particolare
# sono state calcolate l'ultima data di acquisto, il maggio tempo
# per il quale non ha effettuato acquisti ed il tempo minimo.
df_clienti <- df_clienti %>%
  group_by(ID_CLI) %>% # group by per cliente
  mutate(nTRANSAZIONI = n()) %>% # calcolo il numero di transazioni per ogni cliente 
  mutate(nACQUISTI = sum(DIREZIONE == 1)) %>% # numero di acquiesti
  mutate(nRESI = sum(DIREZIONE == -1)) %>% # numero di resti
  mutate(ULTIMA_DIREZIONE = DIREZIONE[row_number()==n()]) %>% # ultima trasazione: è un acquisto o un reso?
  mutate(IMPORTO_NETTO_TOT = sum(abs(IMPORTO_NETTO))) %>% # importo netto tot (tiene in cosiderazione anche i resi in valore assoluto)
  mutate(IMPORTO_LORDO_TOT = sum(abs(IMPORTO_LORDO))) %>% # importo lordo tot (tiene in cosiderazione anche i resi in valore assoluto)
  mutate(SCONTO_TOT = sum(abs(SCONTO))) %>% # sconto tot (tiene in cosiderazione anche i resi in valore assoluto)
  mutate(SPESA_MEDIA = IMPORTO_NETTO_TOT/nTRANSAZIONI) %>% # spesa media
  mutate(QUANTITA_ARTICOLI_TOT = sum(abs(QUANTITA_ARTICOLI))) %>% # # quantità articoli tot (tiene in cosiderazione anche i resi in valore assoluto)
  mutate(QUANTITA_ARTICOLI_MEDI = QUANTITA_ARTICOLI_TOT/nTRANSAZIONI) %>% # quantità articoli medi
  mutate(LAST_DATE = DATETIME[row_number()==n()]) %>%# calcolo l'ultima data di acquiasto per ogni cliente
  mutate(DIFF_ULTIMO_ACQ = DATETIME - lag(DATETIME, default = DATETIME[1])) %>% # calcolo giorni passati rispetto alla data di acquisto precedente
  mutate(MAX_DIFF_ULTIMO_ACQ = as.numeric(max(DIFF_ULTIMO_ACQ))) %>% # calcolo la massima differenza di giorni tra un acquisto e quello successivo
  mutate(MIN_DIFF_ULTIMO_ACQ = case_when(
    nTRANSAZIONI == 0 ~ as.numeric('0'),
    nTRANSAZIONI == 1 | nTRANSAZIONI == 2 ~ as.numeric(max(DIFF_ULTIMO_ACQ)),
    length(unique(DATETIME)) == 1 ~ as.numeric(max(DIFF_ULTIMO_ACQ)),
    TRUE ~ as.numeric(min(DIFF_ULTIMO_ACQ[DIFF_ULTIMO_ACQ > 0]))
  )) %>% # calcolo la minima differenza di giorni tra un acuisto e quello successivo
  mutate(DIFF_ULTIMO_ACQ = as.numeric(as.Date("2019-04-30") - LAST_DATE)) %>% # calcolo da quanti giorni non ha effettuato acquisti ripetto all'ultima data presente nel DB 
  distinct(ID_CLI, .keep_all = TRUE) %>%
  select(-c(ID_NEG, DATETIME, DIREZIONE,IMPORTO_LORDO, SCONTO, IMPORTO_NETTO, QUANTITA_ARTICOLI)) %>% # rimozione variabili non più utilizzate
  as.data.frame()

str(df_clienti) # I clienti risultano essere 212124

#### CLIENTI SENZA ACQUISTI ####
# In df_1_cli_fid sono presenti più clienti che rispetto a quelli riportati
# sugli scontrini. Si è supposto che questi ultimi siano clienti fidelizzati che, però,
# non hanno mai effettuato acquisti. Di conseguenza sono stati isolati per essere aggiunti
# al DF finale.
df_non_clienti <- df_1_cli_fid_clean %>%
  left_join(df_clienti, by="ID_CLI")
df_non_clienti <- subset(df_non_clienti, is.na(df_non_clienti$ID_SCONTRINO))
df_non_clienti <- df_non_clienti %>% 
  rename(STATUS_FID = STATUS_FID.x) %>%
  rename(DT_ACTIVE = DT_ACTIVE.x) %>%
  rename(COD_FID = COD_FID.x) %>%
  select(ID_CLI, STATUS_FID, COD_FID, DT_ACTIVE) %>%
  as.data.frame()

str(df_non_clienti)
# I non clienti risultano essere 157348
# Poiche i non clienti avranno tutte i campi impostati a 0
# si è deciso di tenere solo un piccolo campione di questi ultimi in quanto
# la grande mole di dati con tutti i campi a 0 potrebbe falsificare l'efficienza 
# dei modelli che verranno successivamente utilizzati.
df_non_clienti <- sample_n(df_non_clienti, size = 250)

# Aggiunta clienti non in scontrino
df_churn_def <- bind_rows(df_clienti, df_non_clienti)

# Ci sono clienti che hanno attivato l'account prima dell'ultima data presente 
# negli scontrini. Poiché questa data è stata considerata come data attuale
# questi ultimi sono stati considerati come errori nel DF e di conseguenza rimossi
df_churn_def <- subset(df_churn_def, DT_ACTIVE <= as.Date("2019-04-30"))

# Sostituzione valori NA per i clienti senza acquisti
df_churn_def <- df_churn_def %>%
  mutate(nTRANSAZIONI = replace(nTRANSAZIONI, is.na(nTRANSAZIONI), 0)) %>%
  mutate(nACQUISTI = replace(nACQUISTI, is.na(nACQUISTI), 0)) %>%
  mutate(nRESI = replace(nRESI, is.na(nRESI), 0)) %>%
  mutate(ULTIMA_DIREZIONE = as.character(ULTIMA_DIREZIONE)) %>%
  mutate(ULTIMA_DIREZIONE = replace(ULTIMA_DIREZIONE, is.na(ULTIMA_DIREZIONE), as.character(0))) %>%
  mutate(IMPORTO_NETTO_TOT = replace(IMPORTO_NETTO_TOT, is.na(IMPORTO_NETTO_TOT), 0)) %>%
  mutate(IMPORTO_LORDO_TOT = replace(IMPORTO_LORDO_TOT, is.na(IMPORTO_LORDO_TOT), 0)) %>%
  mutate(SCONTO_TOT = replace(SCONTO_TOT, is.na(SCONTO_TOT), 0)) %>%
  mutate(SPESA_MEDIA = replace(SPESA_MEDIA, is.na(SPESA_MEDIA), 0)) %>%
  mutate(QUANTITA_ARTICOLI_TOT = replace(QUANTITA_ARTICOLI_TOT, is.na(QUANTITA_ARTICOLI_TOT), 0)) %>%
  mutate(QUANTITA_ARTICOLI_MEDI = replace(QUANTITA_ARTICOLI_MEDI, is.na(QUANTITA_ARTICOLI_MEDI), 0)) %>%
  mutate(LAST_DATE = replace(LAST_DATE, is.na(LAST_DATE), DT_ACTIVE)) %>%
  mutate(MAX_DIFF_ULTIMO_ACQ = replace(MAX_DIFF_ULTIMO_ACQ, is.na(MAX_DIFF_ULTIMO_ACQ), as.numeric(as.Date("2019-04-30") - DT_ACTIVE))) %>%
  mutate(MIN_DIFF_ULTIMO_ACQ = replace(MIN_DIFF_ULTIMO_ACQ, is.na(MIN_DIFF_ULTIMO_ACQ), as.numeric(as.Date("2019-04-30") - DT_ACTIVE))) %>%
  mutate(DIFF_ULTIMO_ACQ = replace(DIFF_ULTIMO_ACQ, is.na(DIFF_ULTIMO_ACQ), as.numeric(as.Date("2019-04-30") - DT_ACTIVE))) %>%
  select(-c(ID_SCONTRINO)) %>%
  as.data.frame()

# Rimozione clienti con data di attivazione maggiore dell'ultima data di acquisto (considerato come errore)
df_churn_def <- subset(df_churn_def, DT_ACTIVE <= LAST_DATE)


#### ANALISI DATI ####
library(ggplot2)
# Plot della media dei giorni senza acquisti per le diverse categorie di clienti 
fid_plot <- aggregate(df_churn_def$MAX_DIFF_ULTIMO_ACQ, by=list(df_churn_def$COD_FID), FUN=mean)  # calcolo media
colnames(fid_plot) <- c("COD_FID", "MAX_DIFF")  # change column names
fid_plot <- fid_plot[order(fid_plot$MAX_DIFF), ]  # sort
fid_plot$COD_FID <- factor(fid_plot$COD_FID, levels = fid_plot$COD_FID)  # to retain the order in plot.

ggplot(fid_plot, aes(x=COD_FID, y=MAX_DIFF)) + 
  geom_point(col="darkblue", size=3) +
  geom_segment(aes(x=COD_FID, 
                   xend=COD_FID, 
                   y=min(MAX_DIFF), 
                   yend=max(MAX_DIFF)), 
               linetype="dashed", 
               size=0.1) +
  labs(title="Media dei giori massimi senza acquisti per categoria di cliente") +
  coord_flip()

# Plot media numero resi, media numero acquisti, media numero transazioni  
fid_plot <- as.data.frame(matrix(data = c("Media resi", "Media acquisti",
                                          mean(df_churn_def$nRESI),
                                          mean(df_churn_def$nACQUISTI)), ncol = 2, nrow = 2))
colnames(fid_plot) <- c("name", "value")
fid_plot <- fid_plot[order(fid_plot$value), ]  # sort
fid_plot$name <- factor(fid_plot$name, levels = fid_plot$name)  # to retain the order in plot.

theme_set(theme_bw())
ggplot(fid_plot, aes(x=name, y= value)) + 
  geom_bar(stat="identity", width=.5, fill="darkblue") + 
  labs(title="Media dei giori massimi senza acquisti per categoria di cliente") + 
  theme(axis.text.x = element_text(angle=55, vjust=0.6))

# distribuzione dei giorni da cui i clienti non acquistano
ggplot(df_churn_def %>%
         group_by(DIFF_ULTIMO_ACQ) %>%
         summarise(TOT_EVENTs = n_distinct(ID_CLI)) %>%
         arrange(DIFF_ULTIMO_ACQ) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))
       , aes(y=PERCENT_COVERED, x=DIFF_ULTIMO_ACQ)) +
  geom_line(color = "darkblue") + 
  geom_point(color = "darkblue") + 
  #scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
  theme_bw()

# CALCOLO SUBSET PER OGNI TIPO DI FIDELIZZAZIONE
# I clienti risultano essere divisi in quattro diverse categorie:
# Business 
# Business Premium
# Standard
# Standard Premium
# Viene calcolata la media tra i giorni massimi per ogni categoria in cui i clienti non
# hanno effettuato acquisti in modo tale da utilizzare quest'ultima come limite per
# considerare un cliente churn
cli_business_premium <- subset(df_churn_def, COD_FID == "PREMIUM BIZ")
mean_cli_business_premium <- as.numeric(mean(cli_business_premium$MAX_DIFF_ULTIMO_ACQ))
cli_business_standard <- subset(df_churn_def, COD_FID == "STANDARD BIZ")
mean_cli_business_standard <- as.numeric(mean(cli_business_standard$MAX_DIFF_ULTIMO_ACQ))
cli_premium <- subset(df_churn_def, COD_FID == "PREMIUM")
mean_cli_premium <- as.numeric(mean(cli_premium$MAX_DIFF_ULTIMO_ACQ))
cli_standard <- subset(df_churn_def, COD_FID == "STANDARD")
mean_cli_standard <- as.numeric(mean(cli_standard$MAX_DIFF_ULTIMO_ACQ))


ultimo_giorno <- as.Date("2019-04-30") # ultimo giorno presente nel dataset (considerato come giorno ultimo)

#### CREAZIONE COLONNA CHURN ####
# Di seguito tutti i casi in cui un cliente viene considerato CHURN
df_churn_def <- df_churn_def %>%
  mutate(CHURN = case_when(
    # stato fidelizzazione = -1 (cliente non più fidelizzato)
    STATUS_FID == -1 ~ 1,
    #  stato fid = 0 (mai effettuato acquisti) ed il tempo dal quale sono clienti è maggiore della media di acquisto
    STATUS_FID == 0 & COD_FID == "PREMIUM BIZ" & (ultimo_giorno - LAST_DATE) > mean_cli_business_premium ~ 1,
    STATUS_FID == 0 & COD_FID == "STANDARD BIZ" & (ultimo_giorno - LAST_DATE) > mean_cli_business_standard ~ 1,
    STATUS_FID == 0 & COD_FID == "PREMIUM" & (ultimo_giorno - LAST_DATE) > mean_cli_premium ~ 1,
    STATUS_FID == 0 & COD_FID == "STANDARD" & (ultimo_giorno - LAST_DATE) > mean_cli_standard ~ 1,
    
    # non vengono effettuati acquisti da più di sei mesi
    ultimo_giorno - LAST_DATE > 180 ~ 1,
    
    # più dei due terzi delle transazioni sono resi, tra cui l'ultimo acquisto
    nRESI/nTRANSAZIONI >= 2/3 & ULTIMA_DIREZIONE == -1 ~ 1,
    
    # num_transazioni > 4 , Max_Day senza acquisti > 180, il tempo da cui non acquista
    # è maggiore di ((Max + min)/2 + media_categoria) / 2
    COD_FID == "PREMIUM BIZ" & nTRANSAZIONI >= 4 & MAX_DIFF_ULTIMO_ACQ >= 180 &
      ultimo_giorno - LAST_DATE > ((MAX_DIFF_ULTIMO_ACQ + MIN_DIFF_ULTIMO_ACQ)/2 + mean_cli_business_premium)/2 ~ 1,
    COD_FID == "STANDARD BIZ" & nTRANSAZIONI >= 4 & MAX_DIFF_ULTIMO_ACQ >= 180 &
      ultimo_giorno - LAST_DATE > ((MAX_DIFF_ULTIMO_ACQ + MIN_DIFF_ULTIMO_ACQ)/2 + mean_cli_business_standard)/2 ~ 1,
    COD_FID == "PREMIUM" & nTRANSAZIONI >= 4 & MAX_DIFF_ULTIMO_ACQ >= 180 &
      ultimo_giorno - LAST_DATE > ((MAX_DIFF_ULTIMO_ACQ + MIN_DIFF_ULTIMO_ACQ)/2 + mean_cli_premium)/2 ~ 1,
    COD_FID == "STANDARD" & nTRANSAZIONI >= 4 & MAX_DIFF_ULTIMO_ACQ >= 180 &
      ultimo_giorno - LAST_DATE > ((MAX_DIFF_ULTIMO_ACQ + MIN_DIFF_ULTIMO_ACQ)/2 + mean_cli_standard)/2 ~ 1,
    
    # num_transzioni > 4 , Max_Day senza acquisti < 180, il tempo da cui non acquista
    # è maggiore di ((Max + media_categoria) / 2
    COD_FID == "PREMIUM BIZ" & nTRANSAZIONI >= 4 & 
      ultimo_giorno - LAST_DATE > (MAX_DIFF_ULTIMO_ACQ + mean_cli_business_premium)/2 ~ 1,
    COD_FID == "STANDARD BIZ" & nTRANSAZIONI >= 4 & 
      ultimo_giorno - LAST_DATE > (MAX_DIFF_ULTIMO_ACQ + mean_cli_business_standard)/2 ~ 1,
    COD_FID == "PREMIUM" & nTRANSAZIONI >= 4 & 
      ultimo_giorno - LAST_DATE > (MAX_DIFF_ULTIMO_ACQ + mean_cli_premium)/2 ~ 1,
    COD_FID == "STANDARD" & nTRANSAZIONI >= 4 & 
      ultimo_giorno - LAST_DATE > (MAX_DIFF_ULTIMO_ACQ + mean_cli_standard)/2 ~ 1,
    
    # num_transzioni < 4 , il tempo da cui non acquista
    # è maggiore della media tipo cliente
    COD_FID == "PREMIUM BIZ" & nTRANSAZIONI < 4 &
      ultimo_giorno - LAST_DATE > mean_cli_business_premium ~ 1,
    COD_FID == "STANDARD BIZ" & nTRANSAZIONI < 4 &
      ultimo_giorno - LAST_DATE > mean_cli_business_standard ~ 1,
    COD_FID == "PREMIUM" & nTRANSAZIONI < 4 &
      ultimo_giorno - LAST_DATE > mean_cli_premium ~ 1,
    COD_FID == "STANDARD" & nTRANSAZIONI < 4 &
      ultimo_giorno - LAST_DATE > mean_cli_standard ~ 1,
    
    TRUE ~ 0
  
  ))  %>%
  as.data.frame()
