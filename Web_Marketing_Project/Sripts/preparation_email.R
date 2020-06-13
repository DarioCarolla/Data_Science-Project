# L'obiettivo è preparare i dati per il modello

# explore the distribution

# JOIN tra evento email (DF6) e tipo di campagna (DF5)
df_6_camp_event_w_type <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

# vengono isolate le email inviate
df_sents <- df_6_camp_event_w_type %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE)

# vengono isolate le email aperte
df_opens <- df_6_camp_event_w_type %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, OPEN_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(OPEN_DATE == min(OPEN_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# vengono isolate le email con click
df_clicks <- df_6_camp_event_w_type %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, CLICK_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(CLICK_DATE == min(CLICK_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# vengono isolate le email fallite
df_fails <- df_6_camp_event_w_type %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATE == min(FAIL_DATE)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# join tra email inviate ed aperte
# email in cui è presente open date sono state aperte
# se open date -> NA la mail non è stata aperta
df_sents_w_open <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>% 
  mutate(DIFF = as.integer(OPEN_DATE - SEND_DATE))

# numero di email inviate ma non aperte
df_sents_w_open %>%
  group_by(w_open = !is.na(DIFF)) %>%
  summarise(TOT_SENTs = n_distinct(ID_EVENT_S)) %>%
  mutate(PERCENT = TOT_SENTs/sum(TOT_SENTs)) %>%
  arrange(desc(PERCENT))

ggplot(df_sents_w_open, aes(x=!is.na(DIFF))) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "True -> numero di mail aperte") +
  theme_bw()

# distribuzione giorni apertura email
df_sents_w_open %>% filter(!is.na(DIFF)) %>%
  group_by(DIFF) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
  arrange(DIFF) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs))

ggplot(df_sents_w_open %>% filter(!is.na(DIFF)) %>%
         group_by(DIFF) %>%
         summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)) %>%
         arrange(DIFF) %>%
         mutate(PERCENT_COVERED = cumsum(TOT_EVENTs)/sum(TOT_EVENTs)) %>%
         filter(DIFF <= 14)
       , aes(y=PERCENT_COVERED, x=DIFF)) + geom_line() + geom_point() + scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14)

# si sceglie come finestra tre giorni supponendo che ogni campagna
# duri circa due settimane. Potendo analizzare anche quelle che durano
# da tre giorni in poi
window_days <- 3

### costruzione datamart ###

# la variabile target sarà se un evento di invio che
# è stato aperto entro il periodo di tempo della finestra (3 giorni)
# TARGET -> 1 se la mail è stata aperta entro tre giorni
# 0 altrimenti
target_event <- df_sents_w_open %>%
  mutate(TARGET = as.factor(if_else(!is.na(DIFF) & DIFF <= window_days, "1", "0"))) %>%
  select(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE, TARGET)

# alcune variabili rilevanti che si volgiono includere sono:
# - tasso medio apertura (entro 14 giorni) delle comunicazioni 
#   ricevute dal cliente nei 30 giorni precedenti l'invio
# - tasso medio di click-through (entro 14 giorni) delle 
#   comunicazioni ricevute dal cliente nei 30 giorni precedenti l'invio

# per avere una situazione analoga che si sta considerando:
# - Invio mirato effettuato dopo il 2019-02-01 e giorni della finestra prima del 2019-04-30
# - indirizzato a clienti registrati da almeno 30 giorni

rate_window <- 14
prev_window <- 30

dt_start <- as.Date("2019-02-01")
dt_end <- as.Date("2019-04-30") - window_days # perché se la mail è stata mandata
# l'ultimo giorno registrato non posso considerare il tasso di mail aperte
# ENTRO TRE GIORNI


# relevant_event rappresenta tutte le mail (inviate, aperte, click e fallite)
# la cui DIFF_EVENT è minore di 14 giorni
relevant_event <- df_sents %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")) %>%
  filter(is.na(CLICK_DATE) | SEND_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(DIFF_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  mutate(DIFF_CLICK = as.integer(CLICK_DATE - SEND_DATE)) %>%
  filter(is.na(DIFF_OPEN) | DIFF_OPEN < rate_window) %>%
  filter(is.na(DIFF_CLICK) | DIFF_CLICK < rate_window)

names(relevant_event) <- sapply(names(relevant_event), paste0, "_PREV")

target_event_w_prev <- target_event %>% filter(SEND_DATE >= dt_start & SEND_DATE <= dt_end) %>%
  left_join(relevant_event
            , by = c("ID_CLI" = "ID_CLI_PREV")
  ) %>%
  filter(is.na(SEND_DATE_PREV) | (SEND_DATE_PREV < SEND_DATE & SEND_DATE <= SEND_DATE_PREV + prev_window)) %>%
  mutate(OPENED = if_else(OPEN_DATE_PREV <= SEND_DATE & SEND_DATE <= OPEN_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(CLICKED = if_else(CLICK_DATE_PREV <= SEND_DATE & SEND_DATE <= CLICK_DATE_PREV + prev_window, 1, 0)) %>%
  mutate(FAILED = if_else(!is.na(ID_EVENT_F_PREV), 1, 0)) %>%
  group_by(ID_EVENT_S, ID_CLI, ID_CAMP, ID_DELIVERY, SEND_DATE,  TARGET) %>%
  summarise(NUM_SEND_PREV = n_distinct(ID_EVENT_S_PREV, na.rm = T)
            , NUM_OPEN_PREV = sum(OPENED, na.rm = T)
            , NUM_CLICK_PREV = sum(CLICKED, na.rm = T)
            , NUM_FAIL_PREV = sum(FAILED, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(OPEN_RATE_PREV = NUM_OPEN_PREV/NUM_SEND_PREV) %>%
  mutate(CLICK_RATE_PREV = NUM_CLICK_PREV/NUM_OPEN_PREV) %>%
  mutate(W_SEND_PREV = as.factor(NUM_SEND_PREV > 0)) %>% # in binario se l'utente ha ricevuto almeno una mail nell'ultimo mese
  mutate(W_FAIL_PREV = as.factor(NUM_FAIL_PREV > 0)) %>% # in binario se è fallita almeno una mail nell'ultimo mese
  mutate(SEND_WEEKDAY = as.factor(weekdays(SEND_DATE))) %>% # giorno della settimana attuale
  mutate(OPEN_RATE_PREV = if_else(is.na(OPEN_RATE_PREV), 0, OPEN_RATE_PREV)) %>%
  mutate(CLICK_RATE_PREV = if_else(is.na(CLICK_RATE_PREV), 0, CLICK_RATE_PREV))

# aggiunta dati clienti
df_master <- target_event_w_prev %>%
  left_join(df_1_cli_fid_clean %>%
              select(ID_CLI, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, FIRST_DT_ACTIVE, NUM_FIDs)
            , by = "ID_CLI") %>%
  filter(FIRST_DT_ACTIVE <= SEND_DATE) %>%
  # filter(FIRST_DT_ACTIVE <= SEND_DATE - 30) %>%
  mutate(AGE_FID = as.integer(SEND_DATE - FIRST_DT_ACTIVE)) %>%
  left_join(df_2_cli_account_clean
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, PRV, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP") %>%
  mutate(PRV = fct_explicit_na(PRV)) %>%
  mutate(REGION = fct_explicit_na(REGION)) %>%
  select(-ID_ADDRESS, -ID_CAMP, -ID_DELIVERY, -SEND_DATE, -FIRST_DT_ACTIVE)

# controllo duplicati
df_master %>%
  group_by(ID_EVENT_S) %>% 
  summarise(num = n()) %>% 
  group_by(num) %>%
  count()

#### DATA ESPLORATION ####

# lets see the frequency of the event
df_master %>%
  group_by(TARGET) %>%
  summarise(NUM_EVENTs = n_distinct(ID_EVENT_S))

df_master %>%
  group_by(TARGET,  W_SEND_PREV) %>%
  summarise(NUM_EVENTs = n_distinct(ID_EVENT_S), mean_OR = mean(OPEN_RATE_PREV, na.rm = T))

str(df_master)
summary(df_master)

### T-TEST ###
# check if a continuous variable has a significative difference
pander(t.test(NUM_SEND_PREV ~ TARGET, data = df_master))
pander(t.test(NUM_OPEN_PREV ~ TARGET, data = df_master))
pander(t.test(NUM_CLICK_PREV ~ TARGET, data = df_master))
pander(t.test(NUM_FAIL_PREV ~ TARGET, data = df_master))
pander(t.test(OPEN_RATE_PREV ~ TARGET, data = df_master))
pander(t.test(CLICK_RATE_PREV ~ TARGET, data = df_master))
pander(t.test(AGE_FID ~ TARGET, data = df_master))


library(tidyr) # needed for the pivot function spread()

prepare_chisq <- function(df, x){
  y <- enquo(x)
  
  
  test_df <- df %>%
    mutate(KEY = if_else(TARGET == "1", "OK", "KO")) %>%
    select(UQ(y), KEY, ID_EVENT_S) %>%
    group_by(UQ(y), KEY) %>%
    summarise(n = n()) %>%
    spread(KEY, n) %>%
    ungroup() %>%
    as.data.frame()
  
  test_m <- test_df %>%
    select(OK, KO) %>%
    mutate(OK = if_else(is.na(OK), as.integer(0), OK)) %>%
    mutate(KO = if_else(is.na(KO), as.integer(0), KO)) %>%
    as.matrix() 
  row.names(test_m) <- as.character(test_df[,1])
  
  return(test_m)
}

plot_factor <- function(df, x, lab){
  y <- enquo(x)
  
  df_count_tot <- df %>%
    group_by(UQ(y)) %>%
    summarise(n_tot = n_distinct(ID_EVENT_S)) %>%
    ungroup()
  
  df_count <- df %>%
    group_by(UQ(y), TARGET) %>%
    summarise(n = n_distinct(ID_EVENT_S))
  
  df <- df_count %>%
    left_join(df_count_tot, by = lab) %>%
    mutate(frac = round(n / n_tot, 2))
  
  ggplot(data=df, aes(x=UQ(y), y=frac, fill=TARGET)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(x=UQ(y), y=frac, label = frac),
              position = position_dodge(width = 1),
              vjust = 2, size = 3, color = "white", fontface = "bold")
}


chisq.test(prepare_chisq(df_master, W_SEND_PREV))
plot_factor(df_master, W_SEND_PREV, "W_SEND_PREV")

chisq.test(prepare_chisq(df_master, W_FAIL_PREV))
plot_factor(df_master, W_FAIL_PREV, "W_FAIL_PREV")

chisq.test(prepare_chisq(df_master, SEND_WEEKDAY))
plot_factor(df_master, SEND_WEEKDAY, "SEND_WEEKDAY")

chisq.test(prepare_chisq(df_master, ID_NEG))
plot_factor(df_master, ID_NEG, "ID_NEG")

chisq.test(prepare_chisq(df_master, TYP_CLI_FID))
plot_factor(df_master, TYP_CLI_FID, "TYP_CLI_FID")

chisq.test(prepare_chisq(df_master, COD_FID))
plot_factor(df_master, COD_FID, "COD_FID")

chisq.test(prepare_chisq(df_master, STATUS_FID))
plot_factor(df_master, STATUS_FID, "STATUS_FID")

chisq.test(prepare_chisq(df_master, NUM_FIDs))
plot_factor(df_master, NUM_FIDs, "NUM_FIDs")

chisq.test(prepare_chisq(df_master, W_PHONE))
plot_factor(df_master, W_PHONE, "W_PHONE")

chisq.test(prepare_chisq(df_master, TYP_JOB))
plot_factor(df_master, TYP_JOB, "TYP_JOB")

chisq.test(prepare_chisq(df_master, EMAIL_PROVIDER_CLEAN))
plot_factor(df_master, EMAIL_PROVIDER_CLEAN, "EMAIL_PROVIDER_CLEAN")

chisq.test(prepare_chisq(df_master, PRV))
plot_factor(df_master, PRV, "PRV")

chisq.test(prepare_chisq(df_master, REGION))
plot_factor(df_master, REGION, "REGION")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_1))
plot_factor(df_master, FLAG_PRIVACY_1, "FLAG_PRIVACY_1")

chisq.test(prepare_chisq(df_master, FLAG_PRIVACY_2))
plot_factor(df_master, FLAG_PRIVACY_2, "FLAG_PRIVACY_2")

chisq.test(prepare_chisq(df_master, FLAG_DIRECT_MKT))
plot_factor(df_master, FLAG_DIRECT_MKT, "FLAG_DIRECT_MKT")