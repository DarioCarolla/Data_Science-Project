### df_2_cli_account ###

# Summary df_2_cli_account
str(df_2_cli_account)
summary(df_2_cli_account)

# Dataset df_2_cli_account_clean
df_2_cli_account_clean <- df_2_cli_account

# Trasformo variabile booleana in factor (W_PHONE)
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

# Trasformo variabile numerica in factor (TYP_CLI_ACCOUNT)
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

# Agli NA presenti viene assegnato un valore numerico (0) o una stringa (missing).
# Le variabili considerate sono W_PHONE, EMAIL_PROVIDER, TYP_JOB
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0")) %>% 
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

# Esplorazione della variabile EMAIL_PROVIDER, per ogni email provider viene considerato il 
# numero di clienti 
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# Esplorazione della variabile W_PHONE (1, 0)
df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=W_PHONE)) +
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Numero di telefono aggiunto") +
  theme_bw()

# Esplorazione della variabile TYP_JOB, per ogni lavoro viene considerato il numero di clienti
df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# Il numero totale di lavori presenti è pari a 15 e in grande maggioranza assumono valore "missing"
df_2_cli_account_clean %>%
  summarise(TOT_TYP_JOB = n_distinct(TYP_JOB)) 

ggplot(df_2_cli_account_clean, aes(x=TYP_JOB)) +
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Lavoro cliente") +
  theme_bw()

# Summary df_2_cli_account_clean
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

# La variabile EMAIL_PROVIDER contiene circa 20.000 varibili diverse. Si decide quindi di mantenere le
# dieci variabili con il maggior numero di clienti e di inserire le rimanenti in un'unica variabile "other".
# La variabile "missing" viene trattata allo stesso modo delle altre

freq_email_providers <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

head(freq_email_providers, 20)

clean_email_providers <- freq_email_providers %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))

# Esplorazione della variabile EMAIL_PROVIDER 
df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_2_cli_account_clean, aes(x=EMAIL_PROVIDER_CLEAN)) +
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Email provider") +
  theme_bw()

# Summary df_2_cli_account_clean
str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)
