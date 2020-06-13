### df_4_cli_privacy ###

# Summary df_4_cli_privacy
str(df_4_cli_privacy)
summary(df_4_cli_privacy)

# Dataset df_4_cli_privacy_clean
df_4_cli_privacy_clean <- df_4_cli_privacy

head(df_4_cli_privacy_clean)

# Trasformazione delle variabili boolean in factor (FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT)
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

# Esplorazione della variabile FLAG_PRIVACY_1 (1, 0)
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_1)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Privacy") +
  theme_bw()

# Esplorazione della variabile FLAG_PRIVACY_2 (1, 0)
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_PRIVACY_2)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Privacy 2") +
  theme_bw()

# Esplorazione della variabile FLAG_DIRECT_MKT (1, 0)
df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_4_cli_privacy_clean, aes(x=FLAG_DIRECT_MKT)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Direct MKT") +
  theme_bw()

# Controllo quanti clienti contengono le variabili FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT
df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1, FLAG_PRIVACY_2, FLAG_DIRECT_MKT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# Summary df_4_cli_privacy_clean
str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)
head(df_4_cli_privacy_clean)
