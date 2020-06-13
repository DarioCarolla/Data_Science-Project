### df_7_tic ###

# Summary df_7_tic
str(df_7_tic)
summary(df_7_tic)
head(df_7_tic)

# Formattazione della variabile DATETIME in formato data e trasformazione delle variabili numeriche in factor
# (COD_REPARTO, DIREZIONE)
df_7_tic_clean <- df_7_tic %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO)) %>%
  mutate(DIREZIONE = as.factor(DIREZIONE)) %>%
  mutate(DATETIME = as.Date(DATETIME, format="%Y-%m-%dT%H%M%S"))

# Esplorazione della variabile COD_REPARTO (da 1 a 14)
df_7_tic_clean %>%
  group_by(COD_REPARTO) %>%
  summarise(TOT_IMPORTO = n_distinct(IMPORTO_LORDO))

ggplot(df_7_tic_clean, aes(x=COD_REPARTO)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Reparti") +
  theme_bw()

ggplot(df_7_tic_clean, aes(x=DIREZIONE)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Acquistati (1) e romborsati (-1)") +
  theme_bw()
