### df_1_cli_fid ###

# Summary df_1_cli_fid 
str(df_1_cli_fid)
summary(df_1_cli_fid)

# Dataset df_1_cli_fid_clean 
df_1_cli_fid_clean <- df_1_cli_fid

# Formattazione della variabile DT_ACTIVE in formato data
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

# Trasformo le variabili numeriche in factor (ID_NEG, TYP_CLI_FID, STATUS_FID)
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(ID_NEG = as.factor(ID_NEG)) %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

# Alcuni clienti risultano avere più date di attivazione, si mantiene la prima (ormai disattivata)
# e l'ultima. Si crea quindi un dataframe (df_1_cli_fid_clean) contenente entrambe le date. 
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID), NUM_DATEs = n_distinct(DT_ACTIVE))

dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI))

dist_num_fid_x_cli 

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI, FIRST_ID_NEG = ID_NEG, FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI, NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

# Summary df_1_cli_fid_clean
str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)
head(df_1_cli_fid_clean)

# Esplorazione della variabile COD_FID (STANDARD, PREMIUM, STANDARD BIZ, PREMIUM BIZ)
df_1_cli_fid_clean %>%
  group_by(COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=COD_FID)) + 
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Programmi fidelty") +
  theme_bw()

# Esplorazione della variabile TYP_CLI_FID (1, 0)
df_1_cli_fid_clean %>%
  group_by(TYP_CLI_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=TYP_CLI_FID)) + geom_bar()

# Esplorazione della variabile STATUS_FID (1, 0)
df_1_cli_fid_clean %>%
  group_by(STATUS_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=STATUS_FID)) +
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Stato fidelizzazione") +
  theme_bw()

# Esplorazione della variabile ID_NEG (da 1 a 50)
df_1_cli_fid_clean %>%
  group_by(ID_NEG) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))  %>%
  arrange(desc(PERCENT))

ggplot(df_1_cli_fid_clean, aes(x=ID_NEG)) +
  geom_bar(fill = 'darkblue', color = 'black') +
  labs(title = "Negozio di riferimento") +
  theme_bw()
