### df_6_camp_event ###

# Summary df_6_camp_event
str(df_6_camp_event)
summary(df_6_camp_event)
head(df_6_camp_event)

# Dataset df_6_camp_event_clean 
df_6_camp_event_clean <- df_6_camp_event

# Formattazione della variabile EVENT_DATE in formato data
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S"))

# TYP_EVENT contiene sia "ERRORI" che "BOUNCE". Si decide di unire tali categorie in un'unica nominata 
# "FAILURE". Successivamente la variabile TYP_EVENT viene trasformata in factor. 
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

# Esplorazione della variabile TYP_EVENT (S, V, C, F)
df_6_camp_event_clean %>%
  group_by(TYP_EVENT) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT), TOT_CLIs = n_distinct(ID_CLI), TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT_EVENT = TOT_EVENTs/sum(TOT_EVENTs), PERCENT_CLI = TOT_CLIs/sum(TOT_CLIs), PERCENT_CAMP = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT_EVENT), desc(PERCENT_EVENT), desc(PERCENT_CAMP))

ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_EVENT) %>% distinct(), aes(x=TYP_EVENT)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Direct MKT per eventi") +
  theme_bw()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CLI) %>% distinct(), aes(x=TYP_EVENT)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Direct MKT per clienti") +
  theme_bw()
ggplot(df_6_camp_event_clean %>% select(TYP_EVENT, ID_CAMP) %>% distinct(), aes(x=TYP_EVENT)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Direct MKT per campagne") +
  theme_bw()

# Prima data di EVENT_DATE e ultima data di EVENT_DATE
df_6_camp_event_clean %>% summarise(MIN_DATE = min(EVENT_DATE), MAX_DATE = max(EVENT_DATE))
