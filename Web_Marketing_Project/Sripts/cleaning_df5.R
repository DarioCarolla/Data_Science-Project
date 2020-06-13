### df_5_camp_cat ###

# Summary df_5_camp_cat
str(df_5_camp_cat)
summary(df_5_camp_cat)
head(df_5_camp_cat)

# Dataset df_5_camp_cat_clean
df_5_camp_cat_clean <- df_5_camp_cat

# La variabile CHANNEL_CAMP contiene un unico valore (EMAIL). Per questo motivo viene eliminata. 
df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

# Esplorazione della variabile TYP_CAMP (PRODUCT, PERSONALIZED, NATIONAL, NEWSLETTER, LOCAL)
df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarise(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>%
  arrange(desc(PERCENT))

ggplot(df_5_camp_cat_clean, aes(x=TYP_CAMP)) +
  geom_bar(width = 0.5, fill = 'darkblue', color = 'black') +
  labs(title = "Direct MKT") +
  theme_bw()
