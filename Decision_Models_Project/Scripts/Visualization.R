library(ggplot2)
library(stringr)

gift_plot <- gift
gift_plot$Toy <- factor(str_extract(gift_plot$GiftId, pattern = "[a-z]+"))

# Numento di giocattoli per tipologia
ggplot(data = gift_plot, aes(x = reorder(Toy, -table(Toy)[Toy]), fill = Toy)) + 
  geom_bar(stat = "count") + 
  theme_classic() + 
  xlab("Toy") +
  guides(fill = FALSE) +
  ggtitle("Numento di giocattoli per tipologia")

# Densità del peso per tiplogia di giocattoli
ggplot(data = gift_plot, aes(x = weight, fill = Toy)) + 
  geom_density() + 
  facet_wrap( ~ Toy, ncol = 3, scales = "free") +
  theme_bw() + 
  guides(fill=FALSE) +
  ggtitle("Densità del peso per tiplogia di giocattoli")

# Distribuzione dei pesi per tipologia di giocattoli
ggplot(data = gift_plot, aes(y = weight, x = Toy, fill = Toy)) +
  geom_boxplot() +
  theme_classic() + 
  guides(fill=FALSE) +
  labs(x = "Giocattoli", y = "Peso") +
  ggtitle("Distribuzione dei pesi per tipologia di giocattoli") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  


