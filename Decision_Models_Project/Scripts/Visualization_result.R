# Peso inserito con first-fit
weight_FF <- sum(res_FF$Peso_tot)

# Peso inserito con first-fit ordinata
weight_FF_ordered <- sum(res_FF_ordered$Peso_tot)

# Peso inserito con first-fit GA
weight_GA <- sum(res_GA$Peso_tot)

# creazione datatset per plot
algorithm=rep(c("First Fit", "First Fit ordered", "Genetic Algotithm"))
value=abs(c(weight_FF, weight_FF_ordered, weight_GA))
data=data.frame(algorithm, value)

# Rappresentazione risultati algoritmi implementati
ggplot(data, aes(y=value, x=algorithm)) + 
  geom_bar(position="dodge", stat="identity", fill = "red3", width = 0.6) +
  #coord_flip() +
  geom_hline(yintercept = 50000, linetype="longdash", 
             color = "gray40", size=1.5) +
  labs(x ="Algoritmo", y = "Peso totale") +
  theme_bw()

# Riempimento sacchi per first fit
ggplot(res_FF, aes(x = c(1:1000), y =Peso_tot)) +
  geom_point(col = "gray35") + 
  labs(title="First fit",
       x ="Sacco", y = "Peso") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Riempimento sacchi per first fit ordinato
ggplot(res_FF_ordered, aes(x = c(1:1000), y =Peso_tot)) +
  geom_point(col = "gray35") + 
  labs(title="First fit ordinato",
       x ="Sacco", y = "Peso") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Riempimento sacchi per GA
ggplot(res_GA, aes(x = c(1:1000), y =Peso_tot)) +
  geom_point(col = "gray35") + 
  labs(title="Genetic algorithm",
       x ="Sacco", y = "Peso") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# bar plot dei regali inseriti per sacchi GA
ggplot(res_GA,
       aes(x = N_regali)) +
  geom_bar(fill = "darkgreen") + 
  coord_flip() +
  labs(title="Regali inseriti nei sacchi",
       x ="Regali", y = "Numero sacchi") +
  scale_x_discrete(limit = c(2:9)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))





