# Applicazione del FirstFit utilizzando il Genetic Algorithm

# Creazione finess function per GA
firstFit_ga <- function(weight, seq){
  # Assegnamento della sequenza creata da GA
  weight <- weight[seq]
  
  # Vettore con spazio rimanente in ciascun sacco
  bin_rem <- c(rep(50, 1000))
  
  # Vettore con numero di elementi aggiunti per sacco
  gift <- c(rep(0, 1000))
  
  # posizionamento elementi
  for (i in 1:length(weight)) {
    # cerca il primo sacco che possa ospitare l'elemento
    for (j in 1:1000) {
      # controlla che ci sia spazio nel sacco e che ci siano
      # meno di nove elementi all'interno
      if(bin_rem[j] >= weight[i] & gift[j] < 9){
        bin_rem[j] <- bin_rem[j] - weight[i]
        gift[j] <- gift[j] + 1
        break()
      }
    }
  }
  
  # Calcolo peso raggiunto
  res <- 50000 - sum(bin_rem)
  return(res)
}


# Caricamento libreria GA
library(GA)

# Esecusione genetic algorithm
GA <- ga(type = "permutation",
         fitness = firstFit_ga, 
         weight = gift$weight,
         lower = 1,
         upper = 7166,
         popSize = 100,
         maxiter = 50,
         run = 20,
         pmutation = 0.2,
         keepBest = TRUE,
         monitor = TRUE)
sol_ga <- summary(GA)
sol_ga


# plot GA
out <- plot(GA)

# soluzione del GA
sol <- sol_ga$solution[1,]

# ordinamento dei regali secondo la soluzione del GA
gift_ga <- gift[sol, ]

# Creazione dataset dei risultati del GA
res_GA <- firstFit(gift_ga$weight, 1000, 50)

