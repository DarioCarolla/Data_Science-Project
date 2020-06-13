# Implementazione first fit
firstFit <- function(weight, n_bins, c){
  # numero di elementi
  n_items <- length(weight)
  
  # creazione data frame risultati
  res <- data.frame(matrix(NA, ncol = 11, nrow = n_bins))
  colnames(res) <- (c("Regalo_1", "Regalo_2", "Regalo_3", "Regalo_4", "Regalo_5",
                      "Regalo_6", "Regalo_7", "Regalo_8", "Regalo_9", "Peso_tot", "N_regali"))
  
  # vettore con numero regali rimanenti
  gift <- c(rep(0, n_bins))
  
  # vettore con spazio rimanente in ciascun sacco
  bin_rem <- c(rep(c, n_bins))
  
  # posizionamento elementi
  for (i in 1:n_items) {
    # cerca il primo sacco che possa ospitare l'elemento
    for (j in 1:n_bins) {
      # controlla che ci sia spazio nel sacco e che ci siano
      # meno di nove elementi all'interno
      if(bin_rem[j] >= weight[i] & gift[j] < 9){
        bin_rem[j] <- bin_rem[j] - weight[i]
        gift[j] <- gift[j] + 1
        # Inserimento all'interno del DF risultati
        res[j, gift[j]] <- weight[i]
        res[j, 10] <- c - bin_rem[j]
        res[j, 11] <- gift[j]
        break()
      }
    }
  }
  return(res)
}


# Implementazione first fit con ordinamento
firstFit_ordered <- function(weight, n_bins, c){
  # Pesi ordinati in maniera decrescente
  weight <- sort(weight, decreasing = TRUE)
  
  # chiamata a first fit
  firstFit(weight, n_bins, c)
}

# Chiamata a FirstFit
res_FF <- firstFit(gift$weight, 1000, 50)

# Chiamata a FirstFit con ordinamento
res_FF_ordered <- firstFit_ordered(gift$weight, 1000, 50)
