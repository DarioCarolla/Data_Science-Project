set.seed(12345)

gift <- read.csv("~/Desktop/Progetto Decision Models/Carolla_Dario_807547_Licciardello_Matteo_799368/gifts.csv", stringsAsFactors = F)

# Generazione dei pesi dalle distribuzioni date
for (i in 1:nrow(gift)){
  if(grepl(pattern = "horse", gift$GiftId[i]))
    gift$weight[i] <- max(0, rnorm(1, 5, 2))
  else if(grepl(pattern = "ball", gift$GiftId[i]))
    gift$weight[i] <- max(0, 1 + rnorm(1, 1, 0.3))
  else if(grepl(pattern = "bike", gift$GiftId[i]))
    gift$weight[i] <- max(0, rnorm(1, 20, 10))
  else if(grepl(pattern = "train", gift$GiftId[i]))
    gift$weight[i] <- max(0, rnorm(1, 10, 5))
  else if(grepl(pattern = "coal", gift$GiftId[i]))
    gift$weight[i] <- 47 * rbeta(1, 0.5, 0.5)
  else if(grepl(pattern = "book", gift$GiftId[i]))
    gift$weight[i] <- rchisq(1, 2)
  else if(grepl(pattern = "doll", gift$GiftId[i]))
    gift$weight[i] <- rgamma(1, shape = 5, scale = 1)
  else if(grepl(pattern = "block", gift$GiftId[i]))
    gift$weight[i] <- rtriangle(1, a = 5, b = 20, c = 10)
  else if(grepl(pattern = "gloves", gift$GiftId[i])){
    x <- runif(1)
    if(x < 0.3)
      x <- 3.0 + runif(1)
    gift$weight[i] <- x
  }
}