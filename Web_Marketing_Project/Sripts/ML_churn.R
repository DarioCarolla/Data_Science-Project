library(e1071)
library(dplyr)
library(caret)
library(sjmisc)
library(BBmisc)
library(ROCR)
set.seed(12345)
#############################################
############ PROPENSITY TO CHURN ############ 
#############################################
# Di seguito sono stati testati tre modelli per prevedere la propensione di un cliente
# ad essere un churn. In paricolare sono stati utilizzati:
#   Random forest
#   Regressione logistica
#   Rete Neurale

################################################
################ RANDOM FOREST #################
################################################
#### PREPROCESSING #####
# Per utilizzare il random forest tutti i dati sono stati trasformati in dati numerici
df_churn_prep <- df_churn_def %>%
  # Binarizzazione dei valori di COD_FID, si ottengono cosi' 4 diversi attributi:
  # - COD_FID_STANDARD
  # - COD_FID_PREMIUM
  # - COD_FID_STANDARD BIZ
  # - COD_FID_PREMIUM BIZ
  bind_cols(to_dummy(df_churn_def$COD_FID, suffix = "label")) %>%
  # Si rendono numerici tutti gli attributi factor per necessita' date dal modello
  mutate(STATUS_FID = as.numeric(if_else(STATUS_FID == "1", 1, 0))) %>%
  mutate(ULTIMA_DIREZIONE = as.numeric(ULTIMA_DIREZIONE)) %>%
  # L'unico factor rimanente e' la variabile dipendente
  mutate(CHURN = as.factor(CHURN)) %>%
  select(-c(ID_CLI, COD_FID, DT_ACTIVE, LAST_DATE, "COD_FID_STANDARD BIZ")) %>%
  as.data.frame()

# Si vuole visualizzare la matrice di correlazione
# Pertanto si esclude la variabile dipendente dal dataframe
df_churn_corr <- df_churn_prep %>% select(-c(CHURN))
correlations <- cor(df_churn_corr)
corrplot(correlations, method="circle")

# Eliminazione delle variabili correlate, si escludono:
# - nTransazioni e nAcquisti, entrambi fortemente correlati con nResi
# - QUANTITA_ARTICOLI_TOT, IMPORTO_LORDO_TOT, SCONTO_TOT, tutti correlati tra loro
# - MAX_DIFF_ULTIMO_ACQ e MIN_DIFF_ULTIMO_ACQUISTO, entrambi fortementi correlati con DIFF_ULTIMO_ACQ
# - Pur essendo COD_FID_STANDARD correlato con COD_FID_PREMIUM, si sceglie di non eliminare le variabili
#   in quanto rappresentanti di una porzione di clienti
df_churn_prep <- df_churn_prep %>%
  select(-c(nACQUISTI, nTRANSAZIONI, SPESA_MEDIA, QUANTITA_ARTICOLI_TOT, IMPORTO_LORDO_TOT, SCONTO_TOT,
            MAX_DIFF_ULTIMO_ACQ, MIN_DIFF_ULTIMO_ACQ))

#Si traccia la matrice di correlazione risultante
df_churn_corr <- df_churn_prep %>% select(-c(CHURN))
correlations <- cor(df_churn_corr)
corrplot(correlations, method="circle")

#### Test set and training set ####
# Creazione delle partizioni di train e test per il modello
train <- sample(nrow(df_churn_prep), 0.7*nrow(df_churn_prep), replace = FALSE)
train_set_rf <- df_churn_prep[train,]
test_set_rf <- df_churn_prep[-train,]
# Si verifica che il dataset ottenuto sia bilanciato
table(train_set_rf$CHURN)
train_set_rf <- train_set_rf %>% 
  select(-CHURN, CHURN)
# Non essendo bilanciato, si provvede al bilanciamento del training set secondo l'attributo CHURN
train_set_rf <- downSample(x = train_set_rf[, -ncol(train_set_rf)], y = train_set_rf$CHURN)
train_set_rf <- rename(train_set_rf, "CHURN" = "Class") 
# Ora il training set risulta bilanciato
table(train_set_rf$CHURN)

# Si rinominano le variabili COD_FID_PREMIUM BIZ sia nel training set che nel test set
# per renderle richiamabili nel modello tramite il nome della colonna
colnames(train_set_rf)[colnames(train_set_rf) == "COD_FID_PREMIUM BIZ"] <- "COD_FID_PREMIUM_BIZ"
colnames(test_set_rf)[colnames(test_set_rf) == "COD_FID_PREMIUM BIZ"] <- "COD_FID_PREMIUM_BIZ"

#### MODELLO RANDOM FOREST ####
# Prima esecuzione del modello random forest
# Vengono utilizzati tutti gli attributi del dataset
# Si escludono i valori NA derivati dalla variabile REGION
# Si decide di utilizzare un numero di alberi pari a 10 perche' il lievissimo miglioramento
# dell'accuracy non giustifica il maggior tempo richiesto per l'esecuzione
model_rf <- train(CHURN ~ .,
                  data = train_set_rf,
                  method="rf",
                  ntree = 10,
                  family="binomial",
                  na.action = na.pass,
                  tuneLength = 5)

# Si testa il modello appena trainato sul test set
# Il test set e' una porzioni di dati sconosciuta al modello
predtest_rf <- predict(model_rf, test_set_rf)
# Creazione della confusion matrix
CM_rf <- confusionMatrix(reference = test_set_rf$CHURN,
                         data = predtest_rf,
                         mode = "everything",
                         positive = '1')
#Stampa delle misure ottneute
CM_rf

# Estrazione dei dati per la valutazione della curva ROC per il primo modello random forest
pred_rf <- prediction(predictions = as.numeric(predtest_rf),
                      labels = as.numeric(test_set_rf$CHURN))
perf_roc_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
# Stampa della curva ROC ottenuta e della misura di AUC
plot(perf_roc_rf, main  = "ROC curve for Random Forest",
     col = "blue", lwd = 3)
perf.auc_rf <- performance(pred_rf, measure = "auc")
perf.auc_rf@y.values[[1]]

# Esecuzione della feature selection:
# test dell'utilita' delle variabili per il modello
plot(varImp(model_rf))

#### MODELLO RANDOM FOREST CON FEATURE SELECTION ####
# Le variabili selezionate per questo secondo modello sono le 3 che sono risultate piu' 
# esplicative dall'analisi di feature selection appena effettuata, risultano rilevanti:
# - DIFF_ULTIMO_ACQ
# - IMPORTO_NETTO_TOT
# - QUANTITA_ARTICOLI_MEDI
model_rf_fs <- train(CHURN ~ DIFF_ULTIMO_ACQ + IMPORTO_NETTO_TOT + QUANTITA_ARTICOLI_MEDI, 
                     data = train_set_rf,
                     method="rf",
                     ntree = 10,
                     family="binomial",
                     na.action = na.pass,
                     tuneLength = 5)

# Si testa il modello apprena trainato sul test set
predtest_rf_fs <- predict(model_rf_fs, test_set_rf)
# Creazione della confusion matrix
CM_rf_fs <- confusionMatrix(reference = test_set_rf$CHURN,
                         data = predtest_rf_fs,
                         mode = "everything",
                         positive = '1')
# Stampa delle misure ottenute
CM_rf_fs

# Estrazione dei dati per la valutazione della curva ROC per il secondo modello random forest
pred_rf_fs <- prediction(predictions = as.numeric(predtest_rf_fs),
                      labels = as.numeric(test_set_rf$CHURN))
perf_roc_rf_fs <- performance(pred_rf_fs, measure = "tpr", x.measure = "fpr")
# Stampa della curva ROC ottenuta e della misura di AUC (0.950)
plot(perf_roc_rf_fs, main  = "ROC curve for Random Forest",
     col = "blue", lwd = 3)
perf.auc_rf_fs <- performance(pred_rf_fs, measure = "auc")
perf.auc_rf_fs@y.values[[1]]


#### 10-CROSS VALIDATION RANDOM FOREST ####
# La cross validation e' un processo utilizzato per verificare
# che i valori di accuratezza ottenuti dal modello non siano casuali
# infatti il risultato restituito e' un valore medio tra i valori di
# accuratezza ottenuti in tutte le iterazoni dell'algoritmo
ctrl_churn_CV <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = TRUE)

#L'algoritmo cosiì impostato esegue 10 iterazioni e ne stampa il risultato
rf_churn_CV <- train(CHURN ~ DIFF_ULTIMO_ACQ + IMPORTO_NETTO_TOT + QUANTITA_ARTICOLI_MEDI,
                        data = df_churn_prep,
                        method="rf",
                        ntree = 10,
                        family="binomial",
                        na.action = na.pass,
                        tuneLength = 5)
rf_churn_CV$results
# Confrontando il risultato della cross validation con quello ottenuto
# precedentemente sul test set, e' possibile affermare che i due risultati siano
# comparabili, dai dati non appare traccia di overfitting dei dati


################################################
############ LOGISTIC REGRESSION ###############
################################################
# Per la regressione logistica viene utilizzato lo stesso preprocessing
# utilizzato per la random forest. Di conseguenza gli stessi test e train set
test_set_rl <- test_set_rf
train_set_rl <- train_set_rf

# Creazione e train del modello basato su regressione logistica
# Anche in questo caso al primo modello vengono passati tutti gli attributi
logit_churn <- glm(CHURN ~., data = train_set_rl, family=binomial)

# Verifica del modello con applicazione sul test set
test_set_rl$model_prob <- predict(logit_churn, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                       CHURN_bin = 1*(CHURN == 1) + 0)
test_set_rl$CHURN <- as.factor(test_set_rl$CHURN)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl <- confusionMatrix(reference = test_set_rl$model_pred,
                                      data = test_set_rl$CHURN,
                                      mode = "everything", positive = "1")
ConfusionMatrix_rl
# L'accuracy risultante e' prossima al 94% 


# Si prosegue con la valutazione della curva ROC
pred_rl <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                      labels = as.numeric(test_set_rl$CHURN))
perf_roc_rl <- performance(pred_rl, measure = "tpr", x.measure = "fpr")
#Stampa del grafico e del valore di AUC (0.991)
plot(perf_roc_rl, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
perf.auc_rl <- performance(pred_rl, measure = "auc")
perf.auc_rl@y.values[[1]]

# L'esecuzione della feature selecton permette di scoprire quali sono le variabili piu' influenti
# sul modello, e' possibile anche eseguire questa operazione con il test anova
varImp(logit_churn)
anova(logit_churn, test = "Chisq")


#### LOGISTIC REGRESSION CON FEATURE SELECTION ####
# Il nuovo modello tiene in considerazione meno variabili del modello precedente,
# in particolare sono rimaste solamente DIFF_ULTIMO_ACQ, nRESI, STATUS_FID e ULTIMA_DIREZIONE
logit_churn_FS <- glm(CHURN ~ DIFF_ULTIMO_ACQ + nRESI + STATUS_FID + ULTIMA_DIREZIONE,
                      data = train_set_rl, family=binomial)

# Esecuzione su test set con stampa delle misure indicative
test_set_rl$model_prob <- predict(logit_churn_FS, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                       CHURN_bin = 1*(CHURN == 1) + 0)
test_set_rl$CHURN <- as.factor(test_set_rl$CHURN)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl_FS <- confusionMatrix(reference = test_set_rl$model_pred,
                                         data = test_set_rl$CHURN,
                                         mode = "everything", positive = "1")
ConfusionMatrix_rl_FS
# L'accuracy ottenuta e' pari al 93%,
# si perde poca accuratezza in favore di un'elaborazione piu' rapida


# Si prosegue con la valutazione della curva ROC sul secondo modello logistic
pred_rl <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                      labels = as.numeric(test_set_rl$CHURN))
perf_roc_rl <- performance(pred_rl, measure = "tpr", x.measure = "fpr")
#Stampa del grafico e del valore di AUC (0.991), invariato rispetto a prima
plot(perf_roc_rl, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
perf.auc_rl <- performance(pred_rl, measure = "auc")
perf.auc_rl@y.values[[1]]


#### LOGISTIC REGRESSION CON CROSS VALIDATION ####
ctrl_churn_CV <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = TRUE)

logit_churn_CV <- train(CHURN ~ DIFF_ULTIMO_ACQ + nRESI + STATUS_FID + ULTIMA_DIREZIONE,
                        data = df_churn_prep,
                        method="glm",
                        family="binomial",
                        trControl = ctrl_churn_CV,
                        na.action = na.pass,
                        tuneLength = 5)
logit_churn_CV$results
# Il risultato della cross validation e' estremamente vicino a quello della singola esecuzione
# L'accuracy media ottenuta dalle esecuzioni e' di 94.9% 


##########################################
############# NEURAL NETWORK #############
##########################################

#### PREPROCESSING ####
# Il preprocessing della rete neurale e' finalizzato, come i precedenti, a trasformare in numeriche
# le variabili di tipo factor, ma non solo: la rete neurale necessita che tali valori siano normalizzati,
# nel caso non lo fossero si assisterebbe ad una perdita rapida della capacita' di apprendimento
df_churn_prep_NN <- df_churn_def %>%
  bind_cols(to_dummy(df_churn_def$COD_FID, suffix = "label")) %>%
  mutate(STATUS_FID = as.numeric(if_else(STATUS_FID == "1", 1, 0))) %>%
  mutate(nTRANSAZIONI = normalize(nTRANSAZIONI, range = c(0,1), method = "range")) %>%
  mutate(nACQUISTI = normalize(nACQUISTI, range = c(0,1), method = "range")) %>%
  mutate(nRESI = normalize(nRESI, range = c(0,1), method = "range")) %>%
  mutate(ULTIMA_DIREZIONE = as.numeric(ULTIMA_DIREZIONE)) %>%
  mutate(IMPORTO_NETTO_TOT = normalize(IMPORTO_NETTO_TOT, range = c(0,1), method = "range")) %>%
  mutate(IMPORTO_LORDO_TOT = normalize(IMPORTO_LORDO_TOT, range = c(0,1), method = "range")) %>%
  mutate(SCONTO_TOT = normalize(SCONTO_TOT, range = c(0,1), method = "range")) %>%
  mutate(SPESA_MEDIA = normalize(SPESA_MEDIA, range = c(0,1), method = "range")) %>%
  mutate(QUANTITA_ARTICOLI_TOT = normalize(QUANTITA_ARTICOLI_TOT, range = c(0,1), method = "range")) %>%
  mutate(QUANTITA_ARTICOLI_MEDI = normalize(QUANTITA_ARTICOLI_MEDI, range = c(0,1), method = "range")) %>%
  mutate(nRESI = normalize(nRESI, range = c(0,1), method = "range")) %>%
  mutate(MAX_DIFF_ULTIMO_ACQ = normalize(MAX_DIFF_ULTIMO_ACQ, range = c(0,1), method = "range")) %>%
  mutate(MIN_DIFF_ULTIMO_ACQ = normalize(MIN_DIFF_ULTIMO_ACQ, range = c(0,1), method = "range")) %>%
  mutate(DIFF_ULTIMO_ACQ = normalize(DIFF_ULTIMO_ACQ, range = c(0,1), method = "range")) %>%
  mutate(CHURN = as.factor(CHURN)) %>%
  select(-c(ID_CLI, COD_FID, DT_ACTIVE, LAST_DATE, "COD_FID_STANDARD BIZ")) %>%
  as.data.frame()

# Stampa della matrice di correlazione
df_churn_corr <- df_churn_prep_NN %>% select(-c(CHURN))
correlations <- cor(df_churn_corr)
corrplot(correlations, method="circle")

# Eliminazione variabili correlate, le correlazioni sono uguali a quelle dei precedenti modelli
# pertanto si eliminano le stesse variabili
df_churn_prep_NN <- df_churn_prep_NN %>% select(-c(nTRANSAZIONI, IMPORTO_LORDO_TOT,
                    SCONTO_TOT, QUANTITA_ARTICOLI_TOT, nACQUISTI, SPESA_MEDIA, MIN_DIFF_ULTIMO_ACQ))

# Stampa della matrice di correlazione senza le variabili eliminate
df_churn_corr <- df_churn_prep_NN %>% select(-c(CHURN))
correlations <- cor(df_churn_corr)
corrplot(correlations, method="circle")

#### Test set and training set ####
# Creazione del train set (70% dei record) e del test_set (30% dei record)
train <- sample(nrow(df_churn_prep_NN), 0.7*nrow(df_churn_prep_NN), replace = FALSE)
train_set_nn <- df_churn_prep_NN[train,]
test_set_nn <- df_churn_prep_NN[-train,]
table(train_set_nn$CHURN)
#Essendo il dataset sbilanciato rispetto alla variabile dipendente si decide di bilanciarlo
train_set_nn <- train_set_nn %>% 
  select(-CHURN, CHURN)
# Bilanciamento di CHURN per il training set
train_set_nn <- downSample(x = train_set_nn[, -ncol(train_set_nn)], y = train_set_nn$CHURN)
train_set_nn <- rename(train_set_nn, "CHURN" = "Class") 
table(train_set_nn$CHURN)


# Creazione del controllo per il modello neural netowrk
rctrl <- trainControl(method = "none")
nnetGrid <-  expand.grid(size = 4,
                         decay = 0.4)

# Creazione del modello neural network dalla libreria caret
# Si decide di utilizzare una rete neurale ordinaria con un numero massimo di iterazioni a 500
# passando al modello tutti gli attributi
neural_net_1 <- caret::train(CHURN ~ .,
                             data=train_set_nn,    
                             method = "nnet",
                             maxit = 500,
                             tuneLength = 1,
                             trControl = rctrl,
                             tuneGrid = nnetGrid,
                             verbose =FALSE)

# Previsioni sul test set
test_set_nn$CHURN <- as.factor(test_set_nn$CHURN)
predictions <- predict(neural_net_1, test_set_nn)

# Misure ottenute
CM_1 <- confusionMatrix(predictions, test_set_nn$CHURN)
print(CM_1) # Accuracy : 0.974 

# Rappresentazione grafica NN
plotnet(neural_net_1$finalModel, y_names = "IncomeLevel")
title("Graphical Representation of our Neural Network")

# Feature selection sul primo modello NN
garson(neural_net_1$finalModel)
# Si mantengono solamente le 3 variabili piu' influenti, le altre vengono eliminate

# Analisi della curva ROC del primo modello NN
roc_nn_1 <- prediction(predictions = as.numeric(predictions),
                       labels = as.numeric(test_set_nn$CHURN))
perf_roc_cv <- performance(roc_nn_1, measure = "tpr", x.measure = "fpr")
# Stampa della curva e del valore AUC (0.975)
plot(perf_roc_cv, main  = "ROC curve for Neural Network",
     col = "blue", lwd = 3)
perf.auc_cv <- performance(roc_nn_1, measure = "auc")
perf.auc_cv@y.values[[1]]


##############################################
######## NN POST FEATURE SELECTION ###########
##############################################

# Creazione del secondo modello di rete neurale, le variabili esplicative mantenute sono:
# DIFF_ULTIMO_ACQ, MAX_DIFF_ULTIMO_ACQ, nRESI
# I parametri della rete neurale vengono mantenuti uguali ai precedenti
rctrl <- trainControl(method = "none")
nnetGrid <-  expand.grid(size = 2,
                         decay = 0.4)
neural_net_FS <- caret::train(CHURN ~ DIFF_ULTIMO_ACQ + MAX_DIFF_ULTIMO_ACQ + nRESI,
                              data=train_set_nn,    
                              method = "nnet",
                              maxit = 500,
                              tuneLength = 1,
                              trControl = rctrl,
                              tuneGrid = nnetGrid,
                              verbose =FALSE)

# Confronto test set
test_set_nn$CHURN <- as.factor(test_set_nn$CHURN)
predictions <- predict(neural_net_FS, test_set_nn)

# Misure ottenute
CM_FS <- confusionMatrix(predictions, test_set_nn$CHURN)
print(CM_FS) # Accuracy : 0.9723  

# Reppresentazione grafica NN
plotnet(neural_net_FS$finalModel, y_names = "IncomeLevel")
title("Graphical Representation of our Neural Network")


# Analisi della curva ROC del modello NN dopo la feature seleciton
roc_nn_2 <- prediction(predictions = as.numeric(predictions),
                       labels = as.numeric(test_set_nn$CHURN))
perf_roc_cv <- performance(roc_nn_2, measure = "tpr", x.measure = "fpr")
# Stampa del grafico ROC e del valore AUC
plot(perf_roc_cv, main  = "ROC curve for Neural Network",
     col = "blue", lwd = 3)
perf.auc_cv <- performance(roc_nn_2, measure = "auc")
perf.auc_cv@y.values[[1]]


#######################################
########## CROSS-VALIDATION ###########
#######################################

# I test eseguiti in precedenza per una singola coppia train-test vengono ripetuti nella
# cross-validation per un totale di 10 iterazioni

rctrl_cv <- trainControl(method = "cv",
                         number = 10,
                         savePredictions = TRUE)
nnetGrid <-  expand.grid(size = 2,
                         decay = 0.4)
neural_net_cv <- caret::train(CHURN ~ DIFF_ULTIMO_ACQ + MAX_DIFF_ULTIMO_ACQ + nRESI,
                              data=df_churn_prep_NN,    
                              method = "nnet",
                              maxit = 500,
                              tuneLength = 1,
                              trControl = rctrl_cv,
                              tuneGrid = nnetGrid,
                              verbose =FALSE)
neural_net_cv$results
# L'accuracy media ottenuta dal processo di cross-validation supera il 97% e conferma
# l'ottimo valore trovato con la singola verifica sul test set