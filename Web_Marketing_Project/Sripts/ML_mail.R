library(e1071)
library(dplyr)
library(caret)
library(sjmisc)
library(BBmisc)
library(ROCR)
set.seed(12345)

########################################################
############ PROPENSITY OF EMAIL ENGAGEMENT ############ 
########################################################
# Di seguito sono stati testati tre modelli per prevedere la propensione di un cliente
# ad aprire una email. In paricolare sono stati utilizzati:
#   Random forest
#   Regressione logistica
#   Rete Neurale

#######################################
############ RANDOM FOREST ############ 
#######################################
library(randomForest)

#### PREPROCESSING #####
# Per utilizzare il random forest tutti i dati sono stati trasformati in dati numerici
# In paricolare i giorni della settimana in binari (0 giorni infrasettimanali, 1 weekend)
# La variabile AGE_FID è stata normalizzata
# TYP_CLI_ACCOUNT è stato binarizza (4 = 1, 2 = 0)
# Le regioni sono state divise in nord, centro, sud ed isole e poi sono trasformate in variabili dummy
df_master_rf <- df_master %>%
  mutate(W_SEND_PREV = as.integer(if_else(W_SEND_PREV == "TRUE", 1, 0))) %>%
  mutate(W_FAIL_PREV = as.integer(if_else(W_FAIL_PREV == "TRUE", 1, 0))) %>%
  mutate(SEND_WEEKDAY = ifelse(SEND_WEEKDAY %in% c("Lunedì", "Martedì", "Mercoledì",
                                                   "Giovedì", "Venerdì"), 0,  1)) %>% # giorni infrasettimanali -> 0
                                                                                      # fine settimana -> 1 
  mutate(TYP_CLI_FID = as.integer(if_else(TYP_CLI_FID == "1", 1, 0))) %>%
  bind_cols(to_dummy(df_master$COD_FID, suffix = "label")) %>%
  mutate(STATUS_FID = as.integer(if_else(STATUS_FID == "1", 1, 0))) %>%
  mutate(NUM_FIDs = as.integer(NUM_FIDs)) %>%
  mutate(AGE_FID = as.numeric(normalize(AGE_FID, method = "standardize", range = c(0, 1)))) %>% # standardizzazione
  mutate(TYP_CLI_ACCOUNT = as.integer(if_else(TYP_CLI_ACCOUNT == 4, 1, 0))) %>% # il tipo di cliente è cosiderata come una categoria, se 4 = 1 2 = 0
  bind_cols(to_dummy(df_master$TYP_CAMP, suffix = "label")) %>%
  mutate(REGION = case_when(
    # NORD REGION
    REGION == "LIGURIA" | REGION == "LOMBARDIA" | REGION == "PIEMONTE" | 
    REGION == "VALLE D'AOSTA" | REGION == "EMILIA ROMAGNA" | REGION == "FRIULI VENEZIA GIULIA" | 
    REGION == "TRENTINO ALTO ADIGE" | REGION == "VENETO" ~ "NORD",
    # CENTRO REGION
    REGION == "LAZIO" | REGION == "MARCHE" | 
    REGION == "TOSCANA" | REGION == "UMBRIA" ~ "CENTRO",
    # SUD REGION
    REGION == "ABRUZZO" | REGION == "BASILICATA" | REGION == "CALABRIA" |
    REGION == "CAMPANIA" | REGION == "MOLISE" | REGION == "PUGLIA" ~ "SUD",
    # ISOLE REGION
    REGION == "SICILIA" | REGION == "SARDEGNA" ~ "ISOLE"
  )) %>%
  select(-c(ID_EVENT_S, ID_CLI, ID_NEG, COD_FID, W_FAIL_PREV, W_PHONE, TYP_JOB, 
            EMAIL_PROVIDER_CLEAN, PRV, FLAG_PRIVACY_1, FLAG_PRIVACY_2,
            FLAG_DIRECT_MKT, TYP_CAMP, "COD_FID_STANDARD BIZ", TYP_CAMP_PRODUCT))

# dummy isole
df_master_rf <- df_master_rf %>%
  bind_cols(to_dummy(df_master_rf$REGION, suffix = "label")) %>%
  select(-c("REGION", "REGION_ISOLE"))

# matrice di correlazione
library(corrplot)
correlations <- cor(df_master_rf[,2:20])
corrplot(correlations, method="circle")
# COD_FID_PREMIUM E COD_FID_STANDARD sono correlate ma è la stessa variabile
# OPEN_RATE_PREV e NUM_OPEN_PREV naturalmente sono correlate, è stato rimosso NUM_OPEN_PREV
# CLIK_RATE_PREV e NUM_CLIK_PREV naturalmente sono correlate, è stato rimosso NUM_CLIK_PREV
# TYP CLI ACCOUNT dice se un cliente è privato o business quindi è normale che si correlata 
# con COD_FID_STANDARD è stato rimosso TYP_CLI_ACCOUNT
df_master_rf <- df_master_rf %>%
  select(-c(NUM_CLICK_PREV, NUM_OPEN_PREV, TYP_CLI_ACCOUNT))

correlations <- cor(df_master_rf[,2:17])
corrplot(correlations, method="circle")

str(df_master_rf)
summary(df_master_rf)

#### CREAZIONE TRAIN SET E TEST SET #### 
train <- sample(nrow(df_master_rf), 0.8*nrow(df_master_rf), replace = FALSE)
train_set <- df_master_rf[train,]
nrow(train_set)
test_set <- df_master_rf[-train,]
table(train_set$TARGET)
# La variabile TARGET risulta troppo sbilanciata all'interno del training set
# questo potrebbe compromettere la validità del modello. Quindi i dati sono stati
# ridotti per ottenere lo stesso numero di TARGET = 0 e 1

train_set <- train_set %>% 
  select(-TARGET, TARGET)
# bilanciamento per il train set
train_set <- downSample(x = train_set[, -ncol(train_set)], y = train_set$TARGET)
train_set <- rename(train_set, "TARGET" = "Class") 

table(train_set$TARGET) # bilaciata e con meno variabili

# Rinomino colonne con spazi (in quanto potrebbe creare degli errori nella creazione del modello)
colnames(train_set)[colnames(train_set) == "COD_FID_PREMIUM BIZ"] <- "COD_FID_PREMIUM_BIZ"
colnames(test_set)[colnames(test_set) == "COD_FID_PREMIUM BIZ"] <- "COD_FID_PREMIUM_BIZ"


#### MODELLO RANDOM FOREST ####
model_fs <- train(TARGET ~ NUM_SEND_PREV + W_SEND_PREV + AGE_FID + COD_FID_PREMIUM_BIZ +
                           SEND_WEEKDAY + OPEN_RATE_PREV + COD_FID_PREMIUM +
                           COD_FID_STANDARD + CLICK_RATE_PREV + NUM_FIDs + TYP_CLI_FID + STATUS_FID +
                           TYP_CAMP_LOCAL + TYP_CAMP_NATIONAL + TYP_CAMP_PERSONALIZED + NUM_FAIL_PREV,
                         data = train_set, 
                         method="rf",
                         ntree = 10,
                         family="binomial",
                         na.action = na.pass,
                         tuneLength = 5)

# Valori su test set
fitted <- predict(model_fs)
predtest_fs <- predict(model_fs, test_set)
CM_test_fs <- confusionMatrix(reference = test_set$TARGET,
                              data = predtest_fs,
                              mode = "everything",
                              positive = '1')
CM_test_fs # ACCURACY 0.82

# CURVA ROC
pred_fs <- prediction(predictions = as.numeric(predtest_fs),
                      labels = as.numeric(test_set$TARGET))
perf_roc_fs <- performance(pred_fs, measure = "tpr", x.measure = "fpr")
plot(perf_roc_fs, main  = "ROC curve for Random Forest",
     col = "blue", lwd = 3)

# VALORE AUC
perf.auc_fs <- performance(pred_fs, measure = "auc")
perf.auc_fs@y.values[[1]] # AUC = 0.7836369

# FEATURE SELECTION
fs <- varImpPlot(model_fs)

# Il modello risulta avere una buona accuracy ma dalla feature selection
# si nota che non tutte le variabili sono significative. In particolare,
# la più significativa è OPEN_RATE_PREV, insieme ad essa 
# risultano moderatamente significative anche le variabili
# AGE_FID, COD_FID_PREMIUM, COD_FID_STANDARD e COD_FID_PREMIUM_BIZ
# quindi viene testato un nuovo modello utlizzando solo queste ultime.

#### MODELLO RANDOM FOREST POST FEATURE SELECTION ####
model_rf_fs <- train(TARGET ~ AGE_FID + OPEN_RATE_PREV + COD_FID_PREMIUM +
                            COD_FID_STANDARD + COD_FID_PREMIUM_BIZ, 
                          data = train_set, 
                          method="rf",
                          ntree = 10,
                          family="binomial",
                          na.action = na.pass,
                          tuneLength = 5)

# Valori su test set
fitted <- predict(model_rf_fs)
predtest_rf_fs <- predict(model_rf_fs, test_set)
CM_test_rf_fs <- confusionMatrix(reference = test_set$TARGET,
                               data = predtest_rf_fs,
                               mode = "everything",
                               positive = '1')
CM_test_rf_fs # ACCURAY 0.827

# CURVA ROC
pred_rf_fs <- prediction(predictions = as.numeric(predtest_rf_fs),
                       labels = as.numeric(test_set$TARGET))
perf_roc_rf_fs <- performance(pred_rf_fs, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rf_fs, main  = "ROC curve for Random Forest",
     col = "blue", lwd = 3)

# VALORE AUC
perf.auc_rf_fs <- performance(pred_rf_fs, measure = "auc")
perf.auc_rf_fs@y.values[[1]] # AUC 0.7834048

# FEATURE SELECTION
fs2 <- varImpPlot(model_rf_fs)

# Si nota che l'accuracy del modelo rimane quasi uguale, quindi viene effettuata nuovamente
# la feature selection nel tentativo di rimuovere le variabili meno significative.
# Di seguito viene creato il modello definitivo utilizzando solo le variabili 
# OPEN_RATE_PREV ed AGE_FID

#### MODELLO DEFINITIVO ####
model_rf_fs2 <- train(TARGET ~ OPEN_RATE_PREV + AGE_FID, 
                          data = train_set, 
                          method="rf",
                          ntree = 10,
                          family="binomial",
                          na.action = na.pass,
                          tuneLength = 5)
# Valori su test set
fitted <- predict(model_rf_fs2)
predtest_rf_fs2 <- predict(model_rf_fs2, test_set)
CM_test_rf_fs2 <- confusionMatrix(reference = test_set$TARGET,
                               data = predtest_rf_fs2,
                               mode = "everything",
                               positive = '1')
CM_test_rf_fs2 # ACCURACY 0.8235

# CURVA ROC
pred_rf_fs2 <- prediction(predictions = as.numeric(predtest_rf_fs2),
                       labels = as.numeric(test_set$TARGET))
perf_roc_rf_fs2 <- performance(pred_rf_fs2, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rf_fs2, main  = "ROC curve for Random Forest",
     col = "blue", lwd = 3)

# VALORE AUC
perf.auc_rf_fs2 <- performance(pred_rf_fs2, measure = "auc")
perf.auc_rf_fs2@y.values[[1]] # AUC = 0.7836558

# Ancora una volta l'accuracy risulta essere tendenzialmente invariata.
# Viene eseguita la cross validation per controllare un eventuale 
# overfitting

#### 10-CROSS VALIDATION MODELLO RANDOM FOREST DEFINITIVO ####
library(rfUtilities)
ctrl_CV <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = TRUE)

rf_CV <- train(TARGET ~ OPEN_RATE_PREV + AGE_FID, 
                  data = df_master_rf,
                  method="rf",
                  ntree = 10,
                  family="binomial",
                  na.action = na.pass,
                  tuneLength = 5)
rf_CV$results

# L'accuracy dopo la crossvalidation risulta essere uguale al modello
# generato di conseguenza quest'ultimo può essere considerato valido in quanto
# non si verifica overfitting nei dati.


#############################################
############ LOGISTIC REGRESSION ############ 
#############################################

#### PREPROCESSING ####
# Per la linear regression non è stato effettuato il preprocessing
# in quanto anche in questo caso i dati devono essere numerici, dunque,
# è possibile utilizzare gli stessi dati generati per il Random Forest
# di conseguenza anche train set e test set sono gli stessi utilizzati per il Random Forest

df_master_rl <- df_master_rf
train_set_rl <- train_set
test_set_rl <- test_set

#### MODELLO LOGISTIC REGRESSION ####
logit <- glm(TARGET ~., data = train_set_rl, family=binomial)

test_set_rl$model_prob <- predict(logit, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                       TARGET_bin = 1*(TARGET == 1) + 0)
test_set_rl$TARGET <- as.factor(test_set_rl$TARGET)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl <- confusionMatrix(reference = test_set_rl$model_pred,
                                      data = test_set_rl$TARGET,
                                      mode = "everything", positive = "1")
ConfusionMatrix_rl #accuracy: 0.8559

# CURVA ROC
pred_rl <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                      labels = as.numeric(test_set_rl$TARGET))
perf_roc_rl <- performance(pred_rl, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rl, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_rl <- performance(pred_rl, measure = "auc")
perf.auc_rl@y.values[[1]] # AUC = 0.8225827

# FEATURE SELECTION
varImp(logit) #feature selection
anova(logit, test = "Chisq") #l'importanza delle variabili può essere vista anche con anova

# Il modello risulta avere una buona accuracy ma dalla feature selection
# si nota che non tutte le variabili sono significative. In particolare,
# la più significativa, anche in questo caso è OPEN_RATE_PREV, insieme ad essa 
# risultano moderatamente significative anche le variabili
# NUM_FAIL_PREV, TYP_CAMP_LOCAL, TYP_CAMP_NATIONAL, TYP_CAMP_PERSONALIZED
# REGION_CENTRO, REGION_SUD, REGION_NORD, SEND_WEEKDAY, AGE_FID, 
# CLICK_RATE_PREV, NUM_SEND_PREV, STATUS_FID , NUM_FIDs
# quindi viene testato un nuovo modello utlizzando queste variabili.

#### MODELLO LOGISTIC REGRESSION CON FEATURE SELECTION ####
logit_2 <- glm(TARGET ~ OPEN_RATE_PREV + NUM_FAIL_PREV + 
                 TYP_CAMP_LOCAL + TYP_CAMP_NATIONAL + TYP_CAMP_PERSONALIZED + #TYP_CAMP dummy
                 REGION_CENTRO + REGION_SUD + REGION_NORD + #REGION dummy
                 SEND_WEEKDAY + AGE_FID + CLICK_RATE_PREV + NUM_SEND_PREV + STATUS_FID + 
                 NUM_FIDs, data = train_set_rl, family=binomial)

test_set_rl$model_prob <- predict(logit_2, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                         TARGET_bin = 1*(TARGET == 1) + 0)
test_set_rl$TARGET <- as.factor(test_set_rl$TARGET)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl2 <- confusionMatrix(reference = test_set_rl$model_pred,
                                       data = test_set_rl$TARGET,
                                       mode = "everything", positive = "1")
ConfusionMatrix_rl2 #accuracy: 0.8557

# CURVA ROC
pred_rl2 <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                       labels = as.numeric(test_set_rl$TARGET))
perf_roc_rl2 <- performance(pred_rl2, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rl2, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_rl2 <- performance(pred_rl2, measure = "auc")
perf.auc_rl2@y.values[[1]] # AUC = 0.8220677

# FEATURE SELECTION
varImp(logit_2) #feature selection
anova(logit_2, test = "Chisq")

# L'accuracy risulta essersi ridotta di una quantita insignificante
# quindi viene effettuata nuovamente la feature selection nel tentativo
# di rimuovere le variabili meno significative.

#### MODELLO LOGISTIC REGRESSION CON FEATURE SELECTION TEST 2 ####
logit_3 <- glm(TARGET ~ OPEN_RATE_PREV + NUM_FAIL_PREV + 
                 TYP_CAMP_LOCAL + TYP_CAMP_NATIONAL + TYP_CAMP_PERSONALIZED + #TYP_CAMP dummy
                 REGION_CENTRO + REGION_SUD + REGION_NORD + #REGION dummy
                 SEND_WEEKDAY + AGE_FID + CLICK_RATE_PREV + STATUS_FID, 
               data = train_set_rl, family=binomial)

test_set_rl$model_prob <- predict(logit_3, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                         TARGET_bin = 1*(TARGET == 1) + 0)
test_set_rl$TARGET <- as.factor(test_set_rl$TARGET)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl3 <- confusionMatrix(reference = test_set_rl$model_pred,
                                       data = test_set_rl$TARGET,
                                       mode = "everything", positive = "1")
ConfusionMatrix_rl3 #accuracy: 0.8555

# CURVA ROC
pred_rl3 <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                       labels = as.numeric(test_set_rl$TARGET))
perf_roc_rl3 <- performance(pred_rl3, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rl3, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_rl3 <- performance(pred_rl3, measure = "auc")
perf.auc_rl3@y.values[[1]] # AUC = 0.819957

# FEATURE SELECTION
varImp(logit_3) #feature selection
anova(logit_3, test = "Chisq")

#### MODELLO LOGISTIC REGRESSION CON FEATURE SELECTION TEST 3 ####
logit_4 <- glm(TARGET ~ OPEN_RATE_PREV + NUM_FAIL_PREV + 
                 REGION_CENTRO + REGION_SUD + REGION_NORD + #REGION dummy
                 SEND_WEEKDAY + AGE_FID + CLICK_RATE_PREV + STATUS_FID, 
               data = train_set_rl, family=binomial)

test_set_rl$model_prob <- predict(logit_4, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                         TARGET_bin = 1*(TARGET == 1) + 0)
test_set_rl$TARGET <- as.factor(test_set_rl$TARGET)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl4 <- confusionMatrix(reference = test_set_rl$model_pred,
                                       data = test_set_rl$TARGET,
                                       mode = "everything", positive = "1")
ConfusionMatrix_rl4 #accuracy: 0.8551

# CURVA ROC
pred_rl4 <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                       labels = as.numeric(test_set_rl$TARGET))
perf_roc_rl4 <- performance(pred_rl4, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rl4, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_rl4 <- performance(pred_rl4, measure = "auc")
perf.auc_rl4@y.values[[1]] # AUC = 0.820964

# FEATURE SELECTION
varImp(logit_4) #feature selection
anova(logit_4, test = "Chisq")


#### MODELLO LOGISTIC REGRESSION CON FEATURE SELECTION TEST 4 ####
logit_5 <- glm(TARGET ~ OPEN_RATE_PREV + NUM_FAIL_PREV + 
                 REGION_CENTRO + REGION_SUD + REGION_NORD + #REGION dummy
                 SEND_WEEKDAY + AGE_FID + CLICK_RATE_PREV + STATUS_FID, 
               data = train_set_rl, family=binomial)

test_set_rl$model_prob <- predict(logit_5, test_set_rl, type = "link")
test_set_rl <- test_set_rl  %>% mutate(model_pred = 1*(model_prob > 0.5) + 0,
                                         TARGET_bin = 1*(TARGET == 1) + 0)
test_set_rl$TARGET <- as.factor(test_set_rl$TARGET)
test_set_rl$model_pred <- as.factor(test_set_rl$model_pred)
ConfusionMatrix_rl5 <- confusionMatrix(reference = test_set_rl$model_pred,
                                       data = test_set_rl$TARGET,
                                       mode = "everything", positive = "1")
ConfusionMatrix_rl5 #accuracy: 0.8551

# CURVA ROC
pred_rl5 <- prediction(predictions = as.numeric(test_set_rl$model_prob),
                       labels = as.numeric(test_set_rl$TARGET))
perf_roc_rl5 <- performance(pred_rl5, measure = "tpr", x.measure = "fpr")
plot(perf_roc_rl5, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_rl5 <- performance(pred_rl5, measure = "auc")
perf.auc_rl5@y.values[[1]] # AUC = 0.820964

# FEATURE SELECTION
varImp(logit_5) #feature selection
anova(logit_5, test = "Chisq")


# Con il modello logit_5 l'accuracy si abbassa (di valori irrilevanti) ma, essendo il modello 
# molto veloce si è scelto di utilizzare come modello definitivo pred_rl4 il quale utilizza
# due variabili in più ma il tempo di eseuzione non aumenta eccessivamente.
# Le variabili utilizzate sono: OPEN_RATE_PREV, NUM_FAIL_PREV, REGION_CENTRO, 
# REGION_SUD, REGION_NORD, SEND_WEEKDAY, AGE_FID, CLICK_RATE_PREV, STATUS_FID.


###########################################################
############# 10-CROSS VALIDATION SU PROD_RL4 ##############
###########################################################
ctrl_rl <- trainControl(method = "cv",
                         number = 10,
                         savePredictions = TRUE)

logitcv <- train(TARGET ~ OPEN_RATE_PREV + NUM_FAIL_PREV +
                   REGION_CENTRO + REGION_SUD + REGION_NORD + #REGION dummy
                   SEND_WEEKDAY + AGE_FID + CLICK_RATE_PREV + STATUS_FID,
                 data = df_master_rl,
                 method="glm",
                 family="binomial",
                 trControl = ctrl_rl4,
                 na.action = na.pass,
                 tuneLength = 5)

logitcv$results
# L'accuracy dopo la crossvalidation risulta essere uguale al modello
# generato di conseguenza quest'ultimo può essere considerato valido in quanto
# non si verifica overfitting nei dati.


########################################
############ NEURAL NETWORK ############ 
########################################
library(nnet)
library(NeuralNetTools)

#### PREPROCESSING ####
# E' stato effettuato un nuovo preprocessing per la rete neurale
# in quanto la rete non prende in input valori non numerici.
# Inoltre, per ottimizzarla, tutte le variabili numeriche sono state normalizzate.
df_master_nn <- df_master %>%
  mutate(NUM_SEND_PREV = normalize(NUM_SEND_PREV, range = c(0,1), method="range")) %>%
  mutate(NUM_OPEN_PREV = normalize(NUM_OPEN_PREV, range = c(0,1), method="range")) %>%
  mutate(NUM_CLICK_PREV = normalize(NUM_CLICK_PREV, range = c(0,1), method="range")) %>%
  mutate(NUM_FAIL_PREV = normalize(NUM_FAIL_PREV, range = c(0,1), method="range")) %>%
  mutate(W_SEND_PREV = as.numeric(if_else(W_SEND_PREV == TRUE, 1, 0))) %>%
  mutate(W_FAIL_PREV = as.numeric(if_else(W_FAIL_PREV == TRUE, 1, 0))) %>%
  mutate(SEND_WEEKDAY = ifelse(SEND_WEEKDAY %in% c("Lunedì", "Martedì", "Mercoledì", 
                                                   "Giovedì", "Venerdì"), # giorni infrasettimanali -> 0
                               0,  1)) %>% 
  bind_cols(to_dummy(df_master$COD_FID, suffix = "label")) %>%
  mutate(STATUS_FID = as.numeric(STATUS_FID)) %>%
  mutate(AGE_FID = normalize(AGE_FID, range = c(0,1), method="range")) %>%
  select(-c("ID_EVENT_S", "ID_CLI", "ID_NEG", "W_PHONE", "TYP_CLI_ACCOUNT", "TYP_JOB",
            "EMAIL_PROVIDER_CLEAN", "PRV", "FLAG_PRIVACY_1", "FLAG_PRIVACY_2", "FLAG_DIRECT_MKT",
            "TYP_CAMP", "REGION", "COD_FID_STANDARD BIZ", "COD_FID", "TYP_CLI_FID", "NUM_FIDs"))

# matrice di correlazione
correlations <- cor(df_master_nn[,2:15])
corrplot(correlations, method="circle")

# rimozione var correlate
df_master_nn <- df_master_nn %>% select(-c(NUM_FAIL_PREV, NUM_SEND_PREV, NUM_OPEN_PREV, NUM_CLICK_PREV))

# train and test
train_nn <- sample(nrow(df_master_nn), 0.7*nrow(df_master_nn), replace = FALSE)
train_set_nn <- df_master_nn[train_nn,]
str(train_set_nn)
nrow(train_set_nn)
str(train_set_nn)
test_set_nn <- df_master_nn[-train_nn,]
table(train_set_nn$TARGET) # classe troppo sbilanciata per il train set
# sposto la colonna TARGET in ultima posizione in quanto downSample la rinomina
train_set_nn <- train_set_nn %>% 
  select(-TARGET, TARGET)
# bilanciamento per il train set
train_set_nn <- downSample(x = train_set_nn[, -ncol(train_set_nn)],
                           y = train_set_nn$TARGET)
train_set_nn <- rename(train_set_nn, "TARGET" = "Class")  
table(train_set_nn$TARGET)

#### MODELLO NEURAL NETWORK ####
# Per generare una rete neurale tramite la libreria caret devono essere
# specificati due parametri: 
#   size: che rappresenta il numero di unità nel livello nascosto, in questo caso è
#         stato settato a 4
#   decay: è il parametro di regolarizzazione per evitare un adattamento eccessivo.
#           in questo caso è stato settato a 0.4
rctrl <- trainControl(method = "none")
nnetGrid <-  expand.grid(size = 4,
                         decay = 0.4)
neural_net_1 <- caret::train(TARGET ~ .,
                             data=train_set_nn,    
                             method = "nnet",
                             maxit = 300,
                             tuneLength = 1,
                             trControl = rctrl,
                             tuneGrid = nnetGrid,
                             verbose =FALSE)

test_set_nn$TARGET <- as.factor(test_set_nn$TARGET)
predictions <- predict(neural_net_1, test_set_nn)

# Confusion Matrix
CM_nn_1 <- confusionMatrix(predictions, test_set_nn$TARGET)
print(CM_nn_1) # Accuracy 0.8278

# rappresentazione nn
plotnet(neural_net_1$finalModel, y_names = "IncomeLevel")
title("Graphical Representation of our Neural Network")

# CURVA ROC
roc_nn_1 <- prediction(predictions = as.numeric(predictions),
                       labels = as.numeric(test_set_nn$TARGET))
perf_roc_cv <- performance(roc_nn_1, measure = "tpr", x.measure = "fpr")
plot(perf_roc_cv, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_cv <- performance(roc_nn_1, measure = "auc")
perf.auc_cv@y.values[[1]] # AUC 0.7838511

# feature selection
garson(neural_net_1$finalModel)

# L'accuracy del modello risulta soddisfacente, è stata effettuata
# la feature selection per eliminare le variabili meno influenti
# anche in questo caso si è potuto osservare che la variabile più
# significativa è OPEN_RATE_PREV


#### NEURAL NETWORK FEATURE SELECTION ####
rctrl <- trainControl(method = "none")
nnetGrid <-  expand.grid(size = 3,
                         decay = 0.4)
neural_net_FS <- caret::train(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV + AGE_FID +
                                W_FAIL_PREV + W_SEND_PREV,
                              data=train_set_nn,    
                              method = "nnet",
                              maxit = 300,
                              tuneLength = 1,
                              trControl = rctrl,
                              tuneGrid = nnetGrid,
                              verbose =FALSE)

test_set_nn$TARGET <- as.factor(test_set_nn$TARGET)
predictions <- predict(neural_net_FS, test_set_nn)

# Confusion Matrix
CM_nn_fs <- confusionMatrix(predictions, test_set_nn$TARGET)
print(CM_nn_fs) # Accuracy 0.8255

# rappresentazione nn
plotnet(neural_net_FS$finalModel, y_names = "IncomeLevel")
title("Graphical Representation of our Neural Network")

# CURVA ROC
roc_nn_FS <- prediction(predictions = as.numeric(predictions),
                          labels = as.numeric(test_set_nn$TARGET))
perf_roc_cv <- performance(roc_nn_FS, measure = "tpr", x.measure = "fpr")
plot(perf_roc_cv, main  = "ROC curve for Linear Regression",
     col = "blue", lwd = 3)
# VALORE AUC
perf.auc_cv <- performance(roc_nn_1_FS, measure = "auc")
perf.auc_cv@y.values[[1]] # AUC 0.783899

# Il valore dell'accuracy non risulta essere diminuito notevolmente
# dunque è possibile utilizzre questo secondo modello.
# E' stata efettuata la cross-validation per valutare un eventuale
# overfitting.

#### CROSS-VALIDATION NEAURAL NET ####
rctrl_cv <- trainControl(method = "cv",
                         number = 3,
                         savePredictions = TRUE)
nnetGrid <-  expand.grid(size = 3,
                         decay = 0.4)
neural_net_cv <- caret::train(TARGET ~ OPEN_RATE_PREV + CLICK_RATE_PREV + AGE_FID +
                                W_FAIL_PREV + W_SEND_PREV,
                              data=df_master_nn,    
                              method = "nnet",
                              maxit = 300,
                              tuneLength = 1,
                              trControl = rctrl_cv,
                              tuneGrid = nnetGrid,
                              verbose =FALSE)

neural_net_cv$results

# Dalla cross-validation non risulta esserci overfitting nei dati
# dunque il modello risulta essere valido ed efficiente.
