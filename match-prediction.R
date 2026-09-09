library(dplyr)
library(naniar)
library(readr)
library(VIM)
library(xgboost)
library(naniar)
library(SHAPforxgboost)
library(pROC)

file_path <- "insert speed dating.csv here"
dati <- read_csv(file_path, locale = locale(encoding = "latin1"), show_col_types = FALSE)

#####Prevision##############


y=dati$match
colonne<-c('iid','id','gender','idg','condtn','wave','round','position','positin1',
           'order','partner','pid','samerace','age_o','race_o','pf_o_att',
           'age','field','field_cd','undergra','mn_sat','tuition',
           'race','imprace','imprelig','from','zipcode','income','goal','date','go_out',
           'career','career_c','sports','tvsports','exercise','dining','museums','art',
           'hiking','gaming','clubbing','reading','tv','theater','movies','concerts','music',
           'shopping','yoga','exphappy','expnum',
           'attr1_1' ,'sinc1_1','intel1_1','fun1_1','amb1_1','shar1_1',
           'attr2_1' ,'sinc2_1','intel2_1','fun2_1','amb2_1','shar2_1',
           'attr3_1' ,'sinc3_1','intel3_1','fun3_1','amb3_1',
           'attr4_1' ,'sinc4_1','intel4_1','fun4_1','amb4_1','shar4_1',
           'attr5_1' ,'sinc5_1','intel5_1','fun5_1','amb5_1')
x=dati[,colonne]
df=data.frame(y,x)
summary(df)

eliminabili<-c('position','positin1','field','undergra','mn_sat','tuition',
               'from','zipcode','income','career','expnum','attr4_1' ,'sinc4_1',
               'intel4_1','fun4_1','amb4_1','shar4_1',
               'attr5_1' ,'sinc5_1','intel5_1','fun5_1','amb5_1','career_c')      #not needed variable


#attr4_1 and attr5_1 were eliminated since they had too many missing
#also some rows have almost only missing 


df <- df %>% select(-all_of(eliminabili))
num_col=ncol(df)
df <- df[rowSums(is.na(df)) / num_col <= 0.5, ]


df$id[is.na(df$id)] = 22     #ID has only one missing, and it can be manually imputed

#a bit of feature engeneering since some var have been registered differently in each wave

dataset_pulito <- df %>%
  
  mutate(
    somma_1_1 = attr1_1 + sinc1_1 + intel1_1 + fun1_1 + amb1_1 + shar1_1,
    somma_2_1 = attr2_1 + sinc2_1 + intel2_1 + fun2_1 + amb2_1 + shar2_1
  ) %>%
  
  mutate(
    attr1_1 = ifelse(wave %in% 6:9, (attr1_1 / somma_1_1) * 100, attr1_1),
    sinc1_1 = ifelse(wave %in% 6:9, (sinc1_1 / somma_1_1) * 100, sinc1_1),
    intel1_1 = ifelse(wave %in% 6:9, (intel1_1 / somma_1_1) * 100, intel1_1),
    fun1_1  = ifelse(wave %in% 6:9, (fun1_1  / somma_1_1) * 100, fun1_1),
    amb1_1  = ifelse(wave %in% 6:9, (amb1_1  / somma_1_1) * 100, amb1_1),
    shar1_1 = ifelse(wave %in% 6:9, (shar1_1 / somma_1_1) * 100, shar1_1)
  ) %>%
  
  mutate(
    attr2_1 = ifelse(wave %in% 6:9, (attr2_1 / somma_2_1) * 100, attr2_1),
    sinc2_1 = ifelse(wave %in% 6:9, (sinc2_1 / somma_2_1) * 100, sinc2_1),
    intel2_1= ifelse(wave %in% 6:9, (intel2_1/ somma_2_1) * 100, intel2_1),
    fun2_1  = ifelse(wave %in% 6:9, (fun2_1  / somma_2_1) * 100, fun2_1),
    amb2_1  = ifelse(wave %in% 6:9, (amb2_1  / somma_2_1) * 100, amb2_1),
    shar2_1 = ifelse(wave %in% 6:9, (shar2_1 / somma_2_1) * 100, shar2_1)
  ) %>%
  
  select(-somma_1_1, -somma_2_1)

df<-dataset_pulito


agediff=df$age-df$age_o
df$age<-NULL
df$age_o<-NULL
df<-data.frame(df,agediff)

delusion_attr<-df$attr1_1-df$attr3_1


look_mismatch_self_reported<-rep(NA,nrow(df))
for(i in 1:nrow(df)){
  subject_iid <- df$iid[i]
  altro <- df$pid[i]
  if(is.na(altro)) {
    next
  }
  carat_partner <- df[df$iid == altro & !is.na(df$iid), ]
  scelta <- carat_partner[carat_partner$pid == subject_iid & !is.na(carat_partner$pid), ]
  if(nrow(scelta) > 0) {
    look_mismatch_self_reported[i] <- df$attr3_1[i] - scelta$attr3_1[1]
  }
}

sociability=abs(df$date+df$go_out-14)

df$date<-NULL
df$go_out<-NULL
df$attr1_1<-NULL
df$attr3_1<-NULL

df=data.frame(df,sociability,delusion_attr,look_mismatch_self_reported)

df$field_cd=factor(df$field_cd, exclude = NULL)
df$goal=factor(df$goal, exclude = NULL)
df$race=factor(df$race, exclude = NULL)
df$race_o=factor(df$race_o, exclude = NULL)
variabili<-model.matrix(~ field_cd + goal + race + race_o - 1, data=df)
df$field_cd<-df$goal<-df$race<-df$race_o<-NULL
df<-data.frame(df,variabili)
n=nrow(df)
set.seed(1234)
id_unici <- unique(df$iid)
train_id <- sample(id_unici, length(id_unici)/2)

train <- df[df$iid %in% train_id, ]
test <- df[!(df$iid %in% train_id), ]

eliminabili<-c('iid','id','wave','partner','pid')
train <- train %>% select(-all_of(eliminabili))
test <- test %>% select(-all_of(eliminabili))



y_train <- train$y
y_test <- test$y

X_train <- as.matrix(train %>% select(-y))
X_test  <- as.matrix(test  %>% select(-y))


dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test,  missing = NA)


numero_zeri <- sum(y_train == 0)
numero_uni <- sum(y_train == 1)
peso_bilanciamento <- numero_zeri / numero_uni


params <- list(
  objective   = "binary:logistic", 
  eval_metric = "auc",             
  max_depth   = 5,                 
  eta         = 0.05,                
  scale_pos_weight = peso_bilanciamento
)


evals_list <- list(train = dtrain, test = dtest)

modello_xgb <- xgb.train(
  params                = params,
  data                  = dtrain,
  nrounds               = 1000,           
  watchlist             = evals_list,
  early_stopping_rounds = 20,             
  print_every_n         = 10              
)



previsioni_prob <- predict(modello_xgb, dtest)
previsioni_classi <- ifelse(previsioni_prob > 0.5, 1, 0)

roc_obj <- roc(y_test, previsioni_prob)
auc(roc_obj)

plot(
  roc_obj,
  main = paste0("Curva ROC - Test Set (AUC = ", round(auc(roc_obj), 3), ")"),
  col = "#1c61b6",
  lwd = 2.5,
  legacy.axes = TRUE,     
  xlab = "Tasso Falsi Positivi (1 - Specificità)",
  ylab = "Tasso Veri Positivi (Sensibilità)",
  print.auc = TRUE,       
  grid = TRUE
)


matrice_confusione <- table(Previsto = previsioni_classi, Reale = y_test)
print("Matrice di Confusione:")
print(matrice_confusione)

# Final accuracy
accuratezza <- sum(diag(matrice_confusione)) / sum(matrice_confusione)
print(paste("Accuratezza finale sul Test Set:", round(accuratezza * 100, 2), "%"))


importanza <- xgb.importance(feature_names = colnames(X_train), model = modello_xgb)

# top 15 most important var
xgb.plot.importance(importance_matrix = importanza, top_n = 15, 
                    main = "Le 15 variabili più importanti per XGBoost")



shap_values <- shap.values(xgb_model = modello_xgb, X_train = X_train)

shap_long <- shap.prep(xgb_model = modello_xgb, X_train = X_train, top_n = 15)


shap.plot.summary(data_long = shap_long) #la mancante è date


#train the model again on the whole dataset

best_iter_str <- if (!is.null(modello_xgb$best_iteration) && length(modello_xgb$best_iteration) > 0) {
  modello_xgb$best_iteration
} else {
  1000
}



y_totale <- df$y
X_totale <- as.matrix(df %>% select(-y, -all_of(eliminabili)))


dmatrix_totale <- xgb.DMatrix(data = X_totale, label = y_totale, missing = NA)


numero_zeri_tot <- sum(y_totale == 0)
numero_uni_tot <- sum(y_totale == 1)
peso_bilanciamento_tot <- numero_zeri_tot / numero_uni_tot


params_finali <- list(
  objective        = "binary:logistic", 
  eval_metric      = "auc",             
  max_depth        = 5,                 
  eta              = 0.05,
  scale_pos_weight = peso_bilanciamento_tot 
)



modello_produzione <- xgb.train(
  params  = params_finali,
  data    = dmatrix_totale,
  nrounds = best_iter_str
)

#final model
