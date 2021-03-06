library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(purrr)
library(tidyr)
library(tibble)
library(magrittr)
library(lubridate)
library(data.table)
library(gridExtra)
library(countrycode)
library(ggExtra)
library(sqldf)
library(zoo)
library(forecast)
library(survMisc)
library(stats)

train <- read_csv("~/Downloads/all/train.csv")
test <- read_csv("~/Downloads/all/test.csv")

glimpse(train)

#Variables en format json : device, geoNetwork, totals, traffic_source => Changement

format_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

#bind_cols pour les mettre ensemble

changement_json <- . %>% 
  bind_cols(format_json(.$device)) %>%
  bind_cols(format_json(.$geoNetwork)) %>% 
  bind_cols(format_json(.$trafficSource)) %>% 
  bind_cols(format_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

train <- changement_json(train)
test <- changement_json(test)

###### transformation des phrases en NA ######

na_fction <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

train %<>% mutate_all(funs(ifelse(na_fction(.), NA, .)))

##### transfo variable  #####

train %<>%
  mutate(pageviews = ifelse(is.na(pageviews), 0L, as.integer(pageviews)),
         visitNumber =  visitNumber,
         newVisits = ifelse(newVisits == "1", 1L, 0L),
         bounces = ifelse(is.na(bounces), 0L, 1L),
         isMobile = ifelse(isMobile, 1L, 0L),
         adwordsClickInfo.isVideoAd = ifelse(is.na(adwordsClickInfo.isVideoAd), 0L, 1L),
         isTrueDirect = ifelse(is.na(isTrueDirect), 0L, 1L),
         browser_dev = str_c(browser, "_", deviceCategory),
         browser_os = str_c(browser, "_", operatingSystem),
         browser_chan = str_c(browser,  "_", channelGrouping),
         campaign_medium = str_c(campaign, "_", medium),
         chan_os = str_c(operatingSystem, "_", channelGrouping),
         country_adcontent = str_c(country, "_", adContent),
         country_medium = str_c(country, "_", medium),
         country_source = str_c(country, "_", source),
         dev_chan = str_c(deviceCategory, "_", channelGrouping),
         date = ymd(date),
         year = year(date),
         wday = wday(date),
         hour = hour(date),
         transactionRevenue = as.integer(transactionRevenue))

### Var cible : TransactionRevenue###

summary(train$transactionRevenue) #892138 NA : 98,7% de NA
#min : 0
#moy : 1,704e6 
#max : 2,313e10
train[is.na(train)] <- 0 #Assimilation des NA au 0

train %>% filter(transactionRevenue>0) %>% 
  summarise('Revenue par transaction'=sum(transactionRevenue)/(n()*1000000))

#Representation de la var TransactionRevenue sans les 0
#log pour lisser, bcp de variance
as_tibble(log(train$transactionRevenue[train$transactionRevenue>0])) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill="orange") 

#Détail des transactions par rapport aux temps

setDT(train, keep.rownames=TRUE, key=NULL, check.names=FALSE)

plot_temps_transac <- train[, .(revenue = sum(transactionRevenue, na.rm=TRUE)), by=date] %>%
  ggplot(aes(x=date, y=revenue)) + 
  geom_line() +
  labs(
    x='Date',
    y='TransactionRevenue',
    title='TransactionRevenue par jour'
  )

plot(plot_temps_transac)

#Nombre de visite par jour

temps_visite <- train[, .(visite = .N), by=date] %>%
  ggplot(aes(x=date, y=visite))+
  geom_line() +
  labs(
    x='Date',
    y='Nombre de visite',
    title='Visite par jour'
  )
``

#Fréquentation dans la journée

train$visitStartTime <- as_datetime(train$visitStartTime, tz="Europe/Paris") #

transaction_journee <- train[, .(revenue=sum(transactionRevenue, na.rm =T)), by = (heure=hour(visitStartTime))] %>%
  ggplot(aes(x = heure , y = revenue / 10000)) +
  geom_line(color = 'steelblue', size = 1) +
  labs(
    x = 'Heure de la journée',
    y = 'Visite',
    title = 'Frequentation dans la journée'
  )

plot(transaction_journee)

#Transactions dans la journée

transaction_journee <- train[, .(transactionRevenue, heure_visite = hour(visitStartTime))][
  , .(revenue = sum(transactionRevenue, na.rm =T)), by = heure_visite] %>%
  ggplot(aes(x = heure_visite, y = revenue)) +
  geom_line(color = 'steelblue', size = 1) +
  labs(
    x = 'Heure de la journée',
    y = 'Revenue des transactions ',
    title = 'Revenue dans la journée'
  )

plot(transaction_journee)

### Evolution du revenue dans la semaine #####

transaction_semaine <- train[,.(transactionRevenue,jour_visite=weekdays(date))][
  , .(revenue=sum(transactionRevenue,na.rm = T)/10000000), by = jour_visite] %>% 
  ggplot(aes(x=jour_visite,y=revenue)) + 
  geom_bar(stat="identity") +
  labs(
    x = 'Jour',
    y = 'Visite',
    title = 'Revenue dans la semaine'
  )

plot(transaction_semaine)


### Evolution de la fréquentation dans la semaine #####

freq_semaine <- train[,.(freq=.N , jour_visite=weekdays(date))][
  , by = jour_visite] %>% 
  ggplot(aes(x=jour_visite,y=freq)) + 
  geom_bar(stat="identity") +
  labs(
    x = 'Jour',
    y = 'Visite',
    title = 'Frequentation dans la semaine'
  )

plot(freq_semaine)


### Evolution de la fréquentation dans le mois #####

freq_mois <- train[,.(freq=.N , mois_visite=months.Date(date))][
  , by = mois_visite] %>% 
  ggplot(aes(x=mois_visite,y=freq)) + 
  geom_bar(stat="identity") +
  labs(
    x = 'mois',
    y = 'Visite',
    title = 'Frequentation dans le mois '
  )

plot(freq_mois)

### Evolution du revenue dans le mois #####

transaction_mois <- train[,.(transactionRevenue,mois_transac=months.Date(date))][
  , .(revenue=sum(transactionRevenue,na.rm = T)/10000000), by = mois_transac] %>% 
  ggplot(aes(x=mois_transac,y=revenue)) + 
  geom_bar(stat="identity") +
  labs(
    x = 'Mois',
    y = 'Visite',
    title = 'Revenue dans le mois'
  )

plot(transaction_mois)

### serie temp - prédiction revenue en fonction du temps : MARCHE PAS ###

rev2 <- train %>%
  group_by(date) %>%
  summarise(moy_rev = mean(transactionRevenue)) %>%
  with(zoo(moy_rev, order.by = date)) 

rev2.ts <- auto.arima(rev2)

forecast(rev2.ts) %>% 
  plot(xlim=c(17375,17390))

plot(forecast(stl(ts(rev2, frequency =7), s.window = 7)))


#####################################################
####################MODELISATION#####################
#####################################################



### En 2 temps : ###
### 1 etape : 0 = pas de vente, 1 = vente ###
### 2 etape : Si une vente, combien ? ###

#1#

#Création var : vente oui/non#

vente = rep(0,nrow(train))
train <- cbind(train,vente)

train[, vente := ifelse(train$transactionRevenue > 0, 1, 0)]

# On réduit notre jeu de données pour tester nos modèles

train_model <- train[]

#Decomposition de l'heure pour des var catégorielles et changement en facteur pour les autres

train_model %<>%
  mutate(
    annee = factor(year(date)),
    mois = factor(month(date)),
    heure = factor(hour(as_datetime(visitStartTime)))) %>%
      select(-date, -fullVisitorId, -visitId, -sessionId, -hits, -visitStartTime) %>%
    mutate_if(is.character, factor)

train_model2 <- train_model[,lapply(train_model, n_distinct) %>% unlist() %>% as.vector() < 50 | 
                             !(lapply(train_model, is.factor) %>% unlist() %>% as.vector())]
#ajout de la var vente :

vente_fact <- factor(train_model$vente)

train_model2 <- cbind(train_model2,vente_fact)



##RF##

library(randomForest)

w = sample(which(train_model2$vente==0),size = sum(train_model2$vente == 1))

indexes = c(w, which(train_model2$vente==1))

res.rf2 <- randomForest(vente_fact ~ channelGrouping+visitNumber+operatingSystem+ isMobile+ deviceCategory+ continent+ subContinent+ campaign+ medium+
                          isTrueDirect+ adContent+ campaignCode+ adwordsClickInfo.page+ adwordsClickInfo.slot+ adwordsClickInfo.adNetworkType+
                          adwordsClickInfo.isVideoAd+ pageviews+ bounces+ newVisits+ campaign_medium+ dev_chan+ year+ wday+  annee+ mois+
                          heure, data=train_model2[indexes,])

preds = predict(res.rf2, train_model2)

lmmodel <- randomForest(transactionRevenue ~ channelGrouping+visitNumber+operatingSystem+ isMobile+ deviceCategory+ continent+ subContinent+ campaign+ medium+
                          isTrueDirect+ adContent+ campaignCode+ adwordsClickInfo.page+ adwordsClickInfo.slot+ adwordsClickInfo.adNetworkType+
                          adwordsClickInfo.isVideoAd+ pageviews+ bounces+ newVisits+ campaign_medium+ dev_chan+ year+ wday+  annee+ mois+
                          heure, data = train_model2[which(train_model2$transactionRevenue>0),])

## 4-6% d'erreur ##
## On regarde la prédiction de revenue ##



data_vente <- train[which(train$transactionRevenue>0)]

data_vente$transactionRevenue <- log1p(data_vente$transactionRevenue)

mean(data_vente$transactionRevenue)

lmmodel <- randomForest(transactionRevenue ~ channelGrouping+visitNumber+operatingSystem+ isMobile+ deviceCategory+ continent+ subContinent+ campaign+ medium+
                          isTrueDirect+ adContent+ campaignCode+ adwordsClickInfo.page+ adwordsClickInfo.slot+ adwordsClickInfo.adNetworkType+
                          adwordsClickInfo.isVideoAd+ pageviews+ bounces+ newVisits+ campaign_medium+ dev_chan+ year+ wday, data = data_vente)
