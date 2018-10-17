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


test <- read_csv("~/Downloads/all/test.csv")
train <- read_csv("~/Downloads/all/train.csv")

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


###### transformation des phrases en NA ######

na_fction <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

train %<>% mutate_all(funs(ifelse(na_fction(.), NA, .)))

##### transfo variable  #####

train %<>%
  mutate(date = ymd(date),
         hits = as.integer(hits),
         pageviews = as.integer(pageviews),
         bounces = as.integer(bounces),
         newVisits = as.integer(newVisits),
         transactionRevenue = as.numeric(transactionRevenue))

### Var cible : TransactionRevenue###

summary(train$transactionRevenue) #892138 NA : 98,7% de NA
#min : 0
#moy : 1,704e6 
#max : 2,313e10
train[is.na(train)] <- 0 #Assimilation des NA au 0

#Representation de la var TransactionRevenue sans les 0
#log pour lisser, bcp de variance
as_tibble(log(train$transactionRevenue[train$transactionRevenue>0])) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill="orange") 

#On peut regarder s'il y a plusieurs sessions pour un ID

table_test <- train[1:100000,]

sqldf('select distinct fullVisitorId, count(*) from table_test group by fullVisitorId')



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

#On peut regarder le nombre de visite par jour

temps_visite <- train[, .(visite = .N), by=date] %>%
  ggplot(aes(x=date, y=visite))+
  geom_line() +
  labs(
    x='Date',
    y='Nombre de visite',
    title='Visite par jour'
  )
plot(temps_visite, plot_temps_transac)

#on peut regarder quand arrive la fréquentation dans la journée

train$visitStartTime <- as_datetime(train$visitStartTime) #

transaction_journee <- train[, .(heure_visite = hour(visitStartTime))][
  , .(revenue = sum(train$transactionRevenue, na.rm=TRUE)), by = heure_visite] %>%
  ggplot(aes(x = heure_visite, y = revenue / 10000)) +
  geom_line(color = 'steelblue', size = 1) +
  labs(
    x = 'Heure de la journée',
    y = 'Visite',
    title = 'Frequentation dans la journée'
  )

plot(transaction_journee)

#De même pour le cumule des transactions

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


