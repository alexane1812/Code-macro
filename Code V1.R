
# Nettoyer l'environnement
rm(list = ls())

# Effacer les graphiques précédents
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

# Installation des packages (à faire une fois seulement)
# install.packages(c("dplyr","lubridate","zoo","ggplot2","scales","tidyr","eurostat","fredr"))

# --- Packages
library(dplyr)        # Manipuler des data frames
library(lubridate)    # Gérer les dates
library(zoo)          # Gérer les trimestres
library(ggplot2)       # Pour tracer les graphiques
library(scales)        # Pour formater les axes en %
library(tidyr)         # Pour pivoter en format “long”
library(eurostat)     # Interroger Eurostat
library(fredr)        # Interroger FRED
library(urca)     
library(tseries) 


#Importation des données
fredr_set_key("830541de942329682ca9b61471d13a99")  

gdp_r <- fredr(series_id = "CLVMNACSCAB1GQEU272020") %>%
  transmute(
    date,
    gdp_real = value,
  )

#Conversion en série temporelle trimestrielle
start_year <- year(min(gdp_r$date))
start_qtr  <- quarter(min(gdp_r$date))

gdp_ts <- ts(gdp_r$gdp_real, frequency = 4,
             start = c(start_year, start_qtr))



#Visualisation
df <- gdp_r %>% mutate(log_gdp = log(gdp_real))

p1 <- ggplot(df, aes(date, gdp_real)) +
  geom_line() +
  labs(title = "PIB réel – Niveau",
       x = "Date", y = "PIB réel") +
  theme_minimal()

p2 <- ggplot(df, aes(date, log_gdp)) +
  geom_line() +
  labs(title = "Log(PIB réel) – Niveau",
       x = "Date", y = "log(PIB)") +
  theme_minimal()

print(p1); print(p2)

ggAcf(log(gdp_ts), lag.max = 40) +
  ggtitle("ACF : log(PIB) – Niveau")

#Test de stationnarité

# Tests de racine unitaire
adf_lvl  <- ur.df(log(gdp_ts), type = "drift", selectlags = "AIC")
summary(adf_lvl)

pp_lvl   <- pp.test(log(gdp_ts))
pp_lvl

kpss_lvl <- kpss.test(log(gdp_ts), null = "Level")
kpss_lvl


#test ADF et KPSS indique une non stationnarité, on conclue à la présence d'une racine unitaire

#Transformation vers la stationnarité 
dlog_gdp <- diff(log(gdp_ts))

# Graphique
df_dlog <- data.frame(
  date = gdp_r$date[-1],
  dlog = as.numeric(dlog_gdp)
)

p3 <- ggplot(df_dlog, aes(date, dlog)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Variation trimestrielle log(PIB)",
       x = "Date", y = "Δlog(PIB)") +
  theme_minimal()

print(p3)

# ACF
ggAcf(na.omit(dlog_gdp), lag.max = 40) +
  ggtitle("ACF : Δlog(PIB)")

#On teste la première différence
adf_diff  <- ur.df(na.omit(dlog_gdp), type = "drift", selectlags = "AIC")
summary(adf_diff)

pp_diff   <- pp.test(na.omit(dlog_gdp))
pp_diff

kpss_diff <- kpss.test(na.omit(dlog_gdp), null = "Level")
kpss_diff



##conclusion : Log PIB réel pas stationnaire et présente une racine unitaire (n'est ps stationnaire en niveau)
#La premiere différence du log PIB est stationnaire
#La serie est donc I(1)




