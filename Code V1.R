
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

#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------

# Nettoyer l'environnement
rm(list = ls())

# Effacer les graphiques précédents
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

# Installation des packages (à faire une fois seulement)
# install.packages(c("dplyr","lubridate","zoo","ggplot2","scales","tidyr","eurostat","fredr","forecast","tseries","urca","mFilter", "quantmod", "tibble"))

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
# --- Packages en plus
library(quantmod)
library(tibble)
library(mFilter)
library(forecast)

#--- 1) Données : FRED - Real GDP UE, Seasonally Adjusted - 1995-2025
getSymbols("CLVMNACSCAB1GQEU272020", src = "FRED") # Real Gross Domestic Product for European Union (27 Countries from 2020)
fr <- tibble(
  date  = as.Date(index(CLVMNACSCAB1GQEU272020)),
  ratio = as.numeric(CLVMNACSCAB1GQEU272020)/100        # conversion en proportion (plus en % du PIB)
) |>
  arrange(date) |>                          # Optionnel : ordonner chronologiquement (bonne pratique avant graphes/joins)
  distinct(date, .keep_all = TRUE) |>       # Optionnel : supprimer les doublons de dates (garder la 1re occurrence)
  mutate(
    lratio = log(ratio),
    yq     = as.yearqtr(date)
  )
# Séries trimestrielles (niveaux et logs)
ratio_ts <- ts(fr$ratio, start = c(year(min(fr$date)), quarter(min(fr$date))), frequency = 4)
lratio_ts <- ts(fr$lratio, start = c(year(min(fr$date)), quarter(min(fr$date))), frequency = 4)

#--- 2) Analyse des tendances déterministes :

df <- fr |> 
  mutate(t = row_number(), 
         t2 = t^2)
  
fit_lin  <- lm(lratio ~ t, data = df)
fit_quad <- lm(lratio ~ t + t2, data = df)

df <- df |> 
  mutate(trend_lin  = as.numeric(predict(fit_lin)),
         trend_quad = as.numeric(predict(fit_quad)))
  
print(
  ggplot(df, aes(date)) +
    geom_line(aes(y = lratio, color = "Données"), alpha = 0.75) +
    geom_line(aes(y = trend_lin, color = "Tendance linéaire déterministe"), linewidth = 1) + 
    geom_line(aes(y = trend_quad, color = "Tendance quadratique déterministe"), 
              linewidth = 1, linetype = 1) +
    labs(x = NULL, y = "log(ratio)", color = NULL) +
    theme(legend.position = "bottom")
)

#--- 3) Filtres :

# (1) HP (λ = 1600, trimestriel)
hp <- hpfilter(lratio_ts, freq = 1600)
df$hp_trend <- as.numeric(hp$trend)
df$hp_cycle <- as.numeric(hp$cycle)

# (2) BK (6-32 trimestres ; rogne les extremités via nfix)
bk <- bkfilter(lratio_ts, pl = 6, pu = 32, nfix = 12, drift = FALSE)
df$bk_trend <- as.numeric(bk$trend)
df$bk_cycle <- as.numeric(bk$cycle)

# (3) CF asymétrique (sans rognage). Utiliser drift = FALSE pour éviter une tendance trop faible en fin d'échantillon.
cf <- cffilter(lratio_ts, pl = 6, pu = 32, root = TRUE, drift = FALSE, type = "asymmetric")
df$cf_trend <- as.numeric(cf$trend)
df$cf_cycle <- as.numeric(cf$cycle)

# (4) Hamilton (2018) : h = 8, p = 4 (tendance alignée à t+h ; résidu = cycle à t+h)
h <- 8 ; p <- 4 
df_h <- df |>
  mutate(
    date_y = dplyr::lead(date, h),
    y_lead = dplyr::lead(lratio, h),
    y_lag0 = lratio,
    y_lag1 = dplyr::lag(lratio, 1),
    y_lag2 = dplyr::lag(lratio, 2),
    y_lag3 = dplyr::lag(lratio, 3)
  ) |>
  filter(!is.na(y_lead), !is.na(y_lag3))

ham_fit <- lm(y_lead ~ y_lag0 + y_lag1 + y_lag2 + y_lag3, data = df_h)
ham_pred <- as.numeric(predict(ham_fit, newdata = df_h))
ham_res <- as.numeric(resid(ham_fit))

ham_tr <- df_h |>
  transmute(date = date_y, ham_trend = ham_pred, ham_cycle = ham_res)

df <- df |>
  mutate(date = as.Date(date)) |>
  left_join(ham_tr, by = "date")


# ---- Comparaison des tendances ----
trend_long <- df |>
  select(date, hp_trend, bk_trend, cf_trend, ham_trend) |>
  pivot_longer(-date, names_to = "Méthode", values_to = "trend") |>
  mutate(Méthode = dplyr::recode(Méthode,
                                 hp_trend  = "Tendance HP",
                                 bk_trend  = "Tendance BK",
                                 cf_trend  = "Tendance CF",
                                 ham_trend = "Tendance Hamilton"))

print(
ggplot() +
  geom_line(data = df, aes(date, lratio), alpha = 0.3) +
  geom_line(data = trend_long, aes(date, trend, color = Méthode, linetype = Méthode), linewidth = 1) +
  labs(title = "Tendances de log(ratio crédit/PIB) : HP, BK, CF, Hamilton (BIS via FRED)",
       x = NULL, y = "log(ratio)", color = NULL, linetype = NULL) +
  theme(legend.position = "bottom")
)


# ---- Comparaison des cycles (≈ % car en unités log) ----
cycle_long <- df |>
  transmute(date,
            `Cycle HP`        = hp_cycle,
            `Cycle BK`        = bk_cycle,
            `Cycle CF (CK)`   = cf_cycle,
            `Cycle Hamilton`  = ham_cycle) |>
  pivot_longer(-date, names_to = "Méthode", values_to = "cycle")

print(
ggplot(cycle_long, aes(date, cycle, color = Méthode, linetype = Méthode)) +
  geom_line() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "Cycles de log(ratio crédit/PIB) : HP, BK, CF (CK), Hamilton",
       x = NULL, y = "Pourcentage (approx., écart en log)", color = NULL, linetype = NULL) +
  theme(legend.position = "bottom")
)


