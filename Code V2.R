# Nettoyer l'environnement
rm(list = ls())
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(dev.off(), silent = TRUE)

############################################################
# 0) Packages
############################################################

# À faire une fois si besoin :
# install.packages(c("dplyr","lubridate","zoo","ggplot2","scales","tidyr",
#                    "eurostat","urca","tseries","quantmod","tibble",
#                    "mFilter","vars","forecast","readxl"))

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(tidyr)
library(eurostat)
library(urca)
library(tseries)
library(quantmod)
library(tibble)
library(mFilter)
library(vars)
library(forecast)
library(readxl)

# Réinitialiser les marges graphiques par défaut
par(mar = c(5, 4, 4, 2) + 0.1)

# ⚠️ Forcer les fonctions dplyr après chargement de quantmod
#    (quantmod écrase select, filter, lag de dplyr)
select    <- dplyr::select
filter    <- dplyr::filter
lag       <- dplyr::lag
summarise <- dplyr::summarise
mutate    <- dplyr::mutate
transmute <- dplyr::transmute
rename    <- dplyr::rename


##################################################################
# 1) Collecte du PIB réel annuel UE27 (Eurostat)
##################################################################

gdp_eu_a <- get_eurostat_json(
  "nama_10_gdp",
  filters = list(
    geo     = "EU27_2020",
    na_item = "B1GQ",
    unit    = "CLV10_MEUR"
  ),
  time_format = "raw"
) %>%
  dplyr::mutate(
    date     = eurotime2date(time),
    gdp_real = as.numeric(values)
  ) %>%
  dplyr::filter(date >= as.Date("1995-01-01"),
                date <= as.Date("2025-12-31")) %>%
  dplyr::arrange(date) %>%
  dplyr::transmute(
    date,
    gdp_real,
    gdp_growth = gdp_real / dplyr::lag(gdp_real, 1) - 1
  )

head(gdp_eu_a)


##################################################################
# 2) Séries log et tendances déterministes
##################################################################

gdp_eu_a <- gdp_eu_a %>%
  dplyr::mutate(
    log_gdp = log(gdp_real),
    t  = dplyr::row_number(),
    t2 = t^2
  )

fit_lin  <- lm(log_gdp ~ t,      data = gdp_eu_a)
fit_quad <- lm(log_gdp ~ t + t2, data = gdp_eu_a)

gdp_eu_a <- gdp_eu_a %>%
  dplyr::mutate(
    trend_lin  = predict(fit_lin,  newdata = gdp_eu_a),
    trend_quad = predict(fit_quad, newdata = gdp_eu_a)
  )

ggplot(gdp_eu_a, aes(x = date)) +
  geom_line(aes(y = log_gdp,     color = "Log(PIB réel)"),       alpha = 0.7) +
  geom_line(aes(y = trend_lin,   color = "Tendance linéaire")) +
  geom_line(aes(y = trend_quad,  color = "Tendance quadratique")) +
  labs(title = "Tendances déterministes du PIB réel UE27",
       y = "log(PIB réel)", x = "Année") +
  theme_minimal() +
  theme(legend.position = "bottom")


##################################################################
# 3) Filtres cycliques
##################################################################

# HP
hp <- hpfilter(gdp_eu_a$log_gdp, freq = 100)
gdp_eu_a$hp_trend <- hp$trend
gdp_eu_a$hp_cycle <- hp$cycle

# BK
bk <- bkfilter(gdp_eu_a$log_gdp, pl = 2, pu = 5, drift = FALSE)
gdp_eu_a$bk_trend <- bk$trend
gdp_eu_a$bk_cycle <- bk$cycle

# CF
cf <- cffilter(gdp_eu_a$log_gdp, pl = 2, pu = 5, root = TRUE, drift = FALSE, type = "asymmetric")
gdp_eu_a$cf_trend <- cf$trend
gdp_eu_a$cf_cycle <- cf$cycle

# Hamilton
h <- 2
df_h <- gdp_eu_a %>%
  dplyr::mutate(
    y_lead = dplyr::lead(log_gdp, h),
    y_lag0 = log_gdp,
    y_lag1 = dplyr::lag(log_gdp, 1),
    y_lag2 = dplyr::lag(log_gdp, 2)
  ) %>%
  dplyr::filter(!is.na(y_lead), !is.na(y_lag2))

ham_fit <- lm(y_lead ~ y_lag0 + y_lag1 + y_lag2, data = df_h)

ham_tr <- df_h %>%
  dplyr::transmute(
    date      = date,
    ham_trend = predict(ham_fit),
    ham_cycle = resid(ham_fit)
  )

gdp_eu_a <- gdp_eu_a %>%
  dplyr::left_join(ham_tr, by = "date")

# ---- Comparaison des tendances ----
trend_long <- gdp_eu_a %>%
  dplyr::select(date, hp_trend, bk_trend, cf_trend, ham_trend) %>%
  tidyr::pivot_longer(-date, names_to = "Méthode", values_to = "trend") %>%
  dplyr::mutate(Méthode = dplyr::recode(Méthode,
                                        hp_trend  = "Tendance HP",
                                        bk_trend  = "Tendance BK",
                                        cf_trend  = "Tendance CF",
                                        ham_trend = "Tendance Hamilton"
  ))

print(
  ggplot() +
    geom_line(data = gdp_eu_a,   aes(date, log_gdp), alpha = 0.3) +
    geom_line(data = trend_long, aes(date, trend, color = Méthode, linetype = Méthode), linewidth = 1) +
    labs(title = "Tendances du PIB réel UE27 : HP, BK, CF, Hamilton",
         x = NULL, y = "log(PIB réel)", color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
)

# ---- Comparaison des cycles ----
cycle_long <- gdp_eu_a %>%
  dplyr::transmute(
    date,
    `Cycle HP`       = hp_cycle,
    `Cycle BK`       = bk_cycle,
    `Cycle CF (CK)`  = cf_cycle,
    `Cycle Hamilton` = ham_cycle
  ) %>%
  tidyr::pivot_longer(-date, names_to = "Méthode", values_to = "cycle")

print(
  ggplot(cycle_long, aes(date, cycle, color = Méthode, linetype = Méthode)) +
    geom_line() +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = "Cycles du PIB réel UE27 : HP, BK, CF, Hamilton",
         x = NULL, y = "Écart en log (approx. %)", color = NULL, linetype = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
)


##################################################################
# 4) Graphiques PIB réel et log(PIB)
##################################################################

p1 <- ggplot(gdp_eu_a, aes(x = date, y = gdp_real / 1000)) +
  geom_line(color = "blue") +
  labs(title = "PIB réel annuel UE27", x = "Année", y = "PIB réel (milliards €)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

p2 <- ggplot(gdp_eu_a, aes(x = date, y = log_gdp)) +
  geom_line(color = "red") +
  labs(title = "Log(PIB réel annuel)", x = "Année", y = "log(PIB réel)") +
  theme_minimal()

print(p1)
print(p2)


##################################################################
# 5) ACF et différenciation
##################################################################

log_gdp_ts <- ts(gdp_eu_a$log_gdp, start = year(min(gdp_eu_a$date)), frequency = 1)
dlog_gdp   <- diff(log_gdp_ts)

ggAcf(log_gdp_ts, lag.max = 40) +
  ggtitle("ACF : log(PIB réel annuel UE27)")


##################################################################
# 6) Tests de stationnarité — PIB
##################################################################

log_gdp_ts <- ts(na.omit(gdp_eu_a$log_gdp), start = 1995, frequency = 1)

# En niveau
adf_lvl  <- ur.df(log_gdp_ts, type = "drift", selectlags = "AIC")
summary(adf_lvl)

pp_lvl   <- pp.test(log_gdp_ts)
pp_lvl

kpss_lvl <- tseries::kpss.test(log_gdp_ts, null = "Level")
kpss_lvl

# En différence
dlog_gdp <- diff(log_gdp_ts)

ggAcf(na.omit(dlog_gdp), lag.max = 40) +
  ggtitle("ACF : Δlog(PIB)")

adf_diff <- ur.df(na.omit(dlog_gdp), type = "drift", selectlags = "AIC")
summary(adf_diff)

pp_diff  <- pp.test(na.omit(dlog_gdp))
pp_diff

kpss_diff <- tseries::kpss.test(na.omit(dlog_gdp), null = "Level")
kpss_diff


##################################################################
# 7) Chargement proxy vert + construction
##################################################################

rd <- read_excel("C:/Users/alexa/OneDrive/Bureau/Fac/Macroeconometrie/Investissement.xlsx", sheet = "R&D")
brevets <- read_excel("C:/Users/alexa/OneDrive/Bureau/Fac/Macroeconometrie/Investissement.xlsx", sheet = "brevets")

data_proxy <- rd %>%
  dplyr::inner_join(brevets, by = c("TIME_PERIOD", "geo")) %>%
  dplyr::rename(
    rd_valeur      = `valeur normalisée`,
    brevets_valeur = `obs normalisées brevets`
  ) %>%
  dplyr::mutate(
    rd_norm      = as.numeric(scale(rd_valeur)),
    brevets_norm = as.numeric(scale(brevets_valeur)),
    proxy        = 0.5 * rd_norm + 0.5 * brevets_norm
  ) %>%
  dplyr::group_by(TIME_PERIOD) %>%
  dplyr::summarise(proxy = mean(proxy, na.rm = TRUE), .groups = "drop")

pib <- gdp_eu_a %>%
  dplyr::transmute(TIME_PERIOD = lubridate::year(date), PIB = gdp_real)

data_final <- data_proxy %>%
  dplyr::inner_join(pib, by = "TIME_PERIOD") %>%
  dplyr::filter(!is.na(proxy), !is.na(PIB))

# Régression OLS
model <- lm(PIB ~ proxy, data = data_final)
summary(model)

ggplot(data_final, aes(x = proxy, y = PIB)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Effet du proxy Innovation sur le PIB UE27",
       x = "Proxy Innovation", y = "PIB réel UE27") +
  theme_minimal()


###################################################################
# 8) Passage en log pour le VAR/VECM
###################################################################

data_final <- data_final %>%
  dplyr::arrange(TIME_PERIOD) %>%
  dplyr::mutate(
    log_PIB   = log(PIB),
    log_proxy = log(proxy - min(proxy) + 1)   # décalage si valeurs négatives
  )

start_yr <- min(data_final$TIME_PERIOD)

log_PIB_ts   <- ts(data_final$log_PIB,   start = start_yr, frequency = 1)
log_proxy_ts <- ts(data_final$log_proxy, start = start_yr, frequency = 1)


###################################################################
# 9) Tests de stationnarité — Proxy vert
###################################################################

cat("\n===== ADF — log(Proxy vert) en niveau =====\n")
adf_proxy_lvl <- ur.df(log_proxy_ts, type = "drift", selectlags = "AIC")
summary(adf_proxy_lvl)

cat("\n===== ADF — Δlog(Proxy vert) =====\n")
adf_proxy_diff <- ur.df(diff(log_proxy_ts), type = "drift", selectlags = "AIC")
summary(adf_proxy_diff)

pp.test(log_proxy_ts)
tseries::kpss.test(log_proxy_ts, null = "Level")

ggAcf(log_proxy_ts, lag.max = 20) +
  ggtitle("ACF : log(Proxy vert UE27)")

ggAcf(diff(log_proxy_ts), lag.max = 20) +
  ggtitle("ACF : Δlog(Proxy vert)")


###################################################################
# 10) Test Engle–Granger (cointégration bivariée)
###################################################################

eg_reg   <- lm(log_PIB_ts ~ log_proxy_ts)
summary(eg_reg)

resid_eg <- residuals(eg_reg)

cat("\n===== ADF — Résidus Engle–Granger =====\n")
adf_eg <- ur.df(resid_eg, type = "none", lags = 4)
summary(adf_eg)


###################################################################
# 11) Test de Johansen et sélection du rang de cointégration
###################################################################

Y <- cbind(log_PIB_ts, log_proxy_ts)
colnames(Y) <- c("log_PIB", "log_Proxy")

lag_select <- vars::VARselect(Y, lag.max = 8, type = "const")
lag_select
lag_select$selection

p <- as.numeric(lag_select$selection["AIC(n)"])
p

cat("\n===== Test de Johansen (trace) =====\n")
joh_trace <- urca::ca.jo(
  Y,
  type  = "trace",
  ecdet = "const",
  K     = p
)
summary(joh_trace)


###################################################################
# 12) Estimation du VECM (rang r = 1)
###################################################################

vecm <- urca::cajorls(joh_trace, r = 1)
summary(vecm$rlm)

cat("\n===== Vecteur de cointégration =====\n")
joh_trace@V


###################################################################
# 13) Représentation VAR du VECM — IRF et FEVD
###################################################################

vecm_var <- vars::vec2var(joh_trace, r = 1)

irf_gdp <- vars::irf(
  vecm_var,
  impulse  = "log_Proxy",
  response = "log_PIB",
  n.ahead  = 10,
  boot     = TRUE
)

irf_proxy <- vars::irf(
  vecm_var,
  impulse  = "log_PIB",
  response = "log_Proxy",
  n.ahead  = 10,
  boot     = TRUE
)

plot(irf_gdp,   main = "IRF : Choc sur log(Proxy vert) → log(PIB UE27)")
plot(irf_proxy, main = "IRF : Choc sur log(PIB) → log(Proxy vert)")

dev.new()  # ouvre une nouvelle fenêtre graphique indépendante
plot(irf_gdp, main = "IRF : Choc sur log(Proxy vert) → log(PIB UE27)")
fevd_res <- vars::fevd(vecm_var, n.ahead = 10)
plot(fevd_res)


###################################################################
# 14) Tests de validation des résidus du VECM
###################################################################

serial_test <- vars::serial.test(vecm_var, lags.pt = 5, type = "PT.asymptotic")
serial_test

norm_test <- vars::normality.test(vecm_var)
norm_test

arch_test <- vars::arch.test(vecm_var, lags.multi = 5)
arch_test
