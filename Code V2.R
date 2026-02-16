# ============================================================
# Master 1 APE - MPE
# PIB réel UE : Filtres de tendance + Stationnarité
# ============================================================

# Nettoyer l'environnement
rm(list = ls())

# Effacer les graphiques précédents
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)


# Packages

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(tidyr)
library(eurostat)
library(fredr)
library(urca)
library(tseries)
library(quantmod)
library(tibble)
library(mFilter)
library(forecast)



# Données FRED
getSymbols("CLVMNACSCAB1GQEU272020", src = "FRED")

fr <- tibble(
  date  = as.Date(index(CLVMNACSCAB1GQEU272020)),
  ratio = as.numeric(CLVMNACSCAB1GQEU272020)/100
) |>
  arrange(date) |>
  distinct(date, .keep_all = TRUE) |>
  mutate(
    lratio = log(ratio),
    yq     = as.yearqtr(date)
  )

ratio_ts <- ts(fr$ratio,
               start = c(year(min(fr$date)),
                         quarter(min(fr$date))),
               frequency = 4)

lratio_ts <- ts(fr$lratio,
                start = c(year(min(fr$date)),
                          quarter(min(fr$date))),
                frequency = 4)

##Partie 1 : Filtres

#Tendances déterministes
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
    geom_line(aes(y = trend_lin, color = "Tendance linéaire")) + 
    geom_line(aes(y = trend_quad, color = "Tendance quadratique")) +
    theme(legend.position = "bottom")
)

# Filtres 

# HP
hp <- hpfilter(lratio_ts, freq = 1600)
df$hp_trend <- as.numeric(hp$trend)
df$hp_cycle <- as.numeric(hp$cycle)

# BK
bk <- bkfilter(lratio_ts, pl = 6, pu = 32, nfix = 12, drift = FALSE)
df$bk_trend <- as.numeric(bk$trend)
df$bk_cycle <- as.numeric(bk$cycle)

# CF
cf <- cffilter(lratio_ts, pl = 6, pu = 32,
               root = TRUE, drift = FALSE,
               type = "asymmetric")
df$cf_trend <- as.numeric(cf$trend)
df$cf_cycle <- as.numeric(cf$cycle)

# Hamilton
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

ham_tr <- df_h |>
  transmute(date = date_y,
            ham_trend = as.numeric(predict(ham_fit)),
            ham_cycle = as.numeric(resid(ham_fit)))

df <- df |> left_join(ham_tr, by = "date")

#Partie 2: Stationnarité
# Visualisation
df_station <- fr |> mutate(log_gdp = log(ratio))

p1 <- ggplot(df_station, aes(date, ratio)) +
  geom_line() +
  labs(title = "PIB réel – Niveau",
       x = "Date", y = "PIB réel") +
  theme_minimal()

p2 <- ggplot(df_station, aes(date, log_gdp)) +
  geom_line() +
  labs(title = "Log(PIB réel) – Niveau",
       x = "Date", y = "log(PIB)") +
  theme_minimal()

print(p1); print(p2)

ggAcf(log(ratio_ts), lag.max = 40) +
  ggtitle("ACF : log(PIB) – Niveau")

# Tests au niveau
adf_lvl  <- ur.df(log(ratio_ts), type = "drift", selectlags = "AIC")
summary(adf_lvl)

pp_lvl   <- pp.test(log(ratio_ts))
pp_lvl

kpss_lvl <- kpss.test(log(ratio_ts), null = "Level")
kpss_lvl

# Différenciation
dlog_gdp <- diff(log(ratio_ts))

ggAcf(na.omit(dlog_gdp), lag.max = 40) +
  ggtitle("ACF : Δlog(PIB)")

adf_diff  <- ur.df(na.omit(dlog_gdp), type = "drift", selectlags = "AIC")
summary(adf_diff)

pp_diff   <- pp.test(na.omit(dlog_gdp))
pp_diff

kpss_diff <- kpss.test(na.omit(dlog_gdp), null = "Level")
kpss_diff
