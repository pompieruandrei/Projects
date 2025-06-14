#-------------------------------#
# Proiect Serii de Timp - Anexe #
#-------------------------------#

library(tseries)
library(ggplot2)
library(forecast)
library(urca)
library(lmtest)
library(FinTS)
library(vars)
library(stargazer)
library(tsDyn)
library(psych)
#setwd("C:\\Users\\danru\\Desktop")
setwd("C:\\Users\\pompi\\Desktop\\serii")
getwd()

#Setul de date
hermes <- read.csv("preturi_hermes.csv",header = T,sep=",",dec = ".")
hermes$Date <- as.Date(hermes$Date, format = "%m/%d/%Y")

#Declararea seriei de timp
hermes_ts <- ts(hermes$Hermes,start = c(2010,1),frequency = 12)
#trend ascendent, fluctuatiile devin tot mai mari in timp (din 2021)
autoplot(hermes_ts) +
  theme_bw() +
  ylab("Preț Hermès (EUR)") + 
  ggtitle("Preț lunar Hermès (2010–2025)") +
  theme(plot.title = element_text(hjust = 0.5))



ggtsdisplay(hermes_ts)

# PARTEA1 -----------------------------------------------------------------



#Teste de stationaritate
#ADF
#H0: Seria prezinta radacina unitara si este nestationara.
#H1: Seria nu prezinta radacina unitara si este stationara
rezultat_adf_trend <- ur.df(hermes_ts,type = "trend",selectlags = c("AIC"))
summary(rezultat_adf_trend)
#t-statisitc : -1.6198 2.6595 3.3003
#critical values 1pct  5pct 10pct
#          tau3 -3.99 -3.43 -3.13
#           phi2  6.22  4.75  4.07
#           phi3  8.43  6.49  5.47
#t-statistic < decat toate critical values
# Acceptam H0, seria este nestationara


#KPSS
#H0: Seria este stationara.
#H1: Seria este nestationara
rezultat_kpss <- ur.kpss(hermes_ts,type = "tau")
#folosim "tau" pentru ca seria are trend
summary(rezultat_kpss)
#t-statistics = 0.8312 >
#critical values 0.119 0.146  0.176 0.216
#Resping H0, accept H1 -> seria este nestationara


#PP(Phillips-Perron)
#H0: Seria admite radacina unitara si este nestationara
#H1: Seria nu admite radacina unitara si este stationara
rezultat_pp <- ur.pp(hermes_ts,type = "Z-tau",model = "trend",lags = "short")
summary(rezultat_pp)

#t-statistic -1.1411 >
#                 1pct      5pct     10pct
#critical values -4.010861 -3.435203 -3.141339
#accept H0 -> seria este nestationara

hermes_returns <- diff(log(hermes_ts))
#Sezonaliatate
findfrequency(hermes_returns)
#1 -> nu avem sezonalitate
n_total <- length(hermes_returns)

n_train <- floor(0.8*n_total) #146 luni = Martie 2022
hermes_returns[146]
#Am împărțit seria de randamente Hermès în două subseturi: 80% pentru estimarea modelului ARIMA (ianuarie 2010 – martie 2022)
#și 20% pentru testare (aprilie 2022 – ianuarie 2025). Această împărțire asigură că modelul surprinde atât dinamica pre-pandemică,
#cât și comportamentul randamentelor în timpul crizei COVID-19, 
#menținând totodată o bază solidă pentru validarea post-pandemică.
train_ts <- window(hermes_returns, end = c(2010 + (n_train - 1) %/% 12, (n_train - 1) %% 12 + 2))
test_ts  <- window(hermes_returns, start = c(2010 + (n_train) %/% 12, (n_train) %% 12 + 2))



autoplot(hermes_returns) +
  theme_bw() +
  ylab("Randamente Hermès") + 
  ggtitle("Randamente lunar Hermès (2010–2025)") +
  theme(plot.title = element_text(hjust = 0.5))


#ADF
#H0: Seria prezinta radacina unitara si este nestationara.
#H1: Seria nu prezinta radacina unitara si este stationara
adf_none <- ur.df(hermes_returns,type = "trend",selectlags = "AIC")
summary(adf_none)
#t-statisitc : -10.2895 35.3017 52.9472 
#critical values 1pct  5pct 10pct
  #tau3 -3.99 -3.43 -3.13
  #phi2  6.22  4.75  4.07
  #phi3  8.43  6.49  5.47
#t-statistic > decat toate critical values
# Acceptam H1, seria este Stationara


#KPSS
#H0: Seria este stationara.
#H1: Seria este nestationara
kpss_returns <- ur.kpss(hermes_returns,type = "mu")
summary(kpss_returns)
# t-statistics < critical values
#   0.0503
                #10pct  5pct 2.5pct  1pct
                 #0.347 0.463  0.574 0.739
#Acceptam H0 -> seria este stationara

#PP(Phillips-Perron)
#H0: Seria admite radacina unitara si este nestationara
#H1: Seria nu admite radacina unitara si este stationara
pp_returns <- ur.pp(hermes_returns,type="Z-tau",model = "constant",lags = "short")
summary(pp_returns)
#t-statistc < critical values
#-13.4967      1pct      5pct     10pct
#              -3.467345 -2.877396 -2.575136
#Respingem H0 si acceptam H1 -> serie stationara

ggtsdisplay(hermes_returns,lag.max = 36)
#din graficul PACF putem observa ca nu avem nici un lag semnificativm,apare la 14 dar e prea mult
#nu exista autocorelare semnificativa
#respectiv un AR(0)
#DIn ACF e la fel ca la PACF, apare un pic la 14 dar e prea mult
#respectiv un MA(0)
#Concluzia este ca nu exista autocorelare in serie ->ARIMA(0,0,0)


model_arima <- auto.arima(hermes_returns,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)
coeftest(model_arima)
summary(model_arima)
#din auto arima obsevam ca avem ARIMA(0,0,0) 
#Nu exista structura predictiva, randamentul se comporta ca zgomot alb cu medie constanta
#media = 0.0174 -> estimarea randamentului mediu lunar
#s.e = 0.0054 -> eroarea standard a estimarii
#RMSE = 0.0724 -> Radacina patrata a erorii medii patratice
#MAE = 0.0569 -> Eroarea medie absoluta
#ACF1 = -0.0028 -> Nu exista autocorelare de ordin 1
#Modelul pare ok, toate sunt relativ mici, iar ACF1 e aproape de 0

arima101 <- Arima(hermes_returns,order = c(1,0,1))
coeftest(arima101)
#ar1 -0.941 *** foarte semnificativ
#ma1 0.956   *** foarte semnificativ
#intercept 0.017 ** semnificativ
summary(arima101)

arima201 <- arima(hermes_returns,order = c(2,0,1))
coeftest(arima201)
#ar1, ma1,intercepr - semnificativi
#ar2 nesemnificativ

arima100 <- arima(hermes_returns,order = c(1,0,0))
coeftest(arima100)
#ar1 nesemnificativ
#intercept semnificativ
arima001 <- arima(hermes_returns,order = c(0,0,1))
coeftest(arima001)
#ma1 nesemnificativ
#intercempt semnificativ

#                AIC       BIC      
#Arima(0,0,0)  436,74     430,32    doar Intercept semnificativ
#Arima(1,0,1)  433,05     420,21    toti coeficientii semnificativ



#Modelul optim arima este Arima(1,0,1)
#AIC=-433.05   AICc=-432.82   BIC=-420.21
#RMSE = 0.0725 (relativ mic)
#MAE = 0.0569
#ACF1 = -0.0148 (aprox 0) -> nu exista autocorelare semnificativa
rezid_arima <- residuals(arima101)
resid_sq <- rezid_arima^2
autoplot(ts(resid_sq, start = c(2010, 2), frequency = 12)) +
  theme_bw() +
  ylab("Reziduuri pătrate") +
  ggtitle("Volatilitatea reziduurilor ARIMA(1,0,1)") +
  theme(plot.title = element_text(hjust = 0.5))

# modele train ------------------------------------------------------------

model_train <- auto.arima(train_ts,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)
summary(model_train)
train_arima101 <- Arima(hermes_returns,order = c(1,0,1))
coeftest(train_arima101)
#ar1 -0.941 *** foarte semnificativ
#ma1 0.956   *** foarte semnificativ
#intercept 0.017 ** semnificativ
summary(arima101)

arima201 <- arima(hermes_returns,order = c(2,0,1))
coeftest(arima201)
#ar1, ma1,intercepr - semnificativi
#ar2 nesemnificativ

train_arima100 <- arima(hermes_returns,order = c(1,0,0))
coeftest(train_arima100)
#ar1 nesemnificativ
#intercept semnificativ
train_arima001 <- arima(hermes_returns,order = c(0,0,1))
coeftest(train_arima001)
#ma1 nesemnificativ
#intercempt semnificativ

#Testarea autocorelarii reziduurilor
#Am aplicat testul Ljung-Box si Box-pierce pentru a verifica autocorelarea reziduurilor, 
#testul Jarque-Bera pentru normalitatea acestora și 
#testul Diebold-Mariano pentru compararea acurateței prognozelor ARIMA și Holt.
#Rezultatele confirmă validitatea specificării modelului ARIMA(1,0,1) 
#și performanța sa superioară în prognozarea randamentelor Hermès

#Ljung-Box
#H0: Reziduurile NU au autocorelare (zgomot alb)
#H1: Reziduurile AU autocorelare (modelul e greșit)
Box.test(residuals(train_arima101),lag = 12,type = "Ljung-Box")
#p-value = 0.7804 > 0.05
#Acceptam H0 -> Reziduurile nu au autocorelare

#Box-pierce
Box.test(residuals(train_arima101),lag = 12,type = "Box-Pierce")
#p-value = 0.8038 > 0.05
#Acceptam H0 -> Reziduurile nu au autocorelare

#Jarque-Bera
#H0: Reziduurile sunt normal distribuite
#H1: Reziduurile NU sunt normal distribuite
jarque.bera.test(residuals(train_arima101))
#p-value = 0.8216 > 0.05
#Acceptam H0 -> Reziduurile sunt normal distribuite

#Modelul ARIMA(1,0,1) a fost validat suplimentar prin testul Box-Pierce (p = 0.8038),Ljung-Box (p=0.7804)
#care nu a indicat autocorelare a reziduurilor, și prin testul Jarque-Bera (p = 0.8216),
#care a confirmat normalitatea distribuției acestora. 
#Ambele rezultate susțin specificarea corectă și robustă a modelului ales.

# PARTEA2 -----------------------------------------------------------------
#Verificam daca avem ecfecte ARCH

#H0: nu există heteroscedasticitate condiționată (volatilitate constantă)
#H1: există efecte ARCH ⇒ volatilitate depinde de valori trecute
ArchTest(rezid_arima,lags = 1)
#p-value = 0.7053 > 0.05 
#Acceotam H0 -> nu exesta efecte arch (volatilitatea este constanta)

#„Modelul ARIMA(1,0,1) explică în mod satisfăcător dinamica randamentelor Hermès.
#Testul ARCH-LM aplicat pe reziduurile modelului nu a indicat prezența heteroscedasticității condiționate,
#motiv pentru care nu a fost necesară estimarea unui model GARCH.
#Astfel, analiza univariată se încheie cu un model valid și stabil din punct de vedere statistic.”


# Prognoza ----------------------------------------------------------------

# Forecasturi
forecast_arima <- forecast(train_arima101, h = length(test_ts))
forecast_holt <- holt(hermes_returns, h = length(test_ts))
# Holt-Winters adăugat doar pentru comparație, deși seria nu prezintă sezonalitate
forecast_hw <- hw(hermes_returns, seasonal = "additive", h = length(test_ts))

# Grafic comparativ
autoplot(forecast_arima, series = "ARIMA(1,0,1)") +
  autolayer(forecast_holt$mean, series = "Holt") +
  autolayer(forecast_hw$mean, series = "Holt-Winters") +
  autolayer(test_ts, series = "Real") +
  ggtitle("Comparatie prognoze: ARIMA vs Holt vs Holt-Winters vs Valori reale") +
  ylab("Randamente Hermès (%)") +
  xlab("Timp") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c(
    "ARIMA(1,0,1)" = "blue",
    "Holt" = "darkgreen",
    "Holt-Winters" = "orange",
    "Real" = "red"
  ))

#Diebold-Mariano
dm.test(forecast_arima$residuals,forecast_holt$residuals,alternative = "two.sided")
#H0: Ambele modele au aceeași acuratețe a prognozei
#H1: Modelele au acuratețe diferită
#DM=-1.7934
#p-value = 0.07457
#p-value>0.05 (dar<0.10) 
#Deși rezultatul testului Diebold-Mariano nu indică o diferență semnificativă statistic la nivelul de 5% (p = 0.07457), 
#se observă o tendință favorabilă modelului ARIMA în ceea ce privește acuratețea prognozei, 
#diferența fiind marginal semnificativă la un nivel de încredere de 90%


# Partea 2 ----------------------------------------------------------------

lvmh <- read.csv("lvmh.csv",header = T,sep=",",dec = ".")
lvmh$Date <- as.Date(lvmh$Date, format = "%m/%d/%Y")

gold <- read.csv("gold.csv",header = T,sep=",",dec = ".")
gold$Date <- as.Date(gold$Date, format = "%m/%d/%Y")

lvmh_ts <- ts(lvmh$LVMH,start = c(2010,1),frequency = 12 )
gold_ts <- ts(gold$Gold,start = c(2010,1),frequency = 12)

multi_ts <- cbind(Hermes = hermes_ts, LVMH = lvmh_ts, Aur = gold_ts)

# Graficul comparativ al celor 3 serii
autoplot(multi_ts) +
  ylab("Randamente (%)") +
  ggtitle("Grafic multivariat: Hermès, LVMH și Aur ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggtsdisplay(lvmh_ts)
ggtsdisplay(gold_ts)
#Seriile par a fi nestationare

#Teste de Stationaritate pentru LVMH si GOLD
#LVMH
rezultat_adf_trend_lvmh <- ur.df(lvmh_ts,type = "trend",selectlags = c("AIC"))
summary(rezultat_adf_trend_lvmh)
#t-statisitc : -2.2375 2.6143 2.6463
#critical values 1pct  5pct 10pct
      #tau3 -3.99 -3.43 -3.13
      #phi2  6.22  4.75  4.07
      #phi3  8.43  6.49  5.47
#t-statistic < decat toate critical values
# Acceptam H0, seria este nestationara


#KPSS
rezultat_kpss_lvmh <- ur.kpss(lvmh_ts,type = "tau")
#folosim "tau" pentru ca seria are trend
summary(rezultat_kpss_lvmh)
#t-statistics = 0.7029  >
#critical values 0.119 0.146  0.176 0.216
#Resping H0, accept H1 -> seria este nestationara


#PP(Phillips-Perron)
rezultat_pp_lvmh <- ur.pp(lvmh_ts,type = "Z-tau",model = "trend",lags = "short")
summary(rezultat_pp_lvmh)
#t-statistic -2.2065  >
#                 1pct      5pct     10pct
#critical values -4.01167 -3.435589 -3.141567
#accept H0 -> seria este nestationara

#Gold
rezultat_adf_trend_gold <- ur.df(gold_ts,type = "trend",selectlags = c("AIC"))
summary(rezultat_adf_trend_gold)
#t-statisitc : -0.3347 1.7608 1.046 
#critical values 1pct  5pct 10pct
#tau3 -3.99 -3.43 -3.13
#phi2  6.22  4.75  4.07
#phi3  8.43  6.49  5.47
#t-statistic < decat toate critical values
# Acceptam H0, seria este nestationara


#KPSS
rezultat_kpss_gold <- ur.kpss(gold_ts,type = "tau")
#folosim "tau" pentru ca seria are trend
summary(rezultat_kpss_gold)
#t-statistics = 0.5558   >
#critical values 0.119 0.146  0.176 0.216
#Resping H0, accept H1 -> seria este nestationara


#PP(Phillips-Perron)
rezultat_pp_gold <- ur.pp(gold_ts,type = "Z-tau",model = "trend",lags = "short")
summary(rezultat_pp_gold)
#t-statistic -0.565   >
#                 1pct      5pct     10pct
#critical values -4.020421 -3.43976 -3.144028
#accept H0 -> seria este nestationara

lvmh_returns <- diff(log(lvmh_ts))
stat_descr_lvmh <- describe(lvmh_returns)
rownames(stat_descr_lvmh)[1] <- "LVMH_returns"
lvmh_stat <- stat_descr_lvmh
boxplot.stats(lvmh_returns)$out

gold_returns <- diff(log(gold_ts))
stat_descr_gold <- describe(gold_returns)
rownames(stat_descr_gold)[1] <- "GOLD_returns"
gold_Stats <- stat_descr_gold
boxplot.stats(gold_returns)$out

ggtsdisplay(lvmh_returns)
#seria pare a fi stationara
ggtsdisplay(gold_returns)
#Seria pare a fi stationara


#Testarea seriilor diferentiate
#LVMH
#ADF
adf_none_lvmh <- ur.df(lvmh_returns,type = "trend",selectlags = "AIC")
summary(adf_none_lvmh)#t-statisitc : -10.615 37.607 56.409  
#critical values 1pct  5pct 10pct
  #tau3 -3.99 -3.43 -3.13
  #phi2  6.22  4.75  4.07
  #phi3  8.43  6.49  5.47
#t-statistic > decat toate critical values
# Acceptam H1, seria este Stationara



#KPSS
kpss_returns_lvmh <- ur.kpss(lvmh_returns,type = "mu")
summary(kpss_returns)
# t-statistics < critical values
#   0.0503
#10pct  5pct 2.5pct  1pct
#0.347 0.463  0.574 0.739
#Acceptam H0 -> seria este stationara

#PP(Phillips-Perron)
#H0: Seria admite radacina unitara si este nestationara
#H1: Seria nu admite radacina unitara si este stationara
pp_returns_lvmh <- ur.pp(lvmh_returns,type="Z-tau",model = "constant",lags = "short")
summary(pp_returns)
#t-statistc < critical values
#-13.4967      1pct      5pct     10pct
#              -3.467345 -2.877396 -2.575136
#Respingem H0 si acceptam H1 -> serie stationara

findfrequency(lvmh_returns)
#1 -> nu avem sezonalitate
#GOLD
#ADF
adf_none_gold <- ur.df(gold_returns,type = "trend",selectlags = "AIC")
summary(adf_none_gold)#t-statisitc : -9.3523 29.156 43.7339   
#critical values 1pct  5pct 10pct
    #tau3 -3.99 -3.43 -3.13
    #phi2  6.22  4.75  4.07
    #phi3  8.43  6.49  5.47
#t-statistic > decat toate critical values
# Acceptam H1, seria este Stationara



#KPSS
kpss_returns_gold <- ur.kpss(gold_returns,type = "mu")
summary(kpss_returns_gold)
# t-statistics < critical values
#   0.1868
#10pct  5pct 2.5pct  1pct
#0.347 0.463  0.574 0.739
#Acceptam H0 -> seria este stationara

#PP(Phillips-Perron)
#H0: Seria admite radacina unitara si este nestationara
#H1: Seria nu admite radacina unitara si este stationara
pp_returns_gold <- ur.pp(gold_returns,type="Z-tau",model = "constant",lags = "short")
summary(pp_returns_gold)
#t-statistc < critical values
#-13.2      1pct      5pct     10pct
#           -3.474233 -2.880475 -2.576754
#Respingem H0 si acceptam H1 -> serie stationara

findfrequency(gold_returns)
#1 -> nu avem sezonalitate




#Identificarea lagurilor optime
df_returns <- cbind(Hermes = hermes_returns, LVMH = lvmh_returns, Aur = gold_returns)
df_returns <- na.omit(df_returns)
n_total <- nrow(df_returns)
n_train <- floor(0.8 * n_total)

# Seturi
train_data <- df_returns[1:n_train, ]
test_data  <- df_returns[(n_train + 1):n_total, ]

lags <- VARselect(train_data,lag.max = 12,type = "const")
lags$selection #Confom AIC, HQ, SC, FPE alegem lagul 1
lags2 <- VARselect(train_data,lag.max = 24,type = "const")
lags2$selection
#pentru lags2 este lagul 1 



#Cointegrare

# Testul de cointegrare Johansen
# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare
#Metoda Trace
johansen_test <- ca.jo(train_data, type = "trace", ecdet = "const", K = 2)  # K = 2, chiar daca lagul 1 sau lagul 2 este optim
summary(johansen_test)
#Values of teststatistic and critical values of test:
#  test 10pct  5pct  1pct
#r <= 2 | 46.33 > 7.52  9.24 12.97
#r <= 1 | 107.80 > 17.85 19.96 24.60
#r = 0  | 179.59 > 32.00 34.91 41.07
#Avem cel mult 2 relatii de cointegrare

#Metoda valorilor proprii maxime
johansen_test2 <- ca.jo(train_data,type = "eigen",ecdet = "const",K=2)
summary(johansen_test2)
#Values of teststatistic and critical values of test:
 # test 10pct  5pct  1pct
#r <= 2 | 46.33 > 7.52  9.24 12.97
#r <= 1 | 61.47 > 13.75 15.67 20.20
#r = 0  | 71.79 > 19.77 22.00 26.81
#Avem cel mult 2 relatii de cointegrare

#Chiar daca avem cel mult 2 vom merge pe o relatie de cointegrare pentu a ne fi mai usor de explicat
Model1 <- VECM(train_data,
               lag = 2, 
               r=1, 
               estim = ('ML'),
               LRinclude = 'const')
summary(Model1)
#   Hermes  LVMH        Aur        const
#r1      1 -1.196488 -0.7453381 0.0016201
#Hermes are o relatie pe termen lung
#Hermes variabila de referinta
#Hermes=1.196488⋅LVMH+0.7453381⋅Aur+0.0016201


#ECT
#Hermès	-0.6690(0.1785)***	 Semnificativ și negativ	Hermès corectează dezechilibrele față de relația de cointegrare 
#există relație pe termen lung între Hermès și celelalte două variabile.
#LVMH	+0.3333(0.1607)*	 Pozitiv și semnificativ la 10%	LVMH nu converge spre echilibru, 
#ci tinde să amplifice dezechilibrul ⇒ nu are rol corectiv.
#Aur	+0.2847(0.1286)*	 Pozitiv și semnificativ la 10%	Aurul nu corectează deviațiile, 
#iar coeficientul pozitiv sugerează divergență (nu echilibru).



#Relatii pe termen scurt
#Hermes
#Hermes(-1)	-0.3005(0.1479)*	Autocorelare negativă slab semnificativă – randamentul trecut influențează negativ prezentul.
#LVMH(-1)	-0.4855(0.1718)**	 Efect negativ semnificativ – când LVMH scade, Hermès scade.
#Aur(-1)	-0.2114(0.1465)	  Nesemnificativ.
#Hermes(-2)	-0.0212(0.1211)	 Nesemnificativ.
#LVMH(-2)	-0.3741(0.1309)** 	Efect negativ semnificativ.
#Aur(-2)	-0.0079(0.1229)	  Nesemnificativ.

#Hermès este influențat pe termen scurt de:
# LVMH (lag 1 și 2) – semnificativ negativ.
#Ușor de propria valoare la lag 1.


#LVMH
#Hermes(-1)	-0.4611(0.1331)***	Efect puternic negativ ⇒ Hermès influențează negativ LVMH.
#LVMH(-1)	-0.2139(0.1547)	  Nesemnificativ.
#Aur(-1)	+0.2838(0.1319)*	Efect pozitiv moderat.
#Hermes(-2)	-0.0073(0.1091)	  Nesemnificativ.
#LVMH(-2)	-0.3092(0.1178)**	Autoreglare negativă semnificativă.
#Aur(-2)	+0.2701(0.1107)*	Efect pozitiv semnificativ.

#LVMH este influențat de:
# Hermès (lag 1) – foarte semnificativ.
#Aur (lag 1 și 2) – pozitiv moderat.
#Propriul lag 2 – autoreglare.




#Aur
#Hermes(-1)	-0.1488(0.1065)	  Nesemnificativ.
#LVMH(-1)	+0.1549(0.1237)	  Nesemnificativ.
#Aur(-1)	-0.6209(0.1055)***	 Autocorelare foarte semnificativă – Aur se autoreglează puternic.
#Hermes(-2)	-0.1569(0.0872).	 ️ Aproape semnificativ.
#LVMH(-2)	+0.1369(0.0942)	  Nesemnificativ.
#Aur(-2)	-0.3792(0.0885)***	 Autoreglare semnificativă.

#Nu este influențat de alte variabile.
#Se autoreglează (lag 1 și 2) ⇒ dinamică proprie.

model2 <- vec2var(johansen_test,r=1)


#Autocorelarea
#Testul pentru autocorelarea reziduurilor (Portmanteau)
#H0: Nu există autocorelare a reziduurilor (reziduurile sunt zgomot alb)
#H1: Există autocorelare a reziduurilor

Serial <- serial.test(model2,lags.pt = 12,type = "PT.asymptotic")
Serial #p-value = p-value = 0.01198 <0.1, Acceptam H1 ->  exista autocorelare in reziduuri

#Heteroscedasticitate
#H0: Nu există heteroscedasticitate (varianța este constantă)
#H1: Există heteroscedasticitate (efecte ARCH)

Arch <- vars::arch.test(model2,lags.multi = 1,multivariate.only = TRUE)
Arch
#p-value= 0.003184 < 0.1 exista heteroscedasticitate (prezinta efecte ARCH)

#Normalitatea riziduurilor(Jarque-Bera multivaria)
#H0: Reziduurile sunt normal distribuite
#H1: Reziduurile nu sunt normal distribuite

Norm <- normality.test(model2,multivariate.only = TRUE)
Norm

#JB: p-value = 1.527e-05 Nu sunt normal distribuite
#Asimetrie(skewnees): p-value = 0.001454 
#Aplatizare(kurtosis): p-value = 0.0008236 


plot(residuals(model2), main = "Reziduuri VECM")




#Funcia de raspuns la impuls (IRF)

irf_lvmh_hermes <- irf(model2, impulse = "Hermes", response = "LVMH", n.ahead = 24, boot = TRUE, ci = 0.90)
plot(irf_lvmh_hermes, ylab = "LVMH", main = "Răspunsul LVMH la șocuri în Hermès")
#Reacție imediată (perioadele 1–2):
#LVMH reacționează puternic pozitiv, sugerând o legătură pe termen scurt între mișcările Hermès și LVMH.
#Ulterior, reacția se inversează brusc, dar fără a deveni semnificativ negativă (nu iese din banda CI).
#Stabilizare (perioadele 5–25):
#Efectul tinde spre o stabilizare pozitivă, dar modestă și încadrată în intervalul de încredere.
#Asta înseamnă că șocurile în Hermès pot avea un impact de durată asupra LVMH, dar nu unul foarte mare.
#Benzile de încredere nu includ 0 la început, ceea ce confirmă un efect semnificativ pe termen scurt,
#dar nu neapărat pe termen lung.

#Acest grafic susține ideea unei relații de cauzalitate Granger pe termen scurt între Hermès și LVMH. 
#Impactul se atenuează după ~5 luni, dar tendința rămâne ușor pozitivă și stabilă.

irf_aur_hermes <- irf(model2, impulse = "Hermes", response = "Aur", n.ahead = 24, boot = TRUE, ci = 0.90)
plot(irf_aur_hermes, ylab = "Aur", main = "Răspunsul Aurului la șocuri în Hermès")
#Efect slab pozitiv inițial:
#La început, șocul Hermès generează un răspuns ușor pozitiv în prețul Aurului.
#Acest efect este nesemnificativ statistic deoarece linia neagră rămâne în interiorul benzilor roșii.
#Persistență scăzută:
#Răspunsul rămâne pozitiv, dar aproape plat (0.002–0.003) și se aplatizează complet după 5–6 luni,
#indicând un impact tranzitoriu.
#Intervalele de încredere includ zero pe tot orizontul:
#Nu există dovezi semnificative statistic că șocurile în Hermès influențează Aurul pe termen scurt sau lung.
#Efectele observate pot fi zgomot.

#Nu există o relație de cauzalitate Granger vizibilă între Hermès și Aur, nici pe termen scurt, nici lung, din perspectiva IRF.
#Aurul rămâne relativ independent față de dinamica Hermès,
#întărind ipoteza lui ca activ de refugiu cu rol de diversificare în contextul pieței de lux.

irf_hermes_hermes <- irf(model2, impulse = "Hermes", response = "Hermes", n.ahead = 24, boot = TRUE, ci = 0.90)
plot(irf_hermes_hermes, ylab = "Hermes", main = "Răspunsul Hermes la șocuri în Hermès")

#Impact inițial puternic:
#În prima lună, Hermès reacționează cu un salt puternic pozitiv (~0.08).
#Acest lucru reflectă un răspuns imediat robust la un șoc propriu (ex: anunț financiar, eveniment în firmă).
#Corecție rapidă:
#În luna 2, valoarea coboară brusc (~0.037), semnalând o ajustare după entuziasmul inițial.
#Reacția intră apoi într-o fază stabilă, ușor sub 0.06.
#Persistență moderată:
#Răspunsul se stabilizează la un nivel moderat pozitiv și nu revine la zero, indicând un efect de durată, dar nu permanent.
#Acest tip de răspuns este caracteristic seriilor cu efecte de memorie scurtă până la medie.
#Semnificație statistică:
#Pe tot intervalul, linia neagră este deasupra zero și în afara benzilor inferioare,
#deci răspunsul este semnificativ statistic pe termen scurt și început de termen mediu.


#Hermès reacționează imediat și semnificativ la propriile sale șocuri, cu o ajustare rapidă și un efect pozitiv de durată.
#Această dinamică întărește concluziile din VECM: Hermès are capacitate de autoreglare și se mișcă independent,
#dar predictibil pe baza propriei istorii.
#Se justifică complet aplicarea modelului VECM pe Hermès.




#Descompunearea variantei
FEVD <- fevd(model2,n.ahead = 24)
plot(FEVD)

#FEVD pentru Hermès:
# 100% din variația lui Hermès este explicată de șocuri proprii pe tot intervalul (bare complet negre).
#Asta confirmă concluziile IRF: Hermès este determinat aproape exclusiv de propria dinamică 
#și este foarte puțin influențat de LVMH sau Aur.
# Implicație: Hermès este autoreglabil și autonom în contextul analizat – probabil indică leadership în acest sistem de active.


#FEVD pentru LVMH:
#La început (h1): doar ~30% din variație provine din LVMH, iar ~70% este din Hermès.
#După 10 luni: peste 75% din variație e determinată de Hermès.
#Aurul contribuie foarte puțin (sub 5% pe tot intervalul).
#Concluzie: LVMH este puternic influențat de Hermès, mai ales pe termen lung,
#ceea ce întărește ideea de cauzalitate Granger Hermès → LVMH.


#FEVD pentru Aur:
#Pe tot intervalul de 24 luni, aproape 100% din variația sa este explicată de șocuri proprii.
#Nici Hermès, nici LVMH nu contribuie semnificativ la variația Aurului.
#Implicație: Aurul se comportă ca un activ de refugiu, izolat de dinamica pieței de lux.



h <- nrow(test_data)

forecast <- predict(model2, n.ahead = h, ci = 0.90)

# Ploturi individuale pentru fiecare variabilă
par(mfrow=c(1,3))
plot(forecast, name = "Hermes")
plot(forecast, name = "LVMH")
plot(forecast, name = "Aur")

# Fancharts (benzi de încredere Bootstrap)
fanchart(forecast, names = "Hermes")
fanchart(forecast, names = "LVMH")
fanchart(forecast, names = "Aur")




# Extrage predicția (mediana)
pred_hermes <- forecast$fcst$Hermes[, 1]
real_hermes <- test_data[, "Hermes"]

# Poți calcula RMSE
rmse <- sqrt(mean((real_hermes - pred_hermes)^2))
print(rmse)




df_plot <- data.frame(
  Time = 1:h,
  Real = real_hermes,
  Predicted = pred_hermes
)

lower <- forecast$fcst$Hermes[,2]
upper <- forecast$fcst$Hermes[,3]

df_plot$Lower <- lower
df_plot$Upper <- upper

ggplot(df_plot, aes(x = Time)) +
  geom_line(aes(y = Real), color = "red", size = 1) +
  geom_line(aes(y = Predicted), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "blue") +
  labs(title = "Forecast vs Real pentru Hermès (cu CI)", y = "Rentabilitate", x = "Luni") +
  theme_bw()

