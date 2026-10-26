library(openxlsx2)
library(forecast)
library(testcorr)
#library(normtest)
library(lmtest)
library(aod)
library(latex2exp)
library(tseries)
library(RJDemetra)
library(forecast)
#library(haven)
library(dplyr)
library(lubridate)
library(zoo)

#Data

##############################################################
# 1. LOAD DATA AND SET TIME SERIES
##############################################################


ts_VolumeSales_P4_Total<- ts(VolumeSales_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_ValueSales_P4_Total<- ts(ValueSales_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_PpV_P4_Total<- ts(PpV_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_ND_P4_Total<- ts(ND_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_CWD_P4_Total<- ts(CWD_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_CWD_AnyPriceReduction_P4_Total<- ts(CWD_AnyPriceReduction_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_CWD_AnyMerchandising_P4_Total<- ts(CWD_AnyMerchandising_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_VolumeBase_P4_Total<- ts(VolumeBase_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_ValueBase_P4_Total<- ts(ValueBase_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_IncrementalVolume_P4_Total<- ts(IncrementalVolume_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_IncrementalValue_P4_Total<- ts(IncrementalValue_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_VolumeAnyMerchandising_P4_Total<- ts(VolumeAnyMerchandising_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_ValueAnyMerchandising_P4_Total<- ts(ValueAnyMerchandising_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_CWD_Feature_TPR_P4_Total<- ts( CWD_Feature_TPR_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_CWD_Display_TPR_P4_Total<- ts( CWD_Display_TPR_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_CWD_Feature_Display_TPR_P4_Total<- ts( CWD_Feature_Display_TPR_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)


ts_TotalVol_Feature_Display_TPR_P4_Total<- ts( TotalVol_Feature_Display_TPR_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_TotalVal_Feature_Display_TPR_P4_Total<- ts( TotalVal_Feature_Display_TPR_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

ts_Lift_P4_Total<- ts( Lift_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_PEI_P4_Total<- ts( PEI_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)

# Average Price
avg_price_P4_Total <- ValueSales_P4_Total/VolumeSales_P4_Total
# Base Price
base_price_P4_Total <- ValueBase_P4_Total/VolumeBase_P4_Total
# Promo Price (μόνο όταν υπάρχει merchandising)
promo_price_P4_Total <- ValueAnyMerchandising_P4_Total/VolumeAnyMerchandising_P4_Total
# Discount 1
discount1_P4_Total <- 1 - (promo_price_P4_Total / base_price_P4_Total)
# Discount 2
discount2_P4_Total <- 1 - (avg_price_P4_Total / base_price_P4_Total)

ts_avg_price_P4_Total<- ts( avg_price_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_base_price_P4_Total<- ts( base_price_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_promo_price_P4_Total<- ts( promo_price_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_discount1_P4_Total<- ts( discount1_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)
ts_discount2_P4_Total<- ts( discount2_P4_Total,st=c(2022,1),end=c(2024, 34),freq=52)


##############################################################
##############################################################
##############################################################
#  PLOTS
##############################################################
##############################################################
##############################################################

################################
######## TOTAL MARKET #########
################################


# ΠΛΟΤ 1: ΑΡΧΙΚΗ ΧΡΟΝΟΣΕΙΡΑ ΠΩΛΗΣΕΩΝ
plot(ts_VolumeSales_P1_Total, main = "Χρονοσειρά Όγκου Πωλήσεων", ylab = "Πωλήσεις (kg)", xlab = "Χρόνος", col = "darkblue", 
     lwd = 2.5, type = "l", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1, xaxt = "n")
grid(lwd = 0.5, col = "gray80")
grid(col = "gray90", lty = "dashed")

################################
# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023","Jan2024","Jul2024", "Jan2025")
axis(1, at = ticks, labels = labels)


##############################################################################
##############################################################################
# ΠΛΟΤ 1: SALES AND PROMOTION DYNAMICS
##############################################################################
##############################################################################

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

plot(ts_VolumeSales_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Volume Sales",
     xlab = "date", ylab = "Volume Sales", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_VolumeBase_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Volume Base",
     xlab = "date", ylab = "Volume Base", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_IncrementalVolume_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Incremental Volume",
     xlab = "date", ylab = "Incremental Volume", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_VolumeAnyMerchandising_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Promo Volume", 
     xlab = "date", ylab = "Volume Any Merchandising", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################
#######################################

mtext("Sales and Promotion Dynamics", outer = TRUE, cex = 1.5, font = 2)

##############################################################################
##############################################################################
# ΠΛΟΤ 2: PRICING DYNAMICS (Average Price, Base Price, Promo Price, PPV)
##############################################################################
##############################################################################

par(mfrow = c(2, 2),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 3, 0))

plot(ts_avg_price_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Average Price",
     xlab = "date", ylab = "Average Price", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_base_price_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Base Price",
     xlab = "date", ylab = "Base Price", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################
plot(ts_promo_price_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Promo Price",
     xlab = "date", ylab = "Promo Price", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_PpV_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "PPV",
     xlab = "date", ylab = "PPV", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################
#######################################

mtext("Pricing Dynamics", outer = TRUE, cex = 1.5, font = 2)

##############################################################################
##############################################################################
# ΠΛΟΤ 3: PROMOTION INTENSITY( CWD Any Merchantising, CWD Any Price Reduction, Discount 1, Discount 2)
##############################################################################
##############################################################################

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

plot(ts_CWD_AnyMerchandising_P4_Total,  type = "l", col = "dodgerblue", lwd = 2, main = "CWD Any Merchantising",
     xlab = "date", ylab = "CWD Any Merchantising", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_CWD_AnyPriceReduction_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "CWD Any Price Reduction",
     xlab = "date", ylab = "CWD Any Price Reduction", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_discount1_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Discount 1",
     xlab = "date", ylab = "Discount 1", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

#######################################

plot(ts_discount2_P4_Total, type = "l", col = "dodgerblue", lwd = 2, main = "Discount 2",
     xlab = "date", ylab = "Discount 2", xaxt = "n")
grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

#######################################
#######################################

mtext("Promotion Intensity", outer = TRUE, cex = 1.5, font = 2)

##############################################################################
# ΠΛΟΤ 4: PROMOTION EFFECTIVENESS (Incremental Volume, Incremental Value, %LIFT , PEI)  
##############################################################################


par(mfrow = c(2, 2),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 3, 0))

plot(ts_IncrementalVolume_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "Incremental Volume",
     xlab = "date",
     ylab = "Incremental Volume",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

plot(ts_IncrementalValue_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "Incremental Value",
     xlab = "date",
     ylab = "Incremental Value",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

plot(ts_Lift_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "%LIFT",
     xlab = "date",
     ylab = "%LIFT",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

plot(ts_PEI_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "PEI",
     xlab = "date",
     ylab = "PEI",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)


mtext("Promotion Effectiveness",
      outer = TRUE,
      cex = 1.5,
      font = 2)


##############################################################################
# ΠΛΟΤ 5: ADVANCED PROMOTION MIX (CWD Feature and TPR, CWD Display and TPR,
#                                 CWD Feature and Display and TPR, 
#                                 Total Vol Only Feature and Display and TPR)  
##############################################################################


par(mfrow = c(2, 2),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 3, 0))

plot(ts_CWD_Feature_TPR_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "CWD Feature and TPR",
     xlab = "date",
     ylab = "CWD Feature and TPR",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")

axis(1, at = ticks, labels = labels)

plot(ts_CWD_Display_TPR_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "CWD Display and TPR",
     xlab = "date",
     ylab = "CWD Display and TPR",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

plot(ts_CWD_Feature_Display_TPR_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "CWD Feature and Display and TPR",
     xlab = "date",
     ylab = "CWD Feature and Display and TPR",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)

plot(ts_TotalVol_Feature_Display_TPR_P4_Total,
     type = "l",
     col = "dodgerblue",
     lwd = 2,
     main = "Total Vol Only Feature and Display and TPR",
     xlab = "date",
     ylab = "Total Vol Only Feature and Display and TPR",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)
labels <- c("Jan2022","Jul2022", "Jan2023","Jul2023", "Jan2024","Jul2024", "Jan2025")


axis(1, at = ticks, labels = labels)


mtext("Advanced Promotion Mix",
      outer = TRUE,
      cex = 1.5,
      font = 2)



par(mfrow = c(1, 1))


##############################################################
# 2. VARIABLE CONSTRUCTION - R VERSION
##############################################################

# Logs
lnQ <- ifelse(ts_VolumeSales_P4_Total > 0,
              log(ts_VolumeSales_P4_Total), NA)

lnPpV <- ifelse(ts_PpV_P4_Total > 0,
                log(ts_PpV_P4_Total), NA)

lnnd <- ifelse(ts_ND_P4_Total > 0,
               log(ts_ND_P4_Total), NA)

lncwd <- ifelse(ts_CWD_P4_Total > 0,
                log(ts_CWD_P4_Total), NA)


# Prices
AvgPrice <- ifelse(ts_VolumeSales_P4_Total > 0,
                   ts_ValueSales_P4_Total / ts_VolumeSales_P4_Total, NA)

PriceBase <- ifelse(ts_VolumeBase_P4_Total > 0,
                    ts_ValueBase_P4_Total / ts_VolumeBase_P4_Total, NA)

PromoPrice <- ifelse(ts_VolumeAnyMerchandising_P4_Total > 0,
                     ts_ValueAnyMerchandising_P4_Total / ts_VolumeAnyMerchandising_P4_Total, NA)


# Discounts
discount_1 <- 1 - (PromoPrice / PriceBase)

discount_2 <- 1 - (AvgPrice / PriceBase)


# Promo share
promo_share <- ifelse(ts_VolumeSales_P4_Total > 0,
                      ts_VolumeAnyMerchandising_P4_Total / ts_VolumeSales_P4_Total, NA)


##############################################################
# Date variable
##############################################################

date <- seq.Date(
  from = as.Date("2022-01-01"),
  by = "week",
  length.out = length(ts_VolumeSales_P4_Total)
)


##############################################################
# Christmas dummy
##############################################################

christmas_dummy <- rep(0, length(date))

christmas_dummy[
  date >= as.Date("2022-12-10") & date <= as.Date("2022-12-31")
] <- 1

christmas_dummy[
  date >= as.Date("2023-12-10") & date <= as.Date("2023-12-31")
] <- 1


##############################################################
# Easter dummy
##############################################################

easter_dummy <- rep(0, length(date))

easter_dummy[
  date >= as.Date("2022-04-11") & date <= as.Date("2022-05-01")
] <- 1

easter_dummy[
  date >= as.Date("2023-04-03") & date <= as.Date("2023-04-23")
] <- 1

easter_dummy[
  date >= as.Date("2024-04-22") & date <= as.Date("2024-05-12")
] <- 1


##############################################################
# Moving average: equivalent to Stata tssmooth ma window(2 1 2)
##############################################################

library(zoo)

cwd_merch_sm5 <- rollapply(
  ts_CWD_AnyMerchandising_P4_Total,
  width = 5,
  FUN = mean,
  align = "center",
  fill = NA
)


##############################################################
# Promo merchandising dummy smoothed
##############################################################

promo_merch_dummy_sm3 <- ifelse(
  !is.na(cwd_merch_sm5) & cwd_merch_sm5 > 30,
  1,
  0
)

promo_merch_dummy_sm[is.na(cwd_merch_sm5)] <- NA

promo_merch_dummy_sm2 <- ifelse(
  !is.na(cwd_merch_sm5) & cwd_merch_sm5 > 20,
  1,
  0
)

promo_merch_dummy_sm1 <- ifelse(
  !is.na(cwd_merch_sm5) & cwd_merch_sm5 > 10,
  1,
  0
)

##############################################################
promo_10 <- ifelse(ts_CWD_AnyMerchandising_P4_Total > 10, 1, 0)

promo_20 <- ifelse(ts_CWD_AnyMerchandising_P4_Total > 20, 1, 0)

promo_30 <- ifelse(ts_CWD_AnyMerchandising_P4_Total > 30, 1, 0)
##############################################################


c(
  Mean = mean(promo_share, na.rm = TRUE),
  SD   = sd(promo_share, na.rm = TRUE),
  Min  = min(promo_share, na.rm = TRUE),
  Max  = max(promo_share, na.rm = TRUE)
)

##############################################################
# 3. VISUAL INSPECTION
##############################################################

# 1. Time-series plot: Log Demand over Time
plot(lnQ,
     type = "l",
     col = "darkblue",
     lwd = 2.5,
     main = "Log Demand over Time",
     xlab = "Time",
     ylab = "lnQ",
     xaxt = "n")

grid(col = "gray90", lty = "dashed")

ticks <- seq(2022, 2025, by = 0.5)

labels <- c("Jan2022", "Jul2022",
            "Jan2023", "Jul2023",
            "Jan2024", "Jul2024",
            "Jan2025")

axis(1, at = ticks, labels = labels)

# 2. ACF of lnQ
acf(lnQ,
    lag.max = 60,
    main = "ACF of lnQ",
    na.action = na.pass)

# 3. PACF of lnQ
pacf(lnQ,
     lag.max = 60,
     main = "PACF of lnQ",
     na.action = na.pass)

############################################################## 
############################################################## 
# 4. BIC MODEL SELECTION - FULL ENUMERATION
# 5.2 - ΔΙΠΛΩΜΑΤΙΚΗ
##############################################################
############################################################## 

##############################################################

# Υποψήφιες μεταβλητές
# 1. Ορίζεις τις 7 μεταβλητές που μπορούν να μπουν στο μοντέλο.
candidates <- c(
  "lnPpV",
  "lnnd",
  "lncwd",
  "discount_1",
  "christmas_dummy",
  "easter_dummy",
  "promo_share"
)

##############################################################
# 2.

candidates <- c(
  "lnPpV",
  "lnnd",
  "lncwd",
  "lnPriceBase" ,
  "lnPromoPrice",
  "discount_1",
  "christmas_dummy",
  "easter_dummy",
  "promo_share"
)

##############################################################
# 3.

candidates <- c(
  "lnPpV",
  "lnnd",
  "lncwd",
  "lnPriceBase" ,
  "lnPromoPrice",
  "discount_1",
  "discount_2",
  "christmas_dummy",
  "easter_dummy",
  "promo_share"
)
##############################################################

# Dataset με όλες τις μεταβλητές
# 2. Φτιάχνεις έναν ενιαίο πίνακα με το dependent variable lnQ και όλες τις candidate explanatory variables.

# 1.
df_model <- data.frame(
  lnQ = as.numeric(lnQ),
  lnPpV = as.numeric(lnPpV),
  lnnd = as.numeric(lnnd),
  lncwd = as.numeric(lncwd),
  discount_1 = as.numeric(discount_1),
  christmas_dummy = as.numeric(christmas_dummy),
  easter_dummy = as.numeric(easter_dummy),
  promo_share = as.numeric(promo_share)
)
##############################################################
############      ΔΙΚΟ ΜΟΥ                        ############
##############################################################
# 2.
df_model <- data.frame(
  lnQ = as.numeric(lnQ),
  lnPpV = as.numeric(lnPpV),
  lnnd = as.numeric(lnnd),
  lncwd = as.numeric(lncwd),
  lnPriceBase = as.numeric(PriceBase),
  lnPromoPrice = as.numeric(PromoPrice),
  discount_1 = as.numeric(discount_1),
  christmas_dummy = as.numeric(christmas_dummy),
  easter_dummy = as.numeric(easter_dummy),
  promo_share = as.numeric(promo_share)
)
##############################################################
# 3.
df_model <- data.frame(
  lnQ = as.numeric(lnQ),
  lnPpV = as.numeric(lnPpV),
  lnnd = as.numeric(lnnd),
  lncwd = as.numeric(lncwd),
  lnPriceBase = as.numeric(PriceBase),
  lnPromoPrice = as.numeric(PromoPrice),
  discount_1 = as.numeric(discount_1),
  discount_2 = as.numeric(discount_2),
  christmas_dummy = as.numeric(christmas_dummy),
  easter_dummy = as.numeric(easter_dummy),
  promo_share = as.numeric(promo_share)
)
##############################################################



# Αφαιρούμε missing values
# 3. Πετάει γραμμές με missing values. Στο Stata αυτό γίνεται αυτόματα μέσα στο regress.
df_model <- na.omit(df_model)

##############################################################

# Πλήθος υποψήφιων μεταβλητών
# 4. Μετράει πόσες υποψήφιες μεταβλητές έχεις. Εδώ p = 7.
p <- length(candidates)

##############################################################

# Όλα τα possible model combinations
# 5. Άδειος πίνακας για αποθήκευση αποτελεσμάτων
results <- data.frame(
  model_vars = character(),
  bic = numeric(),
  aic = numeric(),
  N = numeric(),
  stringsAsFactors = FALSE
)
##############################################################

#6. Τρέχει όλα τα πιθανά μοντέλα. Με 7 μεταβλητές:
for (i in 1:(2^p - 1)) {
  
  # Ποιες μεταβλητές μπαίνουν στο μοντέλο
  selected_vars <- candidates[as.logical(intToBits(i)[1:p])]
  
  # Formula
  formula_i <- as.formula(
    paste("lnQ ~", paste(selected_vars, collapse = " + "))
  )
  
  # Regression
  model_i <- lm(formula_i, data = df_model)
  
  # Store results
  results <- rbind(
    results,
    data.frame(
      model_vars = paste(selected_vars, collapse = " + "),
      bic = BIC(model_i),
      aic = AIC(model_i),
      N = nobs(model_i),
      stringsAsFactors = FALSE
    )
  )
}

##############################################################
# Ταξινόμηση με βάση BIC
results <- results[order(results$bic), ]

# Top 10 models
head(results, 10)

############################################################################################################################
############################################################################################################################
# ΔΙΚΕΣ ΜΟΥ ΠΑΤΕΝΤΕΣ
############################################################################################################################

##############################################################
# ARIMA MODEL SELECTION BY AIC/BIC
##############################################################

arima_results <- data.frame(
  p = integer(),
  d = integer(),
  q = integer(),
  aic = numeric(),
  bic = numeric(),
  stringsAsFactors = FALSE
)

d_value <- 0   # αν η lnQ είναι stationary
# d_value <- 1 # αν χρειάζεται differencing

for (p in 0:5) {
  for (q in 0:5) {
    
    model_try <- tryCatch(
      arima(lnQ, order = c(p, d_value, q)),
      error = function(e) NULL
    )
    
    if (!is.null(model_try)) {
      arima_results <- rbind(
        arima_results,
        data.frame(
          p = p,
          d = d_value,
          q = q,
          aic = AIC(model_try),
          bic = BIC(model_try)
        )
      )
    }
  }
}

arima_results <- arima_results[order(arima_results$bic), ]

head(arima_results, 10)

best_p <- arima_results$p[1]
best_d <- arima_results$d[1]
best_q <- arima_results$q[1]

best_arima <- arima(
  lnQ,
  order = c(best_p, best_d, best_q)
)

best_arima

##############################################################
# ARIMA MODEL SELECTION BY AIC/BIC
##############################################################

arima_results <- data.frame(
  p = integer(),
  d = integer(),
  q = integer(),
  aic = numeric(),
  bic = numeric(),
  stringsAsFactors = FALSE
)

d_value <- 1   # αν η lnQ είναι stationary
# d_value <- 1 # αν χρειάζεται differencing

for (p in 0:5) {
  for (q in 0:5) {
    
    model_try <- tryCatch(
      arima(lnQ, order = c(p, d_value, q)),
      error = function(e) NULL
    )
    
    if (!is.null(model_try)) {
      arima_results <- rbind(
        arima_results,
        data.frame(
          p = p,
          d = d_value,
          q = q,
          aic = AIC(model_try),
          bic = BIC(model_try)
        )
      )
    }
  }
}

arima_results <- arima_results[order(arima_results$bic), ]

head(arima_results, 10)

best_p <- arima_results$p[1]
best_d <- arima_results$d[1]
best_q <- arima_results$q[1]
best_p 
best_d
best_q

best_arima_2 <- arima(
  lnQ,
  order = c(best_p, best_d, best_q)
)

best_arima
best_arima_2
BIC(best_arima)
BIC(best_arima_2)
############################################################################################################################
############################################################################################################################

##############################################################
##############################################################
# 5. FINAL SELECTED SPECIFICATION
# 5.3 - ΔΙΠΛΩΜΑΤΙΚΗ  ΠΙΝΑΚΑΣ 6.3 , column 1
##############################################################
##############################################################

#product 1 

xvars <- c(
  "lnPpV",
  "lnnd",
  "christmas_dummy",
  "easter_dummy",
  "promo_share"
)
##############################################################
#product 4 
xvars <- c(
  "lnPpV",
  "lnnd",
  "discount_1",
  "easter_dummy",
  "promo_share"
)

##############################################################
# 6. BASELINE OLS AND DIAGNOSTICS
##############################################################

library(car)
library(lmtest)

# Final specification
formula_final <- as.formula(
  paste("lnQ ~", paste(xvars, collapse = " + "))
)

# OLS regression
OLS_model <- lm(formula_final, data = df_model)

summary(OLS_model)


# Multicollinearity

vif(OLS_model)


# Heteroskedasticity
bptest(OLS_model)

# Autocorrelation
bgtest(OLS_model, order = 1)
bgtest(OLS_model, order = 2)
bgtest(OLS_model, order = 3)

# Functional Form / RESET Test
resettest(OLS_model)

##############################################################
# 7. NEWEY-WEST DYNAMIC MODEL 
# 5.3 - ΔΙΠΛΩΜΑΤΙΚΗ  ΠΙΝΑΚΑΣ  ΠΙΝΑΚΑΣ 6.3 , column 2
##############################################################

library(lmtest)
library(sandwich)

#------------------------------------------------------------------------------
# Το varsoc στο Stata χρησιμοποιείται κυρίως για VAR lag-order selection. Εσύ εδώ έχεις single-equation dynamic regression, όχι VAR
# Το αντίστοιχο που μπορείς να κάνεις στο R είναι να συγκρίνεις μοντέλα με 1, 2, 3, 4 lags της lnQ με AIC/BIC.

models_lag <- list()

for (k in 1:5) {
  
  df_temp <- df_model
  
  for (j in 1:k) {
    df_temp[[paste0("lag_lnQ_", j)]] <- c(rep(NA, j), head(df_temp$lnQ, -j))
  }
  
  df_temp <- na.omit(df_temp)
  
  lag_vars <- paste0("lag_lnQ_", 1:k)
  
  formula_k <- as.formula(
    paste(
      "lnQ ~",
      paste(c(lag_vars, xvars), collapse = " + ")
    )
  )
  
  models_lag[[k]] <- lm(formula_k, data = df_temp)
}

lag_selection <- data.frame(
  Lags = 1:5,
  AIC = sapply(models_lag, AIC),
  BIC = sapply(models_lag, BIC)
)

lag_selection

#Lags       AIC       BIC
#1    1 -157.6854 -134.4432
#2    2 -159.4238 -133.2763
#3    3 -177.0485 -147.9957
#4    4 -179.5170 -147.6408
#5    5 -178.2889 -143.6047

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ΤΡΕΞΕ META APO EDV ΔΙΟΡΘΩΜΕΝΟ ΑΡΧΕΙΟ: LAG SELECTION 

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#------------------------------------------------------------------------------


# CREATE LAGS OF lnQ

df_model$lag_lnQ_1 <- c(NA, head(df_model$lnQ, -1))
df_model$lag_lnQ_2 <- c(NA, NA, head(df_model$lnQ, -2))
df_model$lag_lnQ_3 <- c(NA, NA, NA, head(df_model$lnQ, -3))

# REMOVE MISSING VALUES
df_dynamic <- na.omit(df_model)

# DYNAMIC FORMULA
dynamic_formula <- as.formula(
  paste(
    "lnQ ~ lag_lnQ_1 + lag_lnQ_2 + lag_lnQ_3 +",
    paste(xvars, collapse = " + ")
  )
)
#------------------------------------------------------------------------------
# ESTIMATION
#------------------------------------------------------------------------------

# OLS dynamic regression
# ΠΙΝΑΚΑΣ 6.3 , column 2 - δεν είναι αυτα τα standard errors 
NW_model <- lm(dynamic_formula, data = df_model)
summary(NW_model)

#------------------------------------------------------------------------------
# Newey-West robust covariance matrix

#δεν σημαίνει «βάζω 3 lags στο μοντέλο».
#Σημαίνει ότι η HAC διόρθωση των standard errors λαμβάνει υπόψη αυτοσυσχέτιση μέχρι lag 3

NW_se <- NeweyWest(NW_model, lag = 3)
summary(NW_se)

#------------------------------------------------------------------------------

# Robust results
# ΠΙΝΑΚΑΣ 6.3 , column 2 - με τα σωστα standart errors !!!!
coeftest(NW_model, vcov = NW_se)


#------------------------------------------------------------------------------
#Διαγνωστικοί Έλεγχοι 
#
Box.test(residuals(NW_model), lag = 40, type = "Ljung-Box")
acf(residuals(NW_model),
    main = "ACF των καταλοίπων του υποδείγματος Newey–West")


pacf(residuals(NW_model),
     main = "PACF των καταλοίπων του υποδείγματος Newey–West")


##############################################################
# 8. UNIT ROOT TESTS - Στασιμότητα
##############################################################

library(tseries)


length(lnQ)
length(df_model$lnQ)

start(lnQ)
end(lnQ)

head(lnQ)
head(df_model$lnQ)

-------------------------------
  
adf.test(lnQ, k = 0)#στασ
adf.test(lnQ, k = 1) #στασ
adf.test(lnQ, k = 2) #στασ
adf.test(lnQ, k = 3)#=adf.test(df_model$lnQ, k = 3)-p-value = 0.04242 στασ
adf.test(lnQ, k = 4) #στασ
adf.test(lnQ, k = 5)#= adf.test(lnQ) οχι στασ

#  | k | p-value |
#  | - | ------- |
#  | 0 | < 0.01  |
#  | 1 | < 0.01  |
#  | 2 | 0.0216  |
#  | 3 | 0.0424  |
#  | 4 | 0.0741  |
#  | 5 | 0.4262  |
  
#Αυτό που βλέπω εγώ
#Για k=2: στάσιμη.
#Για k=3: στάσιμη.
#Για k=4: οριακή.
#Για k=5: μη στάσιμη.

#Άρα το συμπέρασμα δεν είναι καθαρό.
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AUGMENTED DICKEY-FULLER TEST
# Equivalent to: dfuller lnQ, lags(3)
adf_lnQ1 <- adf.test(df_model$lnQ, k = 3)
adf_lnQ2 <- adf.test(lnQ)

print(adf_lnQ1)
print(adf_lnQ2)

acf(df_model$lnQ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PHILLIPS-PERRON TEST
# Equivalent to: pperron lnQ, lags(3)

pp_lnQ <- pp.test(df_model$lnQ)

print(pp_lnQ)

adf.test(lnQ)
pp.test(lnQ)



#----------> και τα δυο λενε ότι έχω στασιμη χρονοσειρά 


##############################################################
# 9. ARIMA BENCHMARK
##############################################################

library(forecast)


# ESTIMATE ARIMA(3,0,0)
# Equivalent to: arima lnQ, arima(3,0,0)

ARIMA_model <- arima(df_model$lnQ,
                     order = c(3,0,0))

summary(ARIMA_model)

#

-------------------------
  # ESTIMATE ARIMA(3,1,1)
  # Equivalent to: arima lnQ, arima(3,1,1)
  
ARIMA_model2 <- arima(lnQ,
                       order = c(3,1,1))

summary(ARIMA_model2) 
  
#------------------------------------------------------------------------------


# INFORMATION CRITERIA
# Equivalent to: estat ic
AIC(ARIMA_model)

BIC(ARIMA_model)

#------------------------------------------------------------------------------
#DIAGNOSTIC CHECKING

# RESIDUALS
# Equivalent to: predict res_arima, resid
res_arima <- residuals(ARIMA_model)


# CORRELOGRAM OF RESIDUALS
# Equivalent to: corrgram res_arima, lags(40)
acf(res_arima,
    lag.max = 40,
    main = "ACF of ARIMA Residuals")

pacf(res_arima,
     lag.max = 40,
     main = "PACF of ARIMA Residuals")


# LJUNG-BOX WHITE NOISE TEST
# Equivalent to: wntestq res_arima
Box.test(res_arima,
         lag = 40,
         type = "Ljung-Box")


# AR ROOTS
# Equivalent to: estat aroots
polyroot(c(1, -ARIMA_model$coef[1:3]))

#AC TEST 
#Q(20)
print(ac.test(
  Residuals_ARIMA311,
  max.lag = 20,
  var.name = "Residuals_ARIMA311",
  plot = c(TRUE,FALSE)
))
##############################################################
# 10. ARIMAX MODEL
##############################################################
#product 1 

xvars <- c(
  "lnPpV",
  "lnnd",
  "christmas_dummy",
  "easter_dummy",
  "promo_share"
)
##############################################################
#product 4 
xvars <- c(
  "lnPpV",
  "lnnd",
  "discount_1",
  "easter_dummy",
  "promo_share"
)


# X variables
X_arimax <- as.matrix(
  df_model[, xvars]
)
#------------------------------------------------------------------------------

# ARIMAX(3,0,0)
# Equivalent to: arima lnQ $xvars, arima(3,0,0)
ARIMAX_model <- Arima(
  df_model$lnQ,
  order = c(3, 0, 0),
  xreg = X_arimax
)

summary(ARIMAX_model)
#------------------------------------------------------------------------------

# ARIMAX(3,1,1)
# Equivalent to: arima lnQ $xvars, arima(3,0,0)
ARIMAX_model2 <- Arima(
  df_model$lnQ,
  order = c(3, 1, 1),
  xreg = X_arimax
)

summary(ARIMAX_model2)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# INFORMATION CRITERIA
# Equivalent to: estat ic
AIC(ARIMAX_model)
BIC(ARIMAX_model)

#------------------------------------------------------------------------------
# RESIDUALS
# Equivalent to: predict res_arimax, resid
res_arimax <- residuals(ARIMAX_model)

# CORRELOGRAM OF RESIDUALS
# Equivalent to: corrgram res_arimax, lags(40)
acf(res_arimax,
    lag.max = 40,
    main = "ACF of ARIMAX Residuals")

pacf(res_arimax,
     lag.max = 40,
     main = "PACF of ARIMAX Residuals")


# LJUNG-BOX TEST
# Equivalent to: wntestq res_arimax
Box.test(res_arimax,
         lag = 40,
         type = "Ljung-Box")


# WALD TEST
library(aod)

wald.test(  Sigma = est.final$var.coef,  b = est.final$coef,  Terms = 1:8)
wald.test(  b     = ARIMAX_model$coef,  Sigma = ARIMAX_model$var.coef,  Terms = 1:8)
wald.test(  b     = ARIMAX_model$coef,  Sigma = ARIMAX_model$var.coef,  Terms = c(1, 2, 3, 5, 6, 7, 8, 9))

#Jarque Bera
jarque.bera.test(res_arimax)


# AR ROOTS
# Equivalent to: estat aroots
ar_coefs <- ARIMAX_model$model$phi

polyroot(c(1, -ar_coefs))


# AR ROOTS - GRAPH INVERSE ROOTS

# AR coefficients από το ARIMAX
ar_coef_arimax <- coef(ARIMAX_model)[c("ar1", "ar2", "ar3")]

# Ρίζες του AR πολυωνύμου
roots_arimax <- polyroot(c(1, -ar_coef_arimax))

# Αντίστροφες ρίζες
inv_roots_arimax <- 1 / roots_arimax

# Διάγραμμα
plot(Re(inv_roots_arimax), Im(inv_roots_arimax),
     xlim = c(-1.2, 1.2),
     ylim = c(-1.2, 1.2),
     xlab = "Real",
     ylab = "Imaginary",
     main = "Αντίστροφες ρίζες του AR πολυωνύμου",
     pch = 19)

# Μοναδιαίος κύκλος
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)

# Άξονες
abline(h = 0, v = 0, lty = 2)


#AC TEST 
#Q(20)
print(ac.test(  res_arimax,  max.lag = 40,  var.name = "Residuals ARIMAΧ",  plot = c(TRUE,FALSE)))
############################################################################################################################
############################################################################################################################
############################################################################################################################
##############################################################
# 11. OPTIONAL SARIMA / SARIMAX ROBUSTNESS
##############################################################


# SARIMA
# arima lnQ, arima(3,0,0) sarima(1,0,0,52)


SARIMA_model <- Arima(
  df_model$lnQ,
  order = c(3, 0, 0),
  seasonal = list(order = c(1, 0, 0), period = 52)
)

summary(SARIMA_model)

AIC(SARIMA_model)
BIC(SARIMA_model)

##############################################################
# SARIMAX
# Stata: arima lnQ $xvars, arima(3,0,0) sarima(1,0,0,52)
##############################################################

X_sarimax <- as.matrix(
  df_model[, xvars]
)

SARIMAX_model <- Arima(
  df_model$lnQ,
  order = c(3, 0, 0),
  seasonal = list(order = c(1, 0, 0), period = 52),
  xreg = X_sarimax
)

summary(SARIMAX_model)

AIC(SARIMAX_model)
BIC(SARIMAX_model)

##############################################################
# SARIMAX
# arima lnQ $xvars, arima(3,0,0) sarima(1,0,0,52)
##############################################################

X_sarimax <- as.matrix(
  df_model[, xvars]
)

SARIMAX_model <- Arima(
  df_model$lnQ,
  order = c(3, 0, 0),
  seasonal = list(order = c(1, 0, 0), period = 52),
  xreg = X_sarimax
)

summary(SARIMAX_model)

AIC(SARIMAX_model)
BIC(SARIMAX_model)



############################################################################################################################
############################################################################################################################
############################################################################################################################
##############################################################
# 12. EXPORT MODEL TABLES
##############################################################
#install.packages("stargazer")
library(stargazer)

##############################################################
# MAIN MODELS TABLE - αν ειναι μονο για να φτιάξει αυτόματα τα tables , το έχω κανει αυτο ήδη χειροκίνητα
##############################################################


OLS_model<-OLS_model
NW_model<-NW_model
ARIMA_model<- est.final_ARIMA300
ARIMAX_model<- ARIMAX_model

class(OLS_model)
class(NW_model)
class(ARIMA_model)
class(ARIMAX_model)

#Tο δεν αναγνωριζει τα υποδειγματα arima 
#"lm"
#"lm"
#"forecast_ARIMA"
#"forecast_ARIMA"

stargazer(
  OLS_model,
  NW_model,
  ARIMA_model,
  ARIMAX_model,
  
  type = "text",
  
  title = "Main Demand Models",
  
  column.labels = c(
    "OLS",
    "Newey-West",
    "ARIMA",
    "ARIMAX"
  ),
  
  digits = 4,
  
  star.cutoffs = c(0.10, 0.05, 0.01),
  
  out = "final_main_models.txt"
)

##############################################################
# SARIMA ROBUSTNESS TABLE
##############################################################

stargazer(
  SARIMA_model,
  SARIMAX_model,
  
  type = "text",
  
  title = "Seasonal ARIMA Robustness Models",
  
  column.labels = c(
    "SARIMA",
    "SARIMAX"
  ),
  
  digits = 4,
  
  star.cutoffs = c(0.10, 0.05, 0.01),
  
  out = "sarima_robustness.txt"
)

##############################################################
# 13. FORECASTING - TRAIN / TEST SPLIT
##############################################################

##############################################################
# TIME INDEX
##############################################################

df_model$t_index <- 1:nrow(df_model)

##############################################################
# TRAIN / TEST DUMMIES
##############################################################

df_model$train <- ifelse(df_model$t_index <= 120, 1, 0)

df_model$test <- ifelse(df_model$t_index > 120, 1, 0)

##############################################################
# 14. ARIMA FORECAST
##############################################################

library(forecast)

# Train / test split
train_data <- df_model[df_model$train == 1, ]
test_data  <- df_model[df_model$test == 1, ]

# ARIMA(3,0,0) στο train sample
arima_train <- Arima(
  train_data$lnQ,
  order = c(3, 0, 0)
)

# Forecast για όσες παρατηρήσεις έχει το test
f_arima_obj <- forecast(
  arima_train,
  h = nrow(test_data)
)

# Predicted values
f_arima <- as.numeric(f_arima_obj$mean)

# Errors
e_arima  <- test_data$lnQ - f_arima
ae_arima <- abs(e_arima)
se_arima <- e_arima^2

# MAE / RMSE
MAE_arima  <- mean(ae_arima, na.rm = TRUE)
RMSE_arima <- sqrt(mean(se_arima, na.rm = TRUE))

MAE_arima
RMSE_arima

##############################################################
# 15. ARIMAX FORECAST
##############################################################

# X variables για train και test
X_train <- as.matrix(train_data[, xvars])
X_test  <- as.matrix(test_data[, xvars])

# ARIMAX(3,0,0) στο train sample
arimax_train <- Arima(  train_data$lnQ,  order = c(3, 0, 0),  xreg = X_train)

# Forecast με future xreg
f_arimax_obj <- forecast(  arimax_train,  h = nrow(test_data),  xreg = X_test)

# Predicted values
f_arimax <- as.numeric(f_arimax_obj$mean)

# Errors
e_arimax  <- test_data$lnQ - f_arimax
ae_arimax <- abs(e_arimax)
se_arimax <- e_arimax^2

# MAE / RMSE
MAE_arimax  <- mean(ae_arimax, na.rm = TRUE)
RMSE_arimax <- sqrt(mean(se_arimax, na.rm = TRUE))

MAE_arimax
RMSE_arimax

##############################################################
# 16. FORECAST ACCURACY COMPARISON
##############################################################

cat("ARIMA MAE:   ", MAE_arima, "\n")
cat("ARIMAX MAE:  ", MAE_arimax, "\n")
cat("ARIMA RMSE:  ", RMSE_arima, "\n")
cat("ARIMAX RMSE: ", RMSE_arimax, "\n")

##################################
##     Πίνακας σύγκρισης 
##################################

forecast_accuracy <- data.frame(
  Model = c("ARIMA", "ARIMAX"),
  MAE = c(MAE_arima, MAE_arimax),
  RMSE = c(RMSE_arima, RMSE_arimax)
)

forecast_accuracy

##############################################################
# Actual vs Forecast: ARIMAX  <- ΑΥΤΟ ΠΟΥ ΘΕΛΩ ΝΑ ΔΕΙΞΩ ΤΟΣΗ ΩΡΑ 
##############################################################


plot(time(ts(df_model$lnQ))[df_model$test == 1],
     test_data$lnQ,
     type = "l",
     col = "darkblue",
     lwd = 2.5,
     main = "Actual vs Forecast: ARIMAX",
     xlab = "Time",
     ylab = "lnQ")

lines(time(ts(df_model$lnQ))[df_model$test == 1],
      f_arimax,
      col = "red",
      lwd = 2.5,
      lty = 2)

legend("topleft",
       legend = c("Actual", "Forecast"),
       col = c("darkblue", "red"),
       lwd = 2.5,
       lty = c(1, 2),
       bty = "n")

grid(col = "gray90", lty = "dashed")


############################################################################################################################
############################################################################################################################
############################################################################################################################
##############################################################
# 15. ARIMAX FORECAST
##############################################################

# X variables για train και test
X_train <- as.matrix(train_data[, xvars])
X_test  <- as.matrix(test_data[, xvars])

# ARIMAX(3,0,0) στο train sample
arimax_train <- Arima(  train_data$lnQ,  order = c(3, 1, 1),  xreg = X_train)

# Forecast με future xreg
f_arimax_obj <- forecast(  arimax_train,  h = nrow(test_data),  xreg = X_test)

# Predicted values
f_arimax <- as.numeric(f_arimax_obj$mean)

# Errors
e_arimax  <- test_data$lnQ - f_arimax
ae_arimax <- abs(e_arimax)
se_arimax <- e_arimax^2

# MAE / RMSE
MAE_arimax  <- mean(ae_arimax, na.rm = TRUE)
RMSE_arimax <- sqrt(mean(se_arimax, na.rm = TRUE))

MAE_arimax
RMSE_arimax

##############################################################
# 16. FORECAST ACCURACY COMPARISON
##############################################################

cat("ARIMA MAE:   ", MAE_arima, "\n")
cat("ARIMAX MAE:  ", MAE_arimax, "\n")
cat("ARIMA RMSE:  ", RMSE_arima, "\n")
cat("ARIMAX RMSE: ", RMSE_arimax, "\n")

##################################
##     Πίνακας σύγκρισης <---------- ΑΥΤΟ ΜΠΑΙΝΕΙ ΣΤΗΝ ΕΡΓΑΣΙΑ
##################################

forecast_accuracy <- data.frame(
  Model = c("ARIMA", "ARIMAX"),
  MAE = c(MAE_arima, MAE_arimax),
  RMSE = c(RMSE_arima, RMSE_arimax)
)

forecast_accuracy

##############################################################
# Actual vs Forecast: ARIMAX  <- ΑΥΤΟ ΠΟΥ ΘΕΛΩ ΝΑ ΔΕΙΞΩ ΤΟΣΗ ΩΡΑ 
##############################################################


plot(time(ts(df_model$lnQ))[df_model$test == 1],
     test_data$lnQ,
     type = "l",
     col = "darkblue",
     lwd = 2.5,
     main = "Actual vs Forecast: ARIMAX(3,1,1)",
     xlab = "Time",
     ylab = "lnQ")

lines(time(ts(df_model$lnQ))[df_model$test == 1],
      f_arimax,
      col = "red",
      lwd = 2.5,
      lty = 2)

legend("topleft",
       legend = c("Actual", "Forecast"),
       col = c("darkblue", "red"),
       lwd = 2.5,
       lty = c(1, 2),
       bty = "n")

grid(col = "gray90", lty = "dashed")



range(test_data$lnQ)
test_data$lnQ

nrow(train_data)
nrow(test_data)

head(test_data)
tail(test_data)

names(df_model)

############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################



##############################################################
# 1. Ορίζεις τη χρονοσειρά
##############################################################


ts_VolumeSales_P4_Total <- ts(VolumeSales_P4_Total,  start = c(2022, 1),  end = c(2024, 34),  frequency = 52)




##############################################################
#2. Κάνεις αρχικό γράφημα πωλήσεων 
#   Στόχος: να δείξεις ότι οι πωλήσεις έχουν ανοδική τάση.
##############################################################
plot(ts_VolumeSales_P1_Total, 
     main = "Χρονοσειρά Όγκου Πωλήσεων",
     ylab = "Πωλήσεις (kg)", 
     xlab = "Χρόνος",
     col = "darkblue", 
     lwd = 2.5,
     type = "l",
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1,
     xaxt = "n")
grid(lwd = 0.5, col = "gray80")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)

labels <- c("Jan2022","Jul2022",
            "Jan2023","Jul2023",
            "Jan2024","Jul2024",
            "Jan2025")

axis(1, at = ticks, labels = labels)

##############################################################
#3. Παίρνεις λογάριθμο
##############################################################

Y <- log(ts_VolumeSales_P4_Total)
identical(Y, lnQ)
plot(Y, 
     main = "Λογαριθμοποιημένη Χρονοσειρά Όγκου Πωλήσεων",
     ylab = "Πωλήσεις (kg)", 
     xlab = "Χρόνος",
     col = "orange", 
     lwd = 2.5,
     type = "l", 
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1,
     xaxt = "n")
grid(lwd = 0.5, col = "gray80")

grid(col = "gray90", lty = "dashed")

# Δημιουργία custom labels
ticks <- seq(2022, 2025, by = 0.5)

labels <- c("Jan2022","Jul2022",
            "Jan2023","Jul2023",
            "Jan2024","Jul2024",
            "Jan2025")

axis(1, at = ticks, labels = labels)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~ Identification Stage του Box–Jenkins. ~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##############################################################
#4.1 Δείχνεις επίσημα την τάση με linear trend model
##############################################################

N <- NROW(Y)
trend <- seq(1, N, 1)

est.lt <- lm(Y ~ trend)
summary(est.lt)

# Ο συντελεστής της χρονικής τάσης είναι ΑΡΝΗΤΙΚΟΣ και ΣΤΑΤΙΣΤΙΚΑ ΣΗΜΑΝΤΙΚΟΣ, 
# άρα οι πωλήσεις παρουσιάζουν αυξητική τάση διαχρονικά.

#Εκτιμήθηκε γραμμικό υπόδειγμα τάσης για τον λογάριθμο των πωλήσεων προκειμένου 
#να διερευνηθεί η ύπαρξη μακροχρόνιας τάσης στη χρονοσειρά.
#Ο συντελεστής της χρονικής τάσης εκτιμήθηκε αρνητικός και στατιστικά σημαντικός
#(p<0.001), γεγονός που υποδηλώνει πτωτική πορεία των πωλήσεων κατά την εξεταζόμενη περίοδο.
#Συνεπώς, η χρονοσειρά δεν μπορεί να θεωρηθεί στάσιμη ως προς τον μέσο της και
#απαιτείται μετασχηματισμός διαφορών στο πλαίσιο της μεθοδολογίας Box–Jenkins.


##############################################################
#4.2 Μετά σχεδιάζεις την εκτιμημένη τάση:
##############################################################

fit.trend <- fitted.values(est.lt)

plot(cbind(Y, fit.trend),
     plot.type = "single",main = "Λογαριθμοποιημένη Χρονοσειρά Ζήτησης και Εκτιμημένη Γραμμική Τάση",
     type = "l",
     lty = 1,
     col = c("blue", "red"),
     ylab = "",
     xlab = "Time")

legend("top",
       legend = c("ln(Q)", "Εκτιμημένη Τάση"),
       lty = 1,
       col = c("blue", "red"))

##############################################################
#5. Αφαιρείς την τάση προσωρινά για να τη δεις καθαρά
##############################################################

#Αυτό είναι detrending, όπως στην Άσκηση 08.

Y_detrended <- residuals(est.lt)

ts.plot(Y_detrended, col = "blue", ylab = "")
legend("top", legend = "Detrended ln(Volume Sales)", lty = 1, col = "blue")
abline(h = 0)

#Αυτό δεν είναι απαραίτητα το τελικό Box–Jenkins μοντέλο. Είναι κυρίως για να 
#δείξεις ότι αφαιρώντας την τάση, η σειρά γίνεται πιο γύρω από το μηδέν.

##############################################################
#6. Κάνεις 1η διαφορά για να διορθώσεις την τάση
##############################################################
#Reference: Άσκηση 05, ερώτημα 1 και Άσκηση 07, ερώτημα 1.

r <- diff(Y, lag = 1)

ts.plot(r,
        col = "blue",
        ylab = "",
        main = "Πρώτες Διαφορές της Λογαριθμοποιημένης Χρονοσειράς Πωλήσεων")

legend("top",
       legend = "Πρώτες Διαφορές του ln(Q)",
       lty = 1,
       col = "blue")

abline(h = 0)
#Ερμηνεία:
#  
#  Η σειρά των πρώτων διαφορών εκφράζει τον εβδομαδιαίο ρυθμό μεταβολής των 
#πωλήσεων. Με αυτόν τον μετασχηματισμό αφαιρείται η μακροχρόνια τάση.

##############################################################
#7.1 Ελέγχεις ACF / PACF για τη στάσιμη σειρά - Διάγνωση στασιμότητας μέσω κορελογραμμάτων
##############################################################

#Reference: Άσκηση 07, ερώτημα 1.
#Τι κοιτάς:
#Αν στο Y οι αυτοσυσχετίσεις πέφτουν αργά → μη στάσιμη σειρά.
#Αν στο r οι αυτοσυσχετίσεις πέφτουν γρήγορα → οι πρώτες διαφορές είναι πιο στάσιμες.
#Αν υπάρχουν spikes στα lags 52, 104 κτλ. → υπάρχει ετήσια εποχικότητα.

library(testcorr)

#\subsubsection{Έλεγχος Στασιμότητας μέσω Χορελογραμμάτων}
#Για τη διερεύνηση της στασιμότητας της χρονοσειράς
#κατασκευάστηκαν τα χορελογραφήματα (ACF) του
#λογαρίθμου των πωλήσεων και των πρώτων διαφορών του.

print(ac.test(Y, max.lag = 52, var.name = "ln(Q)", plot = c(TRUE, FALSE)))

#Το χορελόγραμμα του λογαρίθμου των πωλήσεων παρουσιάζει
#υψηλές θετικές αυτοσυσχετίσεις οι οποίες φθίνουν αργά
#καθώς αυξάνεται η υστέρηση.

#Η συμπεριφορά αυτή αποτελεί ένδειξη μη στασιμότητας
#και υποδηλώνει την ύπαρξη τάσης στη χρονοσειρά.

#------------------------------------------------------------------------------
#Οι πρώτες διαφορές του λογαρίθμου των πωλήσεων ορίζονται ως:
#rt=ΔYt=Yt−Yt−1

print(ac.test(r, max.lag = 52, var.name = "First Differences of ln(Q) ", plot = c(TRUE, FALSE)))

#Το χορελόγραμμα των πρώτων διαφορών παρουσιάζεισημαντικά χαμηλότερες 
#αυτοσυσχετίσεις σε σχέση με την αρχική χρονοσειρά.
#Οι αυτοσυσχετίσεις φθίνουν ταχύτερα και κυμαίνονται γύρω από το μηδέν,γεγονός 
#που υποδηλώνει ότι οι πρώτες διαφορές είναι περισσότερο συμβατές με στάσιμη διαδικασία.

#------------------------------------------------------------------------------

#Συνεπώς, για τη χρονοσειρά των πωλήσεων επιλέγεται διαφοροποίηση πρώτης τάξης,
# δηλαδή \(d=1\), στο πλαίσιο της μεθοδολογίας Box--Jenkins.

#Παράλληλα, παρατηρούνται ενδείξεις εποχικής συσχέτισης σε υψηλές υστερήσεις
#(ιδιαίτερα κοντά στην υστέρηση 52),γεγονός που ενδέχεται να υποδηλώνει
#ετήσια εποχικότητα στα εβδομαδιαία δεδομένα.

##############################################################
#7.2 Έλεγχοι Μονάδας Ρίζας: ADF και Phillips-Perron
##############################################################

# Στόχος:
# Να επιβεβαιώσουμε στατιστικά αν ο λογάριθμος των πωλήσεων Y είναι μη στάσιμος
# και αν οι πρώτες διαφορές r = ΔY είναι στάσιμες.

library(tseries)

# Augmented Dickey-Fuller test για τον λογάριθμο των πωλήσεων
adf_Y <- adf.test(Y)
print(adf_Y)

# Augmented Dickey-Fuller test για τις πρώτες διαφορές
adf_r <- adf.test(r)
print(adf_r)

# Phillips-Perron test για τον λογάριθμο των πωλήσεων
pp_Y <- pp.test(Y)
print(pp_Y)

# Phillips-Perron test για τις πρώτες διαφορές
pp_r <- pp.test(r)
print(pp_r)

identical(Y, df_model$lnQ)

#\subsubsection{Έλεγχοι Μονάδας Ρίζας}

#Πέρα από τη γραφική διερεύνηση της στασιμότητας μέσω των χορελογραμμάτων,
#πραγματοποιήθηκαν και τυπικοί έλεγχοι μονάδας ρίζας μέσω των ελέγχων
#Augmented Dickey--Fuller (ADF) και Phillips--Perron (PP).

#Η μηδενική υπόθεση και των δύο ελέγχων είναι ότι η χρονοσειρά παρουσιάζει 
#μονάδα ρίζα,δηλαδή είναι μη στάσιμη.

#Για τον λογάριθμο των πωλήσεων,ο έλεγχος ADF δεν απορρίπτει τη μηδενική υπόθεση
#μονάδας ρίζας (\(p = 0.426\)),γεγονός που αποτελεί ένδειξη μη στασιμότητας
#της αρχικής χρονοσειράς.

#Αντίθετα, τόσο ο έλεγχος ADF όσο και ο έλεγχος Phillips--Perron για τις πρώτες διαφορές
#του λογαρίθμου των πωλήσεων απορρίπτουν τη μηδενική υπόθεση μονάδας ρίζας
#(\(p < 0.01\)),υποδεικνύοντας ότι η διαφοροποιημένη σειρά είναι στάσιμη.

#Παρότι ο έλεγχος Phillips--Perron εμφανίζει ενδείξεις στασιμότητας ακόμη και για την αρχική
#λογαριθμική σειρά,η συνολική εικόνα από τα χορελογραφήματα,το γραμμικό υπόδειγμα τάσης
#και τον έλεγχο ADF υποστηρίζει την ύπαρξη μη στασιμότητας στην αρχική χρονοσειρά.

#Συνεπώς, τα αποτελέσματα επιβεβαιώνουν την επιλογή διαφοροποίησης πρώτης τάξης,
#δηλαδή \(d=1\),στο πλαίσιο της μεθοδολογίας Box--Jenkins.

##############################################################
#8. Ελέγχεις αν χρειάζεται εποχική διαφορά λόγω εβδομαδιαίων δεδομένων
##############################################################

#Επειδή έχεις frequency 52, πιθανή εποχικότητα σημαίνει ότι συγκρίνεις κάθε 
# εβδομάδα με την ίδια εβδομάδα προηγούμενου έτους.

#------------------------------------------------------------------------------
#\subsubsection{Εποχική Διαφοροποίηση}
#Για τη διερεύνηση πιθανής εποχικότητας στα εβδομαδιαία δεδομένα πωλήσεων,
#υπολογίστηκαν οι εποχικές διαφορές 52 εβδομάδων των πρώτων διαφορών του
#λογαρίθμου των πωλήσεων.

#\begin{equation}
#w_t = \Delta_{52} \Delta Y_t
#= (1 - L^{52})(1 - L)Y_t
#\end{equation}

#(Πρακτικά:συγκρίνεις την εβδομάδα με την ίδια εβδομάδα προηγούμενου έτους.)

w <- diff(r, lag = 52)

ts.plot(w, col = "blue", ylab = "")
legend("top", legend = "Seasonal difference of first differences", lty = 1, col = "blue")
abline(h = 0)

#Η εποχικά διαφοροποιημένη σειρά κυμαίνεται γύρω από σταθερό μέσο χωρίς εμφανή 
#τάση,γεγονός που αποτελεί ένδειξηστασιμότητας.
#------------------------------------------------------------------------------
print(ac.test(w, max.lag = 52, var.name = "Seasonal difference of first differences", plot = c(TRUE, FALSE)))

#Παράλληλα, το χορελόγραμμα της σειράς παρουσιάζει σημαντικά χαμηλότερες 
#αυτοσυσχετίσεις σε σχέση με τις προηγούμενες μορφές της χρονοσειράς,
#ενώ η εποχική δομή έχει αποδυναμωθεί αισθητά.
#------------------------------------------------------------------------------
#Συνεπώς, τα αποτελέσματα υποδηλώνουν ότι η χρονοσειρά των πωλήσεων απαιτεί
# μία μη εποχική διαφοροποίηση (\(d=1\)) και μία εποχική διαφοροποίηση 52 περιόδων
# (\(D=1\)),στο πλαίσιο της μεθοδολογίας Seasonal Box--Jenkins.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~   Estimation Stage του Box–Jenkins    ~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##############################################################
#9. Επιλογή ARIMA / SARIMA με AIC και BIC
##############################################################

library(forecast)

max.p <- 3
max.q <- 3

matrix.ic <- matrix(NA, nrow = 0, ncol = 4)

for (q in 0:max.q) {
  for (p in 0:max.p) {
    est.arima <- Arima(Y, order = c(p, 1, q), include.constant = TRUE)
    matrix.ic <- rbind(matrix.ic, c(p, q, est.arima$aic, est.arima$bic))
  }
}


colnames(matrix.ic) <- c("p", "q", "AIC", "BIC")
print(as.data.frame(matrix.ic), row.names = FALSE)

#d=1

#p q        AIC        BIC
#0 0  -51.17571  -45.33575
#1 0  -76.92637  -68.16643
#2 0 -105.74851  -94.06858
#3 0 -104.81779  -90.21789

#0 1 -109.51009 -100.75015
#1 1 -107.55991  -95.87999
#2 1 -107.89269  -93.29279
#3 1 -112.72695  -95.20707
#0 2 -107.58343  -95.90351
#1 2 -111.05225  -96.45235
#2 2 -107.75785  -90.23796
#3 2 -110.78990  -90.35003
#0 3 -107.91303  -93.31313
#1 3 -109.39372  -91.87383
#2 3 -111.31530  -90.87543
#3 3 -109.17210  -85.81225

#d=0

#p q        AIC       BIC
#0 0  -47.37037 -41.51586
#1 0  -88.26528 -79.48352
#2 0  -94.89538 -83.18636
#3 0 -112.98028 -98.34401

#0 1  -75.64051 -66.85874
#1 1 -111.49115 -99.78214
#2 1 -109.49116 -94.85489
#3 1 -111.23462 -93.67110
#0 2  -76.34158 -64.63256
#1 2 -109.49117 -94.85491
#2 2 -112.89484 -95.33132
#3 2 -110.48864 -89.99787
#0 3  -94.45849 -79.82222
#1 3 -110.68406 -93.12054
#2 3 -111.62405 -91.13328
#3 3 -113.46648 -90.04845

#------------------------------------------------------------------------------
max.p <- 2
max.q <- 2
max.P <- 1
max.Q <- 1

matrix.ic <- matrix(NA, nrow = 0, ncol = 6)

for (q in 0:max.q) {
  for (p in 0:max.p) {
    for (P in 0:max.P) {
      for (Q in 0:max.Q) {
        
        est.sarima <- Arima(
          Y,
          order = c(p, 1, q),
          seasonal = c(P, 1, Q),
          method = "CSS"
        )
        
        matrix.ic <- rbind(
          matrix.ic,
          c(p, q, P, Q, est.sarima$aic, est.sarima$bic)
        )
      }
    }
  }
}
# 'system is exactly singular' 
# σημαίνει ότι κάποιο από τα SARIMA μοντέλα που δοκίμασε δεν μπορεί να εκτιμηθεί σωστά

colnames(matrix.ic) <- c("p", "q", "P", "Q", "AIC", "BIC")
print(as.data.frame(matrix.ic), row.names = FALSE)

# 'system is exactly singular' 
# σημαίνει ότι κάποιο από τα SARIMA μοντέλα που δοκίμασε δεν μπορεί να εκτιμηθεί σωστά.

#Πιθανές αιτίες:
#έχεις λίγα δεδομένα για seasonal ARIMA με περίοδο 52,
#έχεις μόνο περίπου 2.5 χρόνια εβδομαδιαίων παρατηρήσεων,
#το seasonal differencing με lag 52 “τρώει” πολλές παρατηρήσεις,
#κάποιο μοντέλο είναι υπερβολικά σύνθετο.

#Άρα δεν φταις εσύ. Είναι λογικό.
#------------------------------------------------------------------------------

#Για το SARIMA, κάνε πιο ασφαλές loop

max.p <- 2
max.q <- 2
max.P <- 1
max.Q <- 1

matrix.ic.sarima <- matrix(NA, nrow = 0, ncol = 6)

for (q in 0:max.q) {
  for (p in 0:max.p) {
    for (P in 0:max.P) {
      for (Q in 0:max.Q) {
        
        fit <- try(
          Arima(
            Y,
            order = c(p, 1, q),
            seasonal = list(order = c(P, 1, Q), period = 52),
            method = "CSS"
          ),
          silent = TRUE
        )
        
        if (!inherits(fit, "try-error")) {
          matrix.ic.sarima <- rbind(
            matrix.ic.sarima,
            c(p, q, P, Q, fit$aic, fit$bic)
          )
        }
      }
    }
  }
}

colnames(matrix.ic.sarima) <- c("p", "q", "P", "Q", "AIC", "BIC")
print(as.data.frame(matrix.ic.sarima), row.names = FALSE)

#Παρότι εντοπίστηκαν ενδείξεις εποχικής δομής στις εβδομαδιαίες πωλήσεις,
#η εκτίμηση εποχικών υποδειγμάτων SARIMA δεν κατέστη εφικτή με σταθερό τρόπο,
#πιθανόν λόγω του περιορισμένου αριθμού διαθέσιμων παρατηρήσεων σε σχέση
#με τη μεγάλη εποχική περίοδο (\(s=52\)).

#Για τον λόγο αυτό,η ανάλυση συνεχίζεται με μη εποχικά υποδείγματα ARIMA(p,1,q),
#τα οποία παρουσιάζουν ικανοποιητική συμπερ  ιφορά ως προς τη στασιμότητα και τα 
#διαγνωστικά κατάλοιπα. 
#------------------------------------------------------------------------------
#ΥΠΟΨΗΦΙΑ ΜΟΝΤΕΛΑ

# Με βάση AIC : ARIMA(3,1,1)

# Με βάση BIC : ARIMA(0,1,1)
#------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~  Diagnostic Checking του Box–Jenkins  ~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##############################################################
#10. Εκτιμάς το τελικό μοντέλο
##############################################################

#Reference: Άσκηση 05, ερώτημα 3 και Άσκηση 07, ερώτημα 3.

#######################ARIMA300###############################
est.final_ARIMA300 <- Arima(   Y,   order = c(3,0,0),   include.constant = TRUE)

print(est.final_ARIMA300)
summary (est.final_ARIMA300)
##############################################################
est.final_ARIMA311 <- Arima(   Y,   order = c(3,1,1),   include.constant = TRUE)

print(est.final_ARIMA311)
summary (est.final_ARIMA311)
##############################################################
est.final_ARIMA011 <- Arima(  Y,  order = c(0,1,1),  include.constant = TRUE,  method = "CSS")

print(est.final_BIC)

##############################################################
#11. Παίρνεις κατάλοιπα
##############################################################
#Reference: Άσκηση 05, ερώτημα 4 και Άσκηση 07, ερώτημα 4.

par(mfrow = c(1, 2))

#\subsubsection{Διαγνωστικός Έλεγχος Καταλοίπων}
Residuals_ARIMA300 <- est.final_ARIMA300$residuals

Residuals_ARIMA311 <- est.final_ARIMA311$residuals
#--------------------------------------------------
ts.plot(Residuals_ARIMA311, col = "blue", ylab = "")
legend("top", legend = "Residuals_ARIMA311", lty = 1, col = "blue")
abline(h = 0)

Residuals_BIC <- est.final_BIC$residuals
#--------------------------------------------------
ts.plot(Residuals_ARIMA300, col = "blue", ylab = "")
legend("top", legend = "Residuals_ARIMA300", lty = 1, col = "blue")
abline(h = 0)
par(mfrow = c(1, 1)) 
#--------------------------------------------------
Residuals_BIC <- est.final_BIC$residuals

ts.plot(Residuals_BIC, col = "blue", ylab = "")
legend("top", legend = "Residuals_BIC", lty = 1, col = "blue")
abline(h = 0)
par(mfrow = c(1, 1)) 
#Τα κατάλοιπα πρέπει να μοιάζουν με λευκό θόρυβο.

#Για την αξιολόγηση των υποδειγμάτων ARIMA(3,1,1) και ARIMA(0,1,1)
#εξετάστηκαν τα κατάλοιπα των εκτιμημένων υποδειγμάτων.Τα κατάλοιπα και των δύο 
#υποδειγμάτων κυμαίνονται γύρω από το μηδέν χωρίς εμφανή τάση ή συστηματική δομή,
#γεγονός που αποτελεί ένδειξη ότι τα υποδείγματα αποτυπώνουν ΙΚΑΝΟΠΟΙΗΤΙΚΑ 
#ΥΠΟΔΕΙΓΜΑΤΑ για τη δυναμική της χρονοσειράς.


#Παρά το γεγονός ότι το υπόδειγμα ARIMA(3,1,1) παρουσιάζει ελαφρώς χαμηλότερο 
#AIC,το υπόδειγμα ARIMA(0,1,1) επιλέγεται ως τελικό, καθώς είναι πιο φειδωλό,
#παρουσιάζει στατιστικά σημαντικό συντελεστή κινητού μέσου και εμφανίζει παρόμοια
#συμπεριφορά καταλοίπων.

library(testcorr)
-----------------------------------------------
#Q(20)
print(ac.test(
  Residuals_ARIMA311,
  max.lag = 20,
  var.name = "Residuals_ARIMA311",
  plot = c(TRUE,FALSE)
))

Box.test(
  Residuals_ARIMA311,
  lag = 20,
  type = "Ljung"
)

---------------------------------------------
#Q(40)
print(ac.test(
  Residuals_ARIMA311,
  max.lag = 40,
  var.name = "Residuals ARIMA(3,1,1)",
  plot = c(TRUE,FALSE)
))

Box.test(
  Residuals_ARIMA311,
  lag = 40,
  type = "Ljung"
)



-----------------------------------------------
  #Q(20)
  print(ac.test(
    Residuals_ARIMA300,
    max.lag = 20,
    var.name = "Residuals_ARIMA300",
    plot = c(TRUE,FALSE)
  ))

Box.test(
  Residuals_ARIMA300,
  lag = 20,
  type = "Ljung"
)

---------------------------------------------
  #Q(40)
  print(ac.test(
    Residuals_ARIMA300,
    max.lag = 40,
    var.name = "Residuals ARIMA(3,0,0)",
    plot = c(TRUE,FALSE)
  ))

Box.test(
  Residuals_ARIMA300,
  lag = 40,
  type = "Ljung"
)
##############################################################
#12. Κάνεις diagnostics στα κατάλοιπα
##############################################################
#Reference: Άσκηση 05, ερώτημα 4 και Άσκηση 07, ερώτημα 4.

#\subsubsection{Διαγνωστικός Έλεγχος του Υποδείγματος}

#Για το τελικό υπόδειγμα ARIMA(0,1,1)
#εξετάστηκαν τα κατάλοιπα και τα τετράγωνα
#των καταλοίπων μέσω χορελογραμμάτων.

library(lmtest)
library(aod)

Residuals <- Residuals_ARIMA300
est.final <- est.final_ARIMA300
#------------------------------------------------------------------------------

print(ac.test(Residuals, max.lag = 20, var.name = "Residuals", plot = c(TRUE, FALSE)))
print(ac.test(Residuals, max.lag = 40, var.name = "Residuals", plot = c(TRUE, FALSE)))

#Τα χορελογραφήματα των καταλοίπων δεν παρουσιάζουν έντονη αυτοσυσχέτιση,
#γεγονός που υποδηλώνει ότι το υπόδειγμα ,αποτυπώνει ικανοποιητικά τη δυναμική
#της χρονοσειράς.
#------------------------------------------------------------------------------
print(ac.test(Residuals^2, max.lag = 20, var.name = "Squared Residuals", plot = c(TRUE, FALSE)))
print(ac.test(Residuals^2, max.lag = 40, var.name = "Squared Residuals", plot = c(TRUE, FALSE)))

#Αντίστοιχα, τα χορελογραφήματα των τετραγώνων των καταλοίπων δεν εμφανίζουν
#ισχυρή συστηματική δομή μεταβλητότητας.
#------------------------------------------------------------------------------

jarque.bera.test(Residuals)

#------------------------------------------------------------------------------
print(coeftest(est.final))

#Ο συντελεστής κινητού μέσου MA(1) εκτιμήθηκε στατιστικά σημαντικός (\(p<0.001\)),
#ενώ ο όρος drift δεν βρέθηκε στατιστικά σημαντικός.
#------------------------------------------------------------------------------
print(wald.test(
  b = coef(est.final),
  Sigma = vcov(est.final),
  Terms = 1:length(coef(est.final))
))
est.final
#Επιπλέον, πραγματοποιήθηκε έλεγχος Wald για τη συνολική στατιστική σημαντικότητα
#του υποδείγματος. Η μηδενική υπόθεση ότι όλοι οι συντελεστές του υποδείγματος είναι ταυτόχρονα 
#ίσοι με μηδέν απορρίπτεται σε επίπεδο σημαντικότητας \( \alpha = 0.05 \) 
#(\(X^2 = 160.1\), \(p < 0.001\)).


library(aod)

wald.test(
  Sigma = est.final$var.coef,
  b = est.final$coef,
  Terms = 1:3
)

#------------------------------------------------------------------------------

# AR coefficients
ar_coef <- coef(est.final)[c("ar1", "ar2", "ar3")]

# Roots of AR polynomial
roots <- polyroot(c(1, -ar_coef))

# Inverse roots
inv_roots <- 1 / roots

# Plot inverse roots
plot(Re(inv_roots), Im(inv_roots),
     xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     xlab = "Real", ylab = "Imaginary",
     main = "Inverse roots of AR polynomial",
     pch = 19)

# Unit circle
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)

# Axes
abline(h = 0, v = 0, lty = 2)


#Συνεπώς, το υπόδειγμα ARIMA(0,1,1) θεωρείται κατάλληλο για την περιγραφή
#και πρόβλεψη της χρονοσειράς των εβδομαδιαίων πωλήσεων.

##############################################################
#13. Παίρνεις fitted values και τα συγκρίνεις με τις πραγματικές πωλήσεις
##############################################################
#Reference: Άσκηση 05, ερώτημα 5 και Άσκηση 07, ερώτημα 5.

fit.Y <- fitted.values(est.final)
fit.sales <- exp(fit.Y)

ts.plot(cbind(ts_VolumeSales_P4_Total, fit.sales),
        col = c("blue", "red"),
        ylab = "Volume Sales")

legend("top",
       legend = c("Actual Sales", "Fitted Sales"),
       lty = 1,
       col = c("blue", "red"))

#Στο Σχήμα παρουσιάζονται οι πραγματικές εβδομαδιαίες πωλήσεις του προϊόντος
#και οι προσαρμοσμένες τιμές του υποδείγματος ARIMA(0,1,1).

#Το υπόδειγμα αποτυπώνει ικανοποιητικά τη γενική δυναμική και τη μεσοχρόνια
#πορεία των πωλήσεων,παρουσιάζοντας ομαλή προσαρμογή στις βασικές διακυμάνσεις 
#της χρονοσειράς.Παράλληλα, το υπόδειγμα δεν αναπαράγει πλήρως ορισμένες έντονες
#βραχυχρόνιες αιχμές των πωλήσεων,οι οποίες ενδέχεται να σχετίζονται με 
#προωθητικές ενέργειες,εκπτώσεις ή άλλους εξωγενείς παράγοντες.

#Συνολικά, το υπόδειγμα ARIMA(0,1,1) παρουσιάζει ικανοποιητική προσαρμογή
#στα δεδομένα των εβδομαδιαίων πωλήσεων και μπορεί να χρησιμοποιηθεί
#για σκοπούς πρόβλεψης.

#------------------------------------------------------------------------------
#Συνεπώς, το υπόδειγμα ARIMA μπορεί να θεωρηθεί ότι περιγράφει τη ``φυσιολογική''
#ή οργανική ζήτηση#(\textit{baseline demand}), ενώ οι μεταβλητές marketing 
#αναμένεται να εξηγούν #τις επιπλέον αποκλίσεις και τις βραχυχρόνιες διακυμάνσεις
#των πωλήσεων στο πλαίσιο ενός #arketing Mix Modeling υποδείγματος.
#------------------------------------------------------------------------------

acf(lnQ,
    main = "Συνάρτηση Αυτοσυσχέτισης (ACF) της ζήτησης (lnQ)")

pacf(lnQ,
    main = "Συνάρτηση Μερικής Αυτοσυσχέτισης (PACF) της ζήτησης (lnQ)")




acf(lnQ,
    main = "Συνάρτηση Αυτοσυσχέτισης (ACF) της ζήτησης (lnQ)")

pacf(lnQ,
     main = "Συνάρτηση Μερικής Αυτοσυσχέτισης (PACF) της ζήτησης (lnQ)")
