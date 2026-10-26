# Econometric Analysis and Demand Forecasting Using Time Series Models

R code for the empirical part of my diploma thesis (National Technical University of Athens, School of Applied Mathematical and Physical Sciences, 2026).

The thesis studies weekly sales of a single consumer product (**138 weekly observations**, 2022–2024) and asks two questions:

1. How are price, distribution, discounts, promotional activity and seasonality related to demand?
2. Do dynamic time-series models (ARIMA, ARIMAX) forecast demand better than a purely autoregressive benchmark?

> **Data confidentiality.** The data are confidential and are **not** included in this repository. `data/sample/` contains a **synthetic** data set with the same structure so that the code can be run end to end. Results obtained on the synthetic data are not the thesis results. See [`DATA.md`](DATA.md).

## Methods

| Step | What is done | Script |
|---|---|---|
| Exploration | Time-series plots, descriptive statistics, correlation matrix | `R/02_exploratory_plots.R` |
| Variable selection | Exhaustive search over 2⁷ − 1 = 127 OLS models, ranked by BIC (AIC as a secondary criterion) | `R/03_variable_selection_bic.R` |
| Static OLS | ln(volume) on price, numeric distribution, discount, Easter dummy, promotional share; VIF, Breusch–Pagan, Breusch–Godfrey, RESET | `R/04_ols_diagnostics.R` |
| Dynamic regression | Lags of ln(volume) chosen by BIC on a common sample; Newey–West (HAC) standard errors | `R/05_dynamic_newey_west.R` |
| Identification | ADF (several lag orders) and Phillips–Perron tests, ACF/PACF, trend and differences | `R/06_stationarity_identification.R` |
| ARIMA | Grid of ARIMA(p,0,q) by AIC/BIC; ARIMA(3,0,0) benchmark; Ljung–Box, Jarque–Bera, AR roots | `R/07_arima.R` |
| ARIMAX | Regression with ARIMA(3,0,0) errors; same diagnostics | `R/08_arimax.R` |
| Forecast evaluation | 120 weeks for estimation, 18 weeks held out; MAE and RMSE on the log scale | `R/09_forecast_evaluation.R` |
| Sensitivity (extra) | Out-of-sample comparison across ARMA orders | `R/10_sensitivity_arma_order.R` |
| Seasonal models (optional) | SARIMA/SARIMAX with period 52 | `R/11_seasonal_robustness.R` |

Shared settings (regressors, calendar windows, number of lags, train/test split) are in `R/00_setup.R`; variable construction is in `R/01_prepare_data.R`.

## Main results (thesis, real data)

| Model | Price elasticity | Distribution (ND) elasticity |
|---|---|---|
| Static OLS | −1.57 | 0.55 |
| Dynamic regression (3 lags, Newey–West) | −1.12 | 0.49 |
| ARIMAX (AR(3) errors) | −1.79 | 1.04 |

* Demand is price-elastic and increases with numeric distribution in all models; the signs are stable across specifications. Discount, Easter and promotional-share effects change with the model and are not significant in the ARIMAX.
* Residuals of the static and dynamic regressions remain autocorrelated, which motivates the ARIMA/ARIMAX models. Their residuals show no significant autocorrelation (Ljung–Box).
* Out-of-sample (18 weeks, log scale):

| Model | MAE | RMSE |
|---|---|---|
| ARIMA(3,0,0) | 0.1030 | 0.1328 |
| ARIMAX | 0.0616 | 0.0788 |

  ARIMAX errors are about 40% lower. This comparison is **conditional**: the ARIMAX forecasts use the observed values of the regressors in the test period.

## Limitations

* 138 weekly observations are too few for reliable annual (period-52) seasonal modelling; seasonal models often fail to converge.
* Coefficients are conditional associations, not causal effects: price, distribution and promotions may be set in response to expected demand.
* BIC differences between the leading models are small, so more than one specification fits comparably well; `R/10_sensitivity_arma_order.R` checks that the forecasting conclusion does not hinge on one ARMA order.
* The forecast evaluation uses a single 18-week test window, and ARIMAX forecasts treat future regressors as known.


## Author

Sotiria Kontothanasi — NTUA Applied Mathematical and Physical Sciences (Mathematics track, Statistics and Informatics).
