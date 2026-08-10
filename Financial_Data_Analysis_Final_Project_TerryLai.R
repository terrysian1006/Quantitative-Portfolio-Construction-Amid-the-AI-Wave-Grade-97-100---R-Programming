# -------------------------------------------

##Financial_Data_Analysis_Final_Project_112652022
##Name: 賴李育賢
##Student ID: 112652022
##Dept.:法科所3年級
##Title: 人性溢價與反 AI 烏托邦：AI 狂潮下的逆勢投資組合建構與績效比較


rm(list=ls(all=TRUE))
setwd("~/Downloads/Exercise4")

source("function_FDA.R") 
library(xts)
library(PerformanceAnalytics)
library("quantmod")

retx <- function(x){  
  x[-1]/x[-length(x)]-1 
}

library(quantmod)

tickers <- c(
  "CHGG", "FVRR", "UPWK", "COUR", "DLB", "PSO", "LOPE",
  "DIS", "NTDOY", "NYT", "SONY", "WMG", "CMCSA",
  "CRWD", "PANW", "OKTA", "EFX", "FICO", "FTNT",
  "LYV", "LVMUY", "ABNB", "MAR", "SBUX", "LVS", "DAL", "HCA", "BKNG",
  "SPY", "QQQ", "GLD", "TLT", "USO", "DBA", "VNQ", "EWW", "XLU"
)

getSymbols(Symbols = tickers, src = 'yahoo', from = "2021-11-20", to = "2024-12-31")

first_data <- get(tickers[1])
prices_df <- data.frame(Date = as.Date(index(first_data)), first_data[, 6], row.names = NULL)
colnames(prices_df)[2] <- tickers[1]

for(i in 2:length(tickers)){
  sym <- tickers[i]
  temp_data <- get(sym)
  temp_df <- data.frame(Date = as.Date(index(temp_data)), temp_data[, 6], row.names = NULL)
  colnames(temp_df)[2] <- sym
  prices_df <- merge(prices_df, temp_df, by = "Date", all = TRUE)
}

result_df <- data.frame(matrix(0, nrow(prices_df), ncol(prices_df)))
result_df[, 1] <- prices_df[, 1]
colnames(result_df) <- colnames(prices_df)

result_df[2:nrow(result_df), 2:ncol(result_df)] <- apply(prices_df[, 2:ncol(prices_df)], 2, retx)

result_df <- result_df[-1, ]

returns_3Yr <- result_df[result_df[, 1] >= "2021-12-01" & result_df[, 1] <= "2024-12-31", ]
returns_3Yr <- na.omit(returns_3Yr) 

write.table(returns_3Yr, file = "Returns_3Yr_AI_Shock.csv", sep = ",", row.names = FALSE)

cat("✅ 3年期報酬率資料處理完畢！總天數：", nrow(returns_3Yr), "天\n")


source("~/Downloads/function_FDA.R")

stats_3Yr <- rbind(
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, summary),                        
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, var) * 252,                      
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, sd) * sqrt(252),                 
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, my_skewness) / sqrt(252),        
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, my_kurtosis) / 252,              
  apply(returns_3Yr[, 2:ncol(returns_3Yr)], 2, my_acf1)                         
)

rownames(stats_3Yr)[7:nrow(stats_3Yr)] <- c("Var", "Std.", "Skewness", "Kurtosis", "ACF1")

table_3Yr <- t(round(stats_3Yr, 3))

write.csv(table_3Yr, file = "Descriptive_Stats_3Yr.csv")


cor_mat_3Yr <- cor(returns_3Yr[, 2:ncol(returns_3Yr)], use = "complete.obs")
cor_mat_3Yr <- round(cor_mat_3Yr, 3)
write.csv(cor_mat_3Yr, file = "Correlation_Matrix_3Yr.csv")


dates_3yr <- as.Date(returns_3Yr[[1]]) 
ret_core <- as.matrix(returns_3Yr[, -1])
y_limits_ret <- range(ret_core, na.rm = TRUE)

plot(x = dates_3yr, y = ret_core[, 1], type = "l", col = "gray", 
     ylim = y_limits_ret, xlab = "Date", ylab = "Daily Returns", 
     main = "Time Series of Individual Asset Returns (2021-2024)",
     cex.axis = 1.5, cex.lab = 1.2, cex.main = 1.2)

for(j in 2:ncol(ret_core)) {
  lines(x = dates_3yr, y = ret_core[, j], col = "gray")
}

spy_idx <- grep("SPY", colnames(ret_core))
if(length(spy_idx) > 0) {
  lines(x = dates_3yr, y = ret_core[, spy_idx], col = "#0D3B66", lwd = 2)
}


wealth_3yr <- apply(ret_core, 2, function(x) cumprod(1 + x))
y_limits_w <- range(wealth_3yr, na.rm = TRUE)

plot(x = dates_3yr, y = wealth_3yr[, 1], type = "l", col = "gray", 
     ylim = y_limits_w, xlab = "Date", ylab = "Cumulative Wealth ($)", 
     main = "Time Series of Individual Asset Cumulative Wealth (2021-2024)",
     cex.axis = 1.5, cex.lab = 1.2, cex.main = 1.2)

for(j in 2:ncol(wealth_3yr)) {
  lines(x = dates_3yr, y = wealth_3yr[, j], col = "gray")
}

if(length(spy_idx) > 0) {
  lines(x = dates_3yr, y = wealth_3yr[, spy_idx], col = "#0D3B66", lwd = 2)
}


source("~/Downloads/function_FDA.R")
library(quadprog)

data_3Yr <- read.table("Returns_3Yr_AI_Shock.csv", header = TRUE, sep = ",")
dates_3Yr <- as.Date(data_3Yr[, 1])
data_ret <- as.matrix(data_3Yr[, 2:ncol(data_3Yr)])

N_assets <- ncol(data_ret)
T_days <- nrow(data_ret)
cat("目前資產檔數：", N_assets, "檔\n")

rf_daily <- 0.025 / 252          
mu_targ_daily <- 0.10 / 252       


wx_eq <- rep(1 / N_assets, N_assets)
port_ret_eq <- data_ret %*% wx_eq

ann_return <- mean(port_ret_eq) * 252
ann_volatility <- sd(port_ret_eq) * sqrt(252)
sharpe_ratio <- (ann_return - 0.025) / ann_volatility

cat("\n【1/N 等權重策略 (3年期 AI 衝擊組) 績效表現】\n")
cat("年化報酬率:", round(ann_return * 100, 2), "%\n")
cat("年化波動度:", round(ann_volatility * 100, 2), "%\n")
cat("夏普指標:", round(sharpe_ratio, 4), "\n")


asset_wealth <- matrix(0, T_days, N_assets)
for(j in 1:N_assets){
  asset_wealth[, j] <- (1/N_assets) * cumprod(1 + data_ret[, j])
}
total_wealth_bh <- rowSums(asset_wealth)

port_ret_bh <- c(0, total_wealth_bh[-1] / total_wealth_bh[-length(total_wealth_bh)] - 1)

ann_return_bh <- mean(port_ret_bh) * 252
ann_volatility_bh <- sd(port_ret_bh) * sqrt(252)
sharpe_ratio_bh <- (ann_return_bh - 0.025) / ann_volatility_bh

cat("\n【Buy and Hold 策略 (3年期 AI 衝擊組) 績效表現】\n")
cat("年化報酬率:", round(ann_return_bh * 100, 2), "%\n")
cat("年化波動度:", round(ann_volatility_bh * 100, 2), "%\n")
cat("夏普指標:", round(sharpe_ratio_bh, 4), "\n")


wx_gmvp <- gmvp_wx_quad(data_ret)
names(wx_gmvp) <- colnames(data_ret)

cat("\n【GMVP 最佳化權重分配 - Top 3 做多與放空】\n")
print(round(head(sort(wx_gmvp, decreasing = TRUE), 3), 4))
print(round(tail(sort(wx_gmvp, decreasing = TRUE), 3), 4))

port_ret_gmvp <- data_ret %*% wx_gmvp
ann_return_gmvp <- mean(port_ret_gmvp) * 252
ann_volatility_gmvp <- sd(port_ret_gmvp) * sqrt(252)
sharpe_ratio_gmvp <- (ann_return_gmvp - 0.025) / ann_volatility_gmvp

cat("\n【GMVP 策略 (允許放空) - 績效表現】\n")
cat("夏普指標:", round(sharpe_ratio_gmvp, 4), "\n")


wx_ns_gmvp <- nsgmvp_wx_quad(data_ret)
names(wx_ns_gmvp) <- colnames(data_ret)

wx_ns_gmvp[wx_ns_gmvp < 1e-4] <- 0
wx_ns_gmvp <- wx_ns_gmvp / sum(wx_ns_gmvp)

cat("\n【No-Shortsale GMVP 最佳化權重分配 (僅列做多部位)】\n")
print(round(sort(wx_ns_gmvp[wx_ns_gmvp > 0], decreasing = TRUE), 4))

port_ret_nsgmvp <- data_ret %*% wx_ns_gmvp


wx_mvp <- mvp_wx_quad(data_ret, mu_targ_daily)
names(wx_mvp) <- colnames(data_ret)

port_ret_mvp <- data_ret %*% wx_mvp


wx_ns_mvp <- nsmvp_wx_quad(data_ret, mu_targ_daily)
names(wx_ns_mvp) <- colnames(data_ret)
wx_ns_mvp[wx_ns_mvp < 1e-4] <- 0

port_ret_nsmvp <- data_ret %*% wx_ns_mvp

# -------------------------------------------
# 策略七：Standard Tangency Portfolio (切線組合)
# -------------------------------------------
# 呼叫老師的函數！
wx_tan <- tan_wx(data_ret, rf_daily)
names(wx_tan) <- colnames(data_ret)

port_ret_tan <- data_ret %*% wx_tan


data_ret <- na.omit(data_ret)

kx <- 252                      
hx <- nrow(data_ret) - kx     

por_eq <- numeric(hx); tor_eq <- numeric(hx)
por_bh <- numeric(hx); tor_bh <- numeric(hx)
por_gmvp <- numeric(hx); tor_gmvp <- numeric(hx)
por_nsgmvp <- numeric(hx); tor_nsgmvp <- numeric(hx)
por_mvp <- numeric(hx); tor_mvp <- numeric(hx)
por_nsmvp <- numeric(hx); tor_nsmvp <- numeric(hx)
por_tan <- numeric(hx); tor_tan <- numeric(hx)

wx_mat_eq <- matrix(0, hx + 1, N_assets)
wx_mat_bh <- matrix(0, hx + 1, N_assets)
wx_mat_gmvp <- matrix(0, hx + 1, N_assets)
wx_mat_nsgmvp <- matrix(0, hx + 1, N_assets)
wx_mat_mvp <- matrix(0, hx + 1, N_assets)
wx_mat_nsmvp <- matrix(0, hx + 1, N_assets)
wx_mat_tan <- matrix(0, hx + 1, N_assets)

wx_mat_eq[1, ] <- rep(1 / N_assets, N_assets)
wx_mat_bh[1, ] <- rep(1 / N_assets, N_assets)


epx <- 3.5 / 1000  

cat("開始執行樣本外滾動回測，請稍候...\n")

for(i in 1:hx) {
  
  datax <- data_ret[i:(i + kx - 1), ]
  
  rx <- as.numeric(data_ret[i + kx, ])
  rx_lag <- as.numeric(data_ret[i + kx - 1, ])
  
  wx_eq <- rep(1 / N_assets, N_assets)
  
  wx_bh <- wx_mat_bh[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_bh[i, ] * rx_lag))
  
  wx_gmvp <- gmvp_wx_quad(datax)
  wx_nsgmvp <- nsgmvp_wx_quad(datax)
  wx_mvp <- mvp_wx_quad(datax, mu_targ_daily)
  wx_nsmvp <- nsmvp_wx_quad(datax, mu_targ_daily)
  wx_tan <- tan_wx(datax, rf_daily)
  
  wx_nsgmvp[wx_nsgmvp < 1e-4] <- 0; wx_nsgmvp <- wx_nsgmvp / sum(wx_nsgmvp)
  wx_nsmvp[wx_nsmvp < 1e-4] <- 0; wx_nsmvp <- wx_nsmvp / sum(wx_nsmvp)
  
  drift_eq <- wx_mat_eq[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_eq[i, ] * rx_lag))
  drift_bh <- wx_mat_bh[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_bh[i, ] * rx_lag))
  drift_gmvp <- wx_mat_gmvp[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_gmvp[i, ] * rx_lag))
  drift_nsgmvp <- wx_mat_nsgmvp[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_nsgmvp[i, ] * rx_lag))
  drift_mvp <- wx_mat_mvp[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_mvp[i, ] * rx_lag))
  drift_nsmvp <- wx_mat_nsmvp[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_nsmvp[i, ] * rx_lag))
  drift_tan <- wx_mat_tan[i, ] * (1 + rx_lag) / (1 + sum(wx_mat_tan[i, ] * rx_lag))
  
  tor_eq[i] <- sum(abs(wx_eq - drift_eq))
  tor_bh[i] <- sum(abs(wx_bh - drift_bh)) # 理論上應為 0
  tor_gmvp[i] <- sum(abs(wx_gmvp - drift_gmvp))
  tor_nsgmvp[i] <- sum(abs(wx_nsgmvp - drift_nsgmvp))
  tor_mvp[i] <- sum(abs(wx_mvp - drift_mvp))
  tor_nsmvp[i] <- sum(abs(wx_nsmvp - drift_nsmvp))
  tor_tan[i] <- sum(abs(wx_tan - drift_tan))
  
  
  por_eq[i] <- (1 + sum(wx_eq * rx)) * (1 - epx * tor_eq[i]) - 1
  por_bh[i] <- (1 + sum(wx_bh * rx)) * (1 - epx * tor_bh[i]) - 1
  por_gmvp[i] <- (1 + sum(wx_gmvp * rx)) * (1 - epx * tor_gmvp[i]) - 1
  por_nsgmvp[i] <- (1 + sum(wx_nsgmvp * rx)) * (1 - epx * tor_nsgmvp[i]) - 1
  por_mvp[i] <- (1 + sum(wx_mvp * rx)) * (1 - epx * tor_mvp[i]) - 1
  por_nsmvp[i] <- (1 + sum(wx_nsmvp * rx)) * (1 - epx * tor_nsmvp[i]) - 1
  por_tan[i] <- (1 + sum(wx_tan * rx)) * (1 - epx * tor_tan[i]) - 1
  
  wx_mat_eq[i+1, ] <- wx_eq
  wx_mat_bh[i+1, ] <- wx_bh
  wx_mat_gmvp[i+1, ] <- wx_gmvp
  wx_mat_nsgmvp[i+1, ] <- wx_nsgmvp
  wx_mat_mvp[i+1, ] <- wx_mvp
  wx_mat_nsmvp[i+1, ] <- wx_nsmvp
  wx_mat_tan[i+1, ] <- wx_tan
}

cat("✅ 樣本外滾動回測 (", hx, "天 ) 執行完畢！\n")


library(PerformanceAnalytics) 

oos_returns <- data.frame(
  X1.N = por_eq, B.H = por_bh, GMVP = por_gmvp, NS.GMVP = por_nsgmvp,
  MVP.10 = por_mvp, NS.MVP.10 = por_nsmvp, Tangency = por_tan
)

Ann_Ret <- apply(oos_returns, 2, mean) * 252
Ann_Vol <- apply(oos_returns, 2, sd) * sqrt(252)
Sharpe  <- (Ann_Ret - 0.025) / Ann_Vol # 嚴格扣除年化無風險利率 2.5%

VaR_05 <- apply(oos_returns, 2, quantile, probs = 0.05)

ES_05  <- numeric(ncol(oos_returns))
LPSD   <- numeric(ncol(oos_returns))

for(j in 1:ncol(oos_returns)) {
  tmp_ret <- oos_returns[, j]
  ES_05[j] <- mean(tmp_ret[tmp_ret <= VaR_05[j]])
  LPSD[j]  <- sd(tmp_ret[tmp_ret < 0]) * sqrt(252)
}
names(ES_05) <- colnames(oos_returns)
names(LPSD)  <- colnames(oos_returns)

Turnover <- c(mean(tor_eq), mean(tor_bh), mean(tor_gmvp), mean(tor_nsgmvp), 
              mean(tor_mvp), mean(tor_nsmvp), mean(tor_tan))

HHI <- numeric(7)
SLR <- numeric(7)
mats_list <- list(wx_mat_eq, wx_mat_bh, wx_mat_gmvp, wx_mat_nsgmvp, wx_mat_mvp, wx_mat_nsmvp, wx_mat_tan)

for(m in 1:7) {
  w_tmp <- mats_list[[m]][-1, ] # 依照老師習慣，扣除第一列期初權重
  
  hhi_daily <- rowSums(w_tmp^2) / (rowSums(abs(w_tmp))^2)
  HHI[m] <- mean(hhi_daily, na.rm = TRUE)
  
  w_neg <- w_tmp; w_neg[w_neg > 0] <- 0
  w_pos <- w_tmp; w_pos[w_pos < 0] <- 0
  slr_daily <- rowSums(abs(w_neg)) / rowSums(abs(w_pos))
  SLR[m] <- mean(slr_daily, na.rm = TRUE)
}

MDD <- apply(oos_returns, 2, maxDrawdown)

Final_Performance_Table <- rbind(Ann_Ret, Ann_Vol, Sharpe, VaR_05, ES_05, LPSD, Turnover, HHI, SLR, MDD)
rownames(Final_Performance_Table) <- c("Ann_Ret", "Ann_Vol", "Sharpe", "VaR_05", "ES_05", "LPSD", "Turnover", "HHI", "SLR", "MDD")

Final_Performance_Table <- round(Final_Performance_Table, 3)

cat("\n【樣本外 OoS 投資組合策略績效總表結算】\n")
print(t(Final_Performance_Table)) # 轉置成橫向表格，方便你複製到 Word 

write.table(Final_Performance_Table, file = "OoS_Performance_Table_Ultimate.csv", sep = ",", col.names = NA)



library(xts)

oos_dates <- as.Date(data_3Yr[(kx + 1):nrow(data_3Yr), 1])

wealth_eq     <- cumprod(1 + por_eq)
wealth_bh     <- cumprod(1 + por_bh)
wealth_gmvp   <- cumprod(1 + por_gmvp)
wealth_nsgmvp <- cumprod(1 + por_nsgmvp)
wealth_mvp    <- cumprod(1 + por_mvp)
wealth_nsmvp  <- cumprod(1 + por_nsmvp)
wealth_tan    <- cumprod(1 + por_tan)

y_limits <- range(c(wealth_eq, wealth_bh, wealth_gmvp, wealth_nsgmvp, 
                    wealth_mvp, wealth_nsmvp, wealth_tan), na.rm = TRUE)

plot(x = oos_dates, y = wealth_eq, type = "l", col = "#0D3B66", lwd = 2,
     ylim = y_limits, xlab = "Date", ylab = "Cumulative Wealth ($)",
     main = "Out-of-Sample Cumulative Wealth of 7 Strategies",
     cex.axis = 1.5, cex.lab = 1.2, cex.main = 1.2)

lines(x = oos_dates, y = wealth_bh, col = "#D4AC0D", lwd = 2, lty = 2)     # B&H
lines(x = oos_dates, y = wealth_nsgmvp, col = "#41521F", lwd = 2)          # NS-GMVP
lines(x = oos_dates, y = wealth_nsmvp, col = "#9A2735", lwd = 2)           # NS-MVP
lines(x = oos_dates, y = wealth_gmvp, col = "#0B0014", lwd = 1)            # GMVP
lines(x = oos_dates, y = wealth_mvp, col = "#A675A1", lwd = 1)             # MVP
lines(x = oos_dates, y = wealth_tan, col = "#F194B4", lwd = 1)             # Tangency

abline(h = 1, col = "black", lty = 3)

legend("topright",
       legend = c("1/N (Equal)", "B&H", "NS-GMVP", "NS-MVP(10%)", "GMVP (Short)", "MVP (Short)", "Tangency (Short)"),
       col = c("#0D3B66", "#D4AC0D", "#41521F", "#9A2735", "#0B0014", "#A675A1", "#F194B4"),
       lwd = c(2, 2, 2, 2, 1, 1, 1), lty = c(1, 2, 1, 1, 1, 1, 1), cex = 0.8, bg = "white")


y_limits_zoom <- range(c(wealth_eq, wealth_bh, wealth_gmvp, wealth_nsgmvp, 
                         wealth_mvp, wealth_nsmvp), na.rm = TRUE)

plot(x = oos_dates, y = wealth_eq, type = "l", col = "#0D3B66", lwd = 2,
     ylim = y_limits_zoom, xlab = "Date", ylab = "Cumulative Wealth ($)",
     main = "Zoom-In: Cumulative Wealth of Realistic Strategies",
     cex.axis = 1.5, cex.lab = 1.2, cex.main = 1.2)

lines(x = oos_dates, y = wealth_bh, col = "#D4AC0D", lwd = 2, lty = 2)     
lines(x = oos_dates, y = wealth_nsgmvp, col = "#41521F", lwd = 2)          
lines(x = oos_dates, y = wealth_nsmvp, col = "#9A2735", lwd = 2)         
lines(x = oos_dates, y = wealth_gmvp, col = "#0B0014", lwd = 1)             
lines(x = oos_dates, y = wealth_mvp, col = "#A675A1", lwd = 1)        

abline(h = 1, col = "black", lty = 3)

legend("topleft", 
       legend = c("1/N (Equal)", "B&H", "NS-GMVP", "NS-MVP(10%)", "GMVP (Short)", "MVP (Short)"),
       col = c("#0D3B66", "#D4AC0D", "#41521F", "#9A2735", "#0B0014", "#A675A1"),
       lwd = c(2, 2, 2, 2, 1, 1), lty = c(1, 2, 1, 1, 1, 1), cex = 0.8, bg = "white")


rangex_port_ret <- range(oos_returns, na.rm = TRUE)

plot(x = oos_dates, y = oos_returns$X1.N, type = "l", col = "#0D3B66", ylim = rangex_port_ret,
     xlab = "Date", ylab = "OoS Portfolio Returns", 
     main = "Out-of-Sample Portfolio Daily Returns of 7 Strategies",
     cex.axis = 1.5, cex.lab = 1.2, cex.main = 1.2)

lines(x = oos_dates, y = oos_returns$B.H, col = "#D4AC0D", lty = 2)
lines(x = oos_dates, y = oos_returns$GMVP, col = "#0B0014", lwd = 0.8)
lines(x = oos_dates, y = oos_returns$NS.GMVP, col = "#41521F", lwd = 1)
lines(x = oos_dates, y = oos_returns$MVP.10, col = "#A675A1", lwd = 0.8)
lines(x = oos_dates, y = oos_returns$NS.MVP.10, col = "#9A2735", lwd = 1)
lines(x = oos_dates, y = oos_returns$Tangency, col = "#F194B4", lwd = 0.8)

abline(h = 0, col = "black", lty = 3) 