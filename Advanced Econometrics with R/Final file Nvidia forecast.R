# Load necessary libraries
library(tseries)
library(ggplot2)
library(scales)

####Import Data####
NVIDIA= read.csv("C:/Users/ASUS/OneDrive/Desktop/סמסטר ב' שנה ב'/אקונומטריקה מתקדמת עם R/עבודה סופית/NVDA_EDITED.csv")
####arrange the data set####
NVIDIA <- NVIDIA[ , !(names(NVIDIA) %in% c("X", "X.1", "X.2")) ]
names(NVIDIA)[1] <- "Date"
NVIDIA$Date <- gsub("\\.", "/", NVIDIA$Date)
NVIDIA$Date <- as.Date(NVIDIA$Date, format = "%d/%m/%Y")
NVIDIA$Date <- format(NVIDIA$Date, "%d/%m/%Y")
NVIDIA= na.omit(NVIDIA)
head(NVIDIA)
NVIDIA$Date <- as.Date(NVIDIA$Date, format = "%d/%m/%Y")

####Statistic information ####
head(NVIDIA)
tail(NVIDIA)
dim(NVIDIA)
summary(NVIDIA)

####Missing values #### 
NA_detect <- function(df) {
  if(sum(is.na(df)) == 0) {
    # If no NA values are found in the entire data frame
    print("No missing values detected in the data.")
  } else {
    # If NA values are found, print the percentage of NA values in each column
    na_summary <- data.frame(
      Column_Name = character(),
      NA_Percentage = numeric()
    )
    for(i in 1:ncol(df)) {
      if(sum(is.na(df[,i])) > 0) {
        # Calculate the percentage of NA values in the column
        na_percentage <- sum(is.na(df[,i])) / nrow(df) * 100
        # Add column name and NA percentage to the summary dataframe
        na_summary <- rbind(na_summary, data.frame(Column_Name = names(df)[i], NA_Percentage = na_percentage))
      }
    }
    # Print NA summary dataframe
    print("Missing values detected in the following columns:")
    print(na_summary)
  }
}

NA_detect(NVIDIA) #Result: no missing values in this data

### Creating optimal arima function #### 

optimal_arma_MER <- function(data, ar_vec, ma_vec, lag_error_test = 10, alpha = 0.05) {
  
  n <- length(data)
  train <- data[1:(n - 1)]
  test <- data[n]  # תחזית לתצפית אחת קדימה
  
  Min_MER <- Inf
  AR_val <- NA
  MA_val <- NA
  best_forecast <- NA
  
  for (i in ar_vec) {
    for (j in ma_vec) {
      try({
        model_ARMA <- arima(train, order = c(i, 0, j))
        forecast_ARMA <- predict(model_ARMA, n.ahead = 1)$pred
        MER <- abs(forecast_ARMA - test) / abs(test)
        
        if (MER < Min_MER) {
          Min_MER <- MER
          AR_val <- i
          MA_val <- j
          best_forecast <- forecast_ARMA
        }
      }, silent = TRUE)
    }
  }
  
  # בניית המודל הסופי עם הנתונים המלאים
  final_model <- arima(data, order = c(AR_val, 0, MA_val))
  
  # בדיקת שאריות בעזרת Ljung-Box
  pval <- Box.test(final_model$residuals, lag = lag_error_test, type = "Ljung-Box")$p.value
  
  if (pval >= alpha) {
    return(list(
      AR = AR_val,
      MA = MA_val,
      MER = Min_MER,
      Forecast = best_forecast,
      Ljung_Box_p = pval
    ))
  } else {
    warning("The residuals are autocorrelated (p < alpha). Try higher lags or another model.")
    return(NULL)
  }
}

###"Close" column###
#Data visibility - we will check if this column is stationary 

ggplot(NVIDIA, aes(x = Date, y = NVDA.Close)) +
  geom_line(colour = "#2C7FB8", linewidth = 1) +                 # קו כחול‑עמוק אלגנטי
  geom_point(size = 0.8, alpha = 0.6, colour = "#2C7FB8") +      # נקודות קטנות, שקופות
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +     # שנתון על ציר ה‑X
  scale_y_continuous(labels = dollar_format(prefix = "$")) +     # $ לפני המחיר
  labs(
    title    = "NVIDIA (NVDA) – Daily Closing Prices",
    subtitle = "2019–2025 (מקור: Yahoo! Finance)",
    x        = NULL,
    y        = "Closing Price",
    caption  = "גרף: את 😉"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 12) +     # מראה נקי
  theme(
    plot.title      = element_text(face = "bold", size = 18),
    plot.subtitle   = element_text(colour = "grey40"),
    plot.caption    = element_text(size = 8),
    panel.grid.minor = element_blank(),                          # בלי רשת משנית
    panel.grid.major.x = element_blank()                         # בלי קווי רשת אנכיים
  )

#ACF and PACF plots on the first difference of the original time series

par(mfrow = c(1, 2))
acf(NVIDIA$NVDA.Close, main = "ACF")
pacf(NVIDIA$NVDA.Close, main = "PACF")
par(mfrow = c(1, 1))

####Stationary test on Close column####
#Augmented Dickey-Fuller (ADF) test for stationary
adf_test = adf.test(NVIDIA$NVDA.Close, alternative = "stationary")
cat("ADF test p-value:", adf_test$p.value, "\n") #result: The prices time series is NOT stationary .

####"Returns" column ####
#Data visibility
ggplot(NVIDIA, aes(x = Date, y = returns)) +
  geom_line(colour = "#2C7FB8", linewidth = 1) +                 # קו כחול‑עמוק אלגנטי
  geom_point(size = 0.8, alpha = 0.6, colour = "#2C7FB8") +      # נקודות קטנות, שקופות
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +     # שנתון על ציר ה‑X
  scale_y_continuous(labels = dollar_format(prefix = "$")) +     # $ לפני המחיר
  labs(
    title    = "NVIDIA (NVDA) – Daily Returns",
    subtitle = "2019–2025 (מקור: Yahoo! Finance)",
    x        = NULL,
    y        = "Returs",
    caption  = "גרף: את 😉"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 12) +     # מראה נקי
  theme(
    plot.title      = element_text(face = "bold", size = 18),
    plot.subtitle   = element_text(colour = "grey40"),
    plot.caption    = element_text(size = 8),
    panel.grid.minor = element_blank(),                          # בלי רשת משנית
    panel.grid.major.x = element_blank()                         # בלי קווי רשת אנכיים
  )

###Stationary test on returns####
######Augmented Dickey-Fuller (ADF) test for stationary####
adf_test = adf.test(NVIDIA$returns, alternative = "stationary")
cat("ADF test p-value:", adf_test$p.value, "\n")  #Result:The time series is stationary/

######ACF and PACF plots on the first difference of the original time series####
par(mfrow = c(1, 2))
acf(NVIDIA$returns, main = "ACF")
pacf(NVIDIA$returns, main = "PACF")
par(mfrow = c(1, 1))

#The plots indicate that the time series is not pure AR or MA


####Optimize arima model####
# Using the optimization function to find out which ARMA best fits the data 
optimal_arma_MER(data = NVIDIA$returns, ar_vec = 0:12, ma_vec = 0:12, lag_error_test = 25, alpha = 0.05) 

#Set up the plotting area  
####Final Model ####
model_ARMA = arima(NVIDIA[,3], order = c(10, 0, 6))
model_ARMA

actual= NVIDIA$returns[1508:1508]
actual

par(mfrow = c(1, 2))
acf(model_ARMA$residuals, main = "ACF")
pacf(model_ARMA$residuals, main = "PACF")
par(mfrow = c(1, 1))

Box.test(model_ARMA$residuals, lag = 10 , type = "Ljung-Box" ) #result: white noise 


####checking MER on different MA and AR to check if the result of our optimal MER is the minimal . 
optimal_arma_MER(data = NVIDIA$returns, ar_vec = 1:5, ma_vec = 1:5, lag_error_test = 15, alpha = 0.05) 

