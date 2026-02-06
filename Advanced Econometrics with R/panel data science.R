library(plm)
library(quantmod)
library(ggplot2)
library(dplyr)

####Import the file####

panel_data = read.csv("C:/Users/ASUS/OneDrive/Desktop/סמסטר ב' שנה ב'/אקונומטריקה מתקדמת עם R/עבודה סופית/panel_data.csv")
View(head(panel_data,30))

summary(panel_data)

#### organize the data####
panel_data <- subset(panel_data, Year != 2023)

sum(is.na(panel_data)) #no NA 

find_outliers <- function(data) {
  outliers <- lapply(data, function(col) {
    if(!is.numeric(col)) return(NA) #מדלג על עמודות לא מספריות 
    
    Q <- quantile(col, probs = c(0.25, 0.75), na.rm = TRUE)
    IQR <- Q[2] - Q[1]
    lower_bound <- Q[1] - 1.5 * IQR
    upper_bound <- Q[2] + 1.5 * IQR
    
    
    outliers <- col[col < lower_bound | col > upper_bound]
    if(length(outliers) == 0) {
      return(NA)
    }
    else {
      return(outliers)
    }
  })
  
  names(outliers) <- names(data)
  return(outliers)
}

outliers<-find_outliers(panel_data)
summary(outliers)

#There is outliers in variable GDP and Unemployment

#outlier GDP Boxplot
ggplot(panel_data, aes(y = GDP)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot של התמ\"ג (GDP)",
       y = "תמ\"ג לנפש") +
  theme_minimal()
#outlier Uneployment Boxplot

ggplot(panel_data, aes(y = Unemployment)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot של שיעור האבטלה",
       y = "שיעור אבטלה (%)") +
  theme_minimal()

boxplot.stats(panel_data$GDP)$out
boxplot.stats(panel_data$Unemployment)$out

####X-Unemployment####

#Average unemployment rate over the years

panel_data %>%
  group_by(Year) %>%
  summarise(Avg_Unemployment = mean(Unemployment, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_Unemployment)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "ממוצע שיעור אבטלה לאורך השנים",
       x = "שנה", y = "ממוצע אבטלה (%)") +
  theme_minimal()

# Unemployment trends by selected countries
selected_countries <- c("Israel", "Spain", "China")

panel_data %>%
  filter(Country.Name %in% selected_countries) %>%
  ggplot(aes(x = Year, y = Unemployment, color = Country.Name)) +
  geom_line(size = 1) +
  labs(title = "שיעור אבטלה במדינות נבחרות לאורך זמן",
       x = "שנה", y = "אבטלה (%)", color = "מדינה") +
  theme_minimal()

# Heatmap של אבטלה לפי מדינה ושנה

ggplot(panel_data, aes(x = Year, y = Country.Name, fill = Unemployment)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "של שיעור אבטלה לפי מדינה ושנה Heatmap",
       x = "שנה", y = "מדינה", fill = "אבטלה (%)") +
  theme_minimal()

#  שינוי של שיעור אבטלה מול צמיחה כלכלית (GDP)
ggplot(panel_data, aes(x = GDP, y = Unemployment)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "loess", color = "black") +
  labs(title = "קשר בין תוצר לנפש לאבטלה",
       x = "תוצר לנפש ",
       y = "שיעור אבטלה (%)") +
  theme_minimal()


summary(panel_data$Unemployment)

####X-Internet_Users####

# ממוצע שימוש באינטרנט לאורך השנים (כל המדינות)
panel_data %>%
  group_by(Year) %>%
  summarise(Avg_Internet = mean(Internet_Users, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_Internet)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "ממוצע שימוש באינטרנט לאורך השנים",
       x = "שנה", y = "משתמשי אינטרנט (% מהאוכלוסייה)") +
  theme_minimal()

#קשר בין שימוש משתמשי האינטרנט לתמ"ג
ggplot(panel_data, aes(x = Internet_Users, y = GDP, color = Year)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(option = "D") +
  labs(title = "קשר בין שיעור משתמשי אינטרנט לתמ\"ג",
       x = "משתמשי אינטרנט (% מהאוכלוסייה)",
       y = "תמ\"ג לנפש",
       color = "שנה") +
  theme_minimal()

#heatmap internet users over the years

ggplot(panel_data, aes(x = Year, y = Country.Name, fill = Internet_Users)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", direction = 1) +
  labs(title = "Heatmap של שיעור משתמשי אינטרנט לפי מדינה ושנה",
       x = "שנה", y = "מדינה", fill = "אינטרנט (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

summary(panel_data$Internet_Users)

####X-School_Enrollment####

summary(panel_data$School_Enrollment)
class(panel_data$School_Enrollment)

#שיעור ההרשמה להשכלה גבוהה לאורך השנים
panel_data %>%
  group_by(Year) %>%
  summarise(Avg_School_Enrollment = mean(School_Enrollment, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_School_Enrollment)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "ממוצע שיעור ההרשמה להשכלה גבוהה לאורך השנים",
       x = "שנה", y = "ממוצע ההרשמה (%)") +
  theme_minimal()

#שינוי שיעור ההרשמה להשכלה גבוהה אל מול צמיחה כלכלית (GDP)
ggplot(panel_data, aes(x = GDP, y = School_Enrollment)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "loess", color = "black") +
  labs(title = "קשר בין תוצר לנפש לשיעור הרישום להשכלה גבוהה",
       x = "תוצר לנפש ",
       y = "שיעור הרישום (%)") +
  theme_minimal()


####Y-GDP####

summary(panel_data$GDP)

# Heatmap של התמ"ג לפי מדינה ושנה

ggplot(panel_data, aes(x = Year, y = Country.Name, fill = GDP)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", direction = 1) +
  labs(title = "Heatmap של התמ\"ג (GDP) לפי מדינה ושנה",
       x = "שנה", y = "מדינה", fill = "GDP") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# ממוצע תמ"ג עולמי לאורך השנים

panel_data %>%
  group_by(Year) %>%
  summarise(Avg_GDP = mean(GDP, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_GDP)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "ממוצע התמ\"ג העולמי לאורך השנים",
       x = "שנה", y = "GDP") +
  theme_minimal()

#השוואת מגמות GDP במדינות נבחרות לאורך זמן

selected_countries <- c("Israel","China", "Albania")

panel_data %>%
  filter(Country.Name %in% selected_countries) %>%
  ggplot(aes(x = Year, y = GDP, color = Country.Name)) +
  geom_line(size = 1) +
  labs(title = "מגמות התמ\"ג במדינות נבחרות לאורך זמן",
       x = "שנה", y = "GDP", color = "מדינה") +
  theme_minimal()

#  שני קווים באותו גרף לכל מדינה – עם התאמת סולמות (לציר משותף)

selected_countries <- c("Qatar", "Ireland", "Macao SAR, China")

panel_data %>%
  filter(Country.Name %in% selected_countries) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Unemployment, color = "Unemployment"), size = 1) +
  geom_line(aes(y = GDP / 1000, color = "GDP (scaled)"), size = 1) +  # התאמת סולם
  facet_wrap(~Country.Name, scales = "free_y") +
  labs(title = "השוואת שיעור אבטלה ו-GDP לאורך השנים",
       x = "שנה", y = "ערך מתואם", color = "משתנה") +
  theme_minimal()

# איחוד השנים והמדינות 
new_data = pdata.frame(panel_data, index = c("Country.Name", "Year"))
View(head(new_data,100))

####הרצת המודלים ####
fixed_effects_model = plm( GDP ~ School_Enrollment + Internet_Users + Unemployment, data = new_data, model = "within")
summary(fixed_effects_model)
#יש משתנה מסביר אחד שאינו מובהק - School_Enrollment

fixed_effects_model2 = plm( GDP ~ Internet_Users + Unemployment, data = new_data, model = "within")
summary(fixed_effects_model2)
#ללא המשתנה הלא מובהק

fd_model = plm(GDP ~ School_Enrollment + Internet_Users + Unemployment, data = new_data, model = "fd")
summary(fd_model)
#ישנם 2 משתנים שאינם מובהקים במודל זה- School_Enrollment + Internet_Users

fd_model2 = plm(GDP ~ Unemployment, data = new_data, model = "fd") #הסרת משתנים שאינם מובהקים
summary(fd_model2)

unique(panel_data$Country.Name)
