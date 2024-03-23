library(dplyr)
library(ggplot2) 
library(scales) 
library(glue)
library(plotly) 
library(lubridate) 
library(gtools)
library(leaflet)
library(readr)



Billionaires <- read_csv("C:/Users/pc/Desktop/Billionaires.csv")
View(Billionaires)  



orneklem <- sample(1:nrow(Billionaires),1000)
veri <- Billionaires[orneklem,]
write.csv(veri,'C:/Users/pc/Desktop/veri.csv',row.names = FALSE)
View(veri)          
 






      
cols_names <- colnames(veri)
cols_NA_values <- colSums(is.na(veri))
temp_cols <- data.frame(cols_names, cols_NA_values)
cols_NA <- temp_cols[!(cols_NA_values == 0),]
cols_NA







##top15 grafik daðýlým
top15_industries <- veri %>% 
  group_by(industries) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  head(15) %>% 
  # Adding label for tooltip
  mutate(label = glue(
    "Industry: {industries}
    Total: {comma(freq)} Billionaires"
  ))
plot1 <- ggplot(data = top15_industries, 
                aes(x = freq,
                    y = reorder(industries, freq),
                    text = label)) +
  geom_col(mapping = aes(fill = freq)) +
  scale_fill_gradient(low = "red", 
                      high = "darkred") +
  scale_x_continuous(labels = comma) + 
  labs(title = "Top 15 Industries with Most Billionaires",
       x = "Total Billionaires",
       y = "Industry Category") +
  theme_minimal() 

# Creating interactive plot
ggplotly(plot1, tooltip = "text")











##top15 ülke grafik daðýlýmý
top15_country <- veri %>% 
  group_by(country) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  head(15) %>% 
  # Adding label for tooltip
  mutate(label = glue(
    "Residence Country: {country}
    Total: {comma(freq)} Billionaires"
  ))
plot2 <- ggplot(data = top15_country, 
                aes(x = freq,
                    y = reorder(country, freq),
                    color = freq,
                    text = label)) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0,
                   xend = freq,
                   yend = country),
               linewidth = 4.0) +
  scale_color_gradient(low = "orange", 
                       high = "darkorange") +
  scale_x_continuous(labels = comma) + 
  labs(title = "Top 15 Residences Countries with Most Billionaires",
       x = "Total Billionaires",
       y = "Residence Country") +
  theme_minimal() +
  theme(legend.position = "none") 

# Creating interactive plot
ggplotly(plot2, tooltip = "text")











##PASTA GRAFÝÐÝ OLUÞTURMA

regions <- veri %>% 
  group_by(residenceStateRegion) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  # Adding label for tooltip
   mutate(label = glue(
    "regions: {residenceStateRegion}
    Total: {comma(freq)} Billionaires"
))
regionsa <- c(2,92,83,69,31)
etiketler <- c("NA","West", "South", "Northeast", "Midwest")
toplam <- sum(regionsa)
yuzdeler <- (regionsa / toplam) * 100
etiketler <- paste(etiketler, "\n", round(yuzdeler, 1), "%\n", regionsa, "kiþi", sep = "")
pasta_grafigi <- pie(regionsa, labels = etiketler, main = "Pasta Grafiði", col = rainbow(length(veri)))
legend("topright", etiketler, fill = rainbow(length(veri)), cex = 1)












##harita üzerinde gösterim

## Filtreleme
total_billionaires_by_country <-veri %>%
  group_by(country, longitude_country, latitude_country) %>%
  mutate(
    total_billionaire_count = sum(!is.na(finalWorth)),
    total_population = sum(!is.na(population_country)),
    total_gdp = sum(!is.na(gdp_country))
  ) %>%
  distinct(country, longitude_country, latitude_country, .keep_all = TRUE)

## Harita oluþturma
global_billionaires_map <- leaflet(data = total_billionaires_by_country) %>%
  setView(lng = 87.5, lat = 34.5, zoom = 1)  ### Adjust the central coordinates and zoom level


global_billionaires_map <- global_billionaires_map %>%
  addTiles()


global_billionaires_map <- global_billionaires_map %>%
  addCircleMarkers(
    lng = ~longitude_country,   ### 'longitude_country' contains longitude information
    lat = ~latitude_country,    ### 'latitude_country' contains latitude information
    radius = ~sqrt(total_billionaire_count) * 1.5,  ### Adjust the radius based on the billionaire count
    color = "darkorange",
    fillOpacity = 1,
    popup = ~paste(
      "Country: ", country,
      "<br>Billionaire Count: ", total_billionaire_count,
      "<br>Population: ", total_population,
      "<br>GDP: ", total_gdp
    ),
    label = ~paste(country, ", ", total_billionaire_count, " billionaires, Pop: ", population_country, ", GDP: ", gdp_country)
  )

global_billionaires_map













# Temel istatistiksel özetleme
summary(veri$age)

# Yaþlarýn histogramý
hist(veri$age, main = "Yaþ Daðýlýmý", xlab = "Yaþ", ylab = "Frekans", col = "orange")


# Yaþlarý belirli aralýklara gruplandýrarak tablo oluþtur
yas_gruplar <- cut(veri$age, breaks = c(20, 40,60,80,100), labels = c("20-40", "40-60", "60-80","80-1000"))
yas_tablo <- table(yas_gruplar)
print(yas_tablo)











# Cinsiyetlere göre gruplandýrarak istatistiksel özetleme
summary(veri$age)
summary(veri$gender)

erkek_sayisi <- sum(veri$gender == "M")
kadin_sayisi <- sum(veri$gender == "F")

# Sonuçlarý ekrana yazdýr
cat("Erkek sayýsý:", erkek_sayisi, "\n")
cat("Kadýn sayýsý:", kadin_sayisi, "\n")



# Cinsiyetlere göre yaþlarýn kutu grafiði
boxplot(age ~ gender, data = veri, main = "Cinsiyetlere Göre Yaþ Daðýlýmý", xlab = "Cinsiyet", ylab = "Yaþ", col = c("skyblue", "lightpink"))











##doðum yeri analizi
top15_country_ship <- veri %>% 
  group_by(countryOfCitizenship) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  head(15) %>% 
  # Adding label for tooltip
  mutate(label = glue(
    "Place of birth: {countryOfCitizenship}
    Total: {comma(freq)} Billionaires"
  ))
plot3 <- ggplot(data =top15_country_ship , 
                aes(x = freq,
                    y = reorder(countryOfCitizenship, freq),
                    color = freq,
                    text = label)) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0,
                   xend = freq,
                   yend = countryOfCitizenship),
               linewidth = 4.0) +
  scale_color_gradient(low = "orange", 
                       high = "darkorange") +
  scale_x_continuous(labels = comma) + 
  labs(title = "Top 15 Place of birth with Most Billionaires",
       x = "Total Billionaires",
       y = "Place of birth") +
  theme_minimal() +
  theme(legend.position = "none") 

# Creating interactive plot
ggplotly(plot3, tooltip = "text")












##miras pasta grafiði
selfmade <- veri %>% 
  group_by(selfMade) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(-freq) %>% 
  # Adding label for tooltip
  mutate(label = glue(
    "selfMade: {selfMade}
    Total: {comma(freq)} Billionaires"
  ))
selfmades <- c(324,676)
etiketler <- c("Miras","Kendi")
toplam <- sum(selfmades)
yuzdeler <- (selfmades / toplam) * 100
etiketler <- paste(etiketler, "\n", round(yuzdeler, 1), "%\n", selfmades, "kiþi", sep = "")
pasta_grafigi <- pie(selfmades, labels = etiketler, main = "Pasta Grafiði", col = c("orange","purple"))
legend("topright", etiketler, fill = c("orange","purple"), cex = 1)










veri_temiz <- na.omit(veri$cpi_country)

# finalWorth vektörünü temizlenmiþ gdp_country vektörüyle ayný boyuta getir
finalWorth_temiz <- veri$finalWorth[complete.cases(veri$cpi_country)]

# Korelasyon hesapla
correlation <- cor(veri_temiz, finalWorth_temiz)
print(correlation)





veri_temiz1 <- na.omit(veri$tax_revenue_country_country)

# finalWorth vektörünü temizlenmiþ gdp_country vektörüyle ayný boyuta getir
finalWorth_temiz1 <- veri$finalWorth[complete.cases(veri$tax_revenue_country_country)]

# Korelasyon hesapla
correlation <- cor(veri_temiz1, finalWorth_temiz1)
print(correlation)









