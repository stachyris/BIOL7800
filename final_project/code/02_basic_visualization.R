
library(sf)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(cowplot)
library(rnaturalearth)
# load cleaned data as df
df <- read.csv("data/cleaned_vertnet.csv")

ggplot(na.omit(df), aes(x = as.factor(YearBin))) +
  geom_bar(fill = "#11A797") +
  labs(x = "Year of Collection", y = "No of Specimens") +
  ggtitle("Collection distribution across years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

count_data <- df %>%
  group_by(countrycode) %>%
  summarise(Count = n())

df$decimallatitude <- as.numeric(df$decimallatitude)
df$decimallongitude <- as.numeric(df$decimallongitude)

world <- ne_countries(scale = 110, returnclass = "sf", type = "countries")

merged_map_data <- merge(world, count_data, by.x = "iso_a3", by.y = "countrycode", all.x = TRUE)

p2 <- ggplot() +
  geom_sf(data = merged_map_data, color = "black", fill = "lightgray") +
  geom_sf(data = merged_map_data, aes(fill = Count), color = NA) +
  scale_fill_gradient(low = "#F6EEAB", high = "#FF2400", na.value = "lightgray", guide = "legend")+
  theme_minimal()


order_counts <- df %>%
  group_by(order) %>%
  summarise(count = n())

family_counts <- df %>%
  group_by(family) %>%
  summarise(count = n())





