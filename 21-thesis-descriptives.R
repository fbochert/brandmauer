# setwd("M:/user/bochert/RA/Daniel/Brandmauer/Analyse")

library(tidyverse)
library(sf)
library(lubridate)

match <- read_csv("data/match.csv")

###################### DESCRIPTIVES #############################
table(match$vprop_bin) # 0: 190; 1: 79

match %>%
  group_by(vprop_bin) %>%
  summarize(mean = mean(D_AfD, na.rm = TRUE))

# Figure 1
p1 <- read_sf("Shapefiles/EPSG_25832/VG250_KRS.shp") %>%
  mutate(AGS = as.numeric(AGS)) %>%
  left_join(., match, by = c("AGS" = "Kreis"), relationship = "many-to-many") %>%
  mutate(vprop_bin = as.logical(vprop_bin)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = vprop_bin)) + 
  theme_minimal() +
  scale_fill_manual(
    name = "",
    values = c("green", "red"),
    na.value = "grey50",
    labels  = c("No Cooperation", "Cooperation", "No Data"),) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
ggsave("tables/descriptive_map.jpg", width = 6, height = 8)

# Figure 2
Koop <- read_csv("data/Kooperationen.csv") %>%
  mutate(Datum = as.Date(Datum, "%d.%m.%Y"),
         Jahr = year(Datum))
fig2_data <- read_csv("data/Erste_Ergebnisse.csv") %>%
  mutate(Kreis = as.numeric(Nummer)) %>%
  select(Kreis, Insgesamt) %>%
  right_join(., Koop) %>%
  mutate(Stimmen = as.numeric(Stimmen)) %>%
  filter(Stimmen >= (Insgesamt / 10)) %>%
  group_by(Kreis) %>%
  summarize(vKoop = n())
f2 <- read_sf("Shapefiles/EPSG_25832/VG250_KRS.shp") %>%
  mutate(AGS = as.numeric(AGS)) %>%
  left_join(., fig2_data, by = c("AGS" = "Kreis")) %>%
  left_join(., match, by = c("AGS" = "Kreis"), relationship = "many-to-many") %>%
  mutate(vKoop = ifelse(is.na(vKoop), 0, vKoop)) %>%
  mutate(vKoop = ifelse(is.na(aktiv), NA, vKoop)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = vKoop)) + 
  theme_minimal() +
  scale_fill_gradient(
    name = "Number of Strong \nCooperations",
    low = "green", 
    high = "red",
    na.value = "grey50") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
ggsave("tables/figure2.jpg", width = 6, height = 8)


# Figure 3
fig3data <- read_csv("data/Erste_Ergebnisse.csv") %>%
  mutate(Kreis = as.numeric(Nummer)) %>%
  select(Kreis, Insgesamt) %>%
  right_join(., Koop) %>%
  mutate(Stimmen = as.numeric(Stimmen)) %>%
  filter(Stimmen >= (Insgesamt / 10)) %>%
  group_by(Kreis, Jahr) %>%
  summarize(vKoop = n()) %>%
  group_by(Kreis) %>%
  mutate(min_Jahr = min(Jahr)) %>%
  dplyr::select(Kreis, min_Jahr) %>%
  distinct()
f3 <- read_sf("Shapefiles/EPSG_25832/VG250_KRS.shp") %>%
  mutate(AGS = as.numeric(AGS)) %>%
  left_join(., fig3data, by = c("AGS" = "Kreis")) %>%
  left_join(., match, by = c("AGS" = "Kreis"), relationship = "many-to-many") %>%
  mutate(min_Jahr = ifelse(is.na(min_Jahr), 0, min_Jahr)) %>%
  mutate(min_Jahr = as.factor(ifelse(is.na(aktiv), NA, min_Jahr))) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = min_Jahr)) + 
  theme_minimal() +
  scale_fill_manual(
    name = "First Year With \nCooperation",
    values = c("green", "#f8bb8f", "#f4914c", "#ec690d", "#ca5a0b", "#a84b09", "#652d05"),
    na.value = "grey50",
    labels  = c("None", "2019", "2020", "2021", "2022", "2023", "2024", "No Data"),) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
ggsave("tables/figure3.jpg", width = 6, height = 8)


# Figure 4
fig4data <- read_csv("data/Erste_Ergebnisse.csv") %>%
  mutate(Kreis = as.numeric(Nummer)) %>%
  select(Kreis, Insgesamt) %>%
  right_join(., Koop) %>%
  mutate(Stimmen = as.numeric(Stimmen)) %>%
  filter(Stimmen >= (Insgesamt / 10)) %>%
  group_by(Kreis, Jahr) %>%
  summarize(vKoop = n()) %>%
  pivot_wider(names_from = "Jahr", values_from = "vKoop") %>%
  mutate(
    `2019` = ifelse(is.na(`2019`), 0, 1),
    `2020` = ifelse(is.na(`2020`), 0, 1),
    `2021` = ifelse(is.na(`2021`), 0, 1),
    `2022` = ifelse(is.na(`2022`), 0, 1),
    `2023` = ifelse(is.na(`2023`), 0, 1),
    `2024` = ifelse(is.na(`2024`), 0, 1),
    sum = `2019` + `2020` + `2021` + `2022` + `2023` + `2024`)
f4 <- read_sf("Shapefiles/EPSG_25832/VG250_KRS.shp") %>%
  mutate(AGS = as.numeric(AGS)) %>%
  left_join(., fig4data, by = c("AGS" = "Kreis")) %>%
  left_join(., match, by = c("AGS" = "Kreis"), relationship = "many-to-many") %>%
  mutate(sum = ifelse(is.na(sum), 0, sum)) %>%
  mutate(sum = as.factor(ifelse(is.na(aktiv), NA, sum))) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = sum)) + 
  theme_minimal() +
  scale_fill_manual(
    name = "Number of Years \nWith Cooperation",
    values = c("green", "#f8bb8f", "#f4914c", "#ec690d", "#ca5a0b", "#a84b09", "#652d05"),
    na.value = "grey50",
    labels  = c("None", "1", "2", "3", "4", "5", "6", "No Data"),) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
ggsave("tables/figure4.jpg", width = 6, height = 8)

