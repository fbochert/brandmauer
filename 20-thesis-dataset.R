###################### DATENSATZ ERSTELLEN #############################

# setwd("M:/user/bochert/RA/Daniel/Brandmauer/Analyse")

library(tidyverse)

election <- read_csv("data/Elections.csv") %>%
  mutate_if(is.numeric, ~replace_na(., 0))

election <- election %>%
  mutate(wahl = lubridate::dmy(Datum),
         wahlb = Wahlberechtigte / Waehler,
         P_CDU = CDU / Gueltig,
         P_SPD = SPD / Gueltig,
         P_Linke = Linke / Gueltig,
         P_Gruene = Gruene / Gueltig,
         P_FDP = FDP / Gueltig,
         P_AfD = AfD / Gueltig,
         P_NPD = NPD / Gueltig,
         P_BSW = BSW / Gueltig,
         P_FW = FW / Gueltig,
         wahljahr = year(wahl))

# Creating the outcome variables
subsample <- election %>%
  group_by(Nummer) %>%
  reframe(D_AfD = P_AfD[wahljahr > 2019] - P_AfD[wahljahr < 2020],
          D_CDU = P_CDU[wahljahr > 2019] - P_CDU[wahljahr < 2020]) %>%
  left_join(election, relationship = "many-to-many") %>%
  filter(wahljahr > 2019) %>%
  select(Nummer, D_AfD, D_CDU, wahl, wahljahr)

# Merging with records of each session
sessions <- read_csv("data/Sitzungen.csv") %>%
  # sessions <- read_delim("data/Sitzungen.csv", delim = ";") %>%
  janitor::remove_empty() %>%
  mutate(Datum = lubridate::mdy(Datum))

# Adding strong cooperations
Koop <- read_csv("data/Kooperationen.csv") %>%
  # Koop <- read_delim("data/Kooperationen.csv", delim = ";") %>%
  mutate(Datum = as.Date(Datum, "%d.%m.%Y"))
# full <- read_delim("data/Erste_Ergebnisse.csv", delim = ";") %>%
full <- read_csv("data/Erste_Ergebnisse.csv") %>%
  mutate(Kreis = as.numeric(Nummer)) %>%
  select(Kreis, Insgesamt) %>%
  right_join(., Koop) %>%
  mutate(Stimmen = as.numeric(Stimmen)) %>%
  filter(Stimmen >= (Insgesamt / 10)) %>%
  group_by(Kreis, Datum) %>%
  summarize(vKoop = n()) %>%
  right_join(., sessions, by = c("Kreis", "Datum")) %>%
  mutate(vKoop = ifelse(is.na(vKoop), 0, vKoop),
         sdatum = Datum) %>%
  left_join(subsample, by = c("Kreis" = "Nummer"),
            relationship = "many-to-many") %>%
  filter(!is.na(wahl))

# Adding passed motions
full1 <- read_csv("data/Erste_Ergebnisse.csv") %>%
  mutate(Kreis = as.numeric(Nummer)) %>%
  select(Kreis, Insgesamt) %>%
  right_join(., Koop) %>%
  filter(Beschlossen == 1) %>%
  group_by(Kreis, Datum) %>%
  summarize(pKoop = n()) %>%
  right_join(., sessions, by = c("Kreis", "Datum")) %>%
  mutate(pKoop = ifelse(is.na(pKoop), 0, pKoop),
         sdatum = Datum) %>%
  left_join(subsample, by = c("Kreis" = "Nummer"),
            relationship = "many-to-many") %>%
  filter(!is.na(wahl)) %>%
  select(Kreis, Datum, pKoop) %>%
  right_join(., full)

# Creating the independent variables
full <- full1 %>%
  filter(sdatum < wahl) %>% # only looking at sessions in lead-up to the election
  mutate(Organisatorisch = as.numeric(Organisatorisch),
         prop = Kooperation / (`AfD-Anträge` + Personell + Organisatorisch), # IV
         vprop = vKoop / (`AfD-Anträge` + Personell + Organisatorisch),
         pprop = pKoop / `AfD-Anträge`,
         tage = as.numeric(wahl - sdatum),
         laenge = as.numeric(str_replace_all(`Länge (h)`, ",", "."))) %>%
  group_by(Kreis, D_AfD, D_CDU) %>%
  summarize(prop = mean(prop, na.rm = TRUE),
            vprop = mean(vprop, na.rm = TRUE),
            pprop = mean(pprop, na.rm = TRUE),
            laenge = mean(laenge, na.rm = TRUE)) %>%
  replace(is.na(.), 0)

# Adding control variables
# full <- read_delim("data/Erste_Ergebnisse.csv", delim = ";") %>%
full <- read_csv("data/Erste_Ergebnisse.csv") %>%
  rename(flaeche = `Flaeche (km2)`,
         bev = Bevoelkerung,
         dichte = Dichte,
         migr = Migration,
         bip = BIP,
         arb = Arbeitslos,
         sitz = Sitzungen,
         groesse = Insgesamt) %>%
  mutate(Kreis = as.numeric(Nummer),
         sitz = as.numeric(sitz),
         aktiv = `AfD-Anträge` / sitz,
         cdu = CDU_CSU / groesse) %>%
  select(Kreis, sitz, groesse, aktiv, cdu, flaeche, bev, dichte, migr, bip, arb) %>%
  right_join(full)

match <- full %>%
  mutate(prop_bin = ifelse(prop == 0, 0, 1),
         vprop_bin = ifelse(vprop == 0, 0, 1),
         pprop_bin = ifelse(pprop == 0, 0, 1))

write_csv(match, "data/match.csv")
