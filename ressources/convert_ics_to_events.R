library(tidyverse)
library(calendar)
library(glue)

Sys.setlocale("LC_ALL","fr_FR.UTF-8")

cal_path <- "https://calendar.google.com/calendar/ical/973edf391577058452c3bbdf06f87aa436b286c0b0c386320820396cba55872a%40group.calendar.google.com/public/basic.ics"
raw_calendar <- calendar::ic_read(cal_path)

maquette <- readr::read_csv("Maquette_Agenda.csv")


# DEMI-JOURNEES DEFINIES
library(fuzzyjoin)
library(stringr)

calendar_creneaux <- raw_calendar %>%
  filter(!is.na(DTSTART)) %>%
  fuzzy_left_join(y = maquette, by = c("SUMMARY" = "shortName"), match_fun = function(x,y){str_detect(x, y)}) %>%
  mutate(shortName = case_when(
    is.na(shortName) ~ SUMMARY,
    .default = shortName
  )) %>%
  mutate(color = case_when(
    str_ends(string = SUMMARY, pattern = "\\?") ~ "#f34d4d",
    LOCATION == "IG 413" ~ "#cfe2f3",
    LOCATION == "GeoData Paris" ~ "#d9ead3",
    .default = "#00ff00"
  )) %>%
  mutate(start = format_ISO8601(DTSTART), end = format_ISO8601(DTEND)) %>%
  select(UID, start, end, SUMMARY, SEMESTRE, UE, CODE, Matiere, shortName, Enseignant.e, LOCATION, color)

# JOURNEES PAS A DEFINIR
calendar_vacances <- raw_calendar %>%
  filter(is.na(DTSTART)) %>%
  filter(LOCATION == "AUTRE")

calendar_cours <- raw_calendar %>%
  filter(is.na(DTSTART)) %>%
  filter(LOCATION %in% c("GeoData Paris", "OdG")) %>%
  fuzzy_left_join(y = maquette, by = c("SUMMARY" = "shortName"), match_fun = function(x,y){str_detect(x, y)}) %>%
  mutate(shortName = case_when(
    is.na(shortName) ~ SUMMARY,
    .default = shortName
  ))

calendar_nocut <- calendar_vacances %>%
  bind_rows(calendar_cours) %>%
  mutate(shortName = case_when(
    is.na(shortName) ~ SUMMARY,
    .default = shortName
  )) %>%
  mutate(color = case_when(
    str_ends(string = SUMMARY, pattern = "\\?") ~ "#f34d4d",
    LOCATION == "IG 413" ~ "#cfe2f3",
    LOCATION == "GeoData Paris" ~ "#d9ead3",
    LOCATION == "AUTRE" ~ "#E3E3E3",
    .default = "#00ff00"
  )) %>%
  mutate(start = format_ISO8601(`DTSTART;VALUE=DATE`), end = format_ISO8601(`DTEND;VALUE=DATE`)) %>%
  select(UID, start, end, SUMMARY, SEMESTRE, UE, CODE, Matiere, shortName, Enseignant.e, LOCATION, color)

# JOURNEES A DECOUPER

calendar_a_decouper <- raw_calendar %>%
  filter(is.na(DTSTART)) %>%
  filter(LOCATION %in% c("IG 413")) %>%
  fuzzy_left_join(y = maquette, by = c("SUMMARY" = "shortName"), match_fun = function(x,y){str_detect(x, y)}) %>%
  mutate(shortName = case_when(
    is.na(shortName) ~ SUMMARY,
    .default = shortName
  )) %>%
  mutate(color = case_when(
    str_ends(string = SUMMARY, pattern = "\\?") ~ "#f34d4d",
    LOCATION == "IG 413" ~ "#cfe2f3",
    LOCATION == "GeoData Paris" ~ "#d9ead3",
    LOCATION == "AUTRE" ~ "#E3E3E3",
    .default = "#00ff00"
  )) %>%
  mutate(start = format_ISO8601(`DTSTART;VALUE=DATE`), end = format_ISO8601(`DTEND;VALUE=DATE`)) %>%
  select(UID, start, end, SUMMARY, SEMESTRE, UE, CODE, Matiere, shortName, Enseignant.e, LOCATION, color)


# TOUT ENSEMBLE

calendar_complet <- calendar_creneaux %>%
  bind_rows(calendar_nocut) %>%
  bind_rows(calendar_a_decouper) %>%
  rename(title = shortName) %>%
  mutate(allDay = case_when(
    hour(as_datetime(start)) == 9 & hour(as_datetime(end)) == 16 ~ TRUE,
     hour(as_datetime(start)) == 0 ~ TRUE,
    .default = FALSE
  ))

library(jsonlite)
jsonlite::write_json(calendar_complet, "planning_2026-2027.json", pretty = TRUE)


print({
  glue("JSON recréée : {nrow(calendar_complet)} événements")
})



# ###### EXPORT POUR RESERVATION SALLE 413 ######

# # JOURNEES A DECOUPER

# calendar_a_decouper <- raw_calendar %>%
#   filter(is.na(DTSTART)) %>%
#   filter(LOCATION == "IG 413") %>%
#   fuzzy_left_join(y = maquette, by = c("SUMMARY" = "shortName"), match_fun = function(x,y){str_detect(x, y)}) %>%
#    mutate(
#     DTSTART = as_datetime(`DTSTART;VALUE=DATE`, tz = "Europe/Paris") + (9.5*3600) # = 9.5 heures
#   ) %>%
#   mutate(DTEND = as_datetime(`DTEND;VALUE=DATE`, tz = "Europe/Paris") + (16.5*3600) # = 16.5 heures
#   ) %>%
#   mutate(nb_jours =  floor(as.numeric(difftime(DTEND, DTSTART), units = "days"))) %>%
#   uncount(nb_jours) %>%
#   group_by(SUMMARY, DTSTART, DTEND, LOCATION) %>%
#   mutate(seq_number = row_number()) %>%
#   ungroup() %>%
#   mutate(DTSTART = case_when(
#     seq_number == 1 ~ DTSTART,
#     .default = DTSTART + ((seq_number - 1)*3600*24)
#   )) %>%
#   mutate(DTEND = DTSTART + 7*3600) %>%
#   mutate(color = case_when(
#     str_ends(string = SUMMARY, pattern = "\\?") ~ "#f34d4d",
#     LOCATION == "IG 413" ~ "#cfe2f3",
#     LOCATION == "GeoData Paris" ~ "#d9ead3",
#     .default = "#00ff00"
#   )) %>%
#   mutate(start = format_ISO8601(DTSTART), end = format_ISO8601(DTEND)) %>%
#   select(UID, start, end, SUMMARY, SEMESTRE, UE, CODE, Matiere, shortName, Enseignant.e, LOCATION, color)

# # Planning 413 :
# calendar_creneaux %>%
#   bind_rows(calendar_a_decouper) %>%
#   filter(LOCATION == "IG 413") %>%
#   mutate(Matiere = case_when(
#     is.na(Matiere) ~ SUMMARY,
#     .default = Matiere
#   )) %>%
#   mutate(dt_start = as_datetime(start)) %>%
#   mutate(dt_end = as_datetime(end)) %>%
#   mutate(jour = as_date(dt_start)) %>%
#   mutate(debut = glue("{hour(dt_start)}h{minute(dt_start)}")) %>%
#   mutate(fin = glue("{hour(dt_end)}h{minute(dt_end)}")) %>%
#   arrange(dt_start, dt_end) %>%
#   select(jour, debut, fin, Matiere, UE, CODE, `Enseignant.e`) %>%
#   write_csv("planning_413_Carthageo.csv")
