library(tidyverse)
library(glue)
library(kableExtra)

table_from_df <- function(df){

ue_titles <- df %>% filter(is.na(Cours))
ue_indexes <- df %>%
  group_by(UE) %>%
  mutate(UE = glue("{UE} ({max(ECTS, na.rm = TRUE)} ECTS)")) %>%
  ungroup() %>%
  filter(!is.na(Cours)) %>%
  mutate(row_number = row_number()) %>%
  group_by(Semestre, UE) %>%
  summarise(first = min(row_number),
            last = max(row_number),
            range = (last - first) + 1,
            .groups = "drop")

semestre_infos <- ue_indexes %>% group_by(Semestre) %>%
  summarise(length = max(last)) %>%
  ungroup() %>%
  mutate(title = glue("SEMESTRE {Semestre}")) %>%
  mutate(row_number = row_number()) %>%
  mutate(length = case_when(
    row_number == max(row_number) ~ nrow(maquette_m2 %>% filter(!is.na(Cours))) - lag(n = 1, length),
    .default = length
  ))

kbl(df %>% filter(!is.na(Cours)) %>% select(-Semestre, -UE, -ECTS), col.names = NULL) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows(index = pull(semestre_infos, length, title), label_row_css = "text-align: center;") %>%
  pack_rows(index = ue_indexes %>% pull(range, UE),  label_row_css = "text-align: left;")

}

# table_from_df(maquette_m2)

