# library(dplyr)
# library(stringr)
# library(tidyr)
library(tidyverse)

cols <- paste0('foo', '_', c(1:2, '3a', '3b')) %>%
  lapply(\(i) paste0(i, '_', LETTERS[1:6])) %>%
  unlist()

set.seed(1)
df <- lapply(cols, \(i) i = sample(0:1, 5, replace = TRUE)) %>%
  setNames(cols) %>%
  data.frame()

# df <- df %>% mutate(across(
#   starts_with('foo') & ends_with('A'),
#   ~ recode(., `1` = 'A', .default = NA_character_)
# ))

dft <- df %>%
  mutate(rn = row_number()) %>%
  pivot_longer(
    cols = !rn,
    names_to = c(".value", "grp"),
    names_pattern = "^(.*_.*)_(.*)"
  ) %>%
  group_by(rn) %>%
  summarise(across(-grp, ~ str_c(grp[as.logical(.)],
                                 collapse = "")), .groups = 'drop') %>%
  select(-rn)

dft <- df %>%
  mutate(rn = row_number()) %>%
  pivot_longer(
    cols = !rn,
    names_to = c(".value", "grp"),
    names_pattern = "^(.*_.*)_(.*)"
  ) %>%
  group_by(rn) %>%
  summarise(across(-grp, ~ str_c(grp[as.logical(.)], collapse = ""))) %>%
  group_vars()

library(purrr)
df %>%
  summarise(across(everything(),
                   ~ case_when(as.logical(.)
                   ~ rep(str_remove(cur_column(), ".*_.*_"), n()
                               )))) %>%
  split.default(str_remove(names(.), "_[^_]+$")) %>%
  map_dfc(~ .x %>%
            unite(new, everything(), na.rm = TRUE, sep = "") %>%
            pull(new)) 

# Simplest case where column names are character data
relig_income
relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count") %>% data.frame()

# Multiple observations per row
anscombe
test %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )
