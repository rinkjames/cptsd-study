# cleaning data -----------------------------------------------------------

# remove duplicate rows, keeping earliest duplicate
df1 <- df1 %>%
  group_by(student_no) %>%
  filter(id == min(id)) %>%
  ungroup()

## id, lec-5, ctq, socio var cleaning -------------------------------------

# date vars
df1 <- df1 %>%
  mutate(date_submit = date_submit %>% as.Date()) %>%
  relocate(date_start, .before = date_submit)

# uni var
df1 <- df1 %>% mutate(
  uni = ifelse(is.na(uni) == TRUE, 1, uni),
  uni = ifelse(id == 1890, 2, uni),
  uni = uni %>% factor(labels = c("uct", "wits", "ru"))
)

# name and email vars
df1 <- df1 %>%
  mutate(across(name_first:name_last, ~ str_to_title(.x))) %>%
  mutate(email = str_to_lower(email))

# age, sex, socio vars
df1 <- df1 %>%
  mutate(sex = factor(sex, labels = c("male", "female", "intersex"))) %>%
  mutate(across(dx_psych:comm_pref, ~ factor(.x, labels = c("no", "yes")))) %>%
  relocate(c(age:comm_pref), .after = email)

# lec-5 vars
df1 <- df1 %>%
  select(lec_1_A:lec_17b_F) %>%
  nominal_multi("^(lec_\\d+[a-b]?)_([A-F])$") %>%
  mutate(lec_17 = if_else(lec_17a == "", lec_17b, lec_17a)) %>%
  select(-c(lec_17a:lec_17b)) %>%
  bind_cols(df1) %>%
  select(-c(lec_1_A:lec_17b_F)) %>%
  relocate(lec_1:lec_17, .after = comm_pref)

# ctq vars
df1 <- df1 %>% mutate(across(ctq_1:ctq_28, ~ .x + 1))

# income vars
df1 <- df1 %>% mutate(income_source = income_source %>%
                        factor(labels = c("family", "independent")))

df1 <- df1 %>%
  mutate(
    hh_income = case_when(
      income_source == "family" ~ income_parents,
      income_source == "independent" ~ income_indiv
    )
  ) %>%
  mutate(
    hh_members = case_when(
      income_source == "family" ~ income_parents_members,
      income_source == "independent" ~ income_indiv_members
    )
  ) %>%
  select(-c(income_indiv:income_parents_members))

### pcl-5 criterion-a vars ------------------------------------------------

# # export crita_time to score manually
# write_csv(df1 %>%  select(id, crita_time_1),
#           here("output", paste(
#             format(Sys.time(), "%Y.%m.%d"),
#             "crita_time.csv",
#             sep = "-"
#           )))

# time index for crita traumatic event (months)
df1 <-
  # import manually scored data
  here("data", "2024.01.14-crita_time-manual_scoring.csv") %>%
  read_csv(col_select = c(id, manual), show_col_types = F) %>%
  inner_join(df1, by = "id") %>%
  mutate(crita_time_1 = manual %>% num(digits = 2)) %>%
  select(!manual) %>%
  arrange(id) %>%
  mutate(
    crita_time_1 = case_when(
      crita_time_1 < 1 ~ 1,
      crita_time_1 >= 1 & crita_time_1 <= 3 ~ 2,
      crita_time_1 > 3 & crita_time_1 <= 6 ~ 3,
      crita_time_1 > 6 & crita_time_1 <= 8 ~ 4,
      crita_time_1 > 8 & crita_time_1 <= 12 ~ 5,
      crita_time_1 > 12 & crita_time_1 <= 24 ~ 6,
      crita_time_1 > 24 & crita_time_1 <= 60 ~ 7,
      crita_time_1 > 60 & crita_time_1 <= 120 ~ 8,
      crita_time_1 > 120 ~ 9
    )
  ) %>%
  mutate(crita_time = ifelse(id <= 573, crita_time_1, crita_time_2),
         .before = crita_time_1) %>%
  mutate(crita_time = crita_time %>% factor(
    labels = c(
      "< 1 month",
      "1-3 months",
      "3-6 months",
      "6-8 months",
      "8-12 months",
      "1-2 years",
      "2-5 years",
      "5-10 years",
      "> 10 years"
    )
  )) %>%
  select(-crita_time_1, -crita_time_2)

# crita experience, life_in_danger, and injury_death vars
df1 <- df1 %>%
  select(crita_exp_A:crita_injury_death_C & -crita_exp_E_desc) %>%
  nominal_multi("^(crita_.*)_([A-E])$") %>%
  bind_cols(df1) %>%
  rename(crita_exp_other = crita_exp_E_desc) %>%
  select(!c(crita_exp_A:crita_injury_death_C &
              -crita_exp_other)) %>%
  relocate(crita_exp, .before = crita_exp_other) %>%
  relocate(crita_life_in_danger:crita_injury_death,
           .before = crita_sex_violence)

# sexual violence, accidental/natural vars
df1 <- df1 %>%
  mutate(crita_sex_violence = crita_sex_violence %>%
           factor(labels = c("no", "yes"))) %>%
  mutate(crita_accident_natural = crita_accident_natural %>%
           factor(labels = c(
             "accident/violence", "natural causes", "n/a"
           )))

# # export crita_exposure to score manually
# write_csv(df1 %>%  select(id, crita_exposure, crita_exposure_more),
#           here("output", paste(
#             format(Sys.time(), "%Y.%m.%d"),
#             "crita_exposure.csv",
#             sep = "-"
#           )))

# exposure var
df1 <-
  # import manually scored data
  here("data", "2024.01.14-crita_exposure-manual_scoring.csv") %>%
  read_csv(col_select = c(id, manual), show_col_types = F) %>%
  inner_join(df1, by = "id") %>%
  arrange(id) %>%
  mutate(crita_exposure_more = as.numeric(crita_exposure_more) %>%
           suppressWarnings()) %>%
  mutate(
    crita_exposure = case_when(
      crita_exposure != 2 ~ crita_exposure,
      crita_exposure == 2 & id <= 573 ~ manual,
      crita_exposure == 2 & id > 573 ~ crita_exposure_more
    )
  ) %>%
  select(-crita_exposure_more, -manual)

### scid-5 spq vars -------------------------------------------------------

# populating missing items in first round of bpd likert vars (likert-a) from
# items in second round of bpd likert vars (likert-b)
df1 <- paste0("bpd_lika_", 1:15) %>%
  imap_dfc( ~ df1 %>% transmute(
    !!.x := case_when(id >= 664 ~ !!sym(paste0(
      "bpd_likb_", .y, "_1"
    )) %>%
      as.numeric(),
    id < 664 ~ !!sym(.x))
  )) %>% bind_cols(df1 %>% select(!paste0("bpd_lika_", 1:15))) %>%
  relocate(bpd_lika_1:bpd_lika_15, .after = bpd_bin_15)

# bpd likert-b vars
df1 <- df1 %>% select(-paste0("bpd_likb_", c(1, 3:7, 9:15), "_eg"))

df1 <- df1 %>% select(bpd_likb_7_2_A:bpd_likb_7_2_F) %>%
  nominal_multi("^(bpd_likb_7_2.*)_([A-F])$") %>%
  bind_cols(df1) %>%
  select(-c(bpd_likb_7_2_A:bpd_likb_7_2_F)) %>%
  relocate(bpd_likb_7_2, .after = bpd_likb_7_1)

df1 <- df1 %>% relocate(bpd_likb_10_2, .after = bpd_likb_10_1) %>% 
  relocate(bpd_lika_1:bpd_lika_15, .before = bpd_likb_1_1)

# df1 %>% glimpse()