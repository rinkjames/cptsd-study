# calculating vars --------------------------------------------------------
## pcl-5 vars -------------------------------------------------------------

# pcl crita endorsement
df1 <- df1 %>%
  mutate(crita = case_when(
    str_detect(crita_exp, "[A-D]") &
      (
        str_detect(crita_life_in_danger, "[A-B]") |
          str_detect(crita_injury_death, "[A-B]") |
          crita_sex_violence == "yes"
      ) ~ 1,
    TRUE ~ 0
  ))

# # export crita vars to score crita endorsement manually
# write_csv(
#   df1 %>% select(
#     id,
#     lec_17_desc,
#     crita_desc,
#     crita_exp,
#     crita_life_in_danger,
#     crita_injury_death,
#     crita_sex_violence,
#     crita_accident_natural,
#     crita
#   ),
#   here("output", paste(
#     format(Sys.time(), "%Y.%m.%d"),
#     "crita.csv",
#     sep = "-"
#   ))
# )

# import manually scored data
df1 <- here("data", "2023.10.05-crita-manual_scoring.csv") %>%
  read_csv(show_col_types = F) %>%
  mutate(crita_manual = if_else(crita_manual == 2, NA_real_, crita_manual)) %>%
  inner_join(df1, by = "id") %>%
  relocate(crita_manual, .after = crita)

# dsm-5 criteria
df1 <- df1 %>%
  mutate(
    pcl_ptsd_dsm = case_when(
      crita_manual == 1 &
        crita_time != "< 1 month" &
        !is.na(crita_time) &
        rowSums(across(pcl_1:pcl_5) >= 2, na.rm = T) >= 1 &
        rowSums(across(pcl_6:pcl_7) >= 2, na.rm = T) >= 1 &
        rowSums(across(pcl_8:pcl_14) >= 2, na.rm = T) >= 2 &
        rowSums(across(pcl_15:pcl_20) >= 2, na.rm = T) >= 2
      ~ 1,
      is.na(crita_manual) | is.na(crita_time) ~ NA,
      TRUE ~ 0
    )
  )

# cut-offs
df1 <- df1 %>%
  mutate(
    pcl_ptsd_cutoff_low = case_when(
      crita_manual == 1 &
        crita_time != "< 1 month" &
        !is.na(crita_time) &
        rowSums(across(pcl_1:pcl_20), na.rm = T) >= 31 ~ 1,
      is.na(crita_manual) | is.na(crita_time) ~ NA,
      TRUE ~ 0
    )
  )

df1 <- df1 %>%
  mutate(
    pcl_ptsd_cutoff_high = case_when(
      crita_manual == 1 &
        crita_time != "< 1 month" &
        !is.na(crita_time) &
        rowSums(across(pcl_1:pcl_20), na.rm = T) >= 33 ~ 1,
      is.na(crita_manual) | is.na(crita_time) ~ NA,
      TRUE ~ 0
    )
  )

## itq vars ---------------------------------------------------------------

## itq ptsd & dso symptom scores
df1 <- c("re1",
         "re2",
         "av1",
         "av2",
         "th1",
         "th2",
         "ad1",
         "ad2",
         "nsc1",
         "nsc2",
         "dr1",
         "dr2") %>%
  imap_dfc(~ {
    df1 %>% transmute(!!paste0("itq_", ifelse(.y <= 6, "ptsd_", "dso_"), .x) :=
                        case_when(!!sym(
                          str_replace(.x, "(\\w+)(\\d)", "itq_\\1_\\2")
                        )
                        >= 2 ~ 1,
                        TRUE ~ 0))
  }) %>%
  bind_cols(df1) %>%
  relocate(itq_ptsd_re1:itq_dso_dr2, .after = pcl_ptsd_cutoff_high)

# itq ptsd & dso symptom cluster scores
df1 <- c("re", "av", "th", "ad", "nsc", "dr") %>%
  imap_dfc( ~ {
    itq_dx = paste0("itq_", if_else(.y <= 3, "ptsd_", "dso_"), .x)
    rowwise(df1) %>%
      transmute(!!itq_dx := case_when(!!sym(paste0(itq_dx, 1)) == 1 |
                                        !!sym(paste0(itq_dx, 2)) == 1 ~ 1,
                                      TRUE ~ 0))
  }) %>%
  ungroup() %>%
  bind_cols(df1) %>%
  relocate(itq_ptsd_re:itq_dso_dr, .after = itq_dso_dr2)

# itq ptsd & dso functional impairment scores
df1 <- c("ptsd", "dso") %>%
  imap_dfc( ~ {
    transmute(
      rowwise(df1),
      !!paste0("itq_", .x, "_imp") := case_when(
        !!sym(paste0("itq_imp_", .x, "_1")) >= 2 |
          !!sym(paste0("itq_imp_", .x, "_2")) >= 2 |
          !!sym(paste0("itq_imp_", .x, "_3")) >= 2 ~ 1,
        TRUE ~ 0
      )
    )
  }) %>%
  ungroup() %>%
  bind_cols(df1) %>%
  relocate(itq_ptsd_imp:itq_dso_imp, .after = itq_dso_dr)

## itq ptsd, dso, & cptsd diagnostic scores
df1 <- df1 %>% mutate(itq_dso = case_when(
  itq_dso_ad == 1 &
    itq_dso_nsc == 1 &
    itq_dso_dr == 1 &
    itq_dso_imp == 1 &
    (
      itq_ptsd_re == 0 |
        itq_ptsd_av == 0 |
        itq_ptsd_th == 0 |
        itq_ptsd_imp == 0
    ) ~ 1,
  TRUE ~ 0
))

df1 <- df1 %>% mutate(
  itq_ptsd = case_when(
    crita_time != "< 1 month" &
      !is.na(crita_time) &
      itq_ptsd_re == 1 &
      itq_ptsd_av == 1 &
      itq_ptsd_th == 1 &
      itq_ptsd_imp == 1 &
      (itq_dso_ad == 0 |
         itq_dso_nsc == 0 |
         itq_dso_dr == 0 |
         itq_dso_imp == 0) ~ 1,
    is.na(crita_time) ~ NA,
    TRUE ~ 0
  )
)

df1 <- df1 %>% mutate(
  itq_ptsd_noimp = case_when(
    crita_time != "< 1 month" &
      !is.na(crita_time) &
      itq_ptsd_re == 1 &
      itq_ptsd_av == 1 &
      itq_ptsd_th == 1 &
      (itq_dso_ad == 0 |
         itq_dso_nsc == 0 |
         itq_dso_dr == 0) ~ 1,
    is.na(crita_time) ~ NA,
    TRUE ~ 0
  )
)

df1 <- df1 %>% mutate(
  itq_cptsd = case_when(
    crita_time != "< 1 month" &
      !is.na(crita_time) &
      itq_ptsd_re == 1 &
      itq_ptsd_av == 1 &
      itq_ptsd_th == 1 &
      itq_ptsd_imp == 1 &
      itq_dso_ad == 1 &
      itq_dso_nsc == 1 &
      itq_dso_dr == 1 &
      itq_dso_imp == 1 ~ 1,
    is.na(crita_time) ~ NA,
    TRUE ~ 0
  )
)

df1 <- df1 %>% mutate(
  itq_cptsd_noimp = case_when(
    crita_time != "< 1 month" &
      !is.na(crita_time) &
      itq_ptsd_re == 1 &
      itq_ptsd_av == 1 &
      itq_ptsd_th == 1 &
      itq_dso_ad == 1 &
      itq_dso_nsc == 1 &
      itq_dso_dr == 1 ~ 1,
    is.na(crita_time) ~ NA,
    TRUE ~ 0
  )
)


## scid-5 spq vars --------------------------------------------------------

# spq bpd diagnostic scores

# calculated from binary items
df1 <- df1 %>% mutate(
  spq_bpd_bin_score =
    rowSums(across(
      c(
        bpd_bin_1,
        bpd_bin_2,
        bpd_bin_7,
        bpd_bin_10,
        bpd_bin_11,
        bpd_bin_15
      )
    ), na.rm = T) +
    if_else(rowSums(across(
      bpd_bin_3:bpd_bin_6
    ), na.rm = T) >= 1, 1, 0) +
    if_else(rowSums(across(
      bpd_bin_8:bpd_bin_9
    ), na.rm = T) >= 1, 1, 0) +
    if_else(rowSums(across(
      bpd_bin_12:bpd_bin_14
    ), na.rm = T) >= 1, 1, 0)
)

df1 <- df1 %>%
  mutate(spq_bpd_bin = case_when(spq_bpd_bin_score >= 5 ~ 1,
                                 is.na(spq_bpd_bin_score) ~ NA,
                                 TRUE ~ 0))

# calculated from first round of likert items (likert-a)
# endorsement at >=3
df1 <- df1 %>% mutate(spq_bpd_lika_score = (
  rowSums(across(
    c(
      bpd_lika_1,
      bpd_lika_2,
      bpd_lika_7,
      bpd_lika_10,
      bpd_lika_11,
      bpd_lika_15
    )
  ) >= 3, na.rm = T) +
    ifelse(rowSums(across(
      c(bpd_lika_3, bpd_lika_4, bpd_lika_5, bpd_lika_6)
    ) >= 3, na.rm = T) >= 1, 1, 0) +
    ifelse(rowSums(across(
      c(bpd_lika_8, bpd_lika_9)
    ) >= 3, na.rm = T) >= 1, 1, 0) +
    ifelse(rowSums(across(
      c(bpd_lika_12, bpd_lika_13, bpd_lika_14)
    ) >= 3, na.rm = T) >= 1, 1, 0)
))

df1 <- df1 %>%
  mutate(spq_bpd_lika = case_when(
    spq_bpd_lika_score >= 5 ~ 1,
    is.na(spq_bpd_lika_score) ~ NA,
    TRUE ~ 0
  ))

# # export bpd likert-b items to score manually
# write_csv(
#   df1 %>% select(
#     id,
#     age,
#     spq_bpd_bin_score,
#     spq_bpd_lika_score,
#     bpd_likb_1_1:bpd_likb_15_eg3
#   ),
#   here("output", paste(
#     format(Sys.time(), "%Y.%m.%d"),
#     "bpd_likb.csv",
#     sep = "-"
#   ))
# )


## bdi vars ---------------------------------------------------------------

# bdi total scores
df1 <-
  df1 %>% mutate(bdi_mdd_score = rowSums(across(bdi_1:bdi_21), na.rm = T))

# bdi severity scores
# 1=normal; 2=mild; 3=borderline; 4=moderate; 5=severe; 6=extreme
df1 <-
  df1 %>% mutate(bdi_mdd_sev = cut(rowSums(across(bdi_1:bdi_21), na.rm = T),
                               c(-1, 10, 16, 20, 30, 40, 63),
                               labels = 1:6) %>% as.numeric())

# bdi diagnostic scores
# endorsement = moderate-to-severe sx
df1 <- df1 %>%
  mutate(bdi_mdd = case_when(bdi_mdd_sev >= 4 ~ 1, TRUE ~ 0))

## ctq vars ---------------------------------------------------------------

# ctq subscale scores

# emotional abuse
df1 <- df1 %>% mutate(ctq_ea_score = rowSums(across(c(
  ctq_3, ctq_8, ctq_14, ctq_18, ctq_25
))))
# physical abuse
df1 <- df1 %>% mutate(ctq_pa_score = rowSums(across(c(
  ctq_9, ctq_11, ctq_12, ctq_15, ctq_17
))))
# sexual abuse
df1 <- df1 %>% mutate(ctq_sa_score = rowSums(across(c(
  ctq_20, ctq_21, ctq_23, ctq_24, ctq_27
))))
# emotional neglect
df1 <- df1 %>% mutate(ctq_en_score = rowSums(6 - across(c(
  ctq_5, ctq_7, ctq_13, ctq_19, ctq_28
))))
# physical neglect
df1 <- df1 %>% mutate(ctq_pn_score = (rowSums(6 - across(c(
  ctq_2, ctq_26
))) + rowSums(across(
  c(ctq_1, ctq_4, ctq_6)
))))

## ctq subscale severity scores
## 1=minimal; 2=low; 3=moderate; 4=severe
df1 <- df1 %>%
  mutate(ctq_ea_sev =
           cut(df1$ctq_ea_score,
               c(4, 8, 12, 15, 25),
               labels = 1:4) %>% as.numeric())

df1 <- df1 %>%
  mutate(ctq_pa_sev =
           cut(df1$ctq_pa_score,
               c(4, 7, 9, 12, 25),
               labels = 1:4) %>% as.numeric())

df1 <- df1 %>%
  mutate(ctq_sa_sev =
           cut(df1$ctq_sa_score,
               c(4, 5, 7, 12, 25),
               labels = 1:4) %>% as.numeric())

df1 <- df1 %>%
  mutate(ctq_en_sev =
           cut(df1$ctq_en_score,
               c(4, 9, 14, 17, 25),
               labels = 1:4) %>% as.numeric())

df1 <- df1 %>%
  mutate(ctq_pn_sev =
           cut(df1$ctq_pn_score,
               c(4, 7, 9, 12, 25),
               labels = 1:4) %>% as.numeric())

# ctq minimisation/denial flag
df1 <- df1 %>% mutate(ctq_md = case_when(
  rowSums(across(ctq_ea_sev:ctq_pn_sev) == 1) >= 3 &
    rowSums(across(ctq_ea_sev:ctq_pn_sev) >= 3) == 0 &
    rowSums(across(c(
      ctq_10, ctq_16, ctq_22
    )) == 5) >= 2 ~ 1,
  TRUE ~ 0
))

# ctq score totals

# total abuse score
# continuous; 15-75
df1 <-
  df1 %>% mutate(ctq_abuse = ctq_ea_score + ctq_pa_score + ctq_sa_score)

# total neglect score
# continuous; 10-50
df1 <- df1 %>% mutate(ctq_neglect = ctq_en_score + ctq_pn_score)

# total CTQ trauma score
# continuous; 25-125
df1 <- df1 %>% mutate(ctq_trauma = ctq_abuse + ctq_neglect)

# ctq no. of subtypes
# endorsement = moderate-to-severe exposure

# abuse
df1 <- df1 %>%
  mutate(ctq_abuse_types = rowSums(across(c(
    ctq_ea_sev, ctq_pa_sev, ctq_sa_sev
  )) >= 3))
# neglect
df1 <- df1 %>%
  mutate(ctq_neglect_types = rowSums(across(c(
    ctq_en_sev, ctq_pn_sev
  )) >= 3))
# trauma
df1 <- df1 %>%
  mutate(ctq_trauma_types = rowSums(across(c(
    ctq_abuse_types, ctq_neglect_types
  ))))

# df1 %>% glimpse()
