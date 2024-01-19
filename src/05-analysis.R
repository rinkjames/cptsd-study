# mcnemar tests -----------------------------------------------------------

# bpd likert-a items converted to binary items for mcnemar tests
df1 <- df1 %>% transmute(across(
  bpd_lika_1:bpd_lika_15,
  ~ case_when(.x >= 3 ~ 1,
              .x < 3 ~ 0,
              is.na(.x) ~ NA)
)) %>%
  set_names(paste0("bpd_lika_mc_", 1:15)) %>%
  bind_cols(df1) %>%
  relocate(bpd_lika_mc_1:bpd_lika_mc_15, .after = bpd_lika_15)

# compare prevalences of bpd from binary and likert items
table(df1$spq_bpd_bin, df1$spq_bpd_lika, useNA = "no") %>% mcnemar.test()

1:15 %>% map(~ {
  table(df1 %>% pull(!!sym(paste0("bpd_bin_", .x))),
        df1 %>% pull(!!sym(paste0(
          "bpd_lika_mc_", .x
        ))), useNA = "no") %>%
    mcnemar.test() %>%
    .$p.value
}) %>% unlist() %>%
  tibble() %>%
  setNames("p") %>%
  mutate(p = num(p, digits = 4))

# compare prevalences of dsm-5 and icd-11 ptsd
table(
  df1 %>% pull(pcl_ptsd_dsm),
  df1 %>% mutate(itq_trauma = itq_ptsd + itq_cptsd) %>%
    pull(itq_trauma),
  useNA = "no"
) %>% mcnemar.test()

table(
  df1 %>% pull(pcl_ptsd_dsm),
  df1 %>% mutate(itq_trauma = itq_ptsd_noimp + itq_cptsd_noimp) %>%
    pull(itq_trauma),
  useNA = "no"
) %>% mcnemar.test()


# LCA ---------------------------------------------------------------------
## adapt dataframe for poLCA package --------------------------------------

# create binary bpd sx vars from likert-a items
df2 <-
  df1 %>% mutate(spq_bpd_sx_1 = if_else(bpd_lika_1 >= 3, 1, 0)) %>%
  mutate(spq_bpd_sx_2 = if_else(bpd_lika_2 >= 3, 1, 0)) %>%
  mutate(spq_bpd_sx_3 = if_else(rowSums(across(
    c(bpd_lika_3:bpd_lika_6)
  ) >= 3, na.rm = T) >= 1, 1, 0)) %>%
  mutate(spq_bpd_sx_4 = if_else(bpd_lika_7 >= 3, 1, 0)) %>%
  mutate(spq_bpd_sx_5 = if_else(rowSums(across(
    c(bpd_lika_8:bpd_lika_9)
  ) >= 3, na.rm = T) >= 1, 1, 0)) %>%
  mutate(spq_bpd_sx_6 = if_else(bpd_lika_10 >= 3, 1, 0)) %>%
  mutate(spq_bpd_sx_7 = if_else(bpd_lika_11 >= 3, 1, 0)) %>%
  mutate(spq_bpd_sx_8 = if_else(rowSums(across(
    c(bpd_lika_12:bpd_lika_14)
  ) >= 3, na.rm = T) >= 1, 1, 0)) %>%
  mutate(spq_bpd_sx_9 = if_else(bpd_lika_15 >= 3, 1, 0))

# worst TE >= 1 month ago
df2 <- df2 %>% filter(crita_time != "< 1 month" & !is.na(crita_time))

# select indicator vars and
df2 <- df2 %>%
  select(id,
         itq_ptsd_re1:itq_dso_dr2,
         spq_bpd_sx_1:spq_bpd_sx_9,
         bdi_mdd_score)

# convert all values to positive integers
df2 <- df2 %>% mutate(across(!id, ~ .x + 1))

## generate models and assess fit -----------------------------------------

# indicator vars
indicators <- c(
  paste0("itq_ptsd_",
         c("re", "av", "th") %>% map(~ paste0(.x, 1:2)) %>% unlist()),
  paste0("itq_dso_",
         c("ad", "nsc", "dr") %>% map(~ paste0(.x, 1:2)) %>% unlist()),
  paste0("spq_bpd_sx_", 1:9)
)

classes <- 3:6

models <- lca(df2, indicators, "bdi_mdd_score",
              classes, 3) %>% setNames(classes)
models_fit <- fit(models)
print(models_fit)
