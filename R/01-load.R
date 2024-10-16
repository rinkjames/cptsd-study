# import data -------------------------------------------------------------

df1 <- here("data/raw", "2022.11.22-raw.csv") %>%
  read_csv(
    col_types = paste0(
      # id
      "d_cd______ccccD_",
      # lec
      str_dup("d", 108),
      "c__",
      # crita
      "ccd_dddddcdddddddddc",
      # pcl
      str_dup("d", 20),
      # itq
      str_dup("d", 18),
      # bpd
      str_dup("d", 18),
      "cccdddddd",
      str_dup("cccdd", 4),
      "ddddddcccddddcccddccc",
      "dddccc",
      str_dup("ddccc", 4),
      str_dup("d", 15),
      # bdi
      str_dup("d", 21),
      # ctq
      str_dup("d", 28),
      # socio
      "d_dddddddddc_dd"
    ),
    trim_ws = TRUE
  )

# renaming columns
df1 <- df1 %>%
  setNames(
    c(
      "id",
      "date_submit",
      "uni",
      "name_first",
      "name_last",
      "student_no_1",
      "email",
      "date_start",
      # lec5 columns
      paste0("lec_", c(1:16, "17a", "17b")) %>%
        map( ~ .x %>% paste0("_", LETTERS[1:6])) %>%
        unlist(),
      "lec_17_desc",
      # critA columns
      paste0(
        "crita_",
        c(
          "desc",
          "time_1",
          "time_2",
          paste0("exp_", LETTERS[1:5]),
          "exp_E_desc",
          paste0("life_in_danger_", LETTERS[1:3]),
          paste0("injury_death_", LETTERS[1:3]),
          "sex_violence",
          "accident_natural",
          "exposure",
          "exposure_more"
        )
      ),
      # pcl5 columns
      paste0("pcl_", c(1:20)),
      # itq columns
      paste0(
        "itq_",
        c(
          c("re", "av", "th") %>%
            map( ~ .x %>% paste0("_", 1:2)) %>% unlist(),
          paste0("imp_ptsd_", 1:3),
          c("ad", "nsc", "dr") %>%
            map( ~ .x %>% paste0("_", 1:2)) %>% unlist(),
          paste0("imp_dso_", 1:3)
        )
      ),
      # bpd columns
      paste0("bpd_bin_", 1:15),
      paste0(
        "bpd_likb_",
        c(
          "1_1",
          "1_2",
          "1_eg",
          paste0("1_eg", 1:3),
          "2_1",
          "2_2",
          "2_3",
          "2_4",
          "3_1",
          "3_eg",
          paste0("3_eg", 1:3),
          "4_1",
          "4_eg",
          paste0("4_eg", 1:3),
          "5_1",
          "5_eg",
          paste0("5_eg", 1:3),
          "6_1",
          "6_eg",
          paste0("6_eg", 1:3),
          "7_1",
          paste0("7_2_", LETTERS[1:6]),
          "7_eg",
          paste0("7_eg", 1:3),
          "8_1",
          "8_2",
          "9_1",
          "9_eg",
          paste0("9_eg", 1:3),
          "10_1",
          "10_eg",
          paste0("10_eg", 1:3),
          "10_2",
          "11_1",
          "11_eg",
          paste0("11_eg", 1:3),
          "12_1",
          "12_eg",
          paste0("12_eg", 1:3),
          "13_1",
          "13_eg",
          paste0("13_eg", 1:3),
          "14_1",
          "14_eg",
          paste0("14_eg", 1:3),
          "15_1",
          "15_eg",
          paste0("15_eg", 1:3)
        )
      ),
      paste0("bpd_lika_", 1:15),
      # bdi columns
      paste0("bdi_", c(1:21)),
      # ctq columns
      paste0("ctq_", c(1:28)),
      # socio columns
      "income_source",
      "income_indiv",
      "income_indiv_members",
      "income_parents",
      "income_parents_members",
      "age",
      "sex",
      "dx_psych",
      "therapy",
      "meds",
      "student_no_2",
      "comm_pref",
      "complete"
    )
  )

# df1 %>% glimpse()