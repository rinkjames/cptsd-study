# —————————————————————————————————————————————————————————————————————————
# calculate psychometrics
# —————————————————————————————————————————————————————————————————————————

psychometrics <- c(
  # pcl
  "^pcl_\\d+$",
  # ptsd without impairment
  "^itq_(re|av|th)_\\d$",
  # ptsd with impairment
  "^itq_(re|av|th|imp)_(ptsd_)?\\d$",
  # ptsd re-experiencing, avoidance, & threat sx
  "^itq_re_\\d$",
  "^itq_av_\\d$",
  "^itq_th_\\d$",
  # ptsd impairment sx
  "^itq_imp_ptsd_\\d$",
  # dso without impairment
  "^itq_(ad|nsc|dr)_\\d$",
  # dso with impairment
  "^itq_(ad|nsc|dr|imp)_(dso_)?\\d$",
  # dso affect-dysregulation, self-concept deficit, & interpersonal deficit sx
  "^itq_ad_\\d$",
  "^itq_nsc_\\d$",
  "^itq_dr_\\d$",
  # dso impairment sx
  "^itq_imp_dso_\\d$",
  # bpd binary and likert-a
  "^bpd_bin_\\d+$",
  "^bpd_lika_\\d+$",
  # bdi
  "^bdi_\\d+$",
  # ctq
  "^ctq_\\d+$",
  # ctq subscales
  "^ctq_(3|8|14|18|25)$",
  "^ctq_(9|11|12|15|17)$",
  "^ctq_(20|21|23|24|27)$",
  "^ctq_(5|7|13|19|28)$",
  "^ctq_(1|2|4|6|26)$",
  "^ctq_(10|16|22)$"
) %>%
  set_names(
    c(
      "pcl",
      "ptsd_noimp",
      "ptsd",
      "ptsd_re",
      "ptsd_av",
      "ptsd_th",
      "ptsd_imp",
      "dso_noimp",
      "dso",
      "dso_af",
      "dso_nsc",
      "dso_dr",
      "dso_imp",
      "bpd_bin",
      "bpd_lika",
      "bdi",
      "ctq",
      "ctq_emo_abuse",
      "ctq_phy_abuse",
      "ctq_sex_abuse",
      "ctq_emo_negl",
      "ctq_phy_negl",
      "ctq_min_denial"
    )
  ) %>%
  map_dfr(
    ~ df1 %>%
      select(matches(.x)) %>%
      psych::alpha(
        keys = c(
          "ctq_2",
          "ctq_5",
          "ctq_7",
          "ctq_10",
          "ctq_13",
          "ctq_16",
          "ctq_19",
          "ctq_22",
          "ctq_26",
          "ctq_28"
        )
      ) %>%
      .$total %>%
      remove_rownames(),
    .id = "scale"
  )

# print(psychometrics)
