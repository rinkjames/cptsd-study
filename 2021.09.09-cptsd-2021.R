# packages ----------------------------------------------------------------

library(styler) # code formatting
library(tidyverse) # data manipulation
library(psych) # psychometrics
# library(lest)       # case_when()
# library(poLCA)      # lca
# library(reshape2)   # melt()
# library(ggplot2)    # plots


# import data -------------------------------------------------------------

setwd("~/Documents/work/2021 UCT/cptsd masters study/5. analysis/cptsd-2021")
df <- as_tibble(read.csv("20210913-data-export-deidentified.csv"))


# cleaning data -----------------------------------------------------------

# remove unneeded columns
df <- df |>
  select(-c(2, 5:14, 124:125, 262, 272:274))

# labelling columns
colnames(df) <- c(
  "id", "date", "uni",
  # lec5 columns
  paste0("lec", "_", c(1:16, "17a", "17b")) |>
    map(\(i) paste0(i, "_", LETTERS[1:6])) |>
    unlist(), "lec_17_desc",
  # critA columns
  c(
    "desc", "time",
    paste0("exp", "_", LETTERS[1:5]),
    "exp_E_desc",
    paste0("life_in_danger", "_", LETTERS[1:3]),
    paste0("injury_death", "_", LETTERS[1:3]),
    "sex_violence",
    "accident_natural",
    "exposure", "exposure_mult"
  ) %>%
    paste0("critA", "_", .),
  # pcl5 columns
  paste0("pcl", "_", c(1:20)),
  # itq columns
  c(
    c("re", "av", "th") |>
      map(\(i) paste0(i, "_", 1:2)) |>
      unlist(),
    paste0("imp_ptsd", "_", 1:3),
    c("ad", "nsc", "dr") |>
      map(\(i) paste0(i, "_", 1:2)) |>
      unlist(),
    paste0("imp_dso", "_", 1:3)
  ) %>% paste0("itq", "_", .),
  # bpd columns
  c("bin", "lik") %>%
    map(\(i) paste0("bpd", "_", i, "_", 1:15)) |>
    unlist(),
  # bdi columns
  paste0("bdi", "_", c(1:21)),
  # ctq columns
  paste0("ctq", "_", c(1:28)),
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
  "complete",
  "dup_id"
)

# removing incomplete and duplicate responses
df <- df |>
  filter(complete == 2 & !is.na(dup_id)) |>
  select(-complete, -dup_id)

## university vars (nomibal); uct, ru, tut, wits
df$uni <- factor('uct')

## lec5 vars (nominal); A-F
df <- df |>
  select(lec_1_A:lec_17b_F) |>
  mutate(resp = row_number()) |>
  pivot_longer(
    cols = !resp,
    names_to = c(".value", "choices"),
    names_pattern = "^(lec_\\d+[a-b]?)_([A-F])$"
  ) |>
  group_by(resp) |>
  summarise(across(!choices, \(i) str_c(choices[as.logical(i)],
    collapse = ""
  ))) |>
  bind_cols(df) |>
  select(!c(lec_1_A:lec_17b_F, resp)) |>
  relocate(lec_1:lec_17b, .after = uni)

# renaming lec_17a and removing empty lec_17b
df <- df |>
  rename(lec_17 = lec_17a) |>
  select(!lec_17b)

## critA vars
# time index for critA traumatic event (continuous); months
df$critA_time <-
  c(
    2, 2, 7, 3 / 12, 2, 2.5, 3 / 12, 8, NA, 4 / 12,
    6, 32, 2, 4, 16, 6, 2, NA, 13, 11,
    7, 2 / 24 / 365, 0.5, 3.5, 6, 1.5, 2, 1.5, 4, 2,
    5, 22 / 12, 2, 8, 8, 5, 3 / 12, NA, 0.5, 3,
    1, 2, NA, 4, 5 / 12, 4, 1 / 12, 11 / 12, 1 / 12, 32 / 365,
    11, 6, 3, 1.5, 3, 2, 6, 11, 2, 1,
    2, NA, 1.5, 5, NA, 16, 12, NA, 4, 13,
    12, 9, 5, 4, 3.5, 2, 3.5, 3, 3, 3,
    9 / 12, 2, 4, 6 / 365, 2, 14 / 12, 1.5, 6 / 12, 14 / 365, 5,
    11, 7, NA, 4, NA, 9 / 12, 6, NA, 2, 7,
    9, 3, 52 / 12, 2, 10, 10, 3, 7, 1, 6,
    21 / 365, NA, 13, 6, 1, 7, 13, 802 / 365, NA, 12,
    5, 3, 1 / 12, 3651 / 365, 2, 7, 10, 5, 8, NA,
    5, 6, 2, 4, 3, 32 / 365, 1.5, 8, 2, 4,
    3, 3, 21 / 365, 157 / 365, 3, NA, 4, 1 / 12
  ) |>
  (\(i) as.integer(i * 12))()

# critA experience, life_in_danger, and injury_death vars (nominal); A-E
df <- df |>
  select(critA_exp_A:critA_injury_death_C & -critA_exp_E_desc) |>
  mutate(resp = row_number()) |>
  pivot_longer(
    cols = !resp,
    names_to = c(".value", "choices"),
    names_pattern = "^(critA_.*)_([A-E])$"
  ) |>
  mutate(across(
    critA_life_in_danger:critA_injury_death,
    \(i) replace_na(i, 0)
  )) |> 
  group_by(resp) |>
  summarise(across(
    !choices,
    \(i) str_c(choices[as.logical(i)], collapse = "")
  )) |>
  bind_cols(df) |>
  rename(critA_exp_desc = critA_exp_E_desc) |>
  select(!c(critA_exp_A:critA_injury_death_C & -critA_exp_desc, resp)) |>
  relocate(critA_exp, .before = critA_exp_desc) |>
  relocate(critA_life_in_danger:critA_injury_death,
    .before = critA_sex_violence
  )


# psychometrics -----------------------------------------------------------

psych_reg <- c(
  # pcl
  "^pcl_\\d+$",
  # ptsd without impairment
  "^itq_(re|av|th)_\\d$",
  # ptsd with impairment
  "^itq_(re|av|th|imp)_(ptsd_)?\\d$",
  # ptsd re-experiencing, avoidance, & threat sx
  "^itq_re_\\d$", "^itq_av_\\d$", "^itq_th_\\d$",
  # ptsd impairment sx
  "^itq_imp_ptsd_\\d$",
  # dso without impairment
  "^itq_(ad|nsc|dr)_\\d$",
  # dso with impairment
  "^itq_(ad|nsc|dr|imp)_(dso_)?\\d$",
  # dso affect-dysregulation, self-concept deficit, & interpersonal deficit sx
  "^itq_ad_\\d$", "^itq_nsc_\\d$", "^itq_dr_\\d$",
  # dso impairment sx
  "^itq_imp_dso_\\d$",
  # bpd binary and likert
  "^bpd_bin_\\d+$", "^bpd_lik_\\d+$",
  # bdi
  "^bdi_\\d+$",
  # ctq
  "^ctq_\\d+$",
  # ctq subscales
  "^ctq_(3|8|14|18|25)$", "^ctq_(9|11|12|15|17)$", "^ctq_(20|21|23|24|27)$",
  "^ctq_(5|7|13|19|28)$", "^ctq_(1|2|4|6|26)$", "^ctq_(10|16|22)$"
) |> set_names(c(
  "pcl",
  "ptsd", "ptsd+imp", "ptsd_re", "ptsd_av", "ptsd_th", "ptsd_imp",
  "dso", "dso+imp", "dso_af", "dso_nsc", "dso_dr", "dso_imp",
  "bpd_bin", "bpd_lik", "bdi",
  "ctq", "ctq_emo_abuse", "ctq_phy_abuse", "ctq_sex_abuse",
  "ctq_emo_negl", "ctq_phy_negl", "ctq_min_denial"
))

psychometrics <-
  map_df(psych_reg, \(i)
  df |>
    select(matches(i)) |>
    alpha(keys = c(
      "ctq_2", "ctq_5", "ctq_7", "ctq_10", "ctq_13",
      "ctq_16", "ctq_19", "ctq_22", "ctq_26", "ctq_28"
    )) |>
    (\(j) j$total)() |>
    remove_rownames(),
  .id = "scale"
  )


# calculating vars --------------------------------------------------------

## critA endorsement
df <- df |>
  mutate(
    critA = case_when(
      critA_time >= 1 &
        str_detect(critA_exp, "[A-D]") &
        (str_detect(critA_life_in_danger, "[A-B]") |
          str_detect(critA_injury_death, "[A-B]") |
          critA_sex_violence == 1) ~ 1,
      TRUE ~ 0
    ) |> as.integer()
  ) |>
  relocate(critA, .after = critA_exposure_mult)

# manual recoding based on interpretation of critA_desc
df <- df |>
  mutate(critA = ifelse(id %in%
    c(
      7, 8, 9, 15, 18, 19, 27, 52, 59, 67,
      69, 72, 77, 81, 94, 96, 103, 111, 116, 131,
      136, 145, 147, 148, 157, 160, 178, 190, 194, 201,
      203, 206, 212, 214, 220, 224, 225, 227
    ), 0L, critA))

# write_csv(df |> select(id, critA_desc:critA),"critA-inspection.csv")Î

itq.cutoff <- 2 # given range 0 to 4

# PTSD & DSO symptom diagnostic scores (binary); no = 1; yes = 2
df[as.character(expression(re1, re2, av1, av2, th1, th2, ad1, ad2, nsc1, nsc2, dr1, dr2))] <- NA
df[, 54:65] <- llply(c(3:8, 12:17), function(x) {
  case_when(df[, x] >= itq.cutoff ~ 2, TRUE ~ 1)
})
# PTSD & DSO symptom cluster diagnostic scores (binary); no = 1; yes = 2
df[as.character(expression(re, av, th, ad, nsc, dr))] <- NA
df[, 66:71] <- llply(c(54, 56, 58, 60, 62, 64), function(x) {
  case_when(df[, x] == 2 | df[, (x + 1)] == 2 ~ 2, TRUE ~ 1)
})
# PTSD & DSO symptom cluster dimensional scores (continuous); 1 through 9
df[as.character(expression(re.c, av.c, th.c, ad.c, nsc.c, dr.c))] <- NA
df[, 72:77] <- llply(c(3, 5, 7, 12, 14, 16), function(x) {
  rowSums(df[, x:(x + 1)]) + 1
})
# PTSD & DSO functional impairment scores (binary); no = 1; yes = 2
df[as.character(expression(imp.ptsd, imp.dso))] <- NA
df[, 78:79] <- llply(c(9, 18), function(x) {
  case_when(df[, x] >= itq.cutoff | df[, (x + 1)] >= itq.cutoff |
    df[, (x + 2)] >= itq.cutoff ~ 2, TRUE ~ 1)
})

# diagnostic scores for PTSD and CPTSD; no = 1; yes = 2
df$dso <- case_when(
  df$ad == 2 & df$nsc == 2 & df$dr == 2 & df$imp.dso == 2 ~ 2,
  TRUE ~ 1
)
df$ptsd <- case_when(
  df$re == 2 & df$av == 2 & df$th == 2 & df$imp.ptsd == 2 & df$dso == 1 ~ 2,
  TRUE ~ 1
)
df$cptsd <- case_when(
  df$re == 2 & df$av == 2 & df$th == 2 & df$imp.ptsd == 2 & df$dso == 2 ~ 2,
  TRUE ~ 1
)

## dimensional scores for PTSD and DSO; 1 through 25
df$ptsd.c <- rowSums(df[, 72:74] - 1) + 1
df$dso.c <- rowSums(df[, 75:77] - 1) + 1

## CTQ subscales (continuous); 5 through 25
# emotional abuse
df$ea <- rowSums(df[, c("ctq.3", "ctq.8", "ctq.14", "ctq.18", "ctq.25")])
# physical abuse
df$pa <- rowSums(df[, c("ctq.9", "ctq.11", "ctq.12", "ctq.15", "ctq.17")])
# sexual abuse
df$sa <- rowSums(df[, c("ctq.20", "ctq.21", "ctq.23", "ctq.24", "ctq.27")])
# emotional neglect
df$en <- rowSums(6 - df[, c("ctq.5", "ctq.7", "ctq.13", "ctq.19", "ctq.28")])
# physical neglect
df$pn <- rowSums(6 - df[, c("ctq.2", "ctq.26")]) + rowSums(df[, c("ctq.1", "ctq.4", "ctq.6")])

## minimisation / denial (continuous); 0 through 3
df$md <- rowSums(df[, c("ctq.10", "ctq.16", "ctq.22")] == 5)

## CTQ subscales (means); 1 through 5
# emotional abuse
df$ea.m <- rowMeans(df[, c("ctq.3", "ctq.8", "ctq.14", "ctq.18", "ctq.25")])
# physical abuse
df$pa.m <- rowMeans(df[, c("ctq.9", "ctq.11", "ctq.12", "ctq.15", "ctq.17")])
# sexual abuse
df$sa.m <- rowMeans(df[, c("ctq.20", "ctq.21", "ctq.23", "ctq.24", "ctq.27")])
# emotional neglect
df$en.m <- rowMeans(6 - df[, c("ctq.5", "ctq.7", "ctq.13", "ctq.19", "ctq.28")])
# physical neglect
df$pn.m <- (rowSums(6 - df[, c("ctq.2", "ctq.26")]) + rowSums(df[, c("ctq.1", "ctq.4", "ctq.6")])) / 5

## CTQ scale severity cut-offs; 1=minimal; 2=low; 3=moderate; 4=severe
df$ea.sev <- as.numeric(cut(df$ea, c(4, 8, 12, 15, 25), labels = 1:4))
df$pa.sev <- as.numeric(cut(df$pa, c(4, 7, 9, 12, 25), labels = 1:4))
df$sa.sev <- as.numeric(cut(df$sa, c(4, 5, 7, 12, 25), labels = 1:4))
df$en.sev <- as.numeric(cut(df$en, c(4, 9, 14, 17, 25), labels = 1:4))
df$pn.sev <- as.numeric(cut(df$pn, c(4, 7, 9, 12, 25), labels = 1:4))

## CTQ score totals
# total abuse score (continuous); 15 through 75
df$abuse <- df$ea + df$pa + df$sa
# total neglect score (continuous); 10 though 50
df$neglect <- df$en + df$pn
# total CTQ trauma score (continuous); 25 through 125
df$trauma <- df$abuse + df$neglect

## CTQ score means
# mean abuse score; 1 through 5
df$abuse.m <- (df$ea + df$pa + df$sa) / 15
# mean neglect score; 1 through 5
df$neglect.m <- (df$en + df$pn) / 10
# mean CTQ trauma score; 1 through 5
df$trauma.m <- (df$ea + df$pa + df$sa + df$en + df$pn) / 25

## CTQ no. of types of exposure to moderate-to-severe abuse
# abuse (continuous); 0 through 3
df$abuse.types <- rowSums(df[, c("ea.sev", "pa.sev", "sa.sev")] >= 3)
# neglect (continuous); 0 through 2
df$neglect.types <- rowSums(df[, c("en.sev", "pn.sev")] >= 3)
# trauma (continuous); 0 through 5
df$trauma.types <- rowSums(df[, c("abuse.types", "neglect.types")])

## CTQ min/denial flag; 1=flagged; 0=not flagged
df$mdflag <- case_when(
  rowSums(df[, 91:95] == 1) >= 3 & rowSums(df[, 91:95] >= 3) == 0 & df$md == 3 ~ 2,
  TRUE ~ 1
)

## specify lca parametrs ----
# indicator vars
lca.vars <- as.character(expression(
  re1, re2, av1, av2, th1, th2, ad1, ad2, nsc1, nsc2, dr1, dr2
))
# lca formula
lca.f <- as.formula(paste("cbind(", paste(lca.vars, collapse = ","), ")~1"))
# classes to model; min = 1; max = 9
classes <- 1:6
# lca repetitions
lca.nrep <- 20
# no. of classes in best model, if already known/selected
bestmodel.classes <- 4
# manual class order for best model
#  classes=4, nrep=100, seed=45634; 1=ptsd, 2=cptsd, 3=dso, 4=low
# class.order <- c(4,3,1,2)
# manual class order for best model
#  classes=4, nrep=20, seed=45634; 1=ptsd, 2=cptsd, 3=dso, 4=low
class.order <- c(1, 4, 2, 3)

## filtering dataframe by relevant vars  ----
# min/denial flag
# df <- df[df$mdflag==1,]

# moderate to severe abuse
# df.modsev.a <- filter(df,df$abuse.types >= 2)
# df.modsev.t <- filter(df,df$trauma.types >= 2)
# df.ea <- filter(df,df$ea >= 13)
# df.pa <- filter(df,df$pa >= 10)
# df.sa <- filter(df,df$sa >= 8)

# moderate to severe neglect
# df.modsev.n <- filter(df,df$neglect.types >= 2)
# df.en <- filter(df,df$en >= 15)
# df.pn <- filter(df,df$pn >= 10)

# sex
# df.f <- filter(df,df$sex == 'f')
# df.m <- filter(df,df$sex == 'm')

# race
# df.african <- filter(df,df$race == 'african')
# df.asianindian <- filter(df,df$race == 'asian/indian')
# df.mixed <- filter(df,df$race == 'mixed')
# df.white <- filter(df,df$race == 'white')

## generate 1:n models of classes x:y ----
lca <- function(i) {
  llply(classes, function(x) {
    set.seed(45634)
    lc <- poLCA(lca.f, i, x, nrep = lca.nrep, verbose = FALSE, calc.se = FALSE)
    lc.reorder <- poLCA.reorder(
      lc$probs.start,
      if (x == bestmodel.classes) {
        class.order
      } else {
        order(lc$P, decreasing = TRUE)
      }
    )
    poLCA(lca.f, i, x, probs.start = lc.reorder, verbose = FALSE)
  })
}
models <- llply(list(df), lca)[[1]]

## model fit criteria ----
fit <- function(j) {
  ldply(seq_along(classes), function(x) {
    fit <- data.frame(model = paste(classes[x], "-class", sep = ""))
    fit$loglik <- j[[x]]$llik
    fit$bic <- j[[x]]$bic
    fit$abic <- (-2 * j[[x]]$llik) + ((log((j[[x]]$N + 2) / 24)) * j[[x]]$npar)
    fit$aic <- j[[x]]$aic
    fit$caic <- (-2 * j[[x]]$llik) + j[[x]]$npar * (1 + log(j[[x]]$N))
    entfun <- function(p) sum(-p * log(p))
    error.prior <- entfun(j[[x]]$P)
    error.post <- mean(apply(j[[x]]$posterior, 1, entfun), na.rm = TRUE)
    fit$entropy <- ((error.prior - error.post) / error.prior)
    names(fit) <- c(
      "Model", "Log-likelihood", "BIC", "Adjusted BIC", "AIC",
      "Corrected AIC", "Entropy"
    )
    return(fit)
  })
}

models.fit <- fit(models)
models.fit

## profile plots ----
pplot1 <- function(j) {
  ## class-conditional probabilities
  means <- ldply(j, function(M) {
    ldply(M$probs, function(p) colSums((1:ncol(p)) * t(p)) - 1)
  })
  errors <- ldply(j, function(M) {
    ldply(M$probs.se, function(p) colSums((1:ncol(p)) * t(p)))
  })
  melted.means <- melt(means, ".id")
  names(melted.means) <- c("var", "class", "prob")
  melted.means$var <- factor(melted.means$var, levels = {
    lca.vars
  })
  levels(melted.means$class) <- as.character(seq_along(levels(melted.means$class)))
  melted.means$lower <- pmax(0, melt(means[, -1] - errors[, -1])[, 2])
  melted.means$upper <- pmin(melt(means[, -1] + errors[, -1])[, 2], 1)
  melted.means$model <- sapply(seq_along(classes), function(x) {
    model <- paste(classes[x], "-class", sep = "")
  })[
    rep(rep(seq_along(classes), each = {
      length(lca.vars)
    }), classes[length(classes)])
  ]
  melted.means <- na.omit(melted.means)
  # print(melted.means)
  # print(errors)
  ## profile plots
  ggplot(melted.means, aes(x = var, y = prob, shape = class)) +
    facet_wrap(~model) +
    geom_line(aes(colour = class, group = class)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = class, group = class), alpha = 0.15) +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 1, .1), labels = c("0", seq(0.1, .9, 0.1), "1")) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 10, face = "bold")
    ) +
    ggtitle(paste("LCA: N = ", j[[1]]$N, sep = ""))
}

models.plots <- pplot1(models)
models.plots

## best model profile plot ----
# best / selected model, if not already set
bestmodel.classes <- 4
bestmodel <- which(classes == bestmodel.classes)

# descriptive labels for each class in best model
class.labels <- laply(1:bestmodel.classes, function(x) {
  labels <- c(" PTSD ", " CPTSD ", " DSO ", " Low ")
  paste("Class ", x, labels[x], "(",
    round(100 * table(models[[bestmodel]]$predclass)[x] / models[[bestmodel]]$N, 2),
    "%)",
    sep = ""
  )
})

# descriptive labels for indicator variables
lca.vars.labels <- c(
  "PTSD Dreams", "PTSD Flashbacks", "PTSD Av. Thoughts",
  "PTSD Av. Behaviour", "PTSD Hypervigilance", "PTSD Startle",
  "DSO Aff. Hyper.", "DSO Aff. Hypo.", "DSO Guilty",
  "DSO Worthless", "DSO Distant", "DSO Detached"
)

# import SE from mplus calc; seems more accurate than poLCA SE
mplus4se <- as.data.frame(read_excel("mplus4se.xlsx"))

# class-conditional probabilities in best model + mplus SEs
bestmodel.means <- mutate(models.plots$data[
  models.plots$data$model == paste(c(bestmodel.classes, "-class"), collapse = "") &
    as.numeric(models.plots$data$class) <= bestmodel.classes,
],
lower = mplus4se[, 3],
upper = mplus4se[, 5]
)

# profile plot of best model
pplot2 <- function(j) {
  ggplot(j, aes(x = var, y = prob)) +
    geom_hline(yintercept = seq(0.1, 1, .1), color = "#939398") +
    geom_line(aes(colour = class, group = class), size = 1.5) +
    scale_color_manual(
      labels = class.labels,
      values = c("#e87d72", "#7ab582", "#56bcc2", "#ba9f56")
    ) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = class, group = class), alpha = 0.175) +
    coord_cartesian(ylim = c(0, 1)) +
    geom_point(aes(color = class, shape = class, group = class), size = 4.5) +
    scale_shape_manual(values = c(19, 19, 19, 19), labels = class.labels) +
    scale_fill_manual(values = c("#e87d72", "#7ab582", "#56bcc2", "#ba9f56")) +
    theme_bw() +
    guides(shape = FALSE, fill = FALSE) +
    scale_y_continuous(
      breaks = seq(0, 1, .1), labels = c("0", seq(0.1, .9, 0.1), "1"),
      expand = expansion(mult = c(.0025, .0025))
    ) +
    scale_x_discrete(labels = lca.vars.labels) +
    geom_hline(yintercept = seq(0, 0), color = "#939398") +
    theme(
      axis.text.x = element_text(
        angle = 45, vjust = 1,
        hjust = 1, size = 10, color = "#000000"
      ),
      axis.text.y = element_text(size = 10, color = "#000000"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      legend.text = element_text(size = 10),
      plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
      legend.box.margin = margin(0, 0, 0, -1.1, unit = "cm"),
      legend.position = "bottom",
      text = element_text(family = "sans")
    )
}

bestmodel.plot <- pplot2(bestmodel.means)
bestmodel.plot

# ggsave('figure1.p',bestmodel.plot,dev='pdf',scale=2,width=16.7,height=7,dpi=300,units='cm')
# ggsave('models.plots.pdf',models.plots,dev='pdf',scale=2.5,width=12,height=6,units='cm')


