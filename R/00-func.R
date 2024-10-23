# —————————————————————————————————————————————————————————————————————————
# define functions
# —————————————————————————————————————————————————————————————————————————

# always report any existing NAs in table
table <- \(..., useNA = 'ifany') base::table(..., useNA = useNA)

# used to clean multi-choice nominal data, e.g. LEC-5 items
nominal_multi <- \(.data, pattern) {
  .data %>%
    mutate(resp = row_number()) %>%
    pivot_longer(
      cols = !resp,
      names_to = c(".value", "choices"),
      names_pattern = pattern
    ) %>%
    mutate(across(!c(resp,choices), ~ replace_na(.x,0))) %>% 
    group_by(resp) %>%
    summarise(across(!choices, ~ choices[as.logical(.)] %>% str_flatten())) %>%
    mutate(across(-resp, ~ if_else(.x == "", NA_character_, .x))) %>%
    select(-resp)
}

# poLCA; generate 1:n models of classes x:y
lca <-
  \(
    .data,
    indicators,
    covariates,
    classes,
    nrep,
    best_classes = NULL,
    best_order = NULL
  ) {
    map(classes, ~ {
      set.seed(1234)
      
      lca_f =
        paste0("cbind(",
               paste0(indicators, collapse = ","),
               ")~",
               covariates) %>%
        as.formula()
      
      model =
        poLCA::poLCA(
          lca_f,
          .data,
          .x,
          nrep = nrep,
          verbose = F,
          calc.se = F
        )
      
      reorder = poLCA::poLCA.reorder(
        model$probs.start,
        if (is.null(best_classes))
          order(model$P, decreasing = T)
        else if (.x == best_classes)
          best_order
        else
          order(model$P, decreasing = T)
      )
      
      poLCA::poLCA(lca_f,
                   .data,
                   .x,
                   probs.start = reorder,
                   verbose = F)
    })
  }

# poLCA: model fit
fit <- \(models) {
  entfun = \(p) sum(-p * log(p))
  
  imap_dfr(models, ~ {
    error_prior = entfun(.x$P)
    error_post =
      apply(.x$posterior, 1, entfun) %>% mean(na.rm = TRUE)
    
    tibble(
      model = paste0(.y, "-class"),
      log_likelihood = .x$llik,
      bic = .x$bic,
      bic_adjusted = (-2 * .x$llik) + .x$npar * log((.x$N + 2) / 24),
      aic = .x$aic,
      aic_corrected = (-2 * .x$llik) + .x$npar * (1 + log(.x$N)),
      entropy = ((error_prior - error_post) / error_prior)
    ) %>% mutate(across(!model, ~ num(.x, digits = 2)))
  })
}

# class-conditional probabilities
probs <- \(models) {
  imap_dfr(models, \(model.x, model.y) {
    imap_dfr(model.x$probs, ~ {
      tibble(class = .x %>%
               rownames() %>%
               str_extract("\\d") %>%
               as.factor()) %>%
        mutate(
          var = .y %>% str_extract("_(\\w{2,3}\\d$)", 1) %>% as.factor(),
          prob = .x[, 2] %>% num(digits = 4),
          model = paste0(model.y, "-class")
        )
    }) %>% bind_cols(model.x$probs.se %>%
                       imap_dfr( ~ tibble(se = .x[, 2] %>% num(digits = 4))))
  }) %>% mutate(
    lower = pmax(0, prob - 1.96 * se) %>% num(digits = 4),
    upper = pmin(prob + 1.96 * se, 1) %>% num(digits = 4)
  ) %>%
    relocate(c(model, class), .after = var) %>%
    arrange(model, class)
}

# profile plots of all models
pplots <- \(probs) {
  facet_labels = imap(models, ~ {
    paste0(.y, "-class; bic = ", round(.x$bic, 2))
  }) %>% setNames(imap(models, ~ paste0(.y, "-class"))) %>% unlist()
  
  
  ggplot(probs, aes(x = var, y = prob, shape = class)) +
    facet_wrap( ~ model, labeller = as_labeller(facet_labels)) +
    geom_line(aes(colour = class, group = class), show.legend = F) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = class,
        group = class
      ),
      alpha = 0.15,
      show.legend = F
    ) +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 1, .1),
                       labels = c("0", seq(0.1, .9, 0.1), "1")) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
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
    if_else(
      is.na(models[[1]]$coeff),
      paste0("LCA: N = ", models[[1]]$N, "; no covariate"),
      paste0(
        "LCA models: N = ",
        models[[1]]$N,
        "; covariate = ",
        models[[1]]$coeff %>% rownames() %>% .[2]
      )
    ) %>%
    ggtitle()
}