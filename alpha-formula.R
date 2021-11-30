tbl <- 
  tibble::tribble(
    ~id,  ~poms1.0, ~poms2.0, ~poms3.0, ~poms4.0, ~poms5.0, ~poms6.0,  ~fact1.0, ~fact2.0, ~fact3.0, ~fact4.0,
    1,         0,        0,        0,       0L,        0,        0,        4L,       4L,       1L,       0L,
    2,         0,        0,        0,       0L,        0,        0,        2L,       3L,       4L,       4L,
    3,         1,        1,        1,       2L,        0,        1,        2L,       2L,       3L,       1L,
    4,         1,        0,        3,       4L,        0,        2,        1L,       3L,       4L,       4L,
    5,         4,        2,        2,       2L,        0,        0,        3L,       3L,       4L,       3L,
    6,         3,        2,        0,       0L,        3,        2,        1L,       4L,       3L,       3L,
    7,         0,        0,        1,       0L,        0,        1,        3L,       4L,       4L,       3L,
    8,         0,        0,        3,       2L,        0,        0,        2L,       4L,       2L,       4L,
    9,         1,        0,        4,       4L,        1,        0,        1L,       2L,       3L,       4L,
    10,         4,        0,        4,       2L,        2,        0,        2L,       4L,       1L,       4L
  )

# This is to collect variables that start with "poms" and "fact", and end with ".0". The .* means anything in between
reg_poms <- "^poms.*\\.0$"  
reg_fact <- "^fact.*\\.0$"

# create a vector with the strings - also name the vector
regs <- c(reg_poms, reg_fact) %>% set_names(c("poms", "fact"))

alpha_raw2 <- 
  map_df(regs, \(i)
         tbl
         |>  select(dplyr::matches(i))
         |> psych::alpha(check.keys = TRUE)
         |> (\(j) j$total)()
         # |> tibble::rownames_to_column()
         |> remove_rownames()
         ,.id = "scale"
  )

alpha_item <- 
  map_df(regs, \(i)
           tbl |>  
           select(dplyr::matches(i)) |> 
           psych::alpha(check.keys = TRUE) |> (\(j) j$item.stats)() |> 
           tibble::rownames_to_column()
         # capture the name of the regex that is now using
         ,.id = "scale"
  )

alpha_raw <- 
  map_df(regs, ~ 
           tbl %>% 
           select(dplyr::matches(.x)) %>% 
           psych::alpha(check.keys = TRUE) %>% .$total %>% 
           tibble::rownames_to_column()
         ,.id = "scale"
  )

alpha_raw2 <- 
  map_df(regs, \(i)
           tbl
           |>  select(dplyr::matches(i))
           |> psych::alpha(check.keys = TRUE)
           |> (\(j) j$total)()
           # |> tibble::rownames_to_column()
           |> remove_rownames()
         ,.id = "scale"
  )

alpha_raw2[1,]
