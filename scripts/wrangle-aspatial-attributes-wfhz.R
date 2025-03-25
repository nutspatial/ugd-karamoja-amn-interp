################################################################################
#                             ASPATIAL DATA WRANGLING  (WFHZ)                  #
################################################################################

## ---- Wrangle weight-for-height data -----------------------------------------
wfhz_data <- nut_data |> 
  clean_names() |> 
  select(-c(parish, flag_who, whz_who, team, id, hh)) |> 
  mutate(
    age = NA_real_,
    date = date(date),
    child_do_b = date(child_do_b),
    child_oedema = ifelse(child_oedema == "2", "n", "y")
  ) |>
  rename(
    height = child_ht_height,
    weight = child_wt,
    district = adm2_en
  ) |>
  mw_wrangle_age(
    dos = date,
    dob = child_do_b,
    age = age,
    .decimals = 2
  ) |>
  mw_wrangle_wfhz(
    sex = child_sex,
    weight = weight,
    height = height,
    .recode_sex = FALSE,
    .decimals = 3
  ) |>
  define_wasting(
    zscores = wfhz,
    edema = child_oedema,
    .by = "zscores"
  )


################################ End of workflow ###############################
