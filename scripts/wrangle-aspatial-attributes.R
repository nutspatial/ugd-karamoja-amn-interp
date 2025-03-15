################################################################################
#                             ASPATIAL DATA WRANGLING                          #
################################################################################

## ---- Wrangle weight-for-height data -----------------------------------------
wfhz_data <- input_data |> 
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

## ---- Wrangle MUAC data ------------------------------------------------------
muac_data <- input_data |> 
  clean_names() |> 
  select(-c(parish, flag_who, whz_who, team, id, hh)) |> 
  mutate(
    age = NA_real_,
    date = date(date),
    child_do_b = date(child_do_b),
    child_oedema = ifelse(child_oedema == "2", "n", "y"),
    muac = round(muac, 0)
  ) |>
  rename(
    muac = muac,
    district = adm2_en
  ) |>
  mw_wrangle_age(
    dos = date,
    dob = child_do_b,
    age = age,
    .decimals = 2
  ) |>
  mw_wrangle_muac(
    sex = child_sex,
    muac = muac,
    age = age,
    .recode_sex = FALSE,
    .recode_muac = TRUE,
    .decimals = 3
  ) |>
  mutate(muac = recode_muac(muac, .to = "mm")) |>
  define_wasting(
    muac = muac,
    edema = child_oedema,
    .by = "muac"
  )
################################ End of workflow ###############################
