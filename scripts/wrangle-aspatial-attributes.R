## ---- Wrangle weight-for-height data -----------------------------------------
wfhz_data <- data |> 
  mutate(
    age = NA_real_,
    date = date(date),
    ChildDoB = date(ChildDoB),
    ChildOedema = ifelse(ChildOedema == "2", "n", "y")
  ) |> 
  rename(
    height = ChildHt_Height,
    weight = ChildWt, 
    district = ADM2_EN
  ) |> 
  mw_wrangle_age(
    dos = date,
    dob = ChildDoB,
    age = age, 
    .decimals = 2
  ) |> 
  mw_wrangle_wfhz(
    sex = ChildSex, 
    weight = weight, 
    height = height, 
    .recode_sex = FALSE, 
    .decimals = 3
  ) |> 
  define_wasting(
    zscores = wfhz,
    edema = ChildOedema,
    .by = "zscores"
  )

## ---- Wrangle MUAC data ------------------------------------------------------
muac_data <- data |> 
  mutate(
    age = NA_real_,
    date = date(date),
    ChildDoB = date(ChildDoB),
    ChildOedema = ifelse(ChildOedema == "2", "n", "y"),
    MUAC = round(MUAC, 0)
  ) |> 
  rename(
    muac = MUAC, 
    district = ADM2_EN
  ) |> 
  mw_wrangle_age(
    dos = date,
    dob = ChildDoB,
    age = age, 
    .decimals = 2
  ) |> 
  mw_wrangle_muac(
    sex = ChildSex,
    muac = muac,
    age = age,
    .recode_sex = FALSE,
    .recode_muac = TRUE,
    .decimals = 3
  ) |> 
  mutate(muac = recode_muac(muac, .to = "mm")) |> 
  define_wasting(
    muac = muac,
    edema = ChildOedema, 
    .by = "muac"
  )
################################ End of workflow ###############################
