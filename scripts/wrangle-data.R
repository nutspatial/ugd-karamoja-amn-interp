## ---- Wrangle weight-for-height data -----------------------------------------
wfhz <- data |> 
  mutate(
    age = NA_real_,
    date = date(date),
    ChildDoB = date(ChildDoB),
    ChildOedema = ifelse(ChildOedema == "2", "n", "y")
  ) |> 
  rename(
    height = ChildHt_Height,
    weight = ChildWt, 
    muac = MUAC
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
  ) |> 
  filter(!flag_wfhz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326") |> 
  st_transform(crs = "EPSG:32636")

## ---- Wrangle MUAC data ------------------------------------------------------
muac <- data |> 
  mutate(
    age = NA_real_,
    date = date(date),
    ChildDoB = date(ChildDoB),
    ChildOedema = ifelse(ChildOedema == "2", "n", "y"),
    MUAC = round(MUAC, 0)
  ) |> 
  rename(
    muac = MUAC
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
  ) |> 
  filter(!flag_mfaz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326") |> 
  st_transform(crs = "EPSG:32636")
################################ End of workflow ###############################
