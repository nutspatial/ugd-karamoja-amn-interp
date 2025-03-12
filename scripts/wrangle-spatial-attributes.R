################################################################################
#             WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES              #
################################################################################


## ---- Set WFHZ data as an `sf` object  ---------------------------------------
wfhz <- wfhz_data |> 
  filter(!flag_wfhz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
aggr_wfhz <- wfhz |> 
  mutate(
    long_x = st_coordinates(geometry)[, 1], 
    lati_y = st_coordinates(geometry)[, 2]
  ) |>  
  group_by(enumArea) |> 
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    X = mean(long_x, na.rm = TRUE),
    Y = mean(lati_y, na.rm = TRUE)
  ) |> 
  as_tibble() |> 
  select(-geometry) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY", 
    crs = "EPSG:4326"
  ) |> 
  st_transform(crs = st_crs(karamoja_admn3))

### -------------------------- Calculate spatial weights: K-Near Neighbours ----
sp_wts_wfhz <- aggr_wfhz |> 
  knearneigh(
    k = 4,
    longlat = TRUE,
    use_kd_tree = TRUE
  ) |> 
  knn2nb(row.names = NULL)

### ------------------------------------------------------- Calculate rates ----
sebsr_wfhz <- EBlocal(
  ri = aggr_wfhz$cases,
  ni = aggr_wfhz$pop,
  nb = sp_wts_wfhz
)

#### Bind data.frames -----
wrangled_wfhz <- cbind(aggr_wfhz, sebsr_wfhz)

## ---- Map rates --------------------------------------------------------------
### ----------------- Create a categorical variable with custom breakpoints ----
wrangled_wfhz <- wrangled_wfhz |> 
  mutate(
    est = ifelse(est == "NaN", 0, est),
    raw_cat = cut(
      x = raw, 
      breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
      labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
      include.lowest = TRUE
    ), 
    sebsr_cat = cut(
      x = est, 
      breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
      labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
      include.lowest = TRUE
    )
  )

### ----------------------------------------------------------- Color-codes ----
custom_colors <- c(
  "<0.05" = "#40E0D0",  
  "0.05-0.09" = "#DFFF00",
  "0.10-0.149" = "#FFBF00",
  "0.15-0.299" = "#FF7F50",
  "≥0.3" = "#f03b20"
)

#### ------------------------------------------------------- Plot raw rates ----
ggplot(data = karamoja_admn3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = karamoja_admn4, 
    fill = NA, 
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = raw_cat)
  ) +
  scale_color_manual(
    values = custom_colors, 
    name = "Raw rates" 
  ) +
  theme_void() +
  labs(
    title = "Spatial distribution GAM rates by sampling points across the Karamoja region",
    subtitle = "Raw rates: cases / total number of children surveyed"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

### ------------------------------------------------------------ Plot SEBSR ----
ggplot(data = karamoja_admn3) +
  geom_sf(
    fill = "white", 
    color = "#3F4342", 
    size = 0.8
  ) +
  geom_sf(
    data = karamoja_admn4, 
    fill = NA, 
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = sebsr_cat)
  ) +
  scale_color_manual(
    values = custom_colors, 
    name = "Smoothed rates"
  ) +
  theme_void() + 
  labs(
    title = "Spatial distribution of smoothed GAM rates by sampling points across the Karamoja region",
    subtitle = "Rates smoothed using Spatial Empirical Bayesian"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

## ---- Set data as an `sf` object and reproject CRS (MUAC) --------------------
muac <- muac_data |> 
  filter(!flag_mfaz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
aggr_muac <- muac |> 
  mutate(
    long_x = st_coordinates(geometry)[, 1], 
    lati_y = st_coordinates(geometry)[, 2]
  ) |>  
  group_by(enumArea) |> 
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    X = mean(long_x, na.rm = TRUE),
    Y = mean(lati_y, na.rm = TRUE)
  ) |> 
  as_tibble() |> 
  select(-geometry) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY", 
    crs = "EPSG:4326"
  ) |> 
  st_transform(crs = st_crs(karamoja_admn3))

### -------------------------- Calculate spatial weights: K-Near Neighbours ----
sp_wts_muac <- aggr_muac |> 
  knearneigh(
    k = 4, 
    longlat = TRUE,
    use_kd_tree = TRUE
  ) |> 
  knn2nb(row.names = NULL)

### ------------------------------------------------------- Calculate rates ----
sebsr_muac <- EBlocal(
  ri = aggr_muac$cases ,
  ni = aggr_muac$pop,
  nb = sp_wts_muac
)

#### Bind data.frames -----
wrangled_muac <- cbind(aggr_muac, sebsr_muac)

## ---- Map rates --------------------------------------------------------------
### --------------------------------------------------------- Map raw rates ----

#### Create a categorical variable with custom breakpoints ----
wrangled_muac <- wrangled_muac |> 
  mutate(
    est = ifelse(est == "NaN", 0, est),
    sebsr_cat = cut(
      x = est,
      breaks = c(-Inf, 0.05, 0.09, 0.149, Inf),
      labels = c("<0.05", "0.05-0.09", "0.10-0.149", "≥0.15"),
      include.lowest = TRUE
    ), 
    raw_cat = cut(
      x = raw, 
      breaks = c(-Inf, 0.05, 0.09, 0.149, Inf),
      labels = c("<0.05", "0.05-0.09", "0.10-0.149", "≥0.15"),
      include.lowest = TRUE
    )
  )

#### Color-codes ----
custom_colors <- c(
  "<0.05" = "#40E0D0",  
  "0.05-0.09" = "#DFFF00",
  "0.10-0.149" = "#FFBF00",
  "≥0.15" = "#f03b20"
)

#### Plot raw rates ----
ggplot(data = karamoja_admn3) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = karamoja_admn4, 
    fill = NA, 
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = wrangled_muac, 
    aes(color = raw_cat)
  ) +
  scale_color_manual(
    values = custom_colors, 
    name = "Raw rates" 
  ) +
  theme_void() +
  labs(
    title = "Spatial distribution GAM by MUAC rates by sampling points across the Karamoja region",
    subtitle = "Raw rates: cases / total number of children surveyed"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

#### Plot SEBSR ----
ggplot(data = karamoja_admn3) +
  geom_sf(
    fill = "white", 
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = karamoja_admn4, 
    fill = NA, 
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = wrangled_muac, 
    aes(color = sebsr_cat)
  ) +
  scale_color_manual(
    values = custom_colors, 
    name = "Smoothed rates"
  ) +
  theme_void() + 
  labs(
    title = "Spatial distribution of smoothed GAM rates by sampling points across the Karamoja region",
    subtitle = "Rates smoothed using Spatial Empirical Bayesian"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

################################ End of workflow ###############################
