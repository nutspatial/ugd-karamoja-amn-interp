################################################################################
#             WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES              #
################################################################################

## ---- Filter out Karamoja districts and reproject CRS ------------------------

### ---------------------------------------------------- List of districts -----
districts <- c(
  "Abim", "Amudat", "Kotido", "Karenga", "Kaabong", "Napak",
  "Nakapiripirit", "Nabilatuk", "Moroto"
)

### ---------------------------------------------- Filter out and reproject ----
uga2_district <- st_read(
  dsn = "data-raw/uga-shp/uga_admbnda_adm2_ubos_20200824.shp"
) |>
  filter(ADM2_EN %in% districts) |>
  st_transform(crs = "EPSG:32636")

uga4_county <- st_read(
  dsn = "data-raw/uga-shp/uga_admbnda_adm4_ubos_20200824.shp"
) |>
  filter(ADM2_EN %in% districts) |>
  st_transform(crs = "EPSG:32636")


## ---- Set WFHZ data as an `sf` object  ---------------------------------------
wfhz <- wfhz_data |>
  filter(!flag_wfhz == 1) |>
  select(enum_area, x, y, gam) |>
  filter(!is.na(x)) |>
  st_as_sf(
    coords = c("x", "y"),
    dim = "XY"
  ) |>
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
aggr_wfhz <- wfhz |>
  mutate(
    long_x = st_coordinates(geometry)[, 1],
    lati_y = st_coordinates(geometry)[, 2]
  ) |>
  group_by(enum_area) |>
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    x = mean(long_x, na.rm = TRUE),
    y = mean(lati_y, na.rm = TRUE)
  ) |>
  as_tibble() |>
  select(-geometry) |>
  st_as_sf(
    coords = c("x", "y"),
    dim = "XY",
    crs = "EPSG:4326"
  ) |>
  st_transform(crs = st_crs(uga2_district))

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
    est = est * 100,
    raw = raw * 100,
    est = ifelse(est == "NaN", 0, est),
    raw_cat = cut(
      x = raw,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    ),
    sebsr_cat = cut(
      x = est,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

#### ------------------------------------------------------- Plot raw rates ----
ggplot(data = uga2_district) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = uga4_county,
    fill = NA,
    color = "#768492"
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = raw_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
    name = "Raw rates"
  ) +
  theme_void()

### ------------------------------------------------------------ Plot SEBSR ----
uga_sampling_points <- ggplot(data = uga2_district) +
  geom_sf(
    fill = "white",
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf(
    data = uga4_county,
    fill = NA,
    color = "#768492"
  ) +
  geom_sf(
    data = wrangled_wfhz,
    aes(color = sebsr_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
    name = "GAM rates"
  ) +
  theme_void()

################################ End of workflow ###############################
