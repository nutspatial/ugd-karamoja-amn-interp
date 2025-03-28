################################################################################
#             WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES              #
################################################################################

## ---- Set data as an `sf` object and reproject CRS (MUAC) --------------------

muac <- muac_data |>
  filter(!flag_mfaz == 1) |>
  select(enum_area, x, y, gam) |>
  filter(!is.na(x)) |>
  st_as_sf(
    coords = c("x", "y"),
    dim = "XY"
  ) |>
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
aggr_muac <- muac |>
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
sp_wts_muac <- aggr_muac |>
  knearneigh(
    k = 4,
    longlat = TRUE,
    use_kd_tree = TRUE
  ) |>
  knn2nb(row.names = NULL)

### ------------------------------------------------------- Calculate rates ----
sebsr_muac <- EBlocal(
  ri = aggr_muac$cases,
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
    est = est * 100,
    raw = raw * 100,
    est = ifelse(est == "NaN", 0, est),
    sebsr_cat = cut(
      x = est,
      breaks = c(-Inf, 5, 9, 14.9, Inf),
      labels = c("<5.0%", "5.0-9.0%", "10.0-14.9%", "≥15.0%"),
      include.lowest = TRUE
    ),
    raw_cat = cut(
      x = raw,
      breaks = c(-Inf, 5, 9, 14.9, Inf),
      labels = c("<5.0%", "5.0-9.0%", "10.0-14.9%", "≥15.0%"),
      include.lowest = TRUE
    )
  )

#### Plot raw rates ----
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
    data = wrangled_muac,
    aes(color = raw_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
    name = "Raw rates"
  ) +
  theme_void()

#### Plot SEBSR ----
uga_sampling_points_muac <- ggplot(data = uga2_district) +
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
    data = wrangled_muac,
    aes(color = sebsr_cat)
  ) +
  scale_color_manual(
    values = apply_ipc_colours("muac", .map_type = "static"),
    name = "GAM Rates"
  ) +
  theme_void()

################################ End of workflow ###############################
