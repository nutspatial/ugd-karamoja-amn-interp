de <- st_bbox(karamoja) |> 
  st_as_stars(dx = 2000) |> 
  st_crop(karamoja)

i <- idw(raw_rates$rate~1, raw_rates, de)
  
ggplot() + 
  geom_stars(data = i, aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(option = "viridis") +  # Choose a viridis palette (e.g., "plasma", "magma", "inferno")
  geom_sf(data = st_cast(karamoja, "MULTILINESTRING")) + 
  geom_sf(data = raw_rates) +
  theme_minimal()
