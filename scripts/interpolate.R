de <- st_bbox(karamoja_admn3) |> 
  st_as_stars(dx = 2000) |> 
  st_crop(karamoja_admn3)

i <- idw(wrangled_wfhz$est~1, wrangled_wfhz, de)
  
ggplot() + 
  geom_stars(data = i, aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(option = "viridis") +  # Choose a viridis palette (e.g., "plasma", "magma", "inferno")
  geom_sf(data = st_cast(karamoja_admn3, "MULTILINESTRING")) + 
  theme_minimal()
