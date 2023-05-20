
library(tidyverse)
library(sf)

# Load the LSOA shapefile
lsoa <- st_read("LSOA_2021_EW_BFC_V7.shp")

# Merge the income data with the LSOA shapefile
index_data = df %>% filter(year == 2019) %>%
  select(year, lsoa11, samhi_index)
index_data <- index_data %>% left_join(lsoa, by = c("lsoa11" = "LSOA21CD")) %>%
  na.omit()%>%
  st_as_sf()

# Plot the income values on a map
ggplot() +
  geom_sf(data = index_data, aes(fill = samhi_index)) +
  scale_fill_gradient(low = "red", high = "darkgreen") +
  theme_void()


