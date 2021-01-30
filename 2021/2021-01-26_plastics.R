library(tidyverse)
library(patchwork)

# GET RAW DATA
# https://drive.google.com/drive/folders/1mdIsoaj5vW368YWw7-vD2hDFANqaS_Lh
# downloaded to /data/2021-01-26_bfpp_raw.zip
# extracted tp sub-folder of the same name /data folder

# read csv files
plastic_in <- map_dfr(dir("data/2021-01-26_bfpp-raw/",
                 pattern = ".csv",
                 full.names = TRUE),
             read_csv,
             col_types = cols(.default = col_character()))

# clean data
plastic_2020 <- plastic_in %>%
  janitor::clean_names() %>%
  mutate(
    across(c(-country, -parent_company), ~if_else(. == "null", "0", .)),
    across(c(-country, -parent_company), as.numeric),
    iso_a3 = countrycode::countrycode(country, "country.name", "iso3c"),
    parent_company = if_else(
      str_detect(parent_company, "^Unbranded$|^null$|^NULL$|#ERROR!$"),
      "No brand",
      parent_company),
    parent_company = str_replace(parent_company, "International$", "Intl."),
    parent_company = str_squish(str_replace_all(parent_company, "[^[:alnum:]]", " ")))

# get a dataset of just countries, companies and total items
country_companies_2020 <- plastic_2020 %>%
  count(parent_company, iso_a3, wt = grand_total, name = "num_items") %>%
  add_count(parent_company, wt = num_items, name = "company_total") %>%
  group_by(parent_company) %>%
  add_count(parent_company, name = "num_countries") %>%
  ungroup()

# get data on just the companies
companies_2020 <- country_companies_2020 %>%
  distinct(parent_company, company_total, num_countries) %>%
  arrange(desc(num_countries), desc(company_total))

# get the top 9 companies
# 1st row is for unbranded items
top9_companies <- companies_2020$parent_company[2:10]

# get the data for the top 9
top9_data <- country_companies_2020 %>%
  filter(parent_company %in% top9_companies) %>%
  mutate(parent_company  = fct_reorder(parent_company, -num_countries))

# get country sf data
countries <- rnaturalearth::ne_countries(returnclass = "sf")

# transform to polar projection, exclude antarctica
countries_polar <- sf::st_transform(countries, crs = "+proj=aeqd +lat_0=90 +lon_0=0") %>%
  filter(iso_a3 != "ATA")

# subset countries to just those in the audit
countries_covered <- countries_polar %>%
  filter(iso_a3 %in% unique(country_companies_2020$iso_a3))

# join top 9 data to country sf data
countries_data <- countries_polar %>%
  filter(iso_a3 != "ATA") %>%
  inner_join(select(top9_data, iso_a3, parent_company),
             by = c("iso_a3" = "iso_a3"))

# create lat/long lines that reflect UN flag
x <- sf::st_graticule(lon = seq(-180, 180, 45), lat = seq(-60, 90, 30)[1:5])
y <- sf::st_crop(x, c(xmin = -180, xmax = 180, ymin = -60, ymax = 60))

# create a theme for the plots
plot_theme <- theme(
  text = element_text(
    colour = "#ffffff",
    margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")),
  plot.margin = unit(c(5, 5, 5, 5), units = "pt"),
  plot.background = element_rect(fill = "#397bce", colour = "#397bce"),
  plot.title = element_text(
    family = "Ostrich Sans Black",
    size = 42),
  plot.subtitle = element_text(
    family = "Ostrich Sans Black",
    size = 20,
    margin = margin(t = 5, r = 0, b = 12, l = 0, unit = "pt")),
  plot.caption = element_text(
    family = "Hack"),
  strip.text = element_text(
    family = "Ostrich Sans Black",
    size = 12)
  )

# plot maps for the top 9
top9_plot <- ggplot() +
  geom_sf(data = y, colour = "#ffffff", size = 0.5) +
  geom_sf(data = countries_polar, fill = "#397bce", colour = "#397bce", size = 0.2) +
  geom_sf(data = countries_data, fill = "#ffffff", colour = "#ffffff", size = 0.2) +
  coord_sf(crs = "+proj=aeqd +lat_0=90 +lon_0=0") +
  facet_wrap(~parent_company) +
  theme_void() +
  plot_theme

# plot overal coverage map
main_plot <- ggplot() +
  geom_sf(data = y, colour = "#ffffff", size = 2.5) +
  geom_sf(data = countries_polar, fill = "#397bce", colour = "#397bce", size = 0.2) +
  geom_sf(data = countries_polar, fill = "#ffffff33", colour = "#ffffff33", size = 0) +
  geom_sf(data = countries_covered, fill = "#ffffff", colour = "#ffffff", size = 0.2) +
  coord_sf(crs = "+proj=aeqd +lat_0=90 +lon_0=0") +
  theme_void() +
  plot_theme

# combine plots using {patchwork} and add titles & themes
patch_plot <- main_plot + top9_plot + plot_annotation(
  title = "United Nations of Plastic",
  subtitle = "Break Free From Plastic Brand Audit Report 2020\n55 countries + 575 audits + 14,734 volunteers = 346,494 items ... 47,078 from just 9 companies",
  caption = paste0("Source: Break Free From Plastic, 2020\n",
                   "Visualisation: Matt Kerlogue, @mattkerlogue\n",
                   "http://github.com/mattkerlogue/tidytuesday"),
  theme = plot_theme)

patch_plot


## DESIGN CHOICES
##
## COLOUR
## Background colour is the blue of the UN flag, identified from the official
## image of the flag published by the UN at https://research.un.org/en/maps/flags
## UN blue: #397bce
##
## TITLE FONT: Ostrich Sans
## Designed by Tyler Finck for the League of Moveable Type
## https://www.theleagueofmoveabletype.com/ostrich-sans
