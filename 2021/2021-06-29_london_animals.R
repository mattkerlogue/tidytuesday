library(tidyverse)

# get tidy tuesday data
tuesdata <- tidytuesdayR::tt_load('2021-06-29')

# unify references to cats
animal_rescues <- tuesdata$animal_rescues %>%
  mutate(
    animal_group_parent = if_else(animal_group_parent == "cat", "Cat", animal_group_parent)
  )

# select just the ward and animal info
# combine low frequencies
# unify ward names (to make count() work correctly)
# count animals by ward
# group frequencies
animal_rescues_wards <- animal_rescues %>%
  select(ward_code, ward, animal_group_parent) %>%
  mutate(
    animal = fct_collapse(animal_group_parent, "Cat" = "Cat", "Bird" = "Bird", "Dog" = "Dog",
                          "Fox" = "Fox", "Horse/Deer" = c("Horse", "Deer"), other_level = "Other"),
    animal = fct_infreq(animal),
    animal = fct_relevel(animal, "Other", after = Inf),
    ward = str_to_title(tolower(ward))
  ) %>%
  count(ward_code, ward, animal) %>%
  add_count(ward_code, wt = n, name = "total") %>%
  mutate(
    bin_n = case_when(
      n < 5 ~ "Less than 5",
      n < 10 ~ "5 to 9",
      n < 15 ~ "10 to 14",
      !is.na(n) ~ "15 or more"
    ),
    bin_n = factor(bin_n, levels = c("Less than 5", "5 to 9", "10 to 14",
                                     "15 or more"), ordered = TRUE)
  )

# get boundaries for wards and regions
wards <- sf::st_read("https://opendata.arcgis.com/datasets/60ea78fd4f9d47099adfa63c2ccbc8bf_0.geojson")
regions <- sf::st_read("https://opendata.arcgis.com/datasets/cfa25518ddd7408a8da5c27eb42dd428_0.geojson")

# get london's outline
london <- regions %>%
  filter(RGN20NM == "London")

# get the wards that are within london (a small number of rescues happen outside)
# merge the animal counts
# drop wards with no counts
animal_wards_london <- wards %>%
  filter(sf::st_within(wards, london, sparse = FALSE)) %>%
  left_join(animal_rescues_wards, by = c("WD20CD" = "ward_code")) %>%
  drop_na(animal)

# check total counts
animal_wards_london %>% count(animal)

# create facet labeller
animal_labels <- as_labeller(c(
  "Cat" = "Cat (628)",
  "Bird" = "Bird (530)",
  "Dog" = "Dog (481)",
  "Fox" = "Fox (253)",
  "Horse/Deer" = "Horse or Deer (119)",
  "Other" = "Other animals (318)"
))

# build plot
# use Baloo 2 font by EK Type https://github.com/EkType/Baloo2
ggplot(animal_wards_london) +
  geom_sf(aes(fill = bin_n), colour = "#ffffff", size = 0.05) +
  geom_sf(data = london, fill = NA, colour = "#cccccc", size = 0.25) +
  scale_fill_brewer(palette = "PuBuGn") +
  facet_wrap("animal", labeller = animal_labels) +
  theme_void() +
  labs(
    title = "Animal rescues in London by ward",
    subtitle = "Animal rescues 2009-2021 within Greater London by the London Fire Bridge, by ward",
    caption = paste0("Source: London Fire Bridge, 2021 (data); Office for National Statistics and Ordnance Survey 2021 (boundaries)\n",
                     "Visualisation: Matt Kerlogue, @mattkerlogue | ",
                     "http://github.com/mattkerlogue/tidytuesday")
  ) +
  theme(
    text = element_text(colour = "#ffffff", family = "Baloo 2"),
    plot.background = element_rect(fill = "#333333", colour = NA),
    plot.margin = margin(t = 10, l = 5, r = 5, b = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(10, "pt"),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 24, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold", margin = margin(t = 1, b = 1))
  )
