library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2021-01-19")

tmp_file <- tempfile()

download.file(
  "https://github.com/Shelmith-Kariuki/rKenyaCensus/raw/master/data/V4_T2.29.rda",
  tmp_file)
  
x <- load(tmp_file)

kenya_street_pop <- get(x) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

kenya_street_pop_totals <- kenya_street_pop[1,] %>%
  as.list()

street_pop_counties <- kenya_street_pop %>%
  filter(county != "KENYA" & county != "Rural" & county != "Urban") %>%
  mutate(county = str_to_title(county)) %>%
  arrange(desc(total)) %>%
  drop_na(total) %>%
  mutate(county = fct_reorder(county, -total),
         female = replace_na(female, 1),
         log_tot = log10(male) + log10(female)) %>%
  pivot_longer(cols = c(male, female), names_to = "gender") %>%
  mutate(gender = toupper(gender),
         gender = factor(gender, levels = c("MALE", "FEMALE")))

street_pop_county_labels <- street_pop_counties %>%
  distinct(county, total, log_tot) %>%
  arrange(county) %>%
  mutate(angle = ((row_number()/nrow(.)*360) - (1/nrow(.)*360)/2) + 90/pi,
         angle = if_else(angle > 360, angle - 360, angle),
         text_angle = if_else(angle >= 180, 270 - angle, 90 - angle),
         hjust = if_else(angle > 179, 1, 0),
         label = paste0(toupper(county), ": ", scales::comma(total, 1)))


ggplot(street_pop_counties, aes(x = county, y = log10(value), fill = gender)) +
  geom_col(colour = "#000000", size = 0.25) +
  geom_text(data = street_pop_county_labels, 
            aes(y = log_tot + 0.2, x = county, angle = text_angle, 
                label = label, hjust = hjust), 
            size = 3, colour = "#ffffff", fontface = "bold",
            inherit.aes = FALSE) +
  scale_fill_manual(values = c("MALE" = "#BE3A34", "FEMALE" = "#009A44")) +
  coord_polar(start = 0.5, clip = "on") +
  guides(fill = guide_legend(title = NULL, label.position = "left")) +
  labs(
    title = "Street Population in\nKenyan Counties",
    subtitle = paste("Source: Kenya Population and Household Census, 2019",
                    "Kenya National Bureau of Statistics", sep = "\n"),
    caption = paste("@mattkerlogue for Tidy Tuesday 2021-01-19",
                    "https://github.com/mattkerlogue/tidytuesday", sep = "\n")
  ) +
  theme_void() +
  theme(
    text = element_text(size = 10, colour = "#ffffff", face = "bold"),
    plot.title = element_text(family = "Charvet Heavy", size = 24),
    plot.subtitle = element_text(face = "plain",
                                 margin = margin(t = 12, r = 0, b = 0, l = 0, unit = "pt")),
    plot.caption = element_text(family = "Hack", face = "plain", size = 8, lineheight = 1.2),
    plot.background = element_rect(fill = "#000000"),
    plot.margin = unit(rep(0.5,4), units = "cm"),
    legend.position = c(0.1, 0.1)
  )

## DESIGN CHOICES
## 
## COLOUR
## Colours are taken from Kenyan Flag as per 
## https://flagcolor.com/kenya-flag-colors/
## Black: #000000
## Green: #009A44
## Red:   #BE3A34
## 
## TITLE FONT: Charvet
## http://www.thisisthenest.com/charveta-typeface-2013
## In 2013, we commissioned Kenyan graphic artist Kevin Karanja to create 
## a typeface. Charvet was the result—a typeface borne of Kevin's love 
## for Ancient African typography and a love for geometrics.