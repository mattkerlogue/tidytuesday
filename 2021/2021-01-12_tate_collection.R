library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2021-01-12")

why_work <- tuesdata$artwork %>%
  mutate(
    creditLine = tolower(creditLine),
    reason = case_when(
      str_detect(creditLine, "(tate (members|patrons)|friends of the tate)") ~ "members",
      str_detect(creditLine, "art fund") ~ "art fund",
      str_detect(creditLine, "contemporary art society") ~ "cas",
      str_detect(creditLine, "^artist rooms") ~ "artist rooms",
      str_detect(creditLine, "^\\[") ~ "other",
      str_detect(creditLine, "^accepted.*turner") ~ "turner",
      str_detect(creditLine, "^accepted.*tax") ~ "tax",
      str_detect(creditLine, "^acquired") ~ "gift",
      str_detect(creditLine, "^bequeathed") ~ "death",
      str_detect(creditLine, "^commissioned.*will") ~ "death",
      str_detect(creditLine, "^commissioned") ~ "gift",
      str_detect(creditLine, "^exchanged") ~ "other",
      str_detect(creditLine, "^gift") ~ "gift",
      str_detect(creditLine, "^given") ~ "death",
      str_detect(creditLine, "^offered") ~ "tax",
      str_detect(creditLine, "^partial") ~ "gift",
      str_detect(creditLine, "^presented.*bequest") ~ "death",
      str_detect(creditLine, "^presented.*\\blate\\b") ~ "death",
      str_detect(creditLine, "^presented.*widow") ~ "death",
      str_detect(creditLine, "^presented.*in memory") ~ "death",
      str_detect(creditLine, "^presented.*memorial") ~ "death",
      str_detect(creditLine, "^presented.*wish") ~ "death",
      str_detect(creditLine, "^presented") ~ "gift",
      str_detect(creditLine, "^presented.*wish") ~ "death",
      str_detect(creditLine, "^purchase.*legacy") ~ "death",
      str_detect(
          creditLine,
          "^purchased.*(assistance|asssistance|asistance|funds|provided|courtesy|contributions|available)") ~
        "supported purchase",
      str_detect(creditLine, "^purchased") ~ "purchase",
      str_detect(creditLine, "^transferred.*(library|archive|reference collection)") ~ "transfer, internal",
      str_detect(creditLine, "^transferred") ~ "transfer, external",
      TRUE ~ NA_character_
    ),
    short_reason = case_when(
      str_detect(reason, "gift|^purchase|death|turner") ~ reason,
      is.na(reason) ~ NA_character_,
      TRUE ~ "other"
    )
  )

why_year <- why_work %>%
  count(reason, year) %>%
  mutate(decade = str_sub(year, 3, 3),
         reason = str_wrap(reason, 10)) %>%
  drop_na() %>%
  filter(reason != "other")


why_year_reasons <- why_year %>%
  distinct(reason) %>%
  mutate(angle = (row_number()/nrow(.)*360) - (1/nrow(.)*360)/2,
         text_angle = case_when(
           angle < 180 ~ 90 - angle,
           angle > 180 ~ 270 - angle),
         hj = if_else(angle > 180, 1, 0))

p_labs <- ggplot(why_year, aes(x = reason, y  = year, size = log(n), colour = decade)) +
  geom_point(alpha = 0) +
  geom_hline(yintercept = c(17:20) * 100, colour = "#ffffff33", size = 2) +
  geom_path(alpha = 0.5, position = "jitter") +
  geom_vline(xintercept = (0:11) + 0.5, colour = "#000000", size = 2) +
  geom_text(data = why_year_reasons,
            mapping = aes(x = reason, y = 2050, label = reason,
                          angle = text_angle, hjust = hj),
            family = "Blackout Midnight",
            colour = "#ffffffaa",
            inherit.aes =  FALSE) +
  ylim(1500,2100) +
  coord_polar() +
  scale_colour_viridis_d(option = "inferno") +
  scale_size(guide = guide_none()) +
  labs(
    title = "Tate Collection by \nreason of acquisition",
    caption = "Matt Kerlogue, 2021\nhttps://github.com/mattkerlogue/tidytuesday"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Hack", colour = "#ffffffaa", lineheight = 1.1),
    title = element_text(family = "Blackout Sunrise", size = 24),
    plot.caption = element_text(family = "Hack", size = 8),
    legend.position = "none",
    panel.background = element_rect(fill = "#000000"),
    plot.background = element_rect(fill = "#000000"),
    plot.margin = unit(rep(0.5,4), units = "cm")
  )

p_labs

ggsave("2021/2021-01-12_tate_reasons_art_labelled.png", p_labs, width = 20, height = 25, units = "cm")


