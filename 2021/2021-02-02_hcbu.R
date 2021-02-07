library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2021-02-02")

reformat_data <- function(data) {

  x <- data %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = -`Total`, names_to = "var") %>%
    rename(year = `Total`) %>%
    mutate(type = if_else(str_detect(var, "^Standard Errors"), "se", "value"),
           group = case_when(
             str_detect(var, "White") ~ "white",
             str_detect(var, "Black") ~ "black",
             str_detect(var, "Hispanic") ~ "hispanic",
             str_detect(var, "Asian/Pacific Islander$") ~ "asian_pacific",
             str_detect(var, "Asian$") ~ "asian",
             str_detect(var, "Pacific Islander$") ~ "pacific",
             str_detect(var, "Native") ~ "native_american",
             str_detect(var, "Two") ~ "mutli",
             str_detect(var, "Total, percent") ~ "all",
           ),
           year = str_replace(year, "^(19\\d{2})\\d$", "\\1"),
           year = as.numeric(year),
           value_num = as.numeric(value)) %>%
    select(-var, -value) %>%
    drop_na(year) %>%
    pivot_wider(names_from = type, values_from = value_num)

  return(x)

}

all_hs <- reformat_data(tuesdata$hs_students) %>%
  mutate(sex = "all", level = "hs")

male_hs <- reformat_data(tuesdata$male_hs_students) %>%
  mutate(sex = "male", level = "hs")

female_hs <- reformat_data(tuesdata$female_hs_students) %>%
  mutate(sex = "female", level = "hs")

all_bach <- reformat_data(tuesdata$bach_students) %>%
  mutate(sex = "all", level = "bach")

male_bach <- reformat_data(tuesdata$male_bach_students) %>%
  mutate(sex = "male", level = "bach")

female_bach <- reformat_data(tuesdata$female_bach_students) %>%
  mutate(sex = "female", level = "bach")


all_attainment <- bind_rows(all_hs, male_hs, female_hs, all_bach, male_bach, female_bach)

attainment_gap <- all_attainment %>%
  filter(group == "all" | group == "white" | group == "black")

# 1910, 1920, 1930 do not have data split by group
attainment_gap %>%
  drop_na(value) %>%
  count(year, sex, level) %>%
  filter(n != 3)

attainment_gap <- all_attainment %>%
  filter(year > 1939) %>%
  filter(group == "all" | group == "white" | group == "black") %>%
  select(-se) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(
    white_diff = white - all,
    black_diff = black - all
  ) %>%
  select(year, sex, level, all, white_diff, black_diff) %>%
  pivot_longer(cols = c(white_diff, black_diff),
               names_to = "group",
               values_to = "diff") %>%
  mutate(group = str_remove(group, "_diff"))


overall_gap <- attainment_gap %>%
  filter(sex == "all") %>%
  mutate(gap_min = if_else(diff < 0, all + diff, all),
         gap_max = if_else(diff > 0, all + diff, all),
         viz_group = paste(level, group, sep = "_"))

overall_attainment <- all_attainment %>%
  filter(sex == "all", group == "all") %>%
  select(year, level, value)

colour_vals <- c(
  "hs" = "#40004b",
  "hs_white" = "#c2a5cf",
  "hs_black" = "#762a83",
  "bach" = "#00441b",
  "bach_white" = "#a6dba0",
  "bach_black" = "#1b7837"
)

x_lines <- tibble(
  x = seq(1920, 2010, 10),
  ymin = 0,
  ymax = 100
)

y_lines <- tibble(
  y = seq(10, 90, 10),
  xmin = 1910,
  xmax = 2016
)

open_purple_span <- paste0("<span style=\"color:", colour_vals[1], "\">")

open_green_span <- paste0("<span style=\"color:", colour_vals[4], "\">")

label_text <- c(
  paste0(
    "The attainment gap between black and white high school graduates",
    "has reduced from 18.4 percentage points in 1940 to **",
    open_purple_span, "6.1 percentage points in 2016</span>**."
  ),
  paste0(
    "The attainment gap between black and white college graduates has ",
    "increased from 3.6 percentage points in 1940 to **", open_green_span,
    "13.8 percentage points in 2016</span>**.<br /><br />",
    "However, in 1940 only 4.9% of white people and 1.3% of black people ",
    "were college graduates, while in 2016 some 37.3% of white people and 23.5% ",
    "of black people are college gaduates. Meaning that while white people are ",
    "7.6 times more likely to be college graduates in 2016 than in 1940, **",
    open_green_span, "black people are 18.1 times more likely to be college ",
    "graduates in 2016 than in 1940</span>**."
  )
)

label_df <- tibble(
  label = label_text,
  x = 2018,
  y = c(90, 35),
  box.fill = c("#e7d4e8", "#d9f0d3")
)

ggplot(overall_gap, aes(x = year, colour = viz_group)) +
  geom_rect(data = NULL, aes(xmin = 1910, xmax = 2016, ymin = 0, ymax = 100),
            fill = "grey95", colour = NA,
            inherit.aes = FALSE) +
  geom_linerange(data = x_lines, aes(x = x, ymin = ymin, ymax = ymax),
                 inherit.aes = FALSE, size = 1, colour = "white") +
  geom_linerange(data = y_lines, aes(y = y, xmin = xmin, xmax = xmax),
                 inherit.aes = FALSE, size = 1, colour = "white") +
  geom_linerange(aes(ymin = gap_min, ymax = gap_max), size = 1) +
  geom_line(data = overall_attainment, aes(x = year, y = value, colour = level),
            inherit.aes = FALSE, size = 0.3) +
  annotate("text", x = 2018, y = 93,
           label = "High School graduates", hjust = 0, size = 3,
           colour = colour_vals[1], fontface = "bold",
           family = "Junction")+
  annotate("text", x = 2018, y = 38,
           label = "College graduates", hjust = 0, size = 3,
           colour = colour_vals[4], fontface = "bold",
           family = "Junction") +
  ggtext::geom_textbox(data = label_df,
                       aes(x = x, y = y, label = label, fill = I(box.fill)),
                       width = unit(0.4, "npc"),
                       size = 3,
                       hjust = 0,
                       vjust = 1,
                       box.padding = unit(6, "pt"),
                       box.color = "#ffffff",
                       box.size = 0,
                       box.r = unit(0, "mm"),
                       family = "Junction",
                       inherit.aes = FALSE) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "High School and College Graduates in the US, 1910-2016",
    subtitle = "Highlighting the changing attainment gap between white and black Americans aged 25 and over since 1940"
  ) +
  scale_colour_manual(values = colour_vals) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    labels = scales::percent_format(accuracy = 1, scale = 1)) +
  scale_x_continuous(
    breaks = c(1910, seq(1940, 2010, 10), 2016),
    limits = c(1910, 2100)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Junction"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, colour = "grey40"),
    axis.text.y = element_text(colour = "grey40"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 25, family = "Teko"),
  )
