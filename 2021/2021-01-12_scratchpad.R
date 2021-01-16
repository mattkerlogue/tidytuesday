library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2021-01-12")

View(tuesdata$artists)

credit_first_unique <- tuesdata$artwork$creditLine %>%
  str_remove("\\s.*") %>%
  unique() %>%
  sort()

credit_first_unique

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^\\[")) %>%
  select(creditLine)
# 2 works discovered during remounting of other work

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Accepted")) %>%
  distinct(creditLine) %>%
  pull(creditLine)
# Turner bequest & gifts in lieu of tax

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^ARTIST")) %>%
  distinct(creditLine) %>%
  pull(creditLine)
# ARTIST ROOMS programme

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Commissioned")) %>%
  pull(creditLine)
# 2 works, 1 commissioned by a will, 1 presented as gift

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Exchanged")) %>%
  pull(creditLine)
# 1 works

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Gift")) %>%
  pull(creditLine)
# 3 works from other collections/societies

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Given")) %>%
  pull(creditLine)
# 1 work given in memory of deceased

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Offered")) %>%
  pull(creditLine)
# 1 work offered in lieu of tax

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Partial")) %>%
  pull(creditLine)
# 1 work part purchase/part gift

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Presented")) %>%
  distinct(creditLine) %>%
  view()
# gifts from individual persons, name and year

tuesdata$artwork %>%
  filter(str_detect(tolower(creditLine), "^presented.*(bequest|widow|\\blate\\b)")) %>%
  distinct(creditLine) %>%
  view()
# gifts in relation to a death

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Purchased"))
# Purchases

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Purchased")) %>%
  filter(str_detect(creditLine, "^Purchased.*(assistance|asssistance|asistance|funds provided|Art Fund|courtesy|Legacy|contributions|available)")) %>%
  select(creditLine)
# 4,968 purchases with assistance of grants/members/donations

tuesdata$artwork %>%
  filter(str_detect(creditLine, "^Transferred")) %>%
  distinct(creditLine) %>%
  pull(creditLine)
# Transfers from other museums

tuesdata$artwork %>%
  filter(str_detect(creditLine, "(Tate (Members|Patrons))|(Tate Photography Patrons)"))
# 494 works acquired by Tate Members/Patrons

tuesdata$artwork %>%
  filter(str_detect(creditLine, "Art Fund")) %>%
  distinct(creditLine) %>%
  view()
