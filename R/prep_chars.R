library(tidyverse)

psych_dat <- read.csv(here::here("raw_data", "psych_stats.csv"), sep = ";") %>%
  pivot_longer(cols = -char_id, names_to = "item", values_to = "score") %>%
  mutate(likert = case_when(score <= 20 ~ 1,
                            score <= 40 ~ 2,
                            score <= 60 ~ 3,
                            score <= 80 ~ 4,
                            TRUE ~ 5)) %>%
  select(item, likert, char_id) %>%
  pivot_wider(names_from = item, values_from = likert) %>%
  select(char_id, messy_neat, disorganized_self.disciplined, diligent_lazy, on.time_tardy, scheduled_spontaneous, ADHD_OCD, chaotic_orderly, overachiever_underachiever)

## ERstmal: Eine Art Gewissenhaftigkeit wird gemessen.
## Items:
# messy_neat, disorganized_self.disciplined, diligent_lazy, on.time_tardy, scheduled_spontaneous, ADHD_OCD, chaotic_orderly, overachiever_underachiever

## Umkodieren!
# - diligent_lazy, on.time_tardy, scheduled_spontaneous, overachiever_underachiever

psych_dat <- psych_dat %>%
  mutate(lazy_diligent = 6 - diligent_lazy,
         tardy_on.time = 6 - on.time_tardy,
         spontaneous_scheduled = 6 - scheduled_spontaneous,
         underachiever_overachiever = 6 - overachiever_underachiever) %>%
  select(-char_id, -diligent_lazy, -on.time_tardy, -scheduled_spontaneous, -overachiever_underachiever)

saveRDS(psych_dat, here::here("raw_data", "psych_pcm.rds"))
