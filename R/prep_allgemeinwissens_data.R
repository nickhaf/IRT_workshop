library(haven)
library(eatGADS)
library(tidyverse)


# Prepare smaller subset of dataset ---------------------------------------

# full_dat_path <- "/home/nick/Downloads/ZA6268_v1-0-0.sav"
# gads_obj <- import_spss(full_dt_path)

## Quicker to load as .rds file:
# saveRDS(gads_obj, "/home/nick/Downloads/ZA6268_v1-0-0_gads.rds")

## There are 24 Questionaires, that are build of different combinations of Item sets.
## Just select one for the exercise, in this case Nr. 4
## Frage 1:
## Was bedeutet Set? Anscheinend können Leute, die Set 1 und 2 beantwortet haben, die gleichen Items bekommen.

## Aus: https://link.springer.com/chapter/10.1007/978-3-531-92543-1_4
## - 4 Sets für jedes Wissensgebiet, 9 Fragen pro Set.
## - Sets wurden über die 24 Fragebogennummern hinweg variiert. Fragebogen Nr. 1 und 2
## haben z.b. Beide Set1 in Politik präsentiert bekommen, dann aber andere Sets in Wirtschaft.
## Das erklärt auf alle Fälle meine Verwirrung.
## Es scheint daher KEINE Ankeritems zu geben, allerdings haben Personen in bestimmten
## Fächern die gleichen Sets bearbeitet und in anderen nicht.
## Könnte man darüíer irgendwie linken? Als Beispiel vermutlich nicht so optimal,
## da etwas ungewöhnlich.
## Wurden aber parallelisiert, also könnte man so etwas wie equating machen?
## Oder bräuchte man dafür auch Ankeritems? Wahrscheinlich schon.


# gads <- readRDS("/home/nick/Downloads/ZA6268_v1-0-0_gads.rds")
# gads$dat <- gads$dat %>%
##    filter(C8 == 4)
 #saveRDS(gads, "/home/nick/Downloads/student_pisa_4.rds")

gads <- readRDS("/home/nick/Downloads/student_pisa_4.rds")

## This are the Item Sets used in this questionaire (https://doi.org/10.1007/978-3-531-92543-1_4)
## P-Set 1, G-Set2, W-Set-4, K-Set1, NSet4
## Id is build as follows:
# Area of the topic (Politik, Geschichte, Wirtschaft, Kultur, Naturwissenschaften)


answer_cols <- gads$labels %>%
  filter(str_detect(varLabel, "richtig beantwortet$") &
    str_detect(varName, "^N1124|^N2224|^N3424|^N4124|^N5424")) %>%
  pull(varName)

questions <- gads$labels %>%
  filter(str_detect(varName, "01|02$|03$|04$|05$|06$|07$|08$|09$") & str_length(varName) == 5) %>%
  mutate(question_code = gsub("[A-Za-z]", "", .$varName)) %>%
  ## IN case I want to do something with the answers, cut here
  select(question_code, varLabel) %>%
  unique()

answers_4 <- sp4$dat %>%
  select(C8, T1, C32, all_of(answer_cols), C38, C18, N33 ) %>% # C38 Bundesland Studium, C18 Spiegel Lesefrequenz, N33 Alter
  filter(C38 == 3) %>%
  rename("spiegelReadingfreq" = C18,
         "age" = N33,
         "bundeslandStudium" = C38) %>%
  #select(C8, T1, C32, all_of(answer_cols)) %>%
  rename("gender" = C32) %>%
  pivot_longer(
    cols = matches("_"),
    names_to = "question",
    values_to = "answer"
  ) %>%
  mutate(question_code = gsub("24_", "0", question)) %>%
  mutate(question_code = gsub("[A-Za-z]", "", question_code))

q_a_4 <- answers_4 %>%
  left_join(questions, by = "question_code") %>%
  select(-question)
# saveRDS(q_a_4, here::here("raw_data", "q_a.rds")



# Subset auswählen --------------------------------------------------------
# qa_dat <- readRDS(here::here("raw_data", "q_a.rds"))


qa_dat_1 <- q_a_4 %>%
  filter(str_starts(question_code, "1")) %>%
  select(-C8)

saveRDS(qa_dat_1, here::here("raw_data", "q_a_pol_b.rds"))

qa_dat_2 <- q_a_4 %>%
  filter(str_starts(question_code, "2")) %>%
  select(-C8)

saveRDS(qa_dat_2, here::here("raw_data", "q_a_gesch_b.rds"))

qa_dat_3 <- q_a_4 %>%
  filter(str_starts(question_code, "3")) %>%
  select(-C8)

saveRDS(qa_dat_3, here::here("raw_data", "q_a_wirt_b.rds"))


qa_dat_4 <- q_a_4 %>%
  filter(str_starts(question_code, "4")) %>%
  select(-C8)

saveRDS(qa_dat_4, here::here("raw_data", "q_a_kult_b.rds"))



qa_dat_5 <- q_a_4 %>%
  filter(str_starts(question_code, "5")) %>%
  select(-C8)

saveRDS(qa_dat_3, here::here("raw_data", "q_a_wiss_b.rds"))


# With haven for quick check -------------------------------------
# dataset <- read_sav("/home/nick/Downloads/ZA6268_v1-0-0.sav")

## Quicker to read as rds file:
# saveRDS(dataset, "/home/nick/Downloads/ZA6268_v1-0-0.rds")
# dataset <- readRDS("/home/nick/Downloads/ZA6268_v1-0-0.rds")

