library(haven)
library(eatGADS)
library(tidyverse)


# Prepare smaller subset of dataset ---------------------------------------
# full_dat_path <- "/home/nick/Downloads/ZA6268_v1-0-0.sav"
# gads_obj <- import_spss(full_dt_path)

## Quicker to load as .rds file:
# saveRDS(gads_obj, "/home/nick/Downloads/ZA6268_v1-0-0_gads.rds")

## Only keep the first two versions of the questionnaire (do some versions have anchor Items?)

gads <- readRDS("/home/nick/Downloads/ZA6268_v1-0-0_gads.rds")
# gads$dat <- gads$dat %>%
#   filter(C8 %in% c(1, 2))
# saveRDS(gads, "/home/nick/Downloads/gads_1_2.rds")
# gads <- readRDS("/home/nick/Downloads/gads_1_2.rds")

answer_cols <- gads$labels %>%
  filter(str_detect(varLabel, "richtig beantwortet$") & 
           str_detect(varLabel, "Set 1|Set 2")) %>%
  pull(varName)

questions <- gads$labels %>%
  filter(str_detect(varName, "01|02$|03$|04$|05$|06$|07$|08$|09$") & str_length(varName) == 5) %>%
  mutate(question_code = gsub('[A-Za-z]', '', .$varName)) %>%
  ## IN case I want to do something with the answers, cut here
  select(question_code, varLabel) %>%
  unique

answers_pol <- gads$dat %>%
  select(C8, T1, C32, all_of(answer_cols)) %>%
  rename("gender" = C32) %>%
  pivot_longer(
    cols = matches("_"), 
    names_to = "question", 
    values_to = "answer"
  ) %>%
  mutate(question_code = gsub("24_", "0", question)) %>%
  mutate(question_code = gsub('[A-Za-z]', '', question_code)) %>%
  filter(str_detect(question_code, "^1"))


  ## Alle rausschmeißen, die auf answers_cols nur NA haben. Die haben diese Items nicht gesehen. Jetzt Ist die Frage: Gab es da auch Ankeritems? Dann funktioniert das nicht, weil 
  ## zumindest einige dann drin Bleiben, die aber nur ein einzelnes Items gesehen haben. 

# saveRDS(answers, "/home/nick/Downloads/answers_1_2.rds")
answers <- readRDS("/home/nick/Downloads/answers_1_2.rds") 

q_a <- answers %>%
  left_join(questions_clean, by = "question_code") %>%
  select(-question)
# saveRDS(q_a, "/home/nick/Downloads/q_a.rds")

## Check if there are some anchor Items (even if, they probably are not enough to do own linking?)

q_a_1 <- q_a %>% filter(T0 == "v01")
q_a_2 <- q_a %>% filter(T0 == "v02")

unique(q_a_1$question_code)[unique(q_a_1$question_code) %in% unique(q_a_2$question_code)]

## Hmm, most of the items, if not all, seem to be part of both data sets. 
## Okaaay. Irgendwas stimmt hier noch nicht mit den Sets! 
## Können Sets die gleichen Items enthalten? Zumindest haben Personen Verschiedene Sets bearbeitet aber die gleichen Fragen beaantwortet. Ist das die falsche Variable?


q_a %>% filter(question_code == "1101") %>% View

q_a %>%
  filter(str_detect(varLabel, "FB-Nr. 159")) %>% 
  View()

## 5206 in beiden Sets enthalten

# Read in with haven for quick check -------------------------------------
# dataset <- read_sav("/home/nick/Downloads/ZA6268_v1-0-0.sav")

## Quicker to read as rds file:
# saveRDS(dataset, "/home/nick/Downloads/ZA6268_v1-0-0.rds")
dataset <- readRDS("/home/nick/Downloads/ZA6268_v1-0-0.rds")


## Goal: Hersausfinden, was Sets bedeutet, und Woran Ich erkenne, welche Personen den gleichen Fragebogen bekommen haben. 


