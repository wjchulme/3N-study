library("tidyverse")
library("readr")
library("readxl")
library("ggcorrplot")


survey_raw <- read_xlsx(
  here::here("data-export", "3N.xlsx"),
  col_names = c(
    "dt",
    "ptid",
    "phase",
    "ord",
    "retired",
    "retured_start",
    "nnh_start",
    "nns_end",
    "now",
    "profession",
    "specialty",
    "experience",
    "main",
    "schedule",
    "ooo",
    "Q01",
    "Q02",
    "Q03",
    "Q04",
    "Q05",
    "Q06",
    "Q07",
    "Q08",
    "Q09",
    "Q10",
    "Q11",
    "drop1"
  ), skip=1
) %>%
  select(-starts_with("drop"))

nfr_questions <- c(
  "I find it difficult to relax at the end of a working day",
  "By the end of the working day, I feel really worn out",
  "Because of my job, at the end of the working day I feel rather exhausted",
  "After the evening meal, I generally feel in good shape",
  "In general, I only start to feel relaxed on the second non-working day (or later)",
  "I find it difficult to concentrate in my free time after work",
  "I cannot really show any interest in other people when I have just come home myself",
  "Generally, I need more than an hour before I feel completely recuperated after work",
  "When I get home from work, I need to be left in peace for a while",
  "Often, after a day’s work I feel so tired that I cannot get involved in other activities",
  "A feeling of tiredness prevents me from doing my work as well as I normally would during the last part of the working day"
)

survey <- survey_raw %>%
  #filter(!is.na(dt)) %>%
  mutate(
    phase = case_when (
      phase == "pre NNH" ~ "p1_pre",
      phase == ">2w NNH" ~ "p2_during",
      phase == ">2w post" ~ "p3_post",
      TRUE ~ NA_character_
    ),
    nfr_score = rowSums(cbind(Q01,Q02,Q03,!Q04,Q05,Q06,Q07,Q08,Q09,Q10,Q11), na.rm=TRUE),
    nfr_pct = nfr_score*100/(11-(is.na(Q01)+is.na(Q02)+is.na(Q03)+is.na(Q04)+is.na(Q05)+is.na(Q06)+is.na(Q07)+is.na(Q08)+is.na(Q09)+is.na(Q10)+is.na(Q11))),
    nfr_score10 = rowSums(cbind(Q01,Q02,Q03,!Q04,Q06,Q07,Q08,Q09,Q10,Q11), na.rm=TRUE),
    
   # clinical = if_else(str_detect(tolower(profession), "doctor")|
  #                       str_detect(tolower(profession), "nurse")|
  #                       str_detect(tolower(profession), "therapist")|
  #                       str_detect(tolower(profession), "para"), 
  #                     "cinical",
  #                     "non-clinical"),
  ) #%>%
  #select(id, everything())


survey_baseline  <- survey %>%
  group_by(ptid) %>%
  summarise(
    retired = first(retired),
    profession = first(profession),
    
  )

survey_avg_long <- survey %>%
  group_by(ptid, phase) %>%
  summarise(
    nfr_score=mean(nfr_score),
  ) %>%
  left_join(survey_baseline, by="ptid")

survey_avg_wide <- survey_avg_long %>%
  pivot_wider(
    id_cols=c(ptid), names_from=phase, values_from=nfr_score
  ) %>%
  left_join(survey_baseline, by="ptid")


