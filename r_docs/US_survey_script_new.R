# load packages
library(tidyverse)# data wrangling
library(compare)

# load datasets and remove first row 
US_t1 <- read_csv("./data/US survey_t1.csv") %>%
  dplyr::filter(id != "id")
US_t2 <- read_csv("./data/US survey_t2.csv")%>%
  dplyr::filter(id != "id")

# merge dataset based on participation in t1 and t2 survey (https://rfordatascience.slack.com/archives/C8K09CDNZ/p1613388031373600?thread_ts=1613074402.320400&cid=C8K09CDNZ)

common_ids <- US_t1$id[US_t1$id %in% US_t2$id]

long_t1 <- US_t1 %>%
  mutate_all(as.character) %>% 
  pivot_longer(-id) %>% 
  filter(id %in% common_ids) %>% 
  mutate(time = "1")

long_t2 <- US_t2 %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-id) %>% 
  filter(id %in% common_ids) %>% 
  mutate(time = "2")

merge_US <- bind_rows(long_t1,long_t2) %>%
  mutate(name = str_remove_all(name,"_w1|_w2")) %>%
  pivot_wider(names_from = name, values_from = value)

## transform variables

# if to convert into factor: dplyr::mutate(gender = factor(gender, labels = c("Female", "Male", "Other", "Prefer Not To Say"))

merge_US <- merge_US %>%
  dplyr::mutate(id = as.factor(id),
                time = as.factor(time),
                age = as.numeric(age),
                gend = recode(gend, 
                             "1" = "Female",
                             "2" = "Male",
                             "3" = "Non-binary"),
                ethn = recode(ethn, 
                             "1" = "Asian-American",
                             "2" = "Black or African American",
                             "3" = "Hispanic or Latino American",
                             "4" = "Native American",
                             "5" = "White or European American",
                             "6" = "Other"),
                degree = recode(degree, 
                             "1" = "No degree",
                             "2" = "High School",
                             "3" = "Bachelor´s degree",
                             "4" = "Master´s degree",
                             "5" = "Doctoral degree",
                             "6" = "Professional degree",
                             "7" = "Other"),
                employ = recode(employ, 
                             "1" = "Working - paid employee",
                             "2" = "Working - self-employed",
                             "3" = "Not working - temporary layoff from a job",
                             "4" = "Not working - looking for a job",
                             "5" = "Not working - retired",
                             "6" = "Not working - disabled",
                             "7" = "Not working - student",
                             "8" = "Other"),
                income = recode(income, 
                             "1" = "$0 - $10,000",
                             "2" = "$10,000 - $20,000",
                             "3" = "$20,000 - $30,000",
                             "4" = "$30,000 - $40,000",
                             "5" = "$40,000 - $50,000",
                             "6" = "over $50,000"))

# transform rest of variables into numeric (https://stackoverflow.com/questions/3796266/change-the-class-from-factor-to-numeric-of-many-columns-in-a-data-frame)

merge_US[,c(18:67)]= apply(merge_US[,c(18:67)], 2, function(x) as.numeric(as.character(x)))

# remove variables that are unnecessary or could identify participants

merge_US <- merge_US %>%
  dplyr::select(-Duration_sec, -Finished, -consent_sign, -pro_id, -manu, -PROLIFIC_PID, -degree_7_TEXT_Topics, -degree_7_TEXT_Parent_Topics)

# check age
min(merge_US$age)
max(merge_US$age)

# check care items
!is.na(merge_US$care1)

# 3 people with failed attention check
n_manu_fail <- merge_US %>%
  dplyr::filter(!is.na(care1)) %>%
  nrow()

# no one to be excluded 
dplyr::filter(merge_US, care2 != "1")

## transform vote

# align "non-vote" at t2 with party support at t1
merge_US <- merge_US %>%
  dplyr::mutate(vote = ifelse(id == "p014", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p044", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p100", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p117", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p124", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p146", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p189", "1", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p151", "2", vote)) %>%
  dplyr::mutate(vote = ifelse(id == "p164", "2", vote)) 

table(merge_US$vote) # no "vote == 4" anymore

# remove cases in which party support t1 does not match party support t2

t1 <- subset(merge_US, time == "1") %>%
  dplyr::select(id, vote)
t2 <- subset(merge_US, time == "2") %>%
  dplyr::select(id, vote)

mismatch <- dplyr::anti_join(t1,t2) # p012, p065, p066, p150, p177

# remove cases

merge_US <- merge_US %>%
  dplyr::filter(id != "p012" & id != "p065" & id != "p066" & id != "p150" & id != "p177")

# rename variable vote 

merge_US <- merge_US %>%
  dplyr::mutate(vote = factor(vote, labels= c("Democrats", "Republicans")))

table(merge_US$vote)


## transform "exp" in "unexp"

merge_US <- merge_US %>%
  dplyr::mutate(unexp = case_when(
    vote == "Democrats" & exp <= "3" ~ "0",
    vote == "Republicans" & exp <= "3" ~ "1",
    vote == "Democrats" & exp >= "5" ~ "1",
    vote == "Republicans" & exp >= "5" ~ "0")) %>%
  dplyr::mutate(unexp = recode(unexp, 
                               "0" = "Expected",
                               "1" = "Unexpected"))

## transform sd

## transform fc_o_agr variables

## participants




# participant description

# N for t1 and t2
sample_size_t1 <- nrow(US_t1)
sample_size_t2 <- nrow(US_t2)


## outliers











# examine false consensus with correlation (see potentially review Rikki?!; "Measuring Misperceptions?")

# examining change see papers: "Collective narcissism as a basis for nationalism" and "Persistent beliefs: Political extremism predicts ideological stability over time" (syntax here: https://osf.io/358vx/?view_only=f2ab8ce453b34643b6df4f3dfb9d2514)

# MLM: https://www.rensvandeschoot.com/tutorials/lme4/



