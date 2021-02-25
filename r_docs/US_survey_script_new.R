## preliminary data wrangling

# load packages
library(tidyverse)# data wrangling
library(kableExtra) # APA tables
library(lavaan) # CFA
library(psych) # describe funtion

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

## transforming variables

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

# check participant age
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

## transforming vote

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
merge_US <- merge_US %>%
  dplyr::mutate(sd1n = case_when(
                        sd1 <= "5" ~ "0",
                        sd1 >= "6" ~ "1"), 
                sd2n = case_when(
                        sd2 <= "5" ~ "0",
                        sd2 >= "6" ~ "1"),
                sd3n = case_when(
                        sd3 <= "5" ~ "0",
                        sd3 >= "6" ~ "1"),
                sd4n = case_when(
                        sd4 <= "5" ~ "0",
                        sd4 >= "6" ~ "1"),
                sd5n = case_when(
                        sd5 <= "5" ~ "0",
                        sd5 >= "6" ~ "1"),
                sd6n = case_when(
                        sd_6 <= "5" ~ "0",
                        sd_6 >= "6" ~ "1"),
                sd7n = case_when(
                        sd7 <= "5" ~ "0",
                        sd7 >= "6" ~ "1"),
                sd8n = case_when(
                        sd8 <= "5" ~ "0",
                        sd8 >= "6" ~ "1")) %>%
  dplyr::mutate(sd1n = as.numeric(sd1n), 
                sd2n = as.numeric(sd2n), 
                sd3n = as.numeric(sd3n), 
                sd4n = as.numeric(sd4n), 
                sd5n = as.numeric(sd5n), 
                sd6n = as.numeric(sd6n), 
                sd7n = as.numeric(sd7n), 
                sd8n = as.numeric(sd8n))

merge_US <- merge_US %>%
  dplyr::mutate(sd = sd1n + sd2n + sd3n + sd4n + sd5n + sd6n + sd7n + sd8n)
    

## rename fc_o_agr variables
merge_US <- merge_US %>%
  dplyr::rename(fc_1s = fc1s,
                fc_1_o_agr = fc1o_1,
                fc_1_o_disagr = fc1o_2,
                fc_2s = fc2s, 
                fc_2_o_agr = fc2o_2, 
                fc_2_o_disagr = fc2o_2)

## rename dataset
US <- merge_US

## participants

# age
age_by_gender <- US %>%
  dplyr::group_by(gend, time, vote) %>%
  dplyr::summarise(n = dplyr::n(),
                   perc = n/nrow(US)*100,
                   age_mean = mean(age),
                   age_sd = sd(age),
                   age_min = min(age),
                   age_max = max(age)) 
age_by_gender

# APA table
age_by_gender %>%
  kableExtra::kable(col.names = c("Gender", "Time", "Party Support", "*N*", "%", "*M*~age~", "*SD*~age~", "Min~age~", "Max~age~"),
                    caption = "Table 1 *Descriptive statistics by Gender, Time, and Party Support*",
                    digits = 2) %>%
  kableExtra::kable_styling()


# further demographics
table(US$time, US$vote, US$ethn)
table(US$time, US$vote, US$degree)
table(US$time, US$vote, US$income)
table(US$time, US$vote, US$employ)

# N for t1 and t2
sample_size_t1 <- nrow(US_t1)
sample_size_t2 <- nrow(US_t2)

## scale reliability
# cn
key <- list(
  CN = c("cn_1", "cn_2", "cn_3", "cn_4", "cn_5")
)

scoreItems(key, US)

US %>%
  dplyr::select(14:18) %>%
  psych::alpha()

# --> cronbach´s alpha for cn = 0.84

## creating new variable cn
US <- US %>%
  mutate(cn = (cn1 + cn2 + cn3 + cn4 + cn5)/5)

## graphical inspection cn
cn_plot <- US %>%
  ggplot2::ggplot(aes(x = cn)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Collective Narcissism", y = "N") +
  cowplot::theme_cowplot()
cn_plot

# skew and kurtosis
describe(US$cn)

# ingroup satisfaction 
key <- list(
  ingroup_satis = c("secident_4", "secident_5", 
                    "secident_6", "secident_7")
)

scoreItems(key, US)

US %>%
  dplyr::select(28:31) %>%
  psych::alpha()

# --> cronbach´s alpha for ingroup_satis = 0.95

## creating new variable insatis
US <- US %>%
  mutate(insatis = (secident4 + secident5 + secident6 + secident7)/4)

## graphical inspection insatis
insatis_plot <- US %>%
  ggplot2::ggplot(aes(x = insatis)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Ingroup satisfaction", y = "N") +
  cowplot::theme_cowplot()
insatis_plot

# skew and kurtosis
describe(US$insatis)

# xeno
key <- list(
  xeno = c("xeno1", "xeno2", "xeno3", "xeno4", "xeno5", "xeno6")
)

scoreItems(key, US)

US %>%
  dplyr::select(19:24) %>%
  psych::alpha()

# --> cronbach´s alpha for xenophobia support = 0.71

## creating new variable xeno
US <- US %>%
  mutate(xeno = (xeno1 + xeno2 + xeno3 + xeno4 + xeno5 + xeno6)/6)

# xeno without xeno_5 (see CFA)
key <- list(
  xeno_1 = c("xeno_1", "xeno_2", "xeno_3", "xeno_4", "xeno_6")
)

## graphical inspection xeno
xeno_plot <- US %>%
  ggplot2::ggplot(aes(x = xeno)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Xenophobia", y = "N") +
  cowplot::theme_cowplot()
xeno_plot

# skew and kurtosis
describe(US$xeno)


scoreItems(key, US_data)

US_data %>%
  dplyr::select(10, 11, 12, 13, 15) %>%
  psych::alpha()


# --> cronbach´s alpha for xeno without xeno_5 = 0.71

# xeno without xeno_5 and xeno_6 (see CFA)
key <- list(
  xeno_2 = c("xeno_1", "xeno_2", "xeno_3", "xeno_4")
)

scoreItems(key, US_data)

US_data %>%
  dplyr::select(10, 11, 12, 13) %>%
  psych::alpha()


# -->cronbach´s alpha for xeno without xeno_5 = 0.71

## creating new variable xeno_new
US <- US %>%
  mutate(xeno_new = (xeno1 + xeno2 + xeno3 + xeno4)/4)

## graphical inspection xeno
xeno_new_plot <- US %>%
  ggplot2::ggplot(aes(x = xeno_new)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Xenophobia", y = "N") +
  cowplot::theme_cowplot()
xeno_new_plot

# skew and kurtosis
describe(US$xeno_new)

# joy (Spearman-Brown statistic)
cor.test(~ US$joy1 + US$joy2, data = US, alternative = "two.sided", method = "spearman")

# --> Spearman rho = 0.69

## creating new variable joy
US <- US %>%
  mutate(joy = (joy1 + joy2)/2)

## graphical inspection xeno
joy_plot <- US %>%
  ggplot2::ggplot(aes(x = joy)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Joy at Success", y = "N") +
  cowplot::theme_cowplot()
joy_plot

# skew and kurtosis
describe(US$joy)

# eff (Spearman-Brown statistic)
cor.test(~ US$eff1 + US$eff2, data = US, alternative = "two.sided", method = "spearman")

# --> Spearman rho = 0.65

## creating new variable joy
US <- US %>%
  mutate(eff = (eff1 + eff2)/2)

## graphical inspection xeno
eff_plot <- US %>%
  ggplot2::ggplot(aes(x = joy)) +
  geom_histogram(fill="transparent", colour="black") +
  labs(x = "Group Efficacy", y = "N") +
  cowplot::theme_cowplot()
eff_plot

# skew and kurtosis
describe(US$eff)

## CFA

# cn
cn.cfa <- US %>%
  dplyr::select(cn1, cn2, cn3, cn4, cn5) 

mardia(cn.cfa, plot = TRUE)

keep <- function(x) {
  (x >= mean(x) - 3*sd(x)) & 
    (x <= mean(x) + 3*sd(x))  
}

cn.cfa_clean <- cn.cfa %>%
  filter(keep(cn1) &
           keep(cn2) &
           keep(cn3) &
           keep(cn4) &
           keep(cn5))

cn.model <- 'CollectNar =~ cn1 + cn2 + cn3 + cn4 + cn5'

fit_cn <- cfa(cn.model, data = cn.cfa_clean)

fit_cn %>% summary(fit.measures = TRUE, standardized = TRUE)

lavInspect(fit_cn, what = "sampstat")
lavInspect(fit_cn, what = "implied")
residual_cn <- lavInspect(fit_cn, what = "sampstat") $cov - 
  lavInspect(fit_cn, what = "implied") $cov
residual_cn

# Residual divided by an estimate of its standard error.
# If greater than 2, it is significantly different from zero.
resid(fit_cn, type = "standardized")

# xeno
xeno.cfa <- US %>%
  dplyr::select(xeno1, xeno2, xeno3, xeno4, xeno5, xeno6)

xeno.model <- 'Xenophobia =~ xeno1 + xeno2 + xeno3 + xeno4 + xeno5 +
xeno6'

# MLR since we expect no normal distribution
fit_xeno <- cfa(xeno.model, data = xeno.cfa, estimator = "MLR")

fit_xeno %>% summary(fit.measures = TRUE)

lavInspect(fit_xeno, what = "sampstat")
lavInspect(fit_xeno, what = "implied")
residual_xeno <- lavInspect(fit_xeno, what = "sampstat") $cov - 
  lavInspect(fit_xeno, what = "implied") $cov
residual_xeno
resid(fit_xeno, type = "standardized")

# without item 5 and 6 
xeno.cfa_1 <- US %>%
  dplyr::select(xeno1, xeno2, xeno3, xeno4)

xeno.model_1 <- 'Xenophobia =~ xeno1 + xeno2 + xeno3 + xeno4'

fit_xeno_1 <- cfa(xeno.model_1, data = xeno.cfa_1, estimator = "MLR")

fit_xeno_1 %>% summary(fit.measures = TRUE)

lavInspect(fit_xeno_1, what = "sampstat")
lavInspect(fit_xeno_1, what = "implied")
residual_xeno <- lavInspect(fit_xeno_1, what = "sampstat") $cov - 
  lavInspect(fit_xeno_1, what = "implied") $cov
residual_xeno
resid(fit_xeno_1, type = "standardized")

# joy

joy.model <- 'Joy at Success =~ a*joy_1 + a*joy_2'

fit_joy <- cfa(joy.model, data = US_data, estimator = "ML", std.lv = TRUE)

fit_joy %>% summary(fit.measures = TRUE)

# --> fit of model is not accessible due to two-item construction

# eff

eff.model <- 'Group efficacy =~ a*eff_1 + a*eff_2'

fit_eff <- cfa(eff.model, data = US_data, estimator = "ML", std.lv = TRUE)

fit_eff %>% summary(fit.measures = TRUE)

# --> fit of model is not accessible due to two-item construction

# empowerment

emp.cfa <- US_data %>%
  dplyr::select(joy_1, joy_2, eff_1, eff_2) 

mardia(emp.cfa, plot = TRUE)

keep <- function(x) {
  (x >= mean(x) - 3*sd(x)) & 
    (x <= mean(x) + 3*sd(x))  
}

emp.cfa_clean <- emp.cfa %>%
  filter(keep(joy_1) &
           keep(joy_2) &
           keep(eff_1) &
           keep(eff_2))

emp.model <- 'Empowerment =~ joy_1 + joy_2 + eff_1 + eff_2'

fit_emp <- cfa(emp.model, data = emp.cfa_clean)

fit_emp %>% summary(fit.measures = TRUE)

lavInspect(fit_emp, what = "sampstat")
lavInspect(fit_emp, what = "implied")
residual_emp <- lavInspect(fit_emp, what = "sampstat") $cov - 
  lavInspect(fit_emp, what = "implied") $cov
residual_emp
resid(fit_emp, type = "standardized")

# --> overall empowerment shows low construct validity (.82)

## outliers (visually (follow guide on Twitter) and MAD)


## Descriptive Statistic

## Correlation (can be used when one variable is categorical, here income, degree)

## False consensus effect

# examine false consensus a) traditionally: group-based (= between groups), b) with a focus on Republicans only (individual based, see paper "False consensus in the Echo Chamber" --> with correlation between own approval and estimated agreement (see potentially review Rikki?!; see also , Measuring Misperceptions?")

# examining change see papers: "Collective narcissism as a basis for nationalism" and "Persistent beliefs: Political extremism predicts ideological stability over time" (syntax here: https://osf.io/358vx/?view_only=f2ab8ce453b34643b6df4f3dfb9d2514)

# MLM: https://www.rensvandeschoot.com/tutorials/lme4/



