## establishing FC effect: correlation (the extent to which each party support is associated with consensus for one´s views)

# "FCE was conceptualized as the correlation between participants’ own stance towards a topic and their estimated percentage 
# of the public with positive stance towards the topic" (extract from: Luzsa & Mayr, 2021; see also: Sargent & Newman, 2021) 

US_t1 <- US_t1 %>%
  mutate(vote = case_when(
    vote=='Democrats' ~ '1',
    vote=='Republicans' ~ '2')) 

US_t1 <- US_t1 %>%
  mutate(vote = as.numeric(vote))

# individual-based: correlation between own approval (among Republicans) and estimated consensus
cor.test(US_vote_Rep_t1$fc_1_s, US_vote_Rep_t1$fc_1_o_agr)


# group-based: correlation between party support and estimated consensus
cor.test(US_t1$vote, US_t1$fc_1_o_agr)

US_t1 <- US_t1 %>%
  mutate(vote = case_when(
    vote=='1' ~ 'Democrats',
    vote=='2' ~ 'Republicans')) 

# plot association between fc_1_s and fc_1_o_agr by vote

ggplot(data=US_t1,aes(x = fc_1_s, y = fc_1_o_agr, color = vote))+
  geom_point(position = position_dodge(width = 0.5))+
  scale_color_grey() +
  labs(color = "Party support") +
  cowplot::theme_cowplot()
  

## subsequently: between-group analysis: 

# "Consistent with Ross et al. (1977), false consensus was operationalized as existing when individuals rate the incidence 
# of their own opinions and behaviors in the wider community more highly than those who do not share those opinions and 
# behaviors. On this basis, prejudiced respondents would tend to estimate that more community members are prejudiced (i.e 
# ., agree with their views) than non-prejudiced respondents estimate community members are prejudiced (i.e., disagree with 
# their views), and vice versa" (extract from: Watts & Larkin, 2010)

# --> Mdiff = extent of false consensus

# descriptive statistics per party support
US_t1 %>% 
  dplyr::group_by(vote) %>% 
  dplyr::summarise(mean_fc_1 = mean(fc_1_s),
                   sd_approval_fc_1 = sd(fc_1_s),
                   mean_agree_fc_1 = mean(fc_1_o_agr), 
                   sd_agree_fc_1 = sd(fc_1_o_agr),
                   mean_disagr_fc_1 = mean(fc_1_o_disagr),
                   sd_disagr_fc_1 = sd(fc_1_o_disagr),
                   mean_fc_2 = mean(fc_2_s),
                   sd_approval_fc_2 = sd(fc_2_s),
                   mean_agree_fc_2 = mean(fc_2_o_agr), 
                   sd_agree_fc_2 = sd(fc_2_o_agr),
                   mean_disagr_fc_2 = mean(fc_2_o_disagr),
                   sd_disagr_fc_2 = sd(fc_2_o_disagr))

# by creating the descriptives, we also see how the party supporters score on own approval (i.e., Republicans > Democrats on own approval for racist remarks)

# run t-test with Democrats´ mean as reference
# is Mdiff significant?

t.test(US_t1_Rep$fc_1_o_agr, mu = 43.4)


## change in false consensus
# compare tables by time point
US_t1 %>% 
  dplyr::group_by(vote) %>% 
  dplyr::summarise(mean_fc_1 = mean(fc_1_s),
                   sd_approval_fc_1 = sd(fc_1_s),
                   mean_agree_fc_1 = mean(fc_1_o_agr), 
                   sd_agree_fc_1 = sd(fc_1_o_agr),
                   mean_disagr_fc_1 = mean(fc_1_o_disagr),
                   sd_disagr_fc_1 = sd(fc_1_o_disagr),
                   mean_fc_2 = mean(fc_2_s),
                   sd_approval_fc_2 = sd(fc_2_s),
                   mean_agree_fc_2 = mean(fc_2_o_agr), 
                   sd_agree_fc_2 = sd(fc_2_o_agr),
                   mean_disagr_fc_2 = mean(fc_2_o_disagr),
                   sd_disagr_fc_2 = sd(fc_2_o_disagr))

US_t2 %>% 
  dplyr::group_by(vote) %>% 
  dplyr::summarise(mean_fc_1 = mean(fc_1_s),
                   sd_approval_fc_1 = sd(fc_1_s),
                   mean_agree_fc_1 = mean(fc_1_o_agr), 
                   sd_agree_fc_1 = sd(fc_1_o_agr),
                   mean_disagr_fc_1 = mean(fc_1_o_disagr),
                   sd_disagr_fc_1 = sd(fc_1_o_disagr),
                   mean_fc_2 = mean(fc_2_s),
                   sd_approval_fc_2 = sd(fc_2_s),
                   mean_agree_fc_2 = mean(fc_2_o_agr), 
                   sd_agree_fc_2 = sd(fc_2_o_agr),
                   mean_disagr_fc_2 = mean(fc_2_o_disagr),
                   sd_disagr_fc_2 = sd(fc_2_o_disagr))

# plot?
US$time <- as.factor(US$time)

plot_summary <- US %>% 
  dplyr::mutate(time = case_when(
    time =="1" ~ "-.5",
    time =="2" ~ ".5")) %>%
  dplyr::mutate(time, levels = c("Pre", "Post")) %>% # this command doesnt work!!!
  dplyr::group_by(time, vote) %>%
  dplyr::summarise(n = n(),
                   mean_fc_1 = mean(fc_1_s),
                   sd_approval_fc_1 = sd(fc_1_s),
                   mean_agree_fc_1 = mean(fc_1_o_agr), 
                   sd_agree_fc_1 = sd(fc_1_o_agr),
                   se_agree_fc_1 = sd_agree_fc_1/sqrt(n),
                   lower_ci_agree_fc_1 = (mean_agree_fc_1 - (1.96*se_agree_fc_1)),
                   upper_ci_agree_fc_1 = (mean_agree_fc_1 + (1.96*se_agree_fc_1)),
                   mean_disagr_fc_1 = mean(fc_1_o_disagr),
                   sd_disagr_fc_1 = sd(fc_1_o_disagr),
                   mean_fc_2 = mean(fc_2_s),
                   sd_approval_fc_2 = sd(fc_2_s),
                   mean_agree_fc_2 = mean(fc_2_o_agr), 
                   sd_agree_fc_2 = sd(fc_2_o_agr),
                   se_agree_fc_2 = sd_agree_fc_2/sqrt(n),
                   lower_ci_agree_fc_2 = (mean_agree_fc_2 - (1.96*se_agree_fc_2)),
                   upper_ci_agree_fc_2 = (mean_agree_fc_2 + (1.96*se_agree_fc_2)),
                   mean_disagr_fc_2 = mean(fc_2_o_disagr),
                   sd_disagr_fc_2 = sd(fc_2_o_disagr))
plot_summary

ggplot(data = plot_summary, aes(x = time, y = mean_agree_fc_2, group = vote)) +
  geom_point(aes(color = vote), shape = 16, size = 2, pch = 23, position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci_agree_fc_2, ymax = upper_ci_agree_fc_2, color = vote),position = "dodge",  width = .05) +
  scale_color_grey() +
  geom_line(linetype = 3) + 
  scale_x_discrete(name = "Time point") +
  scale_y_continuous(name = "Mean agreement estimation FC2 (%)") +
  labs(color = "Party support") +
  cowplot::theme_cowplot()


## using false consensus measure as predictor

# TFCE as score that is used as predictor
