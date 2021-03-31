## establishing FC effect: correlation (the extent to which each party support is associated with consensus for one´s views)

# "FCE was conceptualized as the correlation between participants’ own stance towards a topic and their estimated percentage 
# of the public with positive stance towards the topic" (extract from: Luzsa & Mayr, 2021; see also: Sargent & Newman, 2021) 

US_t1 <- US_t1 %>%
  mutate(vote = case_when(
    vote=='Democrats' ~ '1',
    vote=='Republicans' ~ '2')) 

US_t1 <- US_t1 %>%
  mutate(vote = as.numeric(vote))

# within-group: correlation between own approval (among Republicans) and estimated consensus
cor.test(US_vote_Rep_t1$fc_1_s, US_vote_Rep_t1$fc_1_o_agr)
cor.test(US_vote_Dem_t1$fc_1_s, US_vote_Dem_t1$fc_1_o_agr)

# between-group: correlation between party support and estimated consensus
cor.test(US_t1$vote, US_t1$fc_1_o_agr)

US_t1 <- US_t1 %>%
  mutate(vote = case_when(
    vote=='1' ~ 'Democrats',
    vote=='2' ~ 'Republicans')) 

# plot association between fc_1_s and fc_1_o_agr by vote

ggplot(data = US_t1,aes(x=fc_1_s,y=fc_1_o_agr, color = vote))+
  stat_ellipse(expand = 0,aes(fill=vote))+
  geom_point(position = "jitter")+
  scale_y_continuous(name = "Mean estimated agreement fc1") +
  scale_x_continuous(name = "Mean own approval fc1") +
  scale_color_grey() +
  labs(color = "Party support") +
  cowplot::theme_cowplot()
  

## subsequently: between-group analysis (because focus of this paper): 

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

# within-groups: conduct t-tests on difference (like what I have already done in paper)

# t-test
t.test(x, mu = y)

# plot
US$time <- as.factor(US$time)

plot_summary <- US %>%
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


# plot 1

US <- tibble(
  vote = c('Democrats', 'Republicans', 'Democrats', 'Republicans'),
  time = c('Pre', 'Pre', 'Post', 'Post'),
  val = c(43.4, 61.6, 45.3, 61.2),
)

n_groups <- length(unique(US$vote))
group_names <- unique(US$vote)

US %>%
  mutate(vote = factor(vote)) %>%
  # Manually calculate x positions
  mutate(x = as.integer(vote) - (n_groups + 1) * (time == "Pre")) %>%
  ggplot(aes(x = x, y = val, vote = vote, col = vote, shape = vote)) +
  geom_point(shape = 16, size = 2, pch = 23) +
  geom_line(linetype = 3) +
  # Manually add labels
  scale_x_continuous(breaks = (-n_groups):n_groups,
                     labels = c(group_names, 'Election', group_names))+
  scale_y_continuous(name = "Consensus estimation (Rep/agreement; Dem/disagreement)")+
  scale_color_grey() +
  cowplot::theme_cowplot() + 
  geom_vline(xintercept = 0, linetype = "dashed")



# plot 2
plot_summary %>%
  ggplot(aes(x = time, y = mean_agree_fc_1, group = vote)) +
  geom_point(aes(color = vote), shape = 16, size = 2, pch = 23, position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci_agree_fc_1, ymax = upper_ci_agree_fc_1, color = vote),position = "dodge",  width = .05) +
  scale_color_grey() +
  geom_line(linetype = 3) + 
  scale_x_discrete(name = "Time point") +
  scale_y_continuous(name = "Mean agreement estimation FC1 (%)") +
  labs(color = "Party support") +
  cowplot::theme_cowplot()



# between-groups: # t1 Rep/agree - Dem/disagree (= value one in t-test)  vs t2 Rep/agree - Dem/disagree (= reference value in t-test)

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

# t-test
t.test(x, mu = y)



## using false consensus measure as predictor (Bauman & Geher (2002))

# Calculating false consensus scores. Initially, participants' false consensus scores were calculated in a number of 
# different ways. Based on Krueger and Zeiger's (1993) truly false consensus measure, a score was computed for each 
# individual which assessed his/her overall tendency (across all issues) to overestimate support for his/her position. A
# similar measure was computed based on people's behavioral endorsements and estimates, rather than their attitudes. 
# Ultimately, neither of these indices of false consensus was significantly related to behavioral intentions for the issues 
# incorporated in this research. Thus, false consensus scores were computed for each issue by subtracting the actual 
# consensus (based on the actual percentage of people who agreed with a particular issue) from each person's estimate to 
# determine the extent to which he or she overestimated support for his or her position using the behavioral measure. 

# actual consensus = frequency of endorsement of items
# estimated consensus = estimated agreement 

# TFCE = estimated agreement - actual consensus

# establish actual consensus time 1

approval_fc_1_s_t1 <- US_t1 %>% 
  dplyr::filter(fc_1_s >= 4) %>%
  nrow()
approval_fc_1_s_t1 # results in 41 (out of 139 --> 29.5%)

# establish actual consensus time 2
approval_fc_1_s_t2 <- US_t2 %>% 
  dplyr::filter(fc_1_s >= 4) %>%
  nrow()
approval_fc_1_s_t2 # results in 37 (out of 139 --> 26.6%)

# --> establish overall mean of estimated agreement for Republicans

US_Rep <- subset(US, vote == "Republicans")

# old
US_Rep <- US_Rep %>% 
  group_by(id) %>%
  dplyr::mutate(fc_1_o_agr_sum = sum(fc_1_o_agr),
                mean_fc_1_o_agr = fc_1_o_agr_sum/2, 
                TFCE = mean_fc_1_o_agr -61) # minus 61 from the overall mean 

# minus the actual consensus from estimated per time each
US_Rep <- US_Rep %>%
  dplyr::mutate(TFCE_t1 = fc_1_o_agr - 29.5,
                TFCE_t2 = fc_1_o_agr - 26.6) 

# export file and deleted every row that was wrongly calculated by hand!
write.csv(US_Rep, "US_Rep.csv")

US_Rep <- read_csv("./data/US_Rep.csv")

# "endorsement" ?? see Krueger & Zeiger (1993)










