library(dplyr)
library(statsr)
library(lme4)
library(tidyverse)
library(glmmTMB)
library(effects)
library(gtsummary)
library(easystats)
library(parameters)
library(see)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)


df =  read.csv("belief_mixed_effects.csv") %>% mutate(post_stage = str_replace(post_stage,"T","T_"))

df$belief_movement = df$post_belief - df$prior_belief

df$belief_movement_bnd = scales::rescale(df$belief_movement, to = c(0.001,0.999))

df$post_uncertainty_bnd = scales::rescale(df$post_uncertainty, to = c(0.001,0.999))

means = df %>% group_by(post_stage,variable) %>%
  summarise(mean_prior=mean(prior_belief),sd_prior=sd(prior_belief),mean_post=mean(post_belief),sd_post=sd(post_belief))

mean_prior = df %>% 
  group_by(variable) %>%
  summarise(mean=mean(prior_belief))

mean_T_0 = df %>% filter(post_stage=="T_0") %>%
  group_by(variable) %>% 
  summarise(mean = mean(post_belief))
  
mean_T_5 = df %>% filter(post_stage=="T_5") %>%
  group_by(variable) %>%
    summarise(mean = mean(post_belief))


Prior_fig = df %>% filter(post_stage=="T_0") %>% 
  ggplot() + 
  geom_histogram(aes(x=prior_belief),alpha=0.3,fill="darkgrey",color="darkgrey",bins=10) +
  geom_vline(data = mean_prior, aes(xintercept=mean),color="black",linetype="dotted", size=0.8)+
  facet_grid(~variable) + theme_minimal()

Prior_fig

belief_Stage_density_fig = df %>%
  ggplot() + 
  geom_density(data=df %>% filter(post_stage=="T_0"),aes(x=prior_belief,fill="prior"),alpha=0.3,color="darkgrey") +
  geom_density(aes(x=post_belief, fill=post_stage),alpha=0.3,color="darkgrey") +
  labs(x = "Belief", y = "Density") +
  facet_grid(cols = vars(variable)) + 
  scale_fill_manual(values=c('darkgrey','#67a9cf','#02818a'),name="Belief Stage")+
  scale_color_manual(values=c('darkgrey','#67a9cf','#02818a'),name="means",limits=c("Prior","T_0","T_5")) + theme_minimal()

belief_Stage_density_fig

## since the models are bounded, we use a beta regression, if there are other things we should test, we can try here.

beta_m2 = glmmTMB(belief_movement_bnd~ prior_uncertainty * post_stage +  (1|ID) ,df,family=list(family="beta", link="logit"))
beta_m3 = glmmTMB(post_uncertainty_bnd ~  post_stage +  (1|ID) ,df,family=list(family="beta", link="logit"))


plot_model(beta_m2,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est", show.intercept = TRUE ) +
  theme(axis.text.y = element_text(size = 10),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = " ", title = "") +
  theme(axis.text.y = element_text(size = 10),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Belief Difference (Posterior - Prior)", title = "") +
  ylim(-0.25, 0.9) + 
  ylim(0.45,1.5)+ theme_minimal()

## This is how you get the significance values. Summary gets you the estimate, and the z and p value
## confint gives you the confidence intervals.

summary(beta_m2)
confint(beta_m2)

summary(beta_m3)
confint(beta_m3)
