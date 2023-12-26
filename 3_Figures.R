# clear all objects in the workspace
rm(list = ls())

# Dependencies
library(dplyr)
library(nlme)
library(ggplot2)
require(ggrepel)
library(stats)
library(ggpubr)
library(piecewiseSEM)

## Linear mixed-effects models
# load data
LM <- read.csv('Data/LMM.csv')
LM$composition <- substr(LM$Treatment, 1, nchar(LM$Treatment)-4) 

# species stability
pop <- lme(stab.pop ~ competition * Temp * Env, random = ~ 1|composition, data = LM) 
anova(pop)

# asynchrony
asyn_lm <- lme(asyn ~ competition * Temp * Env, random = ~ 1|composition, data = LM) 
anova(asyn_lm)

# CPE
cpe_lm <- lme(compensatory ~ competition * Temp * Env, random = ~ 1|composition, data = LM) 
anova(cpe_lm)

# SAE
sae_lm <- lme(statistical ~ competition * Temp * Env, random = ~ 1|composition, data = LM) 
anova(sae_lm)

# ecosystem stability
comm <- lme(stab.comm ~ competition * Temp * Env, random = ~ 1|composition, data = LM) 
anova(comm)

#############
## Figures ##
#############

# Load data
res <- read.csv("Figures/compare_mixture_vs_monoculture.csv")
# log-transformation
res[,c(6:19)] <- apply(res[,c(6:19)], 2, log10)
head(res)

# Calculate mean and standard error for each metric
asyn_mix_mean_se <- res %>%
  group_by(Treatment) %>%
  summarise(asyn_mean_mix = mean(asyn.mix),
            asyn_se_mix = sd(asyn.mix)/sqrt(n()))

asyn_mono_mean_se <- res %>%
  group_by(Treatment) %>%
  summarise(asyn_mean_mono = mean(asyn.mono),
            asyn_se_mono = sd(asyn.mono)/sqrt(n()))

pop_mix <- res %>%
  group_by(Treatment) %>%
  summarise(pop_mean_mix = mean(stab.pop.mix),
            pop_se_mix = sd(stab.pop.mix)/sqrt(n()))

pop_mono <- res %>%
  group_by(Treatment) %>%
  summarise(pop_mean_mono = mean(stab.pop.mono),
            pop_se_mono = sd(stab.pop.mono)/sqrt(n()))

comm_mix <- res %>%
  group_by(Treatment) %>%
  summarise(comm_mean_mix = mean(stab.comm.mix),
            comm_se_mix = sd(stab.comm.mix)/sqrt(n()))

comm_mono <- res %>%
  group_by(Treatment) %>%
  summarise(comm_mean_mono = mean(stab.comm.mono),
            comm_se_mono = sd(stab.comm.mono)/sqrt(n()))

CPE_mix <- res %>%
  group_by(Treatment) %>%
  summarise(CPE_mean_mix = mean(compensatory.mix),
            CPE_se_mix = sd(compensatory.mix)/sqrt(n()))

CPE_mono <- res %>%
  group_by(Treatment) %>%
  summarise(CPE_mean_mono = mean(compensatory.mono),
            CPE_se_mono = sd(compensatory.mono)/sqrt(n()))

SE_mix <- res %>%
  group_by(Treatment) %>%
  summarise(SE_mean_mix = mean(statistical.mix),
            SE_se_mix = sd(statistical.mix)/sqrt(n()))

SE_mono <- res %>%
  group_by(Treatment) %>%
  summarise(SE_mean_mono = mean(statistical.mono),
            SE_se_mono = sd(statistical.mono)/sqrt(n()))

# Merge the data frames
df_merged <- full_join(asyn_mix_mean_se, asyn_mono_mean_se, by = "Treatment") %>%
  full_join(pop_mix, by = "Treatment") %>%
  full_join(pop_mono, by = "Treatment") %>%
  full_join(comm_mix, by = "Treatment") %>%
  full_join(comm_mono, by = "Treatment") %>%
  full_join(CPE_mix, by = "Treatment") %>%
  full_join(CPE_mono, by = "Treatment") %>%
  full_join(SE_mix, by = "Treatment") %>%
  full_join(SE_mono, by = "Treatment")

df_merged$Environment <- substr(df_merged$Treatment, nchar(df_merged$Treatment), nchar(df_merged$Treatment))
df_merged$comp <- substr(df_merged$Treatment, 1, nchar(df_merged$Treatment)-4)
df_merged$Temperature <- substr(df_merged$Treatment, nchar(df_merged$Treatment)-2, nchar(df_merged$Treatment)-1)

head(df_merged)

## Figure 1 - Species stability
ggplot(df_merged, aes(x=pop_mean_mono, y = pop_mean_mix, shape = Temperature, color = Environment)) +
  geom_point(size=5, alpha=0.8) +
  scale_color_manual(values = c("#6C8FC6", "#F7776E")) + 
  geom_errorbar(aes(ymin = pop_mean_mix - pop_se_mix, ymax = pop_mean_mix + pop_se_mix), width = 0) +
  geom_errorbarh(aes(xmin = pop_mean_mono - pop_se_mono, xmax = pop_mean_mono + pop_se_mono), height = 0) +
  xlim(0,1.2) + ylim(0, 1.2) +
  geom_text_repel(aes(label=comp), hjust=-0.2) +
  ylab("Species stability in mix-culture") + xlab("Species stability in monoculture") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(aspect.ratio = 1, axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  geom_abline(slope = 1, intercept = 0, linetype=2) +
  theme(legend.position = c(0.85,0.2), legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        legend.box.background = element_rect(), legend.box.margin = margin(2,2,2,2))

## Figure 2
# plot asynchrony
p1 <- ggplot(df_merged, aes(x=asyn_mean_mono, y = asyn_mean_mix, shape = Temperature, color = Environment)) +
  geom_point(size=5, alpha=0.8) + labs(tag = "(a)") +
  scale_color_manual(values = c("#6C8FC6", "#F7776E")) + 
  geom_errorbar(aes(ymin = asyn_mean_mix - asyn_se_mix, ymax = asyn_mean_mix + asyn_se_mix), width = 0) +
  geom_errorbarh(aes(xmin = asyn_mean_mono - asyn_se_mono, xmax = asyn_mean_mono + asyn_se_mono), height = 0) +
  xlim(0,0.1) + ylim(0, 0.1) +
  geom_text_repel(aes(label=comp), hjust=-0.3) +
  ylab("Asynchrony in mix-culture") + xlab("Asynchrony in monoculture") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(aspect.ratio = 1, axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  geom_abline(slope = 1, intercept = 0, linetype=2) +
  theme(legend.position = c(0.85,0.2), legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        legend.box.background = element_rect(), legend.box.margin = margin(2,2,2,2))

# Plot compensatory effects
p2 <- ggplot(df_merged, aes(x=CPE_mean_mono, y = CPE_mean_mix, shape = Temperature, color = Environment)) +
  geom_point(size=5, alpha=0.8) + labs(tag = "(b)") +
  scale_color_manual(values = c("#6C8FC6", "#F7776E")) + 
  geom_errorbar(aes(ymin = CPE_mean_mix - CPE_se_mix, ymax = CPE_mean_mix + CPE_se_mix), width = 0) +
  geom_errorbarh(aes(xmin = CPE_mean_mono - CPE_se_mono, xmax = CPE_mean_mono + CPE_se_mono), height = 0) +
  xlim(-0.1,0.1) + ylim(-0.1, 0.1) +
  geom_text_repel(aes(label=comp), hjust=-0.2) +
  ylab("Compensatory effect in mix-culture") + xlab("Compensatory effect in monoculture") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(aspect.ratio = 1, axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  geom_abline(slope = 1, intercept = 0, linetype=2) +
  theme(legend.position = "None")

# Plot statistical-averaging effects
p3 <- ggplot(df_merged, aes(x=SE_mean_mono, y = SE_mean_mix, shape = Temperature, color = Environment)) +
  geom_point(size=5, alpha=0.8) + labs(tag = "(c)") +
  scale_color_manual(values = c("#6C8FC6", "#F7776E")) +
  geom_errorbar(aes(ymin = SE_mean_mix - SE_se_mix, ymax = SE_mean_mix + SE_se_mix), width = 0) +
  geom_errorbarh(aes(xmin = SE_mean_mono - SE_se_mono, xmax = SE_mean_mono + SE_se_mono), height = 0) +
  xlim(-0.05,0.2) + ylim(-0.05, 0.2) +
  geom_text_repel(aes(label=comp), hjust=-0.2) +
  ylab("Statistical-averaging effect in mix-culture") + xlab("Statistical-averaging effect in monoculture") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(aspect.ratio = 1, axis.title = element_text(size=15), axis.text = element_text(size=12)) +
  geom_abline(slope = 1, intercept = 0, linetype=2) +
  theme(legend.position = "None")

grid.arrange(p1, p2, p3, ncol = 3)


## Figure 3 - SEM_competition
# Initial model
sem_lme <- psem(
  compensatory %~~% statistical,
  lme(stab.pop ~ TEMP + competition + ENV, random = ~1|composition, LM),
  lme(compensatory ~ TEMP + competition + ENV, random = ~1|composition, LM),
  lme(statistical ~ TEMP + competition + ENV, random = ~1|composition, LM),
  lme(stab.comm ~ stab.pop + compensatory + statistical, random = ~1|composition, LM)
)
summary(sem_lme, LM, conditional = T, standardize = "scale")

# Final model
sem_lme2 <- psem(
  compensatory %~~% statistical,
  lme(stab.pop ~ TEMP + competition + ENV, random = ~1|composition, LM),
  lme(compensatory ~ competition, random = ~1|composition, LM),
  lme(statistical ~ TEMP + competition + ENV, random = ~1|composition, LM),
  lme(stab.comm ~ stab.pop + compensatory + statistical, random = ~1|composition, LM)
)
summary(sem_lme2, LM, conditional = T, standardize = "scale")

AIC(sem_lme2, aicc=T)

## Figure 4 - SEM (ND & RFD)
mix_data <- read.csv("Mixture_SEM.csv")
mix_data[, c(11:17)] <- apply(mix_data[, c(11:17)], 2, log10 )
head(mix_data)

# Initial model
sem5 <- psem(
  ND %~~% RFD,
  compensatory %~~% statistical,
  lm(ND ~ TEMP, mix_data),
  lm(RFD ~ TEMP, mix_data),
  lm(stab.pop ~ TEMP + ND + RFD + ENV, mix_data),
  lm(compensatory ~ TEMP + ND + RFD + ENV, mix_data),
  lm(statistical ~ TEMP + ND + RFD + ENV, mix_data),
  lm(stab.comm ~ ND + RFD + stab.pop + compensatory + statistical, mix_data)
)
summary(sem5, mix_data, conditional = T, standardize = "scale")

# Final model
sem6 <- psem(
  ND %~~% RFD,
  lm(ND ~ TEMP, mix_data),
  lm(stab.pop ~ TEMP + RFD + ENV, mix_data),
  lm(compensatory ~ TEMP + ND + ENV, mix_data),
  lm(statistical ~ TEMP + ND + RFD + ENV, mix_data),
  lm(stab.comm ~ TEMP + RFD + stab.pop + compensatory + statistical, mix_data)
)
summary(sem6, mix_data, conditional = T, standardize = "scale")
AIC(sem6, aicc=T)
