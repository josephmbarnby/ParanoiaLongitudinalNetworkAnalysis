# Libraries ---------------------------------------------------------------

library(tidyverse)
library(bootnet)
library(psychonetrics)
library(qgraph)
library(mgm)
library(graphicalVAR)
library(tidyverse)
library(mlVAR)
library(foreign)
library(mice)
library(Hmisc)
library(imputeTS)
library(patchwork)
library(ggridges)
library(ordinal)
library(standardize)
library(ggsci)

#### PLEASE NOTE - IT IS THE WISH OF THE ORIGINAL DATA OWNER THAT THE
#### RAW DATA NOT BE UPLOADED ONTO GITHUB. INSTEAD, WE HAVE PLACED THE
#### COVARIANCES MATRICES USED TO ESTIMATE THE NETWORKS OM GITHUB TO
#### ENABLE REPLICATION OF THE RESULTS, AS WELL AS ALL THE ANALYSIS CODE
#### USED TO FILTER, CLEAN, IMPUTE, AND MODEL THE DATA.


# Delusions Network -------------------------------------------------------


## Data Load, Filter & Imputation ---------------------------------------------------------------

#data_wide <- read.csv('')
#data_long <- read.csv('')

# Clean

#data_filter <- data_long %>%
#  pivot_longer(c(Delus_ThoughtDissF:Delus_PersecF,
#                 Delus_GrandF:Delus_BizarreF), 'Delusion', values_to = 'Score') %>%
#  group_by(Delusion, Time) %>%
#  count(Score) %>%
#  arrange(Time, Score) %>%
#  pivot_wider(names_from = 'Delusion', values_from = 'n')
#
#  data_filter %>%
#  filter(Score %in% 1:3) %>%
#  mutate(Score = ifelse(Score == 1, 'Not Present', ifelse(Score == 2, 'Borderline', 'Present')),
#         Score = factor(Score, levels = c('Not Present', 'Borderline', 'Present'), ordered = T)) %>%
#  rename('Bizarre' = 3, 'Grandiose' = 4, 'Persecutory' = 5, 'Reference' = 6, 'Religious' = 7, 'Thought \nDissemination' = 8) %>%
#  pivot_longer(3:8, 'Delusions', values_to = 'Cases') %>%
#    ggplot(aes(Time, Cases, color = Delusions))+
#      geom_point(size = 3)+
#      geom_line(size = 1)+
#      scale_color_jama()+
#      scale_x_continuous(breaks = 1:6, labels = as.factor(c(2, 4.5, 7.5, 10, 15, 20)))+
#      scale_y_continuous(name = 'Case Count')+
#      facet_wrap(~Score, nrow = 3, scales = 'free_y')+
#      theme_bw()+
#      theme(text = element_text(size = 18),
#            strip.background.x = element_rect(fill = NA),
#            strip.text.x = element_text(face = 'bold'),
#            legend.position = 'top',
#            legend.title = element_blank())
#
#  data_filter %>%
#  filter(Score %in% 1:3) %>%
#  mutate(Score = ifelse(Score == 1, 'Not Present', ifelse(Score == 2, 'Borderline', 'Present')),
#         Score = factor(Score, levels = c('Present', 'Borderline', 'Not Present'), ordered = T)) %>%
#  rename('Bizarre' = 3, 'Grandiose' = 4, 'Persecutory' = 5, 'Reference' = 6, 'Religious' = 7, 'Thought Dissemination' = 8) %>%
#  pivot_longer(3:8, 'Delusions', values_to = 'Cases') %>%
#    ggplot(aes(Cases, Delusions, fill = Delusions))+
#      geom_density_ridges(alpha = 0.7, scale = 0.95)+
#      scale_fill_jama()+
#      #scale_x_continuous(breaks = 1:6, labels = as.factor(c(2, 4.5, 7.5, 10, 15, 20)))+
#      #scale_y_continuous(name = 'Case Count')+
#      facet_wrap(~Score, scales = 'free_x') +
#      theme_bw()+
#      theme(text = element_text(size = 18),
#            strip.background.x = element_rect(fill = NA),
#            strip.text.x = element_text(face = 'bold'),
#            axis.title.y = element_blank(),
#            axis.text.y = element_blank(),
#            axis.ticks.y = element_blank(),
#            legend.position = 'top',
#            legend.title = element_blank())

# Variables

delus_net_var <- c('ID', 'AgeF1', 'AgeF2','AgeF3','AgeF4','AgeF5','AgeF6','Sex', 'IQ',
                   'MedF1', 'MedF2', 'MedF3', 'MedF4', 'MedF5', 'MedF6',

                   'Delus_ThoughtDissF1', 'Delus_ThoughtDissF2', 'Delus_ThoughtDissF3', 'Delus_ThoughtDissF4', 'Delus_ThoughtDissF5', 'Delus_ThoughtDissF6',
                   'Delus_ReferenceF1', 'Delus_ReferenceF2', 'Delus_ReferenceF3', 'Delus_ReferenceF4', 'Delus_ReferenceF5', 'Delus_ReferenceF6',
                   'Delus_PersecF1', 'Delus_PersecF2', 'Delus_PersecF3', 'Delus_PersecF4', 'Delus_PersecF5', 'Delus_PersecF6',
                   'Delus_BizarreF1', 'Delus_BizarreF2', 'Delus_BizarreF3', 'Delus_BizarreF4', 'Delus_BizarreF5', 'Delus_BizarreF6',
                   'Delus_GrandF1', 'Delus_GrandF2', 'Delus_GrandF3', 'Delus_GrandF4', 'Delus_GrandF5', 'Delus_GrandF6',
                   'Delus_ReligF1', 'Delus_ReligF2', 'Delus_ReligF3', 'Delus_ReligF4', 'Delus_ReligF5', 'Delus_ReligF6'
                   )

n_t        <- 6 # time points
n_v        <- length(delus_net_var[-1:-15])/6 # number of variables
design_mat <- matrix(delus_net_var[-1:-15],nrow=n_v,ncol=n_t, byrow = T) # design matrix for network & regression
rownames(design_mat) <- substr(design_mat[,1],7,nchar(design_mat[,1])-2)

# Imputation and cleaning

#delus_net <- data_wide %>%
#  dplyr::select(delus_net_var) %>%
#  mutate(across(delus_net_var[-10:-15], ~ifelse(.x == 0, 1, .x)))
#
#md.pattern(delus_net, rotate.names = T) # Check missing data
#delus_net_impute <- round(na_interpolation(delus_net), 0)
##delus_net_impute <- delus_net
#
#x <- ts(data_long, frequency = 6)
#x.withoutNA <- round(na_interpolation(x), 0) %>%
#  as.data.frame() %>%
#  mutate(Time = factor(Time, levels = 1:6, ordered = T),
#        across(substr(design_mat[,1],1,nchar(design_mat[,1])-1), ~ifelse(.x == 0, 1, .x)),
#        ID = factor(ID))

## Regress out age and sex -------------------------------------------------

#for (k in 1:n_v){
#for (i in 1:n_t){
#  x <- names(delus_net_impute[design_mat[k,i]])
#  y1<- names(delus_net_impute[paste('AgeF',i, sep = '')])
#  y2<- names(delus_net_impute[paste('MedF',i, sep = '')])
#  f <- paste(x, "~", paste(y2,'Sex', 'IQ', sep = ' + '))
#  resid <- lm(f, delus_net_impute)
#  delus_net_impute[,x] <- resid$residuals
#}
#}

# Center, scale & detrend
# delus_net_impute <- delus_net_impute %>% mutate(across(delus_net_var[-1:-15], ~ scale(.x, center = T, scale = T)))

#ggpubr::ggarrange(
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_PersecF1:Delus_PersecF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Persecutory', x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_ReferenceF1:Delus_ReferenceF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Referential', x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_BizarreF1:Delus_BizarreF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Bizarre', x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_ThoughtDissF1:Delus_ThoughtDissF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Thought Dissemination', x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_GrandF1:Delus_GrandF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Grandiose', x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#ggplot(delus_net_impute %>%
#         pivot_longer(Delus_ReligF1:Delus_ReligF6, 'Time', values_to = 'Value'),
#       aes(Time, Value)) +
#  geom_jitter(alpha = 0.5) +
#  geom_line(aes(group = ID), alpha = 0.1) +
#  stat_summary(color = 'red', size = 1, fun.data = mean_cl_normal)+
#  labs(title = 'Religious, x = 'Time (Years After Index)')+
#  scale_x_discrete(labels = c(2, 4.5, 7.5, 10, 15, 20))+
#  theme_bw()+
#  theme(text = element_text(size = 20)),
#nrow = 3, ncol = 2
#)

## Generate Network ---------------------------------------------------------------

#delus_only_data <- read.csv('Data/Delusions_Imputed_Data.csv') %>% dplyr::select(-X)

#model <- panelgvar(
#  data = delus_only_data, # data
#  vars = design_mat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
#  estimator = 'ULS',
#  storedata = T
#)

#Set model search parms
#alpha  <- 0.01
#adjust <- "none"

#Run models
#delus_model_sat    <- model %>% runmodel()
#delus_model_spa    <- delus_model_sat %>% prune(alpha = alpha, adjust = adjust)

#compare(
#  saturate = delus_model_sat,
#  sparse   = delus_model_spa
#)

#winning_delus <- delus_model_sat

#Check fit
#winning_delus %>% fit
#
#check signif
#winning_delus@parameters %>% filter(matrix == 'beta', p < 0.05)
#winning_delus@parameters %>% filter(matrix == 'omega_zeta_within', p < 0.05)
#winning_delus@parameters %>% filter(matrix == 'omega_zeta_between', p < 0.05)

#Generate data from network
#generate_data <- winning_delus %>% generate

## Recovery ----------------------------------------------------------------
#colnames(generate_data) <- colnames(delus_only_data)
#model_rec <- panelgvar(
#  data = generate_data, # data
#  vars = design_mat, # The design matrix, with a row indicating a variable and a column a wave of measurements.
#  estimator = 'ULS'
#)

#delus_model_sat_rec <- model_rec %>% runmodel()
#delus_model_sat_rec %>% fit

# Extract networks:
#delus_temporal        <- getmatrix(winning_delus, "beta")
#delus_contemporaneous <- getmatrix(winning_delus, "omega_zeta_within")
#delus_between         <- getmatrix(winning_delus, "omega_zeta_between")

#delus_covariances <- list(temp = delus_temporal, contemp = delus_contemporaneous, between = delus_between)

delus_covariances <- readRDS('data/delus_covariances.RData')

delus_temporal        <- delus_covariances[[1]]
delus_contemporaneous <- delus_covariances[[2]]
delus_between         <- delus_covariances[[3]]

labels     <- rownames(design_mat)

# Plot networks:
layout(t(1:3))

labelsD  <- c("THD", "REF", "PER", "BIZ", "GRD", "REL")
Groups   <- as.factor(c(rep('Delusions', 2), rep('Social', 4)))
ColorsD  <- rep("#56B4E9", 6)

g1 <- qgraph(delus_temporal, layout = "spring", labels = labelsD, colors = ColorsD,
       #title = "Temporal - Delusions",
       vsize = 15, vsize2 = 30, label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

g2 <- qgraph(delus_contemporaneous, layout = g1$layout, labels = labelsD,colors = ColorsD,
       #title = "Contemporaneous - Delusions",
       vsize = 15, vsize2 = 20,label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2,edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

g3 <- qgraph(delus_between, layout = g1$layout, labels = labelsD,colors = ColorsD,
       #title = "Between-Individuals - Delusions",
       vsize = 15, vsize2 = 20,label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2,edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

#Centrality
qgraph::centralityPlot(g1)|
qgraph::centralityPlot(g2)|
qgraph::centralityPlot(g3)

## Bootstrap ---------------------------------------------------------------

reps <- 500
#delus_boot_temporal <- list()
#delus_boot_contempo <- list()
#delus_boot_betweens <- list()
#av_fitness          <- list()
#
##This will take a long time
#for (i in 1:reps) {
#
#  defaultW <- getOption("warn")
#  options(warn = -1)
#
#  rowstouse        <- base::sort(sample(nrow(delus_net), nrow(delus_net)*.75))
#  ni_data          <- delus_net[rowstouse,]
#  cv_data          <- round(na_interpolation(ni_data), 0)
#
#  for (k in 1:n_v){
#  for (h in 1:n_t){
#    x <- names(cv_data[design_mat[k,h]])
#    y1<- names(cv_data[paste('AgeF',h, sep = '')])
#    y2<- names(cv_data[paste('MedF',h, sep = '')])
#    f <- paste(x, "~", paste(y2,'Sex', 'IQ', sep = ' + '))
#    resid <- lm(f, cv_data)
#    cv_data[,x] <- resid$residuals
#  }
#  }
#
#  # Center and scale
#  cv_data <- cv_data %>% mutate(across(delus_net_var[-1:-15], ~ scale(.x, center = T, scale = T)))
#
#
#  #Set up model
#  model_boot <- panelgvar(
#    data = cv_data,
#    vars = design_mat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
#    estimator = 'ULS'
#    )
#
#  delus_boot <- model_boot %>%
#    runmodel()
#
#  delus_boot_temporal[[i]] <- getmatrix(delus_boot, "beta")
#  delus_boot_contempo[[i]] <- getmatrix(delus_boot, "omega_zeta_within")
#  delus_boot_betweens[[i]] <- getmatrix(delus_boot, "omega_zeta_between")
#  av_fitness[[i]]          <- delus_boot %>% fit() %>% as.data.frame()
#
#  cat(paste('\n Completed ', i, ' of ', reps))
#
#  options(warn = defaultW)
#
#}

delus_boot_temporal <- readRDS('Data/delus_boot_temporal.RData')
delus_boot_contempo <- readRDS('Data/delus_boot_contempo.RData')
delus_boot_betweens <- readRDS('Data/delus_boot_betweens.RData')

av_temporal <- sortMe(delus_boot_temporal, n_v, reps)
av_contempo <- sortMe(delus_boot_contempo, n_v, reps)
av_betweens <- sortMe(delus_boot_betweens, n_v, reps)

layout(t(1:3))
b1 <- qgraph(av_temporal$mean, layout = g1$layout, labels = labelsD, color = ColorsD,
       title = "Temporal - Delusions Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       threshold = 0.1,
       details = T)
box("figure")

b2 <- qgraph(av_contempo$mean, layout = g1$layout, labels = labelsD, color = ColorsD,
       title = "Contemporaneous - Delusions Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       threshold = 0.1,
       details = T)
box("figure")

b3 <- qgraph(av_betweens$mean, layout = g1$layout, labels = labelsD, color = ColorsD,
       title = "Between-Individuals - Delusions Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       threshold = 0.1,
       details = T)
box("figure")

#Centrality
qgraph::centralityPlot(b1)|
qgraph::centralityPlot(b2)|
qgraph::centralityPlot(b3)

## Plot Intervals ----------------------------------------------------------

intervals <- av_temporal$mean %>%
  as.data.frame() %>%
  rename('THD'=1,'REF'=2,'PER'=3,'BIZ'=4,'GRD'=5,'REL'=6) %>%
  mutate(V2 = labelsD) %>%
  pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_temporal$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_temporal$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = delus_temporal %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1))

intervalsC <- av_contempo$mean %>%
  as.data.frame() %>%
  rename('THD'=1,'REF'=2,'PER'=3,'BIZ'=4,'GRD'=5,'REL'=6) %>%
  mutate(V2 = labelsD) %>%
  pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_contempo$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_contempo$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = delus_contemporaneous %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1)) %>%
  filter(Correlation != 0)
intervalsC <- intervalsC[seq(1, nrow(intervalsC), 2), ]

intervalsB <- av_betweens$mean %>%
  as.data.frame() %>%
  rename('THD'=1,'REF'=2,'PER'=3,'BIZ'=4,'GRD'=5,'REL'=6) %>%
  mutate(V2 = labelsD) %>%
  pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_betweens$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_betweens$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = delus_between %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_v), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1)) %>%
  filter(Correlation != 0)
intervalsB <- intervalsB[seq(1, nrow(intervalsB), 2), ]

#tiff("DelusionsOnlyIntervals.tiff", units="px", width=1000, height=600, res=500)
ggplot(intervals)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  #geom_point(     aes(Recovered,   reorder(V1, Correlation), color = 'Recovered'), alpha = 0.5)+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
#dev.off()
#
ggplot(intervalsC)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggplot(intervalsB)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

# Social Network -------------------------------------------------------

## Data Load, Filter & Imputation ---------------------------------------------------------------

# Variables

social_net_var <- c('ID', 'AgeF1', 'AgeF2','AgeF3','AgeF4','AgeF5','AgeF6','Sex', 'IQ',
                    'MedF1', 'MedF2', 'MedF3', 'MedF4', 'MedF5', 'MedF6',

                   'Delus_ReferenceF1', 'Delus_ReferenceF2', 'Delus_ReferenceF3', 'Delus_ReferenceF4', 'Delus_ReferenceF5', 'Delus_ReferenceF6',
                   'Delus_PersecF1', 'Delus_PersecF2', 'Delus_PersecF3', 'Delus_PersecF4', 'Delus_PersecF5', 'Delus_PersecF6',

                   'N_close_friendsF1', 'N_close_friendsF2', 'N_close_friendsF3', 'N_close_friendsF4', 'N_close_friendsF5', 'N_close_friendsF6',
                   'Quality_friendsF1', 'Quality_friendsF2', 'Quality_friendsF3', 'Quality_friendsF4', 'Quality_friendsF5', 'Quality_friendsF6',
                   'Time_w_friendsF1', 'Time_w_friendsF2', 'Time_w_friendsF3', 'Time_w_friendsF4', 'Time_w_friendsF5', 'Time_w_friendsF6',
                   'Soc_SatisfactionF1', 'Soc_SatisfactionF2', 'Soc_SatisfactionF3', 'Soc_SatisfactionF4', 'Soc_SatisfactionF5', 'Soc_SatisfactionF6'

)
n_ts        <- 6 # time points
n_vs        <- length(social_net_var[-1:-15])/6 #number of variables
design_matS <- matrix(social_net_var[-1:-15],nrow=n_vs,ncol=n_ts, byrow = T) #design matrix for network & regression

#social_net <- data_wide %>%
#  dplyr::select(all_of(social_net_var)) %>%
#  mutate(across(social_net_var[-10:-15], ~ifelse(.x == 0, NA, .x)),
#         #across(N_close_friendsF1:N_close_friendsF6, ~ifelse(.x == 5, 4, .x)),
#         across(N_close_friendsF1:N_close_friendsF6, ~recode(.x, `5`=1L,`4`=2L,`3`=3L,`2`=4L, `1` = 5L)),
#         across(Time_w_friendsF1:Time_w_friendsF6, ~recode(.x, `5`=1L,`4`=2L,`2`=3L,`1`=4L)),
#         across(Quality_friendsF1:Quality_friendsF6, ~recode(.x, `6`=1L, `5`=1L,`4`=2L,`2`=3L,`1`=4L)),
#         across(Soc_SatisfactionF1:Soc_SatisfactionF6, ~recode(.x, `5`=1L,`4`=2L, `3`= 3L, `2`=4L,`1`=5L)),
#         across(Soc_SatisfactionF1:Soc_SatisfactionF6, ~ifelse(.x==9, NA, .x)))

# Imputation

#md.pattern(social_net, rotate.names = T) # Check missing data
#social_net_impute <- round(na_interpolation(social_net), 0)

## Regress out age and sex -------------------------------------------------

#for (k in 1:n_vs){
#  for (i in 1:n_ts){
#    x <- names(social_net_impute[design_matS[k,i]])
#    y1<- names(social_net_impute[paste('AgeF',i, sep = '')])
#    y2<- names(social_net_impute[paste('MedF',i, sep = '')])
#    f <- paste(x, "~", paste(y1,y2,'Sex','IQ', sep = ' + '))
#    resid <- lm(f, social_net_impute)
#    social_net_impute[,x] <- resid$residuals
#  }
#}
#
#social_net_impute <- social_net_impute %>% mutate(across(social_net_var[-1:-15], ~ scale(.x, center = T, scale = T)))


## Generate Network ---------------------------------------------------------------

#Social_Data <- read.csv('Data/Social_Imputed_Data.csv') %>% dplyr::select(-X)
#
#alphas  <- 0.01
#adjusts <- "none"
#
#soc_model <- panelgvar(
#  data = Social_Data, # data
#  vars = design_matS, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
#  estimator = 'ULS',
#  storedata = T
#  )
#
#social_model_sat <- soc_model %>% runmodel()
#social_model_spa <- social_model_sat %>% prune(alpha = alphas)
#
#compare(
#  saturate = social_model_sat,
#  sparse   = social_model_spa
#)
#
#soc_model_winner <- social_model_sat
#soc_model_winner %>% fit
#soc_model_winner %>% parameters
#soc_model_winner@parameters <- soc_model_winner@parameters %>%
#  mutate(est = ifelse(matrix == 'omega_zeta_within' & p < 0.05, est, 0),
#         est = ifelse(matrix == 'omega_zeta_between' & p < 0.05, est, 0))
#
## Extract networks:
#social_temporal        <- getmatrix(soc_model_winner, "beta")
#social_contemporaneous <- getmatrix(soc_model_winner, "omega_zeta_within")
#social_between         <- getmatrix(soc_model_winner, "omega_zeta_between")

#social_covariances <- list(temp = social_temporal, contemp = social_contemporaneous, between = social_between)

social_covariances <- readRDS('data/social_covariances.RData')

social_temporal        <- social_covariances[[1]]
social_contemporaneous <- social_covariances[[2]]
social_between         <- social_covariances[[3]]

# Labels:
labelsS <- c("REF","PER", 'NCF', 'QOF', 'TWF', 'SAT')
Groups  <- as.factor(c(rep('Delusions', 2), rep('Social', 4)))
Colors  <- c("#56B4E9", "#56B4E9", rep('#F6AE2D', 4))
# Plot networks:
layout(t(1:3))
s1G <- qgraph(social_temporal, layout = "spring", labels = labelsS, color = Colors,
       #title = "Temporal - Social",
       vsize = 15, vsize2 = 20, repulsion = 0.5, label.cex = 1.5,
       mar = rep(6,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

s2G <- qgraph(social_contemporaneous, layout = s1G$layout, labels = labelsS,color = Colors,
       #title = "Contemporaneous - Social",
       vsize = 15, vsize2 = 20, label.cex = 1.5,
       mar = rep(6,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

s3G <- qgraph(social_between, layout = s1G$layout, labels = labelsS, color = Colors,
       #title = "Between-Individuals - Social",
       vsize = 15, vsize2 = 20, label.cex = 1.5,
       mar = rep(6,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind")
box("figure")

#Centrality
qgraph::centralityPlot(s1G)|
qgraph::centralityPlot(s2G)|
qgraph::centralityPlot(s3G)

## Recovery ----------------------------------------------------------------

generate_dataS   <- soc_model_winner %>% generate
model_recS <- panelgvar(
  data = generate_dataS, # data
  vars = design_matS, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
  estimator = 'ULS'
)

delus_model_sat_recS <- model_recS %>% runmodel()
delus_model_sat_recS %>% fit

## Bootstrap ---------------------------------------------------------------

reps <- 500
#delus_boot_temporalS2 <- list()
#delus_boot_contempoS2 <- list()
#delus_boot_betweensS2 <- list()
#av_fitnessS2          <- list()

#This will take a long time
#for (i in 1:reps) {
#
#  defaultW <- getOption("warn")
#  options(warn = -1)
#
#  rowstouse        <- base::sort(sample(nrow(social_net), nrow(social_net)*.75))
#  ni_dataS         <- social_net[rowstouse,]
#  cv_dataS         <- round(na_interpolation(ni_dataS), 0)
#
#  for (k in 1:n_vs){
#  for (h in 1:n_ts){
#    x <- names(cv_dataS[design_matS[k,h]])
#    y1<- names(cv_dataS[paste('AgeF',h, sep = '')])
#    y2<- names(cv_dataS[paste('MedF',h, sep = '')])
#    f <- paste(x, "~", paste(y2,'Sex', 'IQ', sep = ' + '))
#    resid <- lm(f, cv_dataS)
#    cv_dataS[,x] <- resid$residuals
#  }
#  }
#
#  # Center and scale
#  cv_dataS <- cv_dataS %>% mutate(across(social_net_var[-1:-15], ~ scale(.x, center = T, scale = T)))
#
#  #Set up model
#  model_bootS <- panelgvar(
#    data = cv_dataS,
#    vars = design_matS, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
#    estimator = 'ULS'
#  )
#
#  delus_bootS <- model_bootS %>%
#    runmodel()
#
#  delus_boot_temporalS2[[i]] <- getmatrix(delus_bootS, "beta")
#  delus_boot_contempoS2[[i]] <- getmatrix(delus_bootS, "omega_zeta_within")
#  delus_boot_betweensS2[[i]] <- getmatrix(delus_bootS, "omega_zeta_between")
#  av_fitnessS2[[i]]          <- delus_bootS %>% fit() %>% as.data.frame()
#
#  cat(paste('\n Completed ', i, ' of ', reps))
#
#  options(warn = defaultW)
#
#  saveRDS(av_fitnessS2, file="delus_boot_social_av_fitness2.RData")
#  saveRDS(delus_boot_temporalS2, file="delus_boot_social_temporal2.RData")
#  saveRDS(delus_boot_contempoS2, file="delus_boot_social_contempo2.RData")
#  saveRDS(delus_boot_betweensS2, file="delus_boot_social_betweens2.RData")
#
#}

delus_boot_temporalS2 <- readRDS('Data/delus_boot_social_temporal2.RData')
delus_boot_contempoS2 <- readRDS('Data/delus_boot_social_contempo2.RData')
delus_boot_betweensS2 <- readRDS('Data/delus_boot_social_betweens2.RData')

av_temporalS <- sortMe(delus_boot_temporalS2, n_vs, reps)
av_contempoS <- sortMe(delus_boot_contempoS2, n_vs, reps)
av_betweensS <- sortMe(delus_boot_betweensS2, n_vs, reps)

layout(t(1:3))
bs1 <- qgraph(av_temporalS$mean, layout = s1G$layout, labels = labelsS,color = Colors,
       title = "Temporal - Social Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       #minimum = 0.1,
       threshold = 0.1)
box("figure")

bs2 <- qgraph(av_contempoS$mean, layout = s1G$layout, labels = labelsS,color = Colors,
       title = "Contemporaneous - Social Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       threshold = 0.1)
box("figure")

bs3 <- qgraph(av_betweensS$mean, layout = s1G$layout, labels = labelsS,color = Colors,
       title = "Between-Individuals - Social Bootstrap",
       vsize = 15, vsize2 = 20,
       mar = rep(8,4), asize = 6,edge.labels = T, edge.label.cex = 2, edge.label.position = 0.4,
       theme = "colorblind",
       threshold = 0.1)
box("figure")

#Centrality
qgraph::centralityPlot(bs1)|
qgraph::centralityPlot(bs2)|
qgraph::centralityPlot(bs3)

## Plot Intervals ----------------------------------------------------------

social_intervals <- av_temporalS$mean %>%
  as.data.frame() %>%
  rename('REF'=1,'PER'=2,'NCF'=3,'QOF'=4,'TWF'=5,'SAT'=6) %>%
  mutate(V2 = labelsS) %>%
  pivot_longer(1:n_vs, 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_temporalS$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:n_vs, 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_temporalS$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:n_vs, 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = social_temporal %>%
           as.data.frame() %>%
           pivot_longer(1:n_vs, 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1))

social_intervalsC <- av_contempoS$mean %>%
  as.data.frame() %>%
  rename('REF'=1,'PER'=2,'NCF'=3,'QOF'=4,'TWF'=5,'SAT'=6) %>%
  mutate(V2 = labelsS) %>%
  pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_contempoS$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_contempoS$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = social_contemporaneous %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1)) %>%
  filter(Correlation != 0)
social_intervalsC <- social_intervalsC[seq(1, nrow(social_intervalsC), 2), ]

social_intervalsB <- av_betweensS$mean %>%
  as.data.frame() %>%
  rename('REF'=1,'PER'=2,'NCF'=3,'QOF'=4,'TWF'=5,'SAT'=6) %>%
  mutate(V2 = labelsS) %>%
  pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
  arrange(Correlation) %>%
  mutate(CI_U = av_betweensS$upperCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         CI_L = av_betweensS$lowerCI %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         RealCorr = social_between %>%
           as.data.frame() %>%
           pivot_longer(1:all_of(n_vs), 'V1', values_to = 'Correlation') %>%
           arrange(Correlation) %>%
           dplyr::select(Correlation) %>%
           unlist(),
         Same = ifelse(V1 == V2, '#DA7422', 'black'),
         AlphaCorr  = ifelse((CI_U > 0 & CI_L < 0) |
                             (CI_U < 0 & CI_L > 0), 0.001, 0.1),
         V1 = paste(V2, '-', V1)) %>%
  filter(Correlation != 0)
social_intervalsB <- social_intervalsB[seq(1, nrow(social_intervalsB), 2), ]

ggplot(social_intervals)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  #geom_point(     aes(Recovered,   reorder(V1, Correlation), color = 'Recovered'), alpha = 0.5)+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggplot(social_intervalsC)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  #geom_point(     aes(Recovered,   reorder(V1, Correlation), color = 'Recovered'), alpha = 0.5)+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggplot(social_intervalsB)+
  geom_vline(xintercept = c(-0.1, 0.1), alpha = 0.5)+
  geom_point(     aes(Correlation, reorder(V1, Correlation), alpha = AlphaCorr, color = 'Bootstrap'))+
  geom_pointrange(aes(Correlation, reorder(V1, Correlation), xmin = CI_U, xmax = CI_L, color = 'Bootstrap'), alpha = 0.5)+
  geom_point(     aes(RealCorr, V1, color = 'Full Data'), alpha = 1)+
  scale_alpha(guide = 'none', range = c(0.05, 0.75))+
  scale_color_manual(name = 'Network', values = c( 'black', '#D64933'))+
  labs(x = 'Estimate')+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.position = c(0.75, 0.25),
        legend.background = element_rect(color = 'black'),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

# Supplementary Material Analysis -------------------------------------------------------

# Variables

S1delus_net_var <- c('ID', 'AgeF1', 'AgeF2','AgeF3','AgeF4','AgeF5','AgeF6','Sex', 'IQ',
                   'MedF1', 'MedF2', 'MedF3', 'MedF4', 'MedF5', 'MedF6',

                   'Delus_ThoughtDissF1', 'Delus_ThoughtDissF2', 'Delus_ThoughtDissF3', 'Delus_ThoughtDissF4', 'Delus_ThoughtDissF5', 'Delus_ThoughtDissF6',
                   'Delus_ReferenceF1', 'Delus_ReferenceF2', 'Delus_ReferenceF3', 'Delus_ReferenceF4', 'Delus_ReferenceF5', 'Delus_ReferenceF6',
                   'Delus_PersecF1', 'Delus_PersecF2', 'Delus_PersecF3', 'Delus_PersecF4', 'Delus_PersecF5', 'Delus_PersecF6',
                   'Delus_BizarreF1', 'Delus_BizarreF2', 'Delus_BizarreF3', 'Delus_BizarreF4', 'Delus_BizarreF5', 'Delus_BizarreF6',
                   'Delus_GrandF1', 'Delus_GrandF2', 'Delus_GrandF3', 'Delus_GrandF4', 'Delus_GrandF5', 'Delus_GrandF6',
                   'Delus_ReligF1', 'Delus_ReligF2', 'Delus_ReligF3', 'Delus_ReligF4', 'Delus_ReligF5', 'Delus_ReligF6',
                   'Delus_NihilismF1', 'Delus_NihilismF2', 'Delus_NihilismF3', 'Delus_NihilismF4', 'Delus_NihilismF5', 'Delus_NihilismF6'
                   )

S1n_t        <- 6 # time points
S1n_v        <- length(S1delus_net_var[-1:-15])/6 # number of variables
S1design_mat <- matrix(S1delus_net_var[-1:-15],nrow=S1n_v,ncol=S1n_t, byrow = T) # design matrix for network & regression
rownames(S1design_mat) <- substr(S1design_mat[,1],7,nchar(S1design_mat[,1])-2)
#
## Imputation and cleaning
#
#S1delus_net <- data_wide %>%
#  dplyr::select(S1delus_net_var) %>%
#  mutate(across(S1delus_net_var[-10:-15], ~ifelse(.x == 0, 1, .x)))
#
#md.pattern(S1delus_net) # Check missing data
#S1delus_net_impute <- round(na_interpolation(S1delus_net), 0)
#
## Regress out age and sex -------------------------------------------------
#
#for (k in 1:S1n_v){
#for (i in 1:S1n_t){
#  x <- names(S1delus_net_impute[S1design_mat[k,i]])
#  y1<- names(S1delus_net_impute[paste('AgeF',i, sep = '')])
#  y2<- names(S1delus_net_impute[paste('MedF',i, sep = '')])
#  f <- paste(x, "~", paste(y2,'Sex', 'IQ', sep = ' + '))
#  resid <- lm(f, S1delus_net_impute)
#  S1delus_net_impute[,x] <- resid$residuals
#}
#}
#
## Center and scale
#S1delus_net_impute <- S1delus_net_impute %>% mutate(across(S1delus_net_var[-1:-15], ~ scale(.x, center = T, scale = T)))

# Generate Network ---------------------------------------------------------------

#supplementary_dat <- read.csv('Data/SupplementaryAnalysis.csv') %>% dplyr::select(-X)
#
#S1model <- panelgvar(
#  data = supplementary_dat, # data
#  vars = S1design_mat, # The design matrix, with a row indicating a variable and a column a wave of measurements. Note that NA indicates missing variables
#  estimator = 'ULS',
#  storedata = T
#)
#
##Set model search parms
#alpha  <- 0.01
#adjust <- "none"
#
##Run models
#S1delus_model_sat    <- S1model %>% runmodel()
#S1winning_delus <- S1delus_model_sat
#
##Check fit
#S1winning_delus %>% fit
#S1winning_delus@parameters %>% filter(matrix == 'beta', p < 0.05)
#S1winning_delus@parameters %>% filter(matrix == 'omega_zeta_within', p < 0.05)
#S1winning_delus@parameters %>% filter(matrix == 'omega_zeta_between', p < 0.05)

# Extract networks:
#S1delus_temporal        <- getmatrix(S1winning_delus, "beta")
#S1delus_contemporaneous <- getmatrix(S1winning_delus, "omega_zeta_within")
#S1delus_between         <- getmatrix(S1winning_delus, "omega_zeta_between")

#supp_covariances <- list(temp = S1delus_temporal, contempo = S1delus_contemporaneous, between = S1delus_between)

supp_covariances <- readRDS('data/supp_covariances.RData')

S1delus_temporal        <- supp_covariances[[1]]
S1delus_contemporaneous <- supp_covariances[[2]]
S1delus_between         <- supp_covariances[[3]]

S1labels     <- rownames(S1design_mat)

# Plot networks:
layout(t(1:3))

labelsS1  <- c("THD", "REF", "PER", "BIZ", "GRD", "REL", 'NIH')
ColorsS1  <- rep("#56B4E9", 7)

S1g1 <- qgraph(S1delus_temporal, layout = "spring", labels = labelsS1, colors = ColorsS1,
       title = "Temporal - Delusions",
       vsize = 15, vsize2 = 30, label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 1, edge.label.position = 0.5,
       theme = "colorblind")
box("figure")

S1g2 <- qgraph(S1delus_contemporaneous, layout = S1g1$layout, labels = labelsS1,colors = ColorsS1,
       title = "Contemporaneous - Delusions",
       vsize = 15, vsize2 = 20,label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 1,edge.label.position = 0.5,
       theme = "colorblind")
box("figure")

S1g3 <- qgraph(S1delus_between, layout = S1g1$layout, labels = labelsS1,colors = ColorsS1,
       title = "Between-Individuals - Delusions",
       vsize = 15, vsize2 = 20,label.cex = 1.5,
       mar = rep(8,4), asize = 6, threshold = 0.1, edge.labels = T, edge.label.cex = 1,edge.label.position = 0.5,
       theme = "colorblind")
box("figure")

#Centrality

qgraph::centralityPlot(S1g1)|
qgraph::centralityPlot(S1g2)|
qgraph::centralityPlot(S1g3)

