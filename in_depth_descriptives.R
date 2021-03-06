################# FUUUCK
library(tidyverse)
library(ggtext)

setwd("C:/Users/Roger/Seafile/Roger_Seafile/SSI-EEG-3/SSI_4_R_project/")

#load data
a4 <- read.csv2("a4_SSI_4.csv")

#okay once again from the top
# first things first: pre-post variable for a4 data
a4[a4$subject %in% c(3,4, 7:18), "lockdown"] <- "pre"
a4[a4$subject %in% c(1,2,5,6, 19:28), "lockdown"] <- "post"

a4$lockdown <- factor(a4$lockdown, levels = c("pre", "post"))

a4$g_ng_count_1_5 <- as.factor(a4$g_ng_count_1_5)

# prep data frames for plotting
# once across all DGs
desc_across <- summarise(group_by(a4, Cond, g_ng_count_1_5), RT = mean(RT))

ord_pos <- ggplot(data = desc_across, mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond))+
  xlab("Ordinal Position")+
  ggtitle("RT by Ordinal Position and Condition")
ggsave("plots/ord_pos.jpg", device = "jpg", dpi = 700)

#mean RT for ord pos by DG and Cond
desc_by_DG <- summarise(group_by(a4, g_ng_count_1_5, Cond, DG), RT = mean(RT))

#plot
ord_pos_by_DG <- ggplot(data = desc_by_DG, mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_DG$DG) +
  xlab("Ordinal Position")+
  ggtitle("Mean RT by Ordinal Position and Condition", subtitle = "for each Repetition")

ggsave("plots/ord_pos_by_DG.jpg", device = "jpg", dpi = 700)

# separate for pre and post lockdown
desc_by_DG_lockdown <- summarise(group_by(a4, g_ng_count_1_5, Cond, DG, lockdown), RT = mean(RT))

#plot
ord_pos_by_DG_lockdown <- ggplot(data = desc_by_DG_lockdown, mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_grid(cols = vars(desc_by_DG_lockdown$DG), rows = vars(desc_by_DG_lockdown$lockdown)) +
  xlab("Ordinal Position")+
  ggtitle("Mean RT by Ordinal Position and Condition", subtitle = "for each Repetition and for pre and post interruption")

ggsave("plots/ord_pos_by_DG_lockdown.jpg", device = "jpg", dpi = 700)

# mean increase per ordinal position by DG and Cond
incr_by_DG <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], g_ng_count_cent, DG, Cond), 
                        RT = mean(RT))
incr_by_DG <- mutate(group_by(incr_by_DG, Cond, DG), diff = diff(RT)/4)

incr_by_DG[incr_by_DG$g_ng_count_cent == -2, "diff"] <- 0


#plot by DG
incr_by_DG$g_ng_count_cent <- as.factor(incr_by_DG$g_ng_count_cent)

plot_incr_by_DG <- ggplot(data = incr_by_DG, mapping = aes(g_ng_count_cent, y = diff, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_wrap(incr_by_DG$DG) +
  xlab("Increase in Ordinal Position")+
  ylab("RT")+
  ggtitle("Mean Difference in RT per Ordinal Position by Condition", subtitle = " for each Repetition")+
  scale_x_discrete(labels = c("ord pos i", "ord pos i+1"))

ggsave("plots/incr_by_DG.jpg", plot = plot_incr_by_DG, device = "jpg", dpi = 700)

#separately for lockdown pre post
# mean increase per ordinal position by DG and Cond
incr_by_DG_lockdown <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], g_ng_count_cent, DG, Cond, lockdown), 
                        RT = mean(RT))
incr_by_DG_lockdown <- mutate(group_by(incr_by_DG_lockdown, Cond, DG, lockdown), diff = diff(RT)/4)

incr_by_DG_lockdown[incr_by_DG_lockdown$g_ng_count_cent == -2, "diff"] <- 0

#plot by DG and lockdown
incr_by_DG_lockdown$g_ng_count_cent <- as.factor(incr_by_DG_lockdown$g_ng_count_cent)

plot_incr_by_DG_lockdown <- ggplot(data = incr_by_DG_lockdown, mapping = aes(g_ng_count_cent, y = diff, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_grid(cols = vars(incr_by_DG_lockdown$DG), rows = vars(incr_by_DG_lockdown$lockdown)) +
  xlab("Increase in Ordinal Position")+
  ylab("RT")+
  ggtitle("Mean Difference in RT per Ordinal Position by Condition", subtitle = " for each Repetition and pre post interruption")+
  scale_x_discrete(labels = c("ord pos i", "ord pos i+1"))

ggsave("plots/incr_by_DG_lockdown.jpg", device = "jpg", dpi = 700)

# mean incr by subject

incr_by_subj <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], g_ng_count_cent, subject, Cond), 
                                 RT = mean(RT))
incr_by_subj <- mutate(group_by(incr_by_subj, Cond, subject), diff = diff(RT)/4)

incr_by_subj[incr_by_subj$g_ng_count_cent == -2, "diff"] <- 0

subj_levels <- numeric()
for (i in 1:14){subj_levels <- c(subj_levels, i, i+14)}

incr_by_subj$subject <- factor(incr_by_subj$subject, levels = subj_levels)

#plot 
incr_by_subj$g_ng_count_cent <- as.factor(incr_by_subj$g_ng_count_cent)

plot_incr_by_subj <- ggplot(data = incr_by_subj, aes(x = g_ng_count_cent, y = diff, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_wrap(incr_by_subj$subject, ncol = 6)+
  xlab("Increase in Ordinal Position")+
  ylab("RT")+
  ggtitle("Mean Difference in RT per Ordinal Position by Condition and Subject", 
          subtitle = "Neighbourng subjects are *mirrors of each other*")+
  scale_x_discrete(labels = c("ord pos i", "ord pos i+1"))+
  theme(plot.subtitle = element_markdown())
ggsave("plots/incr_by_subj.pdf", device = "pdf", dpi = 700, height = 10, width = 12)

#add lockdown
# mean incr by subject and lockdown

incr_by_subj_lock <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], 
                                        g_ng_count_cent, subject, Cond, lockdown), 
                          RT = mean(RT))
incr_by_subj_lock <- mutate(group_by(incr_by_subj_lock, Cond, subject, lockdown), diff = diff(RT)/4)

incr_by_subj_lock[incr_by_subj_lock$g_ng_count_cent == -2, "diff"] <- 0

subj_levels <- numeric()
for (i in 1:14){subj_levels <- c(subj_levels, i, i+14)}

incr_by_subj_lock$subject <- factor(incr_by_subj_lock$subject, levels = subj_levels)

#plot 
incr_by_subj_lock$g_ng_count_cent <- as.factor(incr_by_subj_lock$g_ng_count_cent)

plot_incr_by_subj_lock <- ggplot(data = incr_by_subj_lock, 
                                 aes(x = g_ng_count_cent, y = diff, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond, linetype = lockdown))+
  facet_wrap(incr_by_subj_lock$subject, ncol = 6)+
  xlab("Increase in Ordinal Position")+
  ylab("RT")+
  ggtitle("Mean Difference in RT per Ordinal Position by Condition", 
          subtitle = "separate for Subjects and by Interruption")+
  scale_x_discrete(labels = c("ord pos i", "ord pos i+1"))+
  theme(plot.subtitle = element_markdown())

ggsave("plots/incr_by_subj_lock.jpg", device = "jpg", dpi = 700, height = 10, width = 12)

# mean incr by cat

incr_by_cat <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], g_ng_count_cent, Semantische.Kategorie, Cond), 
                          RT = mean(RT))
incr_by_cat <- mutate(group_by(incr_by_cat, Cond, Semantische.Kategorie), diff = diff(RT)/4)

incr_by_cat[incr_by_cat$g_ng_count_cent == -2, "diff"] <- 0

#plot 
incr_by_cat$g_ng_count_cent <- as.factor(incr_by_cat$g_ng_count_cent)

plot_incr_by_cat <- ggplot(data = incr_by_cat, aes(x = g_ng_count_cent, y = diff, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_wrap(incr_by_cat$Semantische.Kategorie, ncol = 6)+
  xlab("Increase in Ordinal Position")+
  ylab("RT")+
  ggtitle("Mean Difference in RT per Ordinal Position by Condition and Category")+
  scale_x_discrete(labels = c("ord pos i", "ord pos i+1"))+
  theme(plot.subtitle = element_markdown())
ggsave("plots/incr_by_cat.jpg", device = "jpg", dpi = 700, height = 10, width = 12)

## Dip from Ord Pos 4 to 5
#by DG
desc_by_DG_4_5 <- summarise(group_by(a4[a4$g_ng_count_1_5 %in% c("4", "5"), ], 
                                 Cond, DG, g_ng_count_1_5),
                        RT = mean(RT))
# plot it
ord_pos_1_5_by_DG <- ggplot(data = desc_by_DG_4_5, 
                            mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_DG_4_5$DG, ncol = 2) +
  xlab("Ordinal Position") +
  ggtitle("RT at Ordinal Positions 4 and 5 by Condition", subtitle = "for each Repetition")

ggsave("plots/pos_4_5_by_DG.jpg", device = "jpg", dpi = 700)

#by subject
desc_by_subj_4_5 <- summarise(group_by(a4[a4$g_ng_count_1_5 %in% c("4", "5"), ], 
                                       Cond, subject, g_ng_count_1_5),
                              RT = mean(RT))

#plot it (but change levels of subject)
subj_levels <- numeric()
for (i in 1:14){subj_levels <- c(subj_levels, i, i+14)}

desc_by_subj_4_5$subject <- factor(desc_by_subj_4_5$subject, levels = subj_levels)

#plot

ord_pos_by_subj_4_5 <- ggplot(data = desc_by_subj_4_5, 
                          mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_subj_4_5$subject, nrow = 7) +
  xlab("Ordinal Position") +
  ggtitle("RT at Ordinal Positions 4 and 5 by Condition and Subject", 
          subtitle = "Neighboring subjects are *mirrors of each other*") +
  theme(plot.subtitle = ggtext::element_markdown())
ggsave("plots/pos_4_5_by_subj.pdf", device = "pdf", dpi = 700, height = 15, width = 12)

#alternate display of grid
ord_pos_by_subj_4_5_alt <- ggplot(data = desc_by_subj_4_5, 
                          mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_subj_4_5$subject, ncol = 6) +
  xlab("Ordinal Position") +
  ggtitle("RT at Ordinal Positions 4 and 5 by Condition and Subject", 
          subtitle = "Neighboring subjects are *mirrors of each other*") +
  theme(plot.subtitle = ggtext::element_markdown())
ggsave("plots/pos_4_5_by_subj_alt.jpg", device = "jpg", dpi = 700, height = 10, width = 12)

###by cat
desc_by_cat_4_5 <- summarise(group_by(a4[a4$g_ng_count_1_5 %in% c("4", "5"), ], 
                                       Cond, Semantische.Kategorie, g_ng_count_1_5),
                              RT = mean(RT))
#alternate display of grid
ord_pos_by_cat_4_5_alt <- ggplot(data = desc_by_cat_4_5, 
                                  mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_cat_4_5$Semantische.Kategorie, ncol = 6) +
  xlab("Ordinal Position") +
  ggtitle("RT at Ordinal Positions 4 and 5 by Condition and Category") +
  theme(plot.subtitle = ggtext::element_markdown())
ggsave("plots/pos_4_5_by_cat_alt.jpg", device = "jpg", dpi = 700, height = 10, width = 12)


#let's play around with lag (max until 10 o clock)
# add row number to a4
a4 <- mutate(a4, row_num = row_number()) 


### debriefing
colnames(debriefing_SSI_4)[colnames(debriefing_SSI_4) == "ID"] <- "subject"
a4 <- merge(a4, debriefing_SSI_4, by = "subject")

partner_attitude <- colnames(a4)[29:38]
a4 <- mutate(a4, mean_partner = rowMeans(select(a4, all_of(partner_attitude))))

# split in half (median)
a4 <- mutate(a4, med_split_att = mean_partner >= median(mean_partner))

a4$med_split_att <- as.factor(a4$med_split_att)
a4$komp_koop <- as.factor(a4$komp_koop)


#plot basic Cond by Ord Pos by median split
desc_med_split <- summarise(group_by(a4, Cond, g_ng_count_1_5, med_split_att), RT = mean(RT))

#bar graph first
sum_med_split <- summarise(group_by(a4, subject, med_split_att), n = n())

#bar graph of komp_koop
bar_med_split <- ggplot(data = sum_med_split, aes(med_split_att))+
  geom_bar()

plot_med_split <- ggplot(data =desc_med_split, aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_wrap(vars(med_split_att))
ggsave("plots/med_split.jpg", device = "jpg", dpi = 700)

# by komp_koop
sum_komp_koop <- summarise(group_by(a4, subject, komp_koop), n = n())

#bar graph of komp_koop
bar_komp_koop <- ggplot(data = sum_komp_koop, aes(komp_koop))+
  geom_bar()

desc_komp_koop <- summarise(group_by(a4, Cond, g_ng_count_1_5, komp_koop), RT = mean(RT))

plot_komp_koop <- ggplot(data =desc_komp_koop, aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point()+
  geom_line(aes(group = Cond))+
  facet_wrap(vars(komp_koop))
ggsave("plots/komp_koop.jpg", device = "jpg", dpi = 700)

#### ord pos by cat (across Cond)
# mean incr by cat

incr_by_cat_SNJN <- summarise(group_by(a4[a4$g_ng_count_cent %in% c(-2, 2), ], g_ng_count_cent, Semantische.Kategorie), 
                         RT = mean(RT))
incr_by_cat_SNJN <- mutate(group_by(incr_by_cat_SNJN, Semantische.Kategorie), diff = diff(RT)/4)

incr_by_cat_SNJN[incr_by_cat_SNJN$g_ng_count_cent == -2, "diff"] <- 0

incr_by_cat_SNJN <- incr_by_cat_SNJN[incr_by_cat_SNJN$g_ng_count_cent == 2,]

cat_with_ord_pos <- incr_by_cat_SNJN[incr_by_cat_SNJN$diff > 10, "Semantische.Kategorie"]

## mean lag by category and session (a1 df)

mean_lag_by_cat <- summarise(group_by(a1, Semantische.Kategorie, Session), 
                             mean_lag = mean(lag_cat, na.rm =T)/10)
