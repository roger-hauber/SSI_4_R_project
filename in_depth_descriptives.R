################# FUUUCK
library(tidyverse)
library(ggtext)

setwd("C:/Users/Roger/Seafile/Roger_Seafile/SSI-EEG-3/SSI_4_R_project/")

#load data
a4 <- read.csv2("a4_SSI_4.csv")

#okay once again from the top
# first things first: pre-post variable for a4 data
a4[a4$subject %in% c(3,4, 7:18), "lockdown"] <- "pre"
a4[a4$subject %in% c(1,2,5,6, 18:28), "lockdown"] <- "post"

a4$lockdown <- factor(a4$lockdown, levels = c("pre", "post"))

a4$g_ng_count_1_5 <- as.factor(a4$g_ng_count_1_5)

# prep data frames for plotting
# once across all DGs
desc_across <- summarise(group_by(a4, Cond, g_ng_count_1_5), RT = mean(RT))

ord_pos <- ggplot(data = desc_across, mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond))
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
  scale_x_discrete(labels = c("ord pos n", "ord pos n+1"))

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
  scale_x_discrete(labels = c("ord pos n", "ord pos n+1"))

ggsave("plots/incr_by_DG_lockdown.jpg", device = "jpg", dpi = 700)

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
desc_by_subj_4_5$subject <- factor(desc_by_subj_4_5$subject, levels = subj_levels)

#plot

ord_pos_by_subj_1_5 <- ggplot(data = desc_by_subj_4_5, 
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
ord_pos_by_subj_1_5_alt <- ggplot(data = desc_by_subj_4_5, 
                          mapping = aes(x = g_ng_count_1_5, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_subj_4_5$subject, ncol = 6) +
  xlab("Ordinal Position") +
  ggtitle("RT at Ordinal Positions 4 and 5 by Condition and Subject", 
          subtitle = "Neighboring subjects are *mirrors of each other*") +
  theme(plot.subtitle = ggtext::element_markdown())
ggsave("plots/pos_4_5_by_subj_alt.pdf", device = "pdf", dpi = 700, height = 10, width = 12)
