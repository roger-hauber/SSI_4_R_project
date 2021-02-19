################# FUUUCK
#load data
a4 <- read.csv2("../a4_SSI_4.csv")

#okay once again from the top
# first things first: pre-post variable for a4 data
a4[a4$subject %in% c(3,4, 7:18), "lockdown"] <- "pre"
a4[a4$subject %in% c(1,2,5,6, 18:28), "lockdown"] <- "post"

# prep data frames for plotting
#mean RT for ord pos by DG and Cond
desc_by_DG <- summarise(group_by(a4, g_ng_count_cent, Cond, DG), RT = mean(RT))

#plot
ord_pos_by_DG <- ggplot(data = desc_by_DG, mapping = aes(x = g_ng_count_cent, y = RT, colour = Cond)) +
  geom_point() +
  geom_line(aes(group = Cond)) +
  facet_wrap(desc_by_DG$DG) +
  xlab("Ordinal Position (mean centered)")+
  ggtitle("Mean RT by Ordinal Position and Condition", subtitle = "for each Repetition")

ggsave("../plots/ord_pos_by_DG.jpg", device = "jpg", dpi = 700)

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

ggsave("../plots/incr_by_DG.jpg", device = "jpg", dpi = 700)
