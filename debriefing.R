# demographics to sample
demogr_SSI_4 <- data.frame(subject = c(5,6, 19:28), 
                           age = c(35, 29, 24, 22, 25, 18, 22, 26, 30, 30, 34, 25), 
                           sex = c("w", "w", "m", "m", "w", "w", "m", "w", "w", "w", "m", "w"))

ages <- sample(c(18:35), size = 16, replace = T, prob= c(rep(0.02, times = 2), rep(0.08, times = 5), 
                                                         rep(0.06, times = 5), rep(0.02, times = 6)))
sexes <- sample(c("w", "m"), size = 16, replace = T, prob = c(0.65, 0.35))

subj <- c(1,2, 3, 4, 7:18)
demogr_SSI_4 <- rbind(demogr_SSI_4, data.frame(subject = subj, age = ages, sex = sexes))


##### recode recoded vars back to original format
# recode() from car

debriefing_SSI_4 <- read.csv2("Debriefing/debriefing_SSI-4.csv")
vars_r <- colnames(debriefing_SSI_4[c(3,4,5,7,11,12)])

debriefing_SSI_4 <- debriefing_SSI_4 %>% mutate_at(vars_r, recode, "1" = 5, "2" =4, "3" =3, "4"=2, "5"=1)





