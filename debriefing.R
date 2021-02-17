##### recode recoded vars back to original format
# recode() from car

debriefinf_SSI_4 <- read.csv2("Debriefing/debriefinf_SSI-4.csv")
vars_r <- colnames(debriefinf_SSI_4[c(3,4,5,7,11,12)])

debriefinf_SSI_4 <- debriefinf_SSI_4 %>% mutate_at(vars_r, recode, "1" = 5, "2" =4, "3" =3, "4"=2, "5"=1)





