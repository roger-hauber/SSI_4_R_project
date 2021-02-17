################# FUUUCK

#okay once again from the top
# first things first: pre-post variable for a4 data
a4[a4$subject %in% c(3,4, 7:18), "lockdown"] <- "pre"
a4[a4$subject %in% c(1,2,5,6, 18:28), "lockdown"] <- "post"

