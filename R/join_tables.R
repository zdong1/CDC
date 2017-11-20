library(dplyr)
load(file="./MDCRes/gps.s.Rda")
load(file="./MDCRes/rec.s.Rda")

# JOIN TABLES 
rec.l<- rec.s %>% left_join(gps.s)

