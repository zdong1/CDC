library(dplyr)
load(file="./MDCRes/gps.s.Rda")
load(file="./MDCRes/rec.s.Rda")

# JOIN TABLES 
rec.l<- rec.s %>% left_join(gps.s)
# Oops, I found records come with lots of NAs, let's remove them
rec.l<-subset(rec.l,long!="NA")
# > length(rec.l$db_key)
# [1] 11758820
# We get approx. 65,000 unique gps records per person...
# > length(unique(rec.l$personid))
# [1] 185
# > 11758820/185
# [1] 63561.19


