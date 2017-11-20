library(dplyr)
load(file="./MDCRes/gps.s.Rda")
load(file="./MDCRes/rec.s.Rda")
load(file="./MDCRes/dem.Rda") # demographics file
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

# save the linked file as CSV format
save(rec.l,file="./MDCRes/rec.l.csv")

#
rec.ld<- rec.l %>% left_join(dem)
# Joining, by = "personid"
# Check to make sure the size remain unchanged after the left join (unmatched fields become 'NA')
> length(rec.l$long)
> [1] 11758820
> length(rec.ld$long)
> [1] 11758820

