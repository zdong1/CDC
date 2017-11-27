# Split Persons Programs
# Load the data and sort the column you want to slide on
load(file="./MDCRes/rec.ld.Rda")
rec.ld<-rec.ld[order(rec.ld$personid),]

# Slice the data
s <- setNames(split(rec.ld, rec.ld$personid), paste0("person", unique(rec.ld$personid)))
list2env(s, globalenv())
# Now, we have 183 datasets called 'personxxxx' based on his/her personid
# Check and see if it is right
# > summary(person5448$personid)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5448    5448    5448    5448    5448    5448 
# > summary(person6272$personid)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6272    6272    6272    6272    6272    6272 

save(list=ls(),file="./MDCRes/byperson.Rda")