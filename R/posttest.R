# ===========================================================================
# Post Bootstrapping
# @Author: Zhihang Dong
# Part 1: Load necessary packages
# ===========================================================================
# load("finalized_a1.Rda")
# Shapiro-Wilk Normality Test
with(lct1, shapiro.test(lct[gender== "Female"]))
# Results were frightening... let's do a log transformation
lct1<-lct
lct1$lct<-log(lct$lct)
with(lct1, shapiro.test(lct[gender== "Female"]))
with(lct1, shapiro.test(lct[gender== "Male"])) #still not normal.

# Unpaired 2s t-test to see whether there is significant lct difference in different
# create 5 age groups for 5 sets of t-tests...
lct2<-lct1[lct$age==2,]
lct3<-lct1[lct$age==3,]
lct4<-lct1[lct$age==4,]
lct5<-lct1[lct$age==5,]
lct6<-lct1[lct$age==6,]
# run tests
t.test(lct~gender,data=lct6)
