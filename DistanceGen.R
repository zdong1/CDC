# CDC Distance Generator 
# If you don't have this:
# install.packages("gmapsdistance")
library("gmapsdistance")

# Sample data: 3700 observations of a person in CDC (very small partition of a partition...)
# load("d0_one.Rdata") -- See Github repository

get.api.key()

trial1<-gmapsdistance(origin="46.5255+6.65218", destination = "46.5240+6.61600",mode="walking")
trial1
