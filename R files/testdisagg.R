source("./R files/jd3_init.R")
source("./R files/jd3_tempdisagg.R")
load("./Data/retail.rda")

y<-jd3_aggregate(retail$RetailSalesTotal, 1)

#jd3_tempdisagg(y, constant = T)
q1<-jd3_tempdisagg(y, constant = T)
q2<-jd3_tempdisagg(y, constant = T, indicators=list(retail$AllOtherGenMerchandiseStores, retail$AllOtherHomeFurnishingsStores ))

