setwd("~/Documents")
getwd()

setwd("C:/Users/Jurij/Desktop/stockholm/R")

load("data.N_Mar2020")

#library
installed.packages("tidyverse")
installed.packages("ggplot2")
installed.packages("zoo")
library(zoo)
#midprice and D
tq = tq[tq$"Bid Price" > 0 & tq$"Ask Price" > 0 | is.na(tq$"Ask Price")| is.na(tq$"Bid Price"),]
tq$midprice = (tq$`Bid Price` + tq$`Ask Price`) / 2
tq$midprice = na.locf(tq$midprice,na.rm=FALSE,fromLast = FALSE) 

tq$D = sign(tq$Price - tq$midprice)


#check
#neki= as.matrix(tq)
#colSums(neki[, 17, drop=FALSE]<=0 & is.na(neki[, 17, drop=FALSE])==FALSE)
#print((neki[, 17, drop=FALSE]<=0) )



#time strip and filtering

tq$Date <- as.Date(tq$`Date-Time`);
tq$`Date-Time` <- tq$`Date-Time`+tq$`GMT Offset`*60*60
#  tq[, `Date-Time` := `Date-Time`+ `GMT Offset`*60*60,by=.(Date,Stock) ]

tq$Time <- format(as.POSIXct(tq$`Date-Time`),format="%H:%M:%OS")
time = strptime(tq$`Exch Time`, format="%H:%M:%OS")
tq$Seconds = time$hour*3600 + time$min*60 + time$sec + tq$`GMT Offset`*3600
#check

#max(tq$`Date-Time`)
#min(tq$`Date-Time`)



tq = tq[tq$Seconds > (9* 3600 + 35 * 60) & tq$Seconds < (15 * 3600 + 55 * 60),]
tt = tq[tq$Type == 'Trade' & tq$D!=0,]

library(ggplot2)

ggplot(data=subset(tq, !is.na(`Bid Price`)),aes(`Date-Time`, `Bid Price`)) + geom_line()
ggplot(data=subset(tq, !is.na(`Ask Price`)),aes(`Date-Time`, `Ask Price`)) + geom_line()
ggplot(data=subset(tq, !is.na(`midprice`)),aes(`Date-Time`, `midprice`)) + geom_line()



min(tq$midprice)
max(tq$midprice)



###Filtering out negatives and outside trading hours

###Trading volume
tq$Trading.volume = (tq$Price * tq$Volume);

sum(tq$Trading.volume, na.rm = TRUE);

##Qutoed spread
10000 * mean((tq$`Ask Price` - tq$`Bid Price`) / tq$midprice, na.rm = TRUE);

##Effective spread
10000 * mean(tt$D * (tt$Price - tt$midprice) / tt$midprice, na.rm = TRUE)
library(dplyr)
sec_diff <- diff(tq$Seconds)
sec_diff <- c(sec_diff,0)
tq$sec_diff <- abs(sec_diff)
tq$sec_diff[tq$sec_diff>20000]<-0

max(tq$sec_diff )
#12:57 to 13:11
dates = unique(as.Date(tq$`Date-Time`))

efficiency_1 = matrix(nrow=length(dates),ncol = 7,dimnames=
                        list(as.character(dates), c("total_volume_in_mil","quoted spread","quoted spread1", "effective spread","effective spread1","quoted spread weighted","effective spread weighted")))
for(d in 1:length(dates)){
  print(d)
  efficiency_1[d,"total_volume_in_mil"]= sum(tq$Trading.volume [which(tq$`Date`==dates[d])],na.rm=TRUE)/1000000
  
  efficiency_1[d,"quoted spread"]=10000*mean((tq$`Ask Price`-tq$`Bid Price`)/tq$`midprice`[dates[d]==tq$Date],na.rm = TRUE)
  efficiency_1[d,"quoted spread1"]=10000*mean((tt$`Ask Price`-tt$`Bid Price`)/tt$`midprice`[dates[d]==tt$Date],na.rm = TRUE)
  
  efficiency_1[d,"effective spread"]=10000*mean(tt$D*(tt$Price-tt$`midprice`)/tt$`midprice`[tt$Date==dates[d]], na.rm = TRUE)
  
  efficiency_1[d,"effective spread1"]=10000*mean(tq$D*(tq$Price-tq$`midprice`)/tq$`midprice`[tq$Date==dates[d]], na.rm = TRUE)
  efficiency_1[d,"quoted spread weighted"] =10000*weighted.mean((tq$`Ask Price`-tq$`Bid Price`)/tq$midprice[tq$Date==dates[d]],tq$sec_diff, na.rm = TRUE)
  efficiency_1[d,"effective spread weighted"] =10000*weighted.mean(tq$D*(tq$Price-tq$midprice)/tq$midprice[tq$Date==dates[d]], tq$sec_diff, na.rm =TRUE)
}
#check
eff =as.data.frame(efficiency_1)
sum(eff$total_volume_in_mil)






##Advanced data
qq = tq[tq$Type == "Quote" , c("Date", "Seconds", "midprice")];
breakpoints5min = seq(9+35/60, 15+55/60, 5/60)*3600;
qq$interval5min = seq(9+35/60, 15+55/60, 5/60)[findInterval(qq$Seconds, breakpoints5min)];
require(zoo);
head(dates)
efficiency = matrix(nrow = length(dates), ncol = 11, dimnames = list(as.character(dates), c("var5min", "ac5min", "rv5min", "var1min", "ac1min", "rv1min", "vr_5min_1min", "var10sec", "ac10sec", "rv10sec","vr_1min_10sec")));
print(efficiency);
qq$lastobs5min = c(diff(qq$interval5min)>0,T);
for (d in 1:length(dates)){
  equispaced5min = qq[qq$lastobs5min & qq$Date== dates[d],c("interval5min","midprice")];
  equispaced5min = merge(matrix(breakpoints5min/3600,length(breakpoints5min),1), equispaced5min, by=1)
  
  equispaced5min$midprice = na.locf(equispaced5min$midprice);
  equispaced5min$Ret = 100 * c(NA, diff(log(equispaced5min$midprice)));
  efficiency[d, "var5min"] = var(equispaced5min$Ret, na.rm = TRUE)
  efficiency[d, "ac5min"] = cor(equispaced5min$Ret[-length(equispaced5min$Ret)], equispaced5min$Ret[-1], use = "complete.obs")
  efficiency[d, "rv5min"] = mean(equispaced5min$Ret^2, na.rm = TRUE)
}

breakpoints1min = seq(9+35/60, 15+55/60, 1/60)*3600;
qq$interval1min = seq(9+35/60, 15+55/60, 1/60)[findInterval(qq$Seconds, breakpoints1min)];
qq$lastobs1min = c(diff(qq$interval1min)>0,T);
for (d in 1:length(dates)){
  equispaced1min = qq[qq$lastobs1min & qq$Date== dates[d],c("interval1min","midprice")];
  equispaced1min = merge(matrix(breakpoints1min/3600,length(breakpoints1min),1), equispaced1min, by=1)
  
  equispaced1min$midprice = na.locf(equispaced1min$midprice);
  equispaced1min$Ret = 100 * c(NA, diff(log(equispaced1min$midprice)));
  efficiency[d, "var1min"] = var(equispaced1min$Ret, na.rm = TRUE)
  efficiency[d, "ac1min"] = cor(equispaced1min$Ret[-length(equispaced1min$Ret)], equispaced1min$Ret[-1], use = "complete.obs")
  efficiency[d, "rv1min"] = mean(equispaced1min$Ret^2, na.rm = TRUE)
}

breakpoints10sec = seq(9+35/60, 15+55/60, 1/360)*3600;
qq$interval10sec = seq(9+35/60, 15+25/60, 1/360)[findInterval(qq$Seconds, breakpoints10sec)];
head(qq);
qq$lastobs10sec = c(diff(qq$interval10sec)>0,T);
for (d in 1:length(dates)){
  equispaced10sec = qq[qq$lastobs10sec & qq$Date== dates[d],c("interval10sec","midprice")];
  equispaced10sec = merge(matrix(breakpoints10sec/3600,length(breakpoints10sec),1), equispaced10sec, by=1)
  
  equispaced10sec$midprice = na.locf(equispaced10sec$midprice);
  equispaced10sec$Ret = 100 * c(NA, diff(log(equispaced10sec$midprice)));
  efficiency[d, "var10sec"] = var(equispaced10sec$Ret, na.rm = TRUE)
  efficiency[d, "ac10sec"] = cor(equispaced10sec$Ret[-length(equispaced10sec$Ret)], equispaced10sec$Ret[-1], use = "complete.obs")
  efficiency[d, "rv10sec"] = mean(equispaced10sec$Ret^2, na.rm = TRUE)
}

print(efficiency)
efficiency[,"vr_5min_1min"] = efficiency[,"var5min"] / (efficiency[,"var1min"]*5);
efficiency[,"vr_1min_10sec"] = efficiency[,"var1min"] / (efficiency[,"var10sec"]*6);
round(efficiency,3)



#subset
tq_1 = subset(tq,Date== "2020-03-18" & tq$`Seconds` > (12* 3600 + 27 * 60) & tq$`Seconds` < (12 * 3600 + 57 * 60) )
tq_2 = subset(tq,Date== "2020-03-18" & tq$`Seconds` > (13* 3600 + 11 * 60) & tq$`Seconds` < (13 * 3600 + 41 * 60) )

