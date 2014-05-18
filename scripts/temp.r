num = c(50,20,15,15)
spend = c(60,100,150,150) 
gender= c("male","female","male","female") 
df = data.frame(num, spend, gender)    
df
ct <-xtabs(num ~ gender + spend, data = df);ct

dt0 <-dt[960:965,]
ct1 <-xtabs(steps ~ date, data = dt,exclude = c(NA, NaN));ct1[1:3]
ct2 <- aggregate(steps ~ date,data=dt, FUN=sum);ct2[1:3,]

#table(dt0$steps,dt0$date)
#colSums(table(dt0$steps,dt0$date ))[1:4]
#table(dt0$steps,dt0$date)
#table(dt$steps,dt$date, responseName=dt$steps )[1:3,1:4]

dtt <- dt[1:4,]
ct1 <-xtabs(steps ~ date, data = dtt,
            drop.unused.levels =F);ct1[1:3]
ct1 <-xtabs(steps ~ date, data = dtt,drop.unused.levels =T);ct1[1:3]

aggregate(steps ~ interval,data=dt, FUN=mean);


# dt.split <-  split(dww,dww$wd)
# par(mfrow = c(1, 1))
# plot(steps ~ interval , data = dt.split$weekday, type ="l",
#      rows =2, col = c("blue"), main = "Weekday")
# plot(steps ~ interval , data = dt.split$weekend, type ="l",
#      rows =2, col = c("red"), main = "Weekend")
# 
# coplot(steps ~ interval | wd, data = dww, type ="l",
#        rows =2, col = c("red", "blue"))
