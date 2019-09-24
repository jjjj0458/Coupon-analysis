user.list<-read.csv("C:/Users/User/Desktop/user_list3.csv",header=T)
project2<-read.csv("C:/Users/user/Desktop/correct.csv",header=T)
project2<-subset(project2,project2$GENRE_NAME!="宅配")
project3<-read.csv("C:/Users/user/Desktop/correct1.csv",header=T)
project3<-subset(project3,project3$GENRE_NAME!="宅配")
list<-read.csv("C:/Users/user/Desktop/coupon_list_train.csv",header=T)
attach(user.list)
attach(project2)
attach(project3)
attach(list)
qplot(GENRE_NAME,data=project2,geom="bar",fill=factor(GENRE_NAME))+geom_text(stat='bin',aes(label=..count..),vjust=-1)+labs(title="Barplot of GENRE_NAME")
colr=c("#B22222","#EE2C2C","#191970","#104E8B","#009ACD","#00BFFF","#548B54","#32CD32","#90EE90","#C0FF3E","#FFD700","#CDAD00","#8B7500")
hist(user.list$AGE,breaks=seq(15,80,5),xlim=range(15,80),labels=T,col="SEX_ID",main="Histogram of AGE",xlab="AGE",ylim=range(0,4000))
pie<-ggplot(user.list,aes(x=factor(1),fill=factor(SEX_ID)))+geom_bar(width = 1)
pie + coord_polar(theta = "y")
hist(list$DISCOUNT_PRICE,labels=T,xlab="Price",col=rainbow(20),main="Histogram of Price")
hist(list$DISCOUNT_PRICE[list$DISCOUNT_PRICE<=40000],labels=T,col=rainbow(20),ylim=range(0,8000),xlab="Price",main="Histogram of Price")
boxplot(list$DISCOUNT_PRICE~list$GENRE_NAME,col=rainbow(13),main="Boxplot of Category")
boxplot(list$DISCOUNT_PRICE~list$GENRE_NAME,outline=F,col=rainbow(13),main="Boxplot of Category")
hist(user.list$V10,xlim=range(0,105),breaks=seq(0,105,5),labels=T,col=rainbow(21),xlab="Total Purchase Times",main="Histogram of Total Purchase Times")
hist(user.list$V10[user.list$V10<=50],xlim=range(0,50),breaks=seq(0,50,5),labels=T,col=rainbow(10),xlab="Total Purchase Times",main="Histogram of Total Purchase Times")
#################################################
list$price_lvl<-ifelse(list$DISCOUNT_PRICE<=1500,0,ifelse(list$DISCOUNT_PRICE<=3000,1,ifelse(list$DISCOUNT_PRICE<=10000,2,3)))
table(list$GENRE_NAME)
table()


##整合資料
for(i in seq(along=names(a))){
  user.list[user.list$USER_ID_hash==names(a)[i],10]<-a[i]
  
}
table(user.list$V10)
colorspec <- colorRampPalette(colors = c("#C1FFC1", "#698B69"), space = "rgb")

norm_iris <- cbind(GENRE_NAME=list[,2],as.data.frame(scale(iris[,1:4])))

norm_user.list<-cbind(user.list[,3],as.data.frame(scale(user.list[,8:22])))


user.list<-subset(user.list,total!="NA")
summary(user.list)

male=sum(user.list$SEX_ID=="m")
female=sum(user.list$SEX_ID=="f")
sex=c(male,female)

##男女ID註冊比例
par(mfrow=c(1,2))

pct <- round(sex/sum(sex)*100,1)
lbls0 <- c("男性","女性")
lbls0 <- paste(lbls0, pct)
lbls0 <- paste(lbls0,"%",sep="")
pie(sex,labels = lbls0, col=c("blue","green"),main = "男女ID註冊比例")

smale=subset(user.list,user.list$SEX_ID=="m")
sfemale=subset(user.list,user.list$SEX_ID=="f")
male=sum(user.list$SEX_ID=="m")
female=sum(user.list$SEX_ID=="f")
tmale=sum(smale$ntotal)
tfemale=sum(sfemale$ntotal)
sex=c(male,female)
tsex=c(tmale,tfemale)

pct <- round(tsex/sum(tsex)*100,1)
lbls00 <- c("男性","女性")
lbls00 <- paste(lbls00, pct)
lbls00 <- paste(lbls00,"%",sep="")
pie(tsex,labels = lbls00, col=c("blue","green"),main = "男女所佔總消費比例")

project2<-project2$GENRE_NAME[project2$GENRE_NAME!="宅配"]
project3<-project3$GENRE_NAME[project3$GENRE_NAME!="宅配"]
par(mfrow=c(1,2))
##售出的coupon中各種類型所占比例(張數)
a = c(6654,198,15440,154,25494,3702,2101,37293,2313,3876,1585,10532)
pct <- round(a/sum(a)*100,1)
lbls <- c("戶外活動","健康與醫療","住宿","美妝","其他折價券","放鬆","指甲與眼妝","美食","美容","美髮沙龍","課程","禮物卡")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(a,labels = lbls, col=rainbow(length(lbls)),cex=0.7)


smale=subset(user.list,user.list$SEX_ID=="m")
sfemale=subset(user.list,user.list$SEX_ID=="f")
maccom=sum(smale$accom)
mfood=sum(smale$food)
moutdoor=sum(smale$outdoor)
mother=sum(smale$other)
mrelax=sum(smale$relax)
mfingernail=sum(smale$fingernail)
mBeauty=sum(smale$Beauty)
mhair=sum(smale$hair)
mhealthy=sum(smale$healthy)
mcourse=sum(smale$course)
mpresent=sum(smale$present)
mcosmetic=sum(smale$cosmetic)

faccom=sum(sfemale$accom)
ffood=sum(sfemale$food)
foutdoor=sum(sfemale$outdoor)
fother=sum(sfemale$other)
frelax=sum(sfemale$relax)
ffingernail=sum(sfemale$fingernail)
fBeauty=sum(sfemale$Beauty)
fhair=sum(sfemale$hair)
fhealthy=sum(sfemale$healthy)
fcourse=sum(sfemale$course)
fpresent=sum(sfemale$present)
fcosmetic=sum(sfemale$cosmetic)

bmale=c(moutdoor,mcosmetic,maccom,mBeauty,mrelax,mother,mfingernail,mfood,mhair,mpresent,mcourse,mhealthy)
bfemale=c(foutdoor,fcosmetic,faccom,fBeauty,frelax,fother,ffingernail,ffood,fhair,fpresent,fcourse,fhealthy)
par(mfrow=c(1,2))
##售出的coupon中各種類型所占金額比例(男性)
pct <- round(bmale/sum(bmale)*100,1)
lbls11 <- c("戶外活動","健康與醫療","住宿","美妝","其他折價券","放鬆","指甲與眼妝","美食","美容","美髮沙龍","課程","禮物卡")
lbls11 <- paste(lbls11, pct)
lbls11 <- paste(lbls11,"%",sep="")
pie(bmale,labels = lbls11, col=rainbow(length(lbls)),cex=0.7)

##售出的coupon中各種類型所占金額比例(女性)
pct <- round(bfemale/sum(bfemale)*100,1)
lbls12 <- c("戶外活動","美妝","住宿","美容","放鬆","其他折價券","指甲與眼妝","美食","美髮沙龍","禮物卡","課程","健康與醫療")
lbls12 <- paste(lbls12, pct)
lbls12 <- paste(lbls12,"%",sep="")
pie(bfemale,labels = lbls12, col=rainbow(length(lbls)),cex=0.7)

a戶外活動=subset(project2,GENRE_NAME=="戶外活動")
m戶外活動=sum(a戶外活動$total.purchase)
a住宿=subset(project2,GENRE_NAME=="住宿")
m住宿=sum(a住宿$total.purchase)
a其他折價券=subset(project2,GENRE_NAME=="其他折價券")
m其他折價券=sum(a其他折價券$total.purchase)
a放鬆=subset(project2,GENRE_NAME=="放鬆")
m放鬆=sum(a放鬆$total.purchase)
a指甲與眼妝=subset(project2,GENRE_NAME=="指甲與眼妝")
m指甲與眼妝=sum(a指甲與眼妝$total.purchase)
a美妝=subset(project2,GENRE_NAME=="美妝")
m美妝=sum(a美妝$total.purchase)
a美食=subset(project2,GENRE_NAME=="美食")
m美食=sum(a美食$total.purchase)
a美容=subset(project2,GENRE_NAME=="美容")
m美容=sum(a美容$total.purchase)
a美髮沙龍=subset(project2,GENRE_NAME=="美髮沙龍")
m美髮沙龍=sum(a美髮沙龍$total.purchase)
a健康與醫療=subset(project2,GENRE_NAME=="健康與醫療")
m健康與醫療=sum(a健康與醫療$total.purchase)
a課程=subset(project2,GENRE_NAME=="課程")
m課程=sum(a課程$total.purchase)
a禮物卡=subset(project2,GENRE_NAME=="禮物卡")
m禮物卡=sum(a禮物卡$total.purchase)

totalmoney=c(m戶外活動,m健康與醫療,m住宿,m美妝,m其他折價券,m放鬆,m指甲與眼妝,m美食,m美容,m美髮沙龍,m課程,m禮物卡)

##售出的coupon中各種類型所占金額比例
pct <- round(totalmoney/sum(totalmoney)*100,1)
lbls2 <- c("戶外活動","健康與醫療","住宿","美妝","其他折價券","放鬆","指甲與眼妝","美食","美容","美髮沙龍","課程","禮物卡")
lbls2 <- paste(lbls2, pct)
lbls2 <- paste(lbls2,"%",sep="")
pie(totalmoney,labels = lbls2, col=rainbow(length(lbls)),cex=0.7)
class(totalmoney)

table(project2$I_DATE)

hist(user.list$total)
age1525=sum(user.list$AGE>15&user.list$AGE<=25)
lage1525=subset(user.list,user.list$AGE>15&user.list$AGE<=25)
table(lage1525$SEX_ID=="f")
table(lage1525$total>10000)
age2635=sum(user.list$AGE>25&user.list$AGE<=35)
lage2635=subset(user.list,user.list$AGE>25&user.list$AGE<=35)
table(lage2635$SEX_ID=="f")
table(lage2635$total>10000)
age3645=sum(user.list$AGE>35&user.list$AGE<=45)
lage3645=subset(user.list,user.list$AGE>35&user.list$AGE<=45)
table(lage3645$SEX_ID=="f")
table(lage3645$total>10000)
age4655=sum(user.list$AGE>45&user.list$AGE<=55)
lage4655=subset(user.list,user.list$AGE>45&user.list$AGE<=55)
table(lage4655$SEX_ID=="f")
table(lage4655$total>10000)
age5665=sum(user.list$AGE>55&user.list$AGE<=65)
lage5665=subset(user.list,user.list$AGE>55&user.list$AGE<=65)
table(lage5665$SEX_ID=="f")
table(lage5665$total>10000)
age66=sum(user.list$AGE>65)
lage66=subset(user.list,user.list$AGE>65)
table(lage66$SEX_ID=="f")
table(lage66$total>10000)



cc=c(age1525,age2635,age3645,age4655,age5665,age66)
par(mfrow=c(1,2))
##消費者年齡分佈
pct <- round(cc/sum(cc)*100,1)
lbls3 <- c("15歲至25歲","26歲至35歲","36歲至45歲","46歲至55歲","56歲至65歲","66歲或以上")
lbls3 <- paste(lbls3, pct)
lbls3 <- paste(lbls3,"%",sep="")
pie(cc,labels = lbls3, col=c("red","orange","yellow","blue","green","pink"),cex=0.7)

median(user.list$total)

lage1525=subset(user.list,user.list$AGE>15&user.list$AGE<=25)
mage1525=sum(lage1525$ntotal,na.rm=T)
lage2635=subset(user.list,user.list$AGE>25&user.list$AGE<=35)
mage2635=sum(lage2635$ntotal,na.rm=T)
lage3645=subset(user.list,user.list$AGE>35&user.list$AGE<=45)
mage3645=sum(lage3645$ntotal,na.rm=T)
lage4655=subset(user.list,user.list$AGE>45&user.list$AGE<=55)
mage4655=sum(lage4655$ntotal,na.rm=T)
lage5665=subset(user.list,user.list$AGE>55&user.list$AGE<=65)
mage5665=sum(lage5665$ntotal,na.rm=T)
lage66=subset(user.list,user.list$AGE>65)
mage66=sum(lage66$ntotal,na.rm=T)

mm=c(mage1525,mage2635,mage3645,mage4655,mage5665,mage66)

##消費者的總消費分佈
pct <- round(mm/sum(mm)*100,1)
lbls4 <- c("15歲至25歲","26歲至35歲","36歲至45歲","46歲至55歲","56歲至65歲","66歲或以上")
lbls4 <- paste(lbls4, pct)
lbls4 <- paste(lbls4,"%",sep="")
pie(mm,labels = lbls4, col=c("red","orange","yellow","blue","green","pink"),cex=0.7)

barplot(n,col=c("red","orange","yellow","blue","green","pink"),main="各年齡層的平均個人消費",xlab="年齡",ylab="平均個人消費")

n=mm/cc
names(n)<-m
m=c("15歲至25歲","26歲至35歲","36歲至45歲","46歲至55歲","56歲至65歲","66歲或以上")
qplot(n,geom="bar",fill=n)

smale=subset(user.list,user.list$SEX_ID=="m")
sfemale=subset(user.list,user.list$SEX_ID=="f")
colr=c("#B22222","#EE2C2C","#191970","#104E8B","#009ACD","#00BFFF","#548B54","#32CD32","#90EE90","#C0FF3E","#FFD700","#CDAD00","#8B7500")
hist(smale$AGE,breaks=seq(15,80,5),xlim=range(15,80),labels=T,col=colr,main="Histogram of AGE",xlab="AGE of male",ylim=range(0,2000))
hist(sfemale$AGE,breaks=seq(15,80,5),xlim=range(15,80),labels=T,col=colr,main="Histogram of AGE",xlab="AGE of female",ylim=range(0,2000))
##註冊者的年齡分佈
agep=qplot(AGE,data=user.list,geom="histogram",fill=SEX_ID,breaks=seq(15,80,2),xlab="年齡",ylab="數量")
agep+theme(legend.text=element_text(size=12),axis.text.x=element_text(size=12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),axis.text.y=element_text(size=12))

##每月折價券的購買數量
qplot(I_DATE,data=project2,geom="bar",fill=I_DATE)+
xlab("月份") + ylab("購買數量")+
  theme(legend.text=element_text(size=20),axis.text.x=element_text(size=20),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),axis.text.y=element_text(size=20),legend.position="none")

##每日折價券的購買數量
plot(project3$I_DATE,col=rainbow(358),xlab="購買日期",ylab="購買數量")
par(ps = 12, cex = 1.5, cex.main = 2)


summary(project2)

table(project2$I_DATE)

nov=subset(project2,project2$I_DATE=="2011-11")
summary(nov)
table(nov$GENRE_NAME)
##消費者的居住地
table(user.list$dutch)
sarea=c(1372,593,577,627,311,7229,581,1646,3204,6646)
pct <- round(sarea/sum(sarea)*100,1)
lbls13 <- c("九州&沖繩","中國","北信越","北海道","四國","未知","東北","東海","關西","關東")
lbls13 <- paste(lbls13, pct)
lbls13 <- paste(lbls13,"%",sep="")
pie(sarea,labels = lbls13,  col=rainbow(length(lbls)),cex=0.7)


summary(user.list)
lage1525=subset(user.list,AGE>15&AGE<=25)
lage2635=subset(user.list,AGE>25&AGE<=35)
lage3645=subset(user.list,AGE>35&AGE<=45)
lage4655=subset(user.list,AGE>45&AGE<=55)
lage5665=subset(user.list,AGE>55&AGE<=65)
lage66=subset(user.list,AGE>65)

lage1525$ageseq<-"15~25"
lage2635$ageseq<-"26~35"
lage3645$ageseq<-"36~45"
lage4655$ageseq<-"46~55"
lage5665$ageseq<-"56~65"
lage66$ageseq<-"66UP"

userk<-rbind(lage1525,lage2635,lage3645,lage4655,lage5665,lage66)
summary(userk)


boxplot(average~ageseq,data=userk)

##各年齡層平均個人單次消費金額

ggplot(userk, aes(x=ageseq, y=average,fill=ageseq)) +  
  geom_boxplot()+ 
  scale_y_continuous(limits=c(0,20000), breaks=seq(0,20000,4000), expand = c(0, 0))+
xlab("年齡層") + ylab("平均個人單次消費金額")+
theme(legend.text=element_text(size=12), 
      axis.text.x=element_text(size=12), 
      axis.title.x=element_text(size=12), 
      axis.title.y=element_text(size=12), 
      axis.text.y=element_text(size=12),legend.position="none")+ 
stat_summary(fun.y = "mean", geom = "text", label="----------", size= 10, color= "white")


##各年齡層平均個人購買單張折價券的金額
ggplot(userk, aes(x=ageseq, y=naverage2,fill=ageseq)) + geom_boxplot()+scale_y_continuous(limits=c(0,50000), breaks=seq(0,50000,10000), expand = c(0, 0))+
  xlab("年齡層") + ylab("平均個人購買單張折價券的金額")+
  theme(legend.text=element_text(size=10),axis.text.x=element_text(size=10),axis.title.x=element_text(size=10),axis.title.y=element_text(size=10),axis.text.y=element_text(size=10))+
  stat_summary(fun.y = "mean", geom = "text", label="-----------------", size= 10, color= "white") +
geom_jitter(position=position_jitter(width=0, height=0))

c戶外活動=subset(list,GENRE_NAME=="戶外活動")
c住宿=subset(list,GENRE_NAME=="住宿")
c其他折價券=subset(list,GENRE_NAME=="其他折價券")
c放鬆=subset(list,GENRE_NAME=="放鬆")
c指甲與眼妝=subset(list,GENRE_NAME=="指甲與眼妝")
c美妝=subset(list,GENRE_NAME=="美妝")
c美食=subset(list,GENRE_NAME=="美食")
c美容=subset(list,GENRE_NAME=="美容")
c美髮沙龍=subset(list,GENRE_NAME=="美髮沙龍")
c健康與醫療=subset(list,GENRE_NAME=="健康與醫療")
c課程=subset(list,GENRE_NAME=="課程")
c禮物卡=subset(list,GENRE_NAME=="禮物卡")

c戶外活動$avec<-"戶外活動"
c住宿$avec<-"住宿"
c其他折價券$avec<-"其他折價券"
c放鬆$avec<-"放鬆"
c指甲與眼妝$avec<-"指甲與眼妝"
c美妝$avec<-"美妝"
c美食$avec<-"美食"
c美容$avec<-"美容"
c美髮沙龍$avec<-"美髮沙龍"
c健康與醫療$avec<-"健康與醫療"
c課程$avec<-"課程"
c禮物卡$avec<-"禮物卡"

##各類折價券的單一價格
userb<-rbind(c戶外活動,c住宿,c其他折價券,c放鬆,c指甲與眼妝,c美妝,c美食,c美容,c美髮沙龍,c健康與醫療,c課程,c禮物卡)
summary(userb)
ggplot(userb, aes(x=avec, y=DISCOUNT_PRICE,fill=avec)) + geom_boxplot()+scale_y_continuous(limits=c(0,40000), breaks=seq(0,40000,10000), expand = c(0, 0))+
  xlab("折價券名稱") + ylab("金額")+
  theme(legend.text=element_text(size=12),axis.text.x=element_text(size=12),axis.title.y=element_text(size=12),axis.text.y=element_text(size=12),plot.title = element_text(size = rel(2)),legend.position="none")+
  stat_summary(fun.y = "mean", geom = "text", label="-----", size= 12, color= "white")
## 主觀分群#############################
##在資料中新增一欄名為cat1，以年齡和性別為分類依據
user.list$age1<-ifelse(user.list$AGE<36,"A",ifelse(user.list$AGE<46,"B","C"))
user.list$cat1<-ifelse(user.list$age1=="A"&user.list$SEX_ID=="f","年輕女性",ifelse(user.list$age1=="A"&user.list$SEX_ID=="m","年輕男性",ifelse(user.list$age1=="B"&user.list$SEX_ID=="f","中年女性",ifelse(user.list$age1=="B"&user.list$SEX_ID=="m","中年男性",ifelse(user.list$age1=="C"&user.list$SEX_ID=="f","年長女性","年長男性")))))
table(user.list$cat1)
user.list$cat1=ordered(user.list$cat1,levels=c("年輕女性","年輕男性","中年女性","中年男性","年長女性","年長男性"))
str(user.list)

##Total為消費者的總消費金額(已排除宅配類)，以盒狀圖看各類消費者的消費金額分布###########################
user.list$Total1<-user.list$Total/10000       ##將金額轉為單位:萬元(日圓)
qplot(as.factor(cat1),Total1,data=user.list,geom="boxplot",xlab="Groups",ylab="消費金額")+geom_boxplot(aes(fill=factor(cat1)))+labs(title="Total consumption of Groups")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20,colour = "red"),axis.title=element_text(size=20),title=element_text(size=25),legend.title=element_text(size=15))+stat_summary(fun.y = "mean", geom = "text", label="------------", size= 12, color= "white")
qplot(as.factor(cat1),Total,data=user.list,geom="boxplot",xlab="Groups",ylim=c(0,50000),ylab="消費金額")+geom_boxplot(aes(fill=factor(cat1)))+labs(title="Total consumption of Groups")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=25),legend.title=element_text(size=15))+stat_summary(fun.y = "mean", geom = "text", label="------------", size= 12, color= "white")
qplot(as.factor(cat1),Total,data=user.list,geom="boxplot",xlab="Groups",ylim=c(0,50000))+geom_boxplot(aes(fill=factor(cat1)))+labs(title="Total consumption of Groups")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=25),legend.title=element_text(size=15))

summary(user.list$Total)                      ##有一名消費者的消費金額高達70萬
user.list[which.max(user.list$Total),]        ##觀察一下該名消費者的消費分布情形

##以圓餅圖觀察六類消費者的消費結構#########################
##年輕女性的coupon消費圓餅圖
user.AF=subset(user.list,cat1=="年輕女性")
p = c(sum(user.AF$accom),sum(user.AF$food),sum(user.AF$fingernail)+sum(user.AF$Beauty)+sum(user.AF$hair)+sum(user.AF$cosmetic),sum(user.AF$outdoor),sum(user.AF$relax),sum(user.AF$course),sum(user.AF$present),sum(user.AF$other)+sum(user.AF$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls, col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "年輕女性的消費金額比例")
##年輕男性
user.AM=subset(user.list,cat1=="年輕男性")
p = c(sum(user.AM$accom),sum(user.AM$food),sum(user.AM$fingernail)+sum(user.AM$Beauty)+sum(user.AM$hair)+sum(user.AM$cosmetic),sum(user.AM$outdoor),sum(user.AM$relax),sum(user.AM$course),sum(user.AM$present),sum(user.AM$other)+sum(user.AM$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "年輕男性的消費金額比例")
##中年女性
user.BF=subset(user.list,cat1=="中年女性")
p = c(sum(user.BF$accom),sum(user.BF$food),sum(user.BF$fingernail)+sum(user.BF$Beauty)+sum(user.BF$hair)+sum(user.BF$cosmetic),sum(user.BF$outdoor),sum(user.BF$relax),sum(user.BF$course),sum(user.BF$present),sum(user.BF$other)+sum(user.BF$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls, col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "中年女性的消費金額比例")
##中年男性
user.BM=subset(user.list,cat1=="中年男性")
p = c(sum(user.BM$accom),sum(user.BM$food),sum(user.BM$fingernail)+sum(user.BM$Beauty)+sum(user.BM$hair)+sum(user.BM$cosmetic),sum(user.BM$outdoor),sum(user.BM$relax),sum(user.BM$course),sum(user.BM$present),sum(user.BM$other)+sum(user.BM$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls, col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "中年男性的消費金額比例")
##年長女性
user.CF=subset(user.list,cat1=="年長女性")
p = c(sum(user.CF$accom),sum(user.CF$food),sum(user.CF$fingernail)+sum(user.CF$Beauty)+sum(user.CF$hair)+sum(user.CF$cosmetic),sum(user.CF$outdoor),sum(user.CF$relax),sum(user.CF$course),sum(user.CF$present),sum(user.CF$other)+sum(user.CF$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls, col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "年長女性的消費金額比例")
##年長男性
user.CM=subset(user.list,cat1=="年長男性")
p = c(sum(user.CM$accom),sum(user.CM$food),sum(user.CM$outdoor),sum(user.CM$relax),sum(user.CM$other)+sum(user.CM$healthy)+sum(user.CM$fingernail)+sum(user.CM$Beauty)+sum(user.CM$hair)+sum(user.CM$cosmetic)+sum(user.CM$course)+sum(user.CM$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","戶外活動","放鬆","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,  col=c("red","darkorange1","green","skyblue1","snow4"),main = "年長男性的消費金額比例")#############
## K-Means分群
##在user_list中新增一欄名為catkm，代表使用K-Means分群後所屬的類別
usr=read.csv("D:/user_list5.csv",head=T)#############################
user.km1=subset(usr,catkm==1)
user.km2=subset(usr,catkm==2)
user.km3=subset(usr,catkm==3)
user.km4=subset(usr,catkm==4)
user.km5=subset(usr,catkm==5)
user.km6=subset(usr,catkm==6)
user.km7=subset(usr,catkm==7)
user.km8=subset(usr,catkm==8)

##觀察八類消費者的人數與性別比例#####################
table(usr$catkm)
round(sum(user.km1$SEX_ID=="m")/813,digits = 2)
round(sum(user.km2$SEX_ID=="m")/137,digits = 2)
round(sum(user.km3$SEX_ID=="m")/17,digits = 2)
round(sum(user.km4$SEX_ID=="m")/4119,digits = 2)
round(sum(user.km5$SEX_ID=="m")/1425,digits = 2)
round(sum(user.km6$SEX_ID=="m")/379,digits = 2)
round(sum(user.km7$SEX_ID=="m")/2401,digits = 2)
round(sum(user.km8$SEX_ID=="m")/10440,digits = 2)

##八類消費者的平均消費金額與標準差#######################
mean(user.km1$Total) ; sd(user.km1$Total)
mean(user.km2$Total) ; sd(user.km2$Total)
mean(user.km3$Total) ; sd(user.km3$Total)
mean(user.km4$Total) ; sd(user.km4$Total)
mean(user.km5$Total) ; sd(user.km5$Total)
mean(user.km6$Total) ; sd(user.km6$Total)
mean(user.km7$Total) ; sd(user.km7$Total)
mean(user.km8$Total) ; sd(user.km8$Total)

## TotalTotal為消費者的總消費金額(已排除宅配類)，以盒狀圖看各類消費者的消費金額分布###################
usr$Total1 <- usr$Total/10000  ##將金額轉為單位:萬元(日圓)
qplot(as.factor(catkm),Total1,data=usr,geom="boxplot",xlab="K-Means所分出的8群消費者",ylab="消費金額")+geom_boxplot(aes(fill=factor(catkm)))+labs(title="Total consumption of Groups")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=20),legend.title=element_text(size=15))+stat_summary(fun.y = "mean", geom = "text", label="---------", size= 12, color= "white")
qplot(as.factor(catkm),Total1,data=subset(usr,catkm!=3),geom="boxplot",xlab="K-Means所分出的8群消費者",ylab="消費金額")+geom_boxplot(aes(fill=factor(catkm)))+labs(title="Total consumption without Group 3")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=17),legend.title=element_text(size=15))+stat_summary(fun.y = "mean", geom = "text", label="----------", size= 12, color= "white")
qplot(as.factor(catkm),Total1,data=subset(usr,catkm!=3&catkm!=2&catkm!=6),geom="boxplot",xlab="K-Means所分出的8群消費者",ylab="消費金額")+geom_boxplot(aes(fill=factor(catkm)))+labs(title="Total consumption without Group 2,3,6")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=17),legend.title=element_text(size=15))+stat_summary(fun.y = "mean", geom = "text", label="--------------", size= 12, color= "white")

##以圓餅圖觀察八類消費者的消費結構#########################
## KM1
p = c(sum(user.km1$accom),sum(user.km1$food),sum(user.km1$fingernail)+sum(user.km1$Beauty)+sum(user.km1$hair)+sum(user.km1$cosmetic),sum(user.km1$outdoor),sum(user.km1$relax),sum(user.km1$course),sum(user.km1$other)+sum(user.km1$healthy)+sum(user.km1$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第一類消費者的消費金額比例")
## KM2
p = c(sum(user.km2$accom),sum(user.km2$food),sum(user.km2$fingernail)+sum(user.km2$Beauty)+sum(user.km2$hair)+sum(user.km2$cosmetic),sum(user.km2$outdoor),sum(user.km2$relax),sum(user.km2$course),sum(user.km2$other)+sum(user.km2$healthy)+sum(user.km2$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第二類消費者的消費金額比例")
## KM3
p = c(sum(user.km3$accom),sum(user.km3$food),sum(user.km3$fingernail)+sum(user.km3$Beauty)+sum(user.km3$hair)+sum(user.km3$cosmetic),sum(user.km3$relax),sum(user.km3$other)+sum(user.km3$healthy)+sum(user.km3$present)+sum(user.km3$outdoor)+sum(user.km3$course))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","放鬆","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls, col=c("red","darkorange1","yellow","skyblue1","snow4"),main = "第三類消費者的消費金額比例")
## KM4
p = c(sum(user.km4$accom),sum(user.km4$food),sum(user.km4$fingernail)+sum(user.km4$Beauty)+sum(user.km4$hair)+sum(user.km4$cosmetic),sum(user.km4$outdoor),sum(user.km4$relax),sum(user.km4$course),sum(user.km4$other)+sum(user.km4$healthy)+sum(user.km4$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第四類消費者的消費金額比例")
## KM5
p = c(sum(user.km5$accom),sum(user.km5$food),sum(user.km5$fingernail)+sum(user.km5$Beauty)+sum(user.km5$hair)+sum(user.km5$cosmetic),sum(user.km5$outdoor),sum(user.km5$relax),sum(user.km5$course),sum(user.km5$other)+sum(user.km5$healthy)+sum(user.km5$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第五類消費者的消費金額比例")
## KM6
p = c(sum(user.km6$accom),sum(user.km6$food),sum(user.km6$fingernail)+sum(user.km6$Beauty)+sum(user.km6$hair)+sum(user.km6$cosmetic),sum(user.km6$outdoor),sum(user.km6$relax),sum(user.km6$course),sum(user.km6$other)+sum(user.km6$healthy)+sum(user.km6$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第六類消費者的消費金額比例")
## KM7
p = c(sum(user.km7$accom),sum(user.km7$food),sum(user.km7$fingernail)+sum(user.km7$Beauty)+sum(user.km7$hair)+sum(user.km7$cosmetic),sum(user.km7$outdoor),sum(user.km7$relax),sum(user.km7$course),sum(user.km7$other)+sum(user.km7$healthy)+sum(user.km7$present))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","snow4"),main = "第七類消費者的消費金額比例")
## KM8
p = c(sum(user.km8$accom),sum(user.km8$food),sum(user.km8$fingernail)+sum(user.km8$Beauty)+sum(user.km8$hair)+sum(user.km8$cosmetic),sum(user.km8$outdoor),sum(user.km8$relax),sum(user.km8$course),sum(user.km8$present),sum(user.km8$other)+sum(user.km8$healthy))
pct <- round(p/sum(p)*100,1)
lbls <- c("住宿","美食","美容相關","戶外活動","放鬆","課程","禮物卡","其他")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(p,labels = lbls,col=c("red","darkorange1","yellow","green","skyblue1","blue","purple","snow4"),main = "第八類消費者的消費金額比例")

## 購物籃分析##############
newdata<-read.csv("D:/userbuy.csv",header=T)

levels(newdata$ITEM)<-c("outdoor","deliver","houseliving","othercoupon","relax","nailandeyesalon","makeups","food","beuty","hairsalon","health","class","giftcard")
nood<-subset(newdata,newdata$ITEM!="deliver")
data1<-split(nood$ITEM, nood$ID)
data2 <-as(data1,"transactions")
colnames(data2)<-levels(nood$ITEM)[-2]

##利用Apriori演算法，設定最小Support值須在0.01以上，Confidence須在0.5以上
rules <-apriori(data2,parameter=list(support=0.01,confidence=0.5))

inspect(sort(rules,by="confidence")[1:10])                            ##列出Confidence前10高的規則
inspect(sort(rules,by="lift")[1:10])                                  ##列出Lift前10高的規則
plot(rules, measurre = c("support", "lift"), shading = "confidence")  ##畫出141條規則的Support對Confidence散佈關係圖

##將Lift前10高的規則畫出動態的示意圖，可看出各種商品組合構成的集合之間的關聯性
ig <- plot( subrules2, method="graph", control=list(type="items") )
tf <- tempfile( )
saveAsGraph( subrules2, file = tf, format = "dot" )
ig_df <- get.data.frame( ig, what = "both" )
visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ifelse(is.na(ig_df$vertices$support),0,ig_df$vertices$support) # could change to lift or confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,ig_df$vertices
  )
  , edges = ig_df$edges
) %>%
  visEdges( arrows = "to" ) %>%
  visOptions( highlightNearest = T )
######LDA######
#genre
data1.lda<-lda(catkm~GENRE_NAME,data=project2)
data1.lda.p<-predict(data1.lda,newdata=project2)$class
sum(diag(table(data1.lda.p,project2[,28])))/length(project2$COUPON_ID_hash)
#price
data1.lda2<-lda(catkm~DISCOUNT_PRICE,data=project2)
data1.lda2.p<-predict(data1.lda2,newdata=project2)$class
sum(diag(table(data1.lda2.p,project2[,28])))/length(project2$COUPON_ID_hash)
#area
data1.lda3<-lda(catkm~small_area_name,data=project3)
data1.lda3.p<-predict(data1.lda3,newdata=project3)$class
sum(diag(table(data1.lda3.p,project3[,28])))/length(project3$COUPON_ID_hash)
#genre+area
data1.lda4<-lda(catkm~GENRE_NAME+small_area_name,data=project3)
data1.lda4.p<-predict(data1.lda4,newdata=project3[,c(7,30)])$class
sum(diag(table(data1.lda4.p,project3[,35])))/length(project3$COUPON_ID_hash)
#genre+price
data1.lda5<-lda(catkm~GENRE_NAME+DISCOUNT_PRICE,data=project3)
data1.lda5.p<-predict(data1.lda5,newdata=project3)$class
sum(diag(table(data1.lda5.p,project3[,35])))/length(project3$COUPON_ID_hash)
#area+price
data1.lda6<-lda(catkm~DISCOUNT_PRICE+small_area_name,data=project3)
data1.lda6.p<-predict(data1.lda6,newdata=project3)$class
sum(diag(table(data1.lda6.p,project3[,35])))/length(project3$COUPON_ID_hash)
#genre+price+area
data1.lda7<-lda(catkm~GENRE_NAME+DISCOUNT_PRICE+small_area_name,data=project3)
data1.lda7.p<-predict(data1.lda7,newdata=project3)$class
sum(diag(table(data1.lda7.p,project3[,35])))/length(project3$COUPON_ID_hash)
#genre+rate+area
data1.lda8<-lda(catkm~GENRE_NAME+PRICE_RATE+small_area_name,data=project3)
data1.lda8.p<-predict(data1.lda8,newdata=project3)$class
sum(diag(table(data1.lda8.p,project3[,35])))/length(project3$COUPON_ID_hash)
#genre+catprice+area
data1.lda9<-lda(catkm~GENRE_NAME+CATALOG_PRICE+small_area_name,data=project3)
data1.lda9.p<-predict(data1.lda9,newdata=project3)$class
sum(diag(table(data1.lda9.p,project3[,35])))/length(project3$COUPON_ID_hash)
#genre+lar_area
data1.lda10<-lda(catkm~GENRE_NAME+large_area_name,data=project3)
data1.lda10.p<-predict(data1.lda10,newdata=project3)$class
sum(diag(table(data1.lda10.p,project3[,35])))/length(project3$COUPON_ID_hash)
train_control <- trainControl(method="cv", number=10)
project3$catkm<-as.factor(project3$catkm)
model1 <-train(catkm ~GENRE_NAME+small_area_name, data=project3, method="lda",trControl=train_control)
##########################################################
#heatmap
a<-table(project3$cat1,project3$large_area_name)
a<-apply(a,2,function(x){x/sum(x)})
a1<-as.table(a)
a2<-as.data.frame(a1)
names(a2)<-c("cat1","large_area_name","density")
ggplot(a2,aes(large_area_name,cat1,fill=density))+geom_tile()+ scale_fill_gradient(low = "yellow",high="red")+labs(title="Heatmap of large area name")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=30),legend.title=element_text(size=15))

b<-table(project3$cat1,project3$GENRE_NAME)
b<-apply(b,2,function(x){x/sum(x)})
b1<-as.table(b)
b2<-as.data.frame(b1)
names(b2)<-c("cat1","GENRE_NAME","density")
ggplot(b2,aes(GENRE_NAME,cat1,fill=density))+geom_tile()+ scale_fill_gradient(low = "yellow",high="red")+labs(title="Heatmap of GENRE NAME")+theme(legend.text=element_text(size = 20),axis.text=element_text(size = 20),axis.title=element_text(size=20),title=element_text(size=30),legend.title=element_text(size=15))
#######ctree################
library(partykit)
library(party)

fit1 <- ctree(cat1 ~ GENRE_NAME+large_area_name+DISCOUNT_PRICE ,control=ctree_control(maxdepth = 6),data = project2)
fit2 <- ctree(cat1 ~ GENRE_NAME+large_area_name ,control=ctree_control(maxdepth = 6),data = project2)

plot(fit1,gp = gpar(fontsize = 10),type = "simple")

pretree1<-predict(fit1,project2)
sum(pretree1==project2$cat1)/sum(table(pretree1,project2$cat1))

pretree2<-predict(fit2,project2)
sum(pretree2==project2$cat1)/sum(table(pretree2,project2$cat1))
#######neuralnet################
library(NeuralNetTools)
library(neuralnet)
library(nnet)

project3$discount<-scale(project3$discount)

fitnet1<-nnet(cat1 ~ GENRE_NAME+large_area_name+DISCOUNT_PRICE,data=project2,size=6,decay = 0.1)

fitnet2<-nnet(cat1 ~ GENRE_NAME+large_area_name,data=project2,size=6,decay = 0.1)

plotnet(fitnet2,circle_col=rainbow(9),pos_col="blue",neg_col="red",bord_col="black" ,cex_val =0.7,alpha_val=0.1,bias=T)

pre1<-predict(fitnet1,project2,type = "class")
table(pre1,project2$cat1)
sum(pre1==project2$cat1)/sum(table(pre1,project2$cat1))

pre2<-predict(fitnet2,project2,type = "class")
table(pre2,project2$cat1)
sum(pre2==project2$cat1)/sum(table(pre2,project2$cat1))
############################################################knn&naivebayes
library(e1071)
library(class)
library(kknn)
library(klaR)

m1<-kknn(cat1 ~GENRE_NAME+large_area_name+DISCOUNT_PRICE,project2,project2,k=6,distance = 2, kernel = "optimal")
m2<-kknn(cat1 ~GENRE_NAME+large_area_name,project2,project2,k=6,distance = 2, kernel = "optimal")

sum(fitted(m1)==project2$cat1)/sum(table(fitted(m1),project2$cat1))
sum(fitted(m2)==project2$cat1)/sum(table(fitted(m2),project2$cat1))
#############cross validation 手動分群#############
train_control <- trainControl(method="cv", number=10)
model4 <- train(cat1 ~ GENRE_NAME+large_area_name, data=project2, method='ctree2',trControl=train_control,tuneGrid=data.frame(maxdepth=c(2:8)))
model5 <- train(cat1 ~ GENRE_NAME+large_area_name,data=project2, method='nnet',trControl=train_control,tuneGrid=data.frame(size=2:8,decay = 0.1))
model6 <- train(cat1 ~GENRE_NAME+large_area_name,data=project2, method='kknn',trControl=train_control,tuneGrid=data.frame(kmax =8 , distance = 2, kernel = "optimal"))
model7 <- train(cat1 ~GENRE_NAME+large_area_name,data=project2, method='kknn',trControl=train_control,k=7)

cpr4<-predict(model4,project2)
confusionMatrix(cpr4,project2$cat1)

cpr5<-predict(model5,project2)
confusionMatrix(cpr5,  project2$cat1)

cpr6<-predict(model6,project2)
confusionMatrix(cpr6, project2$cat1)

plot(model4)
plot(model5)
plot(model6)
str(model6)
model6$results
plot(model6$coefnames)

print(system.time(train(cat1 ~GENRE_NAME+large_area_name+DISCOUNT_PRICE+CATALOG_PRICE,data=project2, method='kknn',trControl=train_control,tuneGrid=data.frame(kmax =8 , distance = 1.5, kernel = "optimal"))))
######cross validation k-mean分群##########################
library(caret)

train_control <- trainControl(method="cv", number=10)
model1 <- train(catkm ~ GENRE_NAME+large_area_name+DISCOUNT_PRICE , data=project2, method='ctree2',trControl=train_control,tuneGrid=data.frame(maxdepth=2:8))
model2 <- train(catkm ~ GENRE_NAME+large_area_name+DISCOUNT_PRICE,data=project2, method='nnet',trControl=train_control,tuneGrid=data.frame(size=2:8,decay = 0.1))
model3 <- train(catkm ~GENRE_NAME+large_area_name+DISCOUNT_PRICE,data=project2, method='kknn',trControl=train_control,tuneGrid=data.frame(kmax = 8, distance = 2, kernel = "optimal"))


cpr1<-predict(model1,project2)
confusionMatrix(model1$pred,project2$catkm)

cpr2<-predict(model2,project2)
confusionMatrix(cpr2,  project2$catkm)

cpr3<-predict(model3,project2)
confusionMatrix(cpr3, project2$catkm)

plot(model1)
plot(model2)
plot(model3
