##loading the dataset ----
googleplaystore <- read.csv("~/Studium/Data-Science-General/HS2021/EXPD/Projects/googleplaystore/googleplaystore.csv", na.strings="NaN")
playstore<-as.data.frame(googleplaystore)
View(playstore)
str(playstore)








##cleaning some columns of the dataset ----



#to find the NAs after getting:"NAs introduced by coercion"
#I used: which(is.na(x)) after applyin function to find
#what are the abnormalities
#the first abnormality is line 10473. I think it is faulty:
#rating is by 19 and price is everyone and size is 1,000+
#so we delete this line:
playstore[10473,]
playstore<- playstore[-10473,]




#cleaning the Reviews:
str(playstore$Reviews)
playstore$Reviews<-as.numeric(playstore$Reviews)



#cleaning the size:
str(playstore$Size)

#to replace 1.5k with 1500
#we need the following library to do all these:
#install.packages("stringr")
library("stringr")
nonDecimalVec<-stringr::str_extract(string = playstore$Size,pattern ="\\.([0-9])*")
#replace NAs with empty string , so we have an easier
#job, when we use paste function later
nonDecimalVec[is.na(nonDecimalVec)]<-"" 
playstore$Size<-sub(pattern = "\\.[0-9]*k","000",playstore$Size)
playstore$Size<-sub(pattern = "\\.[0-9]*M","000000",playstore$Size)
#finally: adding the nachkommastellen back to the number
#if they had any example: 1.05k= (1+0.05)*1000:
vsel<-nonDecimalVec!=""
temp_size<-(as.numeric(playstore$Size[vsel]))
temp_size2<-rep(1,sum(vsel))
temp_size3<-as.numeric(nonDecimalVec[vsel])
temp_size4<-temp_size2+temp_size3
temp_size<-temp_size4*temp_size
playstore$Size[vsel]<-temp_size

#to replace the likes of 10k with 1000 or m with 1000000:
playstore$Size<-sub(pattern = "k","000",playstore$Size)
playstore$Size<-sub(pattern = "M","000000",playstore$Size)
#size also contains the string: "Varies with device"
#So we should be carful about that!
playstore$Size[playstore$Size=="Varies with device"]<-NA
playstore$Size<-as.numeric(playstore$Size)
# coercedNas<-which(is.na(playstore$Size))
# coercedNas




#cleaning the price:
str(playstore$Price)
playstore$Price<-sub(pattern = "\\$", replacement = "", x = playstore$Price)
playstore$Price<-as.numeric(playstore$Price)
#The following two lines helped me with debugging and cleaning
# coercedNas<-which(is.na(playstore$Price))
# coercedNas




#I removed the following line because we don't want to 
#lose information, forexmpale if an app doesn't have 
#ratings, it could be that it was downloaded very
#little.
#playstore<-playstore[complete.cases(playstore), ]





##lagenmass ----



#calculate the means:
mean_table<-colMeans(playstore[,c("Rating","Reviews","Size","Price")],na.rm = T)
mean_table
#It might make sense to convert this to numeric
#to calculate the mean. This is an ordnial category
#but the difference from categroy to other is not the
#same between each pair of categories:
#(500-100)=400 but (100-50)=50
Installs_<-sub(pattern = "\\+", replacement = "", x = playstore$Installs)
Installs_<-gsub(pattern = ",", replacement = "", x = Installs_)
Installs_<-as.numeric(Installs_)
Installs_mean<-mean(Installs_)
Installs_mean
quantile(playstore$Reviews,probs = c(0.25,0.5,0.75),type=7)
quantile(playstore$Price,probs = c(0.25,0.5,0.75),type=7)
installsfac<-factor(playstore$Installs, labels =c("0","0+","1+","5+","10+","50+","100+","500+","1,000+","5,000+","10,000+","50,000+","100,000+","500,000+","1,000,000+","5,000,000+","10,000,000+","50,000,000+","100,000,000+","500,000,000+","1,000,000,000+"), ordered = T )

playstore$Installs<-installsfac



##some univariate plots----
hist((playstore$Rating[!is.na(playstore$Rating)]),xlab = "between 1 and 6",main = "Ratings",breaks = 20)
hist(log10(playstore$Reviews[!is.na(playstore$Reviews)]),xlab = "reviews",main = "reviews")
par(mar=c(10,3,1,1))
barplot(table(playstore$Category),las=3,cex.names = 0.7)
hist(playstore$Size,xlab = "Size [Bytes]")
hist(log10(playstore$Price),xlab = "Price [Us Dollar]")
pie(table(playstore$Type),col = rainbow(2))


        
##some bivariate plots----

#Review, Price, installs, size, rating, category:
dev.off()
ratingcut<-cut(playstore$Rating,breaks = c(0.99,1.99,2.99,3.49,3.99,4.49,5))
ratingcut<-factor(ratingcut,levels = levels(ratingcut),labels = c("1+","2+","3+","3.5+","4+","4.5+"),ordered = T)
plot(Price~sqrt(Reviews),data = playstore,col=rainbow(6)[ratingcut],xlab = "Review")
legend("topright",legend = c("1+","2+","3+","3.5+","4+","4.5+"),fill = rainbow(6),cex=0.4,ncol = 2)


ggplot(data=playstore,aes(x=Reviews,y=Price))+geom_point()

ggplot(data=playstore)+geom_histogram(aes(x=Reviews))

ggplot(data=playstore)+geom_histogram(aes(x=log(Reviews)))



#or:
library(vcd)
dev.off()
reviewCut<-cut(playstore$Reviews,breaks = c(0,1000,10000,100000,80000000))
reviewCut<-factor(reviewCut,levels = levels(reviewCut),labels = c("0+","1000+","10k+","100k+"),ordered = T)
priceCut<-cut(playstore$Price,breaks = c(0,10,30,500))
priceCut<-factor(priceCut,levels = levels(priceCut),labels = c("0+","10+","30+"),ordered = T)
mosaicplot(table(priceCut,reviewCut),ylab = "Reviews",xlab = "Price",main="Reviews for each Price",col=c(1,2,3,4))




dev.off()
par(xpd=T,mar=c(8,4,3,5))
boxplot(Reviews~Installs_,data = playstore,las=3,xlab = "")
mtext(text = "Installs",side = 1,line = 5)
par(cex=1)
cor(playstore$Reviews,Installs_,method = "spearman")
#why does the following not work? maybe because of pluses in forexample 5,000+
#boxplot(Reviews~Installs_,data = playstore,las=3,xlab = "")



dev.off()
par(xpd=T,mar=c(8,4,3,5))
install_cat_table<-aggregate(Installs_~Category,data = playstore,FUN=sum)
vsel<-order(install_cat_table[,2],decreasing=T)
tab.agg<-install_cat_table[,2]
names(tab.agg)<-install_cat_table[,1]

df.bar<-barplot(tab.agg[vsel],las=3,cex.names = 0.6,ylab = "Number of Installs")
par(cex=1)
axis(1,labels = "categories",at=c(-1),las=2)










dev.off()
stripchart(sqrt(Reviews)~ratingcut,data = playstore,vertical = TRUE,method="jitter",ylab="Reviews",xlab="Ratings")





dev.off()
par(xpd=NA,mar=c(15,5,4,3),cex=0.5)
boxplot(Size~Category,data=playstore,las=3,ylab = "Size",xlab = "",main="Category vs Size")
par(cex=1)
axis(1,labels = "category",at=c(-1),las=2)












