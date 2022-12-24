install.packages('tidyverse')
install.packages('gridExtra')

#load dataframe
setwd("C:/Users/hp/Desktop/R/Du lieu dau vao")
tuyen<-read.csv("Du lieu goc.csv",header=T)
attach(tuyen)

#bieu do E5 voi gia tri trung binh và chu thich
ggplot(tuyen, aes(x = L, y = E5,fill=L))+
xlab("E5(tan/km)")+ylab("Loai xe") + 
geom_boxplot() +
stat_summary(fun = "mean", geom = "point", col = "red") +  #them diem trung binh
stat_summary(fun = "mean", geom = "text",vjust = 3.5, aes(label = paste("Mean:", round(..y.., digits = 4))))+   #them label
theme_bw()

#Anova E5 (phuong sai)
tuyen = aov (E5 ~ L)
summary (tuyen)
TukeyHSD (tuyen)
plot(TukeyHSD (tuyen),ordered=T)

#dánh giá ch? tiêu E6
    #bi?u d? h?p E6
p = ggplot(tuyen, aes(x=LV,y=E6))
p + geom_boxplot(aes(fill=L))+ theme_bw()+xlab("E6 (duong chet/quang duong)")+ylab("Loai xe)")+ stat_summary(fun="mean",geom="point",col="white")
by(E6, d, summary)
p = ggplot(tuyen, aes(x=E6,y=d))
p + geom_boxplot(aes(fill=d))+ theme_bw()+xlab("E6 (duong chet/quang duong)")+ylab("Tuyen thu gom")  + stat_summary(fun="mean",geom="point",col="white")

#Anova E6 (phuong sai)
tuyen = aov (E6 ~ d)
summary (tuyen)
TukeyHSD (tuyen)
plot(TukeyHSD (tuyen),ordered=T)

#Ðánh giá ch? tiêu E4
p = ggplot(tuyen, aes(x=E4,y=XE))+ xlab("E4(thoi gian nghi/thoi gian chay)")+ylab("XE")
p + geom_boxplot(aes(fill=L))+ theme_bw()
by(E5, L, summary)
p = ggplot(tuyen, aes(x=E4,y=L))+ xlab("E4(thoi gian dung/thoi gian chay)")+ylab("Loai xe")
p + geom_boxplot(aes(fill=L))+ theme_bw()+ stat_summary(fun="mean",geom="point",col="white")

#Anova E4
tuyen = aov (E4 ~ d)
summary (tuyen)
TukeyHSD (tuyen)
plot(TukeyHSD (tuyen),ordered=T)


#Ðánh giá ch? tiêu E2
p = ggplot(tuyen, aes(x=L,y=E2))+ xlab("Loai xe")+ylab("E2(dong/tan)")
p + geom_boxplot(aes(fill=L))+ theme_bw()+ stat_summary(fun="mean",geom="point",col="red",size=2)
p = ggplot(tuyen, aes(x=E2,y=XE))+ xlab("E2(dong/tan)")+ylab("XE")
p + geom_boxplot(aes(fill=L))+ theme_bw()+ stat_summary(fun="mean",geom="point",col="red",size=3)

#Anova E2
tuyen = aov (E2 ~ L)
summary (tuyen)
TukeyHSD (tuyen)
plot(TukeyHSD (tuyen),ordered=T)

#Ðánh giá ch? tiêu E1
p = ggplot(tuyen, aes(x=L,y=E1)) +  xlab("Loai xe")+ylab("E1(tan/km)")
p + geom_boxplot(aes(fill=L))+ theme_bw()+ stat_summary(fun="mean",geom="point",col="white")

#Anova E1
tuyen = aov (E1 ~ L)
summary (tuyen)
TukeyHSD (tuyen)
plot(TukeyHSD (tuyen),ordered=T)

#Ðánh giá ch? tiêu E3
p = ggplot(tuyen, aes(x=d,y=E3)) +  xlab("Tuyen duong")+ylab("E3(quang duong chet/chieu dai tuyen)")
p + geom_boxplot(aes(fill=d))+ theme_bw() 

#Anova E3
tuyen = aov (E3 ~ d)
summary (tuyen)
TukeyHSD (tuyen)   
plot(TukeyHSD (tuyen),ordered=T)

#
g0 <- ggplot(tuyen, aes(x=LV, fill=d))+ facet_wrap(~L) + geom_bar(fill="3") + xlab("Ca lam viec") + ylab("so chuyen")+ theme_bw() 
g3 <- ggplot(tuyen, aes(x=L, y=E3, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E3(duong chet/quang duong)") + labs(fill="Loai xe")
g2 <- ggplot(tuyen, aes(x=L, y=E2, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E2(dong/tan)")+ labs(fill="Loai xe")
g1 <- ggplot(tuyen, aes(x=L, y=E1, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E1(tan/km)")+ labs(fill="Loai xe")
g4 <- ggplot(tuyen, aes(x=L, y=E4, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E4(thoi gian nghi/thoi gian chay)")+ labs(fill="Loai xe")
g5 <- ggplot(tuyen, aes(x=L, y=E5, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E5(tan/km)")+ labs(fill="Loai xe")
g6 <- ggplot(tuyen, aes(x=L, y=E6, fill=L)) + geom_boxplot() + theme_bw() + xlab("Loai xe") + ylab("E6(duong chet/quang duong)")+ labs(fill="Loai xe")
grid.arrange(g0, g2, g4, g5, g6, nrow = 1)


g3 <- ggplot(tuyen, aes(x=E3, group=L, fill=L)) + geom_density(adjust=1.5, alpha=0.5)
g2 <- ggplot(tuyen, aes(x=E2, group=L, fill=L)) + geom_density(adjust=1.5, alpha=0.5)
g1 <- ggplot(tuyen, aes(x=E1, group=L, fill=L)) + geom_density(adjust=1.5, alpha=0.5)
g4 <- ggplot(tuyen, aes(x=E4, group=L, fill=L)) + geom_density(adjust=1.5, alpha=0.5)
grid.arrange(g1, g2, g3, g4, nrow = 1)

by(To,L, summary)

#Kh?i lu?ng rác theo ngay
p = ggplot(tuyen, aes(x=M, y=d))
p + geom_bar(stat = "identity",fill="lightblue") + facet_wrap(~NGAY) + xlab("Khoi luong rac(tan)")+ylab("Tuyen duong thu gom") + theme_bw()

#Tong khoi luong rac theo tuyen
p = ggplot(tuyen, aes(x=M, y=d))
p + geom_bar(stat = "identity",fill="lightblue") +xlab("Tong khoi luong rac(tan)")+ylab("Tuyen duong thu gom")+ theme_bw()

#Khoi luong rac trung binh
p = ggplot(tuyen, aes(x=M/14, y=d))
p + geom_bar(stat = "identity",fill="lightblue") +xlab("Khoi luong rac trung binh(tan)")+ylab("Tuyen duong thu gom")+ theme_bw()

#Tong chi phi thu gom rác
p = ggplot(tuyen, aes(x=E2, y=XE))
p + geom_bar(stat = "identity",fill="lightblue") +xlab("Tong chi phi (dong)")+ylab("XE")+ theme_bw()

#Bieu do so chuyen di chuyen theo loai xe
p = ggplot(tuyen, aes(x=L, fill=d))
p + geom_bar() +xlab("Loai xe")+ylab("So chuyen")+ theme_bw()

#Kiem tra tuong quan thoi gian dung va khoi luong rác thu gom duoc
dat=cbind(Tr, M)
require(psych)
pairs.panels(dat)

#Ki?m tra tuong quan
g1=lm(E1~N)
summary(g1)

#Bieu do hoi quy thoi gian dung và khoi luong rac
p = ggplot(tuyen, aes(x=Tr, y=M))
p + geom_point()+geom_smooth(method=lm , color="red", se=FALSE) + theme_bw()+ xlab("thoi gian dung")+ylab("khoi luong rac")

#Bieu do so luong các chuyen xe di chuyen theo chi phi
p = ggplot(tuyen, aes(x=L))
p + geom_bar() + facet_wrap(~MG)

#Bieu do ket h?p
g1<- ggplot(tuyen, aes(x=L, y=Q)) + geom_point() + facet_wrap(~MG)+theme_bw()+ xlab("")+ylab("Chieu dai quang duong")
g2<- ggplot(tuyen, aes(x=L)) + geom_bar(fill="lightblue") + facet_wrap(~MG)+theme_bw()+ xlab("Loai xe")+ylab("So chuyen")
grid.arrange(g1, g2, nrow = 2)
