library(foreign)
dat <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_cov.dta")
dat2 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_cd3.dta")
dat3 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_dl1.dta")
head(dat,3)
dat <- dat[,c("age","marstat","sex","hhid14","pidlink")]
head(dat2,3)
dat2 <- dat2[,c("cdtype","cd05","hhid14","pidlink")]
head(dat3,3)
dat3 <- dat3[,c("dl06","hhid14","pidlink")]
library(dplyr)
datnew <- dat2 %>% left_join(y= dat, by = c("hhid14","pidlink")) %>%
  left_join(y= dat3, by = c("hhid14","pidlink"))
head(datnew)
nrow(datnew)
datnew <- data.frame(id = datnew$pidlink, penyakit = datnew$cdtype, status = datnew$cd05,
                    usia = datnew$age,
                     menikah = datnew$marstat, gender = datnew$sex,
                     pendidikan = datnew$dl06)
head(datnew)
str(datnew)
unique(datnew$pendidikan)
levels(datnew$pendidikan)
library(plyr)
datnew$pendidikan <- revalue(datnew$pendidikan, c("2:Elementary school" = "SD/Sederajat", 
                                "3:Junior high general" = "SMP/Sederajat",
                                "5:Senior high general" = "SMA/Sederajat", 
                                "6:Senior high vocational" = "SMA/Sederajat", 
                                "13:Open university" = "Pend.Tinggi", 
                                "60:College (D1,D2,D3)" = "Pend.Tinggi", 
                                "61:University S1" = "Pend.Tinggi", 
                                "62:University S2" = "Pend.Tinggi", 
                                "63:University S3" = "Pend.Tinggi", 
                                "72:Islamic Elementary School (Madrasah Ibtidaiyah)" = "SD/Sederajat", 
                                "73:Islamic Junior/High School (Madrasah Tsanawiyah)" = "SMP/Sederajat", 
                                "74:Islamic Senior/High School (Madrasah Tsanawiyah)" = "SMA/Sederajat"))
table(datnew$pendidikan)
datnew <- datnew[datnew$pendidikan == "SD/Sederajat"|
            datnew$pendidikan =="SMP/Sederajat"|
            datnew$pendidikan =="SMA/Sederajat"|
            datnew$pendidikan =="Pend.Tinggi",]
unique(datnew$penyakit)
datnew$penyakit <- revalue(datnew$penyakit, c("A" = "Hipertensi", "B" = "Diabetes"))
table(datnew$penyakit)
nrow(datnew)
datnew <- datnew[datnew$penyakit == "Hipertensi"|
                   datnew$penyakit =="Diabetes",]
nrow(datnew)
table(datnew$penyakit)
levels(datnew$status)
datnew$status <- revalue(datnew$status, c("1:Yes" = "Ya", "3:No" = "Tidak"))
datnew <- datnew[datnew$status == "Ya"|
                   datnew$status == "Tidak",]
head(datnew)
nrow(datnew)
levels(datnew$menikah)
datnew$menikah <- revalue(datnew$menikah, c("1:Not yet married" = "Belum",
                                            "2:Married" = "Sudah"))
table(datnew$menikah)
datnew <- datnew[datnew$menikah == "Belum"|
                   datnew$menikah== "Sudah",]
head(datnew)
nrow(datnew)
levels(datnew$gender)
datnew$gender <- revalue(datnew$gender, c("1:Male" = "Pria", "3:Female" = "Wanita"))
datnew <- datnew[datnew$gender == "Pria"|
                 datnew$gender== "Wanita",]
table(datnew$gender)
head(datnew)
nrow(datnew)
str(datnew)
datnew$id <- as.character(datnew$id)
sapply(datnew, function(x) sum(is.na(x)))
nrow(datnew)
d <- datnew[complete.cases(datnew),]
nrow(d)
head(d)
rownames(d) <- NULL
head(d)
str(d)
table(d$penyakit)
levels(d$penyakit)
d[,-c(1,4)] <- lapply(d[,-c(1,4)], function(x) droplevels(x))
head(d)
str(d)
nrow(d)
length(unique(d$id))
head(d)
a <- setNames(as.data.frame(table(d$id)), c("id", "n"))
a
a$id <- as.character(a$id)
head(a)
sum(a$n==1)
head(d[d$id == "002110009",])
buang <- a[a$n ==1,]$id
buang
class(buang)
nrow(d)
head(d)
str(d)
#buang identitas yang n=1
da <- d[!d$id %in% buang,]
nrow(da)
sum(setNames(as.data.frame(table(da$id)), c("id", "n"))$n == 1)
head(da)
library(tidyverse)
length(da$id)
da <- da %>% spread(penyakit, status)
head(da)
length(unique(da$id))*2
head(da)
nrow(da)
length(unique(da$id))
sapply(da, function(x) sum(is.na(x)))
head(da)
str(da)
write.csv2(da, "D:\\Adi\\Workshop Data Wrangling in R\\datwrangling.csv")
str(da)
summary(da)
tapply(da$usia, da$Hipertensi, summary)
tapply(da$usia, da$Diabetes, summary)
library(psych)
describeBy(da[,-1], group = da$Hipertensi)
describeBy(da[,-1], group = da$Diabetes)
describeBy(da[da$Diabetes == "Ya",-1], group = da[da$Diabetes == "Ya",]$Hipertensi)
library(ggplot2)
library(viridis)
library(hrbrthemes)
da %>%
  ggplot( aes(x=da$pendidikan, y=da$usia, fill=Hipertensi)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="B") +
  theme_ipsum() +
  theme(
    legend.position="left",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("Pendidikan")+ylab("Usia")
da %>%
  ggplot( aes(x=da$Diabetes, y=da$usia, fill=Hipertensi)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="B") +
  theme_ipsum() +
  theme(
    legend.position="left",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("Pendidikan")+ylab("Usia")
ggplot(da, aes(x=gender, y=usia, fill=Diabetes)) +
   geom_boxplot()+theme_classic()+
   scale_fill_manual(values =c("lightblue","mistyrose"))
levels(da$Hipertensi)
ggplot(data=da, aes(x=pendidikan, fill=Hipertensi)) +
  geom_bar(stat="count", position=position_dodge())+
  theme_minimal()+xlab("Pendidikan")+ylab("Frekuensi")+
  scale_fill_manual(values=c("green","red"),
                       labels=c("Ya", "Tidak"))
str(da)
summary(da$pendidikan)
a <- as.vector(summary(da$pendidikan))
a
piepercent<- round(100*a/sum(a), 1)
piepercent
b <- c("29.7%","20%","35.1%", "15.2%")
pie(a, labels = b, main = " ",col =c("lightgreen","red", "skyblue", "grey"))
legend("right", levels(da$pendidikan), cex = 0.7,
         fill = c("lightgreen","red", "skyblue", "grey"), box.lty = NULL, bty = "n",
         inset = 0.9)
head(da)
str(da)
levels(da$Hipertensi)
levels(da$Hipertensi) <- c("1","0")
levels(da$Diabetes) <- c("1","0")
da$Hipertensi <- relevel(da$Hipertensi, ref = "0")
da$Diabetes <- relevel(da$Diabetes, ref = "0")
str(da)
head(da)
fit <- glm(Hipertensi~., family = binomial, data = da[,-1] )
summary(fit)
library(caret)
set.seed(2021)
index <- createDataPartition(da$Hipertensi, p = .80, list = FALSE)
train <- da[index,]
test <- da[-index,]
nrow(train)
nrow(test)
library(nnet)
fit <- glm(Hipertensi~., family = binomial, data = train[,-1])
summary(fit)
test$pred <- as.numeric((predict(fit, test, type='response'))>
                                 0.5)
tab <- table(Predicted = test$pred, Reference = test$Hipertensi)
tab
accuracy <- (tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
accuracy
library(pROC)
rocplot <- roc(train$Hipertensi ~ fitted(fit), data=train)
plot.roc(rocplot, legacy.axes=TRUE)
auc(rocplot)
