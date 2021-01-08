install.packages(c("ggplot2"), dep=TRUE)
install.packages(c("corrplot"), dep=TRUE)
install.packages(c("mapdata"), dep=TRUE)
install.packages(c("oz"), dep=TRUE)
install.packages(c("usmap"), dep=TRUE)
install.packages(c("grid"), dep=TRUE)
install.packages(c("moments"), dep=TRUE)
install.packages(c("distr"), dep=TRUE)
install.packages(c("ggpubr"), dep=TRUE)
install.packages(c("Hmisc"), dep=TRUE)
install.packages(c("readxl"), dep=TRUE)
install.packages("readxl")
install.packages("stargazer")

install.packages(c("car"), dep=TRUE)
install.packages(c("lmtest"), dep=TRUE)
install.packages(c("het.test"), dep=TRUE)
install.packages(c("corrplot"), dep=TRUE)
install.packages(c("sandwich"), dep=TRUE)

library(ggplot2)
library(corrplot)
library(maps)
library(mapdata)
library(oz)
library(usmap)
library(grid)
library(moments)
library(distr)
library(ggpubr)
library(Hmisc)
library(readxl)
library(stargazer)

library(car)
library(foreign)
library(MASS)
library(lmtest)
library(het.test)
library(sandwich)

# Setting and import data set
getwd()
setwd("/Users/sannul/Google Drive(sannul@hanyang.ac.kr)/Mathematics/Math Capstone PBL (Data Analysis)/Project 1")

rm(list = ls(all.names = TRUE))
gc()

rndceo2017 = read.csv('rndceo2017.csv')
rndceo2017 <- rndceo2017[,2:28]
rndceo2017t <- read.csv("rndceo2017t.csv")

# After data transformation
write.csv(rndceo2017, 'rndceo2017t.csv', row.names=F)

View(rndceo2017)
summary(rndceo2017)
stargazer(rndceo2017t)

str(rndceo2017)
colnames(rndceo2017)

unique(rndceo2017$femaleceo)
table(rndceo2017$femaleceo)
idcs<-which(rndceo2017$state=='NY')
data$state[idcs]

# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Shapiro-Wilk test
shapiro.test(rndceo2017$size)
summary(rndceo2017)

# correlation test
# fcf roa ceopayslice
# NA: leverage, cashratio, invest, ceoequity
cor.test(rndceo2017$ceotenure, rndceo2017$rndratio, method="pearson")

# Kendall's rank correlation tau
# roa, ceocomp, ceopayslice, ceoequity, ceotenure
cor.test(rndceo2017t$roa, rndceo2017t$rndratio, method="kendall")
cor.test(rndceo2017t$ceocomp, rndceo2017t$rndratio, method="kendall")
cor.test(rndceo2017t$ceoequity, rndceo2017t$rndratio, method="kendall")
cor.test(rndceo2017t$ceotenure, rndceo2017t$rndratio, method="kendall")
cor.test(rndceo2017t$ceopayslice, rndceo2017t$rndratio, method="kendall")

stargazer(kc1, kc2)
kc1[c(kc1$p.value, kc1$estimate)]
kc1[kc1$p.value]
stargazer(c(kc1$estimate, kc1$p.value), c(kc2$estimate, kc2$p.value))

# Spearman's rank correlation rho
# roa, ceocomp, ceopayslice, ceoequity, ceotenure
cor.test(rndceo2017$ceotenure, rndceo2017$rndratio, method="spearman")


sl1 <- lm(rndratio~size, rndceo2017t)
sl2 <- lm(rndratio~leverage, subset(rndceo2017t, leverage>-Inf))
sl3 <- lm(rndratio~tobinsq, rndceo2017t)
sl4 <- lm(rndratio~cashratio, subset(rndceo2017t, cashratio>-Inf))
sl5 <- lm(rndratio~invest, subset(rndceo2017t, invest>-Inf))
sl6 <- lm(rndratio~ceoage, rndceo2017t)

sl7 <- lm(rndratio~bm, rndceo2017t)
sl8 <- lm(rndratio~salesgrowth, rndceo2017t)
sl9 <- lm(rndratio~fcf, rndceo2017t)
sl10 <- lm(rndratio~hhi, rndceo2017t)
sl11 <- lm(rndratio~opperf, rndceo2017t)
sl12 <- lm(rndratio~intan, rndceo2017t)

stargazer(sl1,sl2,sl3,sl4,sl5,sl6, title="Results", align=TRUE)
stargazer(sl7,sl8,sl9,sl10,sl11,sl12, title="Results", align=TRUE)

summary(lm(rndratio~invest, subset(rndceo2017, invest>-Inf)))
summary(lm(rndratio~ceoage, rndceo2017))
coef(lm(rndratio~size, rndceo2017))

cor.test(rndceo2017$ceotenure, rndceo2017$rndratio, method="kendall")

# Plot of between R&D ratio and independent variables
p1 <- ggplot(rndceo2017t, aes(x=size, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=4, y=0.8, label="R-squared:  0.07436") +
  labs(x="Size", y="R&D ratio")
p2 <- ggplot(rndceo2017t, aes(x=bm, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  coord_cartesian(xlim = c(-3,3)) +
  #geom_text(x=-2.3, y=0.8, label="R-squared:  0.009016") +
  labs(x="BM", y="R&D ratio")
p3 <- ggplot(rndceo2017t, aes(x=fcf, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  coord_cartesian(xlim = c(min(rndceo2017$fcf),1)) +
  #geom_text(x=-0.6, y=0.8, label="R-squared:  0.0005925") +
  labs(x="FCF", y="R&D ratio")
p4 <- ggplot(rndceo2017t, aes(x=hhi, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  coord_cartesian(xlim = c(min(rndceo2017$hhi),0.3)) +
  #geom_text(x=0.05, y=0.8, label="R-squared:  0.02198") +
  labs(x="HHI", y="R&D ratio")
p5 <- ggplot(rndceo2017t, aes(x=opperf, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=-0.6, y=0.8, label="R-squared:  0.004678") +
  labs(x="Operating performance", y="R&D ratio")
p6 <- ggplot(rndceo2017t, aes(x=leverage, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=-8.5, y=0.8, label="R-squared:  0.04347") +
  labs(x="Leverage", y="R&D ratio")
p7 <- ggplot(rndceo2017t, aes(x=tobinsq, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=2.5, y=0.8, label="R-squared:  0.1474") +
  labs(x="Tobin's Q", y="R&D ratio")
p8 <- ggplot(rndceo2017t, aes(x=salesgrowth, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=-0.5, y=0.8, label="R-squared:  0.004201") +
  labs(x="Sales growth", y="R&D ratio")
p9 <- ggplot(rndceo2017t, aes(x=cashratio, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=-10.3, y=0.8, label="R-squared:  0.1574") +
  labs(x="Cash ratio", y="R&D ratio")
p10 <- ggplot(rndceo2017t, aes(x=intan, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=0.12, y=0.8, label="R-squared:  0.05728") +
  labs(x="Intangibility", y="R&D ratio")
p11 <- ggplot(rndceo2017t, aes(x=invest, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=-5, y=0.8, label="R-squared:  0.08387") +
  labs(x="Investment ratio", y="R&D ratio")
p12 <- ggplot(rndceo2017t, aes(x=ceoage, y=rndratio)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  #geom_text(x=39.5, y=0.8, label="R-squared:  0.002723") +
  labs(x="CEO's age", y="R&D ratio")
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, cols=3)


# Boxplots of dummy variables
# State 3, 2 categories
state3 <- rndceo2017[,c("state","rndratio")]
state3 <- state3[state3$state!="",]
state3 <- state3[state3$rndratio<0.8,]
state3[(state3$state!="CA")&(state3$state!="MA"),"state"] <- "Complements"
state3[state3$state=="CA","state"] <- "California"
state3[state3$state=="MA","state"] <- "Massachusetts"
state3

statebox <- ggplot(data=state3, aes(x=state, y=rndratio, group=state)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="State", y="R&D ratio")

statebox1 <- ggplot(data=state3, aes(x=state, y=rndratio, group=state)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="State", y="R&D ratio")

var.test(rndratio~state, state3[state3$state!="Complements",])
t.test(rndratio~state, state3[state3$state!="Complements",], var.equal=T)

var.test(rndratio~state, state3[state3$state!="Massachusetts",])
t.test(rndratio~state, state3[state3$state!="Massachusetts",])

var.test(rndratio~state, state3[state3$state!="California",])
t.test(rndratio~state, state3[state3$state!="California",])

state3[(state3$state=="California")|(state3$state=="Massachusetts"),"state"] <- "California and Massachusetts"
statebox2 <- ggplot(data=state3, aes(x=state, y=rndratio, group=state)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="State", y="R&D ratio")

var.test(rndratio~state, state3)
t.test(rndratio~state, state3)

multiplot(statebox1, statebox2, cols=2)

# divpay, insiderceo, femaleceo
boxplot <- rndceo2017[,c("divpay","insiderceo","femaleceo","rndratio")]
boxplot <- boxplot[boxplot$rndratio<0.8,]
boxplot[boxplot$divpay==1,"divpay"] <- "Paid"
boxplot[boxplot$divpay==0,"divpay"] <- "Not paid"
boxplot[boxplot$insiderceo==1,"insiderceo"] <- "Insider"
boxplot[boxplot$insiderceo==0,"insiderceo"] <- "Outsider"
boxplot[boxplot$femaleceo==1,"femaleceo"] <- "Female"
boxplot[boxplot$femaleceo==0,"femaleceo"] <- "Male"

b1 <- ggplot(data=boxplot, aes(x=femaleceo, y=rndratio, group=femaleceo)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="Female CEO", y="R&D ratio")

b2 <- ggplot(data=boxplot, aes(x=insiderceo, y=rndratio, group=insiderceo)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="Insider CEO", y="R&D ratio")

b3 <- ggplot(data=boxplot, aes(x=divpay, y=rndratio, group=divpay)) +
  geom_boxplot(fill='orange',color='darkslategrey',width=0.8) +
  stat_summary(fun="mean", geom="point", shape=22, size=4, fill="blue") +
  labs(x="Dividends paid", y="R&D ratio")

multiplot(b1, b2, b3, cols=3)

var.test(rndratio~femaleceo, rndceo2017t)
t.test(rndratio~femaleceo, rndceo2017t, var.equal=T)

var.test(rndratio~insiderceo, rndceo2017t)
t.test(rndratio~insiderceo, rndceo2017)

var.test(rndratio~divpay, rndceo2017t)
t.test(rndratio~femaleceo, rndceo2017)

# Pie chart
piefemale <- aggregate(rndratio~femaleceo, boxplot, length)
pieinsider <- aggregate(rndratio~insiderceo, boxplot, length)
piedivpay <- aggregate(rndratio~divpay, boxplot, length)
scale_fill_manual(values = mycols)

pc1 <- ggplot(piefemale, aes(x="", y=rndratio, fill=femaleceo)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("darkslategrey","orange")) +
  coord_polar("y", start=0) +
  labs(title = "Female CEO") +
  theme_void() # remove background, grid, numeric labels

pc2 <- ggplot(pieinsider, aes(x="", y=rndratio, fill=insiderceo)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("darkslategrey","orange")) +
  coord_polar("y", start=0) +
  labs(title = "Insider CEO") +
  theme_void() # remove background, grid, numeric labels

pc3 <- ggplot(piedivpay, aes(x="", y=rndratio, fill=divpay)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values = c("darkslategrey","orange")) +
  coord_polar("y", start=0) +
  labs(title = "Dividends paid") +
  theme_void() # remove background, grid, numeric labels
multiplot(pc1, pc2, pc3, cols=3)

ch <- rndceo2017
ch[ch$rndratio==0,"rndratio"] <- "0"
ch[ch$rndratio>0,"rndratio"] <- "not 0"

var.test(size~rndratio, ch)
t.test(size~rndratio, ch)

var.test(bm~rndratio, ch)
t.test(bm~rndratio, ch)

var.test(fcf~rndratio, ch)
t.test(fcf~rndratio, ch)

var.test(hhi~rndratio, ch)
t.test(hhi~rndratio, ch)

var.test(ceotenure~rndratio, ch)
t.test(ceotenure~rndratio, ch, var.equal=T)


#---------------------------------------
# Between independent variables  
cor.test(rndceo2017$ceotenure, rndceo2017$ceoage, method="spearman")

conti <- rndceo2017[,c("size","bm","fcf","hhi","opperf","leverage","tobinsq","roa","salesgrowth","cashratio",
                       "intan","invest","ceoage","ceocomp","ceopayslice","ceoequity","ceotenure","rndratio")]
conti <- conti[(conti$leverage>-Inf)&(conti$cashratio>-Inf)&(conti$invest>-Inf)&(conti$ceoequity>-Inf),]
cor_51 <- rcorr(as.matrix(conti), type = "spearman")
M1 <- cor_51$r
p_mat1 <- cor_51$P
par(mfrow=c(1,1))
corrplot(M1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("darkslategrey","white","orange"))(200),
         p.mat = p_mat1, sig.level = 0.05, diag = FALSE)
corrplot(M1, method = "color", col = colorRampPalette(c("darkslategrey","white","orange"))(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat1, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


conti2 <- rndceo2017[,c("size","bm","fcf","hhi","opperf","leverage","tobinsq","salesgrowth","cashratio",
                       "intan","invest","ceoage")]
conti2 <- conti2[(conti2$leverage>-Inf)&(conti2$cashratio>-Inf)&(conti2$invest>-Inf),]

cor_52 <- rcorr(as.matrix(conti2), type = "spearman")
M2 <- cor_52$r
p_mat2 <- cor_52$P
par(mfrow=c(1,2))
corrplot(M2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("darkslategrey","white","orange"))(200),
         p.mat = p_mat2, sig.level = 0.05, diag = FALSE)
corrplot(M2, method = "color", col = colorRampPalette(c("darkslategrey","white","orange"))(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat2, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


ii1 <- ggplot(rndceo2017t, aes(x=tobinsq, y=bm)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  labs(x="Tobin's Q", y="BM")

ii2 <- ggplot(rndceo2017t, aes(x=opperf, y=bm)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  labs(x="OPPERF", y="BM")

ii3 <- ggplot(rndceo2017t, aes(x=tobinsq, y=opperf)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  labs(x="Tobin's Q", y="OPPERF")

ii4 <- ggplot(rndceo2017t, aes(x=fcf, y=opperf)) + 
  geom_point(color='darkslategrey', cex=.3) +
  geom_smooth(method = 'lm', se=T, color='orange', cex=1) +
  labs(x="FCF", y="OPPERF")
multiplot(ii1, ii2, ii3, ii4, cols=2)

pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)


#----------------------------------------------------------------



# histogram

# Transformation
transf <- rndceo2017[,c("size","bm","leverage","tobinsq","salesgrowth","cashratio","invest","ceocomp","ceoequity","ceotenure", "rndratio")]

transf["expsize"] <- exp(transf$size)

transf["pmbm"] <- transf$bm/abs(transf$bm)
transf["sqrtbm"] <- sqrt(abs(transf$bm))*transf$pmbm
rndceo2017["bm"] <- transf$sqrtbm

transf["logleverage"] <- log(transf$leverage)
rndceo2017["leverage"] <- transf$logleverage

transf["logtobinsq"] <- log(transf$tobinsq)
rndceo2017["tobinsq"] <- transf$tobinsq

transf["pmsalesgrowth"] <- transf$salesgrowth/abs(transf$salesgrowth)
transf["sqrtsalesgrowth"] <- sqrt(abs(transf$salesgrowth))*transf$pmsalesgrowth
rndceo2017["salesgrowth"] <- transf$sqrtsalesgrowth

transf["logcashratio"] <- log(transf$cashratio)
rndceo2017["cashratio"] <- transf$logcashratio

transf["loginvest"] <- log(transf$invest)
rndceo2017["invest"] <- transf$loginvest

transf["logcomp"] <- log(transf$ceocomp)
rndceo2017["ceocomp"] <- transf$logcomp

transf["logequity"] <- log(transf$ceoequity)
rndceo2017["ceoequity"] <- transf$logequity

transf["logtenure"] <- log(transf$ceotenure)
rndceo2017["ceotenure"] <- transf$logtenure

transf["sqrtrndratio"] <- sqrt(transf$rndratio)
transf["logrndratio"] <- log(transf$rndratio)
transf["arcsinrndratio"] <- d(Arcsine())(transf$rndratio)


# non transformation
fcf hhi opperf roa intan ceoage ceopayslice
n1 <- ggplot(rndceo2017, aes(x=fcf, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.03, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="FCF", y="Density")
n2 <- ggplot(rndceo2017, aes(x=hhi, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.008, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="HHI", y="Density")
n3 <- ggplot(rndceo2017, aes(x=opperf, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.03, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Operating performance", y="Density")
n4 <- ggplot(rndceo2017, aes(x=roa, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.03, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="ROA", y="Density")
n5 <- ggplot(rndceo2017, aes(x=intan, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.01, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Intangibility", y="Density")
n6 <- ggplot(rndceo2017, aes(x=ceoage, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="CEO's age", y="Density")
n7 <- ggplot(rndceo2017, aes(x=ceopayslice, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.01, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="CEO's payslice", y="Density")
multiplot(n1, n2, n3, n4, n5, n6, n7, cols=3)

# Log transformation set A
size leverage tobinsq cashratio
la1 <- ggplot(transf, aes(x=expsize)) +
  geom_histogram(binwidth = 30000, fill = "orange", colour = "black", alpha=.7) +
  labs(x="Firm size: total assets", y="Count")
la2 <- ggplot(transf, aes(x=leverage, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.01, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Leverage", y="Density")
la3 <- ggplot(transf, aes(x=tobinsq, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Tobins' Q", y="Density")
la4 <- ggplot(transf, aes(x=cashratio, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.01, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Cash ratio", y="Density")
la5 <- ggplot(transf, aes(x=size, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (Firm size: total assets)", y="Density")
la6 <- ggplot(transf, aes(x=logleverage, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (Leverage)", y="Density")
la7 <- ggplot(transf, aes(x=logtobinsq, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.05, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (Tobins' Q)", y="Density")
la8 <- ggplot(transf, aes(x=logcashratio, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (Cash ratio)", y="Density")
multiplot(la1, la2, la3, la4, la5, la6, la7, la8, cols=2)

# Log transformation set B
lb1 <- ggplot(transf, aes(x=invest, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.5, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Investment ratio", y="Density")
lb2 <- ggplot(transf, aes(x=ceocomp, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=800, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="CEO's compensation", y="Density")
lb3 <- ggplot(transf, aes(x=ceoequity)) +
  geom_histogram(binwidth=0.01, fill = "orange", colour = "black", alpha=.7) +
  labs(x="CEO's equity", y="Count")
lb4 <- ggplot(transf, aes(x=ceotenure, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.4, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="CEO's tenure", y="Density")
lb5 <- ggplot(transf, aes(x=loginvest, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.05, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (Investment ratio)", y="Density")
lb6 <- ggplot(transf, aes(x=logcomp, y=..density..)) +
  #geom_histogram(binwidth=0.1, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (CEO's compensation)", y="Density")
lb7 <- ggplot(transf, aes(x=logequity, y=..density..)) +
  #geom_histogram(binwidth=0.05, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +# alpha 반투명
  geom_histogram(binwidth=0.08, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (CEO's equity)", y="Density")
lb8 <- ggplot(transf, aes(x=logtenure, y=..density..)) +
  #geom_histogram(binwidth=0.05, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +# alpha 반투명
  geom_histogram(binwidth=0.05, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (CEO's tenure)", y="Density")
multiplot(lb1, lb2, lb3, lb4, lb5, lb6, lb7, lb8, cols=2)

# Square root transforamtion
bm salesgrowth
s1 <- ggplot(transf, aes(x=bm)) +
  geom_histogram(binwidth = 2, fill = "orange", colour = "black", alpha=.7) +
  labs(x="BM", y="Count")
s2 <- ggplot(transf, aes(x=sqrtbm, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.15, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="sqrt (BM)", y="Density")
s3 <- ggplot(transf, aes(x=salesgrowth, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.08, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="Sales growth", y="Density")
s4 <- ggplot(transf, aes(x=sqrtsalesgrowth, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.03, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="sqrt (Sales growth)", y="Density")
multiplot(s1, s3, s2, s4, cols=2)

# R&D ratio transformation
r1 <- ggplot(transf, aes(x=rndratio, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.008, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="R&D ratio", y="Density")
r2 <- ggplot(transf, aes(x=sqrtrndratio, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.008, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="sqrt (R&D ratio)", y="Density")
r3 <- ggplot(transf, aes(x=logrndratio, y=..density..)) +
  #geom_histogram(binwidth=0.5, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=0.07, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="log (R&D ratio)", y="Density")
r4 <- ggplot(transf, aes(x=arcsinrndratio)) +
  geom_histogram(binwidth = 0.003, fill = "orange", colour = "black", alpha=.7) +
  labs(x="arcsin (R&D ratio)", y="Count")
multiplot(r1, r2, r3, r4, cols=2)

skewness(transf$salesgrowth)
skewness(transf$sqrtsalesgrowth)
kurtosis(transf$salesgrowth)
kurtosis(transf$sqrtsalesgrowth)

#--------------------------------------------------------------------


# Map of the world:
map('world', col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4), border=0, ylim=c(-80,80))

map('state', col='black', lwd=1, mar=rep(6,4))

# Map of Japan
map('japan', col="black", lwd=1, mar=rep(0,4))

# Map of Australia
par(mar=rep(0,4))
oz(states=TRUE, col="#69b3a2")



plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

plot_usmap("states")
plot_usmap("counties")
plot_usmap("states", include = .mountain, labels = TRUE)
plot_usmap("states", labels=TRUE, label_color="blue")

table(rndceo2017$state)

plot_usmap("counties",
           include = c("MA", "CT", "RI"),
           labels = TRUE, label_color = "blue")

vignette(package = "usmap")
vignette("introduction", package = "usmap")
vignette("mapping", package = "usmap")
vignette("advanced-mapping", package = "usmap")


fips(state = "NJ", county = "Bergen")
fips_info(c("30", "33", "34"))
fips_info(c("01001", "01003", "01005", "01007"))
fips_info("34003")

fips(state="SD")
meanbystate <- fips_info()


state_map <- us_map(regions = "states")
county_map <- us_map(regions = "counties")
str(state_map)
str(county_map)


statelength <- aggregate(rndratio~state, rndceo2017, length)
colnames(statelength) <- c("abbr", "number")
statelength <- merge(fips_info(), statelength, by="abbr", all.x=TRUE)

statemean <- aggregate(cbind(size, bm, fcf, hhi, opperf, leverage, tobinsq, roa, salesgrowth, cashratio,
                             intan, invest, ceoage, ceocomp, ceopayslice, ceoequity, ceotenure, rndratio)
                       ~state, rndceo2017, mean)
colnames(statemean)[1] <- "abbr"
statemean <- merge(fips_info(), statemean, by="abbr", all.x=TRUE)

dec <- subset(rndceo2017, gvkey!=29127)
levc <- rndceo2017[rndceo2017$leverage>-Inf,]
cashc <- rndceo2017[rndceo2017$cashratio>-Inf,]
invc <- rndceo2017[rndceo2017$invest>-Inf,]

decm <- aggregate(cbind(size, bm, fcf, hhi, opperf, leverage, tobinsq, roa, salesgrowth, cashratio,
                        intan, invest, ceoage, ceocomp, ceopayslice, ceoequity, ceotenure, rndratio)
                  ~state, dec, mean)
colnames(decm)[1] <- "abbr"
decm <- merge(fips_info(), decm, by="abbr", all.x=TRUE)

levcm <- aggregate(cbind(size, bm, fcf, hhi, opperf, leverage, tobinsq, roa, salesgrowth, cashratio,
                         intan, invest, ceoage, ceocomp, ceopayslice, ceoequity, ceotenure, rndratio)
                   ~state, levc, mean)
colnames(levcm)[1] <- "abbr"
levcm <- merge(fips_info(), levcm, by="abbr", all.x=TRUE)

cashcm <- aggregate(cbind(size, bm, fcf, hhi, opperf, leverage, tobinsq, roa, salesgrowth, cashratio,
                          intan, invest, ceoage, ceocomp, ceopayslice, ceoequity, ceotenure, rndratio)
                    ~state, cashc, mean)
colnames(cashcm)[1] <- "abbr"
cashcm <- merge(fips_info(), cashcm, by="abbr", all.x=TRUE)

invcm <- aggregate(cbind(size, bm, fcf, hhi, opperf, leverage, tobinsq, roa, salesgrowth, cashratio,
                         intan, invest, ceoage, ceocomp, ceopayslice, ceoequity, ceotenure, rndratio)
                   ~state, invc, mean)
colnames(invcm)[1] <- "abbr"
invcm <- merge(fips_info(), invcm, by="abbr", all.x=TRUE)

# Number of firms
plot_usmap(data=statelength, values="number", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Count")

# RNDRATIO: A firm’s R&D expenditure (divided by total assets)
plot_usmap(data=decm, values="rndratio", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "R&D ratio", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "R&D ratio")

# SIZE: Firm size (log total assets)
us1 <- plot_usmap(data=statemean, values="size", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "size", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Size")

# BM: Book-to-market (book value of equity divided by market value of equity)
us2 <- plot_usmap(data=statemean, values="bm", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "BM", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "BM")

# Free Cash Flow
us3 <- plot_usmap(data=statemean, values="fcf", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "FCF", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "FCF")

# HHI (Herfindhal-Hirschman Index)
us4 <- plot_usmap(data=statemean, values="hhi", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "HHI", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "HHI")

# OPPERF: Operating performance (divided by total assets)
us5 <- plot_usmap(data=statemean, values="opperf", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "opperf", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Operating performance")

# LEVERAGE: Market value of a firm’s leverage (divided by total mark-to-market assets)
us6 <- plot_usmap(data=levcm, values="leverage", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "leverage", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Leverage")

# Tobin's Q
us7 <- plot_usmap(data=statemean, values="tobinsq", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "Tobin's Q", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Tobin's Q")

# ROA
plot_usmap(data=statemean, values="roa", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "ROA", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "ROA")

# Sales Growth
us8 <- plot_usmap(data=statemean, values="salesgrowth", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "sales growth", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Sales growth")

# CASHRATIO: A firm’s cash holdings (divided by total assets)
us9 <- plot_usmap(data=cashcm, values="cashratio", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "cash ration", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Cash ratio")

# INTAN: Intangibility measure. The higher this number, the more intangible a firm’s assets ((total assets – property, plants, and equipments value)/total assets)
us10 <- plot_usmap(data=statemean, values="intan", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "intan", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Intangibility")

# INVEST: A firm’s investment ratio (investment / property, plants, and equipments value)
us11 <- plot_usmap(data=invcm, values="invest", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "ivestment", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Investment")

# CEOAGE: CEO’s age
us12 <- plot_usmap(data=statemean, values="ceoage", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "CEO age", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "CEO age")

# CEOCOMP: CEO’s total compensation, in $1000s
plot_usmap(data=statemean, values="ceocomp", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "CEO comp", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "CEO compensation")

# CEOPAYSLICE: Bebchuk, Cremers, and Peyer (2011) argue this is a good measure of how powerful a CEO is within the company. It is the CEO’s compensation divided by the firm’s top 5- earning directors’ total compensation (the higher this number, the more disproportionately highly a CEO is compensated within the top management team)
plot_usmap(data=statemean, values="ceopayslice", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "CEO payslice", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "CEO payslice")

# CEOEQUITY: CEO’s holdings of the firm’s stock
plot_usmap(data=statemean, values="ceoequity", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "CEO equity", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "CEO equity")

# CEOTENURE: CEO’s tenure in his or her current position, in years
plot_usmap(data=statemean, values="ceotenure", color="orange", labels=TRUE) + 
  scale_fill_continuous(low = "white", high="orange", name = "CEO tenure", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=20, hjust = 0.5), legend.title = element_text(size=10)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "CEO tenure")

multiplot(us1, us2, us3, us4, us5, us6, us7, us8, us9, us10, us11, us12, cols=3)




