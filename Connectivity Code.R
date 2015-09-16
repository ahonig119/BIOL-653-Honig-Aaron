SalmonOrigin <- read.table("~/Desktop/Desktop 11:17:14/STAT607 Files/SalmonOrigin.txt", header=TRUE, quote="\"")
salmon <- SalmonOrigin
strain=salmon[c(1:40,51:90),]
stest=salmon[c(41:50,91:100),]
plot(salmon[,-1],col=as.factor(salmon[,1]))
lsol=lda(strain[,c(2,3)],grouping=strain[,1])
lsol=lda(strain[,c(2,3)],grouping=strain[,1])
lsol$prior
lsol$means
alaskasalmon=salmon[c(1:40),c(2,3)]
canadasalmon=salmon[c(51:90),c(2,3)]
singlecov=(39/78)*(cov(alaskasalmon)+cov(canadasalmon))
lsol
predict(lsol,c(120,380))
lsolcv=lda(salmon[,c(2,3)],grouping=salmon[,1],CV=TRUE)
lsolcv
plot(salmon[,c(2,3)],col=as.factor(salmon[,1]),pch=as.numeric(lsolcv$class))
qsol=qda(strain[,c(2,3)],grouping=strain[,1])
predict(qsol,stest[,c(2,3)])
alaskasalmon=salmon[c(1:40),c(2,3)]
cov(alaskasalmon)
canadasalmon=salmon[c(51:90),c(2,3)]
cov(canadasalmon)
plot(salmon[,c(2,3)],col=as.factor(salmon[,1]),xlim=c(50,190) + ylim=c(290,530))
plot(salmon[,c(2,3)],col=as.factor(salmon[,1]),xlim=c(50,190), ylim=c(290,530))
-----------------------------------------
PB_Juv_Melt <- PB_Juv_Data[,4:27]
ggplot(melt(PB_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Juv_Melt <- PB_Juv_Edit
ggplot(melt(PB_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Larv_Melt <- PB_Larv_Data
ggplot(melt(PB_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Larv_Melt2 <- melt(PB_Larv_Melt)


PB_Juv_Means <- ddply(PB_Juv_Melt2, c("variable"), summarise,  N    = sum(!is.na(value)), mean = mean(log(value), na.rm=TRUE), sd = sd(value, na.rm=TRUE), se = sd / sqrt(N))                                                                                                                                              
ggplot(data=PB_Juv_Means, aes(x=variable, y=mean)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + ylim(-500,1000)
ggplot(data=PB_Juv_Edit, aes(x=ID, y=)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + ylim(-500,1000)

PB_Juv_Melt <- melt(PB_Juv_Edit)                                                                                                                                             
PB_Juv_Means <- aggregate(log(value)~variable, data = PB_Juv_Melt, mean)
View(PB_Juv_Melt2)
View(PB_Juv_Means) 

-----------------------------------------------------
Mg24  Mg26	Si28 %rel	Ca43	Ca44	Ca46	Mn55	Co59	Cu63	Cu65	Zn64	Zn66	Sr86	Sr88	Ba137	Ba138	La139	Pb206	Pb207	Pb208	Cr52	Ba136	Fe57	Ca48

PB_Juv_Mg24 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=PB_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Mg26 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Mg26'),]
ggplot(data=PB_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Si28..rel <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=PB_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ca43 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ca43'),]
ggplot(data=PB_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ca44 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ca44'),]
ggplot(data=PB_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ca46 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ca46'),]
ggplot(data=PB_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Mn55 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=PB_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Co59 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=PB_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Cu63 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=PB_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Cu65 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Cu65'),]
ggplot(data=PB_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Zn64 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=PB_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Zn66 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Zn66'),]
ggplot(data=PB_Juv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Sr86 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Sr86'),]
ggplot(data=PB_Juv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Sr88 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=PB_Juv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ba137 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ba137'),]
ggplot(data=PB_Juv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ba138 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=PB_Juv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_La139 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='La.Ca'),]
ggplot(data=PB_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Pb206 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Pb206'),]
ggplot(data=PB_Juv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Pb207 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Pb207'),]
ggplot(data=PB_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Pb208 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=PB_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Cr52 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Cr.Ca'),]
ggplot(data=PB_Juv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ba136 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ba136'),]
ggplot(data=PB_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Fe57 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=PB_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Juv_Ca48 <- PB_Juv_Melt[which(PB_Juv_Melt$variable=='Ca48'),]
ggplot(data=PB_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
-----------------------------------------------------
PB_Larv_Mg24 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=PB_Larv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Mg26 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Mg26'),]
ggplot(data=PB_Larv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Si28..rel <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Si28..rel'),]
ggplot(data=PB_Larv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ca43 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ca43'),]
ggplot(data=PB_Larv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ca44 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ca44'),]
ggplot(data=PB_Larv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ca46 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ca46'),]
ggplot(data=PB_Larv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Mn55 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Mn.Ca'),]
ggplot(data=PB_Larv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Co59 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Co.Ca'),]
ggplot(data=PB_Larv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + ylim(0, 10000)

PB_Larv_Cu63 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Cu.Ca'),]
ggplot(data=PB_Larv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Cu65 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Cu65'),]
ggplot(data=PB_Larv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Zn64 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Zn.Ca'),]
ggplot(data=PB_Larv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Zn66 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Zn66'),]
ggplot(data=PB_Larv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Sr86 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Sr86'),]
ggplot(data=PB_Larv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Sr88 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Sr.Ca'),]
ggplot(data=PB_Larv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ba137 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ba137'),]
ggplot(data=PB_Larv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ba138 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ba.Ca'),]
ggplot(data=PB_Larv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_La139 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='La.Ca'),]
ggplot(data=PB_Larv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Pb206 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Pb206'),]
ggplot(data=PB_Larv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Pb207 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Pb207'),]
ggplot(data=PB_Larv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Pb208 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Pb.Ca'),]
ggplot(data=PB_Larv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Cr52 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Cr.Ca'),]
ggplot(data=PB_Larv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ba136 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ba136'),]
ggplot(data=PB_Larv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Fe57 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Fe.Ca'),]
ggplot(data=PB_Larv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PB_Larv_Ca48 <- PB_Larv_Melt[which(PB_Larv_Melt$variable=='Ca48'),]
ggplot(data=PB_Larv_Ca48, aes(x=ID, y=log(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  







"Mg24"  "Mg26"	"Si28" "%rel"	"Ca43"	"Ca44"	"Ca46"	"Mn55"	"Co59"	"Cu63"	"Cu65"	"Zn64"	"Zn66"	"Sr86"	"Sr88"	"Ba137"	"Ba138"	"La139"	"Pb206"	"Pb207"	"Pb208"	"Cr52"	"Ba136"	"Fe57"	"Ca48"

View(PB_Larv_Melt2)

PB_Juv_Melt2 <- melt(PB_Juv_Melt)

PB_Larv_Median <- aggregate(PB_Larv_Melt2, by = list(PB_Larv_Melt2$variable), FUN = median, na.rm=TRUE)
PB_Juv_Median <- aggregate(PB_Juv_Melt2, by = list(PB_Juv_Melt2$variable), FUN = median, na.rm=TRUE)

View(PB_Juv_Median)
PB_All_Median <- cbind(PB_Larv_Median, PB_Juv_Median[,3])
View(PB_All_Median)

PB_All_Median[,5] <- log(PB_All_Median[,3])/log(PB_All_Median[,4])

ggplot(PB_All_Median, aes(x = Group.1, y = PB_All_Median[,5])) + geom_bar(stat = "identity") + ylim(-5,10)
------       -----         ----
PB_Juv_Melt2 <- PB_Juv_Edit[,4:27]
ggplot(melt(PB_Juv_Melt2), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Larv_Melt2 <- PB_Larv_Edit[,4:27]
ggplot(melt(PB_Larv_Melt2), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Larv_Melt2 <- melt(PB_Larv_Melt2)
View(PB_Larv_Melt2)

PB_Juv_Melt2 <- melt(PB_Juv_Melt2)

PB_Larv_Median <- aggregate(PB_Larv_Melt2, by = list(PB_Larv_Melt2$variable), FUN = median, na.rm=TRUE)
PB_Juv_Median <- aggregate(PB_Juv_Melt2, by = list(PB_Juv_Melt2$variable), FUN = median, na.rm=TRUE)

View(PB_Juv_Median)
PB_All_Median <- cbind(PB_Larv_Median, PB_Juv_Median[,3])
View(PB_All_Median)

PB_All_Median[,5] <- log(PB_All_Median[,3])/log(PB_All_Median[,4])

ggplot(PB_All_Median, aes(x = Group.1, y = PB_All_Median[,5])) + geom_bar(stat = "identity") + ylim(-5,10)

PB_Larv_Data2 <- PB_Larv_Edit[,1:27]
All_PB_Edit <- rbind(PB_Juv_Edit, PB_Larv_Data2)

PB_All_Melt <- melt(All_PB_Edit)
View(PB_All_Melt)

ggplot(PB_All_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)


PB_WRL_Juv_LDA <- rbind(PB_Juv_NoNA, WRL_Juv_NoNA)

PB_WRL_Juv_log <- log(PB_WRL_Juv_LDA[,4:22])
PB_WRL_Juv_log <- cbind(PB_WRL_Juv_LDA[,1:3], PB_WRL_Juv_log)

PB_WRL_train <- PB_WRL_Juv_log[c(1:21, 27:47),]
PB_WRL_test <- PB_WRL_Juv_log[c(22:26, 48:52),]
PB_WRL_Melt <- melt(PB_WRL_Juv_LDA)
ggplot(PB_WRL_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Site)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)
PB_WRL_LDA <- lda(PB_WRL_train[,c(4:22)], grouping=PB_WRL_train[,3])

strain=salmon[c(1:40,51:90),]
stest=salmon[c(41:50,91:100),]
plot(salmon[,-1],col=as.factor(salmon[,1]))

lsol=lda(strain[,c(2,3)],grouping=strain[,1])
lsol=lda(strain[,c(2,3)],grouping=strain[,1])
lsol$prior
lsol$means
alaskasalmon=salmon[c(1:40),c(2,3)]
canadasalmon=salmon[c(51:90),c(2,3)]
singlecov=(39/78)*(cov(alaskasalmon)+cov(canadasalmon))
lsol
predict(lsol,c(120,380))
lsolcv=lda(salmon[,c(2,3)],grouping=salmon[,1],CV=TRUE)
lsolcv














------------------------------------  
WRL_Larv_Melt2 <- melt(WRL_Larv_Melt)
WRL_Juv_Melt2 <- melt(WRL_Juv_Melt)
    
WRL_Larv_Median <- aggregate(WRL_Larv_Melt2, by = list(WRL_Larv_Melt2$variable), FUN = mean, na.rm=TRUE)
WRL_Juv_Median <- aggregate(WRL_Juv_Melt2, by = list(WRL_Juv_Melt2$variable), FUN = mean, na.rm=TRUE)

View(WRL_Juv_Median)
WRL_All_Median <- cbind(WRL_Larv_Median, WRL_Juv_Median[,3])
View(WRL_All_Median)

WRL_All_Median[,5] <- log(WRL_All_Median[,3])/log(WRL_All_Median[,4])

ggplot(WRL_All_Median, aes(x = Group.1, y = WRL_All_Median[,5])) + geom_bar(stat = "identity") + ylim(-5,10)
-------------------------------------
PB_Larv_Data2 <- PB_Larv_Data[,1:27]
All_PB_Data <- rbind(PB_Juv_Data, PB_Larv_Data2)

PB_All_Melt <- melt(All_PB_Data)
View(PB_All_Melt)
                 
ggplot(PB_All_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

PB_Aggregate <-aggregate(PB_All_Melt, by=list(variable, Age), FUN=mean, na.rm=TRUE)
PB_Aggregate <- aggregate(value ~ variable + Age, data = PB_All_Melt, mean)
View(PB_Aggregate)
-----------------------------------------
WRL_Juv_Melt <- WRL_Juv_Data[,4:27]
ggplot(melt(WRL_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

WRL_Larv_Melt <- WRL_Larv_Data[,4:27]
ggplot(melt(WRL_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

WRL_Larv_Data2 <- WRL_Larv_Data[,1:27]
All_WRL_Data <- rbind(WRL_Juv_Data, WRL_Larv_Data2)

WRL_All_Melt <- melt(All_WRL_Data)
View(WRL_All_Melt)

ggplot(WRL_All_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)
--------------------------------------------
WRL_Juv_Melt2 <- WRL_Juv_Edit[,4:27]
ggplot(melt(WRL_Juv_Melt2), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

WRL_Larv_Melt2 <- WRL_Larv_Edit[,4:27]
ggplot(melt(WRL_Larv_Melt2), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

WRL_Larv_Edit2 <- WRL_Larv_Edit[,1:27]
All_WRL_Data2 <- rbind(WRL_Juv_Edit, WRL_Larv_Edit2)

WRL_All_Melt2 <- melt(All_WRL_Data2)
View(WRL_All_Melt2)

ggplot(WRL_All_Melt2, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)


  
  
  
  
  
  
  
  
  
  







Juv_Larv_Ratio <- function(x,y){
  
          log(value) / log(value)
  
}
  
PB_Aggregate_Ratio <-aggregate(PB_All_Melt, by=list(variable), FUN=log(value), na.rm=TRUE)

PB_Median <-aggregate(PB_All_Melt, by=variable, FUN=median, na.rm=TRUE)


  
  
  
  
  
  
  
--------------------------------------------
MB_Juv_Melt <- MB_Juv_Data[,4:27]
ggplot(melt(MB_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

MB_Larv_Melt <- MB_Larv_Data[,4:27]
ggplot(melt(MB_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

MB_Larv_Data2 <- MB_Larv_Data[,1:27]
All_MB_Data <- rbind(MB_Juv_Data, MB_Larv_Data2)

MB_All_Melt <- melt(All_MB_Data)
View(MB_All_Melt)

ggplot(MB_All_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)
-------------------------------------------------------
FB_Juv_Melt <- FB_Juv_Data[,4:27]
ggplot(melt(FB_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

FB_Larv_Melt <- FB_Larv_Data[,4:27]
ggplot(melt(FB_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

FB_Larv_Data2 <- FB_Larv_Data[,1:27]
All_FB_Data <- rbind(FB_Juv_Data, FB_Larv_Data2)

FB_All_Melt <- melt(All_FB_Data)
View(FB_All_Melt)

ggplot(FB_All_Melt, aes(variable, log(value))) + geom_boxplot(aes(fill = Age)) + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)
---------------------------------------------------------

  









WRL_Juv_Melt <- WRL_Juv_Data[,4:27]
ggplot(melt(WRL_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

WRL_Larv_Melt <- WRL_Larv_Data[,4:27]
ggplot(melt(WRL_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

MB_Juv_Melt <- MB_Juv_Data[,4:27]
ggplot(melt(MB_Juv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)

MB_Larv_Melt <- MB_Larv_Data[,4:27]
ggplot(melt(MB_Larv_Melt), aes(variable, log(value))) + geom_boxplot() + ylim(-10,30) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + geom_hline(yintercept = 0, lty = 2)


PB_Larv_Mg24 <- PB_Larv_Data[,1:4]



findOutlier <- function(data, cutoff=3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm=TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) { 
    which(d > cutoff * s)
  },
  data, sds
  )
  result
}

 findOutlier(PB_Larv_Mg24)

PB_Larv_Mg24_NoNA <- na.omit(PB_Larv_Mg24)
IQR(PB_Larv_Mg24_NoNA$value)
quantile(PB_Larv_Mg24_NoNA$value)
PB_Larv_UpperOutliers_Mg24 <- PB_Larv_Mg24[which(PB_Larv_Mg24$value > 172178.422),]
PB_Larv_LowerOutliers_Mg24 <- PB_Larv_Mg24[which(PB_Larv_Mg24$value < 1759.193),]
PB_Larv_Outliers_Mg24 <- rbind(PB_Larv_UpperOutliers_Mg24, PB_Larv_LowerOutliers_Mg24)
View(PB_Larv_Outliers_Mg24)

PB_Larv_Mg26_NoNA <- na.omit(PB_Larv_Mg26)
IQR(PB_Larv_Mg26_NoNA$value)
quantile(PB_Larv_Mg26_NoNA$value)
PB_Larv_UpperOutliers_Mg26 <- PB_Larv_Mg26[which(PB_Larv_Mg26$value > 173333.35),]
PB_Larv_LowerOutliers_Mg26 <- PB_Larv_Mg26[which(PB_Larv_Mg26$value < 16669.28),]
PB_Larv_Outliers_Mg26 <- rbind(PB_Larv_UpperOutliers_Mg26, PB_Larv_LowerOutliers_Mg26)
View(PB_Larv_Outliers_Mg26)

PB_Larv_Mn55_NoNA <- na.omit(PB_Larv_Mn55)
IQR(PB_Larv_Mn55_NoNA$value)
quantile(PB_Larv_Mn55_NoNA$value)
PB_Larv_UpperOutliers_Mn55 <- PB_Larv_Mn55[which(PB_Larv_Mn55$value > 112864.18),]
PB_Larv_LowerOutliers_Mn55 <- PB_Larv_Mn55[which(PB_Larv_Mn55$value < 6504.67),]
PB_Larv_Outliers_Mn55 <- rbind(PB_Larv_UpperOutliers_Mn55, PB_Larv_LowerOutliers_Mn55)
View(PB_Larv_Outliers_Mn55)

PB_Larv_Co59_NoNA <- na.omit(PB_Larv_Co59)
IQR(PB_Larv_Co59_NoNA$value)
quantile(PB_Larv_Co59_NoNA$value)
PB_Larv_UpperOutliers_Co59 <- PB_Larv_Co59[which(PB_Larv_Co59$value > 640),]
PB_Larv_LowerOutliers_Co59 <- PB_Larv_Co59[which(PB_Larv_Co59$value < 15),]
PB_Larv_Outliers_Co59 <- rbind(PB_Larv_UpperOutliers_Co59, PB_Larv_LowerOutliers_Co59)
View(PB_Larv_Outliers_Co59)

PB_Larv_Cu63_NoNA <- na.omit(PB_Larv_Cu63)
IQR(PB_Larv_Cu63_NoNA$value)
quantile(PB_Larv_Cu63_NoNA$value)
PB_Larv_UpperOutliers_Cu63 <- PB_Larv_Cu63[which(PB_Larv_Cu63$value > 9714),]
PB_Larv_LowerOutliers_Cu63 <- PB_Larv_Cu63[which(PB_Larv_Cu63$value < 186),]
PB_Larv_Outliers_Cu63 <- rbind(PB_Larv_UpperOutliers_Cu63, PB_Larv_LowerOutliers_Cu63)
View(PB_Larv_Outliers_Cu63)

PB_Larv_Cu65_NoNA <- na.omit(PB_Larv_Cu65)
IQR(PB_Larv_Cu65_NoNA$value)
quantile(PB_Larv_Cu65_NoNA$value)
PB_Larv_UpperOutliers_Cu65 <- PB_Larv_Cu65[which(PB_Larv_Cu65$value > 32100),]
PB_Larv_LowerOutliers_Cu65 <- PB_Larv_Cu65[which(PB_Larv_Cu65$value < 1309),]
PB_Larv_Outliers_Cu65 <- rbind(PB_Larv_UpperOutliers_Cu65, PB_Larv_LowerOutliers_Cu65)
View(PB_Larv_Outliers_Cu65)

PB_Larv_Zn64_NoNA <- na.omit(PB_Larv_Zn64)
IQR(PB_Larv_Zn64_NoNA$value)
quantile(PB_Larv_Zn64_NoNA$value)
PB_Larv_UpperOutliers_Zn64 <- PB_Larv_Zn64[which(PB_Larv_Zn64$value > 23515),]
PB_Larv_LowerOutliers_Zn64 <- PB_Larv_Zn64[which(PB_Larv_Zn64$value < 695),]
PB_Larv_Outliers_Zn64 <- rbind(PB_Larv_UpperOutliers_Zn64, PB_Larv_LowerOutliers_Zn64)
View(PB_Larv_Outliers_Zn64)

PB_Larv_Zn66_NoNA <- na.omit(PB_Larv_Zn66)
IQR(PB_Larv_Zn66_NoNA$value)
quantile(PB_Larv_Zn66_NoNA$value)
PB_Larv_UpperOutliers_Zn66 <- PB_Larv_Zn66[which(PB_Larv_Zn66$value > 28063),]
PB_Larv_LowerOutliers_Zn66 <- PB_Larv_Zn66[which(PB_Larv_Zn66$value < 479),]
PB_Larv_Outliers_Zn66 <- rbind(PB_Larv_UpperOutliers_Zn66, PB_Larv_LowerOutliers_Zn66)
View(PB_Larv_Outliers_Zn66)

PB_Larv_Sr88_NoNA <- na.omit(PB_Larv_Sr88)
IQR(PB_Larv_Sr88_NoNA$value)
quantile(PB_Larv_Sr88_NoNA$value)
PB_Larv_UpperOutliers_Sr88 <- PB_Larv_Sr88[which(PB_Larv_Sr88$value > 3652),]
PB_Larv_LowerOutliers_Sr88 <- PB_Larv_Sr88[which(PB_Larv_Sr88$value < 314),]
PB_Larv_Outliers_Sr88 <- rbind(PB_Larv_UpperOutliers_Sr88, PB_Larv_LowerOutliers_Sr88)
View(PB_Larv_Outliers_Sr88)

PB_Larv_Ba137_NoNA <- na.omit(PB_Larv_Ba137)
IQR(PB_Larv_Ba137_NoNA$value)
quantile(PB_Larv_Ba137_NoNA$value)
PB_Larv_LowerOutliers_Ba137 <- PB_Larv_Ba137[which(PB_Larv_Ba137$value > 1647.17),]
PB_Larv_UpperOutliers_Ba137 <- PB_Larv_Ba137[which(PB_Larv_Ba137$value < 107.37),]
PB_Larv_Outliers_Ba137 <- rbind(PB_Larv_UpperOutliers_Ba137, PB_Larv_LowerOutliers_Ba137)
View(PB_Larv_Outliers_Ba137)

PB_Larv_Ba138_NoNA <- na.omit(PB_Larv_Ba138)
IQR(PB_Larv_Ba138_NoNA$value)
quantile(PB_Larv_Ba138_NoNA$value)
PB_Larv_LowerOutliers_Ba138 <- PB_Larv_Ba138[which(PB_Larv_Ba138$value < 29.3),]
PB_Larv_UpperOutliers_Ba138 <- PB_Larv_Ba138[which(PB_Larv_Ba138$value > 979.4),]
PB_Larv_Outliers_Ba138 <- rbind(PB_Larv_UpperOutliers_Ba138, PB_Larv_LowerOutliers_Ba138)
View(PB_Larv_Outliers_Ba138)

PB_Larv_La139_NoNA <- na.omit(PB_Larv_La139)
IQR(PB_Larv_La139_NoNA$value)
quantile(PB_Larv_La139_NoNA$value)
PB_Larv_LowerOutliers_La139 <- PB_Larv_La139[which(PB_Larv_La139$value < 19.73),]
PB_Larv_UpperOutliers_La139 <- PB_Larv_La139[which(PB_Larv_La139$value > 402.7),]
PB_Larv_Outliers_La139 <- rbind(PB_Larv_UpperOutliers_La139, PB_Larv_LowerOutliers_La139)
View(PB_Larv_Outliers_La139)

PB_Larv_Pb206_NoNA <- na.omit(PB_Larv_Pb206)
IQR(PB_Larv_Pb206_NoNA$value)
quantile(PB_Larv_Pb206_NoNA$value)
PB_Larv_LowerOutliers_Pb206 <- PB_Larv_Pb206[which(PB_Larv_Pb206$value < 2.97),]
PB_Larv_UpperOutliers_Pb206 <- PB_Larv_Pb206[which(PB_Larv_Pb206$value > 220.2),]
PB_Larv_Outliers_Pb206 <- rbind(PB_Larv_UpperOutliers_Pb206, PB_Larv_LowerOutliers_Pb206)
View(PB_Larv_Outliers_Pb206)

PB_Larv_Pb207_NoNA <- na.omit(PB_Larv_Pb207)
IQR(PB_Larv_Pb207_NoNA$value)
quantile(PB_Larv_Pb207_NoNA$value)
PB_Larv_LowerOutliers_Pb207 <- PB_Larv_Pb207[which(PB_Larv_Pb207$value < 2.97),]
PB_Larv_UpperOutliers_Pb207 <- PB_Larv_Pb207[which(PB_Larv_Pb207$value > 220.2),]
PB_Larv_Outliers_Pb207 <- rbind(PB_Larv_UpperOutliers_Pb207, PB_Larv_LowerOutliers_Pb207)
View(PB_Larv_Outliers_Pb207)

PB_Larv_Pb208_NoNA <- na.omit(PB_Larv_Pb208)
IQR(PB_Larv_Pb208_NoNA$value)
quantile(PB_Larv_Pb208_NoNA$value)
PB_Larv_LowerOutliers_Pb208 <- PB_Larv_Pb208[which(PB_Larv_Pb208$value < 10.53),]
PB_Larv_UpperOutliers_Pb208 <- PB_Larv_Pb208[which(PB_Larv_Pb208$value > 238.74),]
PB_Larv_Outliers_Pb208 <- rbind(PB_Larv_UpperOutliers_Pb208, PB_Larv_LowerOutliers_Pb208)
View(PB_Larv_Outliers_Pb208)

PB_Larv_Cr52_NoNA <- na.omit(PB_Larv_Cr52)
IQR(PB_Larv_Cr52_NoNA$value)
quantile(PB_Larv_Cr52_NoNA$value)
PB_Larv_LowerOutliers_Cr52 <- PB_Larv_Cr52[which(PB_Larv_Cr52$value < 97.58),]
PB_Larv_UpperOutliers_Cr52 <- PB_Larv_Cr52[which(PB_Larv_Cr52$value > 1190.6),]
PB_Larv_Outliers_Cr52 <- rbind(PB_Larv_UpperOutliers_Cr52, PB_Larv_LowerOutliers_Cr52)
View(PB_Larv_Outliers_Cr52)

PB_Larv_Ba136_NoNA <- na.omit(PB_Larv_Ba136)
IQR(PB_Larv_Ba136_NoNA$value)
quantile(PB_Larv_Ba136_NoNA$value)
PB_Larv_LowerOutliers_Ba136 <- PB_Larv_Ba136[which(PB_Larv_Ba136$value < 88.69),]
PB_Larv_UpperOutliers_Ba136 <- PB_Larv_Ba136[which(PB_Larv_Ba136$value > 30656),]
PB_Larv_Outliers_Ba136 <- rbind(PB_Larv_UpperOutliers_Ba136, PB_Larv_LowerOutliers_Ba136)
View(PB_Larv_Outliers_Ba136)

PB_Larv_Ba136_NoNA <- na.omit(PB_Larv_Ba136)
IQR(PB_Larv_Ba136_NoNA$value)
quantile(PB_Larv_Ba136_NoNA$value)
PB_Larv_LowerOutliers_Ba136 <- PB_Larv_Ba136[which(PB_Larv_Ba136$value < 88.69),]
PB_Larv_UpperOutliers_Ba136 <- PB_Larv_Ba136[which(PB_Larv_Ba136$value > 30656),]
PB_Larv_Outliers_Ba136 <- rbind(PB_Larv_UpperOutliers_Ba136, PB_Larv_LowerOutliers_Ba136)
View(PB_Larv_Outliers_Ba136)

PB_Larv_Fe57_NoNA <- na.omit(PB_Larv_Fe57)
IQR(PB_Larv_Fe57_NoNA$value)
quantile(PB_Larv_Fe57_NoNA$value)
PB_Larv_LowerOutliers_Fe57 <- PB_Larv_Fe57[which(PB_Larv_Fe57$value < 2090.952),]
PB_Larv_UpperOutliers_Fe57 <- PB_Larv_Fe57[which(PB_Larv_Fe57$value > 43967.512),]
PB_Larv_Outliers_Fe57 <- rbind(PB_Larv_UpperOutliers_Fe57, PB_Larv_LowerOutliers_Fe57)
View(PB_Larv_Outliers_Fe57)

PB_Larv_Ca48_NoNA <- na.omit(PB_Larv_Ca48)
IQR(PB_Larv_Ca48_NoNA$value)
quantile(PB_Larv_Ca48_NoNA$value)
PB_Larv_LowerOutliers_Ca48 <- PB_Larv_Ca48[which(PB_Larv_Ca48$value < 1603686),]
PB_Larv_UpperOutliers_Ca48 <- PB_Larv_Ca48[which(PB_Larv_Ca48$value > 219511417),]
PB_Larv_Outliers_Ca48 <- rbind(PB_Larv_UpperOutliers_Ca48, PB_Larv_LowerOutliers_Ca48)
View(PB_Larv_Outliers_Ca48)





PB_Larv_NonOutliers_Mg24 <- PB_Larv_Melt2[which(PB_Larv_Melt2$value <=170419.2*1 & => ),]


PB_Larv_Mg24_NoNA <- na.omit(PB_Larv_Mg24)
IQR(PB_Larv_Mg24_NoNA$value)
quantile(PB_Larv_Mg24_NoNA$value)
-----------------------------------------------------------------
WRL_Juv_Mg24 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=WRL_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Mg26 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Mg26'),]
ggplot(data=WRL_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Si28..rel <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=WRL_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ca43 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ca43'),]
ggplot(data=WRL_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ca44 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ca44'),]
ggplot(data=WRL_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ca46 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ca46'),]
ggplot(data=WRL_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Mn55 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=WRL_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Co59 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=WRL_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Cu63 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=WRL_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Cu65 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Cu65'),]
ggplot(data=WRL_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Zn64 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=WRL_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Zn66 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Zn66'),]
ggplot(data=WRL_Juv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Sr86 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Sr86'),]
ggplot(data=WRL_Juv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Sr88 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=WRL_Juv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ba137 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ba136'),]
ggplot(data=WRL_Juv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ba138 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=WRL_Juv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_La139 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='La.Ca'),]
ggplot(data=WRL_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Pb206 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Pb206'),]
ggplot(data=WRL_Juv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Pb207 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Pb207'),]
ggplot(data=WRL_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Pb208 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=WRL_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Cr52 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Cr.Ca'),]
ggplot(data=WRL_Juv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ba136 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ba136'),]
ggplot(data=WRL_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Fe57 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=WRL_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Juv_Ca48 <- WRL_Juv_Melt[which(WRL_Juv_Melt$variable=='Ca48'),]
ggplot(data=WRL_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
------------------------------------------------------------------
WRL_Larv_Mg24 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=WRL_Larv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Mg26 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Mg26'),]
ggplot(data=WRL_Larv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Si28..rel <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Si28..rel'),]
ggplot(data=WRL_Larv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ca43 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ca43'),]
ggplot(data=WRL_Larv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ca44 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ca44'),]
ggplot(data=WRL_Larv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ca46 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ca46'),]
ggplot(data=WRL_Larv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Mn55 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Mn.Ca'),]
ggplot(data=WRL_Larv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Co59 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Co.Ca'),]
ggplot(data=WRL_Larv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Cu63 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Cu.Ca'),]
ggplot(data=WRL_Larv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Cu65 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Cu65'),]
ggplot(data=WRL_Larv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Zn64 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Zn.Ca'),]
ggplot(data=WRL_Larv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Zn66 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Zn66'),]
ggplot(data=WRL_Larv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Sr86 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Sr86'),]
ggplot(data=WRL_Larv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Sr88 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Sr.Ca'),]
ggplot(data=WRL_Larv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ba137 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ba136'),]
ggplot(data=WRL_Larv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ba138 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ba.Ca'),]
ggplot(data=WRL_Larv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_La139 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='La.Ca'),]
ggplot(data=WRL_Larv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Pb206 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Pb206'),]
ggplot(data=WRL_Larv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Pb207 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Pb207'),]
ggplot(data=WRL_Larv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Pb208 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Pb.Ca'),]
ggplot(data=WRL_Larv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Cr52 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Cr.Ca'),]
ggplot(data=WRL_Larv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ba136 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ba136'),]
ggplot(data=WRL_Larv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Fe57 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Fe.Ca'),]
ggplot(data=WRL_Larv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

WRL_Larv_Ca48 <- WRL_Larv_Melt[which(WRL_Larv_Melt$variable=='Ca48'),]
ggplot(data=WRL_Larv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
----------------------------------------------------------

MB_Juv_Mg24 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=MB_Juv_Mg24, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
ggplot(data=MB_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Mg26 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Mg26'),]
ggplot(data=MB_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Si28..rel <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=MB_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ca43 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ca43'),]
ggplot(data=MB_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ca44 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ca44'),]
ggplot(data=MB_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ca46 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ca46'),]
ggplot(data=MB_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Mn55 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=MB_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Co59 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=MB_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Cu63 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=MB_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Cu65 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Cu65'),]
ggplot(data=MB_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Zn64 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=MB_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Zn66 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Zn66'),]
ggplot(data=MB_Juv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Sr86 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Sr86'),]
ggplot(data=MB_Juv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Sr88 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=MB_Juv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ba137 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ba137'),]
ggplot(data=MB_Juv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ba138 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=MB_Juv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_La139 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='La.Ca'),]
ggplot(data=MB_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Pb206 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Pb206'),]
ggplot(data=MB_Juv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Pb207 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Pb207'),]
ggplot(data=MB_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Pb208 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=MB_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Cr52 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Cr.Ca'),]
ggplot(data=MB_Juv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ba136 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ba136'),]
ggplot(data=MB_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Fe57 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=MB_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Juv_Ca48 <- MB_Juv_Melt[which(MB_Juv_Melt$variable=='Ca48'),]
ggplot(data=MB_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
--------------------------------------------------------------------------
MB_Larv_Mg24 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=MB_Larv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Mg26 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Mg26'),]
ggplot(data=MB_Larv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Si28..rel <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Si28..rel'),]
ggplot(data=MB_Larv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ca43 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ca43'),]
ggplot(data=MB_Larv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ca44 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ca44'),]
ggplot(data=MB_Larv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ca46 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ca46'),]
ggplot(data=MB_Larv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Mn55 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Mn.Ca'),]
ggplot(data=MB_Larv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Co59 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Co.Ca'),]
ggplot(data=MB_Larv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Cu63 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Cu.Ca'),]
ggplot(data=MB_Larv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Cu65 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Cu65'),]
ggplot(data=MB_Larv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Zn64 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Zn.Ca'),]
ggplot(data=MB_Larv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Zn66 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Zn66'),]
ggplot(data=MB_Larv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Sr86 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Sr86'),]
ggplot(data=MB_Larv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Sr88 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Sr.Ca'),]
ggplot(data=MB_Larv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ba137 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ba137'),]
ggplot(data=MB_Larv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ba138 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ba.Ca'),]
ggplot(data=MB_Larv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_La139 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='La.Ca'),]
ggplot(data=MB_Larv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Pb206 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Pb206'),]
ggplot(data=MB_Larv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Pb207 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Pb207'),]
ggplot(data=MB_Larv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Pb208 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Pb.Ca'),]
ggplot(data=MB_Larv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Cr52 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Cr.Ca'),]
ggplot(data=MB_Larv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ba136 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ba136'),]
ggplot(data=MB_Larv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Fe57 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Fe.Ca'),]
ggplot(data=MB_Larv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

MB_Larv_Ca48 <- MB_Larv_Melt[which(MB_Larv_Melt$variable=='Ca48'),]
ggplot(data=MB_Larv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
------------------------------------------------------------------------------------------------
FBW_Juv_Mg24 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=FBW_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Mg26 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Mg26'),]
ggplot(data=FBW_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Si28..rel <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=FBW_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ca43 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ca43'),]
ggplot(data=FBW_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ca44 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ca44'),]
ggplot(data=FBW_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ca46 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ca46'),]
ggplot(data=FBW_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Mn55 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=FBW_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Co59 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=FBW_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Cu63 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=FBW_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Cu65 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Cu65'),]
ggplot(data=FBW_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Zn64 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=FBW_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Zn66 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Zn66'),]
ggplot(data=FBW_Juv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Sr86 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Sr86'),]
ggplot(data=FBW_Juv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Sr88 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=FBW_Juv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ba137 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ba137'),]
ggplot(data=FBW_Juv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ba138 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=FBW_Juv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_La139 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='La.Ca'),]
ggplot(data=FBW_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Pb206 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Pb206'),]
ggplot(data=FBW_Juv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Pb207 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Pb207'),]
ggplot(data=FBW_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Pb208 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=FBW_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Cr52 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Cr.Ca'),]
ggplot(data=FBW_Juv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ba136 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ba136'),]
ggplot(data=FBW_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Fe57 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=FBW_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Juv_Ca48 <- FBW_Juv_Melt[which(FBW_Juv_Melt$variable=='Ca48'),]
ggplot(data=FBW_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
-------------------------------------------------------------------------
FBE_Juv_Mg24 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=FBE_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Mg26 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Mg26'),]
ggplot(data=FBE_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Si28..rel <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=FBE_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ca43 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ca43'),]
ggplot(data=FBE_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ca44 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ca44'),]
ggplot(data=FBE_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ca46 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ca46'),]
ggplot(data=FBE_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Mn55 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=FBE_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + ylim(0,1000)

FBE_Juv_Co59 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=FBE_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Cu63 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=FBE_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Cu65 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Cu65'),]
ggplot(data=FBE_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Zn64 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=FBE_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Zn66 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Zn66'),]
ggplot(data=FBE_Juv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Sr86 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Sr86'),]
ggplot(data=FBE_Juv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Sr88 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=FBE_Juv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ba137 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ba137'),]
ggplot(data=FBE_Juv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ba138 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=FBE_Juv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_La139 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='La.Ca'),]
ggplot(data=FBE_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Pb206 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Pb206'),]
ggplot(data=FBE_Juv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Pb207 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Pb207'),]
ggplot(data=FBE_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Pb208 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=FBE_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Cr52 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Cr.Ca'),]
ggplot(data=FBE_Juv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Ba136 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ba136'),]
ggplot(data=FBE_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBE_Juv_Fe57 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=FBE_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + ylim(0,5000)

FBE_Juv_Ca48 <- FBE_Juv_Melt[which(FBE_Juv_Melt$variable=='Ca48'),]
ggplot(data=FBE_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

--------------------------------------------------------------------------
FBW_Larv_Mg24 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Mg24'),]
ggplot(data=FBW_Larv_Mg24, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Mg26 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Mg26'),]
ggplot(data=FBW_Larv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Si28..rel <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Si28..rel'),]
ggplot(data=FBW_Larv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ca43 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ca43'),]
ggplot(data=FBW_Larv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ca44 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ca44'),]
ggplot(data=FBW_Larv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ca46 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ca46'),]
ggplot(data=FBW_Larv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Mn55 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Mn55'),]
ggplot(data=FBW_Larv_Mn55, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Co59 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Co59'),]
ggplot(data=FBW_Larv_Co59, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Cu63 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Cu63'),]
ggplot(data=FBW_Larv_Cu63, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Cu65 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Cu65'),]
ggplot(data=FBW_Larv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Zn64 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Zn.Ca'),]
ggplot(data=FBW_Larv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Zn66 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Zn66'),]
ggplot(data=FBW_Larv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Sr86 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Sr86'),]
ggplot(data=FBW_Larv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Sr88 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Sr88'),]
ggplot(data=FBW_Larv_Sr88, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ba137 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ba137'),]
ggplot(data=FBW_Larv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ba138 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ba138'),]
ggplot(data=FBW_Larv_Ba138, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_La139 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='La139'),]
ggplot(data=FBW_Larv_La139, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Pb206 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Pb206'),]
ggplot(data=FBW_Larv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Pb207 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Pb207'),]
ggplot(data=FBW_Larv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Pb208 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Pb208'),]
ggplot(data=FBW_Larv_Pb208, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Cr52 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Cr52'),]
ggplot(data=FBW_Larv_Cr52, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ba136 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ba136'),]
ggplot(data=FBW_Larv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Fe57 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Fe57'),]
ggplot(data=FBW_Larv_Fe57, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

FBW_Larv_Ca48 <- FBW_Larv_Melt[which(FBW_Larv_Melt$variable=='Ca48'),]
ggplot(data=FBW_Larv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  
------------------------------------------------------------------------------------
PHB_Juv_Mg24 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=PHB_Juv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Mg26 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Mg26'),]
ggplot(data=PHB_Juv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Si28..rel <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Si28..rel'),]
ggplot(data=PHB_Juv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ca43 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ca43'),]
ggplot(data=PHB_Juv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ca44 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ca44'),]
ggplot(data=PHB_Juv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ca46 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ca46'),]
ggplot(data=PHB_Juv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Mn55 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Mn.Ca'),]
ggplot(data=PHB_Juv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Co59 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Co.Ca'),]
ggplot(data=PHB_Juv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Cu63 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Cu.Ca'),]
ggplot(data=PHB_Juv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Cu65 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Cu65'),]
ggplot(data=PHB_Juv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Zn64 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Zn.Ca'),]
ggplot(data=PHB_Juv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Zn66 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Zn66'),]
ggplot(data=PHB_Juv_Zn66, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Sr86 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Sr.Ca'),]
ggplot(data=PHB_Juv_Sr86, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Sr88 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Sr88'),]
ggplot(data=PHB_Juv_Sr88, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ba137 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ba.Ca'),]
ggplot(data=PHB_Juv_Ba137, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ba138 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ba138'),]
ggplot(data=PHB_Juv_Ba138, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_La139 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='La.Ca'),]
ggplot(data=PHB_Juv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Pb206 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=PHB_Juv_Pb206, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Pb207 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Pb207'),]
ggplot(data=PHB_Juv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Pb208 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Pb.Ca'),]
ggplot(data=PHB_Juv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ba136 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ba136'),]
ggplot(data=PHB_Juv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Fe57 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Fe.Ca'),]
ggplot(data=PHB_Juv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Juv_Ca48 <- PHB_Juv_Melt[which(PHB_Juv_Melt$variable=='Ca48'),]
ggplot(data=PHB_Juv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

--------------------------------------------------------------------------
PHB_Larv_Mg24 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Mg24.Ca46'),]
ggplot(data=PHB_Larv_Mg24, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Mg26 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Mg26'),]
ggplot(data=PHB_Larv_Mg26, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Si28..rel <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Si28..rel'),]
ggplot(data=PHB_Larv_Si28..rel, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ca43 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ca43'),]
ggplot(data=PHB_Larv_Ca43, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ca44 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ca44'),]
ggplot(data=PHB_Larv_Ca44, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ca46 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ca46'),]
ggplot(data=PHB_Larv_Ca46, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Mn55 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Mn.Ca'),]
ggplot(data=PHB_Larv_Mn55, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Co59 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Co.Ca'),]
ggplot(data=PHB_Larv_Co59, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Cu63 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Cu.Ca'),]
ggplot(data=PHB_Larv_Cu63, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Cu65 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Cu65'),]
ggplot(data=PHB_Larv_Cu65, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Zn64 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Zn.Ca'),]
ggplot(data=PHB_Larv_Zn64, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Zn66 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Zn66'),]
ggplot(data=PHB_Larv_Zn66, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Sr86 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Sr86'),]
ggplot(data=PHB_Larv_Sr86, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Sr88 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Sr.Ca'),]
ggplot(data=PHB_Larv_Sr88, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ba137 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ba137'),]
ggplot(data=PHB_Larv_Ba137, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ba138 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ba.Ca'),]
ggplot(data=PHB_Larv_Ba138, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_La139 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='La.Ca'),]
ggplot(data=PHB_Larv_La139, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Pb206 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Pb206'),]
ggplot(data=PHB_Larv_Pb206, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Pb207 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Pb207'),]
ggplot(data=PHB_Larv_Pb207, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Pb208 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Pb.Ca'),]
ggplot(data=PHB_Larv_Pb208, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Cr52 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Cr.Ca'),]
ggplot(data=PHB_Larv_Cr52, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ba136 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ba136'),]
ggplot(data=PHB_Larv_Ba136, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Fe57 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Fe.Ca'),]
ggplot(data=PHB_Larv_Fe57, aes(x=ID, y=value)) + geom_bar(stat="identity", position=position_dodge(), colour="black")  

PHB_Larv_Ca48 <- PHB_Larv_Melt[which(PHB_Larv_Melt$variable=='Ca48'),]
ggplot(data=PHB_Larv_Ca48, aes(x=ID, y=log10(value))) + geom_bar(stat="identity", position=position_dodge(), colour="black")  


R CODE REALLY SUCKS!!





