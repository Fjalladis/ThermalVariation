library(readxl)
library(ggplot2)
library(car)
library(emmeans)
library(performance)
library(see)
library(RColorBrewer)
library(viridis)
library(viridisLite)
library(patchwork)
library(statmod)
library(dplyr)
library(survival)
library(survminer)
library(coxme)

dataE <- read_xlsx("dataE.xlsx") #full dataset cycle A+B
data2E <- read_xlsx("data2E.xlsx") #all size measurements, excludes cycle B
data2E$Treatment <- factor(data2E$Treatment, levels=c("Control", "Low", "Medium", "High"))

#------intrinsic mortality -----------
mortE <- read_excel("mortInter.xlsx")
mortE$Treatment <- factor(mortE$Treatment, levels=c("Control", "Low", "Medium", "High"))

png("P1_Mortality.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(mortE, aes(Treatment, Mortality, fill=Treatment)) +
  geom_bar(aes(Treatment, Mortality, fill=Treatment), colour="black", position=position_dodge(), stat="identity") +
  theme_classic(base_size=15) +
  theme(legend.position= "none")+
  ylab("Mortality (%)")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

Mortality_aov <- aov(Dead ~ Treatment, data = dataE)
check_model(Mortality_aov)

summary(Mortality_aov) #no significance
TukeyHSD(Mortality_aov) 

#------cannibalism-----------
finalInter <- read_xlsx("finalInter.xlsx")

finalInter$Treatment <- factor(finalInter$Treatment, levels=c("Control", "Low", "Medium", "High"))


df <- finalInter %>%                                        
  group_by(Treatment) %>%                         
  summarise_at(vars(Cannibalism),              
               list(name = sum))
df

#significance test with anova

Cann <- aov(Cannibalism~Treatment, data=finalInter)
check_model(Cann)
check_normality(Cann)
summary(Cann)
TukeyHSD(Cann)


df.Inter <- finalInter %>%
  group_by(Treatment) %>%
  summarise(
    sd = sd(Cannibalism, na.rm = TRUE),
    Cannibalism = mean(Cannibalism)
  )
df.Inter

png("P2_Cannibalism.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(df.Inter,
       aes(Treatment, Cannibalism, fill = Treatment)) +
  geom_bar(aes(Treatment, Cannibalism, fill = Treatment), colour="black", position="dodge" , stat="identity") +
  theme_classic(base_size = 15) +
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))+
  geom_errorbar(aes(ymin = Cannibalism-sd, ymax = Cannibalism+sd), position=position_dodge(0.9),width = 0.2)
p
print(p)
dev.off()


#significance test with binomial GLM
Cann_binom <- read_excel("Cannibalism.xlsx")

fitBinom <- glm(Survival~Treatment, family = binomial, data=Cann_binom)
summary(fitBinom)

anova(fitBinom, test = "Chisq")

r2(fitBinom) 

#correlation with size variance

fitLM <- lm(log1p(Cannibalism)~sqrt(Var_width), data=data2E)
check_model(fitLM)
check_normality(fitLM)
summary(fitLM)
Anova(fitLM)

png("P3_SizeVar.png", width = 9, height = 10, units = 'cm', res = 300)
p<- ggplot(data2E,aes(y=Var_width,x=Cannibalism))+
  geom_point(color="#66c2a5")+
  geom_smooth(se = TRUE, method="lm", color="#66c2a5")+
  ylab("Size variance (mm)")+
  theme_classic(base_size = 15)+ 
  theme(legend.position= c(0.3, 0.85))+
  scale_color_brewer(palette="Set2") 
p
print(p)
dev.off()

png("P4_SizeTreat.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(data2E, aes(Treatment, Var_width, fill =Treatment )) +
  geom_boxplot() +
  theme_classic(base_size = 15) +
  ylab("Size Variance (mm)")+
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

#growth interaction

Initial <- read_excel("InitialInter.xlsx")
Initial$Treatment <- factor(Initial$Treatment, levels=c("Control", "Low", "Medium", "High"))

png("P5_SizeInitial.png", width = 9, height = 10, units = 'cm', res = 300)
p<- ggplot(Initial, aes(Treatment, Av_Width, fill = Treatment)) +
  geom_boxplot() +
  ylab("Initial Size (mm)")+
  theme_classic(base_size = 16) +
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

a <- aov(log10(Av_Width)~Treatment,data=Initial)
check_model(a)
check_normality(a)
summary(a)

growth <- read_excel("growthInter.xlsx")
growth$Treatment <- factor(growth$Treatment, levels=c("Control", "Low", "Medium", "High"))

png("P6_Growth.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(growth, aes(Treatment, Growth_width, fill = Treatment)) +
  geom_boxplot() +
  ylab(expression("Growth rate (mm day"^-1*")"))+
  theme_classic(base_size = 15) +
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

I <- aov(Growth_width~Treatment,data=growth)
hist(growth$Growth_width)
check_model(I)
summary(I)


#final size
End <- read_excel("endInter.xlsx")
End$Treatment <- factor(End$Treatment, levels=c("Control", "Low", "Medium", "High"))

png("P7_SizeFinal.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(End, aes(Treatment, Av_width, fill = Treatment)) +
  geom_boxplot() +
  ylab("Final Size (mm)")+
  theme_classic(base_size = 16) +
  theme(legend.position= "none")+ 
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

b <- aov(Av_width~Treatment,data=End)
hist(End$Av_width)
summary(b)
TukeyHSD(b)

#------single experiment -----------
Individuals <- read_excel("Individuals.xlsx") 
Individuals$Cycle <- factor(Individuals$Cycle, levels=c("1", "2", "3", "4"))

Ind_Growth <- read_excel("growthSingle.xlsx") #total growth
Mortality <- read_excel("mortSingle.xlsx")

# mortality
png("P8_IndMort.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(Mortality) + 
  geom_bar(aes(Treatment, Mortality, fill=Treatment), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Mortality (%)")+
  scale_fill_manual(values=c("#969696",  "#a50f15"))+
  theme(legend.position="none")
p
print(p)
dev.off()

mort_aov <- aov(Mortality ~Treatment, data=Mortality)
check_model(mort_aov)
summary(mort_aov)

# growth
png("P9_IndGrowth.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(Ind_Growth, aes(Treatment, Growth, fill = Treatment)) +
  geom_boxplot() +
  ylab(expression("Growth rate (mm day"^-1*")"))+
  theme_classic(base_size = 15) +
  scale_fill_manual(values=c("#969696",  "#a50f15"))+
  theme(legend.position="none")
p
print(p)
dev.off()

growth_aov <- aov(Growth ~Treatment, data=Ind_Growth)
check_model(growth_aov)
summary(growth_aov)

p <- ggplot(Individuals, aes(Cycle, Growth, fill = Treatment)) +
  geom_boxplot() +
  theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#969696",  "#a50f15"))
p


#__________number of larvae (Supplement)________

Cann <- read_excel("CannInter.xlsx") 
Cann$Treatment <- factor(Cann$Treatment, levels=c("Control", "Low", "Medium", "High"))
final_c <- read_excel("CannInter2.xlsx") #survival excludes intrinsic mortality
final_c$Treatment <- factor(final_c$Treatment, levels=c("Control", "Low", "Medium", "High"))

surv <- glm(Survival ~Treatment, family=poisson, data=final_cann)
check_model(surv)
summary(surv)

#basic analysis larvae number initially 
Start <- Cann %>%                                       
  group_by(Treatment) %>%                        
  summarise_at(vars(Larvae),              
               list(name = sum))
Start
#basic analysis larvae number end
End <- final_c %>%                                        
  group_by(Treatment) %>%                         
  summarise_at(vars(Survival),             
               list(name = sum))
End

png("PS1.png", width = 9, height = 10, units = 'cm', res = 300)
p <- ggplot(Start, aes(Treatment, name, fill=Treatment)) +
  geom_bar(aes(Treatment, name, fill=Treatment), colour="black", position=position_dodge(), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Larvae")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()

png("PS2.png", width = 9, height = 10, units = 'cm', res = 300)
p<-ggplot(End, aes(Treatment, name, fill=Treatment)) +
  geom_bar(aes(Treatment, name, fill=Treatment), colour="black",position=position_dodge(), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Larvae")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))
p
print(p)
dev.off()



