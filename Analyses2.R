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

#------intrinsic mortality -----------
mortE <- read_excel("mortInter.xlsx")
mortE$Treatment <- factor(mortE$Treatment, levels=c("Control", "Low", "Medium", "High"))


ggplot(mortE, aes(Treatment, Mortality, fill=Treatment)) +
  geom_bar(aes(Treatment, Mortality, fill=Treatment), colour="black", position=position_dodge(), stat="identity") +
  theme_classic(base_size=15) +
  theme(legend.position= "none")+
  ylab("Mortality (%)")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

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
summary(Cann)
TukeyHSD(Cann)


df.Inter <- finalInter %>%
  group_by(Treatment) %>%
  summarise(
    sd = sd(Cannibalism, na.rm = TRUE),
    Cannibalism = mean(Cannibalism)
  )
df.Intra

ggplot(df.Inter,
       aes(Treatment, Cannibalism, fill = Treatment)) +
  geom_bar(aes(Treatment, Cannibalism, fill = Treatment), colour="black", position="dodge" , stat="identity") +
  theme_classic(base_size = 15) +
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))+
  geom_errorbar(aes(ymin = Cannibalism-sd, ymax = Cannibalism+sd), position=position_dodge(0.9),width = 0.2)


#correlation with size variance

fitLM <- lm(log1p(Cannibalism)~sqrt(Var_width), data=data2E)
check_model(fitLM)
check_normality(fitLM)
summary(fitLM)
Anova(fitLM)


ggplot(data2E,aes(y=Var_width,x=Cannibalism))+
  geom_point(color="#66c2a5")+
  geom_smooth(se = TRUE, method="lm", color="#66c2a5")+
  ylab("Size Variance")+
  theme_classic(base_size = 15)+ 
  theme(legend.position= c(0.3, 0.85))+
  scale_color_brewer(palette="Set2") 


ggplot(data2E, aes(Treatment, Var_width, fill =Treatment )) +
  geom_boxplot() +
  theme_classic(base_size = 15) +
  ylab("Size Variance")+
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

#growth interaction

Initial <- read_excel("InitialInter.xlsx")
Initial$Treatment <- factor(Initial$Treatment, levels=c("Control", "Low", "Medium", "High"))

ggplot(Initial, aes(Treatment, Av_Width, fill = Treatment)) +
  geom_boxplot() +
  ylab("Initial Size (mm)")+
  theme_classic(base_size = 16) +
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

a <- aov(log10(Av_Width)~Treatment,data=Initial)
check_model(a)
check_normality(a)
summary(a)

growth <- read_excel("growthInter.xlsx")
growth$Treatment <- factor(growth$Treatment, levels=c("Control", "Low", "Medium", "High"))

ggplot(growth, aes(Treatment, Growth_width, fill = Treatment)) +
  geom_boxplot() +
  ylab(expression("Growth rate (mm day"^-1*")"))+
  theme_classic(base_size = 15) +
  theme(legend.position= "none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

I <- aov(Growth_width~Treatment,data=growth)
hist(growth$Growth_width)
check_model(I)
summary(I)


#final size
End <- read_excel("endInter.xlsx")
End$Treatment <- factor(End$Treatment, levels=c("Control", "Low", "Medium", "High"))

ggplot(End, aes(Treatment, Av_width, fill = Treatment)) +
  geom_boxplot() +
  ylab("Final Size (mm)")+
  theme_classic(base_size = 16) +
  theme(legend.position= "none")+ 
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

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
p <- ggplot(Mortality) + 
  geom_bar(aes(Treatment, Mortality, fill=Treatment), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Mortality (%)")+
  scale_fill_manual(values=c("#969696",  "#a50f15"))
p + theme(legend.position="none")

mort_aov <- aov(Mortality ~Treatment, data=Mortality)
check_model(mort_aov)
summary(mort_aov)

# growth
p <- ggplot(Ind_Growth, aes(Treatment, Growth, fill = Treatment)) +
  geom_boxplot() +
  ylab(expression("Growth rate (mm day"^-1*")"))+
  theme_classic(base_size = 15) +
  scale_fill_manual(values=c("#969696",  "#a50f15"))
p + theme(legend.position="none")

growth_aov <- aov(Growth ~Treatment, data=Ind_Growth)
check_model(growth_aov)
summary(growth_aov)

ggplot(Individuals, aes(Cycle, Growth, fill = Treatment)) +
  geom_boxplot() +
  theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#969696",  "#a50f15"))

#__________number of larvae (Supplement)________

Cann <- read_excel("CannInter.xlsx") 
final_c <- read_excel("CannInter2.xlsx") #survival excludes intrinsic mortality

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

ggplot(Start, aes(Treatment, name, fill=Treatment)) +
  geom_bar(aes(Treatment, name, fill=Treatment), colour="black", position=position_dodge(), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Larvae")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))

ggplot(End, aes(Treatment, name, fill=Treatment)) +
  geom_bar(aes(Treatment, name, fill=Treatment), colour="black",position=position_dodge(), stat="identity") +
  theme_classic(base_size = 15) +
  ylab("Larvae")+
  theme(legend.position="none")+
  scale_fill_manual(values=c("#969696", "#a6d854", "#045a8d", "#a50f15"))




