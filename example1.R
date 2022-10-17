
hospitals <- read.table("https://bit.ly/2qZwdMn", header =T) 

# 첫 번째 열을 제거합니다.
hospitals <- hospitals[,-1]


# 요인 변수 생성 
hospitals$Region <- as.factor(hospitals$Region)
hospitals$Med.school <- as.factor(hospitals$Med.school)

attach(hospitals)

# 그래프를 작성할 목적으로 범주형 변수를 제거합니다.
hospitals_num <- hospitals[,c(-7,-8)]
pairs(hospitals_num)

library(psych)
# 산점도 행렬 생성 
pairs.panels(hospitals_num)

# 사용하는 것이 더 낫습니다.


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  
  if(missing(cex.cor)) 
    cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # printCoefmat에서 빌려옴
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}


pairs(hospitals_num, upper.panel = panel.cor)



lm <- lm(Infection.risk ~ hospitals_num$Culture, data = hospitals_num)
summary(lm)

# 모델이 데이터를 얼마나 잘 적합합니까?


# 다중 선형 회귀를 사용하여 감염 위험을 예측하는 데 사용할 다른 변수는 무엇입니까?
lm1 <- lm(Infection.risk ~ . , data = hospitals)
summary(lm1)



# 시설, 문화, 체류 기간
# 중요한 변수를 선택하고 여러 변수를 만드십시오. 
lm2 <- lm(Infection.risk ~ hospitals_num$Length.of.stay + hospitals_num$Culture + hospitals_num$Facilities+hospitals$X.ray, data = hospitals)
summary(lm2)






# Q6 우리 지역의 감염 위험이 다른가요? 가설을 작성하고 중요한 테스트를 제공합니다.
# 
# H_0은 모든 그룹의 평균이 동일하다는 것입니다. 
# H_1 적어도 하나의 다른 평균 

m<-aov(Infection.risk~Region, data=hospitals)
summary(m)

#              Df Sum Sq Mean Sq F value Pr(>F)  
# Region        3   14.0   4.666   2.714 0.0484 *
# Residuals   109  187.4   1.719   

# We see that region factor variable is signinficant so that at least two of the regions are different in 
# terms of infection ratio. 


# 우리는 지역 요인 변수가 중요하므로 지역 중 적어도 두 곳이 서로 다른 것을 알 수 있습니다. 
# 그리고 더 정확하려면 p-값을 조정할 수 있습니다. 
pairwise.t.test(Infection.risk, Region, p.adjust.method = "none")
pairwise.t.test(Infection.risk, Region, p.adjust.method = "bonferroni")
# data:  Infection.risk and Region 
#     1     2     3    
#   2 1.000 -     -    
#   3 0.032 0.859 -    
#   4 1.000 1.000 1.000
# P value adjustment method: bonferroni 
#
#
# bonferroni 조정 후 영역(1과 3)이 다른 것을 볼 수 있습니다. 

TukeyHSD(m, conf.level = 0.90)

# Fit: aov(formula = Infection.risk ~ Region, data = hospitals)

# $Region
# diff        lwr        upr     p adj
# 2-1 -0.4669643 -1.2537701  0.3198415 0.5168684
# 3-1 -0.9336873 -1.6952799 -0.1720946 0.0269952
# 4-1 -0.4794643 -1.4323334  0.4734048 0.6489580
# 3-2 -0.4667230 -1.2007205  0.2672746 0.4563333
# 4-2 -0.0125000 -0.9434612  0.9184612 0.9999891
# 4-3  0.4542230 -0.4555290  1.3639749 0.6545991
# 
# 또한 Tuky 절차에서 우리는 두 영역 3-1이 다르다는 것을 알 수 있습니다. 



# Q7: 각 병원의 간호사 수는 유의한 공변량입니까? 차이점이 다른가요
# 간호사 수에 의해 주도되는 지역?
# 위의 질문에 답하려면 ANCOVA를 수행하고 간호사 수를 공변량으로 고려해야 합니다. 
# 간호사 수가 공변량인지 여부를 확인합니다.. 

library(car)
# library(lsmeans) # is depricated 
library(estimability)
library(emmeans)

Anova(lm(Infection.risk~Region+Nurses, data=hospitals), type=3)


# > Anova(lm(Infection.risk~Region+Nurses, data=hospitals), type=3)
# Anova 테이블(유형 III 테스트)

#  Response: Infection.risk
#  Sum Sq  Df  F value    Pr(>F)    
#  (Intercept) 329.78   1 223.9144 < 2.2e-16 ***
#  Region       11.06   3   2.5028   0.06314 .  
#  Nurses       28.32   1  19.2287 2.703e-05 ***
#  Residuals   159.06 108 

# 지역이 더 이상 중요하지 않고 간호사 수가 중요하다는 것을 알 수 있습니다. 
# 이는 감염률의 차이가 간호사 수의 차이로 인한 것임을 의미합니다. 
# 그리고 지역의 차이로 인한 것이 아닙니다. 


# 간호사를 조정한 후 지역이 여기에서 의미가 없음을 알 수 있습니다. 
# 그리고 간호사의 숫자가 여기에서 운전하고 있습니다.
# 간호사 수에 따라 조정할 수 있습니다. 
emm_options(contrasts=c("contr.treatment", "contr.poly"))
my.model<-lm(Infection.risk~Region+Nurses, data=hospitals)
lsmeans(my.model, specs = "Region", contr = "pairwise" )



# Q8: 병원이 의과대학 부속 의과대학이고 지역이 다른 경우를 고려하면,
# 이 병원들은 감염 위험이 모두 다른가요?

# 의과대학(예 또는 아니요)은 범주형 변수입니다. 
# 두 개의 범주형 변수(지역 및 MedSchool)와 하나의 지속적인 변수 감염 위험이 있는 양방향 노바를 수행하려면 여기에서 필요합니다. 

typeof(Med.school)
# "integer" 
# 우리는 그것을 사실로 변환해야 합니다. 

factorMed.School <- ifelse(Med.school==1, TRUE, FALSE)


typeof(factorMed.School)

model<-lm(Infection.risk~Region + factorMed.School + Region * factorMed.School, data=hospitals)
summary(model)

# 어떤 조합도 중요하지 않다는 것을 요약에서 볼 수 있습니다.

interaction.plot(Region , Med.school, Infection.risk, col=1:2)

# 우리는 일부 지역과 의과 대학 사이에 약간의 상호 작용이 있음을 알 수 있습니다. 
