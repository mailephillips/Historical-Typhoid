library(lme4)
library(ggplot2)
library(r2glmm)
library(lattice)
library(merTools)
library(MuMIn)

setwd("~/Desktop/historical_typhoid")

b.ltALL <- read.csv("final B_lts.csv")[,-1]

b.lt.yr.list <- matrix(NA, ncol=16,nrow=30)
for (c in 1:16){
  b.lt.yr <- b.ltALL[,c]
  yrbeta.lt <- rep(NA, 43)
  for (i in 1:43){
    yrbeta.lt[i] <- mean(b.lt.yr[(1+(13*(i-1))):(13+(13*(i-1)))], na.rm=T)
  }
  B.lt.short <- yrbeta.lt[14:length(yrbeta.lt)]
  b.lt.yr.list[,c] <- B.lt.short
}

receipts.W <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                      c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                        rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                        rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                        rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                      as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,1],
                      (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,1]))))
names(receipts.W) <- c("B.lt","Year","City","investment")
receipts.W$investment <- as.numeric(as.character((receipts.W$investment)))
receipts.W$B.lt <- as.numeric(as.character((receipts.W$B.lt)))

exp.W <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                              c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                              as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,2],
                              (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,2]))))
names(exp.W) <- c("B.lt","Year","City","investment")
exp.W$investment <- as.numeric(as.character((exp.W$investment)))
exp.W$B.lt <- as.numeric(as.character((exp.W$B.lt)))

exp.S <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                              c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                              as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,3],
                              (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,3]))))
names(exp.S) <- c("B.lt","Year","City","investment")
exp.S$investment <- as.numeric(as.character((exp.S$investment)))
exp.S$B.lt <- as.numeric(as.character((exp.S$B.lt)))

outlays.W <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                                  c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                    rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                    rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                    rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                                  as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,4],
                                  (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,4]))))
names(outlays.W) <- c("B.lt","Year","City","investment")
outlays.W$investment <- as.numeric(as.character((outlays.W$investment)))
outlays.W$B.lt <- as.numeric(as.character((outlays.W$B.lt)))

outlays.S <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                                  c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                    rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                    rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                    rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                                  as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,5],
                                (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,5]))))
names(outlays.S) <- c("B.lt","Year","City","investment")
outlays.S$investment <- as.numeric(as.character((outlays.S$investment)))
outlays.S$B.lt <- as.numeric(as.character((outlays.S$B.lt)))

value.W <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                                c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                  rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                  rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                  rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                                as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,6],
                              (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,6]))))
names(value.W) <- c("B.lt","Year","City","investment")
value.W$investment <- as.numeric(as.character((value.W$investment)))
value.W$B.lt <- as.numeric(as.character((value.W$B.lt)))

debt.W <-  as.data.frame(cbind(c(b.lt.yr.list[-c(451:480)]), rep(1902:1931,15), 
                               c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                 rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                 rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                 rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30)),
                               as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,7],
                              (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,7]))))
names(debt.W) <- c("B.lt","Year","City","investment")
debt.W$investment <- as.numeric(as.character((debt.W$investment)))
debt.W$B.lt <- as.numeric(as.character((debt.W$B.lt)))

debt.S <-  as.data.frame(cbind(c(b.lt.yr.list[-c(451:480)]), rep(1902:1931,15), 
                               c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                 rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                 rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                 rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30)),
                               as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,8],
                              (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,8]))))
names(debt.S) <- c("B.lt","Year","City","investment")
debt.S$investment <- as.numeric(as.character((debt.S$investment)))
debt.S$B.lt <- as.numeric(as.character((debt.S$B.lt)))

overall.W <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                                  c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                    rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                    rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                    rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                                  as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,9],
                                  (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,9]))))
names(overall.W) <- c("B.lt","Year","City","investment")
overall.W$investment <- as.numeric(as.character((overall.W$investment)))
overall.W$B.lt <- as.numeric(as.character((overall.W$B.lt)))

overall.S <-  as.data.frame(cbind(c(b.lt.yr.list), rep(1902:1931,16), 
                                  c(rep("Baltimore",30),rep("Boston",30),rep("Chicago",30),rep("Cincinnati",30),
                                    rep("Cleveland",30),rep("Milwaukee",30),rep("Nashville",30),rep("NewOrleans",30),
                                    rep("NewYork",30),rep("Philadelphia",30),rep("Pittsburgh",30),rep("Providence",30),
                                    rep("StLouis",30),rep("SanFrancisco",30),rep("Toledo",30),rep("WashDC",30)),
                                  as.numeric(c((read.csv("per capita financial data/percap_financial_Baltimore.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Boston.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Chicago.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Cincinnati.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Cleveland.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Milwaukee.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Nashville.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_New_Orleans.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_New_York.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Philadelphia.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Pittsburgh.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Providence.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_St_Louis.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_San_Francisco.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Toledo.csv")[,-1])[,10],
                                  (read.csv("per capita financial data/percap_financial_Washington_DC.csv")[,-1])[,10]))))
names(overall.S) <- c("B.lt","Year","City","investment")
overall.S$investment <- as.numeric(as.character((overall.S$investment)))
overall.S$B.lt <- as.numeric(as.character((overall.S$B.lt)))


g <- ggplot(overall.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, overall.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(overall.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(overall.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################

library(nloptr)
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA",xtol_rel=1e-6,maxeval=1e5)
nloptwrap2 <- function(fn,par,lower,upper,control=list(),...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0=par,eval_f=fn,lb=lower,ub=upper,opts=control,...)
    with(res,list(par=solution,
                  fval=objective,
                  feval=iterations,
                  conv=if (status>0) 0 else status,
                  message=message))
}


#system.time(test.model2 <- update(ml.model, control=lmerControl(optimizer="nloptwrap2")))
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), data=overall.W, control=lmerControl(optimizer="nloptwrap2"))
relgrad <- with(ml.model@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

summary(ml.model)
fixef(ml.model)
fixed.quant.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

#sjp.lmer(ml.model, sort.coef = "City", free.scale = FALSE)
#library(lattice)
dotplot(ranef(ml.model, condVar=TRUE))$City
qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=overall.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.325    0.402    0.251
# 2 investment 0.325    0.402    0.251
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.917    0.929    0.904
# 2 investment 0.917    0.929    0.904

r.squaredGLMM(ml.model)
# R2m     R2c
# [1,] 0.3264378 0.9837893

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.water <- city.quants


##########################################################
#########################SEWER############################
##########################################################
g <- ggplot(overall.S, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, overall.S)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(overall.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(overall.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), overall.S)
summary(ml.model)
fixef(ml.model)
fixed.quant.S <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City

ICC(outcome="B.lt", group="City", data=overall.S)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.275    0.353    0.201
# 2 investment 0.275    0.353    0.201
r2beta(ml.model, method = "sgv")
# Effect  Rsq upper.CL lower.CL
# 1      Model 0.91    0.923    0.896
# 2 investment 0.91    0.923    0.896

r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.2753709 0.9841945

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.sewer <- city.quants

fixed.quant.W <- matrix(fixed.quant.W, byrow = T, ncol=3)
fixed.quant.S <- matrix(fixed.quant.S, byrow = T, ncol=3)
colnames(fixed.quant.W) <- colnames(fixed.quant.S) <- c("mean", "lCI","uCI")
rownames(fixed.quant.W) <- rownames(fixed.quant.S) <- "Fixed (Average) Effect"

city.quants.water <- as.data.frame(rbind(exp(city.quants.water),fixed.quant.W))
city.quants.water$wCI <- with(city.quants.water, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

city.quants.sewer <- as.data.frame(rbind(exp(city.quants.sewer),fixed.quant.S))
city.quants.sewer$wCI <- with(city.quants.sewer, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

city.quants.all <- as.data.frame(cbind(city.quants.water$wCI,city.quants.sewer$wCI))
rownames(city.quants.all) <-  c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                              "Milwaukee", "Nashville", "New Orleans","New York", 
                              "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                              "San Francisco", "Toledo","Washington, D.C.", "Fixed (Average) Effects")
colnames(city.quants.all) <- c("Overall Water Investments", "Overall Sewer Investments")
write.csv(city.quants.all, "~/Desktop/Typhoid/lmer estimates2.csv")



cor(overall.S$investment, overall.W$investment, use = "complete.obs")
#[1] 0.4354465


##########################################################################################
#######################################OTHER VARIABLES####################################
##########################################################################################

##########################################################
#######################WATER EXPENSES#####################
##########################################################
g <- ggplot(exp.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, exp.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(exp.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(exp.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), exp.W)
summary(ml.model)
fixef(ml.model)
fixed.quant.exp.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=exp.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.014    0.045        0
# 2 investment 0.014    0.045        0
r2beta(ml.model, method = "sgv")
# Effect  Rsq upper.CL lower.CL
# 1      Model 0.07    0.125    0.029
# 2 investment 0.07    0.125    0.029

r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.01355937 0.8613148

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.exp.W <- city.quants

fixed.quant.exp.W <- matrix(fixed.quant.exp.W, byrow = T, ncol=3)
colnames(fixed.quant.exp.W) <- c("mean", "lCI","uCI")
rownames(fixed.quant.exp.W) <- "Fixed (Average) Effect"

city.quants.exp.W <- as.data.frame(rbind(exp(city.quants.exp.W),fixed.quant.exp.W))
city.quants.exp.W$wCI <- with(city.quants.exp.W, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))


##########################################################
#######################SEWER EXPENSES#####################
##########################################################
g <- ggplot(exp.S, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, exp.S)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(exp.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(exp.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), exp.S)
summary(ml.model)
fixef(ml.model)
fixed.quant.exp.S <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=exp.S)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.005    0.027        0
# 2 investment 0.005    0.027        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.022    0.059    0.003
# 2 investment 0.022    0.059    0.003
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.004567301 0.8422922

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.exp.S <- city.quants

fixed.quant.exp.S <- matrix(fixed.quant.exp.S, byrow = T, ncol=3)
colnames(fixed.quant.exp.S) <- c("mean", "lCI","uCI")
rownames(fixed.quant.exp.S) <- "Fixed (Average) Effect"

city.quants.exp.S <- as.data.frame(rbind(exp(city.quants.exp.S),fixed.quant.exp.S))
city.quants.exp.S$wCI <- with(city.quants.exp.S, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################WATER RECEIPTS#####################
##########################################################
g <- ggplot(receipts.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, receipts.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(receipts.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(receipts.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), receipts.W, control=lmerControl(optimizer="nloptwrap2"))
summary(ml.model)
fixef(ml.model)
fixed.quant.receipts.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=receipts.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.002    0.022        0
# 2 investment 0.002    0.022        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.012    0.043        0
# 2 investment 0.012    0.043        0
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.00241537 0.8543498

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.receipts.W <- city.quants

fixed.quant.receipts.W <- matrix(fixed.quant.receipts.W, byrow = T, ncol=3)
colnames(fixed.quant.receipts.W) <- c("mean", "lCI","uCI")
rownames(fixed.quant.receipts.W) <- "Fixed (Average) Effect"

city.quants.receipts.W <- as.data.frame(rbind(exp(city.quants.receipts.W),fixed.quant.receipts.W))
city.quants.receipts.W$wCI <- with(city.quants.receipts.W, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################WATER OUTLAYS#####################
##########################################################
g <- ggplot(outlays.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, outlays.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(outlays.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(outlays.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), outlays.W, control=lmerControl(optimizer="nloptwrap2"))
summary(ml.model)
fixef(ml.model)
fixed.quant.outlays.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=outlays.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.005    0.031        0
# 2 investment 0.005    0.031        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.029    0.072    0.004
# 2 investment 0.029    0.072    0.004
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.005281511 0.8659732

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.outlays.W <- city.quants

fixed.quant.outlays.W <- matrix(fixed.quant.outlays.W, byrow = T, ncol=3)
colnames(fixed.quant.outlays.W) <- c("mean", "lCI","uCI")
rownames(fixed.quant.outlays.W) <- "Fixed (Average) Effect"

city.quants.outlays.W <- as.data.frame(rbind(exp(city.quants.outlays.W),fixed.quant.outlays.W))
city.quants.outlays.W$wCI <- with(city.quants.outlays.W, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################SEWER OUTLAYS#####################
##########################################################
g <- ggplot(outlays.S, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, outlays.S)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(outlays.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(outlays.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), outlays.S)
summary(ml.model)
fixef(ml.model)
fixed.quant.outlays.S <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=outlays.S)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.007    0.035        0
# 2 investment 0.007    0.035        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.038    0.087    0.008
# 2 investment 0.038    0.087    0.008
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.00652669 0.8790309

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.outlays.S <- city.quants

fixed.quant.outlays.S <- matrix(fixed.quant.outlays.S, byrow = T, ncol=3)
colnames(fixed.quant.outlays.S) <- c("mean", "lCI","uCI")
rownames(fixed.quant.outlays.S) <- "Fixed (Average) Effect"

city.quants.outlays.S <- as.data.frame(rbind(exp(city.quants.outlays.S),fixed.quant.outlays.S))
city.quants.outlays.S$wCI <- with(city.quants.outlays.S, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################WATER VALUE#####################
##########################################################
g <- ggplot(value.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, value.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(value.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(value.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), value.W, control=lmerControl(optimizer="nloptwrap2"))
summary(ml.model)
fixef(ml.model)
fixed.quant.value.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=value.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect Rsq upper.CL lower.CL
# 1      Model   0    0.014        0
# 2 investment   0    0.014        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.002     0.02        0
# 2 investment 0.002     0.02        0
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.0002153283 0.9053739


AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.value.W <- city.quants

fixed.quant.value.W <- matrix(fixed.quant.value.W, byrow = T, ncol=3)
colnames(fixed.quant.value.W) <- c("mean", "lCI","uCI")
rownames(fixed.quant.value.W) <- "Fixed (Average) Effect"

city.quants.value.W <- as.data.frame(rbind(exp(city.quants.value.W),fixed.quant.value.W))
city.quants.value.W$wCI <- with(city.quants.value.W, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################WATER DEBT#####################
##########################################################
g <- ggplot(debt.W, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, debt.W)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(debt.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(debt.W, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), debt.W)
summary(ml.model)
fixef(ml.model)
fixed.quant.debt.W <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=debt.W)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.014     0.05        0
# 2 investment 0.014     0.05        0
r2beta(ml.model, method = "sgv")
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.125    0.197    0.066
# 2 investment 0.125    0.197    0.066
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.01366931 0.9344588


AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.debt.W <- city.quants

fixed.quant.debt.W <- matrix(fixed.quant.debt.W, byrow = T, ncol=3)
colnames(fixed.quant.debt.W) <- c("mean", "lCI","uCI")
rownames(fixed.quant.debt.W) <- "Fixed (Average) Effect"

city.quants.debt.W <- as.data.frame(rbind(exp(city.quants.debt.W),fixed.quant.debt.W))
city.quants.debt.W$wCI <- with(city.quants.debt.W, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))

##########################################################
#######################SEWER DEBT#####################
##########################################################
g <- ggplot(debt.S, aes(x = investment, y = log(B.lt)))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(0,6)+
  theme_classic()
g

############################
#########pooled models#######
############################
pooled.model <- lm(log(B.lt) ~ investment, debt.S)
summary(pooled.model)
exp(c(coef(pooled.model)[2],confint(pooled.model, 'investment')))


g <- ggplot(debt.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  theme_classic()
g

g <- ggplot(debt.S, aes(x = investment, y = log(B.lt), group = City))+
  geom_point(alpha = 0.3, size = 3)+
  geom_smooth(method = "lm", se = FALSE, col="darkgrey")+
  facet_wrap(~City) +
  theme_classic()
g


############################
#####hierarchical model#####
############################
ml.model <- lmer(log(B.lt) ~ investment + (1 + investment | City), debt.S)
summary(ml.model)
fixef(ml.model)
fixed.quant.debt.S <- exp(c(fixef(ml.model)[2],confint(ml.model, 'investment')))
ranef(ml.model)

ranef(ml.model)$City+fixef(ml.model)[2]

dotplot(ranef(ml.model, condVar=TRUE))$City
#ggCaterpillar(ranef(ml.model, condVar=TRUE), QQ=FALSE)
#qqmath(ranef(ml.model, condVar = TRUE), strip = FALSE)$City

ICC(outcome="B.lt", group="City", data=debt.S)
# [1] 0.5768497
r2beta(ml.model, method = "nsj", partial = T)
# Effect   Rsq upper.CL lower.CL
# 1      Model 0.275    0.353    0.201
# 2 investment 0.275    0.353    0.201
r2beta(ml.model, method = "sgv")
# Effect  Rsq upper.CL lower.CL
# 1      Model 0.91    0.923    0.896
# 2 investment 0.91    0.923    0.896
r.squaredGLMM(ml.model)
# R2m       R2c
# [1,] 0.0375024 0.9311553

AIC(ml.model)
AIC(pooled.model)

reEx <- REsim(ml.model)
reEx2 <- REextract(ml.model)

fixeff <- FEsim(ml.model, n.sims = 10000)
fix.dist <- rnorm(10000,fixeff$mean[2], fixeff$sd[2])

city.quants <- matrix(NA,ncol=3, nrow=16)
for (i in 1:16) {
  rand.dist <- rnorm(10000,reEx2$investment[i],reEx2$investment_se[i])
  city.eff <- rand.dist+fix.dist
  CI <- quantile(city.eff,probs = c(.5,.025,.975))
  city.quants[i,] <- CI
}

rownames(city.quants) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                           "Milwaukee", "Nashville", "New Orleans","New York", 
                           "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                           "San Francisco", "Toledo","Washington, D.C.")
colnames(city.quants) <- c("mean", "lCI","uCI")

exp(city.quants)
1-exp(city.quants)

city.quants.debt.S <- city.quants

fixed.quant.debt.S <- matrix(fixed.quant.debt.S, byrow = T, ncol=3)
colnames(fixed.quant.debt.S) <- c("mean", "lCI","uCI")
rownames(fixed.quant.debt.S) <- "Fixed (Average) Effect"

city.quants.debt.S <- as.data.frame(rbind(exp(city.quants.debt.S),fixed.quant.debt.S))
city.quants.debt.S$wCI <- with(city.quants.debt.S, sprintf("%.2f (%.2f-%.2f)",mean,lCI,uCI))


all.est <- cbind(city.quants.receipts.W$wCI,city.quants.exp.W$wCI,city.quants.exp.S$wCI,city.quants.outlays.W$wCI,
                 city.quants.outlays.S$wCI,city.quants.value.W$wCI,city.quants.debt.W$wCI,city.quants.debt.S$wCI,
                 city.quants.water$wCI,city.quants.sewer$wCI)

colnames(all.est) <- c("Water Supply Receipts","Water Supply Expenses","Sewer System Expenses",
                       "Water Supply Outlays","Sewer System Outlays","Value of the Water Supply",
                       "Funded Debt of Water Supply","Funded Debt of Sewer System",
                       "Cumulative Investment in Water Supply","Cumulative Investment in Sewer System")
rownames(all.est) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                       "Milwaukee", "Nashville", "New Orleans","New York", 
                       "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
                       "San Francisco", "Toledo","Washington, D.C.","Average (Fixed) Effects")

write.csv(all.est, "~/Desktop/Typhoid/Hierarchical Regression Estimates 2.csv")
