
setwd("~/Desktop/historical_typhoid")

citynames <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
               "Milwaukee", "Nashville", "New Orleans","New York", 
               "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
               "San Francisco", "Toledo","Washington, D.C.", "Median", "Water Source Type")

library(RColorBrewer)
color <- brewer.pal(4, "Set1")

#############################FIGURE 1. SEASONAL TRANSMISSION#########################

b.seas.Baltimore <- read.csv("Baltimore B_seas.csv")
b.seas.Boston <- read.csv("Boston B_seas.csv")
b.seas.Chicago <- read.csv("Chicago B_seas.csv")
b.seas.Cincinnati <- read.csv("Cincinnati B_seas.csv")
b.seas.Cleveland <- read.csv("Cleveland B_seas.csv")
b.seas.WashDC <- read.csv("Washington DC B_seas.csv")
b.seas.Milwaukee <- read.csv("Milwaukee B_seas.csv")
b.seas.Nashville <- read.csv("Nashville B_seas.csv")
b.seas.NewOrleans <- read.csv("New Orleans B_seas.csv")
b.seas.NewYork <- read.csv("New York B_seas.csv")
b.seas.Philadelphia <- read.csv("Philadelphia B_seas.csv")
b.seas.Pittsburgh <- read.csv("Pittsburgh B_seas.csv")
b.seas.Providence <- read.csv("Providence B_seas.csv")
b.seas.StLouis <- read.csv("St. Louis B_seas.csv")
b.seas.SanFran <- read.csv("San Francisco B_seas.csv")
b.seas.Toledo <- read.csv("Toledo B_seas.csv")
bseas.all <- rbind(b.seas.Baltimore[,2], b.seas.Boston[,2], b.seas.Chicago[,2], b.seas.Cincinnati[,2],
                   b.seas.Cleveland[,2],b.seas.Milwaukee[,2],b.seas.Nashville[,2],b.seas.NewOrleans[,2],
                   b.seas.NewYork[,2],b.seas.Philadelphia[,2],b.seas.Pittsburgh[,2],b.seas.Providence[,2],
                   b.seas.StLouis[,2],b.seas.SanFran[,2],b.seas.Toledo[,2],b.seas.WashDC[,2])

all.means <- NULL
for (i in 1:13){
  all.means[i] <- mean(bseas.all[,i])
}
bseas.all <- rbind(bseas.all, all.means)
rownames(bseas.all) <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
                         "Milwaukee", "Nashville", "New Orleans",
                         "New York","Philadelphia", "Pittsburgh", "Providence",
                         "St. Louis", "San Francisco", "Toledo", "Washington, D.C.", "mean")

month <- 1:13

reservoir.seas <- NULL
for (i in 1:13) {
  reservoir.seas[i] <- mean(c(b.seas.Baltimore[i,2], b.seas.Boston[i,2], 
                              b.seas.NewYork[i,2], b.seas.SanFran[i,2]))
}


greatlake.seas <- NULL
for (i in 1:13){
  greatlake.seas[i] <- mean(c(b.seas.Chicago[i,2], b.seas.Cleveland[i,2], 
                              b.seas.Milwaukee[i,2]))
}

river.seas <- NULL
for (i in 1:13){
  river.seas[i] <- mean(c(b.seas.Cincinnati[i,2], b.seas.WashDC[i,2], 
                          b.seas.Nashville[i,2], b.seas.NewOrleans[i,2], 
                          b.seas.Philadelphia[i,2], b.seas.Pittsburgh[i,2],
                          b.seas.Providence[i,2], b.seas.StLouis[i,2],
                          b.seas.Toledo[i,2]))
}


b.seas.list <- list(b.seas.Baltimore[,2],b.seas.Boston[,2],b.seas.Chicago[,2],
                    b.seas.Cincinnati[,2],b.seas.Cleveland[,2],b.seas.Milwaukee[,2], 
                    b.seas.Nashville[,2],b.seas.NewOrleans[,2],b.seas.NewYork[,2],
                    b.seas.Philadelphia[,2],b.seas.Pittsburgh[,2],b.seas.Providence[,2],
                    b.seas.StLouis[,2],b.seas.SanFran[,2],b.seas.Toledo[,2],
                    b.seas.WashDC[,2],bseas.all[17,],rep(NA,13))

citynames <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
               "Milwaukee", "Nashville", "New Orleans","New York", 
               "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
               "San Francisco", "Toledo","Washington, D.C.", "Mean", "Water Source Type")


color <- c("blue", "purple", "seagreen")
v1 <- 1:13
v2 <- c("1","","","","5","","","","9","","","","13")
par(mfrow = c(3, 6))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(mgp = c(2, 0.6, 0))

plot(1:13, b.seas.list[[1]], axes = FALSE, col=color[1], type="l", ylim=c(0,3))
axis(2, col = "grey40", at = seq(0,3, 1))
box(col = "grey60")
mtext(citynames[1], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[2]], axes = FALSE, col=color[1], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[2], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[3]], axes = FALSE, col=color[2], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[3], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[4]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[4], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[5]], axes = FALSE, col=color[2], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[5], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[6]], axes = FALSE, col=color[2], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[6], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[7]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
axis(2, col = "grey40", at = seq(0,3, 1))
box(col = "grey60")
mtext(citynames[7], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[8]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[8], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[9]], axes = FALSE, col=color[1], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[9], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[10]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[10], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[11]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[11], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[12]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
box(col = "grey60")
mtext(citynames[12], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[13]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
axis(1, col = "grey40", at = v1, labels = v2)
axis(2, col = "grey40", at = seq(0,3, 1))
box(col = "grey60")
mtext(citynames[13], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[14]], axes = FALSE, col=color[1], type="l", ylim=c(0,3))
axis(1, col = "grey40", at = v1, labels = v2)
box(col = "grey60")
mtext(citynames[14], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[15]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
axis(1, col = "grey40", at = v1, labels = v2)
box(col = "grey60")
mtext(citynames[15], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[16]], axes = FALSE, col=color[3], type="l", ylim=c(0,3))
axis(1, col = "grey40", at = v1, labels = v2)
box(col = "grey60")
mtext(citynames[16], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[17]], axes = FALSE, type="l", ylim=c(0,3), lwd=3)
axis(1, col = "grey40", at = v1, labels = v2)
box(col = "grey60")
mtext(citynames[17], side = 3, line = -1, adj = 0.1, cex = 0.6)
plot(1:13, b.seas.list[[18]], axes = FALSE, ylim=c(0,3))
lines(1:13, reservoir.seas, col=color[1], lwd=3)
lines(1:13, greatlake.seas, col=color[2], lwd=3)
lines(1:13, river.seas, col=color[3], lwd=3)
axis(1, col = "grey40", at = v1, labels = v2)
box(col = "grey60")
mtext(citynames[18], side = 3, line = -1, adj = 0.1, cex = 0.6)

mtext("(4-Week) Month", side = 1, outer = TRUE, cex = 0.7, line = 2.2)
mtext(expression(paste("Seasonal Transmission (", beta ["seas"], ")")), side = 2, outer = TRUE, cex = 0.7, line = 2.2)

dev.copy(png,'Figure 1. Seasonal transmission ALL.png')
dev.off()

############################FIGURE 2. LONG TERM WITH WATER TREATMENT##########################

b.ltALL <- read.csv("final B_lts.csv")[,-1]

b.lt.Baltimore.yr <- b.ltALL[,1]
b.lt.Boston.yr <- b.ltALL[,2]
b.lt.Chicago.yr <- b.ltALL[,3]
b.lt.Cincinnati.yr <- b.ltALL[,4]
b.lt.Cleveland.yr <- b.ltALL[,5]
b.lt.Milwaukee.yr <- b.ltALL[,6]
b.lt.Nashville.yr <- b.ltALL[,7]
b.lt.NewOrleans.yr <- b.ltALL[,8]
b.lt.NewYork.yr <- b.ltALL[,9]
b.lt.Philadelphia.yr <- b.ltALL[,10]
b.lt.Pittsburgh.yr <- b.ltALL[,11]
b.lt.Providence.yr <- b.ltALL[,12]
b.lt.SanFran.yr <- b.ltALL[,14]
b.lt.StLouis.yr <- b.ltALL[,13]
b.lt.Toledo.yr <- b.ltALL[,15]
b.lt.WashDC.yr <- b.ltALL[,16]

b.lt.list <- cbind(b.lt.Baltimore.yr,b.lt.Boston.yr,b.lt.Chicago.yr,b.lt.Cincinnati.yr,
                   b.lt.Cleveland.yr,b.lt.Milwaukee.yr,b.lt.Nashville.yr,b.lt.NewOrleans.yr,
                   b.lt.NewYork.yr,b.lt.Philadelphia.yr,b.lt.Pittsburgh.yr,b.lt.Providence.yr,
                   b.lt.StLouis.yr,b.lt.SanFran.yr,b.lt.Toledo.yr,b.lt.WashDC.yr)

dates    <- NULL
dates[1] <- 1889 
for (i in 2:558) {
  dates[i] <- dates[i-1] + 1/13
}

plot.new()
par(mfrow = c(4, 4))
par(oma = c(4, 3, 0.5, 0.5))
par(mar = c(1, 1, 0, 0))

for (i in 1:12) {
  plot(dates, b.lt.list[,i], type="l", bty="n", 
       xlab = '', ylab = '', xaxt = 'n', ylim=c(0, max(b.lt.list[,i])))
  mtext(citynames[i], side = 3, line = -1, adj = 0.8, cex=0.8)
}

for (i in 13:16) {
  plot(dates, b.lt.list[,i], type="l", bty="n", 
       xlab = '', ylab = '', ylim=c(0, max(b.lt.list[,i])))
  mtext(citynames[i], side = 3, line = -1, adj = 0.8, cex=0.8)
}

mtext('Date', side = 1, outer = TRUE, line = 2.2, cex=.9)
mtext(expression(paste("Long-Term Transmission (", beta ["lt"], ")")), side = 2, outer = TRUE, line = 1.5, cex=.9)

dev.copy(png,'Figure 2. Long term transmission ALL.png')
dev.off()


