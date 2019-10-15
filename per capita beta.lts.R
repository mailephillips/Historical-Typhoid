setwd("~/Desktop/historical_typhoid")

citynames <- c("Baltimore", "Boston", "Chicago", "Cincinnati", "Cleveland",
               "Milwaukee", "Nashville", "New Orleans","New York", 
               "Philadelphia", "Pittsburgh", "Providence", "Saint Louis",
               "San Francisco", "Toledo","Washington, D.C.")


B.Baltimore <- read.csv("Baltimore Beta_lt.csv")
B.Boston <- read.csv("Boston Beta_lt.csv")
B.Chicago <- read.csv("Chicago Beta_lt.csv")
B.Cincinnati <- read.csv("cincinnati Beta_lt.csv")
B.Cleveland <- read.csv("Cleveland Beta_lt.csv")
B.Milwaukee <- read.csv("Milwaukee Beta_lt.csv")
B.Nashville <- read.csv("Nashville Beta_lt.csv")
B.NewOrleans <- read.csv("New Orleans Beta_lt.csv")
B.NewYork <- read.csv("New York Beta_lt.csv")
B.Philadelphia <- read.csv("Philadelphia Beta_lt.csv")
B.Pittsburgh <- read.csv("Pittsburgh Beta_lt.csv")
B.Providence <- read.csv("Providence Beta_lt.csv")
B.StLouis <- read.csv("St. Louis Beta_lt.csv")
B.SanFrancisco <- read.csv("San Francisco Beta_lt.csv")
B.Toledo <- read.csv("Toledo Beta_lt.csv")
B.WashDC <- read.csv("Washington DC Beta_lt.csv")

pop.Baltimore <- read.csv("monthly pops/monthlypop_baltimore.csv")[-559,]
pop.Boston <- read.csv("monthly pops/monthlypop_boston.csv")[-559,]
pop.Chicago <- read.csv("monthly pops/monthlypop_Chicago.csv")[-559,]
pop.Cincinnati <- read.csv("monthly pops/monthlypop_Cincinnati.csv")[-559,]
pop.Cleveland <- read.csv("monthly pops/monthlypop_Cleveland.csv")[-559,]
pop.Milwaukee <- read.csv("monthly pops/monthlypop_Milwaukee.csv")[-559,]
pop.Nashville <- read.csv("monthly pops/monthlypop_Nashville.csv")[-559,]
pop.NewOrleans <- read.csv("monthly pops/monthlypop_NewOrleans.csv")[-559,]
pop.NewYork <- read.csv("monthly pops/monthlypop_ny.csv")[-559,]
pop.Philadelphia <- read.csv("monthly pops/monthlypop_Philadelphia.csv")[-559,]
pop.Pittsburgh <- read.csv("monthly pops/monthlypop_Pittsburgh.csv")[-559,]
pop.Providence <- read.csv("monthly pops/monthlypop_Providence.csv")[-559,]
pop.StLouis <- read.csv("monthly pops/monthlypop_StLouis.csv")[-559,]
pop.SanFrancisco <- read.csv("monthly pops/monthlypop_SanFrancisco.csv")[-559,]
pop.Toledo <- read.csv("monthly pops/monthlypop_Toledo.csv")[-559,]
pop.WashDC <- read.csv("monthly pops/monthlypop_WashDC.csv")[-559,]


B.Baltimore_adj    <- NULL
B.Boston_adj       <- NULL
B.Chicago_adj      <- NULL
B.Cincinnati_adj   <- NULL
B.Cleveland_adj    <- NULL
B.Milwaukee_adj    <- NULL
B.Nashville_adj    <- NULL
B.NewOrleans_adj   <- NULL
B.NewYork_adj      <- NULL
B.Philadelphia_adj <- NULL
B.Pittsburgh_adj   <- NULL
B.Providence_adj   <- NULL
B.StLouis_adj      <- NULL
B.SanFrancisco_adj <- NULL
B.Toledo_adj       <- NULL
B.WashDC_adj       <- NULL
for (i in 1:558){
  B.Baltimore_adj[i] <- B.Baltimore[i,2]*pop.Baltimore[i,2]
  B.Boston_adj[i] <- B.Boston[i,2]*pop.Boston[i,2]
  B.Chicago_adj[i] <- B.Chicago[i,2]*pop.Chicago[i,2]
  B.Cincinnati_adj[i] <- B.Cincinnati[i,2]*pop.Cincinnati[i,2]
  B.Cleveland_adj[i] <- B.Cleveland[i,2]*pop.Cleveland[i,2]
  B.Milwaukee_adj[i] <- B.Milwaukee[i,2]*pop.Milwaukee[i,2]
  B.Nashville_adj[i] <- B.Nashville[i,2]*pop.Nashville[i,2]
  B.NewOrleans_adj[i] <- B.NewOrleans[i,2]*pop.NewOrleans[i,2]
  B.NewYork_adj[i] <- B.NewYork[i,2]*pop.NewYork[i,2]
  B.Philadelphia_adj[i] <- B.Philadelphia[i,2]*pop.Philadelphia[i,2]
  B.Pittsburgh_adj[i] <- B.Pittsburgh[i,2]*pop.Pittsburgh[i,2]
  B.Providence_adj[i] <- B.Providence[i,2]*pop.Providence[i,2]
  B.StLouis_adj[i] <- B.StLouis[i,2]*pop.StLouis[i,2]
  B.SanFrancisco_adj[i] <- B.SanFrancisco[i,2]*pop.SanFrancisco[i,2]
  B.Toledo_adj[i] <- B.Toledo[i,2]*pop.Toledo[i,2]
  B.WashDC_adj[i] <- B.WashDC[i,2]*pop.WashDC[i,2]
}

B.lts <- cbind(B.Baltimore_adj,
               B.Boston_adj,
               B.Chicago_adj,
               B.Cincinnati_adj,
               B.Cleveland_adj,
               B.Milwaukee_adj,
               B.Nashville_adj,
               B.NewOrleans_adj,
               B.NewYork_adj,
               B.Philadelphia_adj,
               B.Pittsburgh_adj,
               B.Providence_adj,
               B.StLouis_adj,
               B.SanFrancisco_adj,
               B.Toledo_adj,
               B.WashDC_adj)


par(mfrow = c(4, 4))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(mgp = c(2, 0.6, 0))
for (i in 1:16) {
  plot(seq(1889,1930,length.out = 558), B.lts[,i], xaxt = 'n', type="l", ylim=c(0,max(B.lts[,i])))
  mtext(citynames[i], side = 3, line = -1, adj = 0.1, cex = 0.6)
  
  if (i %in% c(13, 14,15,16))
    axis(1, col = "grey40", at = 1889:1930)
  if (i %in% c(1, 5, 9, 13))
    axis(2, col = "grey40")
  box(col = "grey60")
}
mtext("Year", side = 1, outer = TRUE, cex = 0.7, line = 2.2)
mtext("Long Term Transmission Rate", side = 2, outer = TRUE, cex = 0.7, line = 2.2)


write.csv(B.lts, "final B_lts.csv")
