library(corrplot)
library(dplyr)
library(lme4)
library(ggimage)
library(jpeg)
library(png)
library(grid)
library(sjstats)

#data from https://www.advancedsportsanalytics.com/nfl-raw-data
nfl <- read.csv("/Users/coleyant/Downloads/nfl_pass_rush_receive_raw_data.csv")
qb <- subset(nfl, nfl$pos=='QB')

qb <- subset(qb, select=c('player', 'team', 'pass_cmp', 'pass_att', 'pass_yds', 'pass_td', 'pass_int', 'pass_rating', 'rush_yds', 'rush_td', 'fumbles_lost', 'vis_team', 'home_team', 'vis_score', 'home_score', 'off_pct'))

for(i in 1:2102){ #assigns variables for point differential, points scored for each game
  if(qb[i,'team'] == qb[i,'vis_team']){
    team_score = qb[i,'vis_score']
    team_diff = qb[i,'vis_score'] - qb[i,'home_score']
    qb[i, 'team_score'] =  team_score
    qb[i, 'team_diff'] =  team_diff
  }
  else{
    team_score = qb[i,'home_score']
    team_diff = qb[i,'home_score'] - qb[i,'vis_score']
    qb[i, 'team_score'] =  team_score
    qb[i, 'team_diff'] =  team_diff
  }
}

for(i in 1:2102){ #assigns variable representing whether team won
  if(qb[i,'team_diff'] == 0){
    qb[i, 'result'] = 0
  }
  else if(qb[i,'team_diff'] > 0){
    qb[i, 'result'] = 1
  }
  else if(qb[i,'team_diff'] < 0){
    qb[i, 'result'] = -1
  }
}

prelim_qb <- subset(qb, qb$off_pct > 90) #removes games where QB played less than 90% of the snaps
prelim_qb <- subset(prelim_qb, prelim_qb$result != 0)
prelim_qb <- prelim_qb %>% #Converts result to a binary variable (0=loss, 1=win, tie removed)
  mutate(result = ifelse(result == -1,0,1))

players <- names(table(prelim_qb$player))
games <- as.numeric(table(prelim_qb$player))
qb_list = numeric(length=sum(games>30)) #creates empty vector to be populated wtih QBs who played  at least 30 games
x=1
for(i in 1:length(players)){ #populates list of QBs with more than 30 games played
  if(games[i] >30){
    qb_list[x] = players[i]
    x = x+1
  }
  else{
    NA
  }
}

prelim_qb <- subset(prelim_qb, player %in% qb_list) #subsets df down to only the QBs we want

#Exploratory analyses
par(mfrow = c(3, 4))
boxplot(prelim_qb[, c(3, 4)], names=c('Pass  Completions', 'Pass Attempts'))
boxplot(prelim_qb[, 5], xlab='Pass Yards')
hist(prelim_qb[, c(6)], xlab='Passing TDs')
hist(prelim_qb[, c(10)], xlab='Rush TDs')
hist(prelim_qb[, c(7)], xlab='INTs')
hist(prelim_qb[,c(11)], xlab='Fumbles')
boxplot(prelim_qb[, c(8)], xlab='Passer Rating')
boxplot(prelim_qb[, c(9)], xlab='Rush Yards')
boxplot(prelim_qb[, c(16)], xlab='Team Points')
hist(prelim_qb[, c(18)], xlab='Win')

par(mfrow=c(1,1))
pairs(prelim_qb[,c(3, 4, 5, 6, 7, 8, 9, 10, 11, 16, 18)], pch=1)
plot(prelim_qb[,7],prelim_qb[,8], type='p')

qb_cors <- cor(prelim_qb[,c(3, 4, 5, 6, 7, 8, 9, 10, 11, 17, 19)])
corrplot(qb_cors)

#will move forward exploring result vs. yds/attempt, int/attempt, td/attempt, cmp/att, rush_td,  fumbles lost at level 1. Level 2 will consist of these variables aggregated on QB.
#attempting to model win %s from box score stats

test <- prelim_qb #creating level 1 rate variables
test['cmp_att'] = test['pass_cmp'] / test['pass_att']
test['yds_att'] = test['pass_yds'] / test['pass_att']
test['tds_att'] = test['pass_td'] / test['pass_att']
test['int_att'] = test['pass_int'] / test['pass_att']

test_cors <- cor(test[,c(20, 21, 22, 23, 19)])
corrplot(test_cors)

qb_rates <- test %>% select(1, 20, 21, 22, 23, 19)
qb_aggs <- qb_rates %>% group_by(player) %>% #compiles averages for each player
  summarize_all(list(name=mean)) #level 2 rate averages
colnames(qb_aggs) = c('player', 'cmp_att_avg', 'yds_att_avg', 'tds_att_avg', 'int_att_avg', 'win_pct')

avgs <- qb_rates[-1] %>% summarize_all(mean)
sds <- qb_rates[-1] %>% summarize_all(sd)

qb_rates_c <- qb_rates #centered data
qb_rates_c[,2] = (qb_rates[,2] - avgs[1,1])
qb_rates_c[,3] = (qb_rates[,3] - avgs[1,2])
qb_rates_c[,4] = (qb_rates[,4] - avgs[1,3])
qb_rates_c[,5] = (qb_rates[,5] - avgs[1,4])

qb_aggs_c <- qb_aggs
qb_aggs_c[,2] = (qb_aggs[,2] - avgs[1,1])
qb_aggs_c[,3] = (qb_aggs[,3] - avgs[1,2])
qb_aggs_c[,4] = (qb_aggs[,4] - avgs[1,3])
qb_aggs_c[,5] = (qb_aggs[,5] - avgs[1,4])


qb_rates_s <- qb_rates #standardized data
qb_rates_s[,2] = (qb_rates[,2] - avgs[1,1])/sds[1,1]
qb_rates_s[,3] = (qb_rates[,3] - avgs[1,2])/sds[1,2]
qb_rates_s[,4] = (qb_rates[,4] - avgs[1,3]) /sds[1,3]
qb_rates_s[,5] = (qb_rates[,5] - avgs[1,4]) /sds[1,4]

qb_aggs_s <- qb_aggs
qb_aggs_s[,2] = (qb_aggs[,2] - avgs[1,1])/sds[1,1]
qb_aggs_s[,3] = (qb_aggs[,3] - avgs[1,2])/sds[1,2]
qb_aggs_s[,4] = (qb_aggs[,4] - avgs[1,3]) /sds[1,3]
qb_aggs_s[,5] = (qb_aggs[,5] - avgs[1,4]) /sds[1,4]


plot(qb_aggs$win_pct, xlab = 'Player', ylab='Win Percentage')
text(c(1:7), qb_aggs$win_pct[1:7], qb_list[1:7], pos=4)
text(8, qb_aggs$win_pct[8], qb_list[8], pos=1)
text(c(9:17), qb_aggs$win_pct[9:17], qb_list[9:17], pos=4)
text(c(18:22), qb_aggs$win_pct[18:22], qb_list[18:22], pos=2)

qb_rates$player = factor(qb_rates_c$player)
qb_final <- merge(qb_rates, qb_aggs)

qb_rates_c$player = factor(qb_rates_c$player)
qb_rates_s$player = factor(qb_rates_s$player)
qb_final_c <- merge(qb_rates_c, qb_aggs_c)
qb_final_s <- merge(qb_rates_s, qb_aggs_s)

model1 <- glm(result~player, data=qb_final_s) #anova
summary(model1)

model2 <- glmer(result~1+(1|player), data = qb_final_s, family = 'binomial') #random intercepts
summary(model2)

model3 <- glmer(result~(1|player) + yds_att + tds_att + int_att, data = qb_final_s, family = 'binomial') #add lvl 1 fixed effects
anova(model2, model3)
summary(model3)

model4 <- glmer(result~ (1|player) + yds_att_avg + tds_att_avg + yds_att + tds_att + int_att, data = qb_final_s, family = 'binomial') #add lvl 2 effects
anova(model3, model4)
summary(model4)

model5 <- glmer(result~ (1+int_att|player) +yds_att_avg + tds_att_avg + yds_att + tds_att + int_att, data = qb_final_s, family = 'binomial') #add random slopes
anova(model5, model4)
summary(model5)

model6 <- glmer(result~ yds_att*int_att_avg+(1+int_att|player) +yds_att_avg + tds_att_avg + yds_att + tds_att + int_att, data = qb_final_s, family = 'binomial') #add cross-level interaction
anova(model5, model6)
#Figure 1
summary(model6)

slopes= ranef(model6)$player[,2] #slope coeffcients for each player
intercepts = ranef(model6)$player[,1] #intercept coeffcients for each player

sjstats::icc(model6) # ICC

qb_aggs['img'] = c('rodgers.png', 'baker.png', 'bigben.png', 'wentz.png',
                   'dak.png', 'dannydimes.png', 'carr.png', 'watson.png',
                   'goff.png', 'jimmy.png', 'allen.png', 'herbert.png',
                   'kirk.png', 'kyler.png', 'lamar.png', 'ryan.png',
                   'stafford.png', 'mahomes.png', 'wilson.png', 'tannehill.png',
                   'darnold.png', 'brady.png')
qb_aggs['rk'] = seq(1, 22)

nflimg <- readJPEG("/users/coleyant/Desktop/downloads/nfl_qbs/nfl.jpg")

#Figure 2
ggplot(data = qb_aggs, aes(x = rk, y = win_pct)) +
  annotation_custom(rasterGrob(nflimg, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = img), size = 0.12) +
  xlab("QB") +
  ylab("Win %")+
  ylim(0.3, .85)+
  ggtitle('Win Percentage by QB')

qb_aggs['slopes'] = slopes
qb_aggs['ints'] = intercepts

#Figure 3
ggplot(data = qb_aggs, aes(x = ints, y = slopes)) +
  annotation_custom(rasterGrob(nflimg, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = img), size = 0.12) +
  xlab("Intercepts") +
  ylab("Slopes")+
  ggtitle('Slope - Intercept comparison by QB')+
  geom_abline(intercept = .007, slope = 0.524)
  
#Figure 5
plot_model(model6, type = 'pred', terms = c('yds_att', 'int_att_avg[-0.15, 0, .15]'), title = 'Visualizing the nature of the interaction between int_att_avg and tds_att')
#Figure 4
plot_model(model6, title = 'Odds ratios & confidence intervals for final model')

