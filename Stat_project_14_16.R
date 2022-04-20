library(psych)


#read data
library(xlsx)
nba_data <- read.xlsx("Desktop/RIT_Class/2021 Spring/Applied_Linear_Model_Regression/project/nba_data_final copy.xlsx",sheetName = "A_2014_2017_1_2_final",1)

#part_1 build up the model
#fill the na value
nba_data[is.na(nba_data)] <- 0

x <- nba_data[,-1] 
y <- nba_data[,1]

#correlation matrix 
pairs.panels(x, method = "pearson", hist.col = "#00AFBB",
             density = TRUE,  ellipses = TRUE)


#subset of all groups
nba.all <- regsubsets(salary ~., data=nba_data, method="exhaustive")
summary(nba.all)

#selection
nba.null <- lm(salary ~ 1, data=nba_data)
nba.full <- lm(salary ~ ., data=nba_data)

step(nba.null, scope=list(upper=nba.full, lower=nba.null), direction="forward", trace=0)

#build the model (salary ~ pts_per_game + g + trb_per_game + ast_per_game + stl_per_game + tov_per_game + blk_per_game)
nba.lm <- lm(salary ~ pts_per_game + g + trb_per_game + ast_per_game + stl_per_game + tov_per_game + blk_per_game, data = nba_data)
summary(nba.lm)


#Calculate fitted values from a regression of absolute residuals vs fitted values.
#Fit a WLS model using weights = 1/(fitted values)2.
wts <- 1/fitted(lm(abs(residuals(nba.lm)) ~ pts_per_game + g + trb_per_game + ast_per_game + stl_per_game + tov_per_game + blk_per_game, data=nba_data))^2

# weighted least squares model 
model.nba <- lm(salary ~ pts_per_game + g + trb_per_game + ast_per_game + stl_per_game + tov_per_game + blk_per_game, data=nba_data, weights=wts)
summary(model.nba)

#plot residual and qq plot to test norm
res <- resid(nba.lm)
y_hat <- nba.lm$fit

#residual ~ y_hat plot
plot(res ~ y_hat)
abline(h=0, col="blue")

#normal probability plot
rstandard_normal <- rstandard(nba.lm)
qqnorm(rstandard_normal)
abline(a = 0, b = 1 , col='red')

#After plotting the residual ~ y_hat, I found the residual plot being funnel shaped. Try to transform the y.
y_log = log(y)
y_sqrt = sqrt(y)
y_square = y^2

#rebuild the selection process
#log---best choice
nba.null_log <- lm(y_log ~ 1, data = x)
nba.full_log <- lm(y_log ~ ., data = x)
step(nba.null_log, scope=list(upper=nba.full_log, lower=nba.null_log), direction="forward", trace=0)
#Result
# y_log ~ pts_per_game + trb_per_game + g + ast_per_game + tov_per_game + stl_per_game + blk_per_game
#fit the model
nba.lm_log <- lm(y_log ~  pts_per_game + trb_per_game + g + ast_per_game + tov_per_game +stl_per_game + blk_per_game, data = nba_data)
summary(nba.lm_log)

res_log <- resid(nba.lm_log)
y_hat_log <- nba.lm_log$fit
plot(res_log ~ y_hat_log)
abline(h=0, col="blue")

#normal probability plot
rstandard_log <- rstandard(nba.lm_log)
qqnorm(rstandard_log)
abline(a = 0, b = 1 , col='red')

#sqrt
nba.null_sqrt <- lm(y_sqrt ~ 1, data = x)
nba.full_sqrt <- lm(y_sqrt ~ ., data = x)
step(nba.null_sqrt, scope=list(upper=nba.full_sqrt, lower=nba.null_sqrt), direction="forward", trace=0)
#Result
# y_log ~ pts_per_game + trb_per_game + g + ast_per_game + tov_per_game + stl_per_game + blk_per_game
nba.lm_sqrt <- lm(y_sqrt ~ pts_per_game + trb_per_game + g + ast_per_game + tov_per_game + stl_per_game + blk_per_game, data = nba_data)
summary(nba.lm_sqrt)

res_sqrt <- resid(nba.lm_sqrt)
y_hat_sqrt <- nba.lm_sqrt$fit
plot(res_sqrt ~ y_hat_sqrt)
abline(h=0, col="blue")

#normal probability plot
rstandard_sqrt <- rstandard(nba.lm_sqrt)
qqnorm(rstandard_sqrt)
abline(a = 0, b = 1 , col='red')



#Square
nba.null_square <- lm(y_square ~ 1, data = x)
nba.full_square <- lm(y_square ~ ., data = x)
step(nba.null_square, scope=list(upper=nba.full_square, lower=nba.null_square), direction="forward", trace=0)
#Result
# y_sqrt ~ pts_per_game + trb_per_game + g + ast_per_game + tov_per_game + stl_per_game + blk_per_game
#fit the model
nba.lm_square <- lm(y_square ~ pts_per_game + trb_per_game + g + ast_per_game + tov_per_game + stl_per_game + blk_per_game, data = nba_data)
summary(nba.lm_square)

res_square <- resid(nba.lm_square)
y_hat_square <- nba.lm_square$fit
plot(res_square ~ y_hat_square)
abline(h=0, col="blue")

#normal probability plot
rstandard_square <- rstandard(nba.lm_square)
qqnorm(rstandard_square)
abline(a = 0, b = 1 , col='red')


#plot all the figures
dev.new()
par(mfrow=c(2,2))
plot(nba.lm, which=1)
plot(nba.lm_log, which=1)
plot(nba.lm_sqrt, which=1)
plot(nba.lm_square, which=1)

dev.new()
par(mfrow=c(2,2))
qqnorm(rstandard_normal)
abline(a = 0, b = 1 , col='red')
qqnorm(rstandard_log)
abline(a = 0, b = 1 , col='red')
qqnorm(rstandard_sqrt)
abline(a = 0, b = 1 , col='red')
qqnorm(rstandard_square)
abline(a = 0, b = 1 , col='red')


#part_2
dff <- dffits(nba.lm_log)
dff[abs(dff) > 0.1765]
cook <- cooks.distance(nba.lm_log)
cook[cook >1]
hat <- hatvalues(nba.lm_log)
hat[hat > 0.01558]




