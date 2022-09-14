garment = read.table("garments_worker_productivity.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)
View(garment)
nrow(garment)
ncol(garment)
is.na(garment)
sum(is.na(garment))
colSums(is.na(garment))
newdataset <- na.omit(garment)
View(newdataset)
summary(newdataset$actual_productivity)
var(newdataset$actual_productivity)
sd(newdataset$actual_productivity) 
cor.test(newdataset$actual_productivity, newdataset$no_of_workers)

## correlation between productivity and number of changes in the style of a particular product 
cor.test(newdataset$actual_productivity, newdataset$no_of_style_change)

## correlation between productivity and targeted productivity
cor.test(newdataset$actual_productivity, newdataset$targeted_productivity)

## correlation between productivity and standard minute value
cor.test(newdataset$actual_productivity, newdataset$smv)

## correlation between productivity and work in progress
cor.test(newdataset$actual_productivity, newdataset$wip)

## correlation between productivity and over-time 
cor.test(newdataset$actual_productivity, newdataset$over_time)

## correlation between productivity and financial incentive 
cor.test(newdataset$actual_productivity, newdataset$incentive)

## correlation between productivity and the amount of time production was interrupted
cor.test(newdataset$actual_productivity, newdataset$idle_time)

## correlation between productivity and the number of workers who were idle due to production interruption
cor.test(newdataset$actual_productivity, newdataset$idle_men)
var(newdataset$team)
var(newdataset$targeted_productivity)
var(newdataset$smv)
var(newdataset$wip)
var(newdataset$over_time)
var(newdataset$incentive)
var(newdataset$idle_time)
var(newdataset$idle_men)
var(newdataset$no_of_style_change)
var(newdataset$no_of_workers)
var(newdataset$actual_productivity)

pca <- prcomp(~targeted_productivity + smv + over_time + incentive + no_of_workers + actual_productivity, data = newdataset, scale = TRUE)
pca

str(pca)
pca$sdev
loadings = pca$rotation[, 1:2]
print(loadings)

loadings = -loadings
row.names(loadings) <- c("targeted_productivity","smv","over_time","incentive","no_of_workers","actual_productivity")
colnames(loadings) <- c("PC1","PC2")
print(loadings)


plot(pca)
plot(pca, type = "line", main = "scree plot")
plot(pca$sdev, type = "line", main = "scree plot") 

summary(pca)

PVE = pca$sdev
print (PVE)
cummulative = cumsum(PVE)
print (cummulative)
plot(PVE, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     ylim =c(0,8), type = "b")
plot(cummulative, xlab="Principal Component ", 
     ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,8), type = "b")




pca$rotation=-pca$rotation
pca$x=-pca$x
biplot(pca, scale=0)


install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install_github("vqv/ggbiplot", force =TRUE)
library(ggbiplot)
ggbiplot(pca)
bplot = ggbiplot(pcobj = pca, choices = 1:2, obs.scale = 1, var.scale = 1,
                 varname.size= 5, varname.abbrev = FALSE, var.axes = TRUE, circle = TRUE, 
                 ellipse = TRUE)
print (bplot)
biplot = ggbiplot(pcobj = pca,
                  choices = c(1,2),
                  obs.scale = 1, var.scale = 1,  # Scaling of axis
                  labels = row.names(data),     # Add labels as rownames
                  labels.size = 4,
                  varname.size = 5,
                  varname.abbrev = TRUE,  # Abbreviate variable names (TRUE)
                  var.axes = TRUE,      # Remove variable vectors (TRUE)
                  circle = TRUE,        # Add unit variance circle (TRUE)
                  ellipse = TRUE) # Adding ellipses
print(biplot)
summary(pca)

plot(pca)
plot(pca, type = "line", main = "Scree plot") 
plot(pca, type = "l") 

############################################################################################
#####Linear Regression#############################
workerproduct =  lm(actual_productivity ~ incentive, data = newdataset)
print(workerproduct)
summary(workerproduct)

confint(workerproduct)
confint(object= workerproduct,level = 0.95)

install.packages("olsrr")
install.packages("leaps")
library(leaps)
require(olsrr)

predmultreg =  lm(actual_productivity ~ targeted_productivity + smv +wip+over_time+incentive+no_of_workers, data = newdataset)
summary (predmultreg)


newdataset2 = subset(newdataset, select = -c(date, quarter, department, day, team, idle_time, idle_men, no_of_style_change))
print (newdataset2)
view (newdataset2)

bestmodel = regsubsets(actual_productivity ~ ., data = newdataset2, method = "exhaustive") 
bestmodel = regsubsets(actual_productivity ~ ., data = newdataset2) 
summary(bestmodel)

res.sum <- summary(bestmodel)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic),
  RSS = which.min(res.sum$rss),
  RSQ = which.min(res.sum$rsq)
)

res.sum <- summary(bestmodel)
tableADJR <- data.frame(AdjustedR2 = res.sum$adjr2)
tableADJR

plot(res.sum$rsq, xlab="Number of Variables", ylab = "R2", type ="l")

coef (bestmodel, 5)

selectedmodel <- lm(actual_productivity~targeted_productivity + smv + over_time + incentive + no_of_workers, data = newdataset2)
summary(selectedmodel)
