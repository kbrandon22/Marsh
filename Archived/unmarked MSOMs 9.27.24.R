library(unmarked)
library(tidyr)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(mice)
library(ggplot2)
library(coda)

#Set working directory
setwd("C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\Datasets\\Excel Spreadsheets")

#-------------------------------------------------------------------------------
#DATA MANAGEMENT

#1. Read and manage csv files 
rodent_df <- read.csv("Marsh Master Spreadsheet.csv")
rodent_df <- rodent_df %>%
  select(Location, Rrav, Rmeg, Mmus, Mcal, Rrav_prop, Rmeg_prop, Mmus_prop, Mcal_prop, Config, Type)
print(rodent_df)

gis_df <- read.csv("Marsh Master Spreadsheet ArcGIS Pro.csv")
gis_df <- gis_df %>% 
  rename(Location = Marsh,
         Long = Longitude,
         Lat = Latitude)
print(gis_df)

frag_df <- read.csv("Habitat Fragmentation Analysis.csv")
frag_df <- frag_df %>%
  rename(Location = Marsh) %>%
  select(-X, -Habitat.Type, -Percent, -Hectares) %>%
  distinct(Location, .keep_all = TRUE)
print(frag_df)

#----------
#2. Combine into one data frame using fuzzy matching
rodent <- stringdist_join(rodent_df, gis_df, by = "Location", mode = "left", 
                                         method = "jw", max_dist = 0.18)

#a. Subtract unnecessary rows/columns and rename others
rodent <- rodent[-c(50:51),] %>%
  rename(Location = Location.x) %>%
  select(-Location.y) %>%
  mutate(Location = recode(Location,
                           "Bothin Marsh" = "Bothin Marsh (JH)",
                           "Bothin South" = "Bothin Marsh (CA)"))

#c. Combine with fragmentation data frame
rodent <- stringdist_join(rodent, frag_df, by = "Location", mode = "left",
                          method = "jw", max_dist = 0.18)
rodent

#d. Subtract unnecessary rows/columns and rename others
rodent <- rodent[-c(50:51),] %>%
  rename(Location = Location.x) %>%
  select(-Location.y)
str(rodent)

#----------
#3. Standardize the covariates
rodent <- rodent %>%
  mutate(across(c(Area, Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag),
                ~ (. -mean(., na.rm = TRUE))/sd(., na.rm = TRUE),
                .names = "{.col}"))
str(rodent)

#----------
#4. Impute missing data
#a. Copy Bothin Marsh covariates to Bothin South (i.e., Bothin Marsh (CA))
#NOTE: This is the same site but it did not meet the threshold for fuzzy matching 
#1) Assign rows to objects
bothin <- rodent[10,]
bothin_south <- rodent[11,]

#2) Match the column rows
common <- intersect(names(bothin), names(bothin_south))

#3) Update the common columns
bothin_south[common] <- mapply(function(source_value, target_value){
  ifelse(is.na(target_value), source_value, target_value)
}, bothin[common], bothin_south[common])  

#4) Add the updated row back into the original data frame
rodent[11,] <- bothin_south
options(max.print = nrow(rodent)*ncol(rodent))
str(rodent)

#----------
#5. Convert values back to numeric, integer
rodent <- rodent %>%
  mutate(across(c(Rrav, Rmeg, Mmus, Mcal), as.integer)) %>%
  mutate(across(c(Rrav_prop, Rmeg_prop, Mmus_prop, Mcal_prop, Long, Lat, Area, Dist_urban,
                  Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag), as.numeric))
str(rodent)

#----------
#6. Subset data and change number of decimal places
rodent <- rodent[,c(1:5,15:16, 23:29)] %>%
  mutate(across(c(Area, Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag), 
                ~ round(., 4)))
rodent <- na.omit(rodent)

#-------------------------------------------------------------------------------
#FORMULATE THE DATA

#1. Create presence/absence matrices for each species 
Rrav <- as.matrix(rodent$Rrav[1:71])
Rmeg <- as.matrix(rodent$Rmeg[1:71])
Mmus <- as.matrix(rodent$Mmus[1:71])
Mcal <- as.matrix(rodent$Mcal[1:71])

#2. Combine detection data into one named list
ylist <- list(Rrav=Rrav, Rmeg=Rmeg, Mmus=Mmus, Mcal=Mcal)
lapply(ylist, head)
str(ylist)

#3. Create data frame of standardized site covariates
sitecovs <- as.data.frame(rodent[1:71,6:14])
sapply(sitecovs, head)

#4. Create unmarkedFrameOccuMulti object
umf <- unmarkedFrameOccuMulti(y=ylist, siteCovs=sitecovs)
str(umf)

#--------------------------------
#ASSIGN FORMULAS

#1. Occupancy
#a. View fDesign matrix of occupancy formulas
umf@fDesign
colnames(umf@fDesign) 

#b. Create vector of intercept-only occupancy formulas with 3rd and 4th order parameters fixed at 0
stateformulas <- c("~1","~1","~1","~1","~1","~1","~1","~1","~1","~1","0","0","0","0","0")

#2. Detection
#a. Create vector of intercept-only detection formulas
detformulas <- c("~1", "~1", "~1", "~1")

#--------------------------------
#RUN THE NULL MODEL

#1. Fit the model
mod_null <- occuMulti(detformulas=detformulas, stateformulas=stateformulas, data=umf)
summary(mod_null)
  #Model is a poor fit (possible separation), try using penalized likelihood to control model 
  #complexity and reduce variance of parameter estimates (Murphy 2012, Clipp et al. 2021)

#2. Fit the model with penalized likelihood
mod_null <- optimizePenalty(mod_null, penalties=c(0.02,0.1,0.2,0.33,1,2))       #Optimal penalty is 0.33
summary(mod_null)
plot(mod_null)

  #Notable findings (WILL CHANGE WHEN MISSING DATA IS ADDED TO SPREADSHEET)
      #1. Mcal occupancy is the lowest of the four species
      #2. There is a NEGATIVE relationship between Rrav and Rmeg occupancy
      #3. There is a POSITIVE relationship between Rmeg and Mmus occupancy
      #4. Detection is highest for Mmus and lowest for Mcal

#--------------------------------
#CALCULATE OCCUPANCY AND DETECTION PROBABILITIES
install.packages("ggplot2")
library(ggplot2)

#1. Occupancy
#a. Predict probability for each occupancy state at each site
occ_prob <- predict(mod_null, newdata=data.frame(site=1),type="state")
occ_prob$Predicted

#b. Calculate occupancy probability for each species
boot::inv.logit(coef(mod_null)[1])   #Rrav = 0.7638
boot::inv.logit(coef(mod_null)[2])   #Rmeg = 0.4871
boot::inv.logit(coef(mod_null)[3])   #Mmus = 0.5402
boot::inv.logit(coef(mod_null)[4])   #Mcal = 0.3895

#OR; SHOULD BE SAME AS ABOVE, BUT VALUES DON'T MATCH - WHY?
rrav_occ <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav")      #Rrav = 0.5835
rrav_occ
rmeg_occ <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rmeg")      #Rmeg = 0.6122
rmeg_occ
mmus_occ <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Mmus")      #Mmus = 0.7896
mmus_occ
mcal_occ <- predict(mod_null, newdata=data.frame(site=1), type="state", species="Mcal")     #Mcal = 0.5530
mcal_occ

#c. Combine occupancy estimates of all species into data frame
all_occ <- rbind(rrav_occ[1,], rmeg_occ[1,], mmus_occ[1,], mcal_occ[1,])
all_occ$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#d. Visualize occupancy for all species
level_order <- c("Rrav", "Rmeg", "Mmus", "Mcal")

ggplot(all_occ, aes(x=factor(Species, level=level_order), y=Predicted)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
  ylim(0.2,1) +
  labs(x="Species", y="Marginal occupancy and 95% CI") +
  theme(axis.title.y=element_text(size=24, vjust=4),
        axis.title.x=element_text(size=24, vjust=-0.5),
        axis.text.x=element_text(color="black", size=20),
        axis.text.y=element_text(color="black", size=20),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))
#OR
plot(1:4, all_occ$Predicted, 
     ylim=c(0.1,1),
     xlim=c(0.5,4.5), 
     pch=19, cex=1.5, 
     xaxt='n',
     xlab="", ylab="Marginal occupancy and 95% CI")
axis(1, at=1:4, labels=all_occ$Species)
arrows(x0=1:4, y0=all_occ$lower,
       x1=1:4, y1=all_occ$upper, 
       code=3, lwd=1, col="black", angle=90, length=0.1)

#2. Detection
#a. Calculate detection probability for each species (with 95% CI)
boot::inv.logit(coef(mod_null)[11])  #Rrav = 0.8424
boot::inv.logit(coef(mod_null)[12])  #Rmeg = 0.8509
boot::inv.logit(coef(mod_null)[13])  #Mmus = 0.8547
boot::inv.logit(coef(mod_null)[14])  #Mcal = 0.5243

#OR 
rrav_det <- predict(mod_null, newdata=data.frame(site=1),type="det", species="Rrav")      #Rrav = 0.8424
rrav_det
rmeg_det <- predict(mod_null, newdata=data.frame(site=1),type="det", species="Rmeg")      #Rmeg = 0.8509
rmeg_det
mmus_det <- predict(mod_null, newdata=data.frame(site=1),type="det", species="Mmus")      #Mmus = 0.8547
mmus_det
mcal_det <- predict(mod_null, newdata=data.frame(site=1), type="det", species="Mcal")     #Mcal = 0.5243
mcal_det

#b. Combine detection probabilities of all species into data frame
all_det <- rbind(rrav_det[1,], rmeg_det[1,], mmus_det[1,], mcal_det[1,])
all_det$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#c. Visualize detection probabilities for all species
ggplot(all_det, aes(x=factor(Species, level=level_order), y=Predicted)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
  ylim(0.2,1) +
  labs(x="Species", y="Detection probability and 95% CI") +
  theme(axis.title.y=element_text(size=24, vjust=4),
        axis.title.x=element_text(size=24, vjust=-0.5),
        axis.text.x=element_text(color="black", size=20),
        axis.text.y=element_text(color="black", size=20),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))
#OR
plot(1:4, all_det$Predicted, ylim=c(0.1,1),
     xlim=c(0.5,4.5), pch=19, cex=1.5, xaxt='n',
     xlab="", ylab="Detection probability and 95% CI")
axis(1, at=1:4, labels=all_det$Species)
arrows(x0=1:4, y0=all_det$lower,
       x1=1:4, y1=all_det$upper, 
       code=3, lwd=1, col="black", angle=90, length=0.1)

#--------------------------------
#CALCULATE CONDITIONAL OCCUPANCY

#1. Predict the probability of occupancy of one species conditional on another species' presence
rrav_rmeg <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav", cond="Rmeg")
rrav_rmeg
rrav_mmus <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav", cond="Mmus")
rrav_mmus
rrav_mcal <- predict(mod_null, newdata=data.frame(site=1), type="state", species="Rrav", cond="Mcal")
rrav_mcal
mmus_rmeg <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Mmus", cond="Rmeg")
mmus_rmeg
mmus_mcal <- predict(mod_null, newdata=data.frame(site=1), type="state", species="Mmus", cond="Mcal")
mmus_mcal
rmeg_mcal <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rmeg", cond="Mcal")
rmeg_mcal

#2. Predict the probability of occupancy of one species conditional on another species' absence 
rrav_normeg <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav", cond="-Rmeg")
rrav_normeg
rrav_nommus <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav", cond="-Mmus")
rrav_nommus
rrav_nomcal <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rrav", cond="-Mcal")
rrav_nomcal
mmus_normeg <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Mmus", cond="-Rmeg")
mmus_normeg
mmus_nomcal <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Mmus", cond="-Mcal")
mmus_nomcal
rmeg_nomcal <- predict(mod_null, newdata=data.frame(site=1),type="state", species="Rmeg", cond="-Mcal")
rmeg_nomcal

#3. Visualize conditional occupancy of all species pairs
#a. Rrav conditional on other species
#rrav_rmeg_cond_data <- rbind(rrav_rmeg[1,], rrav_normeg[1,])
#rrav_rmeg_cond_data$Rmeg_status <- c("Present","Absent")

rrav_cond_data <- rbind(rrav_rmeg[1,], rrav_normeg[1,],
                        rrav_mmus[1,], rrav_nommus[1,],
                        rrav_mcal[1,], rrav_nomcal[1,])
rrav_cond_data$Species <- c("Rmeg", "Rmeg", "Mmus", "Mmus", "Mcal", "Mcal")
rrav_cond_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")

ggplot(rrav_cond_data, aes(Species_status, Predicted)) +
  geom_point(aes(Species_status, Predicted), size=3, position=position_jitter(width=0.2, seed=123)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=position_jitter(width=0.2, seed=123)) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=18, vjust=4),
        axis.title.x=element_text(size=18, vjust=-0.5),
        axis.text.x=element_text(color="black", size=15),
        axis.text.y=element_text(color="black", size=15),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

#Code for Rmeg
ggplot(rrav_rmeg_cond_data, aes(Rmeg_status, Predicted)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0.3) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=18, vjust=4),
        axis.title.x=element_text(size=18, vjust=-0.5),
        axis.text.x=element_text(color="black", size=15),
        axis.text.y=element_text(color="black", size=15),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))
#OR
plot(1:2, rrav_rmeg_cond_data$Predicted, ylim=c(0,0.9), 
     xlim=c(0.5,2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="Rmeg status", ylab="Rrav occupancy and 95% CI")
axis(1, at=1:2, labels=rrav_rmeg_cond_data$Rmeg_status)
arrows(x0=1:4, y0=rrav_rmeg_cond_data$lower,
       x1=1:4, y1=rrav_rmeg_cond_data$upper, 
       code=3, lwd=1, col="black", angle=90, length=0.1)

#b. Rrav conditional on Mmus
rrav_mmus_cond_data <- rbind(rrav_mmus[1,], rrav_nommus[1,])
rrav_mmus_cond_data$Mmus_status <- c("Present","Absent")

ggplot(rrav_mmus_cond_data, aes(Mmus_status, Predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#c. Rrav conditional on Mcal
rrav_mcal_cond_data <- rbind(rrav_mcal[1,], rrav_nomcal[1,])
rrav_mcal_cond_data$Mcal_status <- c("Present","Absent")

ggplot(rrav_mcal_cond_data, aes(Mcal_status, Predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#d. Mmus conditional on Rmeg
mmus_rmeg_cond_data <- rbind(mmus_rmeg[1,], mmus_normeg[1,])
mmus_rmeg_cond_data$Rmeg_status <- c("Present","Absent")

ggplot(mmus_rmeg_cond_data, aes(Rmeg_status, Predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#e. Mmus conditional on Mcal
mmus_mcal_cond_data <- rbind(mmus_mcal[1,], mmus_nomcal[1,])
mmus_mcal_cond_data$Mcal_status <- c("Present","Absent")

ggplot(mmus_mcal_cond_data, aes(Mcal_status, Predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#f. Rmeg conditional on Mcal
rmeg_mcal_cond_data <- rbind(rmeg_mcal[1,], rmeg_nomcal[1,])
rmeg_mcal_cond_data$Mcal_status <- c("Present","Absent")

ggplot(rmeg_mcal_cond_data, aes(Mcal_status, Predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#--------------------------------
#RUN UNIVARIATE MULTISPECIES OCCUPANCY MODELS 

#1. Review data
colnames(umf@fDesign)
head(siteCovs(umf))

#2. Run models for each covariate
#a. Vegetation richness
sf_veg <- c("~Veg_rich", "~Veg_rich","~Veg_rich","~Veg_rich",
         "~1","~1","~1","~1","~1","~1","0","0","0","0","0")
mod_veg <- occuMulti(stateformulas=sf_veg, detformulas=detformulas, umf)
summary(mod_veg)
  #Model did not converge

#1) Re-run model with increased maxit control argument
mod_veg <- occuMulti(stateformulas=sf_veg, detformulas=detformulas, umf,
                      control=list(maxit=10000))
summary(mod_veg)
  #Model is a poor fit (high estimates and SE)

#2) Re-run model with penalized likelihood
mod_veg_penalty <- optimizePenalty(mod_veg, penalties=c(0,0.02,0.1,0.2,0.33,1,2))
summary(mod_veg_penalty)
 
  #Notable findings:
     #1. 

#3) Compare model fit 
#a) Bias
bias_veg <- 

#b) Variance
var_veg <- 

#c) Mean squared error (MSE)
mse_veg <- (var_veg+(bias_veg)^2)

#4) Visualize MSE 
plot(mse_veg)


#PLOT UNSTANDARDIZED VALUES?
#5) Visualize effects of vegetation richness on marginal occupancy
install.packages("colorspace")
library(colorspace)

#a) Generate sequence of values for X-axis of plot  
veg_range <- range(sitecovs$Veg_rich)
veg_seq <- seq(veg_range[1], veg_range[2], length.out=100)

#b) Predict marginal occupancy for each species along sequence
nd_veg <- data.frame(Veg_rich=veg_seq)

occ_veg_rrav <- predict(mod_veg, type="state", species="Rrav", newdata=nd_veg)
occ_veg_rrav$Species <- "Rrav"
occ_veg_rrav$Veg_rich <- veg_seq
head(occ_veg_rrav)

occ_veg_rmeg <- predict(mod_veg, type = "state", species = "Rmeg", newdata = nd_veg)
occ_veg_rmeg$Species <- "Rmeg"
occ_veg_rmeg$Veg_rich <- veg_seq
head(occ_veg_rmeg)

occ_veg_mmus <- predict(mod_veg, type = "state", species = "Mmus", newdata = nd_veg)
occ_veg_mmus$Species <- "Mmus"
occ_veg_mmus$Veg_rich <- veg_seq
head(occ_veg_mmus)

occ_veg_mcal <- predict(mod_veg, type = "state", species = "Mcal", newdata = nd_veg)
occ_veg_mcal$Species <- "Mcal"
occ_veg_mcal$Veg_rich <- veg_seq
head(occ_veg_mcal)

#c) Plot marginal occupancy as a function of vegetation richness
pal_veg <- choose_palette(pal=heat_hcl)
color_veg <- pal_veg(4)

plot(occ_veg_rrav$Veg_rich, occ_veg_rrav$Predicted, ylim=c(0, 1), type="l",
     col=color_veg[1], lwd = 2, xlab="Vegetation richness", ylab = "Marginal occupancy")
lines(occ_veg_rmeg$Veg_rich, occ_veg_rmeg$Predicted, col=color_veg[2], lwd=2)
lines(occ_veg_mmus$Veg_rich, occ_veg_mmus$Predicted, col=color_veg[3], lwd=2)
lines(occ_veg_mcal$Veg_rich, occ_veg_mcal$Predicted, col=color_veg[4], lwd=2)
legend('topleft', col=c(color_veg[1],color_veg[2], color_veg[3], color_veg[4]),lty=1,lwd=2,
                        legend=c("Rrav", "Rmeg", "Mmus", "Mcal"))

#b. Pickleweed presence
#1) Run univariate model
sf_pick <- c("~Pickleweed_prop", "~Pickleweed_prop", "~Pickleweed_prop", "~Pickleweed_prop",
            "~1","~1","~1","~1","~1","~1","0","0","0","0","0")
mod_pick <- occuMulti(stateformulas=sf_pick, detformulas=detformulas, umf)
summary(mod_pick)
?occuMulti
#CODE NOT WORKING, NEED TO SPECIFY STARTING VALUES FOR THE MAXIMUM LIKELIHOOD ESTIMATION?

#c. Proportion of sites where annual grass is present
sf_ag <- c("~AG_prop", "~AG_prop", "~AG_prop", "~AG_prop",
             "~1","~1","~1","~1","~1","~1","0","0","0","0","0")
mod_ag <- occuMulti(stateformulas=sf_ag, detformulas=detformulas, umf)
summary(mod_ag)

#d. Proportion of sites where high tide escape vegetation is present

#e. Area of the site

#f. Area of the site minus 50m buffer

#g. Area of the site minus 200m buffer

#h. Proportion of marsh habitat within a 50 meter buffer outside the perimeter of the site

#i. Proportion of marsh habitat within a 200 meter buffer outside the perimeter of the site

#j. Proportion of marsh habitat within a 1 kilometer buffer outside the perimeter of the site

#k. Proportion of the matrix (the non-marsh, terrestrial habitat) within a 50 meter buffer outside the perimeter of the site

#l. Proportion of the matrix (the non-marsh, terrestrial habitat) within a 200 meter buffer outside the perimeter of the site

#m. Proportion of the matrix (the non-marsh, terrestrial habitat) within a 1 kilometer buffer outside the perimeter of the site

#--------------------------------
#9. MULTIVARIATE MULTISPECIES OCCUPANCY MODELS
#a. Check for collinearity between variables

#b. Determine optimal penalty term value using K-fold cross-validation
fit_opt <-optimizePenalty(model, penalties=c(0.02,0.1,0.2,0.33,1,2))

#c. Run model
occuMulti(detformulas=detformulas, stateformulas=, data=umf, 
          penalty=fit_opt, starts, method=, 
          se=TRUE, engine=c("R"))

#--------------------------------
#10. MODEL SELECTION
#a. Select the best-fitting model for interpretation
#1) Create fitList object of models
mods <- fitList(mod_null, mod_veg)

#2) Generate a model selection table
modSel(mods)




