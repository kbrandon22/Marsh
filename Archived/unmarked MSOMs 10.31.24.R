library(unmarked)
library(tidyr)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(mice)
library(ggplot2)
library(coda)
library(formattable)
library(car)
library(stats)
library(factoextra)
library(glmnet)
library(AICcmodavg)
library(wesanderson)
library(MuMIn)
library(foreach)
library(pbapply)

set.seed(500)

#Set working directory
setwd("C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\Datasets\\Excel Spreadsheets")

#-------------------------------------------------------------------------------
#DATA MANAGEMENT

#1. Read and manage csv files 
rodent_df <- read.csv("Marsh Master Spreadsheet.csv")
rodent_df <- rodent_df %>%
  select(Location, Rrav, Rmeg, Mmus, Mcal, Rrav_prop, Rmeg_prop, Mmus_prop, Mcal_prop, Config, Type)
rodent_df

gis_df <- read.csv("Marsh Master Spreadsheet ArcGIS Pro.csv")
gis_df <- gis_df %>% 
  rename(Location = Marsh,
         Long = Longitude,
         Lat = Latitude)
gis_df

frag_df <- read.csv("Habitat Fragmentation Analysis.csv")
frag_df <- frag_df %>%
  rename(Location = Marsh) %>%
  select(-X, -Habitat.Type, -Percent, -Hectares) %>%
  distinct(Location, .keep_all = TRUE)
frag_df

#----------
#2. Combine into one data frame using fuzzy matching
#a. Combine rodent and gis data frames
rodent <- stringdist_join(rodent_df, gis_df, by = "Location", mode = "left", 
                                         method = "jw", max_dist = 0.18)j

#b. Subtract unnecessary rows/columns and rename others
rodent <- rodent[-c(50:51),-c(10, 18:23)] %>%
  rename(Location = Location.x) %>%
  select(-Location.y) %>%
  mutate(Location = dplyr::recode(Location,
                           "Bothin Marsh" = "Bothin Marsh (JH)",
                           "Bothin South" = "Bothin Marsh (CA)"))
str(rodent)

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


#b.Convert values back to numeric, integer
rodent <- rodent %>%
  mutate(across(c(Rrav, Rmeg, Mmus, Mcal), as.integer)) %>%
  mutate(across(c(Rrav_prop, Rmeg_prop, Mmus_prop, Mcal_prop, Long, Lat, Area, Dist_urban,
                  Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag), as.numeric))
str(rodent)


#c. Change number of decimal places throughout data frame
rodent <- rodent %>%
  mutate(across(c(Area, Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag), 
                ~ round(., 4)))
rodent

#Write and export CSV file for future use
write.csv(rodent, file = "C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\Datasets\\Excel Spreadsheets\\Marsh Master Spreadsheet R.csv", 
          row.names = TRUE)

#d. Impute other missing data (e.g., above MHW, Edge, Frag)
#1) Subset the columns with missing data and convert to numeric (if needed)
rodent_sub <- rodent[,c(1, 16, 21:22)]

#2) Assign imputation methods 
method <- c("", "pmm", "pmm", "pmm")              

#3) Impute the data
imp <- mice(rodent_sub, method = method, m = 10, maxit = 30, print = F)
summary(imp)

#4) Perform diagnostics and summarize imputation effectiveness with plots
plot(imp, layout = c(2,5))   

mice::stripplot(imp, Above_MHW ~.imp, pch=20, cex=2)
mice::stripplot(imp, Frag ~.imp, pch=20, cex=2)
mice::stripplot(imp, Edge ~.imp, pch=20, cex=2)
    #All imputed values (in red) are among observed values (in blue) and are therefore plausible

#5) Extract imputed data sets
imputed <- lapply(1:imp$m, function(i) complete(imp, i))


#d. Combine all imputed and original data sets
rodent_imp <- lapply(seq_along(imputed), function(i){
  complete_imp <- complete(imputed[[i]])
  merged <- merge(rodent[,-c(16, 21:22)], complete_imp, by = c("Location"), all.x = TRUE)
  return(merged)
})
rodent_imp

#----------
#5. Check assumptions
#Independence, no false detections, constant detection and occupancy probability

#a. Multicollinearity with variance inflation factor
#1) Create a vector of covariates to assess
col_vars <- c("Area", "Dist_urban", "Mcal_conn", "Mmus_conn", "Rrav_conn", "Rmeg_conn", "Above_MHW", "Edge", "Frag")

#2) Create a vector of dependent variables
dep_vars <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#3) Assess multicollinearity 
for(dependent_variable in dep_vars){                                              
  dep_vif <- list()
  for(i in seq_along(rodent_imp)){                                                
    complete_data <- rodent_imp[[i]]
    formula <- as.formula(paste(dependent_variable, "~", paste(col_vars, collapse = "+")))
    model <- lm(formula, data = complete_data)
    vif_values <- vif(model)
    dep_vif[[i]] <- vif_values
  }
}
dep_vif

#-----
#b. Examine pairwise comparisons between collinear variables
#1) Convert character covariates to factor
for(i in seq_along(rodent_imp)){
  complete_Data <- rodent_imp[[i]]
  cols <- c("Type", "Subregion")
  complete_data[cols] <- lapply(complete_data[cols], as.factor)
  rodent_imp[[i]] <- complete_data
}

#2) Create a list to store correlation matrices
cor_mats <- list()

#3) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, col_vars], use = "pairwise.complete.obs")
  cor_mats[[i]] <- cor_matrix
}
cor_mats
      #Due to significant pairwise comparisons between connectivity measures, reduce 
      #collinearity by using principal component analysis (PCA) to reduce dimensionality

#-----
#c. Reduce collinearity between connectivity measures 
#1) Create a list of connectivity measures
conn_vars <- c("Rrav_conn", "Rmeg_conn", "Mcal_conn", "Mmus_conn")

#2) Loop through each imputed dataset and append the results to the imputed datasets
for(i in seq_along(rodent_imp)){
  data <- rodent_imp[[i]]
  pca_result <- prcomp(data[conn_vars])
  var_exp <- (pca_result$sdev^2)/sum(pca_result$sdev^2)
  cum_var <- cumsum(var_exp)
  num_comps <- which(cum_var >= 0.9)[1]                                           #Select principal components that account for at least 90% of the variance
  pca_scores <- as.data.frame(pca_result$x)
  loadings <- pca_result$rotation                                                 #View loadings to determine how each variable in PCA contributes to outcome
  rodent_imp[[i]][paste0("Conn_vars_PC", seq_len(num_comps))] <- pca_scores[, 1:num_comps]
}
rodent_imp

#3) Change the number of decimal places 
decimal <- 4
for(i in seq_along(rodent_imp)){
  conn_var_columns <- grep("Conn_vars_PC", names(rodent_imp[[i]]), value = TRUE)
  if(length(conn_var_columns) > 0){
    rodent_imp[[i]][conn_var_columns] <- round(rodent_imp[[i]][conn_var_columns], decimal)
  }
}
rodent_imp
  
#4) Re-assess correlation
#a) Create a list to store correlation matrices
cor_mats2 <- list()

#b) Specify the variables to include in the correlation matrix
col_vars2 <- c("Area", "Dist_urban", "Above_MHW", "Frag", "Edge", "Conn_vars_PC1", "Conn_vars_PC2")

#c) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, col_vars2], use = "pairwise.complete.obs")
  cor_matrix <- round(cor_matrix, digits = 5)
  cor_mats2[[i]] <- cor_matrix
}
cor_mats2

#5) Remove original connectivity measures from imputed data sets
for(i in seq_along(rodent_imp)){
  rodent_imp[[i]] <- rodent_imp[[i]][,-c(16:19)]
}
rodent_imp

#-----
#d. Remove edge and re-assess correlation
#1) Remove edge metric from imputed data sets
for(i in seq_along(rodent_imp)){
  rodent_imp[[i]] <- rodent_imp[[i]][,-c(17)]
}
rodent_imp

#2) Re-assess correlation
#a) Create a list to store correlation matrices
cor_mats3 <- list()

#b) Specify the variables to include in the correlation matrix
col_vars3 <- c("Area", "Dist_urban", "Above_MHW", "Frag", "Conn_vars_PC1", "Conn_vars_PC2")

#c) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, col_vars3], use = "pairwise.complete.obs")
  cor_mats3[[i]] <- cor_matrix
}
cor_mats3

#-----
#e. Reduce collinearity between fragmentation and area metrics
#1) Create a list of fragmetation metrics and area
frag_vars <- c("Frag", "Area")

#2) Loop through each imputed dataset and append the results to the imputed datasets
for(i in seq_along(rodent_imp)){
  data <- rodent_imp[[i]]
  pca_result <- prcomp(data[frag_vars])
  var_exp <- (pca_result$sdev^2)/sum(pca_result$sdev^2)
  cum_var <- cumsum(var_exp)
  num_comps <- which(cum_var >= 0.9)[1]                                             
  pca_scores <- as.data.frame(pca_result$x[,1:num_comps])
  loadings <- pca_result$rotation
  rodent_imp[[i]][paste0("Frag_vars_PC", seq_len(num_comps))] <- pca_scores[, 1:num_comps]
}
rodent_imp

#3) Change the number of decimal places 
for(i in seq_along(rodent_imp)){
  frag_var_columns <- grep("Frag_vars_PC", names(rodent_imp[[i]]), value = TRUE)
  if(length(frag_var_columns) > 0){
    rodent_imp[[i]][frag_var_columns] <- round(rodent_imp[[i]][frag_var_columns], decimal)
  }
}
rodent_imp

#4) Re-assess correlation
#a) Create a list to store correlation matrices
cor_mats4 <- list()

#b) Specify the variables to include in the correlation matrix
col_vars4 <- c("Dist_urban", "Above_MHW", "Conn_vars_PC1", "Conn_vars_PC2", "Frag_vars_PC1", "Frag_vars_PC2")

#b) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, col_vars4], use = "pairwise.complete.obs")
  cor_matrix <- round(cor_matrix, digits = 5)
  cor_mats4[[i]] <- cor_matrix
}
cor_mats4

#5) Remove original metrics from imputed data sets
for(i in seq_along(rodent_imp)){
  rodent_imp[[i]] <- rodent_imp[[i]][,-c(14, 17)]
}
rodent_imp

#############################################
#OR Run all models and assess VIF afterwards, removing models with VIF >=10
#############################################

#-------------------------------------------------------------------------------
#FORMULATE THE DATA

#1. Create presence/absence matrices 
#a. Extract detection data for each species
Rrav <- as.matrix(rodent$Rrav[1:76])
Rmeg <- as.matrix(rodent$Rmeg[1:76])
Mmus <- as.matrix(rodent$Mmus[1:76])
Mcal <- as.matrix(rodent$Mcal[1:76])

#b. Combine into one named list
ylist <- list(Rrav=Rrav, Rmeg=Rmeg, Mmus=Mmus, Mcal=Mcal)
ylist

#----------
#2. Create data frame of standardized site covariates
#a. Extract covariate names
covs <- colnames(rodent_imp[[1]])
covs <- covs[c(10,14:19)]

#b. Define a function to extract covariates and convert characters to factor
extract_covs <- function(data){
  covariate_df <- data[, covs, drop=FALSE]
  covariate_df[] <- lapply(covariate_df, function(col) {
    if (is.character(col)) {
      as.factor(col)
    } else {
      col
    }
  })
  return(covariate_df)
}

#c. Apply the function to each imputed data set
sitecovs <- lapply(rodent_imp, extract_covs)
sitecovs

#----------
#3. Create unmarkedFrameOccuMulti objects
#a. Define a function to create the umf objects 
create_umf <- function(ylist, site_covs) {
  umf_list <- list()
  for (i in 1:length(site_covs)) {
    umf_list[[i]] <- unmarkedFrameOccuMulti(
      y = ylist, 
      siteCovs = site_covs[[i]]  
    )
  }
  return(umf_list)
}

#b. Apply the function to create a list of umf objects (one for each dataset)
umf_list <- create_umf(ylist, sitecovs)
summary(umf_list[[1]])
plot(umf_list[[1]])

#-------------------------------------------------------------------------------
#ASSIGN FORMULAS

#1. Occupancy
#a. View fDesign matrix of occupancy formulas
view_fDesign <- function(umf_list){
  fDesign_list <- lapply(umf_list, function(umf){
    umf@fDesign
  })
  return(fDesign_list)
}

umf_fDesign <- view_fDesign(umf_list)
colnames(umf_fDesign[[1]])

#b. Create vector of intercept-only occupancy formulas with 3rd and 4th order parameters fixed at 0
stateformulas <- c("~1","~1","~1","~1","~1","~1","~1","~1","~1","~1","0","0","0","0","0") 

#2. Detection
#a. Create vector of intercept-only detection formulas
detformulas <- c("~1", "~1", "~1", "~1")

#-------------------------------------------------------------------------------
#NULL MODEL

#1. Define a function to fit the model
starting_values <- rnorm(14, mean = 0, sd = 0.2571)

fit_null_models <- function(umf_list, stateformulas, detformulas){
  set.seed(500)
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = stateformulas, 
      detformulas = detformulas,
      control = list(maxit=20000),         #Set number of iterations
      method = "Nelder-Mead",              #Avoid Hessian is singular error
      starts = starting_values,            
      data = umf_list[[i]]
    )
  }
  return(model_list)
}

#2. Apply the function
null_models <- fit_null_models(umf_list, stateformulas, detformulas)            
summary(null_models[[1]])
      #NOTE: Model is a poor fit (possible separation), try using penalized likelihood to control model 
      #complexity and reduce variance of parameter estimates (Murphy 2012, Clipp et al. 2021)

#3. Fit the model with penalized likelihood (if needed)                           
#a. Define a function with various penalty values
fit_null_models_pen <- function(null_models){
  set.seed(500)
  model_list <- list()
  for(i in seq_along(null_models)){
    model_list[[i]] <- unmarked::optimizePenalty(
      null_models[[i]], 
      penalties = c(0.05, 0.1, 0.2, 0.33, 1, 2)
    )
  }
  return(model_list)
}

#b. Apply the function to the list of null models - code takes a while to run
null_models_pen <- fit_null_models_pen(null_models)
null_models_pen

#4. Pool results with Rubin's rules for variance estimation
#a. Extract components of model outputs
coefficients <- sapply(null_models_pen, coef)
se <- sapply(null_models_pen, function(model) sqrt(diag(vcov(model))))
z_values <- coefficients/se
p_values <- 2*(1-pnorm(abs(z_values)))
aic_values <- sapply(null_models_pen, function(model) model@AIC)

#b. Calculate model weights based on AIC values
weights <- (1/aic_values)
weights <- weights/sum(weights)

#c. Pool results
pooled_coefs <- rowSums(coefficients*weights)
pooled_se <- sqrt(rowSums(weights*(se^2 + (coefficients - pooled_coefs)^2)))
pooled_z <- pooled_coefs/pooled_se
pooled_p <- 2*(1-pnorm(abs(pooled_z)))

#d. Merge pooled results into one data frame
pool_results <- data.frame(
  Estimate = pooled_coefs,
  SE = pooled_se,
  Z = pooled_z,
  p_value = pooled_p
)
pool_results <- round(pool_results, digits = 5)
pool_results
      #Mmus detection is the highest of all species; no other significant p-values

#--------------------------------
#CALCULATE OCCUPANCY AND DETECTION PROBABILITIES

#1. Occupancy
#a. Predict probability for each occupancy state at each site
occ_probs <- lapply(null_models_pen, function(null_models){
  set.seed(500)
  all_probs <- predict(null_models, newdata = data.frame(site=1),type="state")
  return(all_probs)
})
occ_probs[[1]]$Predicted

#b. Calculate occupancy probability for each species
occ_preds <- lapply(null_models_pen, function(null_models){
  set.seed(500)
  rrav_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav")
  rmeg_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg")
  mmus_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus")
  mcal_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal")
  all_occ <- rbind(rrav_occ[1,], rmeg_occ[1,], mmus_occ[1,], mcal_occ[1,])
  all_occ$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_occ)
})
occ_preds[[1]]$Predicted

#c. Pool the results
occ_pooled <- do.call(rbind, lapply(split(do.call(rbind, occ_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
occ_pooled$Species <- factor(occ_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
occ_pooled
    #Rrav = 0.645, lower = 0.509, upper = 0.765
    #Rmeg = 0.584, lower = 0.453, upper = 0.696
    #Mmus = 0.780, lower = 0.674, upper = 0.852
    #Mcal = 0.479, lower = 0.354, upper = 0.607

#d. Visualize occupancy for all species
level_order <- c("Rrav", "Rmeg", "Mmus", "Mcal")

ggplot(occ_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
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

#2. Detection
#a. Calculate detection probability for each species (with 95% CI)
det_preds <- lapply(null_models_pen, function(null_models){
  rrav_occ <- predict(null_models, newdata = data.frame(site=1), type="det", species="Rrav")
  rmeg_occ <- predict(null_models, newdata = data.frame(site=1), type="det", species="Rmeg")
  mmus_occ <- predict(null_models, newdata = data.frame(site=1), type="det", species="Mmus")
  mcal_occ <- predict(null_models, newdata = data.frame(site=1), type="det", species="Mcal")
  all_occ <- rbind(rrav_occ[1,], rmeg_occ[1,], mmus_occ[1,], mcal_occ[1,])
  all_occ$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_occ)
})
det_preds[[1]]$Predicted

#b. Pool the results
det_pooled <- do.call(rbind, lapply(split(do.call(rbind, det_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
det_pooled$Species <- factor(det_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
det_pooled
    #Rrav = 0.762, lower = 0.622, upper = 0.860
    #Rmeg = 0.822, lower = 0.708, upper = 0.896
    #Mmus = 0.846, lower = 0.751, upper = 0.907
    #Mcal = 0.531, lower = 0.367, upper = 0.688

#c. Visualize detection probabilities for all species
ggplot(det_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
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

#--------------------------------
#CONDITIONAL OCCUPANCY

#1. Predict the probability of occupancy of one species conditional on another species' presence
cond_occ <- lapply(null_models_pen, function(null_models){
  rrav_rmeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="Rmeg")
  rrav_mmus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="Mmus")
  rrav_mcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="Mcal")
  rmeg_rrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="Rrav")
  rmeg_mmus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="Mmus")
  rmeg_mcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="Mcal")
  mmus_rrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="Rrav")
  mmus_rmeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="Rmeg")
  mmus_mcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="Mcal")
  mcal_rrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="Rrav")
  mcal_rmeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="Rmeg")
  mcal_mmus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="Mmus")
  all_cond_occ <- rbind(rrav_rmeg[1,], rrav_mmus[1,], rrav_mcal[1,], rmeg_rrav[1,], rmeg_mmus[1,], rmeg_mcal[1,],
                        mmus_rrav[1,], mmus_rmeg[1,], mmus_mcal[1,], mcal_rrav[1,], mcal_rmeg[1,], mcal_mmus[1,])
  all_cond_occ$Species <- c("Rrav_Rmeg", "Rrav_Mmus", "Rrav_Mcal", "Rmeg_Rrav", "Rmeg_Mmus", "Rmeg_Mcal",
                            "Mmus_Rrav", "Mmus_Rmeg", "Mmus_Mcal", "Mcal_Rrav", "Mcal_Rmeg", "Mcal_Mmus")
  return(all_cond_occ)
})
cond_occ[[1]]$Predicted

#b. Pool the results
cond_occ_pooled <- do.call(rbind, lapply(split(do.call(rbind, cond_occ), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "SE", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    SE = mean_pred["SE"], lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
cond_occ_pooled$Species <- factor(cond_occ_pooled$Species, levels = c("Rrav_Rmeg", "Rrav_Mmus", "Rrav_Mcal", "Rmeg_Rrav", "Rmeg_Mmus", "Rmeg_Mcal",
                                                                      "Mmus_Rrav", "Mmus_Rmeg", "Mmus_Mcal", "Mcal_Rrav", "Mcal_Rmeg", "Mcal_Mmus"))
cond_occ_pooled
    #Mcal is neutrally associated with all species (Mmus: 52%, Rmeg: 62%, Rrav: 46%)
    #Mmus is positively associated with Mcal (84%), Rmeg (84%), and Rrav (83%)
    #Rmeg is positively associated with Mcal (75%) and neutrally with Mmus (63%) and Rrav (52%) 
    #Rrav is neutrally associated with Mcal (62%) and Rmeg (57%) and slightly positively associated with Mmus (68%)

#2. Predict the probability of occupancy of one species conditional on another species' absence 
abs_occ <- lapply(null_models_pen, function(null_models){
  rrav_normeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="-Rmeg")
  rrav_nommus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="-Mmus")
  rrav_nomcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav", cond="-Mcal")
  rmeg_norrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="-Rrav")
  rmeg_nommus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="-Mmus")
  rmeg_nomcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg", cond="-Mcal")
  mmus_norrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="-Rrav")
  mmus_normeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="-Rmeg")
  mmus_nomcal <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus", cond="-Mcal")
  mcal_norrav <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="-Rrav")
  mcal_normeg <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="-Rmeg")
  mcal_nommus <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal", cond="-Mmus")
  all_abs_occ <- rbind(rrav_normeg[1,], rrav_nommus[1,], rrav_nomcal[1,], rmeg_norrav[1,], rmeg_nommus[1,], rmeg_nomcal[1,],
                        mmus_norrav[1,], mmus_normeg[1,], mmus_nomcal[1,], mcal_norrav[1,], mcal_normeg[1,], mcal_nommus[1,])
  all_abs_occ$Species <- c("Rrav_NoRmeg", "Rrav_NoMmus", "Rrav_NoMcal", "Rmeg_NoRrav", "Rmeg_NoMmus", "Rmeg_NoMcal",
                            "Mmus_NoRrav", "Mmus_NoRmeg", "Mmus_NoMcal", "Mcal_NoRrav", "Mcal_NoRmeg", "Mcal_NoMmus")
  return(all_abs_occ)
})
abs_occ[[1]]$Predicted

#b. Pool the results
abs_occ_pooled <- do.call(rbind, lapply(split(do.call(rbind, abs_occ), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "SE", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    SE = mean_pred["SE"], lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
abs_occ_pooled$Species <- factor(abs_occ_pooled$Species, levels = c("Rrav_NoRmeg", "Rrav_NoMmus", "Rrav_NoMcal", "Rmeg_NoRrav", "Rmeg_NoMmus", "Rmeg_NoMcal",
                                                                    "Mmus_NoRrav", "Mmus_NoRmeg", "Mmus_NoMcal", "Mcal_NoRrav", "Mcal_NoRmeg", "Mcal_NoMmus"))
abs_occ_pooled
    #Mcal is not likely to be present when Mmus (34%) and Rmeg (29%) are absent, and occupancy is not associated with Rrav absence (51%)
    #Mmus is likely to be present when Mcal (72%), Rmeg (69%), and Rrav (70%) are absent
    #Rmeg is likely to be present when Rrav (70%) is absent and occupancy is not associated with Mcal (43%) or Mmus (42%) absence
    #Rrav is likely to be present when Rmeg (74%) and less so Mcal (66%) are absent and occupancy is not associated with Mmus (51%)

#3. Visualize conditional occupancy of all species pairs
#a. Rrav conditional on other species
#1) Create a vector of conditional relationships
rrav_species_pairs <- c("Rrav_Rmeg", "Rrav_NoRmeg", "Rrav_Mmus", "Rrav_NoMmus", "Rrav_Mcal", "Rrav_NoMcal")

#2) Create a list to store the results
rrav_list <- list() 

#3) Loop through the pooled results to extract the predicted values for each combination of conditional relationships
for(pair in rrav_species_pairs){
  pred_cond_occ <- cond_occ_pooled[cond_occ_pooled$Species == pair,]
  pred_abs_occ <- abs_occ_pooled[abs_occ_pooled$Species == pair,]
  combined <- data.frame(
    Species = pair,
    Predicted = c(pred_cond_occ$Predicted, pred_abs_occ$Predicted),
    SE = c(pred_cond_occ$SE, pred_abs_occ$SE),
    lower = c(pred_cond_occ$lower, pred_abs_occ$lower),
    upper = c(pred_cond_occ$upper, pred_abs_occ$upper)
  )
  rrav_list[[pair]] <- combined
}
rrav_list

#4) Combine results in a data frame
rrav_data <- do.call(rbind, rrav_list)
rrav_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")
rrav_data

#5) Plot results
colors <- wes_palette("Zissou1", n=3)

rrav_data$Species_simple <- gsub("Rrav_", "", rrav_data$Species)
rrav_data$Species_simple <- gsub("No", "", rrav_data$Species_simple)

ggplot(rrav_data, aes(Species_status, Predicted)) +
  geom_point(aes(color=Species_simple), size=3, position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), width=0.3, position=position_dodge(width=0.4)) +
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
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[1], "Rmeg" = colors[3], "Mmus" = colors[2]),
                     labels=c("Mcal", "Mmus", "Rmeg")) +
  guides(color=guide_legend(title="Species"))

#b. Rmeg conditional on other species
#1) Create a vector of conditional relationships
rmeg_species_pairs <- c("Rmeg_Rrav", "Rmeg_NoRrav", "Rmeg_Mmus", "Rmeg_NoMmus", "Rmeg_Mcal", "Rmeg_NoMcal")

#2) Create a list to store the results
rmeg_list <- list() 

#3) Loop through the pooled results to extract the predicted values for each combination of conditional relationships
for(pair in rmeg_species_pairs){
  pred_cond_occ <- cond_occ_pooled[cond_occ_pooled$Species == pair,]
  pred_abs_occ <- abs_occ_pooled[abs_occ_pooled$Species == pair,]
  combined <- data.frame(
    Species = pair,
    Predicted = c(pred_cond_occ$Predicted, pred_abs_occ$Predicted),
    SE = c(pred_cond_occ$SE, pred_abs_occ$SE),
    lower = c(pred_cond_occ$lower, pred_abs_occ$lower),
    upper = c(pred_cond_occ$upper, pred_abs_occ$upper)
  )
  rmeg_list[[pair]] <- combined
}
rmeg_list

#4) Combine results in a data frame
rmeg_data <- do.call(rbind, rmeg_list)
rmeg_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")
rmeg_data

#5) Plot results
rmeg_data$Species_simple <- gsub("Rmeg_", "", rmeg_data$Species)
rmeg_data$Species_simple <- gsub("No", "", rmeg_data$Species_simple)

ggplot(rmeg_data, aes(Species_status, Predicted)) +
  geom_point(aes(color=Species_simple), size=3, position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), width=0.3, position=position_dodge(width=0.4)) +
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
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[1], "Mmus" = colors[2], "Rrav" = colors[3]),
                     labels=c("Mcal", "Mmus", "Rrav")) +
  guides(color=guide_legend(title="Species"))

#c. Mmus conditional on other species
#1) Create a vector of conditional relationships
mmus_species_pairs <- c("Mmus_Rrav", "Mmus_NoRrav", "Mmus_Rmeg", "Mmus_NoRmeg", "Mmus_Mcal", "Mmus_NoMcal")

#2) Create a list to store the results
mmus_list <- list() 

#3) Loop through the pooled results to extract the predicted values for each combination of conditional relationships
for(pair in mmus_species_pairs){
  pred_cond_occ <- cond_occ_pooled[cond_occ_pooled$Species == pair,]
  pred_abs_occ <- abs_occ_pooled[abs_occ_pooled$Species == pair,]
  combined <- data.frame(
    Species = pair,
    Predicted = c(pred_cond_occ$Predicted, pred_abs_occ$Predicted),
    SE = c(pred_cond_occ$SE, pred_abs_occ$SE),
    lower = c(pred_cond_occ$lower, pred_abs_occ$lower),
    upper = c(pred_cond_occ$upper, pred_abs_occ$upper)
  )
  mmus_list[[pair]] <- combined
}
mmus_list

#4) Combine results in a data frame
mmus_data <- do.call(rbind, mmus_list)
mmus_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")
mmus_data

#5) Plot results
mmus_data$Species_simple <- gsub("Mmus_", "", mmus_data$Species)
mmus_data$Species_simple <- gsub("No", "", mmus_data$Species_simple)

ggplot(mmus_data, aes(Species_status, Predicted)) +
  geom_point(aes(color=Species_simple), size=3, position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), width=0.3, position=position_dodge(width=0.4)) +
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
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[1], "Rmeg" = colors[2], "Rrav" = colors[3]),
                     labels=c("Mcal", "Rmeg", "Rrav")) +
  guides(color=guide_legend(title="Species"))

#d. Mcal conditional on other species
#1) Create a vector of conditional relationships
mcal_species_pairs <- c("Mcal_Rrav", "Mcal_NoRrav", "Mcal_Rmeg", "Mcal_NoRmeg", "Mcal_Mmus", "Mcal_NoMmus")

#2) Create a list to store the results
mcal_list <- list() 

#3) Loop through the pooled results to extract the predicted values for each combination of conditional relationships
for(pair in mcal_species_pairs){
  pred_cond_occ <- cond_occ_pooled[cond_occ_pooled$Species == pair,]
  pred_abs_occ <- abs_occ_pooled[abs_occ_pooled$Species == pair,]
  combined <- data.frame(
    Species = pair,
    Predicted = c(pred_cond_occ$Predicted, pred_abs_occ$Predicted),
    SE = c(pred_cond_occ$SE, pred_abs_occ$SE),
    lower = c(pred_cond_occ$lower, pred_abs_occ$lower),
    upper = c(pred_cond_occ$upper, pred_abs_occ$upper)
  )
  mcal_list[[pair]] <- combined
}
mcal_list

#4) Combine results in a data frame
mcal_data <- do.call(rbind, mcal_list)
mcal_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")
mcal_data

#5) Plot results
mcal_data$Species_simple <- gsub("Mcal_", "", mcal_data$Species)
mcal_data$Species_simple <- gsub("No", "", mcal_data$Species_simple)

ggplot(mcal_data, aes(Species_status, Predicted)) +
  geom_point(aes(color=Species_simple), size=3, position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), width=0.3, position=position_dodge(width=0.4)) +
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
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mmus" = colors[1], "Rmeg" = colors[2], "Rrav" = colors[3]),
                     labels=c("Mmus", "Rmeg", "Rrav")) +
  guides(color=guide_legend(title="Species"))

#--------------------------------
#MULTISPECIES OCCUPANCY MODELS

#1. Fit the global model
#a. Define a function to fit the global model (excluding species interactions)
set.seed(500)
starting_values <- rnorm(32, mean = 0, sd = 0.75)                                 #There should be one starting value for every occupancy
                                                                                  #and detection estimate in the model
fit_global_model <- function(umf_list){
  model_list <- list()
  state_formulas <- c("~Dist_urban + Above_MHW + Conn_vars_PC1 + Conn_vars_PC2 + Frag_vars_PC1 + Frag_vars_PC2")
  state_formulas <- c(rep(list(state_formulas), 4))
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(state_formulas),
      detformulas = detformulas,
      control = list(maxit=50000),
      method = "Nelder-Mead",
      maxOrder = 1,
      starts = starting_values,
      data = umf_list[[i]]
    )
  }
  return(model_list)
}

#b. Apply the function
global_model <- fit_global_model(umf_list)             
summary(global_model[[1]])
    #NOTE: Model runs but is a poor fit, try using penalized likelihood

#-----
#2. Fit the model with penalized likelihood (if necessary)
#a. Define a function to fit the model with penalized likelihood
fit_global_model_pen <- function(global_model){
  model_list <- list()
  for(i in seq_along(global_model)){
    model_list[[i]] <- unmarked::optimizePenalty(
      global_model[[i]], 
      penalties = c(0.1, 0.2, 0.33, 1, 2)
    )
  }
  return(model_list)
}

#b. Apply the function to the list of null models - takes a while to run
global_model_pen <- fit_global_model_pen(global_model)
summary(global_model_pen[[1]])

#-----
#3. Dredge the global model to evaluate all possible combinations of covariates         #dredge() does not work with occuMulti :(
dredge_results <- list()
for(i in seq_along(global_model_pen)){
  dredge_results[[i]] <- dredge(global_model_pen[[i]], rank=AIC, m.max=5)
}
summary(dredge_results[[1]])

#b. Combine the dredged models
global_dredge <- do.call(rbind, dredge_results)

#-----
#4. Average models with delta AIC <=2
global_model_avg <- list()
for(i in seq_along(dredge_results)){
  global_model_avg[[i]] <- model.avg(dredge_results[[i]], subset = delta <=2)
}
?pdredge
summary(global_model_avg)

#--------------------------------
#MODEL SELECTION

#1. Select the best-fitting model for interpretation
#a. Create fitList object of models
mods <- fitList(null_models_pen, global_avg)

#b. Generate a model selection table
modSel(mods)

#--------------------------------
#MODEL AVERAGING

#--------------------------------
#PLOT COVARIATE EFFECTS

#1. Plot the effect of covariates on marginal occupancy      NEED TO ADJUST FOR IMPUTED DATASETS
#a. Generate sequence of possible range values
dist_urban_range <- list()
for(i in seq_along(umf_list)){
  urban_range <- range(siteCovs(umf_list[[i]])$Dist_urban)
  dist_urban_range[[i]] <- urban_range
}
dist_urban_range

dist_urban_seq <- list()
for(i in seq_along(umf_list)){
  urban_seq <- seq(dist_urban_range[[i]][1], dist_urban_range[[i]][2], length.out=100)
  dist_urban_seq <- urban_seq
}
dist_urban_seq

#b. Predict marginal occupancy at dist_urban
#1) Rrav
nd <- data.frame(Dist_urban = dist_urban_seq)
occ_urban_rrav <- predict(model, type="response", species="Rrav", newdata=nd)     #REPLACE 'MODEL' WITH BEST PREDICTING MODEL BASED ON AIC 
occ_urban_rrav$Species <- "Rrav"
occ_urban_rrav$Dist_urban <- dist_urban_seq
head(occ_urban_rrav)

#2) Rmeg
occ_urban_rmeg <- predict(model, type="state", species="Rmeg", newdata=nd)

#3) Mmus
occ_urban_mmus <- predict(model, type="state", species="Mmus", newdata=nd)

#4) Mcal
occ_urban_mcal <- predict(model, type="state", species="Mcal", newdata=nd)

#c. Build the plot
plot(occ_dist_rrav$Dist_urban, occ_dist_rrav$Predicted, type = '1', ylim=c(0,0.6),
     col="X", lwd=2, xlab="Distance to urban area (km)", ylab="Marginal occupancy")
lines(occ_dist_rmeg$Dist_urban, occ_dist_rmeg$Predicted, col="Y", lwd=2)
lines(occ_dist_mmus$Dist_urban, occ_dist_mmus$Predicted, col="Z", lwd=2)
lines(occ_dist_mcal$Dist_urban, occ_dist_mcal$Predicted, col="A", lwd=2)
legend('topleft', col=c("X","Y","Z","A"), lty=1,
       legend=c("Rrav","Rmeg","Mmus","Mcal"))

