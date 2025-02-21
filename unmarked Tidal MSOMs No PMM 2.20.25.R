library(unmarked)
library(AICcmodavg)
library(mice)
library(tidyr)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(tidyverse)
library(combinat)
library(reshape2)
library(coda)
library(formattable)
library(ggplot2)
library(car)
library(stats)
library(factoextra)
library(glmnet)
library(MASS)
library(mgcv)
library(caret)
library(parallel)
library(doParallel)
library(foreach)
library(usethis)

#-------------------------------------------------------------------------------
#GET STARTED

#1. Set seed for reproducibility
set.seed(500)

#2. Set working directory 
setwd("C:\\Users\\Kristin\\Documents\\Multispecies Occupancy\\Data Analysis\\R code\\Occupancy Models\\Marsh")

#-------------------------------------------------------------------------------
#DATA MANAGEMENT

#1. Read and manage csv files 
rodent_df <- read.csv("Marsh Master Spreadsheet R.csv") %>%
  dplyr::select(Location, Longitude, Latitude, Type, Area, Dist_urban, Above_MHW, Mcal_conn,
                Mmus_conn, Rrav_conn, Rmeg_conn, Effort, Year) %>%
  mutate(Year = as.factor(ifelse(Year == 2021, 0, 1)),                   #Create a dummy variable for year
         Effort = as.numeric(Effort))                            

station_df <- read.csv("Bait Station Master Spreadsheet.csv") %>%
  dplyr::select(Location, Surveyor, Rrav, Rmeg, Mmus, Mcal)

#----------
#2. Combine into one data frame and reorder columns
rodent <- left_join(rodent_df, station_df, by = "Location") %>%
  dplyr::select(Location, Surveyor, Longitude, Latitude, Rrav, Rmeg, Mmus, Mcal, Type, Area, 
                Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Effort, Year)
rodent

#----------
#3. Create new columns for species detection/non-detection
rodent <- rodent %>%
  group_by(Location) %>%
  mutate(repeat_id = row_number()) %>%
  tidyr::pivot_wider(
    names_from = repeat_id,
    values_from = c(Rrav, Rmeg, Mmus, Mcal),
    names_glue ="{.value}_{repeat_id}"
  ) %>%
  ungroup()
rodent

#----------
#4. Subset tidal sites
rodent <- rodent[rodent$Type == "Tidal", ]
rodent

#----------
#5. Impute missing covariate data with mean values
Above_MHW_mean <- mean(rodent$Above_MHW, na.rm = TRUE)
rodent$Above_MHW[is.na(rodent$Above_MHW)] <- Above_MHW_mean
rodent

#-------------------------------------------------------------------------------
#DATA EXPLORATION

#1. Visualize relationships between independent and dependent variables with logistic regression curves
#a. Add columns collapsing occupancy across site (e.g., 1 if any bait station was occupied, 0 if not)
rodent <- rodent %>%
    mutate(
      Rrav_occupancy = apply(.[, 15:34], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Rmeg_occupancy = apply(.[, 35:54], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Mmus_occupancy = apply(.[, 55:74], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Mcal_occupancy = apply(.[, 75:94], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0))
    )
rodent

#b. Reshape data frame to create a single column for connectivity and occupancy         
rodent <- rodent %>%
    pivot_longer(
      cols = c(Rrav_conn, Rmeg_conn, Mmus_conn, Mcal_conn, 
               Rrav_occupancy, Rmeg_occupancy, Mmus_occupancy, Mcal_occupancy),
      names_to = c("Species", ".value"),
      names_pattern = "(Rrav|Rmeg|Mmus|Mcal)_(conn|occupancy)"
    ) %>%
    rename(Connectivity = conn, Occupancy = occupancy) %>%
    mutate(Species = factor(Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))) %>%
    arrange(Location, Species) %>%
    dplyr::select(Location, Longitude, Latitude, Surveyor, Species, Occupancy, 
                  Type, Area, Dist_urban, Above_MHW, Connectivity, Effort, Year, 
                  matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"), 
                  matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
rodent

#c. Plot each covariate against the response variable (occupancy)
#1) Area
ggplot(rodent, aes(x = Area, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Patch Area",
       x = "Patch Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#2) Distance to urban area
ggplot(rodent, aes(x = Dist_urban, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Distance to Urban Area",
       x = "Distance to Urban Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#3) Inundation
ggplot(rodent, aes(x = Above_MHW, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Inundation",
       x = "Inundation", 
       y = "Occupancy Probability") +
  theme_minimal()

#4) Connectivity   
ggplot(rodent, aes(x = Connectivity, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Connectivity",
       x = "Connectivity", 
       y = "Occupancy Probability") +
  theme_minimal()
    #NOTE: For each species, all covariates exhibit cubic or higher-order 
    #polynomial relationships with occupancy. However, due to difficulty in 
    #interpreting these relationships, select only those that are biologically
    #relevant to test with AIC

#----------
################EXCLUDE???###################
#2. Fit logistic regression models to evaluate transformations
#a. Define the dependent variables, extracting occupancy values for each species
occ_var <- "Occupancy"
species_list <- unique(rodent$Species)

dep_vars <- list()
for(species in species_list){
  species_data <- rodent %>%
    filter(Species == species)
  occupancy <- species_data[[occ_var]]
  dep_vars[[species]] <- occupancy
}

#b. Define the independent variables
covs <- c("Area", "Dist_urban", "Above_MHW", "Connectivity")

#c. Define the transformations
transformations <- c("linear", "log")

#d. Define a function to calculate shifted covariate values for log transformations (log(x), x>0)
calc_shift <- function(data, covariate) {
  min_value <- min(data[[covariate]], na.rm = TRUE)
  if (min_value <= 0) abs(min_value) + 0.01 else 0
}

#e. Define another function to dynamically apply the transformations and assess best fit with AIC
transform_covs <- function(rodent, dep_vars, covs, transformations) {
  results <- list()
  for (species_name in dep_vars){
    species_results <- list()
    for (cov in covs) {
      
      #Calculate shifted covariate values for transformations
      shift_value <- calc_shift(rodent, cov)
      aic_values <- c()
      species_data <- rodent %>%
        filter(Species == species_name)
      for (trans in transformations) {

        #Apply transformations
        if (trans == "log") {
          species_data <- species_data %>%
            mutate(!!sym(cov) := log(!!sym(cov) + shift_value))
        } else if (trans == "linear"){
          species_data <- species_data
        }
          
        #Fit the linear models and extract AIC values
        formula <- as.formula(paste("Occupancy ~", cov))
        model <- glm(formula, data = species_data, family = binomial)
        aic_values <- c(aic_values, AIC(model))
      }
      
      #Calculate delta AIC <=2
      best_aic <- min(aic_values)
      delta_aic <- aic_values - best_aic
      species_results[[cov]] <- data.frame(Transformation = transformations, AIC = aic_values, Delta_AIC = delta_aic)
    }
    results[[species_name]] <- species_results
  }
  return(results)
}

#1) Apply the function
results <- transform_covs(
  rodent = rodent,  
  dep_vars = unique(rodent$Species),
  covs = covs,
  transformations = transformations
)
results
    #Linear connectivity is a better predictor of Rmeg occupancy and log area is a better predictor
    #of Rrav occupancy, but all other independent variables have delta AIC <=2

####################KEEP, EXCLUDE TRANSFORM#########################
#f. Scale and transform independent variables (as needed)
scale_transform <- function(data){
  transformed_data <- data %>%
    mutate(Log_Area = log(Area))
  covs <- c("Area", "Log_Area", "Dist_urban", "Above_MHW", "Connectivity", "Effort")
  rodent <- transformed_data %>%
    mutate(across(all_of(covs),~as.vector(scale(.)))) %>%
    dplyr::select(Location, Longitude, Latitude, Surveyor, Species, Occupancy, Type, 
                  Area, Log_Area, Dist_urban, Above_MHW, Connectivity, Effort, Year, 
                  matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                  matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
  return(rodent)
}
rodent <- scale_transform(rodent)
rodent
####################KEEP, EXCLUDE TRANSFORM#########################

#g. Plot log-transformed variables
#1) Assess non-linearity assumption violation
ggplot(rodent, aes(x = Log_Area, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Log Patch Area",
       x = "Log Patch Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#2) Assess normality (not a strict assumption of MSOMs)
hist(rodent$Area, main = "Area Distribution", xlab = "Area", col = "lightblue")
hist(rodent$Log_Area, main = "Log Area Distribution", xlab = "Log Area", col = "lightblue")

qqnorm(rodent$Area)
qqnorm(rodent$Log_Area)
################EXCLUDE???###################

#----------
#3. Check for collinearity among continuous independent variables using variance inflation factor (VIF)
vif_results <- list()
for(species in species_list){    
  dep_var <- dep_vars[[species]]
  species_data <- rodent %>%
    filter(Species == species)
  formula <- as.formula(paste("dep_var ~", paste(covs, collapse = "+")))
  model <- lm(formula, data = species_data)
  vif_results[[species]] <- car::vif(model)
}
vif_results
    #No collinearity between independent variables, okay to keep all in model

#----------
#4. Explore non-additive effects of interactions
#a. Connectivity*area (connectivity may enhance occupancy in smaller marshes)
#1) Fit the linear models to evaluate the interaction
conn_area_models <- lm(Connectivity ~ Area + Area:Connectivity, data = rodent)

#2) Extract fitted model predictions and coefficients and combine into data frames
extract_preds_coefs <- function(model, rodent){
  coefs <- tidy(model) %>%
    filter(term == "Connectivity:Area")
  preds <- predict(model, newdata = rodent, type = "response")
  pred_data <- rodent %>%
    mutate(prediction = preds)
  return(list(coefs = coefs, preds = pred_data))
}
preds_coefs <- extract_preds_coefs(conn_area_models, rodent)
preds_df <- preds_coefs$preds
coefs_df <- preds_coefs$coefs
coefs_df
    #There is evidence of an interaction (p < 0.05), although estimates are small
    #Plot to confirm

#3) Visualize the interaction as it relates to occupancy, holding one covariate constant
#a) Fit the linear model to the list of imputed data sets
conn_area_models1 <- lapply(species_list, function(species){
  species_data <- subset(rodent, Species == species)
  formula <- as.formula("Occupancy ~ Connectivity*Area")
  model <- lm(formula, data = species_data)
  return(model)
})
names(conn_area_models1) <- species_list
conn_area_models1

#b) Generate a prediction grid for each covariate, fixing the other variable at its mean
area_grid <- expand.grid(
    Area = seq(min(rodent$Area), max(rodent$Area), length.out = 100),
    Connectivity = mean(rodent$Connectivity)
)

conn_grid <- expand.grid(
    Area = mean(rodent$Area),
    Connectivity = seq(min(rodent$Connectivity), max(rodent$Connectivity), length.out = 100)
)

#c) Predict occupancy for each covariate
area_preds <- lapply(seq_along(conn_area_models1), function(i){
  model <- conn_area_models1[[i]]
  preds <- predict(model, newdata = area_grid, type = "response")
  pred_grid <- cbind(area_grid, Prediction = preds, Variable = "Area", Species = species_list[i])
  return(pred_grid)
})

conn_preds <- lapply(seq_along(conn_area_models1), function(i){
  model <- conn_area_models1[[i]]
  preds <- predict(model, newdata = conn_grid, type = "response")
  pred_grid <- cbind(conn_grid, Prediction = preds, Variable = "Connectivity", Species = species_list[i])
  return(pred_grid)
})


#d) Summarize predictions and combine into a data frame
area_preds_df <- do.call(rbind, lapply(seq_along(area_preds), function(i){
  area_preds[[i]] %>%
    group_by(Area, Connectivity, Species, Variable) %>%
    summarize(Prediction = mean(Prediction), .groups = "drop")
}))
area_preds_df

conn_preds_df <- do.call(rbind, lapply(seq_along(conn_preds), function(i){
  conn_preds[[i]] %>%
    group_by(Area, Connectivity, Species, Variable) %>%
    summarize(Prediction = mean(Prediction), .groups = "drop")
}))

#e) Combine the grids and add a column for species name
plot_data <- rbind(area_preds_df, conn_preds_df)
plot_data

#f) Plot the interaction
ggplot(plot_data, aes(x = ifelse(Variable == "Area", Area, Connectivity),
                      y = Prediction, color = Variable)) +
  geom_line() +
  labs(x = "Covariate value", y = "Predicted occupancy", color = "Variable") +
  theme_minimal() +
  facet_wrap(~Species, scales = "free_x", nrow = 10, ncol = 4) +
  theme(strip.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"))
    #The interaction between connectivity and area likely exists, as evidenced by 
    #the intersecting lines. It is weakest for Mcal and the directionality changes 
    #based on the species. 

#-------------------------------------------------------------------------------
#FORMULATE THE DATA

#1. Create detection/non-detection matrices
#a. Extract detection data for each species 
Rrav <- rodent %>%
  dplyr::select(matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"))
Rrav <- as.matrix(Rrav)
Rmeg <- rodent %>%
  dplyr::select(matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"))
Rmeg <- as.matrix(Rmeg)
Mmus <- rodent %>%
  dplyr::select(matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"))
Mmus <- as.matrix(Mmus)
Mcal <- rodent %>%
  dplyr::select(matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
Mcal <- as.matrix(Mcal)

#d. Combine detection data into a named list
ylist <- list(Rrav = Rrav, Rmeg = Rmeg, Mmus = Mmus, Mcal = Mcal)
class(ylist$Rrav)  

#-----
#2. Create data frame of standardized independent variables
#a. Extract variable names
covs <- colnames(rodent)
covs <- covs[c(7:9, 10:14)]        

#b. Define a function to extract variables and convert characters to factor
extract_covs <- function(data, scale_covs){
  covariate_df <- data[, covs, drop = FALSE]
  covariate_df[] <- lapply(names(covariate_df), function(colname) {
    col <- covariate_df[[colname]]
    if (is.character(col)) {
      as.factor(col)
    } else {
      col
    }
  })
  return(covariate_df)
}

#c. Apply the function to each imputed data set
sitecovs <- extract_covs(rodent)
str(sitecovs)

#----------
#3. Create unmarkedFrameOccuMulti objects
#a. Define a function to create the umf objects
create_umf <- function(ylist, site_covs) {
  umf_list <- unmarkedFrameOccuMulti(
    y = ylist, 
    siteCovs = site_covs
  )
  return(umf_list)
}

#b. Apply the function to create a list of umf objects (one for each dataset)
umf_list <- create_umf(ylist, sitecovs)
summary(umf_list)
str(umf_list@siteCovs)
plot(umf_list)

#-------------------------------------------------------------------------------
#ASSIGN FORMULAS

#1. Occupancy
#a. View fDesign matrix of occupancy formulas
umf_fDesign <- umf_list@fDesign
colnames(umf_fDesign)

#b. Create vector of intercept-only occupancy formulas for the 1st and 2nd order parameters
stateformulas <- c("~1","~1","~1","~1","~1","~1","~1","~1","~1","~1") 

#----------
#2. Detection
#a. Create vector of intercept-only detection formulas
detformulas <- c("~1", "~1", "~1", "~1")

#-------------------------------------------------------------------------------
#DETECTION MODELS

#1. Fit detection models
#a. Assign detection covariates
det_vars <- c("Effort", "Year")

#b. Generate all combinations of covariates
det_combos <- expand.grid(
  Effort = c(TRUE, FALSE),
  Year = c(TRUE, FALSE)
)

#c. Define a function to create detection formulas
create_det_formula <- function(combo, covariates){
  combo_logical <- as.logical(unlist(combo))
  terms <- covariates[combo_logical]
  det_formula <- if(length(terms) > 0){
    as.formula(paste("~", paste(terms, collapse = "+")))
  } else {
    ~1
  }
  return(det_formula)
}

#d. Define a function to fit the models 
fit_det_models <- function(umf_list, det_combos, stateformulas){
  det_results <- list()
  for(i in 1:nrow(det_combos)){
    combo <- det_combos[i, ]
    det_formula <- create_det_formula(combo, det_vars)
    model <- occuMulti(
      stateformulas = stateformulas,
      detformulas = as.character(rep(list(det_formula), 4)),
      data = umf_list,
      maxOrder = 2
    )
    det_results[[length(det_results) + 1]] <- list(
      model = model,
      formula = det_formula,
      AIC = model@AIC
    )
  }
  return(det_results)
}

#e. Apply the function
det_models <- fit_det_models(
  umf_list = umf_list,
  det_combos = det_combos,
  stateformulas = stateformulas
)
det_models

#-----
#2. Compare model performance with AIC
#a. Define a function to extract models with delta AIC <=2
id_best_model <- function(det){
  det_aic <- sapply(det, function(x) x$AIC)
  best_aic <- min(det_aic)
  models <- det[det_aic - best_aic <= 2]
  return(models)
}

#b. Apply the function
best_det_model <- id_best_model(det_models)
best_det_model
    #Best fitting model is ~Effort + Year

#-----
#3. Assess goodness-of-fit on model residuals
#a. Extract the model from the best fitting detection model's output
det_model <- best_det_model[[1]]$model
det_model

#b. Define a function to calculate goodness-of-fit (GOF) measures (e.g., Chi-square)
fitstats <- function(model){
  
  #Extract observed detection histories and initiate vectors to store results
  ylist <- model@data@ylist
  obs <- numeric(0)
  exp <- numeric(0)
  
  #Compute the expected and observed sum of rows (i.e., sites) for each species
  for(species in names(ylist)){
    y <- ylist[[species]]
    obs_sum <- rowSums(y)
    exp_probs <- fitted(model)[[species]]
    exp_sum <- rowSums(exp_probs)
    obs <- c(obs, obs_sum)
    exp <- c(exp, exp_sum)
  }
  
  #Pool low-frequency detection histories
  threshold <- 5     #Minimum expected count for Chi-square validity
  if(any(exp < threshold)){
    obs["Other"] <- sum(obs[exp < threshold])
    exp["Other"] <- sum(exp[exp < threshold])
    obs <- obs[exp >= threshold]
    exp <- exp[exp >= threshold]
  }
  
  #Compute and return chi-square statistic
  chisq <- sum((obs-exp)^2/exp, na.rm = TRUE)
  return(c(Chisq = chisq))
}

#c. Initiate parallel computing
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("det_model", "fitstats", "parboot"))
clusterEvalQ(cl, library(unmarked))

#d. Assess GOF for global detection models
det_fit <- parSapply(cl, 1:100, function(i){
  parboot(det_model, fitstats, nsim = 100)
})
stopCluster(cl)

#e. Save the results of the goodness-of-fit test 
save(det_fit, file = "GOF_det_mod.Rdata")
load("GOF_det_mod.Rdata")

#f. Model fit diagnostics (e.g., convergence, parameterization, SEs)
checkConv(det_model)            
extractCN(det_model)         #Does not have excessively high condition numbers, likely not over-parameterized
checkParms(det_model)    
    #Other fit diagnostics look okay, use c-hat = 1.35 during model selection to adjust for overdispersion

#-----
#4. Predict how detection varies with independent variables
#a. Define a range of effort values and years for prediction
effort_range <- seq(min(rodent$Effort), max(rodent$Effort), length.out = 100)
years <- c(0, 1)

#b. Generate prediction detection probabilities for each combination of effort and year
det_preds <- expand.grid(Effort = effort_range, Year = years) %>%
  mutate(Year = as.factor(Year)) %>%
  rowwise() %>%
  mutate(
    Rrav = predict(det_model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Rrav"),
    Rmeg = predict(det_model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Rmeg"),
    Mmus = predict(det_model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Mmus"),
    Mcal = predict(det_model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Mcal")
  ) %>%
  ungroup()
print(det_preds, n = Inf)
    #Detection varies for each species based on Effort and Year, with detection probability 
    #for all species except Mmus being higher in 2020 than 2022

#c. Visualize trends in detection based on effort and year
#1) Define the species
species <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#2) Combine predictions into a single data frame
det_preds_df <- det_preds %>%
  pivot_longer(cols = all_of(species), names_to = "Species", values_to = "Predicted")
det_preds_df

#3) Plot predicted detection probability vs effort for each species and each year
ggplot(det_preds_df, aes(x = Effort, y = Predicted$Predicted, color = as.factor(Year))) +
  geom_line(linewidth = 1) +
  facet_wrap(~Species, scales = "free_y") +
  labs(title = "Detection Probability vs Effort by Species and Year",
       x = "Effort", 
       y = "Detection Probability",
       color = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

#4) Plot predicted detection probability vs year
ggplot(det_preds_df, aes(x = Year, y = Predicted$Predicted, color = as.factor(Year))) +
  geom_boxplot() +
  labs(title = "Detection Probability by Year",
       x = "Year", y = "Predicted Detection Probability") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

#-------------------------------------------------------------------------------
#GLOBAL OCCUPANCY MODEL

#1. Define the independent variables
model_covs <- c("Area", "Dist_urban", "Above_MHW", "Connectivity", "Connectivity:Area")

#-----
#2. Fit the model
#a. Define a function to fit the model
fit_global_models <- function(umf_list){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(rep(state_formula, 10)),
      detformulas = as.character(rep("~Effort + Year", 4)),
      control = list(maxit = 5000),
      maxOrder = 2,
      starts = rnorm(72, mean = 0, sd = 0.1),
      data = umf_list[[i]]
    )
  }
  return(model_list)
}

#b. Apply the function
global_models <- fit_global_models(umf_list)
save(global_models, file = "Global_models.Rdata")
load("Global_models.Rdata")
summary(global_models[[1]])
#Several estimates and SEs are large, use penalized likelihood

#-----
#3. Fit the model with penalized likelihood
#a. Define a function to penalize the models
penalize_model <- function(models){
  pen_model_results <- list()
  for(i in seq_along(models)){
    pen_model_results[[i]] <- optimizePenalty(
      models[[i]],
      penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
    )
  }
  return(pen_model_results)
}

#b. Apply the function
global_pen_models <- lapply(global_models, penalize_model)
save(global_pen_models, file = "Global_pen_models.Rdata")
load("Global_pen_models.Rdata")

mean(sapply(global_models, function(model) model@AIC))
mean(sapply(global_pen_models, function(model) model@AIC))
#Unpenalized may be better fit with lower AIC (8998 vs 9051)

#-----
#4. Assess goodness-of-fit on model residuals
#a. Flatten the list of models
global_pen_models_flat <- unlist(global_pen_models, recursive = FALSE)
global_models_flat <- unlist(global_models, recursive = FALSE)

#b. Initiate parallel computing - this needs to be reinitialized for each parallel computing segment
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_models_flat", "fitstats", "parboot"))
clusterEvalQ(cl, library(unmarked))

#c. Compute GOF measures with parametric bootstrapping and save the results
global_fit <- parLapply(cl, global_models_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)
save(global_fit, file = "GOF_global_models.Rdata")
load("GOF_global_models.Rdata")

#d. Compute and pool c-hat across imputed models
global_fit_pooled <- pool_fitstats(global_fit)
global_fit_pooled
#Model is moderately overdispersed (Chi-square p = 0, c-hat = 1.85), check other fit diagnostics

#e. Assess other model fit diagnostics (e.g., convergence, parameterization, SEs)
checkConv(global_models[[1]])            
sapply(global_models, extractCN)         #Condition numbers are relatively high, but model is likely not overparameterized
lapply(global_models, checkParms)
#Other fit diagnostics look okay, use c-hat to account for overdispersion


##########
#TEST: PENALIZED MODEL GOF - WHICH IS A BETTER FIT TO THE DATA, PENALIZED OR NON-PENALIZED??
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("global_pen_models_flat", "fitstats", "parboot"))
clusterEvalQ(cl, library(unmarked))

global_pen_fit <- parLapply(cl, global_pen_models_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)
save(global_pen_fit, file = "GOF_global_pen_models.Rdata")
load("GOF_global_pen_models.Rdata")

global_pen_fit_pooled <- pool_fitstats(global_pen_fit)
global_pen_fit_pooled

checkConv(global_pen_models[[1]])            
sapply(global_pen_models, extractCN)         #Condition numbers are relatively high, but model is likely not overparameterized
lapply(global_pen_models, checkParms)
##########

#-----
#DO DURING MODEL SELECTION INSTEAD??
#5. Account for overdispersion with quasi-likelihood adjustment
global_quasi_results <- lapply(global_pen_models, function(model){
  summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
})
global_quasi_results
global_pen_models

#-----
#NOT APPROPRIATE BECAUSE MODELS ARE NON-PARAMETRIC DUE TO PENALIZATION AND QUASI-ADJUSTMENT - EXCLUDE???
#6. Pool the results with Rubin's rules
#a. Define a function to pool the results of the quasi-adjusted models
pool_quasi <- function(quasi_list){
  estimates <- sapply(quasi_list, function(model) model$outMat[,1])
  SE <- sapply(quasi_list, function(model) model$outMat[,2])
  
  #Calculate variance
  within_var <- rowMeans(SE^2)
  between_var <- apply(SE, 1, var)
  total_var <- within_var + (1+1/length(quasi_list))*between_var
  
  #Pool results
  pooled_estimate <- rowMeans(estimates)
  pooled_SE <- sqrt(total_var)
  pooled_z <- pooled_estimate / pooled_SE
  pooled_p <- 2*(1-pnorm(abs(pooled_z)))
  pooled_lower <- pooled_estimate - 1.96*pooled_SE
  pooled_upper <- pooled_estimate + 1.96*pooled_SE
  
  #Return the pooled results in a data frame
  data.frame(
    Estimate = pooled_estimate,
    SE = pooled_SE, 
    Z = pooled_z,
    p_value = pooled_p,
    Lower_CI = pooled_lower, 
    Upper_CI = pooled_upper
  )
}

#b. Apply the function
pooled_global_quasi_results <- pool_quasi(global_quasi_results)
pooled_global_quasi_results
#Area, Connectivity, and Area:Connectivity positively affect occupancy of Rrav:Rmeg
#Effort negatively affects Mmus detection
#Year negatively affects Rmeg and Mcal detection

#-------------------------------------------------------------------------------
#NULL OCCUPANCY MODEL

#1. Fit the model
#a. Define a function to fit the model
fit_null_models <- function(umf_list, stateformulas, detformulas){
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = stateformulas, 
      detformulas = detformulas,
      data = umf_list[[i]],
      maxOrder = 2
    )
  }
  return(model_list)
}

#b. Apply the function
null_models <- fit_null_models(umf_list, stateformulas, detformulas)              
summary(null_models[[1]])

#-----
#2. Account for overdispersion with quasi-likelihood adjustment (using c-hat from global model)
null_quasi_results <- lapply(null_models, function(model){
  summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
})
null_quasi_results

#-----
#NOT ACCEPTABLE??
#3. Pool results with Rubin's rules for variance estimation
null_results <- pool_quasi(null_quasi_results)
null_results
#Notable findings:
#1. Mcal occupancy and detectability is the lowest of the four species
#2. Rrav and Rmeg are not likely to occupy the same sites
#3. Rrav:Mmus and Rmeg:Mcal are likely to occupy the same sites
#4. Detection is highest for Rrav but followed closely by Mmus

#-----
#4. Back-transform to get occupancy and detection estimates
#a. Occupancy
null_occ <- list()
for(i in seq_along(null_quasi_results)){
  
  #Get occupancy estimates
  occ_estimates <- null_models[[i]]@estimates@estimates$state
  estimates <- occ_estimates@estimates
  
  #Get confidence intervals
  occ_confint <- confint(null_models[[i]], type = "state")
  
  #Back-transform the estimates and confidence intervals
  back_estimates <- plogis(estimates)
  back_lower <- plogis(occ_confint[,1])
  back_upper <- plogis(occ_confint[,2])
  
  #Combine into a data frame
  null_occ[[i]] <- data.frame(
    Estimate = back_estimates,
    Lower_CI = back_lower,
    Upper_CI = back_upper
  )
}
null_occ
#Rrav occupancy is 0.40, Rmeg is 0.38, Mmus is 0.49, Mcal is 0.15

#b. Detection
null_det <- list()
for(i in seq_along(null_quasi_results)){
  
  #Get detection estimates
  det_estimates <- null_models[[i]]@estimates@estimates$det
  estimates <- det_estimates@estimates[1:4]
  
  #Get confidence intervals
  det_confint <- confint(null_models[[i]], type = "det")
  
  #Back-transform the estimates and confidence intervals
  back_estimates <- plogis(estimates)
  back_lower <- plogis(det_confint[,1])
  back_upper <- plogis(det_confint[,2])
  
  #Combine into a data frame
  null_det[[i]] <- data.frame(
    Estimate = back_estimates,
    Lower_CI = back_lower,
    Upper_CI = back_upper
  )
  rownames(null_det[[i]]) <- rownames(det_estimates)
}
null_det
#Rrav detection is 0.38, Rmeg is 0.30, Mmus is 0.36, Mcal is 0.11
#Detection probabilities are low, consider modeling with covariates (e.g., Effort, Year)

#-------------------------------------------------------------------------------
#UNIVARIATE OCCUPANCY MODELS

#1. Fit the model
#a. Define a function to fit the model
fit_uni_models <- function(model_covs, umf_list){
  models <- list()
  for(cov in model_covs){
    state_formula <- paste("~", cov)
    cov_models <- list()
    for(i in seq_along(umf_list)){
      cov_models[[i]] <- occuMulti(
        stateformulas = as.character(rep(state_formula, 10)),
        detformulas = as.character(rep("~Effort + Year", 4)),
        control = list(maxit = 5000),
        maxOrder = 2,
        starts = rnorm(32, mean = 0, sd = 0.1),
        data = umf_list[[i]]
      )
    }
    models[[cov]] <- cov_models
  }
  return(models)
}

#b. Apply the function
uni_models <- fit_uni_models(model_covs, umf_list)
save(uni_models, file = "Uni_models.Rdata")
load("Uni_models.Rdata")

#-----
#2. Adjust for overdispersion
#a. Apply the adjustment by specifying c-hat (from global model)
quasi_uni_results <- lapply(uni_models, function(model_list){
  lapply(model_list, function(model){
    summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
  })
})
quasi_uni_results

#-----
#3. Pool the adjusted results with Rubin's rules
#a. Flatten the quasi-adjusted model output 
quasi_uni_list <- unlist(quasi_uni_results, recursive = FALSE)

#b. Define a function to pool outputs for each independent variable
pool_uni <- function(quasi_uni_list, pool_quasi){
  results <- list()
  
  #Extract and group the quasi output by independent variable
  name <- unique(sub("\\d+$", "", names(quasi_uni_list)))
  uni_cov <- setNames(lapply(name, function(cov){
    quasi_uni_list[grep(paste0("^", cov), names(quasi_uni_list))]
  }), name)
  
  #Pool the results
  for(cov in names(uni_cov)){
    cov_data <- uni_cov[[cov]]
    pooled_results <- pool_quasi(cov_data)
    results[[cov]] <- pooled_results
  }
  return(results)
}

#c. Apply the function
uni_results <- pool_uni(quasi_uni_list, pool_quasi)
uni_results

#-------------------------------------------------------------------------------
#BIVARIATE MODELS

#1. Create a list of bivariate combinations from the vector of independent variables
bi_combos <- combinat::combn(model_covs, 2, simplify = FALSE)

#-----
#2. Fit the models
#a. Define a function to fit the model
fit_bi_models <- function(bi_combos, umf_list){
  models <- list()
  for(combo in bi_combos){
    state_formula <- paste("~", paste(combo,  collapse = "+"))
    combo_name <- paste(combo, collapse = "+")
    cov_models <- list()
    for(i in seq_along(umf_list)){
      cov_models[[i]] <- occuMulti(
        stateformulas = as.character(rep(state_formula, 10)),
        detformulas = as.character(rep("~Effort + Year", 4)),
        control = list(maxit = 5000),
        maxOrder = 2,
        starts = rnorm(42, mean = 0, sd = 0.15),
        data = umf_list[[i]]
      )
    }
    models[[combo_name]] <- cov_models
  }
  return(models)
}

#b. Apply the function
bi_models <- fit_bi_models(bi_combos, umf_list)
save(bi_models, file = "Bi_models.Rdata")
load("Bi_models.Rdata")
#Several estimates and SEs are large, use penalized likelihood

#-----
#3. Fit the model with penalized likelihood
bi_pen_models <- lapply(bi_models, penalize_model)
save(bi_pen_models, file = "Bi_pen_models.Rdata")
load("Bi_pen_models.Rdata")

#-----
#4. Adjust for overdispersion
#a. Apply the adjustment by specifying c-hat (from global model)
quasi_bi_results <- lapply(bi_pen_models, function(model_list){
  lapply(model_list, function(model){
    summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
  })
})
quasi_bi_results

#-----
#5. Pool the adjusted results with Rubin's rules
#a. Flatten the quasi-adjusted model output 
quasi_bi_list <- unlist(quasi_bi_results, recursive = FALSE)
names(quasi_bi_list)

#b. Define a function to pool outputs for each independent variable
pool_bi <- function(quasi_bi_list, pool_quasi){
  results <- list()
  
  #Extract and group the quasi output by independent variable
  bi_names <- unique(sub("\\d+$", "", names(quasi_bi_list)))
  bi_pair <- setNames(lapply(bi_names, function(pair) {
    quasi_bi_list[grep(paste0("^", gsub("\\+", "\\\\+", pair), "[0-9]+$"), 
                       names(quasi_bi_list), value = TRUE)]
  }), bi_names)
  
  #Pool the results
  for(pair in names(bi_pair)){
    cov_data <- bi_pair[[pair]]
    pooled_results <- pool_quasi(cov_data)
    results[[pair]] <- pooled_results
  }
  return(results)
}

#c. Apply the function
bi_results <- pool_bi(quasi_bi_list, pool_quasi)
bi_results

#-------------------------------------------------------------------------------
#TRIVARIATE MODELS

#1. Create a list of trivariate combinations from the list of independent variables
tri_combos <- combinat::combn(model_covs, 3, simplify = FALSE)

#-----
#2. Fit the models
#a. Define a function to fit the model
fit_tri_models <- function(tri_combos, umf_list){
  models <- list()
  for(combo in tri_combos){
    state_formula <- paste("~", paste(combo,  collapse = "+"))
    combo_name <- paste(combo, collapse = "+")
    cov_models <- list()
    for(i in seq_along(umf_list)){
      cov_models[[i]] <- occuMulti(
        stateformulas = as.character(rep(state_formula, 10)),
        detformulas = as.character(rep("~Effort + Year", 4)),
        control = list(maxit = 5000),
        maxOrder = 2,
        starts = rnorm(52, mean = 0, sd = 0.15),
        data = umf_list[[i]]
      )
    }
    models[[combo_name]] <- cov_models
  }
  return(models)
}

#b. Apply the function
tri_models <- fit_tri_models(tri_combos, umf_list)
save(tri_models, file = "Tri_models.Rdata")
load("Tri_models.Rdata")

#-----
#3. Fit the model with penalized likelihood
tri_pen_models <- lapply(tri_models, penalize_model)
save(tri_pen_models, file = "Tri_pen_models.Rdata")
load("Tri_pen_models.Rdata")

#-----
#4. Adjust for overdispersion
#a. Apply the adjustment by specifying c-hat (from global model)
quasi_tri_results <- lapply(tri_pen_models, function(model_list){
  lapply(model_list, function(model){
    summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
  })
})
quasi_tri_results

#-----
#5. Pool the adjusted results with Rubin's rules
#a. Flatten the quasi-adjusted model output 
quasi_tri_list <- unlist(quasi_tri_results, recursive = FALSE)
names(quasi_tri_list)

#b. Define a function to pool outputs for each independent variable
pool_tri <- function(quasi_tri_list, pool_quasi){
  results <- list()
  
  #Extract and group the quasi output by independent variable
  tri_names <- unique(sub("\\d+$", "", names(quasi_tri_list)))
  tri_combo <- setNames(lapply(tri_names, function(tri) {
    quasi_tri_list[grep(paste0("^", gsub("\\+", "\\\\+", tri), "[0-9]+$"), 
                        names(quasi_tri_list), value = TRUE)]
  }), tri_names)
  
  #Pool the results
  for(tri in names(tri_combo)){
    cov_data <- tri_combo[[tri]]
    pooled_results <- pool_quasi(cov_data)
    results[[tri]] <- pooled_results
  }
  return(results)
}

#c. Apply the function
tri_results <- pool_tri(quasi_tri_list, pool_quasi)
tri_results

#-------------------------------------------------------------------------------
#QUADVARIATE MODELS

#1. Create a list of covariate combinations from the list of independent variables
quad_combos <- combinat::combn(model_covs, 4, simplify = FALSE)

#-----
#2. Fit the models
#a. Define a function to fit the model
fit_quad_models <- function(quad_combos, umf_list){
  models <- list()
  for(combo in quad_combos){
    state_formula <- paste("~", paste(combo,  collapse = "+"))
    combo_name <- paste(combo, collapse = "+")
    cov_models <- list()
    for(i in seq_along(umf_list)){
      cov_models[[i]] <- occuMulti(
        stateformulas = as.character(rep(state_formula, 10)),
        detformulas = as.character(rep("~Effort + Year", 4)),
        control = list(maxit = 5000),
        maxOrder = 2,
        starts = rnorm(62, mean = 0, sd = 0.15),
        data = umf_list[[i]]
      )
    }
    models[[combo_name]] <- cov_models
  }
  return(models)
}

#b. Apply the function
quad_models <- fit_quad_models(quad_combos, umf_list)
save(quad_models, file = "Quad_models.Rdata")
load("Quad_models.Rdata")

#-----
#3. Fit the model with penalized likelihood
quad_pen_models <- lapply(quad_models, penalize_model)
save(quad_pen_models, file = "Quad_pen_models.Rdata")
load("Quad_pen_models.Rdata")

#-----
#4. Adjust for overdispersion
#a. Apply the adjustment by specifying c-hat (from global model)
quasi_quad_results <- lapply(quad_pen_models, function(model_list){
  lapply(model_list, function(model){
    summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
  })
})
quasi_quad_results

#-----
#5. Pool the adjusted results with Rubin's rules
#a. Flatten the quasi-adjusted model output 
quasi_quad_list <- unlist(quasi_quad_results, recursive = FALSE)
names(quasi_quad_list)

#b. Define a function to pool outputs for each independent variable
pool_quad <- function(quasi_quad_list, pool_quasi){
  results <- list()
  
  #Extract and group the quasi output by independent variable
  quad_names <- unique(sub("\\d+$", "", names(quasi_quad_list)))
  quad_combo <- setNames(lapply(quad_names, function(quad) {
    quasi_quad_list[grep(paste0("^", gsub("\\+", "\\\\+", quad), "[0-9]+$"), 
                         names(quasi_quad_list), value = TRUE)]
  }), quad_names)
  
  #Pool the results
  for(quad in names(quad_combo)){
    cov_data <- quad_combo[[quad]]
    pooled_results <- pool_quasi(cov_data)
    results[[quad]] <- pooled_results
  }
  return(results)
}

#c. Apply the function
quad_results <- pool_quad(quasi_quad_list, pool_quasi)
quad_results

#-------------------------------------------------------------------------------
#MODEL SELECTION

#1. Select the best-fitting model(s) for interpretation
#ba Store models in a list
models <- list(null_models, global_models, global_det_model)

#c. Assign names to each model and store in a list
modnames <- c("null_model", "det_model", "global_model", "uni_model", "bi_model", "tri_model", "quad_model")
modlist <- list("null_model" = null_model, "det_model" = det_model, "global_model" = global_model,
                "uni_model" = uni_model, "bi_model" = bi_model, "tri_model" = tri_model, "quad_model" = quad_model) 

#d. Perform model selection 
top_models <- aictab(cand.set = modlist, c.hat = 1.77)

#2. Compare top fitting models (if more than one)
#a. Use evidence ratio of Aikake weights to compare models
evidence(aic.table = top_models)

#b. Assess effect of significant parameters across the entire model set using model-averaging shrinkage estimator
#1) On occupancy
estX <- modavgShrink(cand.set = modlist, parm = "X", parm.type = "psi", c.hat = 1.77)     #Repeat for other significant predictors

#2) On detection
estY <- modavgShrink(cand.set = modelist, parm = "Y", parm.type = "detect", c.hat = 1.77)        #Repeat for other significant predictors

#-------------------------------------------------------------------------------
#PLOT COVARIATE EFFECTS

#1. Plot the effect of the independent variables on marginal occupancy      
#a. Review the occupancy variables
model_covs

#b. Explore covariate values
#1) Define a function to generate a range of possible covariate values
extract_range <- function(umf_list, model_covs){
  range <- list()
  for(cov in model_covs){
    cov_range <- list()
    for(i in seq_along(umf_list)){
      cov_range[[i]] <- range(siteCovs(umf_list[[i]])[[cov]], na.rm = TRUE)
    }
    range[[cov]] <- cov_range
  }
  return(list(ranges = range))
}

#2) Apply the function
range <- extract_range(umf_list, model_covs)

#c. Explore covariate values based on the range
#1) Define a function to generate a sequence of possible covariate values based on the range
extract_seq <- function(umf_list, model_covs){
  seq <- list()
  for(cov in model_covs){
    seq_range <- list()
    for(i in seq_along(umf_list)){
      cov_seq[[i]] <- seq(range[[i]][1], range[[i]][2], length.out = 100)
    }
    seq[[cov]] <- cov_seq
  }
  return(list(sequences = seq))
}

#2) Apply the function
seq <- extract_seq(umf_list, model_covs)

#d. Assess how changes in a single variable affect occupancy
#1) Define a function to generate new data
generate_data <- function(umf_list, model_covs, range, seq){
  new_data_list <- list()
  for(cov in model_covs){
    new_data <- data.frame()
    for(i in seq_along(umf_list)){
      cov_seq <- seq[[cov]][[i]]
      mean_vals <- sapply(model_covs, function(var){
        if(var != cov){
          return(ean(siteCovs(umf_list[[i]])[[var]], na.rm = TRUE))
        } else {
          return(NULL)
        }
      })
      temp_date <- data.frame(cov = cov_seq)
      temp_data <- cbind(temp_data, as.data.frame(mean_vals))
      colnames(temp_data) <- c(cov, model_covs[model_covs != cov])
      new_data_list[[cov]] <- temp_data
    }
  }
  return(new_data_list)
}

#2) Apply the function
nd <- generate_data(umf_list, model_covs, range, seq)

#e. Predict species occupancy in relation to each independent variable
#1) Define a function to predict occupancy
predict_occ <- function(model, umf_list, species_list, model_covs, range, seq){
  preds <- list()
  for(species in species_list){
    species_preds <- data.frame()
    for(cov in model_covs){
      temp_data <- new_data[[cov]]
      temp_data$Species <- species
      occ_preds <- predict(model, type = "response", species = species, newdata = temp_data)
      temp_data$Predicted_Occupancy <- occ_preds
      species_preds <- rbind(species_preds, temp_data)
    }
    preds[[species]] <- species_preds
  }
  return(preds)
}

#2) Apply the function and view predictions for all species
species_preds <- predict_occ(model, species_list, model_covs, umf_list, nd)
species_preds$Rrav
species_preds$Rmeg
species_preds$Mmus
species_preds$Mcal

#f. Build the plot
plot(occ_dist_rrav$Dist_urban, occ_dist_rrav$Predicted, type = '1', ylim = c(0,0.6),
     col = "X", lwd = 2, xlab = "Distance to urban area (km)", ylab = "Marginal occupancy")
lines(occ_dist_rmeg$Dist_urban, occ_dist_rmeg$Predicted, col = "Y", lwd = 2)
lines(occ_dist_mmus$Dist_urban, occ_dist_mmus$Predicted, col = "Z", lwd = 2)
lines(occ_dist_mcal$Dist_urban, occ_dist_mcal$Predicted, col = "A", lwd = 2)
legend('topleft', col = c("X","Y","Z","A"), lty = 1,
       legend = c("Rrav","Rmeg","Mmus","Mcal"))

#-----
#2. Plot the effect of covariates on conditional occupancy
#a. Calculate species occupancy when another species is present, repeating for each covariate

#-------------------------------------------------------------------------------
#CALCULATE CONDITIONAL OCCUPANCY PROBABILITIES

#1. Predict the probability of occupancy of one species conditional on another species' presence 
cond_occ <- lapply(null_models, function(model){
  rrav_rmeg <- predict(model, type = "state", species = "Rrav", cond = "Rmeg")
  rrav_mmus <- predict(model, type = "state", species = "Rrav", cond = "Mmus")
  rrav_mcal <- predict(model, type = "state", species = "Rrav", cond = "Mcal")
  rmeg_rrav <- predict(model, type = "state", species = "Rmeg", cond = "Rrav")
  rmeg_mmus <- predict(model, type = "state", species = "Rmeg", cond = "Mmus")
  rmeg_mcal <- predict(model, type = "state", species = "Rmeg", cond = "Mcal")
  mmus_rrav <- predict(model, type = "state", species = "Mmus", cond = "Rrav")
  mmus_rmeg <- predict(model, type = "state", species = "Mmus", cond = "Rmeg")
  mmus_mcal <- predict(model, type = "state", species = "Mmus", cond = "Mcal")
  mcal_rrav <- predict(model, type = "state", species = "Mcal", cond = "Rrav")
  mcal_rmeg <- predict(model, type = "state", species = "Mcal", cond = "Rmeg")
  mcal_mmus <- predict(model, type = "state", species = "Mcal", cond = "Mmus")
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
                    SE = mean_pred["SE"], Lower_CI = mean_pred["lower"], Upper_CI = mean_pred["upper"]))
}))
cond_occ_pooled$Species <- factor(cond_occ_pooled$Species, levels = c("Rrav_Rmeg", "Rrav_Mmus", "Rrav_Mcal", "Rmeg_Rrav", "Rmeg_Mmus", "Rmeg_Mcal",
                                                                      "Mmus_Rrav", "Mmus_Rmeg", "Mmus_Mcal", "Mcal_Rrav", "Mcal_Rmeg", "Mcal_Mmus"))
cond_occ_pooled
#Mcal is neutrally associated with Mmus (40%) and Rmeg (51%) and negatively associated with Rrav (33%)
#Mmus is positively associated with all species (Mcal: 80%, Rmeg: 73%, Rrav: 77%)
#Rmeg is positively associated with Mcal (75%) and neutrally with Mmus (54%) and Rrav (40%) 
#Rrav is neutrally associated with all species (Mcal: 50%, Mmus: 57%, Rmeg: 41%) 

#----------
#2. Predict the probability of occupancy of one species conditional on another species' absence 
abs_occ <- lapply(null_models, function(models){
  rrav_normeg <- predict(model, type = "state", species = "Rrav", cond = "-Rmeg")
  rrav_nommus <- predict(model, type = "state", species = "Rrav", cond = "-Mmus")
  rrav_nomcal <- predict(model, type = "state", species = "Rrav", cond = "-Mcal")
  rmeg_norrav <- predict(model, type = "state", species = "Rmeg", cond = "-Rrav")
  rmeg_nommus <- predict(model, type = "state", species = "Rmeg", cond = "-Mmus")
  rmeg_nomcal <- predict(model, type = "state", species = "Rmeg", cond = "-Mcal")
  mmus_norrav <- predict(model, type = "state", species = "Mmus", cond = "-Rrav")
  mmus_normeg <- predict(model, type = "state", species = "Mmus", cond = "-Rmeg")
  mmus_nomcal <- predict(model, type = "state", species = "Mmus", cond = "-Mcal")
  mcal_norrav <- predict(model, type = "state", species = "Mcal", cond = "-Rrav")
  mcal_normeg <- predict(model, type = "state", species = "Mcal", cond = "-Rmeg")
  mcal_nommus <- predict(model, type = "state", species = "Mcal", cond = "-Mmus")
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
                    SE = mean_pred["SE"], Lower_CI = mean_pred["lower"], Upper_CI = mean_pred["upper"]))
}))
abs_occ_pooled$Species <- factor(abs_occ_pooled$Species, levels = c("Rrav_NoRmeg", "Rrav_NoMmus", "Rrav_NoMcal", "Rmeg_NoRrav", "Rmeg_NoMmus", "Rmeg_NoMcal",
                                                                    "Mmus_NoRrav", "Mmus_NoRmeg", "Mmus_NoMcal", "Mcal_NoRrav", "Mcal_NoRmeg", "Mcal_NoMmus"))
abs_occ_pooled
#Mcal is not likely to be present when Mmus (21%), Rmeg (17%), and Rrav (34%) are absent
#Mmus occupancy is not associated with Mcal (61%), Rmeg (62%), or Rrav (59%) absence
#Rmeg is not likely to be present when Mcal is absent (37%), but there is no association with Rrav (60%) or Mmus (41%) absence
#Rrav is not likely to be present when Mmus (37%) is absent and occupancy is not associated with Mcal (51%) or Rmeg (61%) absence

#----------
#3. Visualize naive conditional occupancy of all species pairs
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
    Lower_CI = c(pred_cond_occ$lower, pred_abs_occ$lower),
    Upper_CI = c(pred_cond_occ$upper, pred_abs_occ$upper)
  )
  rrav_list[[pair]] <- combined
}
rrav_list

#4) Combine results in a data frame
rrav_data <- do.call(rbind, rrav_list)
rrav_data$Species_status <- c("Present", "Absent", "Present", "Absent", "Present", "Absent")
rrav_data

#5) Plot results
colors <- c("#2E4D2E", "#7E8050", "#BA8D61", "#BF6534")

rrav_data$Species_simple <- gsub("Rrav_", "", rrav_data$Species)
rrav_data$Species_simple <- gsub("No", "", rrav_data$Species_simple)

ggplot(rrav_data, aes(Species_status, Predicted)) +
  geom_point(aes(color = Species_simple), size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, color = Species_simple), linewidth = 0.8, width = 0.25, position = position_dodge(width = 0.4)) +
  ylim(0,1) +
  labs(x = "Species status", y = "Conditional occupancy probability") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values = c("Mcal" = colors[4], "Rmeg" = colors[2], "Mmus" = colors[3]),
                     labels = c("Mcal", "Mmus", "Rmeg")) +
  guides(color = guide_legend(title = "Species"))

#-----
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
    Lower_CI = c(pred_cond_occ$lower, pred_abs_occ$lower),
    Upper_CI = c(pred_cond_occ$upper, pred_abs_occ$upper)
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
  geom_point(aes(color = Species_simple), size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, color = Species_simple), linewidth = 0.8, width = 0.3, position = position_dodge(width = 0.4)) +
  ylim(0,1) +
  labs(x = "Species status", y = "Conditional occupancy probability") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values = c("Mcal" = colors[4], "Mmus" = colors[3], "Rrav" = colors[1]),
                     labels = c("Mcal", "Mmus", "Rrav")) +
  guides(color = guide_legend(title="Species"))

#-----
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
    Lower_CI = c(pred_cond_occ$lower, pred_abs_occ$lower),
    Upper_CI = c(pred_cond_occ$upper, pred_abs_occ$upper)
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
  geom_point(aes(color = Species_simple), size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, color = Species_simple), linewidth = 0.8, width = 0.3, position = position_dodge(width = 0.4)) +
  ylim(0,1) +
  labs(x = "Species status", y = "Conditional occupancy probability") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values = c("Mcal" = colors[4], "Rmeg" = colors[2], "Rrav" = colors[1]),
                     labels = c("Mcal", "Rmeg", "Rrav")) +
  guides(color = guide_legend(title = "Species"))

#-----
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
    Lower_CI = c(pred_cond_occ$lower, pred_abs_occ$lower),
    Upper_CI = c(pred_cond_occ$upper, pred_abs_occ$upper)
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
  geom_point(aes(color = Species_simple), size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, color = Species_simple), linewidth = 0.8, width = 0.3, position = position_dodge(width = 0.4)) +
  ylim(0,1) +
  labs(x = "Species status", y = "Conditional occupancy probability") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values = c("Mmus" = colors[3], "Rmeg" = colors[2], "Rrav" = colors[1]),
                     labels = c("Mmus", "Rmeg", "Rrav")) +
  guides(color=guide_legend(title = "Species"))

#-------------------------------------------------------------------------------
#CALCULATE PREDICTED OCCUPANCY AND DETECTION PROBABILITIES

#1. Occupancy
#a. Predict probability for each occupancy state
null_occ_probs <- lapply(null_models, function(model){
  all_probs <- predict(model, type = "state")
  return(all_probs)
})
null_occ_probs[[1]]$Predicted

#b. Calculate predicted marginal occupancy (across all possible occupancy states) for each species
null_occ_preds <- lapply(null_models, function(model){
  rrav_occ <- predict(model, type = "state", species = "Rrav")
  rmeg_occ <- predict(model, type = "state", species = "Rmeg")
  mmus_occ <- predict(model, type = "state", species = "Mmus")
  mcal_occ <- predict(model, type = "state", species = "Mcal")
  all_occ <- rbind(rrav_occ[1,], rmeg_occ[1,], mmus_occ[1,], mcal_occ[1,])
  all_occ$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_occ)
})
null_occ_preds[[1]]$Predicted

#c. Pool the results
null_occ_pooled <- do.call(rbind, lapply(split(do.call(rbind, null_occ_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    Lower_CI = mean_pred["lower"], Upper_CI = mean_pred["upper"]))
}))
null_occ_pooled$Species <- factor(null_occ_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
null_occ_pooled
#Rrav = 0.507, lower = 0.454, upper = 0.559
#Rmeg = 0.500, lower = 0.442, upper = 0.555
#Mmus = 0.678, lower = 0.618, upper = 0.726
#Mcal = 0.342, lower = 0.274, upper = 0.416

#d. Visualize marginal occupancy for all species
level_order <- c("Rrav", "Rmeg", "Mmus", "Mcal")

ggplot(null_occ_pooled, aes(x = factor(Species, level = level_order), y = Predicted)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.3) +
  ylim(0.2,1) +
  labs(x = "Species", y = "Marginal occupancy and 95% CI") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

#-----
#2. Detection
#a. Calculate predicted marginal detection for each species
null_det_preds <- lapply(null_models, function(model){
  rrav_det <- predict(model, type = "det", species = "Rrav")
  rmeg_det <- predict(model, type = "det", species = "Rmeg")
  mmus_det <- predict(model, type = "det", species = "Mmus")
  mcal_det <- predict(model, type = "det", species = "Mcal")
  all_det <- rbind(rrav_det[1,], rmeg_det[1,], mmus_det[1,], mcal_det[1,])
  all_det$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_det)
})
null_det_preds[[1]]$Predicted

#b. Pool the results
null_det_pooled <- do.call(rbind, lapply(split(do.call(rbind, null_det_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    Lower_CI = mean_pred["lower"], Upper_CI = mean_pred["upper"]))
}))
null_det_pooled$Species <- factor(null_det_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
null_det_pooled
#Rrav = 0.380, lower = 0.361, upper = 0.400
#Rmeg = 0.271, lower = 0.252, upper = 0.290
#Mmus = 0.367, lower = 0.350, upper = 0.384
#Mcal = 0.094, lower = 0.075, upper = 0.116

#c. Visualize detection probabilities for all species
ggplot(null_det_pooled, aes(x = factor(Species, level = level_order), y = Predicted)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.3) +
  ylim(0.05,1) +
  labs(x = "Species", y = "Detection probability and 95% CI") +
  theme(axis.title.y = element_text(size = 14, vjust = 4),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.text.x = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 14),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)) +
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
