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
#5. Impute missing covariate data
#a. Subset the columns with missing data and convert to numeric (if needed)
rodent_sub <- rodent[,c(1, 8)]

#b. Assign imputation methods (predictive mean matching)
method <- c("", "pmm")              

#c. Impute the data
imp <- mice(rodent_sub, method = method, m = 10, maxit = 30, print = F)
summary(imp)

#d. Perform diagnostics and summarize imputation effectiveness with plots
plot(imp, layout = c(1,2))   
mice::stripplot(imp, Above_MHW ~.imp, pch = 20, cex = 2)
#All imputed values (in red) are among observed values (in blue) and are therefore plausible

#e. Extract imputed data sets
imputed <- lapply(1:imp$m, function(i) complete(imp, i))

#f. Combine all imputed and original data
rodent_imp <- lapply(seq_along(imputed), function(i){
  complete_imp <- complete(imputed[[i]]) %>%
    dplyr::distinct(Location, .keep_all = TRUE)
  merged <- merge(rodent[,-c(8)], complete_imp, by = "Location") %>%
    dplyr::select(Location, Surveyor, Longitude, Latitude, Type, Area, Dist_urban, 
                  Above_MHW, Rrav_conn, Rmeg_conn, Mmus_conn, Mcal_conn, Effort, Year, 
                  matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                  matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
  return(merged)
})
rodent_imp[[1]]

#-------------------------------------------------------------------------------
#DATA EXPLORATION

#1. Visualize relationships between independent and dependent variables with logistic regression curves
#a. Add columns collapsing occupancy across site (e.g., 1 if any bait station was occupied, 0 if not)
rodent_imp <- lapply(rodent_imp, function(df) {
  df %>%
    mutate(
      Rrav_occupancy = apply(.[, 15:34], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Rmeg_occupancy = apply(.[, 35:54], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Mmus_occupancy = apply(.[, 55:74], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      Mcal_occupancy = apply(.[, 75:94], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0))
    )
})
rodent_imp[[1]]

#b. Reshape data frame to create a single column for connectivity and occupancy         
rodent_imp <- lapply(rodent_imp, function(df) {
  df %>%
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
})
rodent_imp[[1]]

#c. Plot each covariate against the response variable (occupancy)
#1) Area
ggplot(rodent_imp[[1]], aes(x = Area, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Patch Area",
       x = "Patch Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#2) Distance to urban area
ggplot(rodent_imp[[1]], aes(x = Dist_urban, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Distance to Urban Area",
       x = "Distance to Urban Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#3) Inundation
ggplot(rodent_imp[[1]], aes(x = Above_MHW, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Inundation",
       x = "Inundation", 
       y = "Occupancy Probability") +
  theme_minimal()

#4) Connectivity   
ggplot(rodent_imp[[1]], aes(x = Connectivity, y = Occupancy)) +
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
species_list <- unique(rodent_imp[[1]]$Species)

dep_vars <- list()
for(species in species_list){
  species_data <- rodent_imp[[1]] %>%
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
transform_covs <- function(rodent_imp, dep_vars, covs, transformations) {
  pooled_results <- list()
  for (species_name in dep_vars){
    species_results <- list()
    for (cov in covs) {
      
      #Calculate shifted covariate values for transformations
      shift_value <- calc_shift(do.call(rbind, rodent_imp), cov)
      
      #Initialize data frame to store pooled AIC results 
      pooled_aic <- data.frame(
        Transformation = character(), 
        AIC = numeric(),
        stringsAsFactors = FALSE)
      
      for (trans in transformations) {
        aic_values <- c()
        for (imp in rodent_imp) {
          transformed_data <- imp
          species_data <- transformed_data %>%
            filter(Species == species_name)
          
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
        
        #Pool AIC values using Rubin's rules
        mean_aic <- mean(aic_values)
        var_aic <- var(aic_values)
        pooled_aic_value <- mean_aic + (var_aic / length(aic_values)) 
        pooled_aic <- rbind(pooled_aic, data.frame(
          Transformation = trans, 
          AIC = pooled_aic_value
        )) %>% arrange(AIC)
      }
      
      #Calculate delta AIC <=2
      best_aic <- min(pooled_aic$AIC)
      pooled_aic$Delta_AIC <- pooled_aic$AIC - best_aic
      species_results[[cov]] <- pooled_aic
    }
    pooled_results[[species_name]] <- species_results
  }
  return(pooled_results)
}

#1) Apply the function
pooled_results <- transform_covs(
  rodent_imp = rodent_imp,  
  dep_vars = unique(rodent_imp[[1]]$Species),
  covs = covs,
  transformations = transformations
)
pooled_results
    #Linear connectivity is a better predictor of Rmeg occupancy and log area is a better predictor
    #of Rrav occupancy, but all other independent variables have delta AIC <=2

####################KEEP, EXCLUDE TRANSFORM#########################
#f. Scale and transform independent variables (as needed)
scale_transform <- function(rodent_imp){
  transformed_data <- lapply(rodent_imp, function(data){
    data <- data %>%
      mutate(Log_Area = log(Area))
    covs <- c("Area", "Log_Area", "Dist_urban", "Above_MHW", "Connectivity", "Effort")
    data <- data %>%
      mutate(across(all_of(covs),~as.vector(scale(.)))) %>%
      dplyr::select(Location, Longitude, Latitude, Surveyor, Species, Occupancy, Type, 
                    Area, Log_Area, Dist_urban, Above_MHW, Connectivity, Effort, Year, 
                    matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                    matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
    return(data)
  })
  return(transformed_data)
}
rodent_imp <- scale_transform(rodent_imp)
rodent_imp[[1]]
####################KEEP, EXCLUDE TRANSFORM#########################

#g. Plot log-transformed variables
#1) Assess non-linearity assumption violation
ggplot(rodent_imp[[1]], aes(x = Log_Area, y = Occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ Species) +
  labs(title = "Occupancy vs Log Patch Area",
       x = "Log Patch Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#2) Assess normality (not a strict assumption of MSOMs)
hist(rodent_imp[[1]]$Area, main = "Area Distribution", xlab = "Area", col = "lightblue")
hist(rodent_imp[[1]]$Log_Area, main = "Log Area Distribution", xlab = "Log Area", col = "lightblue")

qqnorm(rodent_imp[[1]]$Area)
qqnorm(rodent_imp[[1]]$Log_Area)
################EXCLUDE???###################

#----------
#3. Check for collinearity among continuous independent variables using variance inflation factor (VIF)
vif_results <- list()
for(species in species_list){    
  dep_var <- dep_vars[[species]]
  dep_vif <- list()
  for(i in seq_along(rodent_imp)){                                                
    species_data <- rodent_imp[[i]] %>%
      filter(Species == species)
    species_data$dep_var <- rep(dep_var, nrow(species_data)/length(dep_var))
    formula <- as.formula(paste("dep_var ~", paste(covs, collapse = "+")))
    model <- lm(formula, data = species_data)
    vif_values <- car::vif(model)
    dep_vif[[i]] <- vif_values
  }
  vif_results[[species]] <- dep_vif
}
vif_results
    #No collinearity between independent variables, okay to keep all in model

#----------
#4. Explore non-additive effects of interactions
#a. Connectivity*area (connectivity may enhance occupancy in smaller marshes)
#1) Fit the linear models to evaluate the interaction
#a) Define a function to fit the linear models
fit_conn_area_models <- function(rodent_imp) {
  models_list <- list()
  for (i in seq_along(rodent_imp)) {
    data <- rodent_imp[[i]]
    model <- lm(Connectivity ~ Area + Area:Connectivity, data = data)
    models_list[[i]] <- model
  }
  return(models_list)
}

#b) Apply the function and view results
conn_area_models <- fit_conn_area_models(rodent_imp)
conn_area_models

#2) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs <- lapply(seq_along(conn_area_models), function(i){
  model <- conn_area_models[[i]]
  coefs <- tidy(model) %>%
    filter(term == "Connectivity:Area") %>%
    mutate(imp = i)
  preds <- predict(model, newdata = rodent_imp[[i]], type = "response")
  pred_data <- rodent_imp[[i]] %>%
    mutate(prediction = preds)
  return(list(coefs = coefs, preds = pred_data))
})
preds_df <- bind_rows(lapply(preds_coefs, `[[`, "preds"))
coefs_df <- bind_rows(lapply(preds_coefs, `[[`, "coefs"))
coefs_df
#There is evidence of an interaction (p < 0.05), although estimates are small
#Plot to confirm

#3) Visualize the interaction as it relates to occupancy, holding one covariate constant
#a) Fit the linear model to the list of imputed data sets
conn_area_models1 <- lapply(species_list, function(species){
  lapply(rodent_imp, function(data){
    species_data <- subset(data, Species == species)
    formula <- as.formula("Occupancy ~ Connectivity*Area")
    model <- lm(formula, data = species_data)
    return(model)
  })
})
names(conn_area_models1) <- species_list
conn_area_models1

#b) Generate a prediction grid for each covariate, fixing the other variable at its mean
area_grid <- lapply(seq_along(rodent_imp), function(i){
  data <- rodent_imp[[i]]
  grid <- expand.grid(
    Area = seq(min(data$Area), max(data$Area), length.out = 100),
    Connectivity = mean(data$Connectivity)
  )
  return(grid)
})

conn_grid <- lapply(seq_along(rodent_imp), function(i){
  data <- rodent_imp[[i]]
  grid <- expand.grid(
    Area = mean(data$Area),
    Connectivity = seq(min(data$Connectivity), max(data$Connectivity), length.out = 100)
  )
  return(grid)
})

#c) Predict occupancy for each covariate
area_preds <- lapply(seq_along(rodent_imp), function(i){
  lapply(seq_along(conn_area_models1), function(j){
    model <- conn_area_models1[[j]][[i]]
    preds <- predict(model, newdata = area_grid[[i]], type = "response")
    pred_grid <- cbind(area_grid[[i]], Prediction = preds, 
                       Variable = "Area", Imp = i, Species = species_list[j])
    return(pred_grid)
  })
})

conn_preds <- lapply(seq_along(rodent_imp), function(i){
  lapply(seq_along(conn_area_models1), function(j){
    model <- conn_area_models1[[j]][[i]]
    preds <- predict(model, newdata = conn_grid[[i]], type = "response")
    species_name <- names(conn_area_models1[[j]])[1]
    pred_grid <- cbind(conn_grid[[i]], Prediction = preds, 
                       Variable = "Connectivity", Imp = i, Species = species_list[j])
    return(pred_grid)
  })
})

#d) Summarize predictions and combine into a data frame
area_preds_df <- do.call(rbind, lapply(seq_along(area_preds), function(i){
  do.call(rbind, lapply(seq_along(area_preds[[i]]), function(j){
    area_preds[[i]][[j]] %>%
      group_by(Area, Connectivity, Imp, Species, Variable) %>%
      summarize(Prediction = mean(Prediction), .groups = "drop")
  }))
}))
area_preds_df

conn_preds_df <- do.call(rbind, lapply(seq_along(conn_preds), function(i){
  do.call(rbind, lapply(seq_along(conn_preds[[i]]), function(j){
    conn_preds[[i]][[j]] %>%
      group_by(Area, Connectivity, Imp, Species, Variable) %>%
      summarize(Prediction = mean(Prediction), .groups = "drop")
  }))
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
  facet_wrap(Imp~Species, scales = "free_x", nrow = 10, ncol = 4) +
  theme(strip.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"))
#The interaction between connectivity and area likely exists, as evidenced by 
#the intersecting lines. It is weakest for Mcal and the directionality changes 
#based on the species. 

#-------------------------------------------------------------------------------
#FORMULATE THE DATA

#1. Create detection/non-detection matrices
#a. Extract detection data for each species
Rrav <- rodent_imp[[1]] %>%
  dplyr::select(matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"))
Rrav <- as.matrix(Rrav)
Rmeg <- rodent_imp[[1]] %>%
  dplyr::select(matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"))
Rmeg <- as.matrix(Rmeg)
Mmus <- rodent_imp[[1]] %>%
  dplyr::select(matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"))
Mmus <- as.matrix(Mmus)
Mcal <- rodent_imp[[1]] %>%
  dplyr::select(matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
Mcal <- as.matrix(Mcal)

#-----
#2. Create data frame of standardized independent variables
#a. Extract variable names
covs <- colnames(rodent_imp[[1]])
covs <- covs[c(7:9, 10:14)]        #Edit to include log_area if needed

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
sitecovs <- lapply(rodent_imp, extract_covs)
str(sitecovs)

#----------
#3. Create unmarkedFrameOccuMulti objects
#a. Rrav
#1) Define a function to create the umf objects
create_umf_rrav <- function(Rrav, site_covs) {
  umf_list_rrav <- list()
  for (i in 1:length(site_covs)) {
    umf_list_rrav[[i]] <- unmarkedFrameOccu(
      y = Rrav, 
      siteCovs = site_covs[[i]]
    )
  }
  return(umf_list_rrav)
}

#2) Apply the function to create a list of umf objects (one for each dataset)
umf_list_rrav <- create_umf_rrav(Rrav, sitecovs)
summary(umf_list_rrav[[1]])
str(umf_list_rrav[[1]]@siteCovs)
plot(umf_list_rrav[[1]])

#b. Rmeg
#1) Define a function to create the umf objects
create_umf_rmeg <- function(Rmeg, site_covs) {
  umf_list_rmeg <- list()
  for (i in 1:length(site_covs)) {
    umf_list_rmeg[[i]] <- unmarkedFrameOccu(
      y = Rmeg, 
      siteCovs = site_covs[[i]]
    )
  }
  return(umf_list_rmeg)
}

#2) Apply the function to create a list of umf objects (one for each dataset)
umf_list_rmeg <- create_umf_rmeg(Rmeg, sitecovs)
summary(umf_list_rmeg[[1]])
plot(umf_list_rmeg[[1]])

#c. Mmus
#1) Define a function to create the umf objects
create_umf_mmus <- function(Mmus, site_covs) {
  umf_list_mmus <- list()
  for (i in 1:length(site_covs)) {
    umf_list_mmus[[i]] <- unmarkedFrameOccu(
      y = Mmus, 
      siteCovs = site_covs[[i]]
    )
  }
  return(umf_list_mmus)
}

#2) Apply the function to create a list of umf objects (one for each dataset)
umf_list_mmus <- create_umf_mmus(Mmus, sitecovs)
summary(umf_list_mmus[[1]])
plot(umf_list_mmus[[1]])

#d. Mcal
#1) Define a function to create the umf objects
create_umf_mcal <- function(Mcal, site_covs) {
  umf_list_mcal <- list()
  for (i in 1:length(site_covs)) {
    umf_list_mcal[[i]] <- unmarkedFrameOccu(
      y = Mcal, 
      siteCovs = site_covs[[i]]
    )
  }
  return(umf_list_mcal)
}

#2) Apply the function to create a list of umf objects (one for each dataset)
umf_list_mcal <- create_umf_mcal(Mcal, sitecovs)
summary(umf_list_mcal[[1]])
plot(umf_list_mcal[[1]])

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
  for(i in seq_along(umf_list)){
    for(j in 1:nrow(det_combos)){
      combo <- det_combos[j, ]
      det_formula <- create_det_formula(combo, det_vars)
      model <- occuMulti(
        stateformulas = stateformulas,
        detformulas = as.character(rep(list(det_formula), 4)),
        data = umf_list[[i]],
        maxOrder = 2
      )
      det_results[[length(det_results) + 1]] <- list(
        model = model,
        formula = det_formula,
        AIC = model@AIC
      )
    }
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
det_model <- id_best_model(det_models)
det_model
#Best fitting model is ~Effort + Year

#c. Compare null and detection models - UPDATE WITH QUASI-ADJUSTED NULL MODEL, NEED TO CALCULATE AIC
null_aic <- sapply(null_quasi_results, function(x) x$AIC)
null_aic[[1]]
det_aic <- sapply(det_model, function(x) x$AIC)
det_aic[[1]]
#The detection variables have improved model fit

#-----
#3. Assess goodness-of-fit on model residuals
#a. Flatten the list of global models (i.e., best fitting model, in this case)
global_det_model <- lapply(det_model, function(x) x$model)

#b. Initiate parallel computing
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_det_model", "fitstats"))
clusterEvalQ(cl, library(unmarked))

#c. Assess GOF for global detection models
global_det_fit <- parLapply(cl, global_det_model, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#d. Save the results of the goodness-of-fit test 
save(global_det_fit, file = "GOF_global_det_mod.Rdata")
load("GOF_global_det_mod.Rdata")

#e. Pool the results
det_fit_pooled <- pool_fitstats(global_det_fit)
#Model is moderately overdispersed (Chi-square statistic = 0, c-hat = 1.35)

#f. Model fit diagnostics (e.g., convergence, parameterization, SEs)
checkConv(global_det_model[[1]])            
sapply(global_det_model, extractCN)         #Does not have excessively high condition numbers, likely not over-parameterized
lapply(global_det_model, checkParms)    
#Other fit diagnostics look okay, use c-hat to adjust for overdispersion

#-----
#4. Account for overdispersion with quasi-likelihood adjustment
#a. Apply the adjustment by specifying c-hat
quasi_det_results <- lapply(global_det_model, function(model){
  summaryOD(model, c.hat = 1.35, conf.level = 0.95, out.type = "confint")
})
quasi_det_results

#-----
#5. Pool the results with Rubin's rules
pooled_det_quasi_results <- pool_quasi(quasi_det_results)
pooled_det_quasi_results

#-----
#6. Predict how detection varies with independent variables
#a. Define a range of effort values and years for prediction
effort_range <- seq(min(rodent_imp[[1]]$Effort), max(rodent_imp[[1]]$Effort), length.out = 100)
years <- c(0, 1)

#b. Generate prediction detection probabilities for each combination of effort and year
det_preds <- lapply(global_det_model, function(model) {
  expand.grid(Effort = effort_range, Year = years) %>%
    mutate(Year = as.factor(Year)) %>%
    rowwise() %>%
    mutate(
      Rrav = predict(model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Rrav"),
      Rmeg = predict(model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Rmeg"),
      Mmus = predict(model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Mmus"),
      Mcal = predict(model, newdata = data.frame(Effort = Effort, Year = Year), type = "det", species = "Mcal")
    ) %>%
    ungroup()
})
print(det_preds[[1]], n = Inf)
#Detection varies for each species based on Effort and Year, with detection probability 
#for all species except Mmus being higher in 2020 than 2022

#c. Visualize trends in detection based on effort and year
#1) Define the species
species <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#2) Combine predictions into a single data frame
det_preds_df <- do.call(rbind, lapply(seq_along(det_preds), function(i) {
  df <- det_preds[[i]] %>%
    pivot_longer(cols = all_of(species), names_to = "Species", values_to = "Predicted")
  df$Model <- i
  return(df)
}))
det_preds_df$Model

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
#2. Fit the models
#a. Rrav
#1) Define a function to fit the model
fit_global_models_rrav <- function(umf_list_rrav){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_rrav)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_rrav[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models_rrav <- fit_global_models_rrav(umf_list_rrav)
summary(global_models_rrav[[1]])

#b. Rmeg
#1) Define a function to fit the model
fit_global_models_rmeg <- function(umf_list_rmeg){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_rmeg)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_rmeg[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models_rmeg <- fit_global_models_rmeg(umf_list_rmeg)
summary(global_models_rmeg[[1]])

#c. Mmus
#1) Define a function to fit the model
fit_global_models_mmus <- function(umf_list_mmus){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_mmus)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_mmus[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models_mmus <- fit_global_models_mmus(umf_list_mmus)
summary(global_models_mmus[[1]])

#d. Mcal
#1) Define a function to fit the model
fit_global_models_mcal <- function(umf_list_mcal){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_mcal)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_mcal[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models_mcal <- fit_global_models_mcal(umf_list_mcal)
summary(global_models_mcal[[1]])

#-----
#3. Assess goodness-of-fit on model residuals
#a. Rrav
#1) Flatten the list of models
global_models_rrav_flat <- unlist(global_models_rrav, recursive = FALSE)

#2) Calculate fit statistis
rrav_global_gof <- lapply(global_models_rrav_flat, function(model){
  mb.gof.test(model, nsim = 100, plot.hist = FALSE)
})
rrav_global_gof
    #Models are not a good fit. Chi-square statistic is huge, p-value is 0, and quantiles/c-hat are unable to be estimated
    #Tested with Log_Area as well but Chi-square statistic increased

#b. Rmeg
#1) Flatten the list of models
global_models_rmeg_flat <- unlist(global_models_rmeg, recursive = FALSE)

#2) Calculate fit statistis
rmeg_global_gof <- lapply(global_models_rmeg_flat, function(model){
  mb.gof.test(model, nsim = 100, plot.hist = FALSE)
})
rmeg_global_gof
    #Same issues

#c. Mmus
#1) Flatten the list of models
global_models_mmus_flat <- unlist(global_models_mmus, recursive = FALSE)

#2) Calculate fit statistis
mmus_global_gof <- lapply(global_models_mmus_flat, function(model){
  mb.gof.test(model, nsim = 100, plot.hist = FALSE)
})
mmus_global_gof
    #Same issues

#d. Mcal
#1) Flatten the list of models
global_models_mcal_flat <- unlist(global_models_mcal, recursive = FALSE)

#2) Calculate fit statistis
mcal_global_gof <- lapply(global_models_mcal_flat, function(model){
  mb.gof.test(model, nsim = 100, plot.hist = FALSE)
})
mcal_global_gof
    #Same issues

#-----
#4. Check why models are poor fit
#a. Look at unique number of detection histories (should not be one or two)
table(apply(umf_list_rrav[[1]]@y, 1, paste, collapse = ""))     
table(apply(umf_list_rmeg[[1]]@y, 1, paste, collapse = ""))
table(apply(umf_list_mmus[[1]]@y, 1, paste, collapse = ""))
table(apply(umf_list_mcal[[1]]@y, 1, paste, collapse = ""))
    #All have minimum 4 detections

#b. Assess other model fit diagnostics
#1) Check convergence
checkConv(global_models_rrav[[1]])
checkConv(global_models_rmeg[[1]]) 
checkConv(global_models_mmus[[1]]) 
checkConv(global_models_mcal[[1]]) 
    #All models converged

#2) Check condition numbers (will determine if models are overparameterized)
sapply(global_models_rrav, extractCN)    
sapply(global_models_rmeg, extractCN)
sapply(global_models_mmus, extractCN)
sapply(global_models_mcal, extractCN)
    #Condition numbers are highest for Rrav (~5000) but all others are ~100 for most imputed models
    #Likely not overparameterized, but try fitting simpler models to determine if fit improves

#3) Check parameter SSEs
lapply(global_models_rrav, checkParms)
lapply(global_models_rmeg, checkParms)
lapply(global_models_mmus, checkParms)
lapply(global_models_mcal, checkParms)
    #Rrav has the highest standard errors, but still not concerning

#-----
#5. Fit simplified models without interaction term
#a. Define the independent variables
model_covs <- c("Area", "Dist_urban", "Above_MHW", "Connectivity", "Connectivity:Area")
model_covs_noint <- c("Area", "Dist_urban", "Above_MHW", "Connectivity")
model_covs_simple <- c("Dist_urban", "Above_MHW", "Connectivity")

#b. Fit the models
#1) Rrav
#a) Define a function to fit the model
fit_global_models_rrav <- function(umf_list_rrav){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_rrav)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_rrav[[i]]
    )
  }
  return(model_list)
}

fit_global_models_rrav_noint <- function(umf_list_rrav){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs_noint, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_rrav)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_rrav[[i]]
    )
  }
  return(model_list)
}

fit_global_models_rrav_simple <- function(umf_list_rrav){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs_simple, collapse = "+"))
  det_formula <- c("~Effort + Year")
  for(i in seq_along(umf_list_rrav)){
    model_list[[i]] <- occu(
      formula = as.formula(paste(det_formula, "~", state_formula)),
      control = list(maxit = 5000),
      data = umf_list_rrav[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models_rrav <- fit_global_models_rrav(umf_list_rrav)
summary(global_models_rrav[[1]])

global_models_rrav_noint <- fit_global_models_rrav_noint(umf_list_rrav)
summary(global_models_rrav_noint[[1]])

global_models_rrav_simple <- fit_global_models_rrav_simple(umf_list_rrav)
summary(global_models_rrav_simple[[1]])

#c. Re-assess goodness-of-fit on model residuals
#1) Rrav
#a) Flatten the list of models
global_models_rrav_flat <- unlist(global_models_rrav, recursive = FALSE)
global_models_rrav_noint_flat <- unlist(global_models_rrav_noint, recursive = FALSE)
global_models_rrav_simple_flat <- unlist(global_models_rrav_simple, recursive = FALSE)

#b) Calculate fit statistics
#i) Full model
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_models_rrav_flat", "mb.gof.test"))
clusterEvalQ(cl, library("unmarked", "AICcmodavg"))

rrav_global_gof <- parLapply(cl, global_models_rrav_flat, function(model){
  mb.gof.test(model, nsim = 10000, plot.hist = FALSE)
})
stopCluster(cl)
rrav_global_gof[[1]]

#ii) Without interaction
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_models_rrav_noint_flat", "mb.gof.test"))
clusterEvalQ(cl, library("unmarked", "AICcmodavg"))

rrav_global_noint_gof <- parLapply(cl, global_models_rrav_noint_flat, function(model){
  mb.gof.test(model, nsim = 10000, plot.hist = FALSE)
})
stopCluster(cl)
rrav_global_noint_gof[[1]]

#iii) Without interaction or Area
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_models_rrav_simple_flat", "mb.gof.test"))
clusterEvalQ(cl, library("unmarked", "AICcmodavg"))

rrav_global_simple_gof <- parLapply(global_models_rrav_simple_flat, function(model){
  mb.gof.test(model, nsim = 10000, plot.hist = FALSE)
})
stopCluster(cl)
rrav_global_simple_gof[[1]]
    #Models are still not a good fit and this was worsened by removing independent variables

#d. Compare model fit with AIC
mean(sapply(global_models_rrav, function(model) model@AIC))
mean(sapply(global_models_rrav_noint, function(model) model@AIC))
mean(sapply(global_models_rrav_simple, function(model) model@AIC))
    #AIC is lowest for global model with all variables, but only marginally (about 20-40 points)

#e. Manually calculate c-hat?

