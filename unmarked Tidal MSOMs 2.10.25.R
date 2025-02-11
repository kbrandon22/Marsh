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

#d. Combine detection data into a named list
ylist <- list(Rrav = Rrav, Rmeg = Rmeg, Mmus = Mmus, Mcal = Mcal)
str(ylist)  

#-----
#2. Create data frame of standardized independent variables
#a. Extract variable names
covs <- colnames(rodent_imp[[1]])
covs <- covs[c(7:8, 10:14)]        #Edit to include log_area if needed

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
str(umf_list[[1]]@siteCovs)
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

#b. Create vector of intercept-only occupancy formulas for the 1st and 2nd order parameters
stateformulas <- c("~1","~1","~1","~1","~1","~1","~1","~1","~1","~1") 

#----------
#2. Detection
#a. Create vector of intercept-only detection formulas
detformulas <- c("~1", "~1", "~1", "~1")

#-------------------------------------------------------------------------------
#NULL MODEL

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
    #Standard errors are high in relation to estimates, assess GOF and apply penalized likelihood if necessary

#-----
#2. Assess goodness-of-fit on model residuals
#a. Define a function to calculate goodness-of-fit measures
fitstats <- function(model){
  resids <- do.call(rbind, residuals(model))
  observed <- do.call(rbind, model@data@ylist)
  expected <- do.call(rbind, fitted(model))
  SSE <- sum(resids^2, na.rm = TRUE)
  Chisq <- sum((observed-expected)^2/expected, na.rm = TRUE)
  freeTuke <- sum((sqrt(observed)-sqrt(expected))^2, na.rm = TRUE)
  out <- c(SSE = SSE, Chisq = Chisq, freemanTukey = freeTuke)
  return(out)
}

#b. Initiate parallel computing - this needs to be reinitialized for each parallel computing segment
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("fitstats", "null_models"))
clusterEvalQ(cl, library(unmarked))

#c. Fit the GOF function with parallelization
null_fit <- parLapply(cl, null_models, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#d. Save the results of the goodness-of-fit test 
save(null_fit, file = "GOF_tidal_nullmod.Rdata")
load("GOF_tidal_nullmod.Rdata")

#e. Pool the results
#1) Define a function to extract and pool fit statistics 
pool_fitstats <- function(pool_fit){
  
  #Extract p-values
  sse_p <- sapply(pool_fit, function(fit) mean(fit@t.star[,1] >= fit@t0[1]))         
  chisq_p <- sapply(pool_fit, function(fit) mean(fit@t.star[,2] >= fit@t0[2]))
  freeTuke_p <- sapply(pool_fit, function(fit) mean(fit@t.star[,3] >= fit@t0[3]))
  
  #Extract t0 values
  sse <- sapply(pool_fit, function(fit) fit@t0["SSE"])
  chisq <- sapply(pool_fit, function(fit) fit@t0["Chisq"])
  freeTuke <- sapply(pool_fit, function(fit) fit@t0["freemanTukey"])
  
  #Calculate c-hat values
  chat_chisq <- sapply(pool_fit, function(fit) fit@t0[2] / mean(fit@t.star[, 2], na.rm = TRUE))
  chat_freeTuke <- sapply(pool_fit, function(fit) fit@t0[3] / mean(fit@t.star[, 3], na.rm = TRUE))
  
  #Pool p-values
  pooled_sse_p <- mean(sse_p)                                                
  pooled_chisq_p <- mean(chisq_p)                                            
  pooled_freeTuke_p <- mean(freeTuke_p) 
  
  #Average t0
  fit_pooled <- list(
    Chisq = mean(chisq, na.rm = TRUE),
    Chisq_p = pooled_chisq_p,
    Chisq_chat = mean(chat_chisq, na.rm = TRUE),
    SSE = mean(sse, na.rm = TRUE),
    SSE_p = pooled_sse_p,
    freemanTukey = mean(freeTuke, na.rm = TRUE),
    freeTuke_p = pooled_freeTuke_p,
    freeTuke_chat = mean(chat_freeTuke, na.rm = TRUE)
  )
  return(fit_pooled)
}

#2) Apply the function
null_fit_pooled <- pool_fitstats(null_fit)
null_fit_pooled
    #Model is a good fit (all p > 0.05, c-hat ~1)

#-----
#3. Pool results with Rubin's rules for variance estimation
#a. Define a function to pool the results
pool_results <- function(model_list, ci_list, weights = NULL) {
  coefs <- sapply(model_list, coef)
  se <- sapply(model_list, function(model) sqrt(diag(vcov(model))))
  z_values <- coefs / se
  p_values <- 2*(1 - pnorm(abs(z_values)))
  aic <- sapply(model_list, function(model) model@AIC)
  
  #Calculate model weights based on AIC values
  if(is.null(weights)){
    weights <- (1 / aic)
    weights <- weights / sum(weights)
  }
  
  #Pool coefficients
  pooled_coefs <- rowSums(coefs * weights)
  
  #Calculate variance
  within_var <- rowSums(weights*se^2)
  between_var <- rowSums(weights*(coefs-pooled_coefs)^2)
  total_var <- within_var + (1+1/length(model_list))*between_var
  
  #Pool results
  pooled_se <- sqrt(total_var)
  pooled_z <- pooled_coefs / pooled_se
  pooled_p <- 2*(1 - pnorm(abs(pooled_z)))
  pooled_lower <- pooled_coefs - 1.96*pooled_se
  pooled_upper <- pooled_coefs + 1.96*pooled_se
  
  #Return pooled results as a data frame, rounding to 5 decimal places
  data.frame(
    Estimate = round(pooled_coefs, 5),
    SE = round(pooled_se, 5),
    Z = round(pooled_z, 5),
    p_value = round(pooled_p, 5),
    Lower_CI = round(pooled_lower, 5),
    Upper_CI = round(pooled_upper, 5)
  )
}

#b. Apply the function
null_results <- pool_results(null_models, null_model_ci)
null_results
    #Notable findings:
      #1. Mcal occupancy and detectability is the lowest of the four species
      #2. Rrav and Rmeg are not likely to occupy the same sites
      #3. Rrav:Mmus, Rmeg:Mcal, and Mmus:Mcal are likely to occupy the same sites
      #4. Detection is highest for Rrav but followed closely by Mmus

#-----
#5. Back-transform to get occupancy and detection estimates
#a. Occupancy
null_occ <- list()
for(i in seq_along(null_models)){
  
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
    #Rrav occupancy is 0.39, Rmeg is 0.38, Mmus is 0.49, Mcal is 0.15

#b. Detection
null_det <- list()
for(i in seq_along(null_models)){
  
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
#MULTISPECIES DETECTION

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

#c. Compare null and detection models
null_aic <- sapply(null_models, function(x) x@AIC)
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
  summaryOD(model, c.hat = 1.23, conf.level = 0.95, out.type = "confint")
})
quasi_det_results

#b. Pool the results with Rubin's rules
#1) Define a function
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

pooled_det_quasi_results <- pool_quasi(quasi_det_results)

#-----
#5. Predict how detection varies with independent variables
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
#GLOBAL MODEL

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

#-----
#4. Assess goodness-of-fit on model residuals
#a. Flatten the list of models
global_models_flat <- unlist(global_pen_models, recursive = FALSE)

#b. Initiate parallel computing
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("global_models_flat", "fitstats"))
clusterEvalQ(cl, library(unmarked))

#c. Assess GOF
global_pen_fit <- parLapply(cl, global_models_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#d. Save the results of the goodness-of-fit test 
save(global_pen_fit, file = "GOF_global_pen_models.Rdata")
load("GOF_global_pen_models.Rdata")

#e. Pool the results
global_fit_pooled <- pool_fitstats(global_pen_fit)
    #Model is moderately overdispersed (Chi-square statistic = 0, c-hat = 1.77)

#f. Model fit diagnostics (e.g., convergence, parameterization, SEs)
checkConv(global_pen_models[[1]])            
sapply(global_pen_models, extractCN)         #Condition numbers are relatively high, but model is likely not over-parameterized
lapply(global_pen_models, checkParms)
    #Other fit diagnostics look okay, use c-hat to account for overdispersion

#-----
#5. Account for overdispersion with quasi-likelihood adjustment
global_quasi_results <- lapply(global_pen_models, function(model){
  summaryOD(model, c.hat = 1.77, conf.level = 0.95, out.type = "confint")
})
global_quasi_results

#-----
#6. Pool the results with Rubin's rules, correcting for shrinkage using VIF
#a. Determine how much SEs shrank with penalization
#1) Flatten the model outputs
global_models_list <- unlist(global_models, recursive = FALSE)
global_pen_models_list <- unlist(global_pen_models, recursive = FALSE)

#2) Define a function to compute the inflation factor
compute_vif <- function(models, pen_models){
  se_pen_model <- lapply(pen_models, function(m) sqrt(diag(vcov(m))))
  se_model <- lapply(models, function(m) sqrt(diag(vcov(m))))
  var_pen_model <- do.call(rbind, se_pen_model)^2
  var_model <- do.call(rbind, se_model)^2
  vif <- colMeans(var_pen_model, na.rm = TRUE)/colMeans(var_model, na.rm = TRUE)
  vif <- mean(vif, na.rm = TRUE)
  return(vif)
}

#3) Apply the function
global_models_vif <- compute_vif(global_pen_models_list, global_models_list)
global_models_vif
    #VIF is 2.45, use to account for variance when pooling

#b. Adjust the quasi_pool function to inflate within-imputation variance
pool_quasi_pen <- function(quasi_list, vif = global_models_vif){
  estimates <- sapply(quasi_list, function(model) model$outMat[,1])
  SE <- sapply(quasi_list, function(model) model$outMat[,2])
  
  #Calculate variance and adjust for VIF
  within_var <- rowMeans(SE^2)
  between_var <- apply(estimates, 1, var)
  within_var_adj <- within_var*vif
  total_var_adj <- within_var_adj + (1+1/length(quasi_list))*between_var
  
  #Pool the results
  pooled_estimate <- rowMeans(estimates)
  pooled_SE <- sqrt(total_var_adj)
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

#c. Pool the results 
pooled_global_quasi_results <- pool_quasi_pen(global_quasi_results, vif = global_models_vif)
pooled_global_quasi_results
    #Effort has a negative effect on Mmus detection
    #Year has a negative effect on Mcal detection

#Compare results without variance inflation
pooled_global_quasi_results_check <- pool_quasi(global_quasi_results)

comparison <- data.frame(
  Estimate = pooled_global_quasi_results_check$Estimate,
  SE = pooled_global_quasi_results_check$SE,
  SE_Adj = pooled_global_quasi_results$SE,
  Lower_CI = pooled_global_quasi_results_check$Lower_CI,
  Lower_CI_Adj = pooled_global_quasi_results$Lower_CI,
  Upper_CI = pooled_global_quasi_results_check$Upper_CI,
  Upper_CI_Adj = pooled_global_quasi_results$Upper_CI
)
comparison


#-------------------------------------------------------------------------------
#UNIVARIATE MODELS

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
class(quasi_uni_list)

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
bi_pen_models[[1]]

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
#5. Determine how much SEs shrank with penalization
#a. Flatten the model outputs
bi_models_list <- unlist(bi_models, recursive = FALSE)
bi_pen_models_list <- unlist(bi_pen_models, recursive = FALSE)

#b. Compute the inflation factor
bi_models_vif <- compute_vif(bi_pen_models_list, bi_models_list)

#-----
#6. Modify the pool_quasi function to inflate within-imputation variance


#-----
#7. Pool the adjusted results with Rubin's rules, accounting for different penalties
#a. Flatten the quasi-adjusted model output 
quasi_bi_list <- unlist(quasi_bi_results, recursive = FALSE)

#b. Define a function to pool outputs for each independent variable
pool_bi <- function(quasi_bi_list, pool_quasi){
  results <- list()
  
  #Extract and group the quasi output by independent variable
  bi_names <- names(quasi_bi_list)
  bi_pair <- setNames(lapply(bi_names, function(pair) {
    quasi_bi_list[grep(pair, names(quasi_bi_list), fixed = TRUE)]
  }), bi_names)
  
  #Pool the results
  for(pair in names(bi_pair)){
    cov_data <- bi_pair[[pair]]
    pooled_results <- pool_quasi(cov_data)
    results[[cov]] <- pooled_results
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
        starts = rnorm(52, mean = 0, sd = 0.075),
        data = umf_list[[i]]
      )
    }
    models[[combo_name]] <- cov_models
  }
  return(models)
}

#2) Apply the function
tri_models <- fit_tri_models(tri_combos, umf_list_tidal)

#c. Pool results 
#1) Extract unique variable names
tri_names <- names(tri_models)

#2) Group models by variable pairs
tri_group <- setNames(lapply(tri_names, function(trio) {
  tri_models[grep(trio, names(tri_models), fixed = TRUE)]
}), tri_names)

#3) Pool the results for each independent variable
tri_results <- list()
for(trio in names(tri_group)){
  model_list <- flatten_list(tri_group[[trio]])
  model_list <- model_list[sapply(model_list, inherits, "unmarkedFitOccuMulti")]
  tri_results[[trio]] <- pool_results(model_list)
}
tri_results

#-------------------------------------------------------------------------------
#MODEL SELECTION

#1. Select the best-fitting model for interpretation
#a. Store models in a list
models <- list()


#b. Assign names to each model
modnames <- c("null_model", "det_model", "global_model", "x")

#b. Perform model selection 
top_models <- aictab()


#-------------------------------------------------------------------------------
#PLOT COVARIATE EFFECTS

#1. Plot the effect of covariates on marginal occupancy      
#a. Define a vector of occupancy covariates
occ_vars <- c("Dist_urban", "Above_MHW", "Conn_vars_PC1", "Conn_vars_PC2", "Frag_vars_PC1", "Frag_vars_PC2")

#b. Create a function to generate a range of possible covariate values
extract_range <- function(umf_list, occ_vars){
  range <- list()
  for(covariate in occ_vars){
    cov_range <- list()
    for(i in seq_along(umf_list)){
      cov_range[[i]] <- range(siteCovs(umf_list[[i]])[[covariate]], na.rm = TRUE)
    }
    range[[covariate]] <- cov_range
  }
  return(list(ranges = range))
}

#1) Apply the function
range <- extract_range(umf_list, occ_vars)

#c. Create a function to generate a sequence of possible covariate values based on the range
extract_seq <- function(umf_list, occ_vars){
  seq <- list()
  for(covariate in occ_vars){
    seq_range <- list()
    for(i in seq_along(umf_list)){
      cov_seq[[i]] <- seq(range[[i]][1], range[[i]][2], length.out = 100)
    }
    seq[[covariate]] <- cov_seq
  }
  return(list(sequences = seq))
}

#1) Apply the function
seq <- extract_seq(umf_list, occ_vars)

#d. Generate new data to assess how changes in a single covariate affect occupancy
generate_data <- function(umf_list, occ_vars, range, seq){
  new_data_list <- list()
  for(covariate in occ_vars){
    new_data <- data.frame()
    for(i in seq_along(umf_list)){
      cov_seq <- seq[[covariate]][[i]]
      mean_vals <- sapply(occ_vars, function(var){
        if(var != covariate){
          return(ean(siteCovs(umf_list[[i]])[[var]], na.rm = TRUE))
        } else {
          return(NULL)
        }
      })
      temp_date <- data.frame(covariate = cov_seq)
      temp_data <- cbind(temp_data, as.data.frame(mean_vals))
      colnames(temp_data) <- c(covariate, occ_vars[occ_vars != covariate])
      new_data_list[[covariate]] <- temp_data
    }
  }
  return(new_data_list)
}

#1) Apply the function
nd <- generate_data(umf_list, occ_vars, range, seq)

#e) Predict species occupancy in relation to each covariate
predict_occ <- function(model, umf_list, dep_vars, range, seq){
  preds <- list()
  for(species in dep_vars){
    species_preds <- data.frame()
    for(covariate in occ_vars){
      temp_data <- new_data[[covariate]]
      temp_data$Species <- species
      occ_preds <- predict(model, type = "response", species = species, newdata = temp_data)
      temp_data$Predicted_Occupancy <- occ_preds
      species_preds <- rbind(species_preds, temp_data)
    }
    preds[[species]] <- species_preds
  }
  return(preds)
}

#1) Apply the function and view predictions for all species
species_preds <- predict_occ(model, dep_vars, umf_list, occ_vars, nd)
species_preds$Rrav
species_preds$Rmeg
species_preds$Mmus
species_preds$Mcal

#f. Build the plot
plot(occ_dist_rrav$Dist_urban, occ_dist_rrav$Predicted, type = '1', ylim=c(0,0.6),
     col="X", lwd=2, xlab="Distance to urban area (km)", ylab="Marginal occupancy")
lines(occ_dist_rmeg$Dist_urban, occ_dist_rmeg$Predicted, col="Y", lwd=2)
lines(occ_dist_mmus$Dist_urban, occ_dist_mmus$Predicted, col="Z", lwd=2)
lines(occ_dist_mcal$Dist_urban, occ_dist_mcal$Predicted, col="A", lwd=2)
legend('topleft', col=c("X","Y","Z","A"), lty=1,
       legend=c("Rrav","Rmeg","Mmus","Mcal"))

#-----
#2. Plot the effect of covariates on conditional occupancy
#a. Calculate species occupancy when another species is present, repeating for each covariate

#-------------------------------------------------------------------------------
#CALCULATE CONDITIONAL OCCUPANCY PROBABILITIES

#1. Predict the probability of occupancy of one species conditional on another species' presence
cond_occ <- lapply(null_models, function(null_models){
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
#Mcal is neutrally associated with Mmus (40%) and Rmeg (51%) and negatively associated with Rrav (33%)
#Mmus is positively associated with all species (Mcal: 80%, Rmeg: 73%, Rrav: 77%)
#Rmeg is positively associated with Mcal (75%) and neutrally with Mmus (54%) and Rrav (40%) 
#Rrav is neutrally associated with all species (Mcal: 50%, Mmus: 57%, Rmeg: 41%) 

#----------
#2. Predict the probability of occupancy of one species conditional on another species' absence 
abs_occ <- lapply(null_models, function(null_models){
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
colors <- c("#2E4D2E", "#7E8050", "#BA8D61", "#BF6534")

rrav_data$Species_simple <- gsub("Rrav_", "", rrav_data$Species)
rrav_data$Species_simple <- gsub("No", "", rrav_data$Species_simple)

ggplot(rrav_data, aes(Species_status, Predicted)) +
  geom_point(aes(color=Species_simple), size=3, position=position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), linewidth=0.8, width=0.25, position=position_dodge(width=0.4)) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[4], "Rmeg" = colors[2], "Mmus" = colors[3]),
                     labels=c("Mcal", "Mmus", "Rmeg")) +
  guides(color=guide_legend(title="Species"))

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
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), linewidth=0.8, width=0.3, position=position_dodge(width=0.4)) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[4], "Mmus" = colors[3], "Rrav" = colors[1]),
                     labels=c("Mcal", "Mmus", "Rrav")) +
  guides(color=guide_legend(title="Species"))

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
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), linewidth=0.8, width=0.3, position=position_dodge(width=0.4)) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mcal" = colors[4], "Rmeg" = colors[2], "Rrav" = colors[1]),
                     labels=c("Mcal", "Rmeg", "Rrav")) +
  guides(color=guide_legend(title="Species"))

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
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Species_simple), linewidth=0.8, width=0.3, position=position_dodge(width=0.4)) +
  ylim(0,1) +
  labs(x="Species status", y="Conditional occupancy probability") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm")) +
  scale_color_manual(values=c("Mmus" = colors[3], "Rmeg" = colors[2], "Rrav" = colors[1]),
                     labels=c("Mmus", "Rmeg", "Rrav")) +
  guides(color=guide_legend(title="Species"))

#-------------------------------------------------------------------------------
#CALCULATE PREDICTED OCCUPANCY AND DETECTION PROBABILITIES

#1. Occupancy
#a. Predict probability for each occupancy state
null_occ_probs <- lapply(null_models, function(null_models){
  all_probs <- predict(null_models, newdata = data.frame(site=1),type="state")
  return(all_probs)
})
null_occ_probs[[1]]$Predicted

#b. Calculate predicted marginal occupancy for each species at specific sites (across all possible occupancy states)
null_occ_preds <- lapply(null_models, function(null_models){
  rrav_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rrav")
  rmeg_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Rmeg")
  mmus_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mmus")
  mcal_occ <- predict(null_models, newdata = data.frame(site=1), type="state", species="Mcal")
  all_occ <- rbind(rrav_occ[1,], rmeg_occ[1,], mmus_occ[1,], mcal_occ[1,])
  all_occ$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_occ)
})
null_occ_preds[[1]]$Predicted

#c. Pool the results
null_occ_pooled <- do.call(rbind, lapply(split(do.call(rbind, null_occ_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
null_occ_pooled$Species <- factor(null_occ_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
null_occ_pooled
#Rrav = 0.507, lower = 0.454, upper = 0.559
#Rmeg = 0.500, lower = 0.442, upper = 0.555
#Mmus = 0.678, lower = 0.618, upper = 0.726
#Mcal = 0.342, lower = 0.274, upper = 0.416

#d. Visualize marginal occupancy for all species
level_order <- c("Rrav", "Rmeg", "Mmus", "Mcal")

ggplot(null_occ_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
  ylim(0.2,1) +
  labs(x="Species", y="Marginal occupancy and 95% CI") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))

#-----
#2. Detection
#a. Calculate predicted marginal detection for each species 
null_det_preds <- lapply(null_models, function(null_models){
  rrav_det <- predict(null_models, newdata = data.frame(site=1), type="det", species="Rrav")
  rmeg_det <- predict(null_models, newdata = data.frame(site=1), type="det", species="Rmeg")
  mmus_det <- predict(null_models, newdata = data.frame(site=1), type="det", species="Mmus")
  mcal_det <- predict(null_models, newdata = data.frame(site=1), type="det", species="Mcal")
  all_det <- rbind(rrav_det[1,], rmeg_det[1,], mmus_det[1,], mcal_det[1,])
  all_det$Species <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  return(all_det)
})
null_det_preds[[1]]$Predicted

#b. Pool the results
null_det_pooled <- do.call(rbind, lapply(split(do.call(rbind, null_det_preds), f = ~ Species), function(species_data){
  mean_pred <- colMeans(species_data[, c("Predicted", "lower", "upper")], na.rm = TRUE)
  return(data.frame(Species = unique(species_data$Species), Predicted = mean_pred["Predicted"],
                    lower = mean_pred["lower"], upper = mean_pred["upper"]))
}))
null_det_pooled$Species <- factor(null_det_pooled$Species, levels = c("Rrav", "Rmeg", "Mmus", "Mcal"))
null_det_pooled
#Rrav = 0.380, lower = 0.361, upper = 0.400
#Rmeg = 0.271, lower = 0.252, upper = 0.290
#Mmus = 0.367, lower = 0.350, upper = 0.384
#Mcal = 0.094, lower = 0.075, upper = 0.116

#c. Visualize detection probabilities for all species
ggplot(null_det_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
  ylim(0.05,1) +
  labs(x="Species", y="Detection probability and 95% CI") +
  theme(axis.title.y=element_text(size=14, vjust=4),
        axis.title.x=element_text(size=14, vjust=-0.5),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        axis.line.x=element_line(color="black", linewidth=0.5),
        axis.line.y=element_line(color="black", linewidth=0.5)) +
  theme(panel.background=element_rect(fill='transparent', color=NA),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1), "cm"))
