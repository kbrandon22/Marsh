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
library(tidyverse)
library(combinat)
library(reshape2)
library(doParallel)
library(MASS)
library(mgcv)
library(caret)

#-------------------------------------------------------------------------------
#GET STARTED
#1. Set seed for reproducibility
set.seed(500)

#2. Set working directory 
setwd("C:\\Users\\Kristin\\Documents\\Multispecies Occupancy\\Data Analysis\\Datasets\\Excel Spreadsheets")

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
#4. Impute missing covariate data
#a. Subset the columns with missing data and convert to numeric (if needed)
rodent_sub <- rodent[,c(1, 8)]

#b. Assign imputation methods 
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
#There is evidence of an interaction (p > 0.05), although estimates are small
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

#1. Create presence/absence matrices
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

#b. Combine into a separate named list
ylist <- list(Rrav=Rrav, Rmeg=Rmeg, Mmus=Mmus, Mcal=Mcal)
str(ylist)

#----------
#2. Create data frame of standardized independent variables
#a. Extract variable names
covs <- colnames(rodent_imp[[1]])
covs <- covs[c(7:8, 10:14)]        #Edit to include log_area if needed

#b. Define a function to extract variables and convert characters to factor
extract_covs <- function(data, scale_covs){
  covariate_df <- data[, covs, drop=FALSE]
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

#b. Create vector of intercept-only occupancy formulas with the 3rd order parameter fixed at 0
stateformulas <- c("~1","~1","~1","~1","~1","~1","~1","~1","~1","~1","0","0","0","0","0") 

#----------
#2. Detection
#a. Create vector of intercept-only detection formulas
detformulas <- c("~1", "~1", "~1", "~1")

#-------------------------------------------------------------------------------
#RUN THE NULL MODEL

#1. Fit the model
#a. Define a function to fit the model
fit_null_models <- function(umf_list, stateformulas, detformulas){
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = stateformulas, 
      detformulas = detformulas,
      data = umf_list[[i]]
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

#b. Define another function to apply fitstats
calc_fit <- function(model, fitstats){
  return(parboot(model, fitstats, nsim = 100))
}

#c. Set up parallel computing - this needs to be reinitialized for each parallel computing segment
c1 <- makeCluster(detectCores()-1)                                         #Make cluster using all but 1 core processor
registerDoParallel(c1)

#d. Apply the function to the list of models (takes a while to run)
null_fit <- foreach(i = seq_along(null_models), .packages = c("unmarked"), 
                    .export = c("fitstats", "calc_fit")) %dopar% {
  calc_fit(null_models[[i]], fitstats)
}
null_fit
stopCluster(c1)

#e. Save the results of the goodness-of-fit test 
save(null_fit, file = "GOF_nullmod.Rdata")
load("GOF_nullmod.Rdata")

#f. Pool the results
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
  pooled_sse_p <- sse_p[1]                                                
  pooled_chisq_p <- chisq_p[1]                                            
  pooled_freeTuke_p <- freeTuke_p[1] 
  
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
    #Model is a good fit (all p>0.05)

#g. Visualize model fit   #######################REVISIT########################
#1) Extract fitted and residual values from each model
null_fitted <- list()
null_resid <- list()
for(i in seq_along(null_models)){
  null_fitted[[i]] <- fitted(null_models[[i]])
  null_resid[[i]] <- residuals(null_models[[i]])
}

#2) Pool values and combine in a data frame
#a) Fitted
null_pooled_fitted <- lapply(seq_along(null_fitted[[1]]), function(species){
  Reduce("+", lapply(null_fitted, `[[`, species))/length(null_fitted)
})
null_pooled_fitted <- do.call(c, null_pooled_fitted)

#b) Residual
null_pooled_resids <- lapply(seq_along(null_resid[[1]]), function(species){
  Reduce("+", lapply(null_resid, `[[`, species))/length(null_resid)
})
null_pooled_resids <- do.call(c, null_pooled_resids)

#c) Combine fitted and residual values into a data frame
null_pooled <- data.frame(Fitted = null_pooled_fitted,
                          Residuals = null_pooled_resids,
                          Species = rep(names(null_fitted[[1]], each = length(null_fitted[[1]][[1]]))))

#3) Plot
ggplot(null_pooled, aes(x = Fitted, y = Residuals, color = Species)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "bottom")
#If model is a good fit, proceed to Step 3. Otherwise, adjust model parameters  
#(e.g., method, starting values) or apply penalized likelihood and reassess

#-----
#3. Pool results with Rubin's rules for variance estimation
#a. Define a function to pool the results
pool_results <- function(model_list) {
  coefs <- sapply(model_list, coef)
  se <- sapply(model_list, function(model) sqrt(diag(vcov(model))))
  z_values <- coefs / se
  p_values <- 2 * (1 - pnorm(abs(z_values)))
  aic <- sapply(model_list, function(model) model@AIC)
  
  #Calculate model weights based on AIC values
  weights <- (1 / aic)
  weights <- weights / sum(weights)
  
  #Pool results
  pooled_coefs <- rowSums(coefs * weights)
  pooled_se <- sqrt(rowSums(weights * (se^2 + (coefs - pooled_coefs)^2)))
  pooled_z <- pooled_coefs / pooled_se
  pooled_p <- 2 * (1 - pnorm(abs(pooled_z)))
  
  #Return pooled results as a data frame, rounding to 5 decimal places
  data.frame(
    Estimate = round(pooled_coefs, 5),
    SE = round(pooled_se, 5),
    Z = round(pooled_z, 5),
    p_value = round(pooled_p, 5)
  )
}

#b. Apply the function
null_results <- pool_results(null_models)
null_results
    #Notable findings:
      #1. Mcal occupancy and detectability is the lowest of the four species
      #2. Rrav and Rmeg are not likely to occupy the same sites
      #3. Rrav:Mmus, Rmeg:Mcal, and Mmus:Mcal are likely to occupy the same sites
      #4. Detection is highest for Rrav but followed closely by Mmus

#-----
#4. Back-transform to get occupancy and detection estimates
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
    #Rrav occupancy is 0.45, Rmeg is 0.42, Mmus is 0.45, Mcal is 0.1

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
    #Rrav detection is 0.38, Rmeg is 0.27, Mmus is 0.37, Mcal is 0.09
    #Detection probabilities are low, consider modeling with covariates (e.g., Effort, Year)

#-------------------------------------------------------------------------------
#################################REDO WHEN TOP MODEL IS IDENTIFIED###################################

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

#################################REDO WHEN TOP MODEL IS IDENTIFIED###################################
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
        stateformulas = c("~1", "~1", "~1", "~1"),
        detformulas = as.character(rep(list(det_formula), 4)),
        data = umf_list[[i]],
        maxOrder = 1
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
#3. Pool results with Rubin's rules for variance estimation
#a. Extract the model from the output
det_model <- unlist(lapply(det_model, function(x) x$model), recursive = FALSE)

#b. Pool the results
det_results <- pool_results(det_model)
det_results
    #Effort does not influence detection of Rmeg or Mcal, but does for Rrav and Mmus
    #Year influences detection of all species

#-----
#4. Explore trends in detection estimates
#a. Check residuals to identify unaccounted variability or non-linearity
#1) Extract the residuals and fitted values, arranging in a data frame
det_resids <- residuals(det_model[[1]])
det_fitted <- fitted(det_model[[1]])
det_values <- do.call(rbind, lapply(names(det_resids), function(species) {
  df <- data.frame(
    Residuals = as.vector(det_resids[[species]]),
    Fitted = as.vector(det_fitted[[species]]),
    Species = species)
  df_clean <- na.omit(df)
  return(df_clean)
}))

#2) Visualize effort against detection probability
ggplot(det_values, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(main = "Fitted vs Residual values", 
       x = "Fitted Values", y = "Residuals") +
  facet_wrap(~Species) + 
  theme_minimal() 
    #There is a pattern in the residual values, possible issues with model fit??
    #Explore further??

#-----
#5. Predict how detection varies with covariates
#a. Define a range of effort values and years for prediction
effort_range <- seq(min(rodent_imp[[1]]$Effort), max(rodent_imp[[1]]$Effort), length.out = 100)
years <- c(0, 1)

#b. Generate prediction detection probabilities for each combination of effort and year
det_preds <- lapply(det_model, function(model) {
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
    #for all species except Mmus being higher in 2020 than 2022.

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
#MULTISPECIES OCCUPANCY MODELS FOR TIDAL SITES

#1. Re-formulate the data
#a. Create detection/non-detection matrices
#1) Subset tidal sites in the imputed data sets
type <- rodent_imp[[1]] %>%
  dplyr::pull(Type)
tidal_filter <- type == "Tidal"

#2) Extract species detection/non-detection 
Rrav_tidal <- Rrav[tidal_filter, , drop = FALSE]
Rmeg_tidal <- Rmeg[tidal_filter, , drop = FALSE]
Mmus_tidal <- Mmus[tidal_filter, , drop = FALSE]
Mcal_tidal <- Mcal[tidal_filter, , drop = FALSE]

#3) Combine into a named list
ylist_tidal <- list(Rrav=Rrav_tidal, Rmeg=Rmeg_tidal, Mmus=Mmus_tidal, Mcal=Mcal_tidal)
str(ylist_tidal)  

#b. Create a data frame of standardized independent variables
#1) Define a function to extract the variables for the tidal sites and convert characters to factor
extract_covs_tidal <- function(data, scale_covs){
  tidal_data <- data[data$Type == "Tidal", covs, drop=FALSE]
  tidal_data[] <- lapply(names(tidal_data), function(colname){
    col <- tidal_data[[colname]]
    if(is.character(col)){
      as.factor(col)
    } else {
      col
    }
  })
  return(tidal_data)
}

#2) Apply the function to each imputed data set
sitecovs_tidal <- lapply(rodent_imp, extract_covs_tidal)

#c. Create unmarkedFrameOccuMulti objects
#1) Define a function to create the umf objects
create_umf_tidal <- function(ylist_tidal, sitecovs_tidal) {
  umf_list_tidal <- list()
  for (i in 1:length(sitecovs_tidal)) {
    umf_list_tidal[[i]] <- unmarkedFrameOccuMulti(
      y = ylist_tidal, 
      siteCovs = as.data.frame(sitecovs_tidal[[i]])
    )
  }
  return(umf_list_tidal)
}

#2) Apply the function to create a list of umf objects
umf_list_tidal <- create_umf_tidal(ylist_tidal, sitecovs_tidal)
summary(umf_list_tidal[[1]])

#----------
#2. Global model 
#a. Define the independent variables and starting values
model_covs <- c("Area", "Dist_urban", "Above_MHW", "Connectivity", "Connectivity*Area")
starting_values <- rnorm(36, mean = 0, sd = 0.1)

#-----
#b. Fit the model
#1) Define a function to fit the model
fit_global_models <- function(umf_list_tidal){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  for(i in seq_along(umf_list_tidal)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(rep(state_formula, 4)),
      detformulas = as.character(rep("~Effort + Year", 4)),
      control = list(maxit = 5000),
      maxOrder = 1,
      starts = starting_values,
      data = umf_list_tidal[[i]]
    )
  }
  return(model_list)
}

#2) Apply the function
global_models <- fit_global_models(umf_list_tidal)              
summary(global_models[[1]])

#-----
#c. Assess goodness-of-fit on model residuals
#1) Initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                      
registerDoParallel(c1)

#2) Apply the function to the list of models (takes a while to run)
global_fit <- foreach(i = seq_along(global_models), .packages = c("unmarked"), 
                      .export = c("fitstats", "calc_fit")) %dopar% {
  calc_fit(global_models[[i]], fitstats)
}

global_fit
stopCluster(c1)

#3) Save the results of the goodness-of-fit test 
save(global_fit, file = "GOF_global_mod.Rdata")
load("GOF_global_mod.Rdata")

#4) Pool the results
global_fit_pooled <- pool_fitstats(global_fit)

#-----
#d. Apply penalized likelihood if GOF p < 0.05
#1) Define a function to penalize the global models
penalize_global <- function(global_models, global_fit_pooled){
  pen_model_results <- list()
  if(global_fit_pooled$SSE_p < 0.05 || 
     global_fit_pooled$Chisq_p < 0.05 || 
     global_fit_pooled$FreemanTukey_p < 0.05){
    for(i in seq_along(global_models)){
      pen_model_results[[i]] <- optimizePenalty(
        global_models[[i]],
        penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
      )
    }
  } else {
    pen_model_results <- NULL
  }
  return(pen_model_results)
}

#2) Apply the function
global_model_pen <- penalize_global(
  global_models = global_models,
  global_fit_pooled = global_fit_pooled
)
save(global_model_pen, file = "Global_model_pen.Rdata")
load("Global_model_pen.Rdata")
global_model_pen[[1]]
  
#-----
#e. Re-assess goodness-of-fit
#1) Initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                      
registerDoParallel(c1)

#2) Apply the function to the list of models (takes a while to run)
global_pen_fit <- foreach(i = seq_along(global_model_pen), .packages = c("unmarked"), 
                      .export = c("fitstats", "calc_fit")) %dopar% {
  calc_fit(global_model_pen[[i]], fitstats)
}
global_pen_fit
stopCluster(c1)

#3) Save the results of the goodness-of-fit test 
save(global_pen_fit, file = "GOF_global_pen_mod.Rdata")
load("GOF_global_pen_mod.Rdata")

#4) Pool the results
global_pen_fit_pooled <- pool_fitstats(global_pen_fit)
    #Model is still not a good fit, chi-square p-value < 0.05

#-----
#########################WHOLE FUNCTION###############################
#b. Define a function to fit models and calculate GOF statistics
fit_global_model <- function(model_covs, umf_list_tidal) {
  c1 <- makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  results <- foreach(i = seq_along(umf_list_tidal), .packages = c("unmarked"), 
                     .export = c("fitstats", "pool_results", "pool_fitstats")) %dopar% {
                       
    #Define formulas and starting values
    state_formula <- paste("~", paste(model_covs, collapse = "+"))
    starting_values <- rnorm(36, mean = 0, sd = 0.1)
                       
    #Fit the model
    model_results <- occuMulti(
      stateformulas = as.character(rep(state_formula, 4)),
      detformulas = as.character(rep("~Effort + Year", 4)),
      control = list(maxit = 5000),
      maxOrder = 1,
      starts = starting_values,
      data = umf_list_tidal[[i]]
    )
                       
    #Assess goodness-of-fit on model residuals
    fit_list <- parboot(model_results, fitstats, nsim = 100)
    fit <- calc_fit(model_results, fitstats)
    pooled_fit <- pool_fitstats(fit)

    #Apply penalized likelihood if p < 0.05
    if(pooled_fit$SSE_p < 0.05 || pooled_fit$Chisq_p < 0.05 || pooled_fit$FreemanTukey_p < 0.05){
      pen_model_results <- optimizePenalty(
        model_results,
        penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
      )
      
      #Re-assess goodness-of-fit
      pen_fit_list <- parboot(pen_model_results, fitstats, nsim = 100)                     
      pen_fit <- calc_fit(pen_model_results, fitstats)
      pen_pooled_fit <- pool_fitstats(pen_fit)
    } else {
      pen_model_results <- NULL
      pen_pooled_fit <- NULL
    }

    #Pool and store model results 
    pooled_results <- pool_results(model_results)
    pooled_pen_results <- if(!is.null(pen_model_results)) pool_results(pen_model_results) else NULL
    list(
      model = model_results,
      pooled_fit = pooled_fit,
      pooled_results = pooled_results,
      pen_model = pen_model_results,
      pen_pooled_fit = pen_pooled_fit,
      pooled_pen_results = pooled_pen_results
    )
  }
  stopCluster(c1)
  return(results)
}                                 

# Wrapper function
fit_global_model_tidal <- function(umf_list_tidal, model_covs) {
  fit_global_model(umf_list_tidal, model_covs)
}

results <- fit_global_model_tidal(model_covs, umf_list_tidal)
results
#########################################################

#----------
#3. Univariate models
#a. Define a function to fit models, calculate GOF statistics, and run penalized likelihood (if necessary)
fit_uni_model <- function(model_covs, umf_list_tidal){
  
  #Initiate parallel computing and lists to store results
  c1 <- makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  results <- foreach(cov = model_covs, .packages = c("unmarked"), 
                     .export = c("fitstats", "calc_fit", "pool_fitstats", "pool_results")) %dopar% {
    state_formula <- paste("~", cov)
    
    #Fit univariate models
    model_results <- lapply(seq_along(umf_list_tidal), function(i){
      occuMulti(
        stateformulas = as.character(rep(state_formula, 4)),
        detformulas = as.character(rep("~Effort + Year", 4)),
        control = list(maxit = 5000),
        maxOrder = 1,
        starts = rnorm(20, mean = 0, sd = 0.1),
        data = umf_list_tidal[[i]]
      )
    })
  
  #Assess goodness-of-fit
  fit_statistics <- lapply(model_results, function(model) calc_fit(model, fitstats))
  pooled_fit_stats <- pool_fitstats(fit_statistics)
  
  #Run penalized likelihood if p is <0.05
  penalties <- c(0.01, 0.1, 0.2, 0.33, 1, 2)
  pen_model_results <- lapply(seq_along(model_results), function(i) {
    if(pooled_fit_stats$SSE < 0.05 || pooled_fit_stats$Chisq_p < 0.05 || pooled_fit_stats$freeTuke_p < 0.05){
      optimizePenalty(
        model_results[[i]],
        penalties = penalties
      )
    } else {
      NULL
    }
  })
  
  #Re-assess goodness-of-fit
  pen_models <- Filter(Negate(is.null, pen_model_results))
  pen_fit_statistics <- lapply(pen_models, function(model) calc_fit(model, fitstats))
  pooled_pen_fit_stats <- if(length(pen_models) > 0){
    pool_fitstats(pen_fit_statistics)
  } else {
    NULL
  }

  #Pool model results, weighting penalized models based on penalty
  pooled_model_results <- if(length(pen_models) > 0){
    weights <- penalties / sum(penalties)
    pool_results(pen_models, weights)
  } else {
    pool_results(model_results)
  }
    
  #Store final results
  list(
    model = model_results,
    pooled_fit_stats = pooled_fit_stats,
    pen_model = pen_model_results,
    pooled_pen_fit_stats = pooled_pen_fit_stats,
    pooled_model_results = pooled_model_results
    )
  }
  stopCluster(c1)
  return(results)
}                                 

#b. Define a wrapper function to fit models using objects and functions above
fit_uni_models <- function(umf_list_tidal, model_covs) {
  fit_uni_model(umf_list_tidal, model_covs)
}

#c. Apply the wrapper function to the list of global models
uni_models_tidal <- fit_uni_models(model_covs, umf_list_tidal)
uni_models_tidal

#-----
#3. Fit the bivariate models
#a. Create a list of bivariate combinations from the vector of covariates (Step 2)
bi_combos <- combinat::combn(covariates, 2, simplify = FALSE)

#b. Assign starting values
starting_values_length <- 16

#c. Initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("fit_models", "fit_multi_models", "bi_combos", "umf_list", "starting_values_length",
                    "detformulas", "fitstats"))  

#d. Apply the function to the list of models
bi_models <- parLapply(c1, bi_combos, fit_multi_models, umf_list = umf_list,
                       starting_values_length = starting_values_length)
bi_models
stopCluster(c1)
    #Rrav occupancy is positively associated with habitat connectivity PC1 when present (p < 0.05)

#-----
#4. Fit models with three covariates
#a. Create a list of covariate combinations from the vector above (Step 2)
tri_combos <- combinat::combn(covariates, 3, simplify = FALSE)

#b. Assign starting values
starting_values_length <- 20

#c. Re-initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("fit_models", "fit_multi_models", "tri_combos", "umf_list", "starting_values_length",
                    "detformulas", "fitstats"))  

#d. Apply the function to the list of models
tri_models <- parLapply(c1, tri_combos, fit_multi_models, umf_list = umf_list,
                        starting_values_length = starting_values_length)
tri_models[[1]]$pooled_model_results
stopCluster(c1)
#Rrav occupancy is positively associated to conn_vars_PC1 when present (p < 0.05)
#Rmeg occupancy is ALMOST negatively associated with fragmentation (p = 0.06-0.1)

#-----
#5. Fit models with four covariates
#a. Create a list of covariate combinations from the vector above (Step 2)
quad_combos <- combinat::combn(covariates, 4, simplify = FALSE)

#b. Assign starting values
starting_values_length <- 24

#c. Re-initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("fit_models", "fit_multi_models", "quad_combos", "umf_list", "starting_values_length",
                    "detformulas", "fitstats"))  

#d. Apply the function to the list of models
quad_models <- parLapply(c1, quad_combos, fit_multi_models, umf_list = umf_list,
                         starting_values_length = starting_values_length)
quad_models
stopCluster(c1)
#Significant results here

#-------------------------------------------------------------------------------
#MODEL SELECTION

#1. Select the best-fitting model for interpretation
#a. Create a list of pooled model results
pooled_models <- list()
model_lists <- list(uni_models, bi_models, tri_models, quad_models, pent_models)

for(model_list in model_lists){
  for(combo_name in names(model_list)){
    pooled_models[[combo_name]] <- model_list[[combo_name]]$pooled_model_results
  }
}

#b. Create fitList object to store the pooled models 
mods <- fitList(pooled_models)

#c. Generate a model selection table
modSel(mods)

#-------------------------------------------------------------------------------
#MULTISPECIES OCCUPANCY MODELS FOR NON-TIDAL SITES

#1. Formulate the data
#a. Create detection/non-detection matrices
#1) Subset non-tidal sites in the imputed data sets
type <- rodent_imp[[1]] %>%
  dplyr::pull(Type)
tidal_filter <- type == "Tidal"

#2) Extract species detection/non-detection 
Rrav_tidal <- Rrav[tidal_filter, , drop = FALSE]
Rmeg_tidal <- Rmeg[tidal_filter, , drop = FALSE]
Mmus_tidal <- Mmus[tidal_filter, , drop = FALSE]
Mcal_tidal <- Mcal[tidal_filter, , drop = FALSE]

#3) Combine into a named list
ylist_tidal <- list(Rrav=Rrav_tidal, Rmeg=Rmeg_tidal, Mmus=Mmus_tidal, Mcal=Mcal_tidal)
str(ylist_tidal)  

#b. Create a data frame of standardized independent variables
tidal <- siteCovs(umf_list[[1]])$Type == "Tidal"
tidal_covs <- siteCovs(umf_list[[1]])[tidal, , drop = FALSE]


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

