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
library(wesanderson)
library(combinat)
library(reshape2)
library(doParallel)
library(MASS)
library(mgcv)
library(caret)
library(ubms)

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
  dplyr::select(Location, Type, Area, Dist_urban, Above_MHW, Mcal_conn,Mmus_conn, Rrav_conn, Rmeg_conn, Effort, Year) %>%
  mutate(Type = ifelse(Type == "Tidal", 1, 0)) %>%                                #Create a dummy variable for marsh type
  mutate(Year = as.factor(ifelse(Year == 2021, 0, 1)))                            #Create a dummy variable for year

station_df <- read.csv("Bait Station Master Spreadsheet.csv") %>%
  dplyr::select(Location, Surveyor, Rrav, Rmeg, Mmus, Mcal)

#----------
#2. Combine into one data frame and reorder columns
rodent <- left_join(rodent_df, station_df, by = "Location") %>%
  dplyr::select(Location, Surveyor, Rrav, Rmeg, Mmus, Mcal, Type, Area, Dist_urban, Above_MHW, 
                Mcal_conn, Mmus_conn,Rrav_conn, Rmeg_conn, Effort, Year)
rodent

#----------
#3. Mutate data frame to long form
rodent <- rodent %>%
  group_by(Location) %>%
  mutate(repeat_id = row_number()) %>%
  tidyr::pivot_wider(
    names_from = repeat_id,
    values_from = c(Rrav, Rmeg, Mmus, Mcal),
    names_glue ="{.value}_{repeat_id}"
  ) %>%
  ungroup()
print(rodent, n = Inf)

#----------
#4. Impute missing covariate data
#a. Subset the columns with missing data and convert to numeric (if needed)
rodent_sub <- rodent[,c(1, 6)]

#b. Assign imputation methods 
method <- c("", "pmm")              

#c. Impute the data
imp <- mice(rodent_sub, method = method, m = 10, maxit = 30, print = F)
summary(imp)

#d. Perform diagnostics and summarize imputation effectiveness with plots
plot(imp, layout = c(2,5))   
mice::stripplot(imp, Above_MHW ~.imp, pch = 20, cex = 2)
    #All imputed values (in red) are among observed values (in blue) and are therefore plausible

#e. Extract imputed data sets
imputed <- lapply(1:imp$m, function(i) complete(imp, i))

#f. Combine all imputed and original data
rodent_imp <- lapply(seq_along(imputed), function(i){
  complete_imp <- complete(imputed[[i]])
  merged <- merge(rodent[,-c(6)], complete_imp, by = c("Location"), all.x = TRUE) %>%
    dplyr::select(Location, Surveyor, Type, Area, Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Effort, Year, 
                  matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                  matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
  return(merged)
})
rodent_imp

#-------------------------------------------------------------------------------
#DATA EXPLORATION

#1. Visualize relationships between independent and dependent variables with logistic regression curves
#a. Add columns collapsing occupancy across site (e.g., 1 if any bait station was occupied, 0 if not)
rodent_imp <- lapply(rodent_imp, function(df) {
  df %>%
    mutate(
      rrav_occupancy = apply(.[, 11:30], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      rmeg_occupancy = apply(.[, 31:50], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      mmus_occupancy = apply(.[, 51:70], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0)),
      mcal_occupancy = apply(.[, 71:90], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0))
    )
})
rodent_imp[[1]]

#b. Reshape data frame to create a single column for each species names and occupancy
rodent_imp_long <- lapply(rodent_imp, function(df){
  df %>%
    pivot_longer(cols = c(rrav_occupancy, rmeg_occupancy, mmus_occupancy, mcal_occupancy),
                 names_to = "species", values_to = "occupancy") %>%
    filter(!is.na(occupancy))
})
rodent_imp_long[[1]]

#c. Plot each covariate against the response variable (occupancy)
#1) Area
ggplot(rodent_imp_long[[1]], aes(x = Area, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ species) +
  labs(title = "Occupancy vs Patch Area",
       x = "Patch Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#2) Distance to urban area
ggplot(rodent_imp_long[[1]], aes(x = Dist_urban, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ species) +
  labs(title = "Occupancy vs Distance to Urban Area",
       x = "Distance to Urban Area", 
       y = "Occupancy Probability") +
  theme_minimal()

#3) Inundation
ggplot(rodent_imp_long[[1]], aes(x = Above_MHW, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  facet_wrap(~ species) +
  labs(title = "Occupancy vs Inundation",
       x = "Inundation", 
       y = "Occupancy Probability") +
  theme_minimal()

#4) Connectivity   
rrav_data <- rodent_imp_long[[1]] %>%
  filter(species == "rrav_occupancy")
ggplot(rrav_data, aes(x = Rrav_conn, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Rrav Occupancy vs Connectivity",
       x = "Connectivity", 
       y = "Occupancy Probability") +
  theme_minimal()

rmeg_data <- rodent_imp_long[[1]] %>%
  filter(species == "rmeg_occupancy")
ggplot(rmeg_data, aes(x = Rmeg_conn, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Rmeg Occupancy vs Connectivity",
       x = "Connectivity", 
       y = "Occupancy Probability") +
  theme_minimal()

mmus_data <- rodent_imp_long[[1]] %>%
  filter(species == "mmus_occupancy")
ggplot(mmus_data, aes(x = Mmus_conn, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Mmus Occupancy vs Connectivity",
       x = "Connectivity", 
       y = "Occupancy Probability") +
  theme_minimal()

mcal_data <- rodent_imp_long[[1]] %>%
  filter(species == "mcal_occupancy")
ggplot(mcal_data, aes(x = Mcal_conn, y = occupancy)) +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.3) +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Mcal Occupancy vs Connectivity",
       x = "Connectivity", 
       y = "Occupancy Probability") +
  theme_minimal()
    #NOTE: For each species, all covariates exhibit cubic or higher-order 
    #polynomial relationships with occupancy. However, due to difficulty in 
    #interpreting these relationships, select only those that are biologically
    #relevant to test with AIC

#----------
#2. Fit logistic regression models to evaluate transformations
#a. Define the independent and dependent variables
dep_vars <- c("rrav_occupancy", "rmeg_occupancy", "mmus_occupancy", "mcal_occupancy")
covs <- c("Area", "Dist_urban", "Above_MHW", "Rrav_conn", "Rmeg_conn", "Mmus_conn", "Mcal_conn")

#b. Define the transformations
transformations <- c("linear", "log")

#c. Define a function to calculate shifted covariate values for log transformations (log(x), x>0)
calc_shift <- function(data, covariate) {
  min_value <- min(data[[covariate]], na.rm = TRUE)
  if (min_value <= 0) abs(min_value) + 0.01 else 0
}

#d. Define another function to dynamically apply the transformations and assess best fit with AIC
transform_covs <- function(rodent_imp, dep_vars, covs, transformations) {
  pooled_results <- list()
  
  #Map connectivity covariates to the corresponding dependent variables
  cov_map <- list(rrav_occupancy = c("Rrav_conn"), 
                  rmeg_occupancy = c("Rmeg_conn"),
                  mmus_occupancy = c("Mmus_conn"),
                  mcal_occupancy = c("Mcal_conn"))
  
  for (dep_var in dep_vars) {
    dep_results <- list()
    
    #Get the relevant covariates for each dependent variable
    relevant_covs <- cov_map[[dep_var]]
    other_covs <- setdiff(covs, relevant_covs)
    all_covs <- c(relevant_covs, other_covs)
    species_covs <- c(relevant_covs, setdiff(c("Area", "Dist_urban", "Above_MHW"), cov_map[[dep_var]]))
    
    for (cov in species_covs) {
      
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
          
          #Apply transformations
          transformed_data <- imp
          if (trans == "log") {
            transformed_data <- transformed_data %>%
              mutate(!!sym(cov) := log(!!sym(cov) + shift_value))
          } else if (trans == "linear"){
            transformed_data <- transformed_data
          }
          
          #Fit the linear models and extract AIC values
          formula <- as.formula(paste(dep_var, "~", cov))
          model <- lm(formula, data = transformed_data)
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
      dep_results[[cov]] <- pooled_aic
    }
    pooled_results[[dep_var]] <- dep_results
  }
  return(pooled_results)
}

#1) Apply the function
pooled_results <- transform_covs(
  rodent_imp = rodent_imp,  
  dep_vars = dep_vars,
  covs = covs,
  transformations = transformations
)
pooled_results
    #Log-transformed area and connectivity are a better predictor of Rrav occupancy, linear
    #connectivity is a better predictor of Rmeg occupancy. All others have delta AIC <=2

#e. Scale and transform independent variables (as needed)
scale_transform <- function(rodent_imp){
  transformed_data <- lapply(rodent_imp, function(data){
    data <- data %>%
      mutate(
        Log_Area = log(Area),
        Log_Rrav_conn = log(Rrav_conn)
      )
    covs <- c("Area", "Log_Area", "Dist_urban", "Above_MHW", "Rrav_conn", "Log_Rrav_conn", "Rmeg_conn", "Mcal_conn", "Mmus_conn")
    data <- data %>%
      mutate(across(all_of(covs),~as.vector(scale(.)))) %>%
      dplyr::select(Location, Surveyor, Type, Area, Log_Area, Dist_urban, Above_MHW, Rrav_conn, Log_Rrav_conn, Rmeg_conn, 
                    Mcal_conn, Mmus_conn, Effort, Year, rrav_occupancy, rmeg_occupancy, mcal_occupancy, mmus_occupancy,
                    matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                    matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$"))
    return(data)
  })
  return(transformed_data)
}
rodent_imp <- scale_transform(rodent_imp)
rodent_imp[[1]]

#----------
#3. Check for collinearity among continuous independent variables
#a. Define the independent variables
covs2 <- c("Log_Area", "Dist_urban", "Above_MHW", "Log_Rrav_conn", "Rmeg_conn", "Mcal_conn", "Mmus_conn")

#b. Assess multicollinearity with variance inflation factor (VIF) 
for(dep_var in dep_vars){                                              
  dep_vif <- list()
  for(i in seq_along(rodent_imp)){                                                
    complete_data <- rodent_imp[[i]]
    formula <- as.formula(paste(dep_var, "~", paste(covs2, collapse = "+")))
    model <- lm(formula, data = complete_data)
    vif_values <- vif(model)
    dep_vif[[i]] <- vif_values
  }
}
dep_vif

#c. Examine pairwise comparisons between collinear variables
cor_mats <- list()
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, covs2], use = "pairwise.complete.obs")
  cor_mats[[i]] <- cor_matrix
}
cor_mats
    #Connectivity measures are collinear, incorporate random slopes and random
    #intercept of species to account for species-specific effects on occupancy

#-------------------------------------------------------------------------------
#4. Explore non-additive effects of interactions
#a. Connectivity*area (connectivity may enhance occupancy in smaller marshes)
#1) Fit the linear models to evaluate the interaction
#a) Define the connectivity variables
conn_vars <- c("Rrav_conn", "Rmeg_conn", "Mcal_conn", "Mmus_conn")

#b) Define a function to fit the linear models
fit_conn_area_models <- function(rodent_imp, conn_vars) {
  models_list <- list()
  for (i in seq_along(conn_vars)) {
    conn_area_models <- list()
    for (j in seq_along(rodent_imp)) {
      data <- rodent_imp[[j]]
      formula <- as.formula(paste(conn_vars[i], "~ Log_Area + Log_Area:", conn_vars[i]))
      model <- lm(formula, data = data)
      conn_area_models[[j]] <- model
    }
    models_list[[conn_vars[i]]] <- conn_area_models
  }
  return(models_list)
}

#c) Apply the function and view results
conn_area_models <- fit_conn_area_models(rodent_imp, conn_vars)

#2) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs <- lapply(seq_along(conn_area_models), function(i){
  models <- conn_area_models[[i]]
  coefs <- lapply(seq_along(models), function(j){
    tidy(models[[j]]) %>%
      filter(grepl(":Log_Area", term)) %>%
      mutate(imp = j)
  })
  preds <- lapply(seq_along(models), function(j){
    model <- models[[j]]
    preds <- predict(model, newdata = rodent_imp[[j]], type = "response")
    pred_data <- rodent_imp[[j]] %>%
      mutate(prediction = preds, imp = j)
  return(pred_data)
  })
  return(list(coefs = bind_rows(coefs), preds = bind_rows(preds)))
})
preds_df <- bind_rows(lapply(preds_coefs, `[[`, "preds"))
coefs_df <- bind_rows(lapply(preds_coefs, `[[`, "coefs"))
print(coefs_df, n = Inf)
    #There is no evidence of an interaction (p > 0.05), plot to confirm

#3) Visualize the interaction as it relates to occupancy, holding one covariate constant
#a) Fit the linear model to the list of imputed data sets
conn_area_models1 <- lapply(dep_vars, function(response){
  lapply(rodent_imp, function(data){
    int_term <- paste(response, ":Log_Area", sep ="")
    formula <- as.formula(paste(response, "~", int_term))
    lm(formula, data = data)
  })
})
names(conn_area_models1) <- dep_vars
conn_area_models1

#b) Generate a prediction grid for each covariate
pred_grid <- list()
for(i in seq_along(rodent_imp)){
  data <- rodent_imp[[i]]
  grid <- expand.grid(
    Log_Area = seq(min(data$Log_Area), max(data$Log_Area), length.out = 100),
    Rrav_conn = seq(min(data$Rrav_conn), max(data$Rrav_conn), length.out = 100),
    Rmeg_conn = seq(min(data$Rmeg_conn), max(data$Rmeg_conn), length.out = 100),
    Mcal_conn = seq(min(data$Mcal_conn), max(data$Mcal_conn), length.out = 100),
    Mmus_conn = seq(min(data$Mmus_conn), max(data$Mmus_conn), length.out = 100)
  )
  pred_grid[[j]] <- grid
}
head(pred_grid[[1]])

#c) Fix each variable at its mean
area_grid <- data.frame(
  Area = area_range,
  Conn_vars = mean(rodent_comb$Conn_vars)
)
conn_grid <- data.frame(
  Area = mean(rodent_comb$Area),
  Conn_vars = conn_range
)

#d) Predict occupancy for each covariate
area_preds <- lapply(conn_area_models1, function(model_list){
  lapply(model_list, function(model){
    predict(model, newdata = area_grid, type = "response")
  })
})
conn_preds <- lapply(conn_area_models1, function(model_list){
  lapply(model_list, function(model){
    predict(model, newdata = conn_grid, type = "response")
  })
})

#e) Convert predictions to data frame
area_preds_df <- do.call(rbind, lapply(area_preds, function(preds){
  data.frame(area_grid, prediction = unlist(preds))
}))
conn_preds_df <- do.call(rbind, lapply(conn_preds, function(preds){
  data.frame(conn_grid, prediction = unlist(preds))
}))

#f) Add identifier columns in each data frame
num_imp <- 10
grid_size <- 100
species_rows <- num_imp*grid_size

area_preds_df <- area_preds_df %>%
  mutate(variable = "Area", 
         imp = rep(rep(1:num_imp, each = grid_size), times = length(dep_vars2)))
conn_preds_df <- conn_preds_df %>%
  mutate(variable = "Connectivity", 
         imp = rep(rep(1:num_imp, each = grid_size), times = length(dep_vars2)))

#g) Combine the grids and add a column for species name
plot_data <- rbind(area_preds_df, conn_preds_df)
plot_data <- plot_data %>%
  mutate(species = rownames(plot_data)) %>%
  mutate(species = str_extract(species, "^[a-zA-Z]+"))
plot_data

#h) Plot the interaction
ggplot(plot_data, aes(x = ifelse(variable == "Area", Area, Conn_vars),
                      y = prediction, color = variable)) +
  geom_line() +
  labs(x = "Covariate value", y = "Predicted occupancy", color = "Variable") +
  theme_minimal() +
  facet_wrap(imp~species, scales = "free_y", nrow = 10, ncol = 4) +
  theme(strip.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"))
#The interaction between connectivity and fragmentation likely exists, as
#evidenced by the intersecting lines. It is stronger for Mcal and Mmus than
#Rmeg and Rrav, and the directionality changes based on the species. 

#-------------------------------------------------------------------------------
#FORMULATE THE DATA

#1. Create presence/absence matrices 
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

#b. Combine into one named list
ylist <- list(Rrav=Rrav, Rmeg=Rmeg, Mmus=Mmus, Mcal=Mcal)
str(ylist)

#----------
#2. Create data frame of standardized site covariates
#a. Extract covariate names
covs <- colnames(rodent_imp[[1]])
covs <- covs[c(3:9)]

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
  sse <- sum(resids^2, na.rm = TRUE)
  chisq <- sum((observed-expected)^2/expected, na.rm = TRUE)
  freeTuke <- sum((sqrt(observed)-sqrt(expected))^2, na.rm = TRUE)
  out <- c(SSE = sse, Chisq = chisq, freemanTukey = freeTuke)
  return(out)
}

#b. Define another function to apply fitstats
calc_null_fit <- function(model, fitstats){
  return(parboot(model, fitstats, nsim = 100))
}

#c. Set up parallel computing - this needs to be reinitialized for each parallel computing segment
RNGkind("L'Ecuyer-CMRG")                                                   #Manage parallel RNG to get consistent but unique results
c1 <- makeCluster(detectCores()-1)                                         #Make cluster using all but 1 core processor
registerDoParallel(c1)

#d. Apply the function to the list of models
null_fit <- foreach(i = seq_along(null_models), .packages = c("unmarked")) %dopar% {
  calc_null_fit(null_models[[i]], fitstats)
}
null_fit
stopCluster(c1)

#c. Pool the results
#1) Extract fit statistics from each model
#a) p-values
null_sse_p <- sapply(null_fit, function(fit) mean(fit@t.star[,1] >= fit@t0[1]))         
null_chisq_p <- sapply(null_fit, function(fit) mean(fit@t.star[,2] >= fit@t0[2]))
null_freeTuke_p <- sapply(null_fit, function(fit) mean(fit@t.star[,3] >= fit@t0[3]))

#b) t0
null_sse <- sapply(null_fit, function(fit) fit@t0["SSE"])
null_chisq <- sapply(null_fit, function(fit) fit@t0["Chisq"])
null_freeTuke <- sapply(null_fit, function(fit) fit@t0["freemanTukey"])

#2) Pool the fit statistics
#a) p-values (because all are same, select first in list)
null_pooled_sse_p <- null_sse_p[1]                                                #0.27, model is good fit
null_pooled_chisq_p <- null_chisq_p[1]                                            #0.39, model is good fit
null_pooled_freeTuke_p <- null_freeTuke_p[1]                                      #0.42, model is good fit

#b) Average t0
null_fit_pooled <- list(
  Chisq = mean(null_chisq, na.rm = TRUE),
  SSE = mean(null_sse, na.rm = TRUE),
  freemanTukey = mean(null_freeTuke, na.rm = TRUE)
)
null_fit_pooled

#d. Visualize model fit  #####################REVISIT##########################
#1) Plot the fit statistics
plot(null_fit[[1]])

#OR
#2) Plot fitted vs. residual values (NEEDS WORK)
null_fitted <- list()
null_resid <- list()

for(i in seq_along(null_models)){
  null_fitted[[i]] <- fitted(null_models[[i]])
  null_resid[[i]] <- residuals(null_models[[i]])
}
null_resid[[1]]

null_pooled_fitted <- Reduce("+", null_fitted)/length(null_fitted)
null_pooled_resids <- Reduce("+", null_resid)/length(null_resid)

null_pooled <- data.frame(Fitted = as.vector(null_pooled_fitted),
                          Residuals = as.vector(null_pooled_resids),
                          Species = rep(colnames(null_pooled_fitted), each = nrow(null_pooled_fitted)))


ggplot(data.frame(Fitted = null_pooled_fitted, Residuals = null_pooled_resid), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values") +
  theme_minimal()
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
#2. Rmeg and Mcal are likely to occupy the same sites 
#3. Detection is highest for Rrav but followed closely by Mmus

#-------------------------------------------------------------------------------
#CALCULATE OCCUPANCY AND DETECTION PROBABILITIES

#1. Occupancy
#a. Predict probability for each occupancy state
occ_probs <- lapply(null_models, function(null_models){
  all_probs <- predict(null_models, newdata = data.frame(site=1),type="state")
  return(all_probs)
})
occ_probs[[1]]$Predicted

#b. Calculate predicted marginal occupancy for each species
occ_preds <- lapply(null_models, function(null_models){
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
#Rrav = 0.507, lower = 0.401, upper = 0.595
#Rmeg = 0.500, lower = 0.405, upper = 0.607
#Mmus = 0.678, lower = 0.529, upper = 0.782
#Mcal = 0.342, lower = 0.220, upper = 0.496

#d. Visualize marginal occupancy for all species
level_order <- c("Rrav", "Rmeg", "Mmus", "Mcal")

ggplot(occ_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
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
det_preds <- lapply(null_models, function(null_models){
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
#Rrav = 0.380, lower = 0.342, upper = 0.420
#Rmeg = 0.271, lower = 0.234, upper = 0.311
#Mmus = 0.367, lower = 0.333, upper = 0.402
#Mcal = 0.094, lower = 0.060, upper = 0.143

#c. Visualize detection probabilities for all species
ggplot(det_pooled, aes(x=factor(Species, level=level_order), y=Predicted)) +
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
#MULTISPECIES DETECTION

#1. Assess effects of Effort and Year on detectability 
#a. Assign detection covariates
det_vars <- c("Effort", "Year")

#b. Generate all combinations of covariates
det_combos <- expand.grid(
  Effort = c(TRUE, FALSE),
  Year = c(TRUE, FALSE)
)

#b. Define a function to create detection formulas
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

#c. Initiate a list to store results
det_results <- list()

#d. Loop through each combination of detection formulas
for(i in 1:nrow(det_combos)){
  combo <- det_combos[i, ]
  det_formula <- create_det_formula(combo, det_vars)
  model <- tryCatch({
    occuMulti(
      stateformulas = c("~1", "~1", "~1", "~1"),
      detformulas = as.character(rep(list(det_formula), 4)),
      data = umf_list[[i]],
      maxOrder = 1
    )
  })
  if(!is.null(model)){
    det_results[[i]] <- list(
      model = model,
      formula = det_formula,
      AIC = model@AIC
    )
  }
}
det_results

#e. Assess model fit based on AIC
#1) Define a function to extract models with delta AIC <=2
id_best_model <- function(det){
  models <- list()
  det_aic <- sapply(det, function(x) x$AIC)
  best_aic <- min(det_aic)
  delta_aic <- det_aic - best_aic
  models <- det[delta_aic <= 2]
  return(models)
}

#2) Apply the function
det_models <- id_best_model(det_results)
det_models
#Best fitting models are ~Effort and ~1, but only p-values for intercept are significant
#Keep detection in occupancy models as intercept-only

#-------------------------------------------------------------------------------
#MULTISPECIES OCCUPANCY MODELS
#NOTE: THE FOLLOWING MODELS INCLUDE INUNDATION VALUES FOR NON-TIDAL SITES. INTERPRET WITH CAUTION

#1. Fit the global model
#a. Define a function to fit the global model (excluding species interactions)
starting_values <- rnorm(28, mean = 0, sd = 0.02)                                 #There should be one starting value for every occupancy
#and detection estimate in the model
fit_global_model <- function(umf_list, detformulas, starting_values){
  model_list <- list()
  for(i in seq_along(umf_list)){
    if(i == 1) {
      state_formula <- c("~Dist_urban + (Above_MHW*Type + Conn_vars + Frag_vars + (Conn_vars*log(Frag_vars))")
    } else {
      state_formula <- c("~Dist_urban + (Above_MHW*Type) + Conn_vars + Frag_vars + (Conn_vars*Frag_vars)")
    }
    state_formulas <- rep(list(state_formula), 4)
    for(i in seq_along(umf_list)){
      model_list[[i]] <- occuMulti(
        stateformulas = as.character(state_formulas),
        detformulas = detformulas,           
        control = list(maxit=50000),
        method = "Nelder-Mead",               #To avoid calculating Hessian matrix
        maxOrder = 1,                         #Do not include species interactions
        starts = starting_values,
        data = umf_list[[i]]
      )}
  }
  return(model_list)
}

#b. Apply the function to the umf list
global_model <- fit_global_model(umf_list)             
summary(global_model[[1]])
#Model runs but may be poor fit (high SEs), check goodness-of-fit and run penalized likelihood if necessary

#c. Assess goodness-of-fit on model residuals
#1) Define a function to calculate goodness-of-fit measures with parboot
calc_fit <- function(model, fitstats){
  set.seed(500)
  return(parboot(model, fitstats, nsim = 100))
}

#2) Set up parallel computing 
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("global_model", "calc_global_fit", "fitstats"))         

#3) Fit the function to the global models
global_fit <- clusterApply(c1, global_model, calc_global_fit, fitstats)
global_fit
stopCluster(c1)
#Some models do not capture variance well (Chi-square <0.05). Run penalized 
#likelihood on ALL models to maintain consistency while pooling results

#d. Fit the model with penalized likelihood (if needed)                                     
#1) Define a function to fit the model with penalized likelihood
set.seed(500)
fit_global_model_pen <- function(model){
  model_list <- list()
  for(i in seq_along(model)){
    model_list[[i]] <- unmarked::optimizePenalty(
      global_model[[i]], 
      penalties = c(0.1, 0.2, 0.33, 1, 2)
    )
  }
  return(model_list)
}

#2) Re-initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("global_model", "fit_global_model_pen", "starting_values"))  

#3) Apply the function to the list of models
global_model_pen <- clusterApply(c1, global_model, fit_global_model_pen)
global_model_pen
stopCluster(c1)

#e. Assess goodness-of-fit on model residuals
#1) Un-nest list of global models
global_model_pen <- lapply(global_model_pen, `[[`, 1)
summary(global_model_pen)

#2) Re-initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("global_model_pen", "calc_fit", "fitstats"))  

#3) Apply the function to the list of penalized models
global_fit_pen <- clusterApply(c1, global_model_pen, calc_global_fit, fitstats)
global_fit_pen
stopCluster(c1)

#4) View p-values
global_fit[[1]]
#Penalized model is a good fit, okay to pool results for interpretation

#f. Pool the results of the penalized likelihood models
global_results <- pool_results(global_model_pen)
global_results
#No significant results although distance to nearest urban area is almost significant for Mcal (0.0554)

#-----
#2. Fit the univariate models
#a. Create a vector of covariates
covariates <- c("Dist_urban", "Above_MHW", "Conn_vars", "Frag_vars", "(Conn_vars*Frag_vars)")

#b. Create a list of univariate combinations from the vector of covariates
uni_combos <- lapply(covariates, function(cov) list(cov))

#c. Assign starting values
starting_values_length <- 12

#d. Define a function to fit models, calculate GOF statistics, and run penalized likelihood (if necessary)
fit_models <- function(umf_list, uni_combos, starting_values_length){
  results <- list()
  results <- lapply(uni_combos, function(combo){
    state_formula <- paste("~", paste(combo, collapse = "+"))
    starting_values <- rnorm(starting_values_length, mean = 0, sd = 0.01)
    model_results <- list()
    p_values <- list()
    fit_statistics <- list()
    pen_results <- list()
    
    #Fit models
    for(i in seq_along(umf_list)){
      model_results[[i]] <- occuMulti(
        stateformulas = rep(state_formula, 4),
        detformulas = detformulas,
        control = list(maxit = 5000),
        maxOrder = 1,
        starts = starting_values,
        data = umf_list[[i]]
      )
    }
    
    #Assess goodness-of-fit
    for(i in seq_along(model_results)){
      fit_list <- parboot(model_results[[i]], fitstats, nsim = 100)                
      
      #Extract p-values from GOF output
      p_values[[i]] <- list(
        SSE_p = mean(fit_list@t.star[,1] >= fit_list@t0[1]),
        Chisq_p = mean(fit_list@t.star[,2] >= fit_list@t0[2]),
        FreemanTukey_p = mean(fit_list@t.star[,3] >= fit_list@t0[3])
      )
      
      #Extract fit statistics (SSE, Chi-square, and Freeman-Tukey)
      fit_statistics[[i]] <- list(
        SSE = fit_list@t0["SSE"],
        Chisq = fit_list@t0["Chisq"],
        FreemanTukey = fit_list@t0["freemanTukey"]
      )
    }
    
    #Run penalized likelihood if p is <0.05
    for(i in seq_along(fit_statistics)){
      if(p_values[[i]]$SSE_p < 0.05 || p_values[[i]]$Chisq_p < 0.05 || p_values[[i]]$FreemanTukey_p < 0.05){
        pen_model_name <- paste("fit_", paste(combo, collapse = "_"), "_model_pen", i, sep = "")
        pen_model <- occuMulti(
          stateformulas = rep(state_formula, 4),
          detformulas = detformulas,
          control = list(maxit = 5000),
          maxOrder = 1,
          starts = starting_values,
          data = umf_list[[i]],
          penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
        )
        pen_results[[pen_model_name]] <- pen_model
        
        #Re-assess goodness-of-fit
        pen_fit <- parboot(pen_model, fitstats, nsim = 100)                     
        
        #Extract penalized model fit statistics
        fit_statistics[[length(fit_statistics) + 1]] <- list(
          SSE = pen_fit@t0["SSE"],
          Chisq = pen_fit@t0["Chisq"],
          FreemanTukey = pen_fit@t0["freemanTukey"]
        )
      }
    }
    
    #Pool model results and fit statistics
    pooled_model_results <- pool_results(model_results)
    pooled_pen_results <- if(length(pen_results) > 0) pool_results(pen_results) else NULL
    pooled_fit_stats <- list(
      SSE = mean(sapply(fit_statistics, function(stat) stat$SSE), na.rm = TRUE),
      Chisq = mean(sapply(fit_statistics, function(stat) stat$Chisq), na.rm = TRUE),
      FreemanTukey = mean(sapply(fit_statistics, function(stat) stat$FreemanTukey), na.rm = TRUE)
    )
    
    #Store final results
    combo_name <- paste(combo, collapse ="_")
    results[[combo_name]] <- list(
      model = model_results,
      p = p_values,
      pooled_fit = pooled_fit_stats,
      pooled_model_results = pooled_model_results,
      pooled_pen_results = pooled_pen_results
    )
  })
  return(results)
}

#e. Define a wrapper function to fit models using objects and functions above
fit_multi_models <- function(combos, umf_list, starting_values_length) {
  fit_models(umf_list, combos, starting_values_length)
}

#f. Initiate parallel computing
c1 <- makeCluster(detectCores()-1)                                        
clusterEvalQ(c1, library(unmarked))                                     
clusterExport(c1, c("fit_models", "fit_multi_models", "uni_combos", "umf_list", "starting_values_length",
                    "detformulas", "fitstats"))  

#g. Apply the function to the list of univariate models
uni_models <- parLapply(c1, uni_combos, fit_multi_models, umf_list = umf_list,
                        starting_values_length = starting_values_length)
uni_models
stopCluster(c1)
#Rrav is positively associated with habitat connectivity PC1 (p = 0.01)

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

