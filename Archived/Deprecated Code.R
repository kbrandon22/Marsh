#DEPRECATED CODE

#-------------------------------------------------------------------------------
#DATA PREPARATION

#1) Impute missing data
#a)  Subset the columns with missing data and convert to numeric
rodent_sub_std <- rodent[,c(1, 36:40)]
rodent_sub_std[,c(2:6)] <- lapply(rodent_sub_std[,c(2:6)], as.numeric)

#b) Impute the data
imp_std <- lapply(imputed, function(rodent_data){
  mice(rodent_sub_std, method = method, seed = 500, m = 1, maxit = 10, print = F)
})

#c) Perform diagnostics and summarize imputation effectiveness with plots
plot(imp_std, layout = c(2,5))   

mice::stripplot(imp_std, Above_MHW_std ~.imp_std, pch=20, cex=2)
mice::stripplot(imp_std, Above_MHHW_std ~.imp_std, pch=20, cex=2)
mice::stripplot(imp_std, Elev_min_std ~.imp_std, pch=20, cex=2)
mice::stripplot(imp_std, Elev_max_std ~.imp_std, pch=20, cex=2)
mice::stripplot(imp_std, Elev_range_std ~.imp_std, pch=20, cex=2)
    #All imputed values (in red) are among observed values (in blue) and are therefore plausible

#-------------------------------------------------------------------------------
#CHECK FOR COLLINEARITY

#Convert character to factor
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cols <- c("Type")
  complete_data[cols] <- lapply(complete_data[cols], as.factor)
  rodent_imp[[i]] <- complete_data
}

#----------
#4. Reduce collinearity between fragmentation metrics
#a. Remove edge from the imputed datasets
for(i in seq_along(rodent_imp)){
  rodent_imp[[i]] <- rodent_imp[[i]][,-c(7)]
}
rodent_imp

#b. Initiate a list to store correlation matrices
cor_mats3 <- list()

#c. Specify the variables to include in the correlation matrix
occ_vars3 <- c("Area", "Dist_urban", "Above_MHW", "Frag", "Conn_vars")

#d. Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, occ_vars3], use = "pairwise.complete.obs")
  cor_mats3[[i]] <- cor_matrix
}
cor_mats3

#----------
#5. Reduce collinearity between fragmentation and area metrics
#a. Create a vector of fragmentation metrics and area
frag_vars <- c("Frag", "Area")

#b. Loop through each imputed dataset and append the results to the imputed datasets
for(i in seq_along(rodent_imp)){
  data <- rodent_imp[[i]]
  pca_result <- prcomp(data[frag_vars])
  pca_scores <- as.data.frame(pca_result$x[, 1, drop = FALSE])
  pca_scores <- round(pca_scores, 5)
  loadings <- pca_result$rotation[, 1]
  rodent_imp[[i]]["Frag_vars"] <- pca_scores[, 1]
}
rodent_imp

#c. Re-assess correlation
#1) Initiate a list to store correlation matrices
cor_mats4 <- list()

#2) Specify the variables to include in the correlation matrix
occ_vars4 <- c("Dist_urban", "Above_MHW", "Conn_vars", "Frag_vars")

#3) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, occ_vars4], use = "pairwise.complete.obs")
  cor_matrix <- round(cor_matrix, digits = 5)
  cor_mats4[[i]] <- cor_matrix
}
cor_mats4

#d. Remove original metrics from imputed data sets and reorder columns
for(i in seq_along(rodent_imp)){
  rodent_imp[[i]] <- rodent_imp[[i]][,-c(4, 7)]
}

order <- function(data){
  data %>%
    dplyr::select(Location, Surveyor, Type, Dist_urban, Above_MHW, Conn_vars, Frag_vars, Effort, Year,
                  matches("^Rrav_[1-9]$|^Rrav_1[0-9]$|^Rrav_20$"), matches("^Rmeg_[1-9]$|^Rmeg_1[0-9]$|^Rmeg_20$"),
                  matches("^Mmus_[1-9]$|^Mmus_1[0-9]$|^Mmus_20$"), matches("^Mcal_[1-9]$|^Mcal_1[0-9]$|^Mcal_20$")
    )
}
rodent_imp <- lapply(rodent_imp, function(x) order(x))
str(rodent_imp)

#-------------------------------------------------------------------------------
#CHECK FOR POTENTIAL INTERACTIONS

#a. Fragmentation*connectivity
#1) Define the response variables 
dep_vars2 <- c("rrav_occupancy", "rmeg_occupancy", "mmus_occupancy", "mcal_occupancy")

#2) Split the data frame by imputation
rodent_split <- split(rodent_comb, rodent_comb$imputation)

#3) Fit the linear models to evaluate the interaction
frag_conn_models <- lapply(rodent_split, function(data){
  lm(Conn_vars ~ Frag_vars + Frag_vars:Conn_vars, data = data)
})

#4) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs <- lapply(seq_along(frag_conn_models), function(i){
  model <- frag_conn_models[[i]]
  coefs <- tidy(model) %>%
    filter(term == "Conn_vars:Frag_vars") %>%
    mutate(imp = i)
  preds <- predict(model, newdata = rodent_split[[i]], type = "response")
  pred_data <- rodent_imp_long[[i]] %>%
    mutate(prediction = preds)
  return(list(coefs = coefs, preds = pred_data))
})
preds_df <- bind_rows(lapply(preds_coefs, `[[`, "preds"))
coefs_df <- bind_rows(lapply(preds_coefs, `[[`, "coefs"))
    #The interaction term is significant in most models, confirm with plot

#5) Visualize the interaction as it relates to occupancy, holding one covariate constant
#a) Fit the linear model to the list of imputed data sets
frag_conn_models1 <- lapply(dep_vars2, function(response){
  lapply(rodent_split, function(data){
    lm(as.formula(paste(response, "~Frag_vars*Conn_vars")), data = data)
  })
})
names(frag_conn_models1) <- dep_vars2
frag_conn_models1

#b) Generate a prediction grid for each covariate
frag_range <- seq(min(rodent_comb$Frag_vars), max(rodent_comb$Frag_vars), length.out = 100)
conn_range <- seq(min(rodent_comb$Conn_vars), max(rodent_comb$Conn_vars), length.out = 100)

#c) Fix each variable at its mean
frag_grid <- data.frame(
  Frag_vars = frag_range,
  Conn_vars = mean(rodent_comb$Conn_vars)
)
conn_grid <- data.frame(
  Frag_vars = mean(rodent_comb$Frag_vars),
  Conn_vars = conn_range
)

#d) Predict occupancy for each covariate
frag_preds <- lapply(frag_conn_models1, function(model_list){
  lapply(model_list, function(model){
    predict(model, newdata = frag_grid, type = "response")
  })
})
conn_preds <- lapply(frag_conn_models1, function(model_list){
  lapply(model_list, function(model){
    predict(model, newdata = conn_grid, type = "response")
  })
})

#e) Convert predictions to data frame
frag_preds_df <- do.call(rbind, lapply(frag_preds, function(preds){
  data.frame(frag_grid, prediction = unlist(preds))
}))
conn_preds_df <- do.call(rbind, lapply(conn_preds, function(preds){
  data.frame(conn_grid, prediction = unlist(preds))
}))

#f) Add identifier columns in each data frame
num_imp <- 10
grid_size <- 100
species_rows <- num_imp*grid_size

frag_preds_df <- frag_preds_df %>%
  mutate(variable = "Fragmentation", 
         imp = rep(rep(1:num_imp, each = grid_size), times = length(dep_vars2)))
conn_preds_df <- conn_preds_df %>%
  mutate(variable = "Connectivity", 
         imp = rep(rep(1:num_imp, each = grid_size), times = length(dep_vars2)))

#g) Combine the grids and add a column for species name
plot_data <- rbind(frag_preds_df, conn_preds_df)
plot_data <- plot_data %>%
  mutate(species = rownames(plot_data)) %>%
  mutate(species = str_extract(species, "^[a-zA-Z]+"))
plot_data

#h) Plot the interaction
ggplot(plot_data, aes(x = ifelse(variable == "Fragmentation", Frag_vars, Conn_vars),
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

#b. Connectivity*area
#2) Fit the linear models to evaluate the interaction
conn_area_models <- lapply(rodent_split, function(data){
  models <- list()
  
  #Log transform area when the dependent variable is rrav_occupancy
  for(dep_var in dep_vars2){
    if(dep_var == "rrav_occupancy"){
      data <- data %>%
        mutate(Area = log(Area + calc_shift(data, "Area")))
    }
    
    #Fit the linear models and store in a named list of lists
    formula <- as.formula("Conn_vars ~ Area + Area:Conn_vars")
    model <- lm(formula, data = data)
    names <- str_extract(dep_var, "^[^_]+")
    models[[names]] <- model
  } 
  return(models)
})
names(conn_area_models) <- dep_vars2
summary(conn_area_models$rrav_occupancy[[1]])

#b. Fragmentation*Dist_urban
#1) Fit the linear models to evaluate the interaction
frag_dist_models <- lapply(rodent_split, function(data){
  lm(Frag_vars ~ Dist_urban + Dist_urban:Frag_vars, data = data)
})

#2) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs2 <- lapply(seq_along(frag_dist_models), function(i){
  model <- frag_dist_models[[i]]
  coefs <- tidy(model) %>%
    filter(term == "Frag_vars:Dist_urban") %>%
    mutate(imp = i)
  preds <- predict(model, newdata = rodent_split[[i]], type = "response")
  pred_data <- rodent_imp_long[[i]] %>%
    mutate(prediction = preds)
  return(list(coefs = coefs, preds = pred_data))
})
preds_df2 <- bind_rows(lapply(preds_coefs2, `[[`, "preds"))
coefs_df2 <- bind_rows(lapply(preds_coefs2, `[[`, "coefs"))
    #The interaction term is not significant in most models and estimates are close
    #to 0, no evidence for a linear interaction. Check for a nonlinear interaction

#3) Check for a nonlinear interaction with generalized additive models (GAM)
#a) Fit GAM models for each species and each imputation
frag_dist_gam <- lapply(dep_vars2, function(response){
  lapply(rodent_split, function(data){
    formula <- as.formula(paste(response, "~s(Frag_vars, Dist_urban)"))
    gam(formula, data = data)
  })
})
names(frag_dist_gam) <- dep_vars2
summary(frag_dist_gam[[1]][[1]])

#b) Generate prediction grids for each covariate
frag_gam_grid <- expand.grid(
  Frag_vars = seq(min(rodent_comb$Frag_vars), max(rodent_comb$Frag_vars), length.out = 100),
  Dist_urban = mean(rodent_comb$Dist_urban)
)
dist_gam_grid <- expand.grid(
  Frag_vars = mean(rodent_comb$Frag_vars),
  Dist_urban = seq(min(rodent_comb$Dist_urban), max(rodent_comb$Dist_urban), length.out = 100)
)

#c) Create predictions for each GAM model
frag_gam_preds <- lapply(frag_dist_gam, function(model_list){
  lapply(model_list, function(model){
    pred <- predict(model, newdata = frag_gam_grid, type = "response")
    cbind(frag_gam_grid, prediction = pred, variable = "Fragmentation")
  })
})
dist_gam_preds <- lapply(frag_dist_gam, function(model_list){
  lapply(model_list, function(model){
    pred <- predict(model, newdata = dist_gam_grid, type = "response")
    cbind(dist_gam_grid, prediction = pred, variable = "Distance to urban")
  })
})

#d) Combine predictions into a data frame
gam_pred_df <- do.call(rbind, lapply(seq_along(dep_vars2), function(i){
  do.call(rbind, lapply(seq_along(rodent_split), function(j){
    frag_preds <- frag_gam_preds[[i]][[j]]
    dist_preds <- dist_gam_preds[[i]][[j]]
    rbind(
      data.frame(
        species = str_extract(dep_vars2[i], "^[a-zA-Z]+"),
        imp = j,
        Frag_vars = frag_preds$Frag_vars,
        Dist_urban = frag_preds$Dist_urban,
        prediction = frag_preds$prediction,
        variable = "Fragmentation" 
      ),
      data.frame(
        species = str_extract(dep_vars2[i], "^[a-zA-Z]+"),
        imp = j,
        Frag_vars = dist_preds$Frag_vars,
        Dist_urban = dist_preds$Dist_urban,
        prediction = dist_preds$prediction,
        variable = "Distance to urban"
      )
    )
  }))
}))

#e) Plot the interaction
ggplot(gam_pred_df, aes(x = ifelse(variable == "Fragmentation", Frag_vars, Dist_urban),
                        y = prediction, color = variable)) +
  geom_line() +
  labs(x = "Covariate value", y = "Predicted occupancy", color = "Variable") +
  theme_minimal() +
  facet_wrap(imp~species, scales = "free_y", nrow = 10, ncol = 4) +
  theme(strip.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"))
    #The interaction for Rmeg is cubic, all others are linear

#4) Refit the model with cubic interaction for Rmeg
frag_dist_gam_mod <- lapply(dep_vars2, function(response){
  model_list <- lapply(seq_along(rodent_split), function(i){
    data <- rodent_split[[i]]
    if(response == "rmeg_occupancy"){
      formula <- as.formula(paste(response, "~s(Frag_vars, Dist_urban) + poly(Frag_vars,3):poly(Dist_urban, 3)"))
    } else {
      formula <- as.formula(paste(response, "~s(Frag_vars, Dist_urban) + (Frag_vars:Dist_urban)"))
    }
    gam_model <- gam(formula, data = data)
  })
  names(model_list) <- paste0(response, "$Model_", seq_along(rodent_split))
  return(model_list)
})
names(frag_dist_gam_mod) <- dep_vars2
frag_dist_gam_mod$rrav_occupancy

#5) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs3 <- lapply(seq_along(frag_dist_gam_mod), function(i){
  model_list <- frag_dist_gam_mod[[i]]
  coef_list <- list()
  pred_list <- list()
  for(j in seq_along(model_list)){
    model <- model_list[[j]]
    coefs <- coef(model)
    
    #Calculate model coefficients and add to data frame
    se <- sqrt(diag(vcov(model)))
    t_values <- coefs / se
    p_values <- 2*(1-pt(abs(t_values), df = model$df.residual))
    coefs_df <- data.frame(
      term = names(coefs),
      estimate = coefs,
      std_error = se,
      t_value = t_values,
      p_value = p_values,
      stringsAsFactors = FALSE
    )
    
    #Filter only coefficients for interaction terms
    if (any(grepl("poly", coefs_df$term))) {
      coefs_filtered <- coefs_df[grepl("poly\\(Frag_vars", coefs_df$term), , drop = FALSE]
    } else {
      coefs_filtered <- coefs_df[grepl("Frag_vars:Dist_urban", coefs_df$term), , drop = FALSE]
    }
    
    #Add new rows to data frame and store in list
    coefs_filtered$imp <- j
    coefs_filtered$species <- names(frag_dist_gam_mod)[i]
    coefs_filtered$species <- str_extract(coefs_filtered$species, "^[a-zA-Z]+")
    row.names(coefs_filtered) <- NULL
    coef_list[[j]] <- coefs_filtered
    
    #Predict new values when keeping one covariate constant 
    preds <- predict(model, newdata = rodent_split[[i]], type = "response")
    pred_data <- cbind(rodent_imp_long[[i]], prediction = preds)
    pred_list[[j]] <- pred_data
  }
  combined_coefs <- do.call(rbind, coef_list)
  combined_preds <- do.call(rbind, pred_list)
  return(list(coefs = combined_coefs, preds = combined_preds))
})
preds_df3 <- bind_rows(lapply(preds_coefs3, `[[`, "preds"))
coefs_df3 <- bind_rows(lapply(preds_coefs3, `[[`, "coefs"))
coefs_df3
    #No interactions are significant even with transformation, exclude from models

#-------------------------------------------------------------------------------
#CHECK FOR COLLINEARITY

#a. Multicollinearity with variance inflation factor
#1) Subset independent variables
ind_vars <- rodent %>%
  select(c(Area, Dist_urban, Above_MHW, Mcal_conn, Mmus_conn, Rrav_conn, Rmeg_conn, Edge, Frag))

#2) Create a vector of dependent variables
dep_vars <- c("Rrav", "Rmeg", "Mmus", "Mcal")

#3) Create a list to store results
vif_results <- list()

#4) Assess multicollinearity
for(response in dep_vars){
  temp_df <- rodent %>%
    select(all_of(c(response, names(ind_vars))))
  model <- lm(as.formula(paste(response, "~.")), data = temp_df)
  vif_values <- vif(model)
  vif_results[[response]] <- vif_values
}
vif_results

#-----
#b. Examine pairwise comparisons between collinear variables (i.e., correlation)
#1) Convert character covariates to factor/numeric
rodent <- rodent %>%
  mutate(across(c(Type, Subregion), as.factor)) %>%
  mutate(across(c(Lat, Long), as.numeric))
str(rodent)

#2) Create correlation matrix 
cor_mat <- cor(rodent[,14:22], use = "pairwise.complete.obs")

#a) Format values >0.7 or <-0.7 in red
cor_mat <- as.data.frame(cor_mat)
highlight_cor_mat <- function(x) {
  ifelse(x > 0.7 | x < -0.7, 
         formatter("span", style = "color: red;")(round(x, 2)),
         round(x, 2))
}
form_cor_mat <- formattable(cor_mat, list(
  area(col = names(cor_mat)) ~ highlight_cor_mat
))
form_cor_mat
#Due to significant pairwise comparisons between connectivity measures, reduce collinearity
#by using Ridge regression (this keeps all connectivity measures)
#Alternatively, use principal component analysis (PCA) to reduce dimensionality
#Use stepwise regression (backward selection) to remove one habitat fragmentation metric

#-----
#c. Reduce collinearity between connectivity measures
#1) Ridge regression (WAS NOT ABLE TO DE-COUPLE CORRELATION BETWEEN VARIABLES)
#a) Create a detection matrix for the dependent variables
dep_vars1 <- as.matrix(rodent_imp[[1]][, c("Rrav", "Rmeg", "Mmus", "Mcal")])

#b) Create a vector for the connectivity measures
conn_vars <- c("Rrav_conn", "Rmeg_conn", "Mmus_conn", "Mcal_conn")

#Lambdas
lambdas <- 10^seq(2, -2, by = -.1)

#c) Create a list to store results
results <- list()

#d) Run Ridge regression and view predictions from optimal lambda
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  matrix <- complete_data %>%
    select(all_of(conn_vars)) %>%
    data.matrix()
  fit <- glmnet(dep_vars1, matrix, alpha = 0, family = "mgaussian", lambda = lambdas)
  cv_fit <-cv.glmnet(dep_vars1, matrix, alpha = 0, family = "mgaussian", lambda = lambdas)
  opt_lam <- cv_fit$lambda.min
  preds <- predict(fit, newx = matrix, s = opt_lam)
  results[[paste0("Imputed_", i)]] <- (predictions = preds)
}
results

#e) Add predictions back to imputed datasets
for(i in seq_along(rodent_imp)){
  preds <- results[[paste0("Imputed_", i)]]
  preds_df <- as.data.frame(preds)
  colnames(preds_df) <- conn_vars
  rodent_imp[[i]][conn_vars] <- preds_df
}
rodent_imp

#Optional: Add after colnames to add new column to dataframes
for(j in seq_len(ncol(preds_df))){
  new_col <- paste0("PC", "_", conn_vars)
  rodent_imp[[i]][new_col] <- preds_df[j]
}

#f) Re-assess correlation with predicted values
#i) Create a list to store correlation matrices
cor_mats_new <- list()

#ii) Create correlation matrices for each imputed dataset
for(i in seq_along(rodent_imp)){
  complete_data <- rodent_imp[[i]]
  cor_matrix <- cor(complete_data[, col_vars], use = "pairwise.complete.obs")
  cor_mats_new[[i]] <- cor_matrix
}
cor_mats_new


#2) Principal component analysis
#a) Run analysis and get eigen values
conn_pca <- prcomp(conn_data_mat, scale. = TRUE)
summary(conn_pca)

eig <- get_eigenvalue(conn_pca)
eig

#b) Select the first principal component (best fit) and add back to data frame
rodent$Connectivity <- conn_pca$x[, 1]

#3) Re-run correlation matrix
#a) With Ridge regression values
cor_mat_rr <- cor(rodent[,])

#b) With PCA values
cor_mat_pca <- cor(rodent[,])

#OR Select PCs that explain for >=90% variation in data
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


#--------------------
#FORMULATE THE DATA

#1. Create presence/absence matrices for each species 
#b. Define a function to extract presence/absence data
create_pa_matrix <- function(data, species_cols) {
  species_data <- data[, species_cols, drop = FALSE]
  return(as.matrix(species_data))
}

#c. Apply the function to the imputed data sets to create matrices
pa_matrices_list <- lapply(rodent_imp, function(df){
  lapply(species, function(species_name) {
    create_pa_matrix(df, species_name)
  })
})

#ORIGINAL
pa_matrices_list <- lapply(rodent_imp, function(df) create_pa_matrix(df, species))

#d. Combine matrices into a single matrix for all data sets
pa_matrices <- lapply(seq_along(species), function(species_idx){
  species_matrices <- lapply(pa_matrices_list, function(imputation_list){
    imputation_list[[species_idx]]
  })
  combined <- do.call(rbind, species_matrices)
  return(combined)
})

#c. Apply the function to the imputed data sets to create matrices
pa_matrices_list <- lapply(rodent_imp, function(df) {
  lapply(seq_along(species), function(i) {
    create_pa_matrix(df, species[i])
  })
})

#SKIP d. Combine matrices into a list of matrices for all imputed data sets
pa_matrices <- lapply(seq_along(species), function(species_idx) {
  do.call(rbind, lapply(pa_matrices_list, function(imputation_list) {
    imputation_list[[species_idx]]
  }))
})
pa_matrices

#SKIP d. Combine covariate data into a single data frame
sitecovs <- do.call(rbind, sitecovs_list)
sitecovs

umf_list <- lapply(rodent_imp, function(ylist) {
  create_umf(ylist, sitecovs)
})
umf_list


#a. Define a function to create matrices
create_matrices_sp <- function(data) {
  pa_matrices <- list(
    Rrav = as.matrix(data$Rrav),
    Rmeg = as.matrix(data$Rmeg),
    Mmus = as.matrix(data$Mmus),
    Mcal = as.matrix(data$Mcal)
  )
  combined_matrix <- do.call(cbind, pa_matrices)
  return(combined_matrix)
}

#b. Apply the function to create a list of matrices
ylist <- lapply(rodent_imp, create_matrices)
class(ylist[[1]])

#OR
create_matrices_sp <- function(data){
  pa_matrices <- list(
    Rrav = as.matrix(data$Rrav),
    Rmeg = as.matrix(data$Rmeg),
    Mmus = as.matrix(data$Mmus),
    Mcal = as.matrix(data$Mcal)
  )
  return(pa_matrices)
}
ylist <- lapply(rodent_imp, create_matrices_sp)

#3. Select one from the imputed datasets
ylist <- ylist[[1]]


#SIMPLER?

#SEPARATE SPECIES
#a. Define function
create_umf_sp <- function(ylist_sp, site_covs) {
  umf_list <- list()
  for (i in 1:length(ylist_sp)) {
    umf_list[[i]] <- unmarkedFrameOccu(
      y = ylist_sp[[i]], 
      siteCovs = site_covs[[i]]  
    )
  }
  return(umf_list)
}

#b. Apply function
umf_list_sp <- create_umf_sp(ylist_sp, sitecovs)
#-------------------------------------------------------------------------------
#ASSESS LINEARITY ASSUMPTIONS

#e. Investigate model 1 to identify possible assumption violations
#1) Extract the data set
int_1 <- rodent_imp_long[[1]]

#2) Visualize distribution of variables
ggplot(int_1, aes(x = Frag_vars)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Fragmentation for Imputation 1")          #Right-skewed, may be problematic
ggplot(int_1, aes(x = Conn_vars)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Connectivity for Imputation 1")

#3) Fit the linear model 
model_1 <- lm(Conn_vars ~ Frag_vars + Frag_vars:Conn_vars, data = int_1)
par(mfrow = c(2,2))
plot(model_1)

#4) Evaluate model assumptions
#a) Visualize model residuals and check normality
qqnorm(residuals(model_1))
qqline(residuals(model_1))
shapiro.test(residuals(model_1))    #Residuals are not normally distributed

#5) Log-transform fragmentation (right-skewed)
int_1$Frag_vars_log <- log(int_1$Frag_vars + 1)

#6) Re-fit the model with transformed variable
model_1_log <- lm(Conn_vars ~ Frag_vars_log + Frag_vars_log:Conn_vars, data = int_1)
par(mfrow = c(2,2))
plot(model_1_log)

#7) Re-evaluate model assumptions
#a) Visualize distribution of log-transformed fragmentation
ggplot(int_1, aes(x = Frag_vars_log)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Fragmentation for Imputation 1")

#i) Visualize model residuals and normality post-transformation
qqnorm(residuals(model_1_log))
qqline(residuals(model_1_log))
shapiro.test(residuals(model_1_log))    
#Residuals are normally distributed, transformation mitigates violations

#8) Re-evaluate non-additive effects of interaction
#a) Extract transformed predictions
log_preds <- int_1 %>%
  mutate(prediction = predict(model_1_log, newdata = ., type = "response"))

#b) Extract transformed coefficients
log_coefs <- tidy(model_1_log) %>%
  filter(term == "Conn_vars:Frag_vars_log") %>%
  mutate(imp = 1)

#c) Combine transformed predictions and coefficients into a list
log_preds_coefs <- list(log_coefs = log_coefs, log_preds = log_preds)

#d) Update the predicted values and coefficients data frames
log_preds_df <- preds_df %>%
  filter(imp != 1) %>% 
  bind_rows(log_preds)
log_coefs_df <- coefs_df %>%
  filter(imp != 1) %>%
  bind_rows(log_coefs)

#e) Pool the coefficients with Rubin's rule
pooled_int_results <- log_coefs_df %>%
  group_by(term) %>%
  summarize(
    pooled_estimate = mean(estimate),
    pooled_se = sqrt(sum(std.error^2)/n()),
    pooled_p = mean(p.value)
  )
pooled_int_results
#Most models have significant interaction terms, okay to include in occupancy models

#f) Visualize the interaction with updated predictions
ggplot(log_preds_df, aes(x = Frag_vars, y = prediction, color = as.factor(imp))) +
  geom_point(alpha = 0.5) + 
  geom_smooth(data = preds_df %>% filter(imp == 1),
              method = "lm", aes(group = imp), se = FALSE, formula = y ~ log(x+1)) +
  geom_smooth(data = preds_df %>% filter(imp != 1),
              method = "lm", aes(group = imp), se = FALSE) +
  labs(x = "Fragmentation", y = "Predicted Connectivity", color = "Imputation") + 
  theme_minimal()

#2. Test transformations and starting values before fitting models
#a. Distance to urban area
#1) Define a function to calculate and add minimum value to avoid log transformations on negative/zero values
calc_shift <- function(umf_list, covariate){
  min_value <- min(sapply(umf_list, function(umf){
    min(umf@siteCovs[[covariate]], na.rm = TRUE)
  }))
  dist_shift <- abs(min_value) + 0.01
  return(dist_shift)
}
dist_shift <- calc_shift(umf_list, "Dist_urban")
dist_shift

#2) Calculate standard deviation for starting values
calc_dist_sd <- function(umf, covariate){
  sd <- sapply(umf_list, function(umf){
    range <- max(umf@siteCovs[[covariate]]) - min(umf@siteCovs[[covariate]])
    sd <- range * 0.1
    return(sd)
  })
  return(mean(sd))
}
dist_sd <- calc_dist_sd(umf_list, "Dist_urban")
dist_sd

#3) Test transformations and starting values
log_models <- list()
for(i in seq_along(umf_list)){
  starts <- rnorm(12, mean = -0.5, sd = dist_sd)
  state_formulas <- as.formula(paste("~log(Dist_urban +", dist_shift, ")"))
  state_formulas <- rep(list(state_formulas), 4)
  model_list <- occuMulti(
    stateformulas = as.character(state_formulas),
    detformulas = detformulas,
    control = list(maxit = 10000),
    starts = starts,
    maxOrder = 1,
    data = umf_list[[i]]
  )
  log_models[[i]] <- model_list
}
log_models
  #Model works, okay to pass starting values to function in following step

#-------------------------------------------------------------------------------
#TRANSFORMATIONS

#1. Define a function to shift covariates and calculate starting values for each transformation type
calc_starts <- function(umf_list, covariate){
  
  #Calculate shifted covariate values (log(x), x>0; sqrt(x), x>=0)
  min_value <- min(sapply(umf_list, function(umf){                                 
    min(umf@siteCovs[[covariate]], na.rm = TRUE)
  }))
  shift <- abs(min_value) + 0.01
  
  #Extract linear values and calculate sd and mean for starting values
  linear_values <- sapply(umf_list, function(umf){                                 
    umf@siteCovs[[covariate]]
  })
  linear_mean <- mean(unlist(linear_values), na.rm = TRUE)
  linear_range <- max(unlist(linear_values), na.rm = TRUE) - min(unlist(linear_values), na.rm = TRUE)
  linear_sd <- linear_range * 0.1
  
  #Apply log transformation and calculate sd and mean for starting values
  log_values <- sapply(umf_list, function(umf){                                   
    shifted_cov <- umf@siteCovs[[covariate]] + shift
    log_cov <- log(shifted_cov)
    return(log_cov)
  })
  log_mean <- mean(unlist(log_values), na.rm = TRUE)
  log_range <- max(unlist(log_values), na.rm = TRUE) - min(unlist(log_values), na.rm = TRUE)
  log_sd <- log_range * 0.1
  
  #Apply square root transformation and calculate sd and mean for starting values
  quad_values <- sapply(umf_list, function(umf){                                  
    shifted_cov <- umf@siteCovs[[covariate]] + shift
    quad_cov <- I(shifted_cov)^2
    return(quad_cov)
  })
  quad_mean <- mean(unlist(quad_values), na.rm = TRUE)
  quad_range <- max(unlist(quad_values), na.rm = TRUE) - min(unlist(quad_values), na.rm = TRUE)
  quad_sd <- quad_range * 0.05
  
  #Return list of shifted values and starting value parameters
  return(list(                                                                    
    shift = shift,
    linear_mean = linear_mean,
    log_mean = log_mean,
    sqrt_mean = sqrt_mean,
    linear_sd = linear_sd,
    log_sd = log_sd,
    sqrt_sd = sqrt_sd
  ))
}

#a) Apply the function 
starts <- list()
for(i in seq_along(umf_list)){
  starting_values <- list()
  for(cov in covs){
    start_vals <- calc_starts(umf_list, cov)
    starting_values[[cov]] <- start_vals
  }
  starts[[i]] <- starting_values
}
starts

#3) Define another function to assess fit between linear, log, and square root terms          #NEED TO FIX TO CALCULATE DELTA AIC, WEIGHTS, AND CUMULATIVE WEIGHTS
assess_linearity <- function(umf_list, covs, starts, detformulas){
  model_list <- list()
  for(i in seq_along(umf_list)){
    models <- list()
    for(cov in covs){
      
      #Assign starting values and shifts from list created in Step 2b
      starting_values <- list(
        linear_mean = starts[[i]][[cov]]$linear_mean,
        linear_sd = starts[[i]][[cov]]$linear_sd,
        log_mean = starts[[i]][[cov]]$log_mean,
        log_sd = starts[[i]][[cov]]$log_sd,
        sqrt_mean = starts[[i]][[cov]]$sqrt_mean,
        sqrt_sd = starts[[i]][[cov]]$sqrt_sd
      )
      shifts <- starts[[i]][[cov]]$shift
      
      #Assign starting values based on transformation type
      start_values <- list(
        linear = rnorm(12, mean = starting_values$linear_mean, sd = starting_values$linear_sd),
        log = rnorm(12, mean = starting_values$log_mean, sd = starting_values$log_sd),
        sqrt = rnorm(12, mean = starting_values$sqrt_mean, sd = starting_values$sqrt_sd)
      )
      
      #Assign state formulas
      state_formulas <- list(
        linear = as.formula(paste("~", cov)),
        log = as.formula(paste("~log(", cov, "+", shifts, ")")),
        sqrt = as.formula(paste("~sqrt(", cov, "+", shifts, ")"))
      )
      
      #Set dynamic starting values and state formulas to pass to occupancy models
      for(type in names(state_formulas)){
        start_vals <- start_values[[type]]
        formula <- state_formulas[[type]]
        stateformulas <- rep(list(formula), 4)
        
        #Fit occupancy models
        model_name <- paste(type, cov, sep = "_")
        models[[model_name]] <- occuMulti(
          stateformulas = as.character(stateformulas),
          detformulas = detformulas,
          control = list(maxit = 50000),
          starts = start_vals,
          maxOrder = 1,
          data = umf_list[[i]]
        )}
    }
  }
  model_list[[i]] <- models
  
  #Assess best fit with AIC, grouping by covariate
  fit_list <- list()
  for(cov in covs){
    cov_models <- list()
    for(i in seq_along(model_list)){
      cov_models <- c(cov_models, model_list[[i]][grepl(cov, names(model_list[[i]]))])
    }
    
    #Use Rubin's rules to pool the AIC for each transformation across imputations
    pooled_aic <- lapply(names(start_values), function(type){
      flat_models <- unlist(cov_models, recursive = TRUE)
      model_type <- flat_models[grepl(type, names(cov_models))]
      aic_values <- sapply(model_type, function(mod) mod@AIC)
      delta_aic <- aic_values - min(aic_values)
      aic_weights <- exp(-0.5*delta_aic)/sum(exp(-0.5*delta_aic))
      cumltvWt <- cumsum(aic_weights)
      data.frame(
        AIC = aic_values,
        delta = delta_aic,
        AICwt = aic_weights,
        cumltvWt = cumltvWt)
    }
    )
    fit_list[[cov]] <- do.call(rbind, pooled_aic)
  }
  return(fit_list)
}

#a) Apply the function
mod_sel <- assess_linearity(umf_list, covs, starts, detformulas)
mod_sel



#3) Test if transformations improved the model with likelihood ratios (ANOVA)
anova(dist_linear, dist_quad, dist_cubic, dist_quint, dist_fifth, test = "Chisq")

#-----
#c. Inspect residual plots

#-------------------------------------------------------------------------------
#EVALUATE INTERACTIONS  

#2. Explore non-additive effects of interactions
#a. Fragmentation*connectivity
#1) Define the response variables 
dep_vars2 <- c("rrav_occupancy", "rmeg_occupancy", "mmus_occupancy", "mcal_occupancy")

#2) Split the data frame by imputation
rodent_split <- split(rodent_comb, rodent_comb$imputation)

#3) Fit the linear models to evaluate the interaction
int_models <- lapply(dep_vars2, function(response){
  lapply(rodent_split, function(data){
    lm(as.formula(paste(response, "~Frag_vars*Conn_vars")), data = data)
  })
})

names(int_models) <- dep_vars2  

#4) Extract fitted model predictions and coefficients and combine into data frames
preds_coefs <- map(seq_along(int_models), function(i){
  model <- int_models[[i]]
  map(seq_along(model), function(j){
    models <- model[[j]]
    coefs <- tidy(models) %>%
      filter(term == "Frag_vars:Conn_vars") %>%
      mutate(species = dep_vars2[i], imp = j) %>%
      mutate(species = str_extract(species, "^[a-zA-Z]+"))
    preds <- predict(models, newdata = rodent_split[[j]], type = "response")
    pred_data <- rodent_split[[j]] %>%
      mutate(prediction = preds) %>%
      mutate(species = dep_vars2[i], imp = j) %>%
      mutate(species = str_extract(species, "^[a-zA-Z]+"))
    return(list(coefs = coefs, preds = pred_data))
  })
})

preds_df <- bind_rows(lapply(preds_coefs, function(x) bind_rows(lapply(x, `[[`, "preds"))))
coefs_df <- bind_rows(lapply(preds_coefs, function(x) bind_rows(lapply(x, `[[`, "coefs"))))
coefs_df

#d. Visualize the interaction
#1) Fit a 
ggplot(preds_df, aes(x = Frag_vars, y = prediction, color = as.factor(imp), group = imp)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  labs(x = "Fragmentation", y = "Predicted Connectivity") + 
  facet_wrap(~imp, ncol = 5) + 
  theme_minimal()

#5) Visualize the interaction as it relates to occupancy, holding one covariate constant
#a) Subset first imputed dataset
preds_df1 <- preds_df[preds_df$imp == 1,]
summary(preds_df1$prediction)

#b) Fit the linear model to the first imputed dataset
frag_conn_models1 <- lapply(dep_vars2, function(response){
  lm(as.formula(paste(response, "~Frag_vars*Conn_vars")), data = rodent_split[[1]])
})
names(frag_conn_models1) <- dep_vars2
frag_conn_models1

#c) Generate a grid of values for each independent variable
frag_range <- seq(min(preds_df1$Frag_vars), max(preds_df1$Frag_vars), length.out = 100)
conn_range <- seq(min(preds_df1$Conn_vars), max(preds_df1$Conn_vars), length.out = 100)

#d) Fix each variable at its mean
frag_grid <- data.frame(
  Frag_vars = frag_range,
  Conn_vars = mean(preds_df1$Conn_vars)
)
conn_grid <- data.frame(
  Frag_vars = mean(preds_df1$Frag_vars),
  Conn_vars = conn_range
)

#e) Predict occupancy for each covariate and convert to data frame
#i) Fragmentation
frag_preds <- lapply(frag_conn_models1, function(model){
  predict(model, newdata = frag_grid, type = "response")
})
frag_preds_df <- data.frame(
  frag_grid,
  prediction = unlist(frag_preds)
)
frag_preds_df

#ii) Connectivity
conn_preds <- lapply(frag_conn_models1, function(model){
  predict(model, newdata = conn_grid, type = "response")
})
conn_preds_df <- data.frame(
  conn_grid,
  prediction = unlist(conn_preds)
)
conn_preds_df

#f) Add identifier columns in each data frame
frag_preds_df <- frag_preds_df %>%
  mutate(variable = "Fragmentation")
conn_preds_df <- conn_preds_df %>%
  mutate(variable = "Connectivity")

#g) Combine the grids and add a column for species name
plot_data <- rbind(frag_preds_df, conn_preds_df)
plot_data <- plot_data %>%
  mutate(species = rownames(plot_data)) %>%
  mutate(species = str_extract(species, "^[a-zA-Z]+"))
plot_data

#h) Plot the interaction
ggplot(plot_data, aes(x = ifelse(variable == "Fragmentation", Frag_vars, Conn_vars),
                      y = prediction, color = variable)) +
  geom_line() +
  labs(x = "Covariate value", y = "Predicted occupancy", color = "Variable") +
  theme_minimal() +
  facet_wrap(~species, scales = "free_y")
      #The interaction between connectivity and fragmentation likely exists, as
      #evidenced by the intersecting lines. It is stronger for Mcal and Mmus than
      #Rmeg and Rrav. Okay to include the interaction in occupancy models

#-------------------------------------------------------------------------------
#NULL MODEL

#1. Fit the null model (approach a)
mod_null_formula <- function(umf, stateformulas, detformulas){
  occuMulti(
    detformulas = detformulas,
    stateformulas = stateformulas,
    data = umf
  )
}

mod_null <- lapply(umf_list, function(umf){
  mod_null_formula(umf, stateformulas, detformulas)
})

mod_null[[1]]
summary(mod_null[[1]])

optimizePenalty(mod_null, penalties=c(0.02,0.1,0.2,0.33,1,2))       #Optimal penalty is 0.33
summary(mod_null)
plot(mod_null)

#-----
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
#If model is a good fit, proceed to pooling results. Otherwise, adjust model parameters  
#(e.g., method, starting values) or apply penalized likelihood and reassess

#-----
#Pooling null model results
#a. Define function to pool results
pool_results <- function(model_list) {
  estimates <- lapply(model_list, function(model) {
    coef(model)
  })
  estimates_df <- do.call(rbind, estimates)
  pooled_means <- colMeans(estimates_df, na.rm = TRUE)
  pooled_se <- apply(estimates_df, 2, sd, na.rm = TRUE)/sqrt(nrow(estimates_df))
  pooled_z <- pooled_means / pooled_se
  pooled_p <- 2 * (1 - pnorm(abs(pooled_z)))
  error <- 1.96*pooled_se
  lower <- pooled_means - error
  upper <- pooled_means + error
  return(data.frame(Estimate = pooled_means, 
                    SE = pooled_se,
                    z = pooled_z,
                    p_value = pooled_p,
                    Lower_CI = lower,
                    Upper_CI = upper))
}

#b. Apply the function to the penalized likelihood outputs - NEEDS WORK, HOW TO GET POOLED P-VALUES? REDO CI WITH BOOTSTRAP?
pooled_results <- pool_results(null_models_pen)
pooled_results <- round(pooled_results, digits = 5)
pooled_results

#----------------------------------------
#3. Select best-fitting model with AIC
#a. Manually calculate AIC for each model
aic_values <- sapply(null_models_pen, function(model){
  log_likelihood <- logLik(model)
  k <- length(coef(model))
  aic <- (-2 * log_likelihood)+(2*k)
  return(aic)
})

#b. Create a data frame of AIC results, from lowest to highest
names <- paste0("Model ", seq_along(aic_values))
aic_results <- data.frame(Model = names, AIC = aic_values)
aic_results <- aic_results[order(aic_results$AIC),] %>%
  mutate(Delta_AIC = AIC - min(AIC))
aic_results
#NOTE: Multiple models have same AIC. Try model averaging with models of delta AIC <=2

#4. Compute model-averaged parameter estimates
#a. Select the top fitting models
top_models <- aic_results[aic_results$Delta_AIC <=2,]

#b. Create a list that corresponds to the top fitting models
model_list <- null_models_pen[as.numeric(gsub("Model ", "", top_models$Model))]

#c. Prepare a named list for top fitting models
model_names <- top_models$Model
names(model_list) <- model_names

#d. Get parameter names
param_names <- names(coef(model_list[[1]]))

#e. Average models
mod_avg <- lapply(param_names, function(param){
  modavg(cand.set = model_list, parm = param, parm.type = "psi",
         conf.level = 0.95, uncond.se = "revised")
})

#------------------------------------------------------------------------------
#DETECTION MODEL

# First, check the detection probabilities for outliers
outlier_data <- det_preds_df %>%
  group_by(Year) %>%
  mutate(
    Q1 = quantile(Predicted, 0.25),
    Q3 = quantile(Predicted, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Predicted < lower_bound | Predicted > upper_bound)

#View the outlier data
print(outlier_data, n=Inf)

#Refit the model without outliers
det_preds_no_outliers <- det_preds_df %>%
  filter(!(Predicted %in% outlier_data$Predicted))

umf_list_no_outliers <- lapply(det_preds_no_outliers, function(species_data){
  umf <- unmarked::unmarkedFrameOccu(
    y = species_data[, "Detection"],
    siteCovs = species_data[, c("Effort", "Year")]
  )
  return(umf)
})

det_model_no_outliers <- fit_det_models(
  umf_list = det_preds_no_outliers,
  det_combos = det_combos,
  stateformulas = stateformulas
)

#3. Assess goodness-of-fit on model residuals (if necessary)
#a. Set up parallel computing - this needs to be reinitialized for each parallel computing segment
RNGkind("L'Ecuyer-CMRG")                                                   #Manage parallel RNG to get consistent but unique results
c1 <- makeCluster(detectCores()-1)                                         #Make cluster using all but 1 core processor
registerDoParallel(c1)

#b. Apply the fitstats function to the list of models (takes a while to run)
det_fit <- foreach(i = seq_along(det_model), .packages = c("unmarked")) %dopar% {
  model <- det_model[[i]]$model
  fit_results <- calc_fit(model, fitstats)
  return(fit_results)
}
det_fit
save(det_fit, file = "GOF_detmod.Rdata")
stopCluster(c1)

#c. Pool the results
det_fit_pooled <- pool_fitstats(det_fit)
det_fit_pooled
#The chi-square parameter suggests the model is moderately overdispersed
#(p = 0.00, c-hat = 1.23), try adjusting by specifying c-hat

#d. Summarize model output with and without c-hat adjustments (if necessary)
summaryOD(det_model)                          #Without adjustment
summaryOD(det_model, c.hat = 1.23)            #With adjustment

#4. Back-transform estimates to get detection probability
back_det <- coef(det_model[[1]]@estimates@estimates$det)
rrav_det <- back_det[grep("\\[Rrav\\]", names(back_det))]
boot::inv.logit(rrav_det[1])

mcal_det <- back_det[grep("\\[Mcal\\]", names(back_det))]
boot::inv.logit(mcal_det[1])

mmus_det <- back_det[grep("\\[Mmus\\]", names(back_det))]
boot::inv.logit(mmus_det[1])

rmeg_det <- back_det[grep("\\[Rmeg\\]", names(back_det))]
boot::inv.logit(rmeg_det[1])

#4. Back-transform estimates to get average detection probability across years
det_probs <- list()
for (i in seq_along(det_model)) {
  model <- det_model[[i]]
  species_list <- c("Rrav", "Rmeg", "Mmus", "Mcal")
  species_probs <- list()
  
  #Extract coefficients from the detection models
  coefs <- coef(model@estimates@estimates$det)
  names_coefs <- names(coefs)
  for (species in species_list) {
    
    #Find indices for the species-specific coefficients (e.g., connectivity)
    species_index <- which(grepl(species, names_coefs))
    
    #Compute back-transformed probabilities for a given year while fixing effort at its mean
    prob <- function(year_index) {
      full_vector <- rep(0, length(coefs))
      names(full_vector) <- names_coefs
      full_vector[species_index[1]] <- 1  
      full_vector[species_index[2]] <- 0 
      full_vector[species_index[3]] <- ifelse(year_index == 2022, 1, 0) 
      backTransform(linearComb(model, coefficients = full_vector, type = "det"))@estimate
    }
    
    # Calculate average detection probability
    avg_det_prob <- mean(sapply(c(2021, 2022), prob))
    species_probs[[species]] <- avg_det_prob
  }
  
  det_probs[[i]] <- species_probs
}
det_probs[[1]]
#Detection probability only increased for Mmus (from 0.37 to 0.43), otherwise it 
#decreased for all other species. Visualize trends and predictions to understand why.

#-----
#5. Plot the observed vs expected deviance 
#1) Extract the bootstrapped and observed Chi-square statistics
det_boot <- sapply(global_det_fit, function(fit) fit@t.star[,2])
det_boot <- unlist(as.vector(det_boot))
det_observed <- sapply(global_fit, function(fit) fit@t0[2])

#2) Calculate the p-value
det_p_value <- mean(det_boot >= det_observed)

#3) Plot the deviances
ggplot(data.frame(det_boot), aes(x = det_boot)) +
  geom_histogram(binwidth = 500, fill = "gray", color = "black") +
  geom_vline(xintercept = mean(det_observed), color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Bootstrapped Fit Statistic",
       subtitle = paste("P =", det_p_value),
       x = "Simulated statistic",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
  )

#-----
#Assume equal probabilities of detection for each sampling time
if(is.detect.constant == TRUE){
  pi <- predict(object, 'det')[1:nsites, 1]   # Detection probabilities, assuming equal across sites.
  prob0 <- pbinom(0, ni, pi)            # Probability of no detections when present, a site vector.
  
  #Get cdf for detection residuals - as a function of sum of detection events
  xi[xi == 0] <- NA                               # Ignore sites with no detections here.
  pdet <- (pbinom(xi, ni, pi)-prob0)/(1-prob0)   # CDF for number of detections xi, positive binomial.
  pdetMinus <- (pbinom(xi-1, ni, pi)-prob0)/(1-prob0) # Previous value of the cdf of xi.
}
return(pdetMinus)
}

residuals_list <- residuals.occModum(global_models[[1]], is.detect.constant = FALSE)    #TEST ON ONE GLOBAL MODEL


if(is.detect.constant == FALSE){
  prob0 <- apply(1-pi, 1, prod)    # Probability of no detections when present, a site vector.
  
  #Define a function to get the pdf under unequal detections.
  hetpdf <- function(xiSite, niSite, piSite){
    ind <- combn(niSite, xiSite)
    piMat <- matrix(piSite[ind], nrow = xiSite)
    return(sum(apply(piMat/(1-piMat), 2, prod))*prod(1-piSite));
  }
  hetcdf <- function(xiSite, niSite, piSite){
    if(xiSite == 0){cdf <- 0
    } else {
      detiSite <- rep(NA, xiSite)
      for(iX in 1:xiSite){
        detiSite[iX] <- hetpdf(iX, niSite, piSite)
      }
      cdf <- sum(detiSite)       
    }
    return(cdf)
  }
  
  #Get cdf for detection residuals - as a function of sum of detection events.
  isDetected <- xi > 0
  xi[isDetected == FALSE] <- NA  # Ignores sites with no detections.
  pdet = pdetMinus = rep(NA, nSites)
  for(iSite in which(isDetected)){
    xiSite <- xi[iSite]
    niSite <- ni[iSite]
    piSite <- pi[iSite,]
    pdetMinus[iSite] <- hetcdf(xiSite-1, niSite, piSite)
    pdet[iSite] <- pdetMinus[iSite] + hetpdf(xiSite, niSite, piSite)
  }
  pdet <- pdet/(1-prob0)   # 'CDF' for number of detections xi in heterogeneous case.
  pdetMinus <- pdetMinus/(1-prob0) # Previous value of the cdf of xi.
}

#Get cdf for occupancy residuals - as a function of binary detected/not.
probOcc <- psi*(1-prob0)                     # Probability of occupancy.
pOcc <- 1-probOcc+xiOcc*probOcc              # CDF for occupancy, Bernoulli variable with param probOcc.
pOccMinus <- xiOcc*(1-probOcc)               # Previous value of the cdf of occupancy.

#Jitter and get occupancy residuals
uOcc <- runif(nSites)                             # Standard uniform value to "jitter" the cdf.
residOcc <- qnorm(pOcc*uOcc+pOccMinus*(1-uOcc))   # Dunn-Smyth residual, standard normal if cdf correct.

#Jitter and get detection residuals.
u <- runif(nSites);                             # Standard uniform value to "jitter" the cdf.
residDet <- qnorm(pdet*u+pdetMinus*(1-u))      # Dunn-Smyth residual, standard normal if cdf correct.
residuals <- list(occ = residOcc, det = residDet)  
return(residuals) # Return output (i.e., a list with occupancy residuals (occ) and detection residuals (det)).
}

#A function that plots DS residuals against either fitted occupancy/detection probs.
residuals_list <- lapply(global_models, residuals.occModum, is.detect.constant = FALSE)
class(residuals_list)

occ_resids <- lapply(residuals_list, `[[`, "occ") # Stores residuals for occupancy.
det_resids <- res1$det[-which(is.na(res1$det))] # Stores residuals for detection (and remove NA values)

#Use residuals in diagnostic plots, e.g., obtain a normal quantile plot to check distributional assumptions
qqnorm(m1_res_occ,main = "", col = "blue", pch = 16, cex.axis = 1.4, ylab = "", xlab = "")
abline(0, 1, col = "black")
mtext(text = "sample quantiles", side = 2, line = 2.2, cex = 1.2)


DS.resid.plot <- function(x, y, ylim = c(-1,1)*max(abs(y)), alpha = 0.05, k = 5){
  plot(x, y, pch = 16, cex = 1.2, col = "blue", cex.axis = 1.4, ylim = ylim, cex.main = 0.9, ylab = "", xlab = "")
  lsmod <- gam(y~s(x, k = k))
  lsmod.p <- predict(lsmod, se.fit = TRUE)
  z.crit <- qnorm(1-alpha/2)
  upper <- lsmod.p$fit + (z.crit*lsmod.p$se.fit)
  lower <- lsmod.p$fit - (z.crit*lsmod.p$se.fit)
  polygon(c(rev(sort(x)), sort(x)), c(rev(upper[order(x)]), (lower[order(x)])), col = 'grey80', border = NA)
  points(x, y, pch = 16, cex = 1.2, col = "blue")
  lines(sort(x), fitted(lsmod)[order(x)], lwd = 2)
  abline(h = 0, col = "black", lwd = 1)
}

m1_x_occ<-predict(fm1, 'state')[,1] # Stores the fitted values for occupancy probabilities.

DS.resid.plot(m1_x_occ,m1_res_occ)
title(main="Occupancy Dunn???Smyth residuals",cex.main=2)
mtext(text="fitted occupancy values",side=1,las=1,line=2.6,cex=1.2)

m1_x_det<-apply(matrix(predict(fm1, 'det')[,1],ncol=nt),1,sum,na.rm=T)[is.na(res1$det)==FALSE]   #Storesthe sum of fitted values for detection probabilities.

DS.resid.plot(m1_x_det,m1_res_det)
title(main="Detection Dunn???Smyth residuals",cex.main=2)
mtext(text=Sigma[j]~"fitted detection values",side=1,las=1,line=2.7,cex=1.2)

#-------------------------------------------------------------------------------
#GLOBAL MODEL

fit_global_model_tidal <- function(umf_list_tidal, detformulas, starting_values){
  model_list <- list()
  state_formula <- c("~Area + Dist_urban + Above_MHW + Connectivity")
  state_formulas <- rep(list(state_formula), 4)
  for(i in seq_along(umf_list_tidal)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(state_formulas),
      detformulas = rep(c("~ Effort + Year"), 4),           
      control = list(maxit=5000),
      method = "Nelder-Mead",               #Do not calculate Hessian matrix
      maxOrder = 1,                         #Do not include species interactions
      starts = starting_values,
      data = umf_list_tidal[[i]]
    )
  }
  return(model_list)
}

#b. Apply the function to the umf list
global_model <- fit_global_model(umf_list)
save(global_model, file = "Global_model.Rdata")
summary(global_model[[1]])
    #Model runs but may be poor fit (high SEs), check goodness-of-fit and run penalized likelihood if necessary


#b. Fit the model with parallel computing
RNGkind("L'Ecuyer-CMRG")                                                   
c1 <- makeCluster(detectCores()-1)  
registerDoParallel(c1)

global_model_tidal <- foreach(i = seq_along(umf_list_tidal),
                              .packages = c("unmarked")) %dopar% {
                                state_formula <- "~Area + Dist_urban + Above_MHW + Connectivity"
                                state_formulas <- rep(list(state_formula), 4)
                                occuMulti(
                                  stateformulas = as.character(state_formulas),
                                  detformulas = rep(c("~ Effort + Year"), 4),           
                                  control = list(maxit=50000),
                                  method = "Nelder-Mead",               #Do not calculate Hessian matrix
                                  maxOrder = 1,                         #Do not include species interactions
                                  starts = starting_values,
                                  data = umf_list_tidal[[i]]
                                )
                              }
stopCluster(c1)

#c. Save the results of the global model
save(global_model_tidal, file = "Global_model_tidal.Rdata")

#-----
#####WITH LOG AREA####
log_covs <- colnames(rodent_imp[[1]])
log_covs <- log_covs[c(7:9, 10:14)]

extract_log_covs_tidal <- function(data, scale_covs){
  tidal_data <- data[data$Type == "Tidal", log_covs, drop=FALSE]
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

log_sitecovs_tidal <- lapply(rodent_imp, extract_log_covs_tidal)

create_umf_tidal_log <- function(ylist_tidal, log_sitecovs_tidal) {
  umf_list_tidal <- list()
  for (i in 1:length(log_sitecovs_tidal)) {
    umf_list_tidal[[i]] <- unmarkedFrameOccuMulti(
      y = ylist_tidal, 
      siteCovs = as.data.frame(log_sitecovs_tidal[[i]])
    )
  }
  return(umf_list_tidal)
}

umf_list_tidal_log <- create_umf_tidal_log(ylist_tidal, log_sitecovs_tidal)
summary(umf_list_tidal_log[[1]])


model_covs_log <- c("Log_Area", "Dist_urban", "Above_MHW", "Connectivity", "Connectivity:Log_Area")

fit_global_models_log <- function(umf_list_tidal_log){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs_log, collapse = "+"))
  for(i in seq_along(umf_list_tidal_log)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(rep(state_formula, 4)),
      detformulas = as.character(rep("~Effort + Year", 4)),
      control = list(maxit = 5000),
      maxOrder = 1,
      starts = rnorm(36, mean = 0, sd = 0.1),
      data = umf_list_tidal_log[[i]]
    )
  }
  return(model_list)
}

global_models_log <- fit_global_models_log(umf_list_tidal_log)

global_models_log_flat <- unlist(global_models_log, recursive = FALSE)

cl <- makeCluster(detectCores() - 4)  
clusterExport(cl, c("global_models_log_flat", "fitstats"))
clusterEvalQ(cl, library(unmarked))

global_log_fit <- parLapply(cl, global_models_log_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

save(global_log_fit, file = "GOF_global_log_mod.Rdata")
load("GOF_global_log_mod.Rdata")

global_log_fit_pooled <- pool_fitstats(global_log_fit)

######NO INTERACTION######
model_covs_log_noint <- c("Log_Area", "Dist_urban", "Above_MHW", "Connectivity")

fit_global_models_log_noint <- function(umf_list_tidal_log){
  model_list <- list()
  state_formula <- paste("~", paste(model_covs_log_noint, collapse = "+"))
  for(i in seq_along(umf_list_tidal_log)){
    model_list[[i]] <- occuMulti(
      stateformulas = as.character(rep(state_formula, 4)),
      detformulas = as.character(rep("~Effort + Year", 4)),
      control = list(maxit = 5000),
      maxOrder = 1,
      starts = rnorm(36, mean = 0, sd = 0.1),
      data = umf_list_tidal_log[[i]]
    )
  }
  return(model_list)
}

global_models_log_noint <- fit_global_models_log_noint(umf_list_tidal_log)

global_models_log_noint_flat <- unlist(global_models_log_noint, recursive = FALSE)

cl <- makeCluster(detectCores() - 4)  
clusterExport(cl, c("global_models_log_noint_flat", "fitstats"))
clusterEvalQ(cl, library(unmarked))

global_log_noint_fit <- parLapply(cl, global_models_log_noint_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

save(global_log_noint_fit, file = "GOF_global_log_noint_mod.Rdata")
load("GOF_global_log_noint_mod.Rdata")

global_log_noint_fit_pooled <- pool_fitstats(global_log_noint_fit)

#-----
#c. Assess goodness-of-fit on model residuals
#1) Initiate parallel computing 
RNGkind("L'Ecuyer-CMRG") 
c1 <- makeCluster(detectCores()-1)  
registerDoParallel(c1)

#2) Fit the function to the global models
global_fit_tidal <- foreach(i = seq_along(global_model_tidal), .packages = c("unmarked")) %dopar% {
  calc_fit(global_model_tidal[[i]], fitstats)
}

stopCluster(c1)

#4) Save the results of the goodness-of-fit test
save(global_fit_tidal, file = "Global_model_tidal_GOF.Rdata")

#5) Visualize observed vs simulated deviance
#a) Extract the bootstrapped and observed Chi-square statistics
boot <- global_fit[[1]]@t.star[,2]
observed <- global_fit[[1]]@t0[2]

#b) Calculate the p-value
p_value <- mean(boot >= observed)

#c) Plot the deviances
ggplot(data.frame(boot), aes(x = boot)) +
  geom_histogram(binwidth = 500, fill = "gray", color = "black") +
  geom_vline(xintercept = observed, color = "red", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Bootstrapped Fit Statistic",
       subtitle = paste("P =", p_value),
       x = "Simulated statistic",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")
  )

#-----
#Penalized likelihood
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

#Assess goodness-of-fit on model residuals
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

#OR
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
#1) Re-initiate parallel computing
cl <- makeCluster(detectCores() - 4)  
clusterExport(cl, c("global_model_pen", "fitstats"))
clusterEvalQ(cl, library(unmarked))

#2) Assess GOF with fitstats and parboot
global_pen_fit <- parLapply(cl, global_model_pen, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#3) Save the results of the goodness-of-fit test 
save(global_pen_fit, file = "GOF_global_pen_mod.Rdata")
load("GOF_global_pen_mod.Rdata")

#4) Pool the results
global_pen_fit_pooled <- pool_fitstats(global_pen_fit)
#Model is still not a good fit, chi-square p-value < 0.05

#g. Visualize model fit
#1) Extract observed and predicted values from the penalized models
observed <- getY(global_pen_models[[1]]@data)
expected <- fitted(global_pen_models[[1]])

#2) 
n.obs <- apply(observed, 1, sum, na.rm = TRUE)
n.pred <- rowSums(do.call(cbind,expected), na.rm = TRUE)

#3) Plot observed vs predicted values
plot(n.obs, n.pred, frame = F)
abline(0,1)
points(smooth.spline(n.pred~n.obs, df = 4), type = "l", lwd = 2, col = "blue")

#--------------------------------------
fit_global_model <- function(model_covs, umf_list_tidal){
  
  #Initiate parallel computing and lists to store results
  c1 <- makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  results <- foreach(cov = model_covs, .combine = 'list', .packages = c("unmarked"), 
                     .export = c("fitstats", "pool_results")) %dopar% {
                       state_formula <- paste("~", paste(model_covs, collapse = "+"))
                       starting_values <- rnorm(36, mean = 0, sd = 0.1)
                       model_results <- list()
                       p_values <- list()
                       fit_statistics <- list()
                       pen_results <- list()
                       
                       #Fit global models
                       for(i in seq_along(umf_list_tidal)){
                         model_results[[i]] <- occuMulti(
                           stateformulas = rep(state_formula, 4),
                           detformulas = rep("~Effort + Year", 4),
                           control = list(maxit = 5000),
                           maxOrder = 1,
                           starts = starting_values,
                           data = umf_list_tidal
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
                           pen_model <- optimizePenalty(
                             model_results[[i]],
                             penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
                           )
                           
                           #Re-assess goodness-of-fit
                           pen_fit <- parboot(pen_model, fitstats, nsim = 100)                     
                           
                           #Extract penalized model fit statistics
                           fit_statistics <- list(
                             SSE = pen_fit@t0["SSE"],
                             Chisq = pen_fit@t0["Chisq"],
                             FreemanTukey = pen_fit@t0["freemanTukey"]
                           )
                           pen_results[[i]] <- pen_model
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
                       list(
                         model = model_results,
                         p = p_values,
                         pooled_fit = pooled_fit_stats,
                         pooled_model_results = pooled_model_results,
                         pooled_pen_results = pooled_pen_results
                       )
                     }
  stopCluster(c1)
  return(results)
}                                 


#b. Define a wrapper function to fit models using objects and functions above
fit_global_model_tidal <- function(umf_list_tidal, model_covs) {
  fit_global_model(umf_list_tidal, model_covs)
}

#c. Apply the wrapper function to the list of global models
global_model_tidal <- fit_global_model_tidal(model_covs, umf_list_tidal)
save(global_model_tidal, file = "Tidal_global_model.Rdata")

#---------
#######################
#Test single model
fit_global_model <- function(model_covs, umf_tidal_single) {
  
  # Initiate parallel computing
  c1 <- makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  
  # Prepare to fit the model
  state_formula <- paste("~", paste(model_covs, collapse = "+"))
  results <- list()
  
  # Fit the global model
  model_result <- occuMulti(
    stateformulas = rep(state_formula, 4),
    detformulas = rep("~Effort + Year", 4),
    control = list(maxit = 1000),
    maxOrder = 1,
    starts = rnorm(36, mean = 0, sd = 0.1),
    data = umf_tidal_single
  )
  
  # Assess goodness-of-fit
  fit_list <- parboot(model_result, fitstats, nsim = 10)
  
  # Extract p-values and fit statistics
  p_values <- list(
    SSE_p = mean(fit_list@t.star[, 1] >= fit_list@t0[1]),
    Chisq_p = mean(fit_list@t.star[, 2] >= fit_list@t0[2]),
    FreemanTukey_p = mean(fit_list@t.star[, 3] >= fit_list@t0[3])
  )
  
  fit_statistics <- list(
    SSE = fit_list@t0["SSE"],
    Chisq = fit_list@t0["Chisq"],
    FreemanTukey = fit_list@t0["freemanTukey"]
  )
  
  # Check for penalized likelihood requirement
  if (p_values$SSE_p < 0.05 || p_values$Chisq_p < 0.05 || p_values$FreemanTukey_p < 0.05) {
    pen_model <- optimizePenalty(
      model_result,
      penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
    )
    
    # Reassess goodness-of-fit
    pen_fit <- parboot(pen_model, fitstats, nsim = 10)
    
    # Extract penalized model fit statistics
    fit_statistics <- list(
      SSE = pen_fit@t0["SSE"],
      Chisq = pen_fit@t0["Chisq"],
      FreemanTukey = pen_fit@t0["freemanTukey"]
    )
    results$pen_model <- pen_model
  }
  
  # Final results
  results$model = model_result
  results$p = p_values
  results$fit_statistics = fit_statistics
  
  # Stop the cluster
  stopCluster(c1)
  return(results)
}

# b. Define a wrapper function to fit models using the adjusted function
fit_global_model_tidal <- function(umf_tidal_single, model_covs) {
  fit_global_model(model_covs, umf_tidal_single)
}

# c. Apply the wrapper function to a single dataset from umf_list_tidal
model_covs <- c("Area", "Dist_urban", "Above_MHW", "Connectivity", "Connectivity*Area")
umf_tidal_single <- umf_list_tidal[[1]]  

global_model_tidal <- fit_global_model_tidal(umf_tidal_single, model_covs)

# View results
summary(global_model_tidal$model)
#Possible overfitting as indicated by large Freeman-Tukey statistic
#AIC value is significantly lower than that of null model (null = 11945.39, global with no penalization = 9168.55)

#-----
#k-folds cross-validation to test for over-fitting
set.seed(123)  
folds <- createFolds(1:nrow(umf_tidal_single@y), k = 5, list = TRUE)  

# Function to fit the model and calculate AIC for each fold
cv_aic_global <- function(train_umf, test_umf) {
  # Fit your global model
  global_model <- occuMulti(
    stateformulas = rep("~Area + Dist_urban + Above_MHW + Connectivity + Connectivity*Area", 4),
    detformulas = rep("~Effort + Year", 4),
    control = list(maxit = 1000),
    maxOrder = 1,
    starts = rnorm(36, mean = 0, sd = 0.1),
    data = train_umf
  )
  
  # Calculate AIC for the model
  aic_value <- global_model@AIC
  return(aic_value)
}

# Initialize a vector to store AIC values for each fold
aic_values <- numeric(length(folds))

# Perform k-fold cross-validation
for (i in 1:length(folds)) {
  # Split the data into training and testing sets for this fold
  test_indices <- folds[[i]]
  
  # Create training and testing unmarked frames (umf) for this fold
  train_umf <- umf_tidal_single[-test_indices, ] 
  test_umf <- umf_tidal_single[test_indices, ]   
  
  # Calculate AIC for this fold
  aic_values[i] <- cv_aic_global(train_umf, test_umf)
}

# Print the AIC values for each fold
print(aic_values)

# Calculate the average AIC across folds
mean_aic <- mean(aic_values)
sd_aic <- sd(aic_values)
#Model is not overfitting because AIC is relatively consistent across folds

#b. Define a function to fit models and calculate GOF statistics
fit_global_model <- function(model_covs, umf_list_tidal) {
  c1 <- makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  results <- foreach(i = seq_along(umf_list_tidal), .packages = c("unmarked"), 
                     .export = c("fitstats", "pool_results", "pool_fitstats")) %dopar% {
                       
                       #Define formulas and starting values
                       state_formula <- paste("~", paste(model_covs, collapse = "+"))
                       
                       #Fit the model
                       model_results <- occuMulti(
                         stateformulas = as.character(rep(state_formula, 4)),
                         detformulas = as.character(rep("~Effort + Year", 4)),
                         control = list(maxit = 5000),
                         maxOrder = 1,
                         starts = rnorm(36, mean = 0, sd = 0.1),
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

#-------------------------------------------------------------------------------
#UNIVARIATE MODELS

#a. Define a function to fit models, calculate GOF statistics, and run penalized likelihood (if necessary)
fit_uni_model <- function(cov, umf_list_tidal){
  
  #Define formulas
  state_formula <- paste("~", cov)
  
  #Fit univariate models for each independent variable
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
  fit_statistics <- lapply(model_results, function(model) parboot(model, fitstats, nsim = 100))
  pooled_fit_stats <- lapply(pool_fitstats, fit_statistics)
  
  #Run penalized likelihood if p is <0.05
  penalties <- c(0.01, 0.1, 0.2, 0.33, 1, 2)
  pen_models <- mapply(function(model, pooled) {
    if(pooled@SSE_p < 0.05 || pooled@Chisq_p < 0.05 || pooled@freeTuke_p < 0.05) {
      optimizePenalty(model, penalties = penalties)
    } else {
      NULL
    }
  }, model_results, pooled_fit_stats, SIMPLIFY = FALSE)
  
  #Re-assess goodness-of-fit
  pen_fit_statistics <- lapply(pen_model_results, function(model){
    if (!is.null(model)) parboot(model, fitstats, nsim = 100) else NULL
  })
  pooled_pen_stats <- lapply(pen_fit_statistics, function(pen_fit){
    if (!is.null(pen_fit)) pool_fitstats(pen_fit) else NULL
  })
  
  #Pool model results, weighting penalized models based on penalty
  pooled_model_results <- if(length(pen_models) > 0){
    penalty <- penalties[which(!sapply(pen_model_results, is.null))]
    weights <- penalty / sum(penalty)
    pool_results(pen_model_results, weights)
  } else {
    pool_results(model_results)
  }
  
  #Store final results
  results <- list(
    model = model_results,
    pooled_fit_stats = pooled_fit_stats,
    pen_model = pen_model_results,
    pooled_pen_fit_stats = pooled_pen_fit_stats,
    pooled_model_results = pooled_model_results
  )
  return(results)
}                                 

#b. Fit the models with parallel computing
#1) Initiate parallel computing
cl <- makeCluster(detectCores() - 4)  
clusterExport(cl, c("fit_uni_model", "fitstats", "model_covs", "umf_list_tidal", "pool_fitstats", "pool_results"))
clusterEvalQ(cl, library(unmarked))

#2) Fit the models with the predefined function
uni_models <- parLapply(cl, fit_uni_models, function(umf_list_tidal, model_covs){
  fit_uni_model(umf_list_tidal, model_covs)
})
stopCluster(cl)



#b. Define a wrapper function to fit models using objects and functions above
fit_uni_models <- function(umf_list_tidal, model_covs) {
  fit_uni_model(umf_list_tidal, model_covs)
}

#BROKEN DOWN
#Fit model
test_uni_model <- lapply(seq_along(umf_list_tidal), function(i){
  occuMulti(
    stateformulas = as.character(rep("~Area", 4)),
    detformulas = as.character(rep("~Effort + Year", 4)),
    control = list(maxit = 5000),
    maxOrder = 1,
    starts = rnorm(20, mean = 0, sd = 0.1),
    data = umf_list_tidal[[i]]
  )
})
test_uni_model[[1]]             #GOOD

#Pool fit statistics
test_pooled_fit_stats <- lapply(pool_fitstats, test_fit_statistics)

#Penalized models
test_pen_models <- mapply(function(model, pooled) {
  if(pooled@SSE_p < 0.05 || pooled@Chisq_p < 0.05 || pooled@freeTuke_p < 0.05) {
    optimizePenalty(model, penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2))
  } else {
    NULL
  }
}, test_uni_model, pooled_fit_stats, SIMPLIFY = FALSE)


#-----
#b. Assess goodness-of-fit on model residuals
#1) Flatten the list of models
uni_models_flat <- unlist(uni_models, recursive = FALSE)

#2) Initiate parallel computing
cl <- makeCluster(detectCores() - 1)  
clusterExport(cl, c("uni_models_flat", "fitstats"))
clusterEvalQ(cl, library(unmarked))

#2) Apply parboot to each model
uni_fit <- parLapply(cl, uni_models_flat, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#3) Save the results of the goodness-of-fit test 
save(uni_fit, file = "GOF_uni_mods.Rdata")
load("GOF_uni_mods.Rdata")

#4) Pool the results
#a) Group the results by independent variable
cov_name <- unique(sub("\\d+$", "", names(uni_fit)))
par_group <- setNames(lapply(cov_name, function(cov){
  uni_fit[grep(paste0("^", cov), names(uni_fit))]
}), cov_name)

#b) Pool the results for each variable
uni_fit_pooled <- lapply(par_group, function(fit){
  pool_fitstats(fit)
})
uni_fit_pooled$Connectivity
#Models are poor fit and overdispersed, as evidenced by large Chi-square values,
#p < 0.05, and c-hat ~1.4-1.5. 

#-----
#d. Apply penalized likelihood if GOF p < 0.05
#1) Define a function to penalize the global models
penalize_uni <- function(uni_models, uni_fit_pooled){
  uni_model_results <- list()
  if(uni_fit_pooled$SSE_p < 0.05 || 
     uni_fit_pooled$Chisq_p < 0.05 || 
     uni_fit_pooled$FreemanTukey_p < 0.05){
    for(i in seq_along(uni_models)){
      pen_model_results[[i]] <- optimizePenalty(
        uni_models[[i]],
        penalties = c(0.01, 0.1, 0.2, 0.33, 1, 2)
      )
    }
  } else {
    pen_model_results <- NULL
  }
  return(pen_model_results)
}

#2) Apply the function
uni_model_pen <- penalize_uni(uni_models, uni_fit_pooled)
save(uni_model_pen, file = "Uni_model_pen.Rdata")
load("Uni_model_pen.Rdata")
uni_model_pen[[1]]

#-----
#e. Re-assess goodness-of-fit
#1) Re-initiate parallel computing
cl <- makeCluster(detectCores() - 4)  
clusterExport(cl, c("uni_model_pen", "fitstats"))
clusterEvalQ(cl, library(unmarked))

#2) Assess GOF with fitstats and parboot
uni_pen_fit <- parLapply(cl, uni_model_pen, function(model){
  parboot(model, fitstats, nsim = 100)
})
stopCluster(cl)

#3) Save the results of the goodness-of-fit test 
save(uni_pen_fit, file = "GOF_uni_pen_mod.Rdata")
load("GOF_uni_pen_mod.Rdata")

#4) Pool the results
uni_pen_fit_pooled <- pool_fitstats(uni_pen_fit)

#--------------------------------------
#a. Distance to urban area
dist_urban <- c("~Dist_urban", "~Dist_urban","~Dist_urban","~Dist_urban",
                "~1","~1","~1","~1","~1","~1","0","0","0","0","0")
urban_mod <- occuMulti(stateformulas=dist_urban, detformulas=detformulas, umf)
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

#PLOT UNSTANDARDIZED VALUES?
#b. Visualize effects of vegetation richness on marginal occupancy
install.packages("colorspace")
library(colorspace)

#1) Generate sequence of values for X-axis of plot  
veg_range <- range(sitecovs$Veg_rich)
veg_seq <- seq(veg_range[1], veg_range[2], length.out=100)

#2) Predict marginal occupancy for each species along sequence
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

#c. Plot marginal occupancy as a function of vegetation richness
pal_veg <- choose_palette(pal=heat_hcl)
color_veg <- pal_veg(4)

plot(occ_veg_rrav$Veg_rich, occ_veg_rrav$Predicted, ylim=c(0, 1), type="l",
     col=color_veg[1], lwd = 2, xlab="Vegetation richness", ylab = "Marginal occupancy")
lines(occ_veg_rmeg$Veg_rich, occ_veg_rmeg$Predicted, col=color_veg[2], lwd=2)
lines(occ_veg_mmus$Veg_rich, occ_veg_mmus$Predicted, col=color_veg[3], lwd=2)
lines(occ_veg_mcal$Veg_rich, occ_veg_mcal$Predicted, col=color_veg[4], lwd=2)
legend('topleft', col=c(color_veg[1],color_veg[2], color_veg[3], color_veg[4]),lty=1,lwd=2,
       legend=c("Rrav", "Rmeg", "Mmus", "Mcal"))


#CREATE UNIVARIATE MODELS

#1. Review data
colnames(umf_fDesign[[1]])
head(siteCovs(umf_list[[1]]))

#2. Create models with each covariate
#a. Distance to urban area
#1) Assign state formulas
dist_urban <- c("~Dist_urban", "~Dist_urban","~Dist_urban","~Dist_urban",
                "~1","~1","~1","~1","~1","~1","0","0","0","0","0")

#2) Fit the model to the imputed datasets
fit_urban_models <- function(umf_list, dist_urban, detformulas){
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = dist_urban, 
      detformulas = detformulas,
      data = umf_list[[i]]
    )
  }
  return(model_list)
}

urban_models <- fit_urban_models(umf_list, dist_urban, detformulas)             
summary(urban_models[[2]])
#NOTE: Model is a poor fit (possible separation), try using penalized likelihood to control model 
#complexity and reduce variance of parameter estimates (Murphy 2012, Clipp et al. 2021)

#3) Fit the model with penalized likelihood (if needed)
#a) Define a function with various penalty values
set.seed(500)
fit_urban_models_pen <- function(urban_models){
  model_list <- list()
  for(i in seq_along(urban_models)){
    model_list[[i]] <- unmarked::optimizePenalty(
      urban_models[[i]], 
      penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
    )
  }
  return(model_list)
}

#b) Apply the function to the list of models
urban_models_pen <- fit_urban_models_pen(urban_models)
urban_models_pen

#4) Pool results with Rubin's rules for variance estimation
#a) Extract components of model outputs
urban_coefs <- sapply(urban_models_pen, coef)
urban_se <- sapply(urban_models_pen, function(model) sqrt(diag(vcov(model))))
urban_z <- urban_coefs/urban_se
urban_p <- 2*(1-pnorm(abs(urban_z)))
urban_aic <- sapply(urban_models_pen, function(model) model@AIC)

#b) Calculate model weights based on AIC values
urban_weights <- (1/urban_aic)
urban_weights <- urban_weights/sum(urban_weights)

#c) Pool results
urban_pooled_coefs <- rowSums(urban_coefs*urban_weights)
urban_pooled_se <- sqrt(rowSums(urban_weights*(urban_se^2 + (urban_coefs - urban_pooled_coefs)^2)))
urban_pooled_z <- urban_pooled_coefs/urban_pooled_se
urban_pooled_p <- 2*(1-pnorm(abs(urban_pooled_z)))

#d) Merge pooled results into one data frame
urban_results <- data.frame(
  Estimate = urban_pooled_coefs,
  SE = urban_pooled_se,
  Z = urban_pooled_z,
  p_value = urban_pooled_p
)
urban_results <- round(urban_results, digits = 5)
urban_results
#No effect of distance to urban area on occupancy of any species

#-----
#b. Inundation (using subsetted data)
#1) Assign state formulas
above_mhw <- c("~Above_MHW", "~Above_MHW","~Above_MHW","~Above_MHW",
               "~1","~1","~1","~1","~1","~1","0","0","0","0","0")

#2) Subset tidal sites from the imputed datasets
#a) Define a function to subset the datasets
sub_umf <- function(umf_list){
  umf_sub <- list()
  for(i in seq_along(umf_list)){
    y <- umf_list[[i]]@y
    sub_siteCovs <- umf_list[[i]]@siteCovs
    sub_siteCovs <- sub_siteCovs[sub_siteCovs$Type == "Tidal",]
    tidal <- rownames(sub_siteCovs)
    ylist_sub <- ylist[rownames(ylist) %in% tidal, , drop=FALSE]
    umf_sub[[i]] <- unmarkedFrameOccuMulti(y = ylist_sub,
                                           siteCovs = sub_siteCovs)
  }
  return(umf_sub)
}
ylist
sitecovs[1]$Type=="Tidal"

#b) Apply the function
sub_umf_list <- sub_umf(umf_list)

#3) 

#3) Fit the model to the subsetted imputed datasets
fit_mhw_models <- function(sub_umf_list, above_mhw, detformulas){
  model_list <- list()
  for(i in seq_along(sub_umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = above_mhw, 
      detformulas = detformulas,
      data = sub_umf_list[[i]]
    )
  }
  return(model_list)
}

mhw_models <- fit_mhw_models(sub_umf_list, above_mhw, detformulas)             
summary(mhw_models[[1]])
#NOTE: Model is a poor fit (possible separation), try using penalized likelihood to control model 
#complexity and reduce variance of parameter estimates (Murphy 2012, Clipp et al. 2021)

#4) Fit the model with penalized likelihood (if needed)
#a) Define a function with various penalty values
set.seed(500)
fit_urban_models_pen <- function(urban_models){
  model_list <- list()
  for(i in seq_along(urban_models)){
    model_list[[i]] <- unmarked::optimizePenalty(
      urban_models[[i]], 
      penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
    )
  }
  return(model_list)
}

#b) Apply the function to the list of models
urban_models_pen <- fit_urban_models_pen(urban_models)
urban_models_pen

#4) Pool results with Rubin's rules for variance estimation
#a) Extract components of model outputs
urban_coefs <- sapply(urban_models_pen, coef)
urban_se <- sapply(urban_models_pen, function(model) sqrt(diag(vcov(model))))
urban_z <- urban_coefs/urban_se
urban_p <- 2*(1-pnorm(abs(urban_z)))
urban_aic <- sapply(urban_models_pen, function(model) model@AIC)

#b) Calculate model weights based on AIC values
urban_weights <- (1/urban_aic)
urban_weights <- urban_weights/sum(urban_weights)

#c) Pool results
urban_pooled_coefs <- rowSums(urban_coefs*urban_weights)
urban_pooled_se <- sqrt(rowSums(urban_weights*(urban_se^2 + (urban_coefs - urban_pooled_coefs)^2)))
urban_pooled_z <- urban_pooled_coefs/urban_pooled_se
urban_pooled_p <- 2*(1-pnorm(abs(urban_pooled_z)))

#d) Merge pooled results into one data frame
urban_results <- data.frame(
  Estimate = urban_pooled_coefs,
  SE = urban_pooled_se,
  Z = urban_pooled_z,
  p_value = urban_pooled_p
)
urban_results <- round(urban_results, digits = 5)
urban_results
#No effect of distance to urban area on occupancy of any species


#-----
#c. Connectivity (PC1)
#1) Assign state formulas
conn_vars_pc1 <- c("~Conn_vars_PC1", "~Conn_vars_PC1","~Conn_vars_PC1","~Conn_vars_PC1",
                   "~1","~1","~1","~1","~1","~1","0","0","0","0","0")

#2) Fit the model to the imputed datasets
fit_conn_pc1_models <- function(umf_list, conn_vars_pc1, detformulas){
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = conn_vars_pc1, 
      detformulas = detformulas,
      data = umf_list[[i]]
    )
  }
  return(model_list)
}

conn_vars_pc1_models <- fit_urban_models(umf_list, conn_vars_pc1, detformulas)             
summary(conn_vars_pc1_models[[1]])
#NOTE: Model is a poor fit (possible separation), try using penalized likelihood to control model 
#complexity and reduce variance of parameter estimates (Murphy 2012, Clipp et al. 2021)

#3) Fit the model with penalized likelihood (if needed)
#a) Define a function with various penalty values
set.seed(500)
fit_urban_models_pen <- function(urban_models){
  model_list <- list()
  for(i in seq_along(urban_models)){
    model_list[[i]] <- unmarked::optimizePenalty(
      urban_models[[i]], 
      penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
    )
  }
  return(model_list)
}

#b) Apply the function to the list of models
urban_models_pen <- fit_urban_models_pen(urban_models)
urban_models_pen

#4) Pool results with Rubin's rules for variance estimation
#a) Extract components of model outputs
urban_coefs <- sapply(urban_models_pen, coef)
urban_se <- sapply(urban_models_pen, function(model) sqrt(diag(vcov(model))))
urban_z <- urban_coefs/urban_se
urban_p <- 2*(1-pnorm(abs(urban_z)))
urban_aic <- sapply(urban_models_pen, function(model) model@AIC)

#b) Calculate model weights based on AIC values
urban_weights <- (1/urban_aic)
urban_weights <- urban_weights/sum(urban_weights)

#c) Pool results
urban_pooled_coefs <- rowSums(urban_coefs*urban_weights)
urban_pooled_se <- sqrt(rowSums(urban_weights*(urban_se^2 + (urban_coefs - urban_pooled_coefs)^2)))
urban_pooled_z <- urban_pooled_coefs/urban_pooled_se
urban_pooled_p <- 2*(1-pnorm(abs(urban_pooled_z)))

#d) Merge pooled results into one data frame
urban_results <- data.frame(
  Estimate = urban_pooled_coefs,
  SE = urban_pooled_se,
  Z = urban_pooled_z,
  p_value = urban_pooled_p
)
urban_results <- round(urban_results, digits = 5)
urban_results

#OR
starting_values <- rnorm(14, mean = 0, sd = 0.25)

fit_null_models <- function(umf_list, stateformulas, detformulas){
  set.seed(500)
  model_list <- list()
  for(i in seq_along(umf_list)){
    model_list[[i]] <- occuMulti(
      stateformulas = stateformulas, 
      detformulas = detformulas,
      control = list(maxit=50000),
      method = "Nelder-Mead",
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
      penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
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

#-----
#d. Connectivity (PC2)

#-----
#e. Fragmentation (PC1)

#-----
#f. Fragmentation (PC2)

#--------------------------------
#CREATE MULTIVARIATE MODELS
occuMulti(detformulas=detformulas, stateformulas=, data=umf, 
          penalty=fit_opt, starts, method=, 
          se=TRUE, engine=c("R"))

#Stops running after first model, check for errors/warnings
test_fit <- tryCatch({
  result <- unmarked::optimizePenalty(
    global_model[[1]], 
    penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
  )
  return(result)
},
warning = function(w){
  cat("Warning in model: ", conditionMessage(w), "\n")
  return(NULL)
},
error = function(e){
  cat("Error in model: ", conditionMessage(e), "\n")
  return(NULL)
})

#OR with progress bar
fit_global_model_pen <- function(global_model){
  model_list <- pblapply(seq_along(global_model), function(i){
    unmarked::optimizePenalty(
      global_model[[i]], 
      penalties = c(0.02, 0.1, 0.2, 0.33,1,2)
    )
  })
  return(model_list)
}

#3. Dredge the global model to evaluate all possible combinations of covariates         #dredge() does not work with occuMulti :(
dredge_results <- list()
for(i in seq_along(global_model_pen)){
  dredge_results[[i]] <- dredge(global_model_pen[[i]], rank=AICc, m.max=5)
}
summary(dredge_results[[1]])

#b. Combine the dredged models
global_dredge <- do.call(rbind, dredge_results)

#-----
#4. AIC to select best fitting model for interpretation
#a. Calculate average AICs of imputed models
null_models_avg_aic <- mean(sapply(null_models, function(model) model@AIC))
global_model_pen_avg_aic <- mean(sapply(global_model_pen, function(model) model@AIC))
global_model_sub_pen_avg_aic <- mean(sapply(global_model_sub_pen, function(model) model@AIC))

#b. Combine all AIC values into a vector
avg_aic <- c(null_models_avg_aic, global_model_pen_avg_aic, global_model_sub_pen_avg_aic)

#c. Create a data frame and adjust names
avg_aic_df <- data.frame(AIC = avg_aic)
rownames(avg_aic_df) <- c("Null models", "Global model", "Global model sub")
avg_aic_df

#5. Average models with delta AIC <=2
global_model_avg <- list()
for(i in seq_along(dredge_results)){
  global_model_avg[[i]] <- model.avg(dredge_results[[i]], subset = delta <=2)
}

summary(global_model_avg)


