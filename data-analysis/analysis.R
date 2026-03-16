# =============================================================================
# SPATIAL HEDONIC PRICING MODEL - TRENTINO AIRBNB
# Theory-Driven Approach Following Elhorst (2010) Strategy
# =============================================================================

# SETUP & LIBRARIES
rm(list = ls())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("..")
setwd("C:/Users/borto/Desktop/unitn/geospatial/geospatial-project")

# Now load data
df <- read.csv("datasets/trentino_listings_maps.csv")

ensure_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

ensure_package("spdep")
ensure_package("spatialreg")
ensure_package("dplyr")

print("--- LIBRARIES LOADED SUCCESSFULLY ---")

# =============================================================================
# DATA CLEANING (collapse shared rooms)
# =============================================================================

cat("\n--- DATA CLEANING: Handling Rare Categories ---\n")
table_original <- table(df$room_type)
cat("Original room_type:\n"); print(table_original)

df$room_type_clean <- ifelse(df$room_type == "Shared room", 
                               "Private room", as.character(df$room_type))

table_cleaned <- table(df$room_type_clean)
cat("Cleaned (Shared→Private):\n"); print(table_cleaned); cat("\n")

# =============================================================================
# DATA LOADING & PREPARATION
# =============================================================================

periods <- unique(df$period)
results <- list()

# Convert categorical variables
df$room_type_clean <- as.factor(df$room_type_clean)
df$host_is_superhost <- as.factor(df$host_is_superhost)

# Rescale distances from meters to kilometers
# (Section 4.4: Interpretation as "% price change per 1 km")
vars_dist <- grep("dist_", names(df), value = TRUE)
for(v in vars_dist) {
  df[[v]] <- df[[v]] / 1000
}

# =============================================================================
# THEORY-BASED MODEL SPECIFICATION (Section 4.1-4.4)
# =============================================================================
for (quadrimester in periods) {
  print(paste("ANALYZING PERIOD:", quadrimester, "\n"))

  print("--- STEP 1: THEORETICALLY-DRIVEN VARIABLE SELECTION ---")
  print("Theoretical Framework: Hedonic Pricing + Spatial Dependence")
  print("References: Rosen (1974), Tobler, Elhorst (2010), Torres-Luque (2025)")

  # Core Model based on Literature Review (Section 2.1-2.3)
  model_ols <- lm(log_price ~ 
                  # Structural (Section 4.1)
                  room_type_clean + accommodates + bathrooms + 
                  
                  # Reputational (Section 4.2)
                  n_reviews + review_scores_rating + host_is_superhost +
                  
                  # Locational (Section 4.4) - Refined to 4 key distances
                  # Alpine: Ski-Lift Premium (Guest Favorites)
                  dist_ski + 
                  
                  # Lacustrine: Shoreline Premium (Garda studies)
                  dist_lake + 
                  
                  # Urban: Monocentricity (Alonso)
                  dist_center + 
                  
                  # Services: Agglomeration Economies (Section 2.1)
                  dist_restaurant +
                  
                  # Policy variables
                  availability_365 + minimum_nights,
                  
                  data = df)

  print("--- OLS BASELINE MODEL SUMMARY ---")
  summary(model_ols)

  # Residual diagnostics
  par(mfrow=c(2,2))
  plot(model_ols, main="OLS Residual Diagnostics")
  par(mfrow=c(1,1))

  # =============================================================================
  # SPATIAL WEIGHTS MATRIX (Section 3.2)
  # =============================================================================

  print("--- STEP 2: CREATING SPATIAL WEIGHTS MATRIX (KNN) ---")
  print("Justification: Adaptive to variable density (Section 3.2)")

  k <- 15  # Section 3.2: Justified range 10-20, optimal at k=15
  coords <- cbind(df$long, df$lat)

  # Check for duplicate coordinates
  coords_df <- as.data.frame(coords)
  n_duplicates <- sum(duplicated(coords_df))
  pct_duplicates <- round(n_duplicates/nrow(df)*100, 2)

  print(paste("Duplicate coordinates found:", n_duplicates, "(", pct_duplicates, "%)"))

  # L'analisi ha rilevato che 454 osservazioni (5.93%) presentavano coordinate duplicate rispetto a osservazioni precedenti nel dataset, per un totale di 697 listings (9.1%) coinvolte in gruppi di duplicazione. 
  # Questo pattern è attribuibile alla pratica di Airbnb di applicare offset sistematici per privacy e alla presenza di edifici multi-unità.

  # JITTERING: Add small random noise if duplicates > 5%
  if (pct_duplicates > 5) {
    print("--- APPLYING JITTERING (duplicate threshold exceeded) ---")
    set.seed(42)  # Reproducibility
    
    # Add ~15m random noise (appropriate for privacy-shifted Airbnb data)
    df$long_jitter <- df$long + rnorm(nrow(df), 0, 0.00015)
    df$lat_jitter <- df$lat + rnorm(nrow(df), 0, 0.00015)
    
    coords_final <- cbind(df$long_jitter, df$lat_jitter)
    
    # Verify uniqueness
    n_unique_after <- nrow(unique(as.data.frame(coords_final)))
    print(paste("Unique coordinates after jittering:", n_unique_after, "/", nrow(df)))
    
  } else {
    coords_final <- coords
    print("No jittering needed (duplicates < 5%)")
  }

  # Create spatial weights with cleaned coordinates
  knn <- knearneigh(coords_final, k = k)
  nb <- knn2nb(knn)
  listw <- nb2listw(nb, style = "W")  # Row-standardized

  print(paste("Spatial Weights Matrix created: k =", k, "(Section 3.2)"))

  # =============================================================================
  # SPATIAL DIAGNOSTICS (Section 3.1)
  # =============================================================================

  print("--- STEP 3: SPATIAL AUTOCORRELATION TESTS ---")

  # Moran's I on Dependent Variable
  print("--- Moran's I Test on log_price (Global Autocorrelation) ---")
  moran_price <- moran.test(df$log_price, listw)
  print(moran_price)

  # Moran Scatterplot
  moran.plot(df$log_price, listw, labels=FALSE, 
            xlab="Log Price", 
            ylab="Spatially Lagged Log Price",
            main="Moran Scatterplot - Spatial Clustering")

  # Moran's I on OLS Residuals (Section 3.1)
  print("--- Moran's I Test on OLS Residuals ---")
  print("H0: No spatial autocorrelation (independent residuals)")
  print("H1: Spatial autocorrelation exists (OLS is biased)")
  moran_test <- lm.morantest(model_ols, listw)
  print(moran_test)

  if (moran_test$p.value < 0.05) {
    print("RESULT: Spatial autocorrelation detected (p < 0.05)")
    print("IMPLICATION: OLS estimates are inefficient/biased")
    print("ACTION: Proceed with Spatial Regression Models (Section 3.3)")
  } else {
    print("RESULT: No significant spatial autocorrelation")
    print("NOTE: This is rare in real estate data")
  }

  # Lagrange Multiplier Tests (Elhorst Strategy - Section 3.3)
  print("--- Lagrange Multiplier Tests (Model Selection) ---")
  lm_tests <- lm.LMtests(model_ols, listw, test = "all")
  print(lm_tests)

  # Interpretation Guide (FIXED for new syntax)
  p_lag <- lm_tests$RSlag$p.value
  p_err <- lm_tests$RSerr$p.value

  print("--- MODEL SELECTION ADVICE (Elhorst 2010) ---")
  print(paste("LM-Lag p-value:", p_lag))
  print(paste("LM-Error p-value:", p_err))

  if (p_lag < 0.05 & p_err < 0.05) {
    print("Both LM-Lag and LM-Error significant -> Estimate SDM (most general)")
    model_choice <- "SDM"
  } else if (p_lag < 0.05) {
    print("Only LM-Lag significant -> Estimate SAR (price spillover)")
    model_choice <- "SAR"
  } else if (p_err < 0.05) {
    print("Only LM-Error significant -> Estimate SEM (error spillover)")
    model_choice <- "SEM"
  } else {
    print("Neither significant -> OLS sufficient (unlikely)")
    model_choice <- "OLS"
  }

  # =============================================================================
  # SPATIAL MODEL ESTIMATION (Section 3.3 - Elhorst Strategy)
  # =============================================================================

  print("--- STEP 4: ESTIMATING SPATIAL REGRESSION MODELS ---")
  formula_final <- formula(model_ols)

  # SAR Model
  print("Estimating SAR (Spatial Lag Model)...")

  model_sar <- lagsarlm(formula_final, data = df, listw = listw, 
                        method = "LU", quiet = FALSE)

  print(summary(model_sar, Nagelkerke = TRUE))


  # # SDM Model 
  # if (model_choice == "SDM") {
  #   print("Estimating SDM (Spatial Durbin Model)...")
  #   model_sdm <- lagsarlm(formula_final, data = df, listw = listw, 
  #                         Durbin = TRUE, 
  #                         method = "LU") 
  #   print("--- SDM MODEL RESULTS ---")
  #   print(summary(model_sdm, Nagelkerke = TRUE))
  # }

  # SDM Model con Durbin selettivo (prima dava nan e  Fare il lag spaziale di una variabile categorica non ha senso teoric)
  # Verifica correlazione tra variabili di distanza
  # dist_vars <- c("dist_center", "dist_ski", "dist_lake", "dist_castle", 
  #                "dist_museum", "dist_restaurant", "dist_supermarket")
  # cor_matrix <- cor(df[, dist_vars])
  # print(round(cor_matrix, 2))
  # Rimuovi quelle con r > 0.7


  if (model_choice == "SDM") {
    print("Estimating SDM with Selective Durbin...")
    print("Variables with WX (spatial lag): accommodates, bathrooms, n_reviews, availability_365")
    print("Variables WITHOUT WX: room_type_clean, host_is_superhost, all distances")
    print("Rationale: Only intrinsic property characteristics have spillover effects")
    
    # Selective Durbin: only structural/reputation continuous variables
    durbin_formula <- ~ accommodates + bathrooms + n_reviews + availability_365
    
    model_sdm <- lagsarlm(formula_final, data = df, listw = listw, 
                          Durbin = durbin_formula,
                          method = "LU") 
    
    print(summary(model_sdm, Nagelkerke = TRUE))
  }



  # SEM Model
  print("Estimating SEM (Spatial Error Model)...")

  model_sem <- errorsarlm(formula_final, data = df, listw = listw, method = "LU")  # Rimuovi method="spam"

  print(summary(model_sem, Nagelkerke = TRUE))

  # =============================================================================
  # IMPACTS CALCULATION (Section 5 - Spillover Effects)
  # =============================================================================

  # Dopo aver stimato tutti i modelli (SAR, SDM, SEM)
  # Confronta AIC e calcola impacts solo per il migliore

  print("--- MODEL COMPARISON (AIC) ---")
  aic_ols <- AIC(model_ols)
  aic_sar <- AIC(model_sar)
  aic_sem <- AIC(model_sem)
  aic_sdm <- AIC(model_sdm) 

  print(paste("AIC values - OLS:", aic_ols, "SAR:", aic_sar, "SEM:", aic_sem, "SDM:", aic_sdm))
  best_model_name= which.min(c(aic_ols, aic_sar, aic_sem, aic_sdm))
  best_model_name <- c("OLS", "SAR", "SEM", "SDM")[best_model_name]
  print(paste("Best Model (Lowest AIC):", best_model_name))

  # Calcola impacts SOLO per il migliore
  # if (best_model_name == "SAR") {
  #   print("--- CALCULATING IMPACTS (SAR) ---")
  #   imp <- impacts(model_sar, listw = listw, R = 100)
  #   print(summary(imp, zstats = TRUE, short = TRUE))
  # } else if (best_model_name == "SDM") {
  #   print("--- CALCULATING IMPACTS (SDM) ---")
  #   imp <- impacts(model_sdm, listw = listw, R = 100)
  #   print(summary(imp, zstats = TRUE, short = TRUE))
  # }
  # SEM non ha impacts (no spatial lag)

  # Calculate impacts for SAR and SDM for comparison
  print("--- IMPACTS CALCULATION (Direct, Indirect, Total Effects) ---")
  print("Note: Trace calculation handled internally (may take 10-20 seconds per model)")

  print("\nCalculating Impacts for SAR...")
  imp_sar <- impacts(model_sar, R = 100)
  print(summary(imp_sar, zstats = TRUE, short = TRUE))

  print("\nCalculating Impacts for SDM...")
  imp_sdm <- impacts(model_sdm, R = 100)
  print(summary(imp_sdm, zstats = TRUE, short = TRUE))
  
  # Store results for period
  results[[quadrimester]] <- list(
    n_obs = nrow(df),
    mean_price = mean(df$price),
    median_price = median(df$price),
    ols = model_ols,
    sar = model_sar,
    sdm = model_sdm,
    sem = model_sem,
    aic = c(OLS = aic_ols, SAR = aic_sar, SEM = aic_sem, SDM = aic_sdm),
    best_model = best_model_name,
    impacts_sar = imp_sar,
    impacts_sdm = imp_sdm,
    lm_tests = lm_tests,
    moran_test = moran_test,
    moran_price = moran_price
  )
  
  cat("\n✓ Period", toupper(quadrimester), "completed\n")
}

# =============================================================================
# CROSS-PERIOD COMPARISON TABLES
# =============================================================================

cat("\n\n========================================\n")
cat("CROSS-PERIOD COMPARISON\n")
cat("========================================\n\n")

cat("Approach: 'Two Snapshots' - Separate models for September vs December\n")
cat("Key Hypothesis (H3): Ski proximity premium stronger in December\n\n")

# Table 1: Sample Size & Descriptive Statistics
cat("--- TABLE 1: Sample Size & Price Statistics by Period ---\n")
comparison_descriptive <- data.frame(
  Period = names(results),
  N_Listings = sapply(results, function(x) x$n_obs),
  Mean_Price = sapply(results, function(x) round(x$mean_price, 2)),
  Median_Price = sapply(results, function(x) round(x$median_price, 2)),
  Moran_I = sapply(results, function(x) round(x$moran_price$estimate["Moran I statistic"], 3)),
  Moran_pval = sapply(results, function(x) round(x$moran_price$p.value, 4))
)
print(comparison_descriptive)
cat("\n")

# Table 2: Model Selection (AIC Comparison)
cat("--- TABLE 2: Model Selection (AIC) by Period ---\n")
comparison_aic <- data.frame(
  Period = names(results),
  Best_Model = sapply(results, function(x) x$best_model),
  AIC_OLS = sapply(results, function(x) round(x$aic["OLS"], 1)),
  AIC_SAR = sapply(results, function(x) round(x$aic["SAR"], 1)),
  AIC_SEM = sapply(results, function(x) round(x$aic["SEM"], 1)),
  AIC_SDM = sapply(results, function(x) round(x$aic["SDM"], 1))
)
print(comparison_aic)
cat("\n")

# Table 3: dist_ski Effects Across Periods (SEASONALITY TEST)
cat("--- TABLE 3: dist_ski Effects by Period (Seasonality Test H3) ---\n")
cat("Expected: Negative coefficient (closer to ski = higher price), larger in December\n\n")

comparison_ski <- data.frame(
  Period = names(results),
  SAR_Direct = sapply(results, function(x) {
    imp <- summary(x$impacts_sar, zstats = TRUE)
    round(imp$mat["dist_ski", "Direct"], 4)
  }),
  SAR_pval = sapply(results, function(x) {
    imp <- summary(x$impacts_sar, zstats = TRUE)
    round(imp$pzmat["dist_ski", "Direct"], 4)
  }),
  SDM_Direct = sapply(results, function(x) {
    imp <- summary(x$impacts_sdm, zstats = TRUE)
    round(imp$mat["dist_ski", "Direct"], 4)
  }),
  SDM_pval = sapply(results, function(x) {
    imp <- summary(x$impacts_sdm, zstats = TRUE)
    round(imp$pzmat["dist_ski", "Direct"], 4)
  })
)
print(comparison_ski)
cat("\n")

# =============================================================================
# SAVE RESULTS TO ../results FOLDER
# =============================================================================

# Create results directory if it doesn't exist
if (!dir.exists("../results")) {
  dir.create("../results", recursive = TRUE)
  cat("✓ Created ../results/ directory\n")
}

# Save complete R object
save(results, file = "../results/multi_period_analysis.RData")
cat("✓ Results saved to: ../results/multi_period_analysis.RData\n\n")

# Export comparison tables to CSV
write.csv(comparison_descriptive, "../results/table1_descriptive.csv", row.names=FALSE)
write.csv(comparison_aic, "../results/table2_aic.csv", row.names=FALSE)
write.csv(comparison_ski, "../results/table3_seasonality.csv", row.names=FALSE)

cat("✓ Comparison tables exported to CSV:\n")
cat("  - ../results/table1_descriptive.csv\n")
cat("  - ../results/table2_aic.csv\n")
cat("  - ../results/table3_seasonality.csv\n\n")

# Export individual period results as separate CSV files
for (period_name in names(results)) {
  
  # Extract coefficients from best model (SDM)
  period_data <- results[[period_name]]
  sdm_summary <- summary(period_data$sdm)
  
  # Coefficients table
  coef_table <- as.data.frame(sdm_summary$Coef)
  coef_table$Variable <- rownames(coef_table)
  coef_table <- coef_table[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
  
  # Impacts table
  imp_summary <- summary(period_data$impacts_sdm, zstats = TRUE)
  impacts_table <- data.frame(
    Variable = rownames(imp_summary$mat),
    Direct = imp_summary$mat[, "Direct"],
    Indirect = imp_summary$mat[, "Indirect"],
    Total = imp_summary$mat[, "Total"],
    Direct_pval = imp_summary$pzmat[, "Direct"],
    Indirect_pval = imp_summary$pzmat[, "Indirect"],
    Total_pval = imp_summary$pzmat[, "Total"]
  )
  
  # Save to CSV
  write.csv(coef_table, 
            paste0("../results/", period_name, "_sdm_coefficients.csv"), 
            row.names = FALSE)
  write.csv(impacts_table, 
            paste0("../results/", period_name, "_sdm_impacts.csv"), 
            row.names = FALSE)
  
  cat(paste0("✓ Exported ", period_name, " results:\n"))
  cat(paste0("  - ../results/", period_name, "_sdm_coefficients.csv\n"))
  cat(paste0("  - ../results/", period_name, "_sdm_impacts.csv\n"))
}

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat("All results saved to ../results/ folder for cross-period comparison\n")