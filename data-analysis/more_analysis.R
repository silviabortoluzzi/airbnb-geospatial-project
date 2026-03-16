# =============================================================================
# SPATIAL HEDONIC PRICING MODEL - TRENTINO AIRBNB
# Theory-Driven Approach Following Elhorst (2010) Strategy
# =============================================================================

# SETUP & LIBRARIES
rm(list = ls())

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
df$is_hotel   <- ifelse(df$room_type_clean == "Hotel room", 1, 0)
df$is_private <- ifelse(df$room_type_clean == "Private room", 1, 0)
df$superhost  <- ifelse(df$host_is_superhost == "t" | df$host_is_superhost == TRUE, 1, 0)

# Convert categorical variables
df$room_type_clean <- as.factor(df$room_type_clean)
df$host_is_superhost <- as.factor(df$host_is_superhost)

# Rescale distances from meters to kilometers
vars_dist <- grep("dist_", names(df), value = TRUE)
for(v in vars_dist) {
  df[[v]] <- df[[v]] / 1000
}

# =============================================================================
# THEORY-BASED MODEL SPECIFICATION (Section 4.1-4.4)
# =============================================================================
for (quadrimester in periods) {
  cat("\n\n=========================================\n")
  cat("ANALYZING PERIOD:", toupper(quadrimester), "\n")
  cat("=========================================\n\n")

  # Clear previous models
  if(exists("model_sdm")) rm(model_sdm)
  if(exists("model_sar")) rm(model_sar)
  if(exists("model_sem")) rm(model_sem)

  # FILTER DATASET BY PERIOD (CRITICAL FIX)
  df_period <- df[df$period == quadrimester, ]
  df_period <- na.omit(df_period)
  cat(paste("✓ Data filtered. Observations in", quadrimester, ":", nrow(df_period), "\n\n"))

  print("--- STEP 1: THEORETICALLY-DRIVEN VARIABLE SELECTION ---")
  print("Theoretical Framework: Hedonic Pricing + Spatial Dependence")
  print("References: Rosen (1974), Tobler, Elhorst (2010), Torres-Luque (2025)")

  # Core Model based on Literature Review (Section 2.1-2.3)
  model_ols <- lm(log_price ~ 
                # USA QUESTE (Numeriche):
                is_hotel + is_private +     
                superhost +               
                accommodates + bathrooms + n_reviews + review_scores_rating +
                dist_ski + dist_lake + dist_center + dist_restaurant +
                availability_365 + minimum_nights,
                data = df_period)

  print("--- OLS BASELINE MODEL SUMMARY ---")
  summary(model_ols)

  # Residual diagnostics
  par(mfrow=c(2,2))
  plot(model_ols, main=paste("OLS Diagnostics -", quadrimester))
  par(mfrow=c(1,1))

  # =============================================================================
  # SPATIAL WEIGHTS MATRIX (Section 3.2)
  # =============================================================================

  cat("\n--- Creating Spatial Weights Matrix (k-NN) ---\n")
  
  # Prepare coordinates with jittering if duplicates exist
  coords <- cbind(df_period$long, df_period$lat)
  n_duplicates <- sum(duplicated(as.data.frame(coords)))
  
  if (n_duplicates > 0) { 
    set.seed(42)
    coords <- cbind(
      df_period$long + rnorm(nrow(df_period), 0, 0.00015),
      df_period$lat + rnorm(nrow(df_period), 0, 0.00015)
    )
  }
  
  # Adaptive k-NN: increase k until graph is connected
  k <- 15
  nb <- NULL
  max_k <- 150  # Increased limit for problematic periods
  
  for (k_try in 15:max_k) {
    nb_temp <- knn2nb(knearneigh(coords, k = k_try))
    nb_temp <- make.sym.nb(nb_temp)
    n_components <- n.comp.nb(nb_temp)$nc
    
    if (n_components == 1) {
      k <- k_try
      nb <- nb_temp
      cat(sprintf("  ✓ Connected graph found at k=%d\n", k))
      break
    } else if (k_try %% 20 == 0) {
      cat(sprintf("  k=%d: %d components (continuing...)\n", k_try, n_components))
    }
    
    if (k_try == max_k) {
      k <- k_try
      nb <- nb_temp
      cat(sprintf("  ⚠ Graph has %d components at k=%d (will use robust listw)\n", n_components, k))
    }
  }
  
  listw <- nb2listw(nb, style = "W")
  is_connected <- (n.comp.nb(nb)$nc == 1)
  cat(sprintf("Spatial weights: k=%d, avg neighbors=%.1f, connected=%s\n", 
              k, mean(card(nb)), is_connected))

  # =============================================================================
  # SPATIAL DIAGNOSTICS (Section 3.1)
  # =============================================================================

  print("--- STEP 3: SPATIAL AUTOCORRELATION TESTS ---")

  # Moran's I on Dependent Variable
  print("--- Moran's I Test on log_price (Global Autocorrelation) ---")
  moran_price <- moran.test(df_period$log_price, listw)
  print(moran_price)

  # Moran Scatterplot
  moran.plot(df_period$log_price, listw, labels=FALSE, 
            xlab="Log Price", 
            ylab="Spatially Lagged Log Price",
            main=paste("Moran Scatterplot -", quadrimester))

  # Moran's I on OLS Residuals
  print("--- Moran's I Test on OLS Residuals ---")
  print("H0: No spatial autocorrelation (independent residuals)")
  print("H1: Spatial autocorrelation exists (OLS is biased)")
  moran_test <- lm.morantest(model_ols, listw)
  print(moran_test)

  if (moran_test$p.value < 0.05) {
    print("RESULT: Spatial autocorrelation detected (p < 0.05)")
    print("IMPLICATION: OLS estimates are inefficient/biased")
    print("ACTION: Proceed with Spatial Regression Models")
  } else {
    print("RESULT: No significant spatial autocorrelation")
  }

  # =============================================================================
  # ELHORST (2010) MODEL SELECTION STRATEGY - STEP 1
  # =============================================================================
  
  cat("\n--- Step 1: LM Tests on OLS Residuals ---\n")
  lm_tests <- lm.RStests(model_ols, listw, test = "all")
  print(lm_tests)
  
  # Extract p-values from the htest objects (lm.RStests uses RS* names)
  p_lag <- lm_tests$RSlag$p.value
  p_err <- lm_tests$RSerr$p.value
  p_rlag <- lm_tests$adjRSlag$p.value
  p_rerr <- lm_tests$adjRSerr$p.value
  
  cat(sprintf("  RS-lag: p=%.4f | RS-err: p=%.4f\n", p_lag, p_err))
  cat(sprintf("  adjRS-lag: p=%.4f | adjRS-err: p=%.4f\n", p_rlag, p_rerr))

  # =============================================================================
  # STEP 2: Estimate spatial models
  # =============================================================================
  
  cat("\n--- Step 2: Estimating Spatial Models ---\n")
  formula_final <- formula(model_ols)
  durbin_formula <- ~ accommodates + bathrooms + n_reviews + availability_365
  
  model_sar <- lagsarlm(formula_final, data = df_period, listw = listw, 
                        method = "LU", quiet = TRUE)
  model_sem <- errorsarlm(formula_final, data = df_period, listw = listw, 
                          method = "LU", quiet = TRUE)
  model_slx <- lmSLX(formula_final, data = df_period, listw = listw, 
                     Durbin = durbin_formula)
  
  # Estimate SDM if LM tests indicate spatial dependence (Step 2)
  if (p_lag < 0.05 | p_err < 0.05) {
    cat("  OLS rejected → Estimating SDM (most general model)\n")
    model_sdm <- lagsarlm(formula_final, data = df_period, listw = listw, 
                          Durbin = durbin_formula, method = "LU", quiet = TRUE)
    estimate_sdm <- TRUE
  } else {
    cat("  OLS not rejected → SDM not needed, will test SLX at Step 7\n")
    model_sdm <- NULL
    estimate_sdm <- FALSE
  }


  # =============================================================================
  # STEPS 3-7: MODEL SELECTION (Elhorst 2010 Strategy)
  # =============================================================================
  
  if (estimate_sdm) {
    # STEP 3: LRT tests using anova()
    cat("\n--- Step 3: Likelihood Ratio Tests ---\n")
    lrt_sar <- anova(model_sdm, model_sar)
    lrt_sem <- anova(model_sdm, model_sem)
    
    p_sdm_sar <- lrt_sar$`p-value`[2]
    p_sdm_sem <- lrt_sem$`p-value`[2]
    
    cat(sprintf("  SDM vs SAR: LR=%.2f, p=%.4f\n", lrt_sar$L.Ratio[2], p_sdm_sar))
    cat(sprintf("  SDM vs SEM: LR=%.2f, p=%.4f\n", lrt_sem$L.Ratio[2], p_sdm_sem))
    
    reject_sar <- (p_sdm_sar < 0.05)
    reject_sem <- (p_sdm_sem < 0.05)
    
    # STEP 4: Both restrictions rejected?
    if (reject_sar & reject_sem) {
      best_model_name <- "SDM"
      best_model <- model_sdm
      cat("\nStep 4: Both restrictions rejected → SDM best describes data\n")
      
    # STEP 5: Only one restriction rejected
    } else if (!reject_sar & p_rlag < p_rerr) {
      best_model_name <- "SAR"
      best_model <- model_sar
      cat("\nStep 5: SAR restriction not rejected + RLM-lag < RLM-err → SAR preferred\n")
      
    } else if (!reject_sem & p_rerr < p_rlag) {
      # STEP 6: Test SDEM
      cat("\nStep 6: SEM restriction not rejected. Testing SDEM...\n")
      model_sdem <- errorsarlm(formula_final, data = df_period, listw = listw,
                               Durbin = durbin_formula, method = "LU", quiet = TRUE)
      
      lrt_sdem <- anova(model_sdem, model_sem)
      p_sdem <- lrt_sdem$`p-value`[2]
      cat(sprintf("  SDEM vs SEM: LR=%.2f, p=%.4f\n", lrt_sdem$L.Ratio[2], p_sdem))
      
      if (p_sdem < 0.05) {
        best_model_name <- "SDEM"
        best_model <- model_sdem
        cat("  WX terms significant → SDEM preferred\n")
      } else {
        best_model_name <- "SEM"
        best_model <- model_sem
        cat("  WX terms not significant → SEM preferred\n")
      }
      
    } else {
      best_model_name <- "SDM"
      best_model <- model_sdm
      cat("\nAmbiguous results → Defaulting to SDM\n")
    }
    
  } else {
    # STEP 7: OLS not rejected - test SLX
    cat("\n--- Step 7: Testing SLX vs OLS ---\n")
    
    # Test if any WX coefficients are significant
    slx_summary <- summary(model_slx)
    wx_coefs <- grep("^lag\\.", rownames(slx_summary$coefficients), value = TRUE)
    
    if (length(wx_coefs) > 0) {
      wx_pvals <- slx_summary$coefficients[wx_coefs, "Pr(>|t|)"]
      any_significant <- any(wx_pvals < 0.05)
      cat(sprintf("  WX variables: %d tested, %d significant (p<0.05)\n", 
                  length(wx_pvals), sum(wx_pvals < 0.05)))
      
      if (any_significant) {
        best_model_name <- "SLX"
        best_model <- model_slx
        cat("  WX terms significant → SLX preferred\n")
      } else {
        best_model_name <- "OLS"
        best_model <- model_ols
        cat("  WX terms not significant → OLS sufficient\n")
      }
    } else {
      best_model_name <- "OLS"
      best_model <- model_ols
      cat("  No WX terms specified → OLS sufficient\n")
    }
  }
  
  cat(sprintf("\n✓ FINAL MODEL: %s\n", best_model_name))


  # =============================================================================
  # IMPACTS CALCULATION (Direct, Indirect, Total Effects)
  # Only for the FINAL BEST MODEL (faster)
  # =============================================================================

  cat("\n--- Calculating Impacts for Best Model ---\n")
  
  # Strategy: Try sparse matrix (fast) OR use listw (robust)
  trMat <- NULL
  R_sims <- 50  # Reduced from 100 to balance speed/precision
  
  if (is_connected) {
    # Graph is connected - try sparse matrix method
    trMat <- tryCatch({
       cat("  Attempting sparse matrix traces (fast)...\n")
       # Convert listw to sparse matrix format required by trW
       W_sparse <- as(listw, "CsparseMatrix")
       tr_result <- trW(W_sparse, type = "mult")
       cat("  ✓ Sparse traces calculated\n")
       tr_result
    }, error = function(e) { 
       cat(sprintf("  Sparse failed: %s\n", e$message))
       NULL 
    })
  } else {
    cat("  Graph disconnected - using listw method\n")
  }
  
  # Calculate impacts for best model only (with robust fallback)
  best_impacts <- NULL
  
  if (best_model_name %in% c("SDM", "SAR")) {
    # Try standard impacts first
    best_impacts <- tryCatch({
      if (!is.null(trMat)) {
        impacts(best_model, tr = trMat, R = R_sims)
      } else {
        impacts(best_model, listw = listw, R = R_sims)
      }
    }, error = function(e) {
      cat(sprintf("  ⚠ %s impacts failed: %s\n", best_model_name, e$message))
      
      # FALLBACK: If SDM fails, try SAR (simpler, more stable)
      if (best_model_name == "SDM" && !is.null(model_sar)) {
        cat("  → Attempting SAR impacts as fallback...\n")
        tryCatch({
          if (!is.null(trMat)) {
            sar_imp <- impacts(model_sar, tr = trMat, R = R_sims)
          } else {
            sar_imp <- impacts(model_sar, listw = listw, R = R_sims)
          }
          cat("  ✓ SAR impacts calculated (fallback)\n")
          sar_imp
        }, error = function(e2) {
          cat(sprintf("  ⚠ SAR fallback also failed: %s\n", e2$message))
          NULL
        })
      } else {
        NULL
      }
    })
    
    if (!is.null(best_impacts)) {
      cat(sprintf("  ✓ %s impacts calculated (R=%d)\n", best_model_name, R_sims))
    }
  } else if (best_model_name == "SLX") {
    best_impacts <- tryCatch({
      impacts(best_model, listw = listw)
    }, error = function(e) { 
      cat(sprintf("  ⚠ SLX impacts failed: %s\n", e$message))
      NULL 
    })
    if (!is.null(best_impacts)) cat("  ✓ SLX impacts calculated\n")
  } else {
    cat(sprintf("  No impacts for %s model\n", best_model_name))
  }

  # =============================================================================
  # STORE RESULTS
  # =============================================================================
  
  results[[quadrimester]] <- list(
    n_obs = nrow(df_period),
    mean_price = mean(df_period$price),
    median_price = median(df_period$price),
    k_neighbors = k,
    graph_connected = is_connected,
    ols = model_ols,
    sar = model_sar,
    sdm = model_sdm,
    sem = model_sem,
    slx = model_slx,
    best_model = best_model_name,
    best_model_obj = best_model,
    impacts_best = best_impacts,
    lm_tests = lm_tests,
    moran_test = moran_test,
    moran_price = moran_price
  )
  
  cat(sprintf("\n✓ Period %s completed\n", toupper(quadrimester)))
}

# =============================================================================
# CROSS-PERIOD COMPARISON TABLES
# =============================================================================

cat("\n\n========================================\n")
cat("CROSS-PERIOD COMPARISON\n")
cat("========================================\n\n")

# Table 1: Sample & Spatial Characteristics
cat("--- TABLE 1: Sample Size & Spatial Characteristics by Period ---\n")
comparison_descriptive <- data.frame(
  Period = names(results),
  N_Listings = sapply(results, function(x) x$n_obs),
  Mean_Price = sapply(results, function(x) round(x$mean_price, 2)),
  k_neighbors = sapply(results, function(x) x$k_neighbors),
  Moran_I = sapply(results, function(x) round(x$moran_price$estimate["Moran I statistic"], 3)),
  Moran_pval = sapply(results, function(x) round(x$moran_price$p.value, 4))
)
print(comparison_descriptive)
cat("\n")

# Table 2: Model Selection (Elhorst Strategy)
cat("--- TABLE 2: Model Selection by Period ---\n")
comparison_models <- data.frame(
  Period = names(results),
  Best_Model = sapply(results, function(x) x$best_model),
  AIC_SAR = sapply(results, function(x) round(AIC(x$sar), 1)),
  AIC_SDM = sapply(results, function(x) if(!is.null(x$sdm)) round(AIC(x$sdm), 1) else NA),
  AIC_SEM = sapply(results, function(x) round(AIC(x$sem), 1))
)
print(comparison_models)
cat("\n")

# Table 3: dist_ski Effects (Seasonality Analysis)
cat("--- TABLE 3: dist_ski Direct Effects by Period ---\n")

get_ski_impact <- function(res) {
  imp_obj <- res$impacts_best
  if (!is.null(imp_obj)) {
    # Controllo se è SAR/SDM (ha zstats) o SLX (non ha zstats complesse)
    if (inherits(imp_obj, "lagImpact")) { 
       imp <- summary(imp_obj, zstats = TRUE)
       mat <- imp$mat
       pzmat <- imp$pzmat
    } else if (inherits(imp_obj, "WXImpact")) {
       imp <- summary(imp_obj, zstats = TRUE)
       mat <- imp$mat
       pzmat <- imp$pzmat
    } else {
       return(c(Direct = NA, Pval = NA))
    }
    
    if ("dist_ski" %in% rownames(mat)) {
      return(c(
        Direct = round(mat["dist_ski", "Direct"], 4), 
        Pval = round(pzmat["dist_ski", "Direct"], 4)
      ))
    }
  }
  return(c(Direct = NA, Pval = NA))
}

ski_impacts <- t(sapply(results, get_ski_impact))
comparison_ski <- data.frame(
  Period = names(results),
  dist_ski_Direct = ski_impacts[, "Direct"],
  P_Value = ski_impacts[, "Pval"]
)
print(comparison_ski)
cat("\n")

# =============================================================================
# SAVE RESULTS TO ../results
# =============================================================================

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

save(results, file = "results/multi_period_analysis.RData")
cat("✓ Saved: results/multi_period_analysis.RData\n\n")

write.csv(comparison_descriptive, "results/table1_descriptive.csv", row.names=FALSE)
write.csv(comparison_models, "results/table2_model_selection.csv", row.names=FALSE)
write.csv(comparison_ski, "results/table3_seasonality.csv", row.names=FALSE)

cat("✓ Exported comparison tables to results/\n\n")

# Export individual period results
for (period_name in names(results)) {
  period_data <- results[[period_name]]
  best_model_name <- period_data$best_model
  model_obj <- period_data$best_model_obj
  imp_obj <- period_data$impacts_best
  
  # Save coefficients
  if (!is.null(model_obj)) {
    if (inherits(model_obj, "sarlm")) {
       coef_table <- as.data.frame(summary(model_obj)$Coef)
    } else {
       coef_table <- as.data.frame(summary(model_obj)$coefficients)
    }
    coef_table$Variable <- rownames(coef_table)
    write.csv(coef_table, paste0("results/", period_name, "_coef.csv"), row.names = FALSE)
  }

  # Save impacts (if available)
  if (!is.null(imp_obj)) {
    cat(sprintf("  DEBUG: imp_obj class = %s\n", paste(class(imp_obj), collapse=", ")))
    tryCatch({
      imp_summary <- summary(imp_obj, zstats = TRUE)
      cat(sprintf("  DEBUG: imp_summary$mat dimensions = %dx%d\n", 
                  nrow(imp_summary$mat), ncol(imp_summary$mat)))
      
      if (nrow(imp_summary$mat) > 0) {
        impacts_table <- data.frame(
          Variable = rownames(imp_summary$mat),
          Direct = imp_summary$mat[, "Direct"],
          Indirect = imp_summary$mat[, "Indirect"],
          Total = imp_summary$mat[, "Total"]
        )
        write.csv(impacts_table, paste0("results/", period_name, "_impacts.csv"), row.names = FALSE)
        cat(sprintf("  ✓ Impacts CSV written (%d variables)\n", nrow(impacts_table)))
      } else {
        cat("  ⚠ imp_summary$mat is EMPTY (0 rows)!\n")
        cat("  DEBUG: Trying direct access to impacts object...\n")
        # Try alternative extraction methods
        if (!is.null(imp_obj$mat)) {
          cat(sprintf("  DEBUG: imp_obj$mat dimensions = %dx%d\n", 
                      nrow(imp_obj$mat), ncol(imp_obj$mat)))
        }
        write.csv(data.frame(Error="No impacts data available"), 
                  paste0("results/", period_name, "_impacts.csv"), row.names = FALSE)
      }
    }, error = function(e) {
      cat(sprintf("  ⚠ ERROR writing impacts: %s\n", e$message))
    })
  } else {
    cat(sprintf("  ⚠ No impacts to export (imp_obj is NULL)\n"))
  }
  
  cat(sprintf("✓ Exported %s results (Model: %s)\n", period_name, best_model_name))
}

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")

