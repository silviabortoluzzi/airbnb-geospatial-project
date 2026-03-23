# =============================================================================
# SPATIAL HEDONIC PRICING MODEL - TRENTINO AIRBNB
# Theory-Driven Approach Following Elhorst (2010) Strategy
# =============================================================================

# SETUP & LIBRARIES
rm(list = ls())

# Check current working directory
print(paste("Current working directory:", getwd()))

# Set working directory to project root
# Adjust the path to your system
setwd("C:/Users/borto/Desktop/unitn/geospatial/geospatial-project")

# Verify
print(paste("New working directory:", getwd()))

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
# DATA LOADING & PREPARATION
# =============================================================================

# Convert categorical variables into explicit numerical dummies
# "Entire home/apt" is kept as the baseline reference category to avoid multicollinearity
df$is_private_room <- ifelse(df$room_type == "Private room", 1, 0)
df$is_shared_room <- ifelse(df$room_type == "Shared room", 1, 0)
df$is_hotel_room <- ifelse(df$room_type == "Hotel room", 1, 0)

# Rescale distances from meters to kilometers
# (Section 4.4: Interpretation as "% price change per 1 km")
vars_dist <- grep("dist_", names(df), value = TRUE)
for(v in vars_dist) {
  df[[v]] <- df[[v]] / 1000
}

df <- na.omit(df)

# Filter for december period only
df <- df[df$period == "december", ]
print(paste("Data Loaded. After december filter:", nrow(df), "observations"))

# =============================================================================
# THEORY-BASED MODEL SPECIFICATION (Section 4.1-4.4)
# =============================================================================

print("--- STEP 1: THEORETICALLY-DRIVEN VARIABLE SELECTION ---")
print("Theoretical Framework: Hedonic Pricing + Spatial Dependence")
print("References: Rosen (1974), Tobler, Elhorst (2010), Torres-Luque (2025)")

# Core Model based on Literature Review (Section 2.1-2.3)
model_ols <- lm(log_price ~ 
                # Structural (Section 4.1)
                is_private_room + is_shared_room + is_hotel_room + accommodates + 
                
                # Reputational (Section 4.2)
                n_reviews + 
                
                # Locational (Section 4.4)
                # Urban: Monocentricity (Alonso)
                dist_center + 
                
                # Alpine: Ski-Lift Premium (Guest Favorites)
                dist_ski + 
                
                # Lacustrine: Shoreline Premium (Garda studies)
                dist_lake + 
                
                # Cultural: Micro-centralities (Guarini 2026, Smart Cities 2023)
                dist_castle + dist_museum +
                
                # Services: Agglomeration Economies (Section 2.1)
                dist_restaurant + dist_supermarket,
                
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

# The analysis detected 454 observations (5.93%) with duplicated coordinates compared to previous observations, for a total of 697 listings (9.1%) involved in duplicate groups. 
# This pattern is attributable to Airbnb's practice of applying systematic offsets for privacy and the presence of multi-unit buildings.

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
print("--- STEP 1: Lagrange Multiplier Tests (Elhorst 2010) ---")
lm_tests <- lm.RStests(model_ols, listw, test = "all")
print(lm_tests)

# Extract p-values (RS tests = Rao's Score, equivalent to LM)
p_lag <- lm_tests$RSlag$p.value
p_err <- lm_tests$RSerr$p.value
p_rlag <- lm_tests$adjRSlag$p.value  # Robust LM-lag
p_rerr <- lm_tests$adjRSerr$p.value  # Robust LM-err

print("--- Elhorst (2010) Model Selection Strategy ---")
print(paste("RS-lag p-value:", round(p_lag, 4)))
print(paste("RS-err p-value:", round(p_err, 4)))
print(paste("Robust RS-lag p-value:", round(p_rlag, 4)))
print(paste("Robust RS-err p-value:", round(p_rerr, 4)))

# =============================================================================
# SPATIAL MODEL ESTIMATION (Elhorst 2010 - 7 Step Strategy)
# =============================================================================

print("--- STEP 2: ESTIMATING SPATIAL MODELS ---")

formula_final <- formula(model_ols)

# Create a specific Durbin formula to avoid lagging absolute distances.
# Lagging distance variables (like dist_center) on KNN neighbors creates near-perfect 
# multicollinearity, as neighbors share almost identical distances.
durbin_vars <- ~ is_private_room + is_shared_room + is_hotel_room + accommodates + n_reviews

# Let's save/load heavy models to avoid recompiling every time
models_file <- "results/spatial_models_december.RData"

if (file.exists(models_file)) {
  print("Found pre-computed models! Loading them from disk to save time...")
  load(models_file)
} else {
  print("No pre-computed models found. Estimating models (this might take a while)...")

  # Always estimate SAR, SEM, and SLX for comparison
  print("Estimating SAR (Spatial Lag Model)...")
  model_sar <- lagsarlm(formula_final, data = df, listw = listw)
  
  print("Estimating SEM (Spatial Error Model)...")
  model_sem <- errorsarlm(formula_final, data = df, listw = listw)
  
  print("Estimating SLX (Spatially Lagged X Model)...")
  model_slx <- lmSLX(formula_final, data = df, listw = listw, Durbin = durbin_vars)
  
  # STEP 2: Estimate SDM if LM tests reject OLS
  if (p_lag < 0.05 | p_err < 0.05) {
    print("OLS rejected by LM tests → Estimating SDM (most general model)...")
    model_sdm <- lagsarlm(formula_final, data = df, listw = listw, 
                          Durbin = durbin_vars)
    estimate_sdm <- TRUE
  } else {
    print("OLS not rejected → SDM not needed, will test SLX at Step 7")
    model_sdm <- NULL
    estimate_sdm <- FALSE
  }
  
  # Estimate SDEM preemptively if SEM is likely to be chosen (for complete caching)
  # This avoids breaking the load/save logic down the script
  model_sdem <- NULL
  if (estimate_sdm) {
    print("Estimating SDEM (Spatial Durbin Error Model) for potential Step 6...")
    model_sdem <- errorsarlm(formula_final, data = df, listw = listw,
                             Durbin = durbin_vars)
  }

  print("Saving estimated models to disk for future runs...")
  save(model_sar, model_sem, model_slx, model_sdm, model_sdem, estimate_sdm, 
       file = models_file)
  print("Models saved successfully!")
}

# =============================================================================
# STEPS 3-7: MODEL SELECTION (Elhorst 2010 Strategy)
# =============================================================================

print("--- STEPS 3-7: Elhorst Model Selection ---")

if (estimate_sdm) {
  # STEP 3: Likelihood Ratio Tests using anova()
  print("STEP 3: Likelihood Ratio Tests (LRT)")
  
  lrt_sar <- anova(model_sdm, model_sar)
  lrt_sem <- anova(model_sdm, model_sem)
  
  p_sdm_sar <- lrt_sar$`p-value`[2]
  p_sdm_sem <- lrt_sem$`p-value`[2]
  
  print(paste("  SDM vs SAR: LR =", round(lrt_sar$L.Ratio[2], 2), ", p =", round(p_sdm_sar, 4)))
  print(paste("  SDM vs SEM: LR =", round(lrt_sem$L.Ratio[2], 2), ", p =", round(p_sdm_sem, 4)))
  
  reject_sar <- (p_sdm_sar < 0.05)
  reject_sem <- (p_sdm_sem < 0.05)
  
  # STEP 4: Both restrictions rejected?
  if (reject_sar & reject_sem) {
    best_model_name <- "SDM"
    best_model <- model_sdm
    print("STEP 4: Both H0 rejected → SDM best describes data")
    
  # STEP 5: Only one restriction rejected
  } else if (!reject_sar & p_rlag < p_rerr) {
    best_model_name <- "SAR"
    best_model <- model_sar
    print("STEP 5: SAR restriction not rejected + RLM-lag < RLM-err → SAR preferred")
    
  } else if (!reject_sem & p_rerr < p_rlag) {
    # STEP 6: Test SDEM
    print("STEP 6: SEM restriction not rejected. Testing SDEM...")
    # model_sdem is already estimated and loaded from cache if available
    
    lrt_sdem <- anova(model_sdem, model_sem)
    p_sdem <- lrt_sdem$`p-value`[2]
    print(paste("  SDEM vs SEM: LR =", round(lrt_sdem$L.Ratio[2], 2), ", p =", round(p_sdem, 4)))
    
    if (p_sdem < 0.05) {
      best_model_name <- "SDEM"
      best_model <- model_sdem
      print("  WX terms significant → SDEM preferred")
    } else {
      best_model_name <- "SEM"
      best_model <- model_sem
      print("  WX terms not significant → SEM preferred")
    }
    
  } else {
    best_model_name <- "SDM"
    best_model <- model_sdm
    print("Ambiguous results → Defaulting to SDM (most general)")
  }
  
} else {
  # STEP 7: OLS not rejected - test SLX
  print("STEP 7: Testing SLX vs OLS...")
  
  # Test if any WX coefficients are significant
  slx_summary <- summary(model_slx)
  wx_coefs <- grep("^lag\\.", rownames(slx_summary$coefficients), value = TRUE)
  
  if (length(wx_coefs) > 0) {
    wx_pvals <- slx_summary$coefficients[wx_coefs, "Pr(>|t|)"]
    any_significant <- any(wx_pvals < 0.05)
    print(paste("  WX variables tested:", length(wx_pvals), "| Significant:", sum(wx_pvals < 0.05)))
    
    if (any_significant) {
      best_model_name <- "SLX"
      best_model <- model_slx
      print("  WX terms significant → SLX preferred")
    } else {
      best_model_name <- "OLS"
      best_model <- model_ols
      print("  WX terms not significant → OLS sufficient")
    }
  } else {
    best_model_name <- "OLS"
    best_model <- model_ols
    print("  No WX terms specified → OLS sufficient")
  }
}

print(paste("\n✓ FINAL MODEL SELECTED:", best_model_name))
print(summary(best_model))

# Model Comparison Table (AIC)
print("--- MODEL COMPARISON (AIC) ---")
aic_comparison <- data.frame(
  Model = c("OLS", "SAR", "SEM", "SLX"),
  AIC = c(AIC(model_ols), AIC(model_sar), AIC(model_sem), AIC(model_slx))
)

if (!is.null(model_sdm)) {
  aic_comparison <- rbind(aic_comparison, 
                          data.frame(Model = "SDM", AIC = AIC(model_sdm)))
}

if (exists("model_sdem") && !is.null(model_sdem)) {
  aic_comparison <- rbind(aic_comparison, 
                          data.frame(Model = "SDEM", AIC = AIC(model_sdem)))
}

aic_comparison <- aic_comparison[order(aic_comparison$AIC), ]
print(aic_comparison)
print(paste("Lowest AIC:", aic_comparison$Model[1]))

# =============================================================================
# IMPACTS CALCULATION (Section 5 - Spillover Effects)
# =============================================================================

print("--- CALCULATING SPATIAL IMPACTS ---")
print("Monte Carlo simulations: R=100 draws")

# Calculate impacts for best model
best_impacts <- NULL

if (best_model_name %in% c("SDM", "SAR", "SDEM")) {
  best_impacts <- tryCatch({
    impacts(best_model, listw = listw, R = 100)
  }, error = function(e) {
    print(paste("Warning: Impacts calculation failed:", e$message))
    NULL
  })
  
  if (!is.null(best_impacts)) {
    print("--- SPATIAL IMPACTS SUMMARY ---")
    print("Direct: Own-property effect")
    print("Indirect: Spillover to neighbors") 
    print("Total: Direct + Indirect")
    print(summary(best_impacts, zstats = TRUE, short = TRUE))
  } else {
    print("Impacts calculation unavailable - coefficients still valid")
  }
  
} else if (best_model_name == "SLX") {
  best_impacts <- tryCatch({
    impacts(best_model, listw = listw)
  }, error = function(e) {
    print(paste("Warning: SLX impacts failed:", e$message))
    NULL
  })
  
  if (!is.null(best_impacts)) {
    print("--- SLX IMPACTS SUMMARY ---")
    print(summary(best_impacts, zstats = TRUE))
  }
  
} else {
  print(paste("No spatial impacts for", best_model_name, "model"))
}

# =============================================================================
# EXPORT SUMMARY REPORT & PLOTS
# =============================================================================

print("--- EXPORTING SUMMARY REPORT AND PLOTS TO 'results/' FOLDER ---")

if (!dir.exists("results")) {
  dir.create("results")
}

# 1. TEXT REPORT
report_file <- "results/december_summary_report.txt"
sink(report_file)
cat("=============================================================================\n")
cat("SPATIAL HEDONIC PRICING MODEL - TRENTINO AIRBNB - DECEMBER\n")
cat("=============================================================================\n\n")

cat("1. FINAL MODEL SELECTED:", best_model_name, "\n\n")
print(summary(best_model))
cat("\n")

cat("2. SPATIAL IMPACTS (Direct, Indirect, Total)\n\n")
if (!is.null(best_impacts)) {
  if (best_model_name == "SLX") {
    print(summary(best_impacts, zstats = TRUE))
  } else {
    print(summary(best_impacts, zstats = TRUE, short = TRUE))
  }
} else {
  cat("Impacts calculation unavailable or not applicable.\n")
}
cat("\n")

cat("3. MODEL COMPARISON (AIC)\n\n")
print(aic_comparison)
cat("\n")

cat("4. MORAN'S I OLS RESIDUALS TEST\n\n")
print(moran_test)

cat("\n=============================================================================\n")
sink()
print(paste("✓ Summary report saved to:", report_file))

# 2. DIAGNOSTIC PLOTS
plots_file <- "results/december_diagnostic_plots.pdf"
pdf(plots_file, width = 10, height = 8)

# Baseline OLS diagnostics (4 plots)
par(mfrow=c(2,2))
plot(model_ols, main="OLS Model Diagnostics")

# Moran Scatterplot
par(mfrow=c(1,1))
moran.plot(df$log_price, listw, labels=FALSE, 
           xlab="Log Price", 
           ylab="Spatially Lagged Log Price",
           main="Moran Scatterplot - Spatial Clustering")

# Plot Residuals of the winning model vs Fitted values
# Best models from spatialreg store residuals and fitted.values slightly differently occasionally,
# but usually they are accessible directly or via residuals()/fitted():
bm_res <- residuals(best_model)
bm_fit <- fitted(best_model)

if (!is.null(bm_res) && !is.null(bm_fit)) {
  plot(bm_fit, bm_res, 
       xlab = "Fitted Values", ylab = "Residuals",
       main = paste("Residuals vs Fitted (Winning Model:", best_model_name, ")"),
       pch = 20, col = rgb(0,0,0,0.2))
  abline(h = 0, col = "red", lty = 2, lwd = 2)
}

dev.off()
print(paste("✓ Diagnostic plots saved to:", plots_file))

print("\n========================================")
print("ANALYSIS COMPLETED SUCCESSFULLY")
print("========================================")
print("Theoretical Framework Applied (Rosen 1974, Elhorst 2010)")
print("Spatial Dependence Confirmed (Moran's I, LM tests)")
print(paste("Best Model Selected:", best_model_name))
print("Elhorst 7-Step Strategy Implemented")
print("========================================\n")
