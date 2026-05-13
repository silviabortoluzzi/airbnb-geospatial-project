# Key Pricing Factors of Airbnb's in Trentino: A Spatial Analysis


##  Abstract
This project investigates the core structural, reputational, and locational determinants of short-term rental prices (Airbnb) within the Province of Trentino during the winter peak tourism season. Grounded in classical hedonic pricing theory and advanced spatial econometrics, the study enriches platform listings with dynamic geospatial layers extracted via OpenStreetMap Overpass APIs. 
Initial Ordinary Least Squares (OLS) estimations yielded highly autocorrelated residuals, confirming that spatial independence assumptions are strongly violated in the Alpine accommodation market. Following Elhorst’s sequential model selection framework, a **Spatial Durbin Model (SDM)** was implemented using a row-standardized $k$-nearest neighbors ($k=15$) spatial weights matrix. The empirical findings uncover significant endogenous spatial spillovers and demonstrate that "Alpine Gravity" (proximity to winter ski infrastructure) entirely overshadows traditional urban centrality or summer-related amenities during winter. Additionally, a counterintuitive positive relationship between price and distance from cultural landmarks serves as a crucial spatial proxy for higher elevation.

---

##  Research Question
> *"What are the key factors influencing the pricing of short-term rentals (Airbnb) in Trentino during the winter season?"*

---

## Repository Structure

```text
airbnb-geospatial-project/
│
├── data-analysis/
│   └── analysis.R                    # Spatial econometric models 
│
├── data-extraction/
│   ├── exploratory_analysis.ipynb    # EDA
│   └── trentino_maps_download.py     #  data download 
│
├── datasets/
│   └── airbnb-datasets/december/
│       ├── listings.csv.gz           # Raw Inside Airbnb snapshot (Dec 31 2024 – Jan 1 2025)
│       ├── istat_municipalities.gpkg # ISTAT administrative boundaries (Province of Trento)
│       └── trentino_listings_maps.csv # Final integrated dataset (5,710 obs, 32 variables)
│
├── results/
│   ├── airbnb_map.html               # Interactive map 
│   ├── december_diagnostic_plots.pdf # OLS/SDM residual diagnostics, Moran plots, LISA maps
│   ├── december_summary_report.txt   # Model coefficients and effect decomposition summary
│   └── spatial_models_december.RData # Saved R model objects
│
├── visualization/
│   └── visualization.ipynb           # Map visualizations 
│
├── .gitignore
└── requirements.txt
