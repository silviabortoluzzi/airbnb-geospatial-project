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
```

## How to Reproduce

### Requirements

Install Python dependencies:
```bash
pip install -r requirements.txt
```

## Interactive Map

The file `results/airbnb_map.html` is a interactive map that can be opened directly in any browser. It covers the entire Province of Trentino and displays 5,710 Airbnb listings scraped in December 2024.

---

### Navigation
The map opens already framed on the Province of Trentino. 

In the **top-right corner** there is a panel listing all available layers. Each layer has a **checkbox** that you can tick or untick to show or hide it. Several layers can be active at the same time.

---

#### Rooms: Entire home/apt · Rooms: Private room · Rooms: Hotel room
Three separate layers, one per room type. Each listing is represented by a **small coloured circle** on the map. The colour encodes the **nightly price in €**, following a yellow → orange → red gradient:

| Colour | Price range |
|---|---|
| 🟡 Yellow | Lowest prices |
| 🟠 Orange | Mid-range |
| 🔴 Dark red | Highest prices |

**Activating/deactivating** each room type independently lets you compare the spatial distribution of price across categories. The colour legend (bottom-right) updates automatically: it appears only when at least one of these layers is active, and switches to the municipality legend when that layer takes over.

**Hover** over any circle to see a tooltip with:
- Listing ID
- Room type
- Nightly price (€)
- Number of guests (`accommodates`)
- Price per guest (€)

---

####  Listings density (Heatmap)
A continuous **heat map** that shows where listings are most spatially concentrated. Areas with a higher density of listings appear in warmer colours (yellow → red). Useful for identifying the main tourist poles at a glance, without the visual noise of individual dots.

> Turn off the room-type layers before enabling this one for a cleaner view.

---

#### Mean price by municipality
A **choropleth map** that colours each municipality polygon according to its average Airbnb nightly price. The colour scale runs from light yellow (cheapest municipalities) to dark red (most expensive).

**Hover** over any municipality to see:
- Municipality name
- Average nightly price (€), or "No listings" if no Airbnb is present

This layer makes it easy to spot at a glance which valleys and resort areas command the highest prices at the municipal level. When this layer is active, the colour legend switches automatically to show the municipal price scale.

---

####  Listing Clusters (Spatial Spillover)
A **marker cluster** layer. Instead of plotting all 5,710 points at once, nearby listings are grouped into numbered circle clusters. As you zoom in, clusters split into smaller groups and eventually into individual listing markers.

Individual markers are colour-coded by price tier:
- 🟢 **Green** — price ≤ €80/night
- 🟠 **Orange** — price €81–150/night
- 🔴 **Dark red** — price > €150/night

**Hover** over an individual marker to see the same tooltip as the dot layers (ID, room type, price, accommodates, price per guest). This layer is especially useful for exploring listings in densely packed ski resort areas where individual dots overlap.

---

#### OSM: Siti Culturali e Buffer (3km)
Points of interest extracted from OpenStreetMap for **castles and museums** across the province, each surrounded by a **3 km buffer circle**. Useful for visually assessing how listings relate to cultural heritage sites — and for understanding the counterintuitive positive `dist_castle` coefficient: most castles sit in the valley floors, far from the high-altitude ski clusters.

---

#### OSM: Impianti Sciistici e Buffer (3km)
**Ski lift locations** from OpenStreetMap, each with a **3 km buffer circle**. This is the most analytically relevant POI layer: overlaying it with the price dot layers immediately shows how the highest-priced listings (dark red) cluster tightly within or adjacent to the ski lift buffers, visually confirming the model's key finding.

---

#### OSM: Laghi e Buffer (3km)
**Selected lakes** (19 lakes with genuine tourist relevance, e.g. Garda, Ledro, Levico), each with a **3 km buffer circle**. Overlaying this layer with the price dots illustrates why `dist_lake` is not significant in winter: lake areas show no consistent price premium during the December season.

---

### Legend

The colour legend appears in the corner and is context-aware:
- When a **Rooms:** layer is active → shows the *Listing price per night (€)* scale
- When the **Mean price by municipality** layer is active → switches to the *Average Airbnb price by municipality (€)* scale
- When neither is active → the legend disappears automatically


## Methodology & Data Analysis

The quantitative analysis pipeline strictly follows the spatial econometrics framework proposed by Elhorst (2010) to account for spatial interdependence:

1. **Hedonic Baseline Model (OLS):** Grounded in Rosen's (1974) hedonic pricing theory, an initial OLS regression decomposes log-prices (`log_price`) into structural capacity, reputation, and linear distances to key amenities. While visual residual diagnostics confirm normal error distributions and homoscedasticity, OLS fails to account for spatial interdependencies.
2. **Coordinate Jittering & Spatial Weights ($W$):** Due to platform privacy policies obfuscating locations up to 150 meters, exactly 9% of observations shared identical spatial coordinates. To avoid zero-distance computational singularities, a **micro-jittering technique** (Bivand et al., 2013) applies a random 5–10 meter perturbation. A row-standardized spatial weights matrix is then constructed using the $k$-nearest neighbors criterion ($k=15$) to capture authentic resort-level agglomeration economies.
3. **Spatial Autocorrelation Diagnostics:** The Global Moran's I test applied to baseline OLS residuals yields highly significant spatial dependence ($I=0.136, p<0.001$). Local Indicators of Spatial Association (LISA) successfully map significant "High-High" price clusters precisely over the main Alpine ski domains.
4. **Sequential Model Selection:** Robust Lagrange Multiplier (LM) tests on OLS residuals reject the null hypothesis of spatial independence ($p<0.001$) in favor of both Spatial Lag (SAR) and Spatial Error (SEM) specifications. Consequently, the overarching **Spatial Durbin Model (SDM)** is estimated. Subsequent Likelihood Ratio (LR) tests strongly reject linear restrictions reducing the SDM to nested SAR or SEM forms ($p<2.2 \times 10^{-16}$), formally establishing the SDM as the optimal model.
5. **Impact Decomposition & Validation:** Direct, indirect (spatial spillovers), and total impacts are simulated via Monte Carlo methods. Diagnostics confirm that the SDM effectively neutralizes global spatial autocorrelation (yielding a flat residual Moran scatterplot with $I=-0.002, p=0.697$) and optimizes overall model fit (reducing AIC from 8525.25 in OLS to 8074.21).

   
## Conclusions

Winter rental pricing in Trentino is an intrinsically **spatial and seasonal phenomenon**. Urban centrality (Alonso's monocentric model) is entirely superseded by **"Alpine Gravity"**: ski infrastructure defines the true value centres of the winter market. The SDM reveals that the benefits of proximity to ski slopes extend beyond individual listings to entire surrounding neighbourhoods through positive spatial spillovers.

**Policy implication:** Municipalities outside ski resort clusters face a structural competitive disadvantage. Investing in integrated transport infrastructure to reduce logistical distance to slopes could extend the spatial reach of positive spillovers and distribute tourism-generated wealth more equitably across the province.

---

## Limitations and Future Work

- Unobserved micro-spatial features (interior design, panoramic views, dynamic pricing algorithms) likely explain remaining residual variance
- Future work: multi-period model across all four seasons; integration of digital elevation models (DEM) to explicitly control for altitude
