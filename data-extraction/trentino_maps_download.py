import geopandas as gpd
import pandas as pd
import osmnx as ox
import numpy as np
import os
import requests
import re

# CONFIGURATION
CRS_METRIC = "EPSG:32632" # UTM Zone 32N is standard metric projection for Italy
REGION_NAME = "Trentino-Alto Adige/Südtirol, Italy"
INPUT_BASE_DIR = "../datasets/airbnb-datasets"
PERIODS = ["december"] #alternatives: "june", "march", "september"
OUTPUT_DIR = "../datasets"
OUTPUT_FILENAME = "trentino_listings_maps.csv" 

# Create the cache directory for the visualization notebook
cache_dir = os.path.join(OUTPUT_DIR, "osm_poi_cache")
os.makedirs(cache_dir, exist_ok=True)

ox.settings.use_cache = True # cache responses speeds up re-runs
ox.settings.cache_folder = os.path.join(OUTPUT_DIR, "cache_osm_all")

def parse_bathrooms(text):
    """Extract number from bathrooms_text (e.g., '2 baths' -> 2.0, '1 shared bath' -> 1.0, 'Half-bath' -> 0.5)"""
    if pd.isna(text):
        return np.nan
    text = str(text).lower()
    if 'half' in text:
        return 0.5
    match = re.search(r'(\d+\.?\d*)', text)
    if match:
        return float(match.group(1))
    return np.nan

def main():
    print(f" STARTING DATA EXTRACTION FOR: {REGION_NAME} ")

    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
        print(f"Created directory: {OUTPUT_DIR}")

    # 1. LOAD AND CLEAN AIRBNB DATA FROM ALL PERIODS
    all_periods_data = []
    
    for period in PERIODS:
        INPUT_FILE = os.path.join(INPUT_BASE_DIR, period, "listings.csv.gz")
        print(f"\nLoading {INPUT_FILE}...")

        try:
            df = pd.read_csv(INPUT_FILE, compression='gzip')
            
            # Add period column
            df['period'] = period
            
            # Basic Cleaning
            # Convert price from string "$100.00" to numeric
            df['price'] = df['price'].replace('[\$,]', '', regex=True).astype(float)

            # Filter out price errors (0) and extreme luxury outliers (> 2000)
            df = df[(df.price > 0) & (df.price < 2000)]
            
            # Cap outliers at 99th percentile
            p99 = df['price'].quantile(0.99)
            df['price_capped'] = df['price'].clip(upper=p99)
            df['log_price_capped'] = np.log(df['price_capped'])
            print(f"99th percentile for {period}: ${p99:.2f}")
            
            # Parse bathrooms_text to numeric
            df['bathrooms'] = df['bathrooms_text'].apply(parse_bathrooms)
            
            # Convert host_is_superhost to binary (t/f -> 1/0)
            df['host_is_superhost'] = df['host_is_superhost'].map({'t': 1, 'f': 0})
            
            # Select columns relevant for Hedonic Pricing Model
            cols_to_keep = [
                'id', 'price', 'latitude', 'longitude', 
                'room_type', 'accommodates', 'number_of_reviews',
                'bedrooms', 'beds', 'bathrooms',
                'review_scores_rating', 'host_is_superhost',
                'availability_365', 'minimum_nights', 'property_type',
                'period'
            ]
            df = df[cols_to_keep].dropna()
            
            all_periods_data.append(df)
            print(f"--> {len(df)} listings loaded for {period}")
            
        except FileNotFoundError:
            print(f"ERROR: File {INPUT_FILE} not found. Skipping {period}.")
            continue
    
    # Combine all periods
    if not all_periods_data:
        print("ERROR: No data found in any period folder. Exiting.")
        return
    
    df_combined = pd.concat(all_periods_data, ignore_index=True)
    print(f"\n--> TOTAL: {len(df_combined)} listings across all periods")

    # Convert to GeoDataFrame
    gdf_airbnb = gpd.GeoDataFrame(
        df_combined, geometry=gpd.points_from_xy(df_combined.longitude, df_combined.latitude), crs="EPSG:4326"
    )
    
    # Project to Metric CRS for accurate distance calculation
    gdf_airbnb = gdf_airbnb.to_crs(CRS_METRIC)
    print(f"--> {len(gdf_airbnb)} listings ready for analysis.")


    # 2. DOWNLOAD OPENSTREETMAP DATA
    print("\n DOWNLOADING OSM LAYERS (This may take a few minutes) ")

    def download_and_project(tags, layer_name):
        """
        Downloads data from OSM, projects it to meters, 
        and returns a unified geometry for fast distance calculation.
        """
        print(f"Downloading: {layer_name}...")
        try:
            gdf = ox.features_from_place(REGION_NAME, tags=tags)
            if gdf.empty:
                print(f"WARNING: No data found for {layer_name}")
                return None
            
            gdf = gdf.to_crs(CRS_METRIC)
            # unary_union merges all shapes into one object (optimizes distance calc)
            return gdf[['geometry']].unary_union 
        except Exception as e:
            print(f"Error downloading {layer_name}: {e}")
            return None


    # ==========================================
    # CUSTOM FILTERING AND EXPORT FOR SKI LIFTS
    # ==========================================
    print("Downloading: Ski Lifts (Custom Filter and Cache Export)...")
    tags_ski = {
        'aerialway': ['chair_lift', 'drag_lift', 't-bar', 'j-bar', 'platter', 'rope_tow', 'magic_carpet', 'gondola', 'cable_car']
    }
    try:
        ski_pois = ox.features_from_place(REGION_NAME, tags=tags_ski)
        if not ski_pois.empty:
            if 'aerialway' in ski_pois.columns:
                ski_pois = ski_pois[ski_pois['aerialway'].isin(tags_ski['aerialway'])]
            
            # Remove entries that are likely huts/refuges
            if 'name' in ski_pois.columns:
                ski_pois = ski_pois[
                    ~ski_pois['name'].str.contains(r'malga|rifugio', case=False, na=False)
                ].copy()

            # Export to GPKG for the visualization notebook
            for col in ski_pois.columns:
                if ski_pois[col].dtype == object:
                    ski_pois[col] = ski_pois[col].astype(str)
            
            ski_file_path = os.path.join(cache_dir, "ski_lifts.gpkg")
            ski_pois.to_file(ski_file_path, driver="GPKG")
            print(f"--> Saved filtered ski lifts to {ski_file_path}")

            # Project and union for spatial distances
            ski_pois_metric = ski_pois.to_crs(CRS_METRIC)
            geom_ski = ski_pois_metric[['geometry']].unary_union
        else:
            geom_ski = None
            print("--> WARNING: No data found for Ski Lifts")
    except Exception as e:
        print(f"Error downloading Ski Lifts: {e}")
        geom_ski = None


    # ==========================================
    # CUSTOM FILTERING AND EXPORT FOR TOP LAKES
    # ==========================================
    print("Downloading: Top 19 Lakes (Custom Filter and Cache Export)...")
    tags_lake = {'natural': 'water'}
    try:
        lake_pois = ox.features_from_place(REGION_NAME, tags=tags_lake)
        if not lake_pois.empty:
            if 'water' in lake_pois.columns:
                lake_pois = lake_pois[lake_pois['water'].isin(['lake', 'reservoir'])]
            
            # Filter by specific tourist lakes
            target_lakes = [
                'garda', 'ledro', 'tenno', 'toblino', 'terlago', 'cavedine', 
                'santa massenza', 'lamar', 'roncone', 'molveno', 'tovel', 
                'caldonazzo', 'levico', 'lavarone', 'serraia', 'santa colomba', 
                "cima d'asta", 'welsperg', 'loppio'
            ]
            pattern = '|'.join(target_lakes)
            lake_pois = lake_pois[lake_pois['name'].str.lower().str.contains(pattern, na=False)]
            lake_pois = lake_pois[lake_pois.geometry.type.isin(['Polygon', 'MultiPolygon'])].copy()
            
            # Export to GPKG for the visualization notebook
            for col in lake_pois.columns:
                if lake_pois[col].dtype == object:
                    lake_pois[col] = lake_pois[col].astype(str)
            
            lake_file_path = os.path.join(cache_dir, "lakes.gpkg")
            lake_pois.to_file(lake_file_path, driver="GPKG")
            print(f"--> Saved strictly filtered lakes to {lake_file_path}")
            
            # Project and union for spatial distances
            lake_pois_metric = lake_pois.to_crs(CRS_METRIC)
            geom_lakes = lake_pois_metric[['geometry']].unary_union
        else:
            geom_lakes = None
            print("--> WARNING: No data found for Lakes")
    except Exception as e:
        print(f"Error downloading Lakes: {e}")
        geom_lakes = None


    # ==========================================
    # CUSTOM EXPORT FOR CULTURE (Castles & Museums)
    # ==========================================
    print("Downloading: Culture Sites (Cache Export)...")
    tags_culture = {'historic': 'castle', 'tourism': 'museum'}
    try:
        culture_pois = ox.features_from_place(REGION_NAME, tags=tags_culture)
        if not culture_pois.empty:
            # Export to GPKG for the visualization notebook
            for col in culture_pois.columns:
                if culture_pois[col].dtype == object:
                    culture_pois[col] = culture_pois[col].astype(str)
                    
            culture_file_path = os.path.join(cache_dir, "culture_castles_museums.gpkg")
            culture_pois.to_file(culture_file_path, driver="GPKG")
            print(f"--> Saved culture sites to {culture_file_path}")

            # Project to metric to calculate distances separately
            culture_pois_metric = culture_pois.to_crs(CRS_METRIC)
            
            # Split castles and museums for the econometric model
            castles = culture_pois_metric[culture_pois_metric['historic'] == 'castle']
            museums = culture_pois_metric[culture_pois_metric['tourism'] == 'museum']
            
            geom_castles = castles[['geometry']].unary_union if not castles.empty else None
            geom_museums = museums[['geometry']].unary_union if not museums.empty else None
        else:
            geom_castles, geom_museums = None, None
            print("--> WARNING: No data found for Culture Sites")
    except Exception as e:
        print(f"Error downloading Culture Sites: {e}")
        geom_castles, geom_museums = None, None


    #  NATURE (Parks using the standard function)
    geom_parks = download_and_project({"leisure": "park"}, "Parks")

    #  TRANSPORT 
    geom_trains = download_and_project({"railway": "station"}, "Train Stations")
    geom_bus = download_and_project({"highway": "bus_stop"}, "Bus Stops")

    #  SERVICES 
    geom_supermarkets = download_and_project({"shop": "supermarket"}, "Supermarkets")
    geom_restaurants = download_and_project({"amenity": "restaurant"}, "Restaurants")
    geom_bars = download_and_project({"amenity": ["bar", "pub"]}, "Bars & Pubs")
    geom_pharmacies = download_and_project({"amenity": "pharmacy"}, "Pharmacies")

     
    # 3. DOWNLOAD ISTAT ADMINISTRATIVE BOUNDARIES
    print("\n DOWNLOADING MUNICIPALITIES FROM ISTAT (Official Italian Borders) ")
    
    istat_url = 'https://github.com/napo/geospatialcourse2025/raw/refs/heads/main/data/istat_administrative_units_generalized_2025.gpkg'
    geopackage_file = os.path.join(OUTPUT_DIR, "istat_municipalities.gpkg")
    
    if not os.path.exists(geopackage_file):
        print("Downloading ISTAT geopackage...")
        r = requests.get(istat_url, allow_redirects=True)
        open(geopackage_file, 'wb').write(r.content)
    
    # Load municipalities layer
    municipalities = gpd.read_file(geopackage_file, layer="municipalities")

    # Filter only Trentino-Alto Adige region (COD_REG = 4)
    municipalities = municipalities[municipalities['COD_REG'] == 4]  # note: number, not string
    
    # Project to metric CRS
    municipalities = municipalities.to_crs(CRS_METRIC)
    
    # Calculate centroids for each municipality
    municipalities['centroid'] = municipalities.geometry.centroid
    
    print(f"--> Loaded {len(municipalities)} municipalities from Trentino-Alto Adige")

     
    # 4. CALCULATE DISTANCES
    print("\n CALCULATING SPATIAL DISTANCES ")

    def calc_dist(geom_target, col_name):
        if geom_target is None:
            return 99999 # Dummy value if layer is missing
        print(f"...calculating distance to {col_name}")
        return gdf_airbnb.geometry.distance(geom_target)

    # Apply calculations
    gdf_airbnb['dist_ski'] = calc_dist(geom_ski, "Ski Lifts")
    gdf_airbnb['dist_lake'] = calc_dist(geom_lakes, "Lakes")
    gdf_airbnb['dist_park'] = calc_dist(geom_parks, "Parks")
    
    gdf_airbnb['dist_station'] = calc_dist(geom_trains, "Train Stations")
    gdf_airbnb['dist_bus'] = calc_dist(geom_bus, "Bus Stops")
    
    gdf_airbnb['dist_supermarket'] = calc_dist(geom_supermarkets, "Supermarkets")
    gdf_airbnb['dist_restaurant'] = calc_dist(geom_restaurants, "Restaurants")
    gdf_airbnb['dist_bar'] = calc_dist(geom_bars, "Bars")
    gdf_airbnb['dist_pharmacy'] = calc_dist(geom_pharmacies, "Pharmacies")
    
    gdf_airbnb['dist_castle'] = calc_dist(geom_castles, "Castles")
    gdf_airbnb['dist_museum'] = calc_dist(geom_museums, "Museums")

    #  Special Calculation: Distance to Municipality Center 
    print("...calculating distance to Municipality Center (Spatial Join)")
    
    # 1. Join Airbnb points to the Municipality polygon they are inside
    gdf_joined = gpd.sjoin(
        gdf_airbnb, 
        municipalities[['geometry', 'centroid', 'COMUNE', 'PRO_COM']],  # COMUNE instead of 'nome', PRO_COM instead of 'cod_istat'
        how="left", 
        predicate="within"
    )
    
    # 2. Calculate distance from the Airbnb to the centroid of ITS municipality
    gdf_joined['dist_center'] = gdf_joined.geometry.distance(gdf_joined['centroid'])
    
    # 3. Fill NaNs (points slightly outside borders) with the mean
    gdf_joined['dist_center'] = gdf_joined['dist_center'].fillna(gdf_joined['dist_center'].mean())

    # 5. EXPORT FINAL DATASET
    print("\n PREPARING OUTPUT ")

    # Create clean DataFrame for R (No geometries)
    df_export = pd.DataFrame({
        'id': gdf_joined['id'],
        'price': gdf_joined['price'],
        'log_price': np.log(gdf_joined['price']), # Log-transf for regression
        'price_per_person': gdf_joined['price'] / gdf_joined['accommodates'], # Price per person per night
        'room_type': gdf_joined['room_type'],
        'property_type': gdf_joined['property_type'],
        'accommodates': gdf_joined['accommodates'],
        'bedrooms': gdf_joined['bedrooms'],
        'beds': gdf_joined['beds'],
        'bathrooms': gdf_joined['bathrooms'],
        'n_reviews': gdf_joined['number_of_reviews'],
        'review_scores_rating': gdf_joined['review_scores_rating'],
        'host_is_superhost': gdf_joined['host_is_superhost'],
        'availability_365': gdf_joined['availability_365'],
        'minimum_nights': gdf_joined['minimum_nights'],
        'period': gdf_joined['period'],
        
        # Municipality info (from ISTAT)
        'municipality': gdf_joined['COMUNE'], 
        'cod_istat': gdf_joined['PRO_COM'],    
        
        # Spatial Variables
        'dist_ski': gdf_joined['dist_ski'],
        'dist_lake': gdf_joined['dist_lake'],
        'dist_park': gdf_joined['dist_park'],
        'dist_station': gdf_joined['dist_station'],
        'dist_bus': gdf_joined['dist_bus'],
        'dist_center': gdf_joined['dist_center'],
        'dist_supermarket': gdf_joined['dist_supermarket'],
        'dist_restaurant': gdf_joined['dist_restaurant'],
        'dist_bar': gdf_joined['dist_bar'],
        'dist_pharmacy': gdf_joined['dist_pharmacy'],
        'dist_castle': gdf_joined['dist_castle'],
        'dist_museum': gdf_joined['dist_museum'],
        
        # Coordinates (Needed for KNN weights in R)
        'lat': gdf_joined['latitude'],
        'long': gdf_joined['longitude']
    })

    output_path = os.path.join(OUTPUT_DIR, OUTPUT_FILENAME)
    df_export.to_csv(output_path, index=False)

    print(f"SUCCESS! Dataset saved to: {output_path}")
    print(f"Total rows: {len(df_export)}")
    print(f"Period distribution:")
    print(df_export['period'].value_counts())

if __name__ == "__main__":
    main()
