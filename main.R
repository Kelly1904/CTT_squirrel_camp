library(renv)
library(celltracktech)
library(dplyr)
library(mapview)

#webshot::install_phantomjs()
#renv::activate()
#renv::snapshot()
#renv::install('cellular-tracking-technologies/celltracktech')


# Load the env file
load_dot_env(file='.env')

#### Set variables ####
# Load env variable into my_token / get your api key from the env file
my_token <- Sys.getenv('API_KEY') 

# Where your downloaded files are to go
outpath <- "data/nodes/" 

# This is your project name on your CTT account
myproject <- "Thompson Rivers University" 

# Set significant digits (number of digits after decimal)
options(digits = 10) 

# Specify the path to your database file
database_file <- "data/squirrel.duckdb"

# Specify the tag ID that you used in your calibration
my_tag_id <- "6133334B"

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where you nodes were in
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
start_time <- as.POSIXct("2026-08-08 00:00:00", tz = "GMT")
stop_time <- as.POSIXct("2026-08-12 18:00:00", tz = "GMT")

# Map tile URL
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"

# Specify a list of node Ids if you only want to include a subset in calibration
# IF you want to use all nodes, ignore this line and SKIP the step below
# IF you want to use a subset, uncomment the step below
# my_nodes <- c("B25AC19E", "44F8E426", "FAB6E12", "1EE02113", "565AA5B9", "EE799439", "1E762CF3", "A837A3F4", "484ED33B")

# connect to the database
#con <- DBI::dbConnect(duckdb::duckdb(), 
#                      dbdir = database_file, 
#                      read_only = TRUE)

# load node_health table in to RStudio and subset it based on your start and stop times
#node_health_df <- tbl(con, "node_health") |> 
#  filter(time >= start_time & time <= stop_time) |>
#  collect()

# disconnect from the database
#DBI::dbDisconnect(con)

# remove any duplicate records
#node_health_df <- node_health_df %>% 
#  distinct(node_id, 
#           time, 
#           recorded_at, 
#           .keep_all = TRUE)


#### Download data from the CTT API ####
# Connect to Database using DuckDB
con <- DBI::dbConnect(duckdb::duckdb(), 
                      dbdir = "data/squirrel.duckdb", 
                      read_only = FALSE)

get_my_data(my_token = my_token,
            outpath = outpath, 
            db_name = con, 
            myproject = myproject,
            begin = as.Date("2026-08-01"), 
            end = as.Date("2026-08-13"), #Modify this to get new data
            filetypes=c("raw", "blu", "gps", "node_health", "sensorgnome", "telemetry", 'log')
)

update_db(con, outpath, myproject)
DBI::dbDisconnect(con)

#### Get node locations ####
con <- DBI::dbConnect(duckdb::duckdb(), 
                      dbdir = database_file, 
                      read_only = TRUE)

# Load node_health table in to RStudio and subset it based on your start and stop times (set above)
node_health_df <- tbl(con, "node_health") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()

DBI::dbDisconnect(con)

# Remove any duplicate records
node_health_df <- node_health_df %>% 
  distinct(node_id, 
           time, 
           recorded_at, 
           .keep_all = TRUE)

# Calculate the average node locations
node_locs <- calculate_node_locations(node_health_df)

# Plot the average node locations
node_loc_plot <- plot_node_locations(node_health_df, 
                                     node_locs,
                                     theme = classic_plot_theme())
node_loc_plot

# Write the node locations to a file
create_outpath('results')

export_node_locations("results/node_locations.csv", 
                      node_locs)

# Draw a map with the node locations
node_map <- map_node_locations(node_locs, 
                               tile_url = my_tile_url)
node_map


#### Load node detections from files
con <- DBI::dbConnect(duckdb::duckdb(), 
                      dbdir = database_file, 
                      read_only = TRUE)

# Load raw data table and filter from start_time to stop_time
detection_df <- tbl(con, "raw") |> 
  filter(time >= start_time && time <= stop_time) |>
  collect()

# if you are working with blu data, uncomment the lines below and load data from the blu table
# detection_blu <- tbl(con, "blu") |>
#   filter(time >= start_time && time <= stop_time) |>
#   collect()


DBI::dbDisconnect(con)

# Get beeps from test tag only
detection_df <- subset.data.frame(detection_df, 
                                  tag_id == my_tag_id)

#### Load Sidekick calibration data ####
# Get Sidekick data from CSV
sidekick_files <- list.files("data/sidekick")
sidekick_all_df <- c()
for (file in sidekick_files) {
  sidekick_df <- load_sidekick_data(paste0("data/sidekick/", file))
  sidekick_all_df <- rbind(sidekick_all_df, sidekick_df)
}

# Remove odd positions 
summary(sidekick_all_df$lat)
summary(sidekick_all_df$lon)
sidekick_all_df <- sidekick_all_df[sidekick_all_df$lat> 60.85 
                                   & sidekick_all_df$lat< 61.05 
                                   & sidekick_all_df$lon< -137.9 
                                   & sidekick_all_df$lon> -138.1, ] # Adjust to suit your needs

# Get beeps from test tag only
sidekick_tag_df <- subset.data.frame(sidekick_all_df, 
                                     tag_id == my_tag_id)

# Show location of all beeps in relation to node locations
calibration_map <- map_calibration_track(node_locs, 
                                         sidekick_tag_df, 
                                         tile_url = my_tile_url)

calibration_map
mapshot(calibration_map, 
        file = "results/calibration_map.png")


#### Calculate the RSSI vs. Distance Relationship ####
# Correct the name of the 04BBDF57 station in the detection df
detection_df$node_id[detection_df$node_id == "4BBDF57"] <- "04BBDF57" 

# Filter the detection df so it only contains nodes that have a location
detection_df<- detection_df |> 
  filter(node_id %in% node_locs$node_id)


rssi_v_dist <- calc_rssi_v_dist(node_locs = node_locs, 
                                sidekick_tag_df = sidekick_tag_df, 
                                detection_df = detection_df, 
                                use_sync = FALSE) #For Blu Series tags use_sync=TRUE, for 434 MHz tags use_sync=FALSE.

# Plot the resulting RSSI and distance data
library(ggplot2)
ggplot() +
  geom_point(data = rssi_v_dist, 
             aes(x = distance, 
                 y = rssi, 
                 colour = node_id)) +
  labs(title="RSSI vs. Distance",
       x="Distance (m)",
       y="RSSI (dBm)",
       colour="Node ID") +
  classic_plot_theme()

# Fit the RSSI vs distance data with exponential relationship
a <- min(rssi_v_dist$rssi)                      # asymptotic weak-signal RSSI
b <- a - max(rssi_v_dist$rssi)                  # negative-ish, sets curve height
c <- 1 / mean(rssi_v_dist$distance)             # rough decay rate scaled to your distance units

nlsfit <- nls(
  rssi ~ a - b * exp(-c * distance),
  rssi_v_dist,
  start = list(a = a, b = b, c = c)
)

summary(nlsfit)

# Get the coefficients from the fit result
co <- coef(summary(nlsfit))
rssi_coefs <- c(co[1, 1], co[2, 1], co[3, 1])

# Add a predicted column to the RSSI vs distance data
rssi_v_dist$pred <- predict(nlsfit)

# Plot the RSSI vs distance data with the fit curve
calibration_plot <- plot_calibration_result(rssi_v_dist, classic_plot_theme())
calibration_plot

# Print the coefficients from the fit. You'll need these coefficients later for localization
print(rssi_coefs)
