

## Notes ----
# Randy Swaty
# July 31, 2025
# try to streamline LF data processing
# GIS plus s-class chart



## Dependancies ----

# load libraries

library(foreign)
library(rlandfire)
library(scales)
library(sf)
library(terra)
library(tidyverse)
library(tools)
library(varhandle)

#  read shape
shp <- st_read("inputs/st_croix.shp") %>% # small tester area in MN
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

vect(shp)
plot(shp)

### CONUS attribute tables ----

# BpS
bps_url <- "https://landfire.gov/sites/default/files/CSV/LF2016/LF16_BPS.csv" # will likely get warning, but it's OK
bps_atts_conus <- read.csv(bps_url)

# View the first few rows
head(bps_atts_conus)

# EVT

evt_url <- "https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVT.csv" # will likely get warning, but it's OK

evt_atts_conus <- read.csv(evt_url)

# SCLASS

scls_url <- "https://landfire.gov/sites/default/files/CSV/LF2023/LF23_SCla_240.csv" # will likely get warning, but it's OK

scls_atts_conus <- read.csv(scls_url)

# Ref Con-long version created by Randy Swaty and BpS model name
ref_con <- read_csv("inputs/ref_con_long.csv")
bps_names <- read_csv("inputs/bps_model_number_name.csv")




## Get LANDFIRE data ----

aoi <- getAOI(shp)
products <- c("200BPS", "240EVT", "240EVC", "240EVH", "240SCLASS")
projection <- 5070
resolution <- 30
email <- "rswaty@tnc.org"

save_file <- tempfile(fileext = ".zip")
ncal <- landfireAPIv2(products, aoi, projection, resolution, path = save_file, email = email)

# Define the destination path
dest_file <- file.path("inputs", "landfire_data.zip")

# Move and rename the file
file.rename(save_file, dest_file)

# Create a temporary directory for unzipping
temp_dir <- tempfile()
dir.create(temp_dir)

# Unzip the file into the temporary directory
unzip(dest_file, exdir = temp_dir)

# Get the list of unzipped files
unzipped_files <- list.files(temp_dir, full.names = TRUE)

# Rename each unzipped file to "landfire_data" with its full original extension
for (file in unzipped_files) {
  file_name <- basename(file)
  file_extension <- sub("^[^.]*", "", file_name)  # Extract the full extension
  new_file_path <- file.path("inputs", paste0("landfire_data", file_extension))
  file.rename(file, new_file_path)
}

# Clean up the temporary directory
unlink(temp_dir, recursive = TRUE)


## Crop, mask and split rasters ----

# load in landfire stacked raster
stacked_rasters <- rast("inputs/landfire_data.tif")

# crop and mask stacked raster 
aoi_stacked_rasters <- stacked_rasters %>%
  crop(shp) %>%
  mask(shp)

# "split" cropped and masked stacked raster into separate layers
for(lyr in names(aoi_stacked_rasters)) assign(lyr, aoi_stacked_rasters[[lyr]])


## BpS attribute table ----

# Assign categories
levels(US_200BPS) <- bps_atts_conus
activeCat(US_200BPS) <- "VALUE"

# Get frequency table 
bps_freq <- freq(US_200BPS) %>%
  as.data.frame()

# Join with attributes and calculate acres and percent
bps_aoi_atts <- bps_freq %>%
  rename(VALUE = value, COUNT = count) %>%
  mutate(VALUE = as.integer(VALUE)) %>%
  left_join(bps_atts_conus, by = "VALUE") %>%
  filter(COUNT != 0) %>%
  mutate(
    ACRES = round((COUNT * 900 / 4046.86), 0),
    REL_PERCENT = round((COUNT / sum(COUNT)) * 100, 3)
  ) %>%
  arrange(desc(REL_PERCENT)) %>%
  select(-layer)  # Optional: remove 'layer' column


## EVT attribute table ----

# Assign categories
levels(US_240EVT) <- evt_atts_conus
activeCat(US_240EVT) <- "VALUE"

# Get frequency table 
evt_freq <- freq(US_240EVT) %>%
  as.data.frame()

# Join with attributes and calculate acres and percent
evt_aoi_atts <- evt_freq %>%
  rename(VALUE = value, COUNT = count) %>%
  mutate(VALUE = as.integer(VALUE)) %>%
  left_join(evt_atts_conus, by = "VALUE") %>%
  filter(COUNT != 0) %>%
  mutate(
    ACRES = round((COUNT * 900 / 4046.86), 0),
    REL_PERCENT = round((COUNT / sum(COUNT)) * 100, 3)
  ) %>%
  arrange(desc(REL_PERCENT)) %>%
  select(-layer)  # Optional: remove 'layer' column


## SCLASS attribute Table ----

levels(US_240SCLASS) <- scls_atts_conus
activeCat(US_240SCLASS) <- "VALUE"

# Get frequency table 
scls_freq <- freq(US_240SCLASS) %>%
  as.data.frame()

# Join with attributes and calculate acres and percent
scls_aoi_atts <- scls_freq %>%
  rename(VALUE = value, COUNT = count) %>%
  mutate(VALUE = as.integer(VALUE)) %>%
  left_join(scls_atts_conus, by = "VALUE") %>%
  filter(COUNT != 0) %>%
  mutate(
    ACRES = round((COUNT * 900 / 4046.86), 0),
    REL_PERCENT = round((COUNT / sum(COUNT)) * 100, 3)
  ) %>%
  arrange(desc(REL_PERCENT)) %>%
  select(-layer)  # Optional: remove 'layer' column



## Get Reference and Current Conditions per BpS_Model


# Filter reference conditions to AOI BpS models
aoi_bps_models <- unique(bps_aoi_atts$BPS_MODEL)
aoi_ref_con <- filter(ref_con, model_code %in% aoi_bps_models)

# Create data frame of raster values
bps_vals <- values(US_200BPS)
scls_vals <- values(US_240SCLASS)

raster_df <- data.frame(
  bps = as.factor(bps_vals),
  scls = as.factor(scls_vals)
)

# Stack rasters and extract values
library(terra)
library(dplyr)

# Assuming these are already loaded

# Rename layers to avoid duplicate or ambiguous names
names(US_200BPS) <- "bps"
names(US_240SCLASS) <- "scls"

# Stack and extract values
stacked <- c(US_200BPS, US_240SCLASS)
vals <- values(stacked)

# Convert to data frame and count combinations
combo_freq <- as.data.frame(vals) %>%
  filter(!is.na(bps) & !is.na(scls)) %>%
  count(bps, scls, name = "Freq")



# Convert factors to characters
combo_freq <- combo_freq %>%
  mutate(across(c(bps, scls), as.character))

# Convert VALUE columns to character before joining
scls_aoi_atts <- scls_aoi_atts %>%
  mutate(VALUE = as.character(VALUE))

bps_aoi_atts <- bps_atts_conus %>%
  mutate(VALUE = as.character(VALUE))

# Now perform the join
combo_labeled <- combo_freq %>%
  left_join(select(scls_aoi_atts, VALUE, LABEL), by = c("scls" = "VALUE")) %>%
  left_join(select(bps_aoi_atts, VALUE, BPS_MODEL, BPS_NAME ), by = c("bps" = "VALUE"))



# Calculate current percent cover per BpS
combo_percent <- combo_labeled %>%
  group_by(BPS_MODEL) %>%
  mutate(total = sum(Freq),
         current_percent = round((Freq / total) * 100)) %>%
  unite("model_label", BPS_MODEL, LABEL, remove = FALSE)

# Join with reference conditions
bps_scls_combined <- left_join(combo_percent, ref_con, by = "model_label")

# Ensure all reference combinations are included and clean a bit
bps_scls_full <- left_join(aoi_ref_con, combo_percent, by = "model_label") %>%
  mutate(across(c(Freq, current_percent, ref_percent), ~replace_na(.x, 0))) %>%
  select(-c(LABEL, BPS_MODEL, BPS_NAME))


# Export result
write.csv(bps_scls_full, file = 'outputs/bps_scls_full.csv')


## Make Faceted S-Class Chart ----


bps_scls_long <- bps_scls_full %>%
  group_by(model_code) %>%
  mutate(total_bps_count = sum(Freq)) %>%
  ungroup() %>%
  # dplyr::filter(bps_name %in% c("Laurentian-Acadian Northern Pine(-Oak) Forest - Pine Dominated",
  #                               "Laurentian Pine-Oak Barrens - Jack Pine")) %>%
  
  dplyr::select(c("model_code", "bps_name", "ref_label",  "current_percent", "ref_percent")) %>%
  pivot_longer(
    cols = c(`ref_percent`, `current_percent`), 
    names_to = "ref_cur", 
    values_to = "percent") %>%
  filter(ref_label != "Water") # water



# order classes

bps_scls_long$ref_label <- factor(bps_scls_long$ref_label, levels= c(
  "Developed",
  "Agriculture",
  "UE",
  "UN",
  "E",
  "D",
  "C",
  "B",
  "A"))


sclasplot <-
  ggplot(bps_scls_long, aes(fill=factor(ref_cur), y=percent, x=ref_label)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  #facet_grid(. ~BpS) +
  scale_x_discrete(limits = (levels(bps_scls_long$ref_label))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "X BpSs selected for illustration. Not all succession classes present in all BpSs",
    caption = "\nData from landfire.gov.",
    x = "",
    y = "Percent")+
  theme_minimal(base_size = 12)+
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  facet_wrap(~model_code, ) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        strip.background = element_rect(color = "black", size = 1))

sclasplot







