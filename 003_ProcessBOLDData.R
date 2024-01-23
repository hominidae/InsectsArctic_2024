# Process data from public BOLD data
# These are public data from the BOLD search tool.
# You can select all of Canada by searching for "Canada"
# Or you can select individual provinces/territories to ensure the download doesn't fail.

# OBJECTIVE:
#  - Take public BOLD data and prepare it for comparison against own collected data

# Note:
#  This code uses ProcessBOLDPublicData available via github:
#  https://github.com/hominidae/ProcessBOLDPublicData
#  "canada_data.tsv" is the result of combining provincial and territorial barcodes in Canada
#  "nunavut_data.tsv" is also the result of that code, but we'll use that later in another script.

# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)

# Load the data set containing publicly available BOLD data, warning uses about 4.61GB of memory
# This data was stitched together from across Canada using code from my other ProcessBOLDPublicData code
# It involves downloading BOLD data from various provinces and stitching it all back together.
canada_data <- read_tsv("data/canada_data.tsv")

# Have a quick peek to see how easy this will be.
table(canada_data$province_state)
# Easy peasy. We just need to remove the isolated items from odd places.

# We will do that by selecting only the provinces we want. So, getting rid of US states and other odd matches.
canada_data_ab <- canada_data %>%
  filter(province_state == "Alberta")
canada_data_bc <- canada_data %>%
  filter(province_state == "British Columbia")
canada_data_mb <- canada_data %>%
  filter(province_state == "Manitoba")
canada_data_nb <- canada_data %>%
  filter(province_state == "New Brunswick")
canada_data_nf <- canada_data %>%
  filter(province_state == "Newfoundland and Labrador")
canada_data_nwt <- canada_data %>%
  filter(province_state == "Northwest Territories")
canada_data_ns <- canada_data %>%
  filter(province_state == "Nova Scotia")
canada_data_nt <- canada_data %>%
  filter(province_state == "Nunavut")
canada_data_on <- canada_data %>%
  filter(province_state == "Ontario")
canada_data_pei <- canada_data %>%
  filter(province_state == "Prince Edward Island")
canada_data_qc <- canada_data %>%
  filter(province_state == "Quebec")
canada_data_sk <- canada_data %>%
  filter(province_state == "Saskatchewan")
canada_data_yk <- canada_data %>%
  filter(province_state == "Yukon Territory")

# Great, we've got 'em isolated. Now put 'em all back together.
canada_data <- bind_rows(canada_data_ab,canada_data_bc,canada_data_mb,canada_data_nb,canada_data_nf,canada_data_nwt,canada_data_ns,canada_data_nt,canada_data_on,canada_data_pei,canada_data_qc,canada_data_sk,canada_data_yk)

# Garbage clean-up
rm(canada_data_ab,canada_data_bc,canada_data_mb,canada_data_nb,canada_data_nf,canada_data_nwt,canada_data_ns,canada_data_nt,canada_data_on,canada_data_pei,canada_data_qc,canada_data_sk,canada_data_yk)

# Let's check that we've effectively cleaned up our data.
table(canada_data$province_state)
# Right, the table now returns only valid provinces. We've effectively dropped anything that was out of place.

# Before we move on however, we need to trim some of this data. To do that, we'll use a select statement.
canada_truncated <- canada_data %>%
  select(processid,sampleid,recordID,catalognum,fieldnum,bin_uri,phylum_name,class_name,order_name,family_name,subfamily_name,genus_name,species_name,subspecies_name,identification_provided_by,identification_method,identification_reference,voucher_status,tissue_type,collectors,collection_note,site_code,sampling_protocol,lifestage,sex,reproduction,habitat,notes,lat,lon,coord_source,province_state,region,sector,exactsite,sequenceID,markercode,nucleotides,run_dates)

# You can check over the data, but let's just rename canada_truncated to canada_data and save it.
canada_data <- canada_truncated
rm(canada_truncated)

# Righteous. Clean data. Let's save it before it gets lost.
write_tsv(x = canada_data, "data/canada_data_clean.tsv")

# Reload here if necessary.
# canada_data <- read_tsv("data/canada_data_clean.tsv")

# Let's filter that data down to just arthropods
canada_data_arthropoda <- canada_data %>%
  filter(phylum_name == "Arthropoda")
# Let's remove canada_data since we don't need it any more.
rm(canada_data)

# Next, remove any NA's from lat/lon. Without lat/lon for collections it's kind of moot.
canada_data_arthropoda <- canada_data_arthropoda %>%
  drop_na(lat,lon)

# Also, let's remove any data without a bin_uri
canada_data_arthropoda <- canada_data_arthropoda %>%
  drop_na(bin_uri)

# Also, if we cannot determine what order the arthropoda fall under it's not very useful.
canada_data_arthropoda <- canada_data_arthropoda %>%
  drop_na(order_name)

# Save it as a clean slate.
write_tsv(x = canada_data_arthropoda, "data/canada_data_clean_arthropoda.tsv")

# Load in case it's needed.
# canada_data_arthropoda <- read_tsv("data/Canada_data_clean_arthropoda.tsv")

# This plot simply details the major arthropod orders and sorts them by province.
ggplot(canada_data_arthropoda, aes(y = province_state)) +
  geom_bar(aes(fill = order_name), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Specimens", y = "Province/Territory", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  scale_fill_viridis(discrete=TRUE) +
  geom_label(nudge_x = 30000, stat='count', aes(label=after_stat(count))) +
  ggtitle("Sequenced specimens in Public DNA Barcoding Data from Canada")

# Let's generate a plot detailing the number of collected specimens vs the number of unique BIN's detected.
# To do that, we need numbers. How many specimens are there per province?
# How many unique BIN's are there per province?

# First, copy the data to a temporary data frame, we'll use sampleid to sort later
uniquebins <- data.frame(
  bin_uri = canada_data_arthropoda$bin_uri,
  sampleid = canada_data_arthropoda$sampleid
)

# Unique bins by province, no NA's
uniquebins <- canada_data_arthropoda %>%
  select(sampleid,bin_uri,province_state,order_name) %>%
  drop_na() %>%
  group_by(province_state)

# Retain only unique bins
uniquebins <- uniquebins %>% distinct(bin_uri, .keep_all = TRUE)

# Make a plot with just the unique bins per province
ggplot(uniquebins, aes(y = province_state)) +
  geom_bar(aes(fill = order_name), position = position_stack(reverse = TRUE)) +
  labs(x = "# of unique BINs", y = "Province/Territory", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  scale_fill_viridis(discrete=TRUE) +
  geom_label(nudge_x = 1500, stat='count', aes(label=after_stat(count))) +
  ggtitle("Unique BINs in Public DNA Barcoding Data from Canada")

# Let's do some garbage collection
rm(canada_data_arthropoda,uniquebins)
