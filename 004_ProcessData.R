# Process own data from the Kitikmeot region of Nunavut

# Objective:
#  Characterize data collected in Kitikmeot region of Nunavut
#  Generate a map of the sampled locations for each community

# The data sets involved:
# BCHAR - Malaise Trap Samples 2018
#  Cambridge Bay, 4001 specimens
# CCHAR - IMA 2018
#  Cambridge Bay, 7295 specimens
# DCHAR - Water Lake Site 2018
#  Cambridge Bay, 8552 specimens
# FCHAR - General Terrestrial Collection 2019
#  Cambridge Bay, 2986 specimens
#  Kugluktuk, 4788 specimens
# GCHAR - Freshwater Aquatic 2019
# HCHAR - Marine Aquatic Collection 2019
# CBAY - 2021 Collection
#  Cambridge Bay, 66 specimens 
# KUGA - 2021 Collection
#  Kugaaruk, 1044 specimens
# GJOA - 2021 Collection
#  Gjoa Haven, 3708 specimens
# KUGL - Kugluktuk, Malaise and general collection 2021
#  Kugluktuk, 
# Update: Additional ARCBIO collections occurred in 2022
# As a result, we need to tally ARCBA, ARCBB, ARCBIOY5, and ARCBIOY6
# ARCBA - 2022 collection
# ARCBB - 
# ARCBIOY5 -
# ARCBIOY6 - 

# Load libraries ----
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(ggmap)
library(osmdata)
library(viridis)
library(phylotools)

# Load our working data set ----
workingdata <- read_tsv("data/kitikmeot_data.tsv")

# Before we remove NA's in the bin.uri column, let's look at what sequences we're losing
nonworkingdata <- (workingdata[is.na(workingdata$bin.uri),])

# Let's get a count of the order's represented in that data
# Reminder: These are data without an assigned BIN
out <- nonworkingdata %>%
  select(Class,Order) %>%
  count(Class,Order)
# Do some garbage collection
rm(out,nonworkingdata)

# Remove any NA's from bin.uri in workingdata
workingdata <- workingdata %>%
  drop_na(bin.uri)

# Clean up Kitikmeot data ----
# Remove any sector that does not conform to a few selected communities by renaming them.
# To do that, we'll replace any instance where "3km NW Cambridge Bay, Water Lake Site" as just "Cambridge Bay" instead
workingdata$Sector[workingdata$Sector == "3 km NW Cambridge Bay, Water Lake site"] <- "Cambridge Bay"

# Trust, but verify.
table(workingdata$Sector)
# There are quite a few more not within the communities:
# 100km ENE of Bathrust Inlet
# Bathurst Inlet
# Churchill Northern Studies Centre
# Icebreaker Channel
# North Shaler Mountains
# Queen Maud Gulf Bird Sanctuary
# Robinson
# Somerset Island
# Taloyoak
# Victoria Island
# Whitehorse

# I wonder if we took that and compared it to the public Nunavut BOLD data.
# Let's reload our Nunavut data from the previous script and see where that particular BIN is present. Is it all across the arctic?
canada_data_arthropoda <- read_tsv("data/canada_data_clean.tsv")

# Okay cool, let's move on. "Victoria Island" will need to be dropped. "North Shaler Mountains" will also need to be dropped too.
# Matter of fact, let's drop anything that doesn't match the named communities we've sampled in.
# Let's try that. To do that, we'll create by community then re-combine.
test <- workingdata %>%
  filter(Sector == "Cambridge Bay")
test1 <- workingdata %>%
  filter(Sector == "Gjoa Haven")
test2 <- workingdata %>%
  filter(Sector == "Kugaaruk")
test3 <- workingdata %>%
  filter(Sector == "Kugluktuk")
test4 <- workingdata %>%
  filter(Sector == "Taloyoak")
test5 <- rbind(test,test1,test2,test3,test4)
workingdata <- test5
rm(test,test1,test2,test3,test4,test5)
# So, we've eliminated specimens that are not from the places our target location(s) of Cambridge Bay, Kugluktuk, Gjoa Haven, Kugaaruk, and Taloyoak

# Let's filter that down even further to just Arthropoda. Doesn't do much because we already have, but just in case.
workingdata <- workingdata %>%
  filter(Phylum == "Arthropoda")

# Let's remove any duplicates from working data too.
# This is important because we're working with duplicate data from ARCBIO, which our specimens are part of.
workingdata <- workingdata %>%
  distinct(`Sample ID`, .keep_all = TRUE)

# Before we try plotting anything, let's rename "Collection Date" to something that actually works as a date.
names(workingdata)[names(workingdata) == "Collection Date"] <- "CollectionDate"

# We also need to change the CollectionDate column type from Character to Date
workingdata <- workingdata %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%d-%b-%Y"))

# Let's remove some NA's from Order
workingdata <- workingdata %>%
  drop_na(Order)

# Generate some graphs ----
# Let's create a graph showing what we've got by order. We'll remove NA's and narrow it down to arthropoda we're interested in the next few scripts.
ggplot(workingdata, aes(y = Sector)) +
  geom_bar(aes(fill = Order)) +
  labs(x = "# of Specimens", y = "Community", title = "Specimens prior to filtering out all aquatic invertebrates") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma, breaks=seq(0, 30000, by = 5000)) +
  scale_fill_viridis(discrete=TRUE) +
  geom_label(nudge_x = 750, stat='count', aes(label=after_stat(count)))

# Save our state
write_tsv(x = workingdata, "data/workingdata_2024_01_22.tsv")

# Right, within arthropoda, let's remove some of the aquatic invertabrate orders that we aren't interested in.
# What are they?
table(workingdata$Order)
# Here's a list of what we need to keep:
# Araneae - Spiders
# Coleoptera - Beetles
# Diptera - Flies
# Entomobryomorpha - Springtails
# Ephemeroptera - Mayflies
# Hemiptera - "true bugs"
# Hymenoptera - Sawflies, wasps, bees, and ants
# Lepidoptera - Butterflies and moths
# Mesostigmata - Mites, including parasitiformes
# Neuroptera - Net-winged insects
# Orthoptera - Grasshoppers, locusts, and crickets
# Plecoptera - Stoneflies
# Poduromorpha - Springtails
# Psocodea - Bark lice, book lice, parasitic lice
# Sarcoptiformes - Mites
# Siphonaptera - Fleas
# Symphypleona - Springtails
# Thysanoptera - Thrips
# Trichoptera - Aquatic larvae, terrestrial adults. Caddisflies, closely related to lepidoptera
# Trombidiformes - Mites

# Let's use a select statement for the values we want to keep.
workingdata <- workingdata %>%
  filter(Order %in% c("Araneae", "Coleoptera", "Diptera", "Entomobryomorpha", "Ephemeroptera",
                      "Hemiptera", "Hymenoptera", "Lepidoptera", "Mesostigmata", "Neuroptera",
                      "Orthoptera", "Plecoptera", "Poduromorpha", "Psocodea", "Sarcoptiformes",
                      "Siphonaptera", "Siphonaptera", "Symphypleona", "Thysanoptera", "Trichoptera",
                      "Trombidiformes"))

# Now, let's do what we really came here for. We're going to generate some site collection maps from data I was involved in collecting samples for.
cambridgebay <- workingdata %>%
  filter(Sector == "Cambridge Bay")
kugluktuk <- workingdata %>%
  filter(Sector == "Kugluktuk")
gjoahaven <- workingdata %>%
  filter(Sector == "Gjoa Haven")
kugaaruk <- workingdata %>%
  filter(Sector == "Kugaaruk")
taloyoak <- workingdata %>%
  filter(Sector == "Taloyoak")

# Before we move on, we need to remove duplicates. This is because we added duplicate data with the Arcbio data.
# We use distinct() to do that.
cambridgebay <- cambridgebay %>%
  distinct(`Sample ID`, .keep_all = TRUE)
kugluktuk <- kugluktuk %>%
  distinct(`Sample ID`, .keep_all = TRUE)
kugaaruk <- kugaaruk %>%
  distinct(`Sample ID`, .keep_all = TRUE)
gjoahaven <- gjoahaven %>%
  distinct(`Sample ID`, .keep_all = TRUE)
taloyoak <- taloyoak %>%
  distinct(`Sample ID`, .keep_all = TRUE)

# Let's summarize:
# Cambridge Bay - 26639
# Kugluktuk - 10845
# Gjoa Haven - 10160
# Kugaaruk - 7024
# Taloyoak -4102
# All duplicates removed.

# Let's perform a save of what we've done so far
write_tsv(x = cambridgebay, "data/cambridgebay_2024_01_22.tsv")
write_tsv(x = kugluktuk, "data/kugluktuk_2024_01_22.tsv")
write_tsv(x = gjoahaven, "data/gjoahaven_2024_01_22.tsv")
write_tsv(x = kugaaruk, "data/kugaaruk_2024_01_22.tsv")
write_tsv(x = taloyoak, "data/taloyoak_2024_01_22.tsv")

# We also need to save the new kitikmeot data
# To do that, we'll need to combine the communities again
new_workingdata <- rbind(cambridgebay,kugluktuk,gjoahaven,kugaaruk,taloyoak)
write_tsv(x = new_workingdata, "data/workingdata_2024_02_05.tsv")

# Let's generate some stats, percentage-wise what are the most represented orders?
tabulated <- table(new_workingdata$Order)
tabulated <- data.frame(tabulated)

# Let's do a little mapping. We want a figure to represent our sampling sites in Cambridge Bay.
#register_google(key = "YOURKEYHERE")

# Let's start with mapping Cambridge Bay specimens.
cbay_map <- get_map(
  location = c(-105.060,69.116),
  scale = "auto",
  zoom = 9,
  source = "google",
  force = TRUE)

# Draw the map. See if it lines up where we want.
mp <- ggmap(cbay_map)

# This looks perfect. Let's plot out our points.
cbaymap <- mp +
  geom_point(data = cambridgebay,
             aes(x = Lon, y = Lat,
                 color=Order))
cbaymap

# Map Kugluktuk
# 67.825433738975, -115.0979912275245
kugl_map <- get_map(
  location = c(-115.0979,67.8254),
  scale = "auto",
  zoom = 9,
  source = "google",
  force = TRUE)
mp <- ggmap(kugl_map)
kuglmap <- mp +
  geom_point(data = kugluktuk,
             aes(x = Lon, y = Lat,
                 color=Order))
kuglmap

# Map Gjoa Haven
# 68.63577789587174, -95.85026693927082
gjoa_map <- get_map(
  location = c(-95.8502,68.6357),
  scale = "auto",
  zoom = 9,
  source = "google",
  force = TRUE)
mp <- ggmap(gjoa_map)
gjoamap <- mp +
  geom_point(data = gjoahaven,
             aes(x = Lon, y = Lat,
                 color=Order))
gjoamap

# Map Kugaaruk
# 68.53463953708601, -89.82512154873845
kuga_map <- get_map(
  location = c(-89.8251,68.5346),
  scale = "auto",
  zoom = 9,
  source = "google",
  force = TRUE)
mp <- ggmap(kuga_map)
kugamap <- mp +
  geom_point(data = kugaaruk,
             aes(x = Lon, y = Lat,
                 color=Order))
kugamap

# Last, let's do Taloyoak
talo_map <- get_map(
  location = c(-93.5354,69.5380),
  scale = "auto",
  zoom = 9,
  source = "google",
  force = TRUE)
mp <- ggmap(talo_map)
talomap <- mp +
  geom_point(data = taloyoak,
             aes(x = Lon, y = Lat,
                 color=Order))
talomap
# That's it for now, we'll return to this mapping later once we've done more processing.

# Let's do something fun. Let's use the alluvial package to plot some things.
# Namely, arrange the year along the X Axis, with species arranged by family in the Y Axis

# First though, we need a BIN_URI count
workingdata_boldcount <- workingdata %>%
  select(Sector,bin.uri,Class,Order,Family,Species,CollectionDate) %>%
  group_by(CollectionDate,bin.uri,Sector,Class,Order,Family,Species) %>%
  summarise(Freq = n()) %>%
  arrange(-Freq)

# Let's do something interesting and break 'CollectionDate' into years, months, days with the Lubridate package
library(lubridate)

# Convert into separate columns with mutate
workingdata_boldcount_dates <- workingdata_boldcount %>%
  dplyr::mutate(year = lubridate::year(CollectionDate), 
                month = lubridate::month(CollectionDate, label=TRUE),
                day = lubridate::day(CollectionDate))

# Here's the code to do that
# Load library
library(ggalluvial)

# Generate the plot. Warning, takes a long time.
ggplot(data = workingdata_boldcount_dates,
       aes(axis1 = Order, axis2 = Sector,
           y = Freq)) +
  scale_x_discrete(limits = c("Year","Community","Order"), expand = c(.2, .05)) +
  xlab("Phylogeny") +
  geom_alluvium(aes(fill = Order)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Test plot")

# Garbage collection
rm(cbay_map,kugl_map,gjoa_map,kuga_map,talo_map)
rm(cbaymap,gjoamap,kugamap,kuglmap,talomap)
rm(cambridgebay,kugluktuk,gjoahaven,kugaaruk,taloyoak)
rm(clean_syrphidae,diptera,syrphidae)
rm(canada_data_arthropoda,mp,workingdata,workingdata_boldcount,workingdata_boldcount_dates)
