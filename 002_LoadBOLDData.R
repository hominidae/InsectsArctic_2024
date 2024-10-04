# Process data from DNA Barcoding collection efforts in the Kitikmeot region of Nunavut
# This script takes the sequencing data from 001_LoadSequencingData.R script and combines it
# with the associated taxonomy data and collection data.
# These are matched together by the 'Sample ID' value.

# OBJECTIVE:
#  - Recombine multiple tsv files into singular tsv files for analysis

# Load Libraries ####
library(tidyverse)
library(dplyr)

# Load Data ####
# BCHAR Biodiversity Survey - Malaise Trap Samples 2018
bchar1 <- read_tsv("data/BCHAR/collection_data.tsv")
bchar5 <- read_tsv("data/BCHAR/taxonomy.tsv")
bchar7 <- read_tsv("data/BCHAR/bchar-sequencedata.tsv")
# CCHAR Biodiversity Survey - Standardized Sampling - Intensive Monitoring Area Site 2018
cchar1 <- read_tsv("data/CCHAR/collection_data.tsv")
cchar5 <- read_tsv("data/CCHAR/taxonomy.tsv")
cchar7 <- read_tsv("data/CCHAR/cchar-sequencedata.tsv")
# DCHAR Biodiversity Survey - Standardized Sampling - Water Lake Site 2018
dchar1 <- read_tsv("data/DCHAR/collection_data.tsv")
dchar5 <- read_tsv("data/DCHAR/taxonomy.tsv")
dchar7 <- read_tsv("data/DCHAR/dchar-sequencedata.tsv")
# FCHAR Biodiversity Survey 2019 - General Terrestrial Collection (Cambridge Bay & Kugluktuk)
fchar1 <- read_tsv("data/FCHAR/collection_data.tsv")
fchar5 <- read_tsv("data/FCHAR/taxonomy.tsv")
fchar7 <- read_tsv("data/FCHAR/fchar-sequencedata.tsv")
# GCHAR Biodiversity Survey 2019 - Freshwater Aquatic Collection
gchar1 <- read_tsv("data/GCHAR/collection_data.tsv")
gchar5 <- read_tsv("data/GCHAR/taxonomy.tsv")
gchar7 <- read_tsv("data/GCHAR/gchar-sequencedata.tsv")
# HCHAR Biodiversity Survey 2019 - Marine Aquatic Collection 2019 (Likely not the target, but will need to see)
hchar1 <- read_tsv("data/HCHAR/collection_data.tsv")
hchar5 <- read_tsv("data/HCHAR/taxonomy.tsv")
hchar7 <- read_tsv("data/HCHAR/hchar-sequencedata.tsv")
# CBAY ARCBIO 2021 - Cambridge Bay, Nunavut - Malaise and general collection
cbay1 <- read_tsv("data/CBAY/collection_data.tsv")
cbay5 <- read_tsv("data/CBAY/taxonomy.tsv")
cbay7 <- read_tsv("data/CBAY/cbay-sequencedata.tsv")
# KUGA ARCBIO 2021 - Kugaaruk, Nunavut - Malaise and general collection
kuga1 <- read_tsv("data/KUGA/collection_data.tsv")
kuga5 <- read_tsv("data/KUGA/taxonomy.tsv")
kuga7 <- read_tsv("data/KUGA/kuga-sequencedata.tsv")
# GJOA ARCBIO 2021 - Gjoa Haven, Nunavut - Malaise and general collection
gjoa1 <- read_tsv("data/GJOA/collection_data.tsv")
gjoa5 <- read_tsv("data/GJOA/taxonomy.tsv")
gjoa7 <- read_tsv("data/GJOA/gjoa-sequencedata.tsv")
# KUGL ARCBIO 2021 - Malaise and general collection 2021
kugl1 <- read_tsv("data/KUGL/collection_data.tsv")
kugl5 <- read_tsv("data/KUGL/taxonomy.tsv")
kugl7 <- read_tsv("data/KUGL/kugl-sequencedata.tsv")
# ARCBA 2018 - Biodiversity Survey 2018
arcba1 <- read_tsv("data/ARCBA/collection_data.tsv")
arcba5 <- read_tsv("data/ARCBA/taxonomy.tsv")
arcba7 <- read_tsv("data/ARCBA/arcba-sequencedata.tsv")
# ARCBB 2018 - Biodiversity Survey 2018
arcbb1 <- read_tsv("data/ARCBB/collection_data.tsv")
arcbb5 <- read_tsv("data/ARCBB/taxonomy.tsv")
arcbb7 <- read_tsv("data/ARCBB/arcbb-sequencedata.tsv")
# ARCBIOY5 - Arctic BIOSCAN Year 5 (2022)
ARCBIOY51 <- read_tsv("data/DS-ARCBIOY5/collection_data.tsv")
ARCBIOY55 <- read_tsv("data/DS-ARCBIOY5/taxonomy.tsv")
ARCBIOY57 <- read_tsv("data/DS-ARCBIOY5/ARCBIOY5-sequencedata.tsv")
# ARCBIOY6 - Arctic BIOSCAN Year 6 (2023)
ARCBIOY61 <- read_tsv("data/DS-ARCBIOY6/collection_data.tsv")
ARCBIOY65 <- read_tsv("data/DS-ARCBIOY6/taxonomy.tsv")
ARCBIOY67 <- read_tsv("data/DS-ARCBIOY6/ARCBIOY6-sequencedata.tsv")

# Rename/Join columns ####
########## BCHAR
names(bchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(bchar5, bchar7, by="Sample ID")
bchar_final <- inner_join(test, bchar1, by="Sample ID")
rm(test)
rm(bchar1,bchar5,bchar7)

########## CCHAR
names(cchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cchar5, cchar7, by="Sample ID")
cchar_final <- inner_join(test, cchar1, by="Sample ID")
rm(test)
rm(cchar1,cchar5,cchar7)

########## DCHAR
names(dchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(dchar5, dchar7, by="Sample ID")
dchar_final <- inner_join(test, dchar1, by="Sample ID")
rm(test)
rm(dchar1,dchar5,dchar7)

########## FCHAR
names(fchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(fchar5, fchar7, by="Sample ID")
fchar_final <- inner_join(test, fchar1, by="Sample ID")
rm(test)
rm(fchar1,fchar5,fchar7)

########## GCHAR
names(gchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gchar5, gchar7, by="Sample ID")
gchar_final <- inner_join(test, gchar1, by="Sample ID")
rm(test)
rm(gchar1,gchar5,gchar7)

########## HCHAR
names(hchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(hchar5, hchar7, by="Sample ID")
hchar_final <- inner_join(test, hchar1, by="Sample ID")
rm(test)
rm(hchar1,hchar5,hchar7)

########## CBAY
names(cbay7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cbay5, cbay7, by="Sample ID")
cbay_final <- inner_join(test, cbay1, by="Sample ID")
rm(test)
rm(cbay1,cbay5,cbay7)

########## KUGA
names(kuga7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kuga5, kuga7, by="Sample ID")
kuga_final <- inner_join(test, kuga1, by="Sample ID")
rm(test)
rm(kuga1,kuga5,kuga7)

########## GJOA
names(gjoa7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gjoa5, gjoa7, by="Sample ID")
gjoa_final <- inner_join(test, gjoa1, by="Sample ID")
rm(test)
rm(gjoa1,gjoa5,gjoa7)

########## KUGL
names(kugl7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kugl5, kugl7, by="Sample ID")
kugl_final <- inner_join(test, kugl1, by="Sample ID")
rm(test)
rm(kugl1,kugl5,kugl7)

########## ARCBA
names(arcba7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(arcba5, arcba7, by="Sample ID")
arcba_final <- inner_join(test, arcba1, by="Sample ID")
rm(test)
rm(arcba1,arcba5,arcba7)

########## ARCBB
names(arcbb7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(arcbb5, arcbb7, by="Sample ID")
arcbb_final <- inner_join(test, arcbb1, by="Sample ID")
rm(test)
rm(arcbb1,arcbb5,arcbb7)

########## ARCBIOY5
names(ARCBIOY57) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(ARCBIOY55, ARCBIOY57, by="Sample ID")
ARCBIOY5_final <- inner_join(test, ARCBIOY51, by="Sample ID")
rm(test)
rm(ARCBIOY51,ARCBIOY55,ARCBIOY57)

########## ARCBIOY6
names(ARCBIOY67) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(ARCBIOY65, ARCBIOY67, by="Sample ID")
ARCBIOY6_final <- inner_join(test, ARCBIOY61, by="Sample ID")
rm(test)
rm(ARCBIOY61,ARCBIOY65,ARCBIOY67)

# Combine all the finals together ####
test <- rbind(bchar_final,cchar_final)
test1 <- rbind(test,dchar_final)
test2 <- rbind(test1,fchar_final)
test3 <- rbind(test2,gchar_final)
test4 <- rbind(test3,hchar_final)
test5 <- rbind(test4,cbay_final)
test6 <- rbind(test5,kuga_final)
test7 <- rbind(test6,gjoa_final)
test8 <- rbind(test7,kugl_final)
test9 <- rbind(test8,arcba_final)
test10 <- rbind(test9,arcbb_final)
test11 <- rbind(test10,ARCBIOY5_final)
test12 <- rbind(test11,ARCBIOY6_final)
final_final <- test12
rm(test,test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12)

# Write out our tsv file
write_tsv(x = final_final, "data/kitikmeot_data.tsv")
rm(bchar_final,cchar_final,dchar_final,fchar_final,gchar_final,hchar_final,cbay_final,kuga_final,gjoa_final,kugl_final,arcba_final,arcbb_final,ARCBIOY5_final,ARCBIOY6_final,final_final)

# Load the data again here we are going to generate a before analysis graph or two ####
kitikmeot_data <- read_tsv("data/kitikmeot_data.tsv")

# Break the years out into their own fields
#	Format: 15-Aug-2018
# dd-mmm-yyyy
library(lubridate)

# Example
#test_cbay <- cbay %>%
#  mutate(year = year(cbay$date),
#         month = month(cbay$date),
#         month = month.abb[month],
#         maxtemp = cbay$maxtemp,
#         mintemp = cbay$mintemp) %>%
#  select(year,month,maxtemp,mintemp) %>%
#  mutate(month = factor(month, levels = month.abb))

# Using dplyr
#df2 <- df %>% select(-c(id, name, chapters))

# First, let's convert the date format into something usable in their own columns
kitikmeot_ymd <- kitikmeot_data %>%
  mutate(yearmonthday = dmy(kitikmeot_data$`Collection Date`),
         year = year(yearmonthday),
         month = month(yearmonthday),
         day = day(yearmonthday)
  ) %>%
  select(-c(yearmonthday))
# Next, let's start selecting by years
kitikmeot_2018 <- kitikmeot_ymd %>%
  filter(year == "2018")
# Let's do 2019
kitikmeot_2019 <- kitikmeot_ymd %>%
  filter(year == "2019")
# Do 2021, there were no collections in 2020 due to COVID
kitikmeot_2021 <- kitikmeot_ymd %>%
  filter(year == "2021")
# Do 2022
kitikmeot_2022 <- kitikmeot_ymd %>%
  filter(year == "2022")
# Do 2023
kitikmeot_2023 <- kitikmeot_ymd %>%
  filter(year == "2023")
