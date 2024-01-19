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
ARCBIOY51 <- read_tsv("data/ARCBIOY5/collection_data.tsv")
ARCBIOY55 <- read_tsv("data/ARCBIOY5/taxonomy.tsv")
ARCBIOY57 <- read_tsv("data/ARCBIOY5/ARCBIOY5-sequencedata.tsv")
# ARCBIOY6 - Arctic BIOSCAN Year 6 (2023)
ARCBIOY61 <- read_tsv("data/ARCBIOY6/collection_data.tsv")
ARCBIOY65 <- read_tsv("data/ARCBIOY6/taxonomy.tsv")
ARCBIOY67 <- read_tsv("data/ARCBIOY6/ARCBIOY6-sequencedata.tsv")

# Let's separate out all arthropods.
bchar_arth <- bchar5 %>%
  filter(Phylum == "Arthropoda")
cchar_arth <- cchar5 %>%
  filter(Phylum == "Arthropoda")
dchar_arth <- dchar5 %>%
  filter(Phylum == "Arthropoda")
fchar_arth <- fchar5 %>%
  filter(Phylum == "Arthropoda")
gchar_arth <- gchar5 %>%
  filter(Phylum == "Arthropoda")
hchar_arth <- hchar5 %>%
  filter(Phylum == "Arthropoda")
cbay_arth <- cbay5 %>%
  filter(Phylum == "Arthropoda")
kuga_arth <- kuga5 %>%
  filter(Phylum == "Arthropoda")
gjoa_arth <- gjoa5 %>%
  filter(Phylum == "Arthropoda")
kugl_arth <- kugl5 %>%
  filter(Phylum == "Arthropoda")
arcba_arth <- arcba5 %>%
  filter(Phylum == "Arthropoda")
arcbb_arth <- arcbb5 %>%
  filter(Phylum == "Arthropoda")
ARCBIOY5_arth <- ARCBIOY55 %>%
  filter(Phylum == "Arthropoda")
ARCBIOY6_arth <- ARCBIOY65 %>%
  filter(Phylum == "Arthropoda")

# Rename/Join columns ####
########## BCHAR
names(bchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(bchar5, bchar7, by="Sample ID")
test1 <- inner_join(test, bchar1, by="Sample ID")
temp <- data.frame(bchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

bchar_final <- test1
bchar_arth <- test2
rm(temp,test,test1,test2)
rm(bchar1,bchar5,bchar7)

########## CCHAR
names(cchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cchar5, cchar7, by="Sample ID")
test1 <- inner_join(test, cchar1, by="Sample ID")
temp <- data.frame(cchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cchar_final <- test1
cchar_arth <- test2
rm(temp,test,test1,test2)
rm(cchar1,cchar5,cchar7)

########## DCHAR
names(dchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(dchar5, dchar7, by="Sample ID")
test1 <- inner_join(test, dchar1, by="Sample ID")
temp <- data.frame(dchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

dchar_final <- test1
dchar_arth <- test2
rm(temp,test,test1,test2)
rm(dchar1,dchar5,dchar7)

########## FCHAR
names(fchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(fchar5, fchar7, by="Sample ID")
test1 <- inner_join(test, fchar1, by="Sample ID")
temp <- data.frame(fchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

fchar_final <- test1
fchar_arth <- test2
rm(temp,test,test1,test2)
rm(fchar1,fchar5,fchar7)

########## GCHAR
names(gchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gchar5, gchar7, by="Sample ID")
test1 <- inner_join(test, gchar1, by="Sample ID")
temp <- data.frame(gchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

gchar_final <- test1
gchar_arth <- test2
rm(temp,test,test1,test2)
rm(gchar1,gchar5,gchar7)

########## HCHAR
names(hchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(hchar5, hchar7, by="Sample ID")
test1 <- inner_join(test, hchar1, by="Sample ID")
temp <- data.frame(hchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

hchar_final <- test1
hchar_arth <- test2
rm(temp,test,test1,test2)
rm(hchar1,hchar5,hchar7)

########## CBAY
names(cbay7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cbay5, cbay7, by="Sample ID")
test1 <- inner_join(test, cbay1, by="Sample ID")
temp <- data.frame(cbay_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cbay_final <- test1
cbay_arth <- test2
rm(temp,test,test1,test2)
rm(cbay1,cbay5,cbay7)

########## KUGA
names(kuga7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kuga5, kuga7, by="Sample ID")
test1 <- inner_join(test, kuga1, by="Sample ID")
temp <- data.frame(kuga_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

kuga_final <- test1
kuga_arth <- test2
rm(temp,test,test1,test2)
rm(kuga1,kuga5,kuga7)

########## GJOA
names(gjoa7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gjoa5, gjoa7, by="Sample ID")
test1 <- inner_join(test, gjoa1, by="Sample ID")
temp <- data.frame(gjoa_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

gjoa_final <- test1
gjoa_arth <- test2
rm(temp,test,test1,test2)
rm(gjoa1,gjoa5,gjoa7)

########## KUGL
names(kugl7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kugl5, kugl7, by="Sample ID")
test1 <- inner_join(test, kugl1, by="Sample ID")
temp <- data.frame(kugl_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

kugl_final <- test1
kugl_arth <- test2
rm(temp,test,test1,test2)
rm(kugl1,kugl5,kugl7)

########## ARCBA
names(arcba7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(arcba5, arcba7, by="Sample ID")
test1 <- inner_join(test, arcba1, by="Sample ID")
temp <- data.frame(arcba_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

arcba_final <- test1
arcba_arth <- test2
rm(temp,test,test1,test2)
rm(arcba1,arcba5,arcba7)

########## ARCBA
names(arcbb7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(arcbb5, arcbb7, by="Sample ID")
test1 <- inner_join(test, arcbb1, by="Sample ID")
temp <- data.frame(arcbb_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

arcbb_final <- test1
arcbb_arth <- test2
rm(temp,test,test1,test2)
rm(arcbb1,arcbb5,arcbb7)

########## ARCBIOY5
names(ARCBIOY57) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(ARCBIOY55, ARCBIOY57, by="Sample ID")
test1 <- inner_join(test, ARCBIOY51, by="Sample ID")
temp <- data.frame(ARCBIOY5_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

ARCBIOY5_final <- test1
ARCBIOY5_arth <- test2
rm(temp,test,test1,test2)
rm(ARCBIOY51,ARCBIOY55,ARCBIOY57)

########## ARCBIOY6
names(ARCBIOY67) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(ARCBIOY65, ARCBIOY67, by="Sample ID")
test1 <- inner_join(test, ARCBIOY61, by="Sample ID")
temp <- data.frame(ARCBIOY6_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

ARCBIOY6_final <- test1
ARCBIOY6_arth <- test2
rm(temp,test,test1,test2)
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

# Combine arthropods together
test <- rbind(bchar_arth,cchar_arth)
test1 <- rbind(test,dchar_arth)
test2 <- rbind(test1,fchar_arth)
test3 <- rbind(test2,gchar_arth)
test4 <- rbind(test3,hchar_arth)
test5 <- rbind(test4,cbay_arth)
test6 <- rbind(test5,kuga_final)
test7 <- rbind(test6,gjoa_arth)
test8 <- rbind(test7,kugl_arth)
test9 <- rbind(test8,arcba_arth)
test10 <-rbind(test9,arcbb_arth)
test11 <-rbind(test10,ARCBIOY5_arth)
test12 <-rbind(test11,ARCBIOY6_arth)
final_arth <- test12
rm(test,test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12)

# Write out our tsv file
write_tsv(x = final_final, "data/kitikmeot_data.tsv")
rm(bchar_final,cchar_final,dchar_final,fchar_final,gchar_final,hchar_final,cbay_final,kuga_final,gjoa_final,kugl_final,arcba_final,arcbb_final,ARCBIOY5_final,ARCBIOY6_final,final_final)

# Write out another tsv containing just the arthropods
write_tsv(x = final_arth, "data/kitikmeot_data_arth.tsv")
rm(bchar_arth,cchar_arth,dchar_arth,fchar_arth,gchar_arth,hchar_arth,cbay_arth,kuga_arth,gjoa_arth,kugl_arth,arcba_arth,arcbb_arth,ARCBIOY5_arth,ARCBIOY6_arth,final_arth)
