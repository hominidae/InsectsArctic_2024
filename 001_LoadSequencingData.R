# Process sequencing data from collection efforts in communities in the Kitikmeot region of Nunavut
# These data were downloaded from the BOLD systems data console with Sample ID included to match
# up against the other data files.

# OBJECTIVE:
#  1 Load sequencing data in the FASTA format
#  2 Save sequencing data as a tsv file for use later
#  3 Repeat as needed to create a new datasets combining sequencing data and associated data

# Load Libraries ----------------------------------------------------------
library(phylotools)
library(tidyverse)
library(stringr)

# We need to separate out the following into their own columns.
# ACHAR1776-18|Psammitis deichmanni|CHARS00069-C01|BOLD:AAB7094
# ACHAR1776-18
# Psammitis deichmanni
# CHARS500069-C01
# BOLD:AAB7094

# Process BCHAR ---------------------------------------------------------------
# Load sequencing data using read.fasta
bchar_sequencedata <- read.fasta(file = "data/BCHAR/SequenceData.fas")

# First, copy the seq.name column to x temporarily
x <- bchar_sequencedata$seq.name

# Next, let's use str_split to take the x list and split it by the | character
y <- x %>%
  str_split("\\|")

# Now that we have that, let's use a simple solution to turn the data from being wide to being tall instead when we turn it into a data frame
bchar_splitdata <- data.frame(Reduce(rbind, y))

bchar_expanded <- data.frame(bchar_sequencedata$seq.text,bchar_sequencedata$seq.name,bchar_splitdata$X1,bchar_splitdata$X2,bchar_splitdata$X3,bchar_splitdata$X4)

# Great! Now that that's done, let's rename all the columns
names(bchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")

# Next up, we need replace blank entries for BIN URI's with NA's
# Why? Because we're classy like that.
bchar_expanded_na <- bchar_expanded %>%
  mutate_all(na_if,"")

# Fabulous. Now that we have the sequencing data sorted let's do some cool shit.
# That's it for this script. But before we go, let's save our work.
write_tsv(x = bchar_expanded_na, "data/BCHAR/bchar-sequencedata.tsv")

# Let's clean up our workspace.
rm(x,y,bchar_sequencedata,bchar_splitdata,bchar_expanded,bchar_expanded_na)

# Process CCHAR ---------------------------------------------------------------
cchar_sequencedata <- read.fasta(file = "data/CCHAR/SequenceData.fas")
x <- cchar_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
cchar_splitdata <- data.frame(Reduce(rbind, y))
cchar_expanded <- data.frame(cchar_sequencedata$seq.text,cchar_sequencedata$seq.name,cchar_splitdata$X1,cchar_splitdata$X2,cchar_splitdata$X3,cchar_splitdata$X4)
names(cchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
cchar_expanded_na <- cchar_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = cchar_expanded_na, "data/CCHAR/cchar-sequencedata.tsv")
rm(x,y,cchar_sequencedata,cchar_splitdata,cchar_expanded,cchar_expanded_na)

# Process DCHAR ---------------------------------------------------------------
dchar_sequencedata <- read.fasta(file = "data/DCHAR/SequenceData.fas")
x <- dchar_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
dchar_splitdata <- data.frame(Reduce(rbind, y))
dchar_expanded <- data.frame(dchar_sequencedata$seq.text,dchar_sequencedata$seq.name,dchar_splitdata$X1,dchar_splitdata$X2,dchar_splitdata$X3,dchar_splitdata$X4)
names(dchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
dchar_expanded_na <- dchar_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = dchar_expanded_na, "data/DCHAR/dchar-sequencedata.tsv")
rm(x,y,dchar_sequencedata,dchar_splitdata,dchar_expanded,dchar_expanded_na)

# Process FCHAR ---------------------------------------------------------------
fchar_sequencedata <- read.fasta(file = "data/FCHAR/SequenceData.fas")
x <- fchar_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
fchar_splitdata <- data.frame(Reduce(rbind, y))
fchar_expanded <- data.frame(fchar_sequencedata$seq.text,fchar_sequencedata$seq.name,fchar_splitdata$X1,fchar_splitdata$X2,fchar_splitdata$X3,fchar_splitdata$X4)
names(fchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
fchar_expanded_na <- fchar_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = fchar_expanded_na, "data/FCHAR/fchar-sequencedata.tsv")
rm(x,y,fchar_sequencedata,fchar_splitdata,fchar_expanded,fchar_expanded_na)

# Process GCHAR ---------------------------------------------------------------
gchar_sequencedata <- read.fasta(file = "data/GCHAR/SequenceData.fas")
x <- gchar_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
gchar_splitdata <- data.frame(Reduce(rbind, y))
gchar_expanded <- data.frame(gchar_sequencedata$seq.text,gchar_sequencedata$seq.name,gchar_splitdata$X1,gchar_splitdata$X2,gchar_splitdata$X3,gchar_splitdata$X4)
names(gchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
gchar_expanded_na <- gchar_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = gchar_expanded_na, "data/GCHAR/gchar-sequencedata.tsv")
rm(x,y,gchar_sequencedata,gchar_splitdata,gchar_expanded,gchar_expanded_na)

# Process GCHAR ---------------------------------------------------------------
hchar_sequencedata <- read.fasta(file = "data/HCHAR/SequenceData.fas")
x <- hchar_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
hchar_splitdata <- data.frame(Reduce(rbind, y))
hchar_expanded <- data.frame(hchar_sequencedata$seq.text,hchar_sequencedata$seq.name,hchar_splitdata$X1,hchar_splitdata$X2,hchar_splitdata$X3,hchar_splitdata$X4)
names(hchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
hchar_expanded_na <- hchar_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = hchar_expanded_na, "data/HCHAR/hchar-sequencedata.tsv")
rm(x,y,hchar_sequencedata,hchar_splitdata,hchar_expanded,hchar_expanded_na)

# Process CBAY ---------------------------------------------------------------
cbay_sequencedata <- read.fasta(file = "data/CBAY/SequenceData.fas")
x <- cbay_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
cbay_splitdata <- data.frame(Reduce(rbind, y))
cbay_expanded <- data.frame(cbay_sequencedata$seq.text,cbay_sequencedata$seq.name,cbay_splitdata$X1,cbay_splitdata$X2,cbay_splitdata$X3,cbay_splitdata$X4)
names(cbay_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
cbay_expanded_na <- cbay_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = cbay_expanded_na, "data/CBAY/cbay-sequencedata.tsv")
rm(x,y,cbay_sequencedata,cbay_splitdata,cbay_expanded,cbay_expanded_na)

# Process KUGA ---------------------------------------------------------------
kuga_sequencedata <- read.fasta(file = "data/KUGA/SequenceData.fas")
x <- kuga_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
kuga_splitdata <- data.frame(Reduce(rbind, y))
kuga_expanded <- data.frame(kuga_sequencedata$seq.text,kuga_sequencedata$seq.name,kuga_splitdata$X1,kuga_splitdata$X2,kuga_splitdata$X3,kuga_splitdata$X4)
names(kuga_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
kuga_expanded_na <- kuga_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = kuga_expanded_na, "data/KUGA/kuga-sequencedata.tsv")
rm(x,y,kuga_sequencedata,kuga_splitdata,kuga_expanded,kuga_expanded_na)

# Process KUGA ---------------------------------------------------------------
gjoa_sequencedata <- read.fasta(file = "data/GJOA/SequenceData.fas")
x <- gjoa_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
gjoa_splitdata <- data.frame(Reduce(rbind, y))
gjoa_expanded <- data.frame(gjoa_sequencedata$seq.text,gjoa_sequencedata$seq.name,gjoa_splitdata$X1,gjoa_splitdata$X2,gjoa_splitdata$X3,gjoa_splitdata$X4)
names(gjoa_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
gjoa_expanded_na <- gjoa_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = gjoa_expanded_na, "data/GJOA/gjoa-sequencedata.tsv")
rm(x,y,gjoa_sequencedata,gjoa_splitdata,gjoa_expanded,gjoa_expanded_na)

# Process KUGL ---------------------------------------------------------------
kugl_sequencedata <- read.fasta(file = "data/KUGL/SequenceData.fas")
x <- kugl_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
kugl_splitdata <- data.frame(Reduce(rbind, y))
kugl_expanded <- data.frame(kugl_sequencedata$seq.text,kugl_sequencedata$seq.name,kugl_splitdata$X1,kugl_splitdata$X2,kugl_splitdata$X3,kugl_splitdata$X4)
names(kugl_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
kugl_expanded_na <- kugl_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = kugl_expanded_na, "data/KUGL/kugl-sequencedata.tsv")
rm(x,y,kugl_sequencedata,kugl_splitdata,kugl_expanded,kugl_expanded_na)

# Process ARCBA ---------------------------------------------------------------
arcba_sequencedata <- read.fasta(file = "data/arcba/SequenceData.fas")
x <- arcba_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
arcba_splitdata <- data.frame(Reduce(rbind, y))
arcba_expanded <- data.frame(arcba_sequencedata$seq.text,arcba_sequencedata$seq.name,arcba_splitdata$X1,arcba_splitdata$X2,arcba_splitdata$X3,arcba_splitdata$X4)
names(arcba_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
arcba_expanded_na <- arcba_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = arcba_expanded_na, "data/arcba/arcba-sequencedata.tsv")
rm(x,y,arcba_sequencedata,arcba_splitdata,arcba_expanded,arcba_expanded_na)

# Process ARCBB ---------------------------------------------------------------
arcbb_sequencedata <- read.fasta(file = "data/arcbb/SequenceData.fas")
x <- arcbb_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
arcbb_splitdata <- data.frame(Reduce(rbind, y))
arcbb_expanded <- data.frame(arcbb_sequencedata$seq.text,arcbb_sequencedata$seq.name,arcbb_splitdata$X1,arcbb_splitdata$X2,arcbb_splitdata$X3,arcbb_splitdata$X4)
names(arcbb_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
arcbb_expanded_na <- arcbb_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = arcbb_expanded_na, "data/arcbb/arcbb-sequencedata.tsv")
rm(x,y,arcbb_sequencedata,arcbb_splitdata,arcbb_expanded,arcbb_expanded_na)

# Process ARCBIOY5 ---------------------------------------------------------------
ARCBIOY5_sequencedata <- read.fasta(file = "data/ARCBIOY5/SequenceData.fas")
x <- ARCBIOY5_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
ARCBIOY5_splitdata <- data.frame(Reduce(rbind, y))
ARCBIOY5_expanded <- data.frame(ARCBIOY5_sequencedata$seq.text,ARCBIOY5_sequencedata$seq.name,ARCBIOY5_splitdata$X1,ARCBIOY5_splitdata$X2,ARCBIOY5_splitdata$X3,ARCBIOY5_splitdata$X4)
names(ARCBIOY5_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
ARCBIOY5_expanded_na <- ARCBIOY5_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = ARCBIOY5_expanded_na, "data/ARCBIOY5/ARCBIOY5-sequencedata.tsv")
rm(x,y,ARCBIOY5_sequencedata,ARCBIOY5_splitdata,ARCBIOY5_expanded,ARCBIOY5_expanded_na)

# Process ARCBIOY6 ---------------------------------------------------------------
ARCBIOY6_sequencedata <- read.fasta(file = "data/ARCBIOY6/SequenceData.fas")
x <- ARCBIOY6_sequencedata$seq.name
y <- x %>%
  str_split("\\|")
ARCBIOY6_splitdata <- data.frame(Reduce(rbind, y))
ARCBIOY6_expanded <- data.frame(ARCBIOY6_sequencedata$seq.text,ARCBIOY6_sequencedata$seq.name,ARCBIOY6_splitdata$X1,ARCBIOY6_splitdata$X2,ARCBIOY6_splitdata$X3,ARCBIOY6_splitdata$X4)
names(ARCBIOY6_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")
ARCBIOY6_expanded_na <- ARCBIOY6_expanded %>%
  mutate_all(na_if,"")
write_tsv(x = ARCBIOY6_expanded_na, "data/ARCBIOY6/ARCBIOY6-sequencedata.tsv")
rm(x,y,ARCBIOY6_sequencedata,ARCBIOY6_splitdata,ARCBIOY6_expanded,ARCBIOY6_expanded_na)
