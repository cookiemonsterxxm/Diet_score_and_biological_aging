library(readr)
library(survey)
library(lavaan)
library(dplyr)
library(reshape2)
library(circlize)
library(ComplexHeatmap)
library(correlation)
library(tidyr)
source("~/NHANES_diet/000func.R")
load("C:/Users/xuxin/Desktop/06DietBAPack/NHANES_DietPattern_19992004.rda")
demo_df <- readRDS("C:/Users/xuxin/Desktop/06DietBAPack/demo_df.rds")

# 1. Obtain dietary data ------------------------------------------------------------------
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_19992000.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20012002.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20032004.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20052006.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20072008.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20092010.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20112012.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20132014.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20152016.rda")
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/05DietaryIndex/NHANES_20172020.Rda")

# 2. Dietary pattern calculation ------------------------------------------------------------------
## Year 2005-2006 ====
NHANES_20052006_design_d1d2 = NHANES_20052006$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20052006 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20052006$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20052006$NUTRIENT2)
MEDI_20052006 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20052006$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20052006$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20052006$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20052006$NUTRIENT_IND2
)
DASH_20052006 = DASH_NHANES_FPED(
  NHANES_20052006$FPED_IND,
  NHANES_20052006$NUTRIENT_IND,
  NHANES_20052006$FPED_IND2,
  NHANES_20052006$NUTRIENT_IND2
)
MED_20052006 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20052006$FPED,
  NUTRIENT_PATH = NHANES_20052006$NUTRIENT,
  DEMO_PATH = NHANES_20052006$DEMO,
  FPED_PATH2 = NHANES_20052006$FPED2,
  NUTRIENT_PATH2 = NHANES_20052006$NUTRIENT2
)
AHEI_20052006 = AHEI_NHANES_FPED(
  NHANES_20052006$FPED_IND,
  NHANES_20052006$NUTRIENT_IND,
  NHANES_20052006$FPED_IND2,
  NHANES_20052006$NUTRIENT_IND2
)
DII_20052006 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20052006$FPED,
  NUTRIENT_PATH = NHANES_20052006$NUTRIENT,
  DEMO_PATH = NHANES_20052006$DEMO,
  FPED_PATH2 = NHANES_20052006$FPED2,
  NUTRIENT_PATH2 = NHANES_20052006$NUTRIENT2
)
HEI2020_20052006 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20052006$FPED,
  NUTRIENT_PATH = NHANES_20052006$NUTRIENT,
  DEMO_PATH = NHANES_20052006$DEMO,
  FPED_PATH2 = NHANES_20052006$FPED2,
  NUTRIENT_PATH2 = NHANES_20052006$NUTRIENT2
)
NHANES_20052006_dietaryindex_d1d2 = inner_join(NHANES_20052006_design_d1d2, DASHI_20052006, by = "SEQN") %>%
  inner_join(MEDI_20052006, by = "SEQN") %>%
  inner_join(DASH_20052006, by = "SEQN") %>%
  inner_join(MED_20052006, by = "SEQN") %>%
  inner_join(AHEI_20052006, by = "SEQN") %>%
  inner_join(DII_20052006, by = "SEQN") %>%
  inner_join(HEI2020_20052006, by = "SEQN")
colnames(NHANES_20052006_dietaryindex_d1d2)


## Year 2007-2008 ====
NHANES_20072008_design_d1d2 = NHANES_20072008$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20072008 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20072008$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20072008$NUTRIENT2)
MEDI_20072008 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20072008$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20072008$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20072008$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20072008$NUTRIENT_IND2
)
DASH_20072008 = DASH_NHANES_FPED(
  NHANES_20072008$FPED_IND,
  NHANES_20072008$NUTRIENT_IND,
  NHANES_20072008$FPED_IND2,
  NHANES_20072008$NUTRIENT_IND2
)
MED_20072008 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20072008$FPED,
  NUTRIENT_PATH = NHANES_20072008$NUTRIENT,
  DEMO_PATH = NHANES_20072008$DEMO,
  FPED_PATH2 = NHANES_20072008$FPED2,
  NUTRIENT_PATH2 = NHANES_20072008$NUTRIENT2
)
AHEI_20072008 = AHEI_NHANES_FPED(
  NHANES_20072008$FPED_IND,
  NHANES_20072008$NUTRIENT_IND,
  NHANES_20072008$FPED_IND2,
  NHANES_20072008$NUTRIENT_IND2
)
DII_20072008 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20072008$FPED,
  NUTRIENT_PATH = NHANES_20072008$NUTRIENT,
  DEMO_PATH = NHANES_20072008$DEMO,
  FPED_PATH2 = NHANES_20072008$FPED2,
  NUTRIENT_PATH2 = NHANES_20072008$NUTRIENT2
)
HEI2020_20072008 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20072008$FPED,
  NUTRIENT_PATH = NHANES_20072008$NUTRIENT,
  DEMO_PATH = NHANES_20072008$DEMO,
  FPED_PATH2 = NHANES_20072008$FPED2,
  NUTRIENT_PATH2 = NHANES_20072008$NUTRIENT2
)
NHANES_20072008_dietaryindex_d1d2 = inner_join(NHANES_20072008_design_d1d2, DASHI_20072008, by = "SEQN") %>%
  inner_join(MEDI_20072008, by = "SEQN") %>%
  inner_join(DASH_20072008, by = "SEQN") %>%
  inner_join(MED_20072008, by = "SEQN") %>%
  inner_join(AHEI_20072008, by = "SEQN") %>%
  inner_join(DII_20072008, by = "SEQN") %>%
  inner_join(HEI2020_20072008, by = "SEQN")
colnames(NHANES_20072008_dietaryindex_d1d2)

## Year 2009-2010 ====
NHANES_20092010_design_d1d2 = NHANES_20092010$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20092010 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20092010$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20092010$NUTRIENT2)
MEDI_20092010 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20092010$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20092010$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20092010$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20092010$NUTRIENT_IND2
)
DASH_20092010 = DASH_NHANES_FPED(
  NHANES_20092010$FPED_IND,
  NHANES_20092010$NUTRIENT_IND,
  NHANES_20092010$FPED_IND2,
  NHANES_20092010$NUTRIENT_IND2
)
MED_20092010 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20092010$FPED,
  NUTRIENT_PATH = NHANES_20092010$NUTRIENT,
  DEMO_PATH = NHANES_20092010$DEMO,
  FPED_PATH2 = NHANES_20092010$FPED2,
  NUTRIENT_PATH2 = NHANES_20092010$NUTRIENT2
)
AHEI_20092010 = AHEI_NHANES_FPED(
  NHANES_20092010$FPED_IND,
  NHANES_20092010$NUTRIENT_IND,
  NHANES_20092010$FPED_IND2,
  NHANES_20092010$NUTRIENT_IND2
)
DII_20092010 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20092010$FPED,
  NUTRIENT_PATH = NHANES_20092010$NUTRIENT,
  DEMO_PATH = NHANES_20092010$DEMO,
  FPED_PATH2 = NHANES_20092010$FPED2,
  NUTRIENT_PATH2 = NHANES_20092010$NUTRIENT2
)
HEI2020_20092010 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20092010$FPED,
  NUTRIENT_PATH = NHANES_20092010$NUTRIENT,
  DEMO_PATH = NHANES_20092010$DEMO,
  FPED_PATH2 = NHANES_20092010$FPED2,
  NUTRIENT_PATH2 = NHANES_20092010$NUTRIENT2
)
NHANES_20092010_dietaryindex_d1d2 = inner_join(NHANES_20092010_design_d1d2, DASHI_20092010, by = "SEQN") %>%
  inner_join(MEDI_20092010, by = "SEQN") %>%
  inner_join(DASH_20092010, by = "SEQN") %>%
  inner_join(MED_20092010, by = "SEQN") %>%
  inner_join(AHEI_20092010, by = "SEQN") %>%
  inner_join(DII_20092010, by = "SEQN") %>%
  inner_join(HEI2020_20092010, by = "SEQN")
colnames(NHANES_20092010_dietaryindex_d1d2)


## Year 2011-2012 ====
NHANES_20112012_design_d1d2 = NHANES_20112012$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20112012 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20112012$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20112012$NUTRIENT2)
MEDI_20112012 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20112012$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20112012$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20112012$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20112012$NUTRIENT_IND2
)
DASH_20112012 = DASH_NHANES_FPED(
  NHANES_20112012$FPED_IND,
  NHANES_20112012$NUTRIENT_IND,
  NHANES_20112012$FPED_IND2,
  NHANES_20112012$NUTRIENT_IND2
)
MED_20112012 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20112012$FPED,
  NUTRIENT_PATH = NHANES_20112012$NUTRIENT,
  DEMO_PATH = NHANES_20112012$DEMO,
  FPED_PATH2 = NHANES_20112012$FPED2,
  NUTRIENT_PATH2 = NHANES_20112012$NUTRIENT2
)
AHEI_20112012 = AHEI_NHANES_FPED(
  NHANES_20112012$FPED_IND,
  NHANES_20112012$NUTRIENT_IND,
  NHANES_20112012$FPED_IND2,
  NHANES_20112012$NUTRIENT_IND2
)
DII_20112012 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20112012$FPED,
  NUTRIENT_PATH = NHANES_20112012$NUTRIENT,
  DEMO_PATH = NHANES_20112012$DEMO,
  FPED_PATH2 = NHANES_20112012$FPED2,
  NUTRIENT_PATH2 = NHANES_20112012$NUTRIENT2
)
HEI2020_20112012 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20112012$FPED,
  NUTRIENT_PATH = NHANES_20112012$NUTRIENT,
  DEMO_PATH = NHANES_20112012$DEMO,
  FPED_PATH2 = NHANES_20112012$FPED2,
  NUTRIENT_PATH2 = NHANES_20112012$NUTRIENT2
)
NHANES_20112012_dietaryindex_d1d2 = inner_join(NHANES_20112012_design_d1d2, DASHI_20112012, by = "SEQN") %>%
  inner_join(MEDI_20112012, by = "SEQN") %>%
  inner_join(DASH_20112012, by = "SEQN") %>%
  inner_join(MED_20112012, by = "SEQN") %>%
  inner_join(AHEI_20112012, by = "SEQN") %>%
  inner_join(DII_20112012, by = "SEQN") %>%
  inner_join(HEI2020_20112012, by = "SEQN")
colnames(NHANES_20112012_dietaryindex_d1d2)

## Year 2013-2014 ====
NHANES_20132014_design_d1d2 = NHANES_20132014$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20132014 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20132014$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20132014$NUTRIENT2)
MEDI_20132014 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20132014$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20132014$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20132014$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20132014$NUTRIENT_IND2
)
DASH_20132014 = DASH_NHANES_FPED(
  NHANES_20132014$FPED_IND,
  NHANES_20132014$NUTRIENT_IND,
  NHANES_20132014$FPED_IND2,
  NHANES_20132014$NUTRIENT_IND2
)
MED_20132014 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20132014$FPED,
  NUTRIENT_PATH = NHANES_20132014$NUTRIENT,
  DEMO_PATH = NHANES_20132014$DEMO,
  FPED_PATH2 = NHANES_20132014$FPED2,
  NUTRIENT_PATH2 = NHANES_20132014$NUTRIENT2
)
AHEI_20132014 = AHEI_NHANES_FPED(
  NHANES_20132014$FPED_IND,
  NHANES_20132014$NUTRIENT_IND,
  NHANES_20132014$FPED_IND2,
  NHANES_20132014$NUTRIENT_IND2
)
DII_20132014 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20132014$FPED,
  NUTRIENT_PATH = NHANES_20132014$NUTRIENT,
  DEMO_PATH = NHANES_20132014$DEMO,
  FPED_PATH2 = NHANES_20132014$FPED2,
  NUTRIENT_PATH2 = NHANES_20132014$NUTRIENT2
)
HEI2020_20132014 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20132014$FPED,
  NUTRIENT_PATH = NHANES_20132014$NUTRIENT,
  DEMO_PATH = NHANES_20132014$DEMO,
  FPED_PATH2 = NHANES_20132014$FPED2,
  NUTRIENT_PATH2 = NHANES_20132014$NUTRIENT2
)
NHANES_20132014_dietaryindex_d1d2 = inner_join(NHANES_20132014_design_d1d2, DASHI_20132014, by = "SEQN") %>%
  inner_join(MEDI_20132014, by = "SEQN") %>%
  inner_join(DASH_20132014, by = "SEQN") %>%
  inner_join(MED_20132014, by = "SEQN") %>%
  inner_join(AHEI_20132014, by = "SEQN") %>%
  inner_join(DII_20132014, by = "SEQN") %>%
  inner_join(HEI2020_20132014, by = "SEQN")
colnames(NHANES_20132014_dietaryindex_d1d2)

## Year 2015-2016 ====
NHANES_20152016_design_d1d2 = NHANES_20152016$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20152016 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20152016$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20152016$NUTRIENT2)
MEDI_20152016 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20152016$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20152016$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20152016$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20152016$NUTRIENT_IND2
)
DASH_20152016 = DASH_NHANES_FPED(
  NHANES_20152016$FPED_IND,
  NHANES_20152016$NUTRIENT_IND,
  NHANES_20152016$FPED_IND2,
  NHANES_20152016$NUTRIENT_IND2
)
MED_20152016 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20152016$FPED,
  NUTRIENT_PATH = NHANES_20152016$NUTRIENT,
  DEMO_PATH = NHANES_20152016$DEMO,
  FPED_PATH2 = NHANES_20152016$FPED2,
  NUTRIENT_PATH2 = NHANES_20152016$NUTRIENT2
)
AHEI_20152016 = AHEI_NHANES_FPED(
  NHANES_20152016$FPED_IND,
  NHANES_20152016$NUTRIENT_IND,
  NHANES_20152016$FPED_IND2,
  NHANES_20152016$NUTRIENT_IND2
)
DII_20152016 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20152016$FPED,
  NUTRIENT_PATH = NHANES_20152016$NUTRIENT,
  DEMO_PATH = NHANES_20152016$DEMO,
  FPED_PATH2 = NHANES_20152016$FPED2,
  NUTRIENT_PATH2 = NHANES_20152016$NUTRIENT2
)
HEI2020_20152016 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20152016$FPED,
  NUTRIENT_PATH = NHANES_20152016$NUTRIENT,
  DEMO_PATH = NHANES_20152016$DEMO,
  FPED_PATH2 = NHANES_20152016$FPED2,
  NUTRIENT_PATH2 = NHANES_20152016$NUTRIENT2
)
NHANES_20152016_dietaryindex_d1d2 = inner_join(NHANES_20152016_design_d1d2, DASHI_20152016, by = "SEQN") %>%
  inner_join(MEDI_20152016, by = "SEQN") %>%
  inner_join(DASH_20152016, by = "SEQN") %>%
  inner_join(MED_20152016, by = "SEQN") %>%
  inner_join(AHEI_20152016, by = "SEQN") %>%
  inner_join(DII_20152016, by = "SEQN") %>%
  inner_join(HEI2020_20152016, by = "SEQN")
colnames(NHANES_20152016_dietaryindex_d1d2)

## Year 2017-2018 ====
NHANES_20172018_design_d1d2 = NHANES_20172018$FPED %>%
  filter(!is.na(WTDR2D)) # Dietary two-day sample weight
DASHI_20172018 = DASHI_NHANES_FPED(NUTRIENT_PATH = NHANES_20172018$NUTRIENT,
                                   NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2)
MEDI_20172018 = MEDI_NHANES_FPED(
  FPED_IND_PATH = NHANES_20172018$FPED_IND,
  NUTRIENT_IND_PATH = NHANES_20172018$NUTRIENT_IND,
  FPED_IND_PATH2 = NHANES_20172018$FPED_IND2,
  NUTRIENT_IND_PATH2 = NHANES_20172018$NUTRIENT_IND2
)
DASH_20172018 = DASH_NHANES_FPED(
  NHANES_20172018$FPED_IND,
  NHANES_20172018$NUTRIENT_IND,
  NHANES_20172018$FPED_IND2,
  NHANES_20172018$NUTRIENT_IND2
)
MED_20172018 = MED_NHANES_FPED(
  FPED_PATH = NHANES_20172018$FPED,
  NUTRIENT_PATH = NHANES_20172018$NUTRIENT,
  DEMO_PATH = NHANES_20172018$DEMO,
  FPED_PATH2 = NHANES_20172018$FPED2,
  NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2
)
AHEI_20172018 = AHEI_NHANES_FPED(
  NHANES_20172018$FPED_IND,
  NHANES_20172018$NUTRIENT_IND,
  NHANES_20172018$FPED_IND2,
  NHANES_20172018$NUTRIENT_IND2
)
DII_20172018 = DII_NHANES_FPED(
  FPED_PATH = NHANES_20172018$FPED,
  NUTRIENT_PATH = NHANES_20172018$NUTRIENT,
  DEMO_PATH = NHANES_20172018$DEMO,
  FPED_PATH2 = NHANES_20172018$FPED2,
  NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2
)
HEI2020_20172018 = HEI2020_NHANES_FPED(
  FPED_PATH = NHANES_20172018$FPED,
  NUTRIENT_PATH = NHANES_20172018$NUTRIENT,
  DEMO_PATH = NHANES_20172018$DEMO,
  FPED_PATH2 = NHANES_20172018$FPED2,
  NUTRIENT_PATH2 = NHANES_20172018$NUTRIENT2
)
NHANES_20172018_dietaryindex_d1d2 = inner_join(NHANES_20172018_design_d1d2, DASHI_20172018, by = "SEQN") %>%
  inner_join(MEDI_20172018, by = "SEQN") %>%
  inner_join(DASH_20172018, by = "SEQN") %>%
  inner_join(MED_20172018, by = "SEQN") %>%
  inner_join(AHEI_20172018, by = "SEQN") %>%
  inner_join(DII_20172018, by = "SEQN") %>%
  inner_join(HEI2020_20172018, by = "SEQN")
colnames(NHANES_20172018_dietaryindex_d1d2)

## Year 1999–2004 ====
## Merge above 2005–2018 ====
NHANES_20051018_dietaryindex_d1d2 <-
  reduce(list(
    NHANES_20052006_dietaryindex_d1d2,
    NHANES_20072008_dietaryindex_d1d2,
    NHANES_20092010_dietaryindex_d1d2,
    NHANES_20112012_dietaryindex_d1d2,
    NHANES_20132014_dietaryindex_d1d2,
    NHANES_20152016_dietaryindex_d1d2,
    NHANES_20172018_dietaryindex_d1d2
  ),
  function(df1, df2)
    full_join(df1, df2))
NHANES_20051018_dietaryindex_d1d2 %>% nrow  # 49576


# Use gsub() function to remove "_DII"
names(DII_19992004) <- gsub("_DII$", "", names(DII_19992004))

# View modified variable names
names(DII_19992004)

# Merge dietary indices for 1999–2004
NHANES_19992004 = full_join(HEI2020_19992004, AHEI_19992004, by = c("SEQN")) %>%
  full_join(pat2_19992004, by = c("SEQN")) %>%
  full_join(DII_19992004, by = c("SEQN")) %>%
  full_join(MED_19992004, by = c("SEQN"))

# Remove duplicated or irrelevant variables
NHANES_19992004$REPEATNUM = NULL
NHANES_19992004$RIDAGEYR.x = NHANES_19992004$RIDAGEYR
NHANES_19992004$RIDAGEYR.y = NHANES_19992004$RIDAGEYR

# Combine 1999–2004 and 2005–2018 dietary index datasets
NHANES_19992018 = rbind(
  NHANES_19992004[, intersect(names(NHANES_19992004), names(NHANES_20051018_dietaryindex_d1d2))],
  NHANES_20051018_dietaryindex_d1d2[, intersect(names(NHANES_19992004), names(NHANES_20051018_dietaryindex_d1d2))]
)
dim(NHANES_19992018) # 75093

# Summarize missing data
NHANES_19992018_missing <- missing_data_summary(NHANES_19992018)

# NHANES_19992018 = merge(NHANES_19992018,PA_Energy_D2J,by='SEQN',all.x=T)

# Filter adults aged 20–84
NHANES_19992018 %>% filter(RIDAGEYR.x > 19, RIDAGEYR.x < 85) %>% nrow  # 43489

# Merge demographic data
data_9918 = merge(NHANES_19992018, demo_df, by = "SEQN")

# Merge covariate data
nhanes4_cov_df_1028 <- readRDS("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/nhanes4_cov_df_1028.rds")
data_9918 = merge(
  data_9918,
  nhanes4_cov_df_1028[, c("SEQN",
                          "Smoke", "BMI", "PhysicalActivity", "Malignancy",
                          "Liver_disease", "Cardiovascular_disease", "Hypertension", "Diabetes",
                          "Thyroid_disease", "Arthritis", "Kidney_disease", "Chronic_bronchitis")],
  by = "SEQN", all.x = TRUE
)

# Filter 20–79 years old, not pregnant
data_9918 %>% filter(RIDAGEYR.x > 19, RIDAGEYR.x < 80, Pregnant == 0) %>% nrow  # 39724
data_9918 = data_9918 %>% filter(RIDAGEYR.x > 20, RIDAGEYR.x < 80, Pregnant == 0)

# Remove rows missing MED_ALL or pat2_ALL
data_9918 = data_9918[!is.na(data_9918$MED_ALL), ]
data_9918 = data_9918[!is.na(data_9918$pat2_ALL), ]

# Merge average kcal data
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/NHANES_kcal9918.rda")
data_9918 = merge(data_9918, KCAL9918, by = "SEQN")

# Merge PA_2018 column
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/NHANES9918_proc.Rdata")
data_9918 = merge(data_9918, NHANES9918[, c("SEQN", "PA_2018")], by = "SEQN", all.x = TRUE)

# Filter plausible kcal
data_9918 = data_9918 %>% filter(KCAL_mean < 4000, KCAL_mean > 500)

# Save combined dataset with diet + covariates
save(data_9918, file = "data_9918_38315.RData") # diet + cov

# Load all_data including biomarker aging
load("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/all_data_xm0422.RData")

# Merge with phenotypic aging
data_9918_ba = merge(
  data_9918,
  NHANES4[, c("SEQN", "PhenoAgeAccel", "PhenoAge")],
  by = "SEQN", all.x = TRUE
) # 4418 PhenoAge NA

# Merge with organ-specific biological age data
data_9918_ba = merge(
  data_9918_ba,
  organ_ba_dat[, c("SEQN", "CardiacAgeAccel", "CardiacAge", "CardiacAgePredict",
                   "KidneyAgeAccel", "KidneyAge", "KidneyAgePredict",
                   "LiverAgeAccel", "LiverAge", "LiverAgePredict",
                   "MuscAgeAccel", "MuscAge", "MuscAgePredict")],
  by = "SEQN", all.x = TRUE
)

# Remove rows missing PhenoAgeAccel
data_9918_ba = data_9918_ba[!is.na(data_9918_ba$PhenoAgeAccel), ]

# Save version with BA data
save(data_9918_ba, file = "data_9918_ba_34330.RData")
dim(data_9918_ba[!is.na(data_9918_ba$MuscAgeAccel), ]) # 13139 rows, 127 columns


# Impute missing categorical covariates -----------------------------------------
missing_data_summary(data_9918_ba[, c("Smoke", "Ethnicity", "Education", "Marital_status", "PIR",
                                      "Average_cal",
                                      "PhysicalActivity", "PA_2018",
                                      "BMI", "Diabetes", "Hypertension", "Liver_disease")])

table(data_9918_ba$Smoke, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Smoke), ]$Smoke = "Never"

table(data_9918_ba$Education, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Education), ]$Education = "More than high school"

table(data_9918_ba$Marital_status, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Marital_status), ]$Marital_status = "Married/cohabiting"

table(data_9918_ba$PIR, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$PIR), ]$PIR = "Missing"

table(data_9918_ba$PA_2018, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$PA_2018), ]$PA_2018 = FALSE

table(data_9918_ba$BMI, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$BMI), ]$BMI = "25-30"

table(data_9918_ba$Diabetes, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Diabetes), ]$Diabetes = 0

table(data_9918_ba$Hypertension, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Hypertension), ]$Hypertension = 0

table(data_9918_ba$Liver_disease, useNA = "ifany")
data_9918_ba[is.na(data_9918_ba$Liver_disease), ]$Liver_disease = 0

# Merge survey weights/strata/PSU
data_9918_ba = merge(data_9918_ba, NHANES9918[, c("SEQN", "WTMEC2YR")], by = "SEQN", all.x = TRUE)
data_9918_ba = merge(data_9918_ba, NHANES9918[, c("SEQN", "SDMVPSU", "SDMVSTRA")], by = "SEQN", all.x = TRUE)

# Save imputed dataset
save(data_9918_ba, file = "data_9918_ba_34330_imputed.RData")

# (1) 1999–2002 & DNA methylation age analysis -------------------------------------------

# Load DNA methylation data (Horvath, Hannum, PhenoAge, GrimAge, DunedinPoAm)
methy_99_02 <- read_sas("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/dnmepi.sas7bdat")
data_99_02 <- merge(data_9918_ba, methy_99_02[, c("SEQN", "HorvathAge",
                                                  "HannumAge", "PhenoAge",
                                                  "GrimAge2Mort", "DunedinPoAm")],
                    by = "SEQN", all.x = TRUE)

# Keep only rows with methylation age
data_99_02 = data_99_02[!is.na(data_99_02$HorvathAge), ] # 1871 rows

# Calculate age acceleration residuals
data_99_02$HorvathAgeAccel = residuals(lm(HorvathAge ~ age + sex + age:sex, data = data_99_02))
data_99_02$HannumAgeAccel  = residuals(lm(HannumAge ~ age + sex + age:sex, data = data_99_02))
data_99_02$PhenoAgeMethy   = data_99_02$PhenoAge.y
data_99_02$PhenoAgeMethyAccel = residuals(lm(PhenoAgeMethy ~ age + sex + age:sex, data = data_99_02))
data_99_02$GrimAge2MortAccel = residuals(lm(GrimAge2Mort ~ age + sex + age:sex, data = data_99_02))
data_99_02$DunedinPoAmAccel  = residuals(lm(DunedinPoAm ~ age + sex + age:sex, data = data_99_02))

saveRDS(data_99_02, file = "data_99_02.rds")

# Merge survey weights for 1999-2002 (WTDR4YR)
load("~/NHANES_diet/NHANES_20012002.rda")
load("~/NHANES_diet/NHANES_19992000.rda")
tmp = rbind(
  NHANES_19992000$NUTRIENT[, c("SEQN", "WTDRD1", "WTDR4YR")] %>% data.frame(),
  NHANES_20012002$NUTRIENT[, c("SEQN", "WTDRD1", "WTDR4YR")] %>% data.frame()
)
data_99_02 = merge(data_99_02, tmp, by = "SEQN", all.x = TRUE)

## Analysis ----
### Model 1 ----

diet_index = c('HEI2020_ALL',
               'AHEI_ALL',
               'pat2_ALL',
               'MED_ALL',
               'DII_ALL')

# Create quintiles for specified columns
data_99_02_quit <- data_99_02 %>%
  mutate(across(all_of(diet_index), ~ ntile(., 5)))  # Create quintiles

# Rename columns by adding "_quintile" suffix
names(data_99_02_quit)[names(data_99_02_quit) %in% diet_index] <-
  paste0(names(data_99_02_quit)[names(data_99_02_quit) %in% diet_index], "_quintile")

# Set up survey design object
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ WTDR4YR,
    data = data_99_02_quit
  ) # Create survey design, fixed format

# Create a list to store results
results <- list()

# Function to extract estimates, CI, and p-values for quintiles
extract_info <- function(model) {
  exp2 = paste0(coef(model)[2] %>% round(2), '(', confint(model)[2, 1] %>% round(2), ',', confint(model)[2, 2] %>% round(2), ')')
  info2 <- data.frame(q2 = exp2, q2_p = summary(model)$coefficients[, "Pr(>|t|)"][2] %>% round(3))

  exp3 = paste0(coef(model)[3] %>% round(2), '(', confint(model)[3, 1] %>% round(2), ',', confint(model)[3, 2] %>% round(2), ')')
  info3 <- data.frame(q3 = exp3, q3_p = summary(model)$coefficients[, "Pr(>|t|)"][3] %>% round(2))

  exp4 = paste0(coef(model)[4] %>% round(2), '(', confint(model)[4, 1] %>% round(2), ',', confint(model)[4, 2] %>% round(2), ')')
  info4 <- data.frame(q4 = exp4, q4_p = summary(model)$coefficients[, "Pr(>|t|)"][4] %>% round(2))

  exp5 = paste0(coef(model)[5] %>% round(2), '(', confint(model)[5, 1] %>% round(2), ',', confint(model)[5, 2] %>% round(2), ')')
  info5 <- data.frame(q5 = exp5, q5_p = summary(model)$coefficients[, "Pr(>|t|)"][5] %>% round(2))

  info = cbind(info2, info3) %>% cbind(info4, info5)
  return(info)
}

diet_indexx = c("HEI2020_ALL_quintile", "AHEI_ALL_quintile",
                "pat2_ALL_quintile", "MED_ALL_quintile",
                "DII_ALL_quintile")

BA_index = c("HorvathAgeAccel", "HannumAgeAccel",
             "PhenoAgeMethyAccel", "GrimAge2MortAccel", "DunedinPoAmAccel")

# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each diet index quintile
  for (X_variable in diet_indexx) {
    # Build formula
    formula <- as.formula(paste(Y_variable, "~", paste('as.factor(', X_variable, ') + Age + Sex', collapse = "")))
    # Fit model
    model <- svyglm(formula, design = design)
    # Extract results
    info <- extract_info(model)
    # Store
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to data frame and save
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df, file = 'association_quin_9902_methy.csv')

### Model 2 ----
results <- list()
for (Y_variable in BA_index) {
  for (X_variable in diet_indexx) {
    formula <- as.formula(paste(
      Y_variable, "~",
      paste('factor(', X_variable, ') + Age + Sex + Smoke + factor(Ethnicity) + PA_2018 +',
            'factor(Education) + factor(Marital_status) + factor(PIR) + Average_cal +',
            'BMI + Diabetes + Hypertension + Liver_disease', collapse = "")
    ))
    model <- svyglm(formula, design = design)
    info <- extract_info(model)
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df, file = 'association_quin_9902_methy_ml2.csv')

### Per 10th–90th percentile (IQR scaling) ----

# Function to extract per-IQR estimate, CI & p-value
ext_info <- function(model) {
  exp2 = paste0(coef(model)[2] %>% round(2), '(', confint(model)[2, 1] %>% round(2), ',', confint(model)[2, 2] %>% round(2), ')')
  info2 <- data.frame(q2 = exp2, q2_p = summary(model)$coefficients[, "Pr(>|t|)"][2] %>% round(3))
  return(info2)
}

# Calculate interquartile ranges for each diet index
iqr_AHEI    = quantile(data_99_02$AHEI_ALL, probs = 0.9) - quantile(data_99_02$AHEI_ALL, probs = 0.1)
iqr_pat2    = quantile(data_99_02$pat2_ALL, probs = 0.9) - quantile(data_99_02$pat2_ALL, probs = 0.1)
iqr_DII     = quantile(data_99_02$DII_ALL, probs = 0.9) - quantile(data_99_02$DII_ALL, probs = 0.1)
iqr_HEI2020 = quantile(data_99_02$HEI2020_ALL, probs = 0.9) - quantile(data_99_02$HEI2020_ALL, probs = 0.1)
iqr_MED     = quantile(data_99_02$MED_ALL, probs = 0.9) - quantile(data_99_02$MED_ALL, probs = 0.1)

# Create scaled variables
data_99_02$AHEI_iqr    = data_99_02$AHEI_ALL / iqr_AHEI
data_99_02$pat2_iqr    = data_99_02$pat2_ALL / iqr_pat2
data_99_02$DII_iqr     = data_99_02$DII_ALL / iqr_DII
data_99_02$HEI2020_iqr = data_99_02$HEI2020_ALL / iqr_HEI2020
data_99_02$MED_iqr     = data_99_02$MED_ALL / iqr_MED

iqr_index = c('HEI2020_iqr', 'AHEI_iqr', 'pat2_iqr', 'MED_iqr', 'DII_iqr')

# Survey design for IQR analysis
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ WTDR4YR,
    data = data_99_02
  )

### Model 1 ----
results <- list()
for (Y_variable in BA_index) {
  for (X_variable in iqr_index) {
    formula <- as.formula(paste(Y_variable, "~", paste(X_variable, '+ Age + Sex', collapse = "")))
    model <- svyglm(formula, design = design)
    info <- ext_info(model)
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df, file = 'association_per_iqr_age_sex.csv')

### Model 2 ----
results <- list()
for (Y_variable in BA_index) {
  for (X_variable in iqr_index) {
    formula <- as.formula(paste(
      Y_variable, "~",
      paste(X_variable, '+ Age + Sex + Smoke + factor(Ethnicity) + PA_2018 +',
            'factor(Education) + factor(Marital_status) + factor(PIR) + Average_cal +',
            'BMI + Diabetes + Hypertension + Liver_disease', collapse = "")
    ))
    model <- svyglm(formula, design = design)
    info <- ext_info(model)
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df, file = 'association_per_iqr_ml2_noadjustsmoke.csv')

### Current smokers only ----
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ WTMEC2YR7,
    data = data_99_02[data_99_02$Smoke == 'Current', ]
  )

results <- list()
for (Y_variable in BA_index) {
  for (X_variable in iqr_index) {
    formula <- as.formula(paste(
      Y_variable, "~",
      paste(X_variable, '+ Age + Sex + factor(Ethnicity) + PA_2018 +',
            'factor(Education) + factor(Marital_status) + factor(PIR) + Average_cal +',
            'BMI + Diabetes + Hypertension + Liver_disease', collapse = "")
    ))
    model <- svyglm(formula, design = design)
    info <- ext_info(model)
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df, file = 'association_per_iqr_ml2_current.csv')



#table 1 demographic----
#(2) 1999-2018 diet & ba/oa-------
###quintiles----
load("~/NHANES_diet/data_9918_ba_34330_imputed.RData")
load("~/NHANES_diet/NHANES_20012002.rda")
load("~/NHANES_diet/NHANES_19992000.rda")

tmp=rbind(NHANES_19992000$NUTRIENT[,c('SEQN','WTDR4YR')]%>%data.frame(),NHANES_20012002$NUTRIENT[,c('SEQN','WTDR4YR')]%>%data.frame())
data_9918_ba=merge(data_9918_ba,tmp,by='SEQN',all.x=T)
data_9918_ba=merge(data_9918_ba,NHANES9918[,c('SEQN','WTDRD1')],by='SEQN',all.x=T)

data_9918_ba$MEC10YR=ifelse(data_9918_ba$cycle %in% c('A','B'),
                            2/10*data_9918_ba$WTDR4YR, 1/10*data_9918_ba$WTDRD1)


load("~/NHANES_diet/NHANES9918_proc.Rdata")
data_9918_ba$ethnicity=NULL

data_9918_ba=merge(data_9918_ba,NHANES9918[,c('SEQN','ethnicity','education3',
                                              'poverty3','BMI3','hbp','cvd','diabetes_status','cancer')],by='SEQN',all.x=T)
missing_data_summary(data_9918_ba[,c('SEQN','ethnicity','education3',
                                   'poverty3','BMI3','hbp','cvd','diabetes_status','cancer')])
# Variable MissingCount  MissingRate
# SEQN                       SEQN            0 0.0000000000
# ethnicity             ethnicity            0 0.0000000000
# education3           education3           28 0.0008156132
# poverty3               poverty3         2675 0.0779201864
# BMI3                       BMI3          303 0.0088260996
# hbp                         hbp            0 0.0000000000
# cvd                         cvd            0 0.0000000000
# diabetes_status diabetes_status            0 0.0000000000
# cancer                   cancer           35 0.0010195165

data_9918_ba[, poverty3 := as.character(poverty3)]
data_9918_ba[, poverty3 := ifelse(is.na(poverty3), "missing", poverty3)]
data_9918_ba[, BMI3 := as.character(BMI3)]
data_9918_ba[, BMI3 := ifelse(is.na(BMI3), "< 30 kg/m2", BMI3)]
data_9918_ba[,cancer := as.character(cancer)]
data_9918_ba[, cancer := ifelse(is.na(cancer), "non-cancer", cancer)]

nhanes4_ba <- readRDS("~/NHANES_diet/nhanes4_ba.rds")
data_9918_ba <- data_9918_ba[, !(names(data_9918_ba) %in% 
                                   c("PhenoAgeAccel", "PhenoAge", "CardiacAgePredict",
                                     "CardiacAgeAccel", "CardiacAge", "KidneyAgePredict",
                                     "KidneyAgeAccel", "KidneyAge","LiverAgePredict",
                                     "LiverAgeAccel" ,  "LiverAge",
                                     "MuscAgePredict", "MuscAgeAccel", "MuscAge"))]
data_9918_ba=merge(data_9918_ba,nhanes4_ba,by='SEQN',all.x=T)
save(data_9918_ba,file='data_9918_ba_1104_34330.RData')
handle_dat=data_9918_ba %>%data.frame()

diet_index=c('HEI2020_ALL',
             'AHEI_ALL',
             'DASH_ALL_quintile',
            'MED_ALL',
             'DII_ALL')
BA_index=c('PhenoAgeAccel',"CardiacAgeAccel", "KidneyAgeAccel","LiverAgeAccel" ,'MuscleAgeAccel' )

# Create quintiles for specified columns
handle_dat_quit <- handle_dat %>%
  mutate(across(all_of(diet_index), ~ntile(., 5)))  # Create quintiles
#handle_dat_quit$WTMEC2YR7 <- handle_dat_quit$WTMEC2YR / 7

# Rename columns
names(handle_dat_quit)[names(handle_dat_quit) %in% diet_index] <- paste0(names(handle_dat_quit)[names(handle_dat_quit) %in% diet_index], "_quintile")
#handle_dat_quit$pat2I_ALL_quintile=as.factor(handle_dat_quit$pat2I_ALL_quintile)

design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_quit
  )#生成svy数据，格式固定
##model1----
# Create a list to store results
results <- list()

diet_indexx=c("HEI2020_ALL_quintile", "AHEI_ALL_quintile" ,
              "DASH_ALL_quintile"   , "MED_ALL_quintile",
              "DII_ALL_quintile"  )
# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in diet_indexx) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste('as.factor(',X_variable,')+age+sex', collapse = "")))
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    # Extract information
    info <- extract_info(model)
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_quin9918_ml1.csv')


results <- list()
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in diet_indexx) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste('as.factor(',X_variable,')+age + sex +Smoke+ ethnicity + education3 + 
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- extract_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_quin9918_ml2.csv')
###per 10th-90th ----

iqr_AHEI=quantile(handle_dat$AHEI_ALL, probs = 0.9)-quantile(handle_dat$AHEI_ALL, probs = 0.1)
iqr_DASH=quantile(handle_dat$DASH_ALL, probs = 0.9)-quantile(handle_dat$DASH_ALL, probs = 0.1)
iqr_DII=quantile(handle_dat$DII_ALL, probs = 0.9)-quantile(handle_dat$DII_ALL, probs = 0.1)
iqr_HEI2020=quantile(handle_dat$HEI2020_ALL, probs = 0.9)-quantile(handle_dat$HEI2020_ALL, probs = 0.1)
iqr_MED=quantile(handle_dat$MED_ALL, probs = 0.9)-quantile(handle_dat$MED_ALL, probs = 0.1)

handle_dat$AHEI_iqr=handle_dat$AHEI_ALL/iqr_AHEI
handle_dat$DASH_iqr=handle_dat$DASH_ALL/iqr_DASH
handle_dat$DII_iqr=handle_dat$DII_ALL/iqr_DII
handle_dat$HEI2020_iqr=handle_dat$HEI2020_ALL/iqr_HEI2020
handle_dat$MED_iqr=handle_dat$MED_ALL/iqr_MED
#handle_dat$MEDI_iqr=handle_dat$MEDI_ALL/iqr_MEDI

iqr_index=c('HEI2020_iqr','AHEI_iqr','DASH_iqr','MED_iqr','DII_iqr')
BA_index=c('PhenoAgeAccel',"CardiacAgeAccel", "KidneyAgeAccel","LiverAgeAccel" ,'MuscleAgeAccel' )


design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~MEC10YR,
    data = handle_dat
  )#生成svy数据，格式固定
results <- list()
# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in iqr_index) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste(X_variable,'+age+sex', collapse = "")))
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    # Extract information
    info <- ext_info(model)
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_per_iqr_age_sex.csv')

results <- list()
# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in iqr_index) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste(X_variable,'+age + sex + ethnicity + education3 + Smoke+
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- ext_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_per_iqr_ml2.csv')

#(3) diet & mor----
survNhanes4A2J_1999.2018 <- read.csv("~/NHANES_diet/survNhanes4A2J_1999-2018.csv")
View(survNhanes4A2J_1999.2018)
survNhanes4A2J_1999_2018 <-survNhanes4A2J_1999.2018 #read_csv("C:/Users/xuxin/Desktop/06DietBAPack/06DietBAPack/survNhanes4A2J_1999-2018.csv")

handle_dat_quit <- handle_dat %>%
  mutate(across(all_of(diet_index), ~ntile(., 5)))  # Create quintiles
#handle_dat_quit$WTMEC2YR7 <- handle_dat_quit$WTMEC2YR / 7

handle_dat_mor=merge(handle_dat_quit,survNhanes4A2J_1999_2018,by='SEQN',all.x=T)
handle_dat_mor=handle_dat_mor[!is.na(handle_dat_mor$mortstat),]#34282


##quintiles----
diet_index=c('HEI2020_ALL',
             'AHEI_ALL',
             'DASH_ALL',
             'MED_ALL',
             'DII_ALL')

# Create quintiles for specified columns
# Rename columns
names(handle_dat_mor)[names(handle_dat_mor) %in% diet_index] <- paste0(names(handle_dat_mor)[names(handle_dat_mor) %in% diet_index], "_quintile")

handle_dat_mor[diet_indexx]=lapply(handle_dat_mor[diet_indexx], function(x) factor(x))
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_mor
  )#生成svy数据，格式固定

results <- list()

diet_indexx=c("HEI2020_ALL_quintile", "AHEI_ALL_quintile" ,
              "DASH_ALL_quintile"   , "MED_ALL_quintile",
              "DII_ALL_quintile"  )

# Loop over each combination of independent variables
extractcox_info <- function(model) {

  exp1=paste0(coef(model)[1] %>%exp()%>%round(2),'(',confint(model)[1,1]%>%exp()%>%round(2),',',confint(model)[1,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info1 <- data.frame(q2=exp1, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][1]%>%round(1))
  exp2=paste0(coef(model)[2] %>%exp()%>%round(2),'(',confint(model)[2,1]%>%exp()%>%round(2),',',confint(model)[2,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][2]%>%round(3))
  
  exp3=paste0(coef(model)[3] %>%exp()%>%round(2),'(',confint(model)[3,1]%>%exp()%>%round(2),',',confint(model)[3,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info3 <- data.frame(q2=exp3, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][3]%>%round(3))
  
  exp4=paste0(coef(model)[4] %>%exp()%>%round(2),'(',confint(model)[4,1]%>%exp()%>%round(2),',',confint(model)[4,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info4 <- data.frame(q2=exp4, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][4]%>%round(4))
  info=cbind(info1,info2)%>%cbind(info3,info4) %>% cbind((coef(model)[1]*(1)) %>%exp(),(confint(model)*(1))[1,1]%>%exp(),(confint(model)[1,2]*(1)) %>%exp(),
                                                         (coef(model)[2]*(1))%>%exp(),(confint(model)[2,1]*(1))%>%exp(),(confint(model)[2,2]*(1)) %>%exp(),
                                                         (coef(model)[3]*(1)) %>%exp(),(confint(model)[3,1]*(1))%>%exp(),(confint(model)[3,2]*(1)) %>%exp(),
                                                         (coef(model)[4]*(1)) %>%exp(),(confint(model)[4,1]*(1))%>%exp(),(confint(model)[4,2]*(1)) %>%exp())
  
  return(info)
}

for (X_variable in diet_indexx) {
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
  
  model <- svycoxph(formula, design = design_mor)
  # Extract information
  info <- extractcox_info(model)
  # Store results
  results[[paste(X_variable, collapse = "+")]] <- info
}


# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
diet_mor_quintile=results_df
names(diet_mor_quintile)<-c('q2','q2_p','q3','q3_p','q4','q4_p','q5','q5_p',
                               'q2_hr','q2_lci','q2_uci',
                               'q3_hr','q3_lci','q3_uci',
                               'q4_hr','q4_lci','q4_uci',
                               'q5_hr','q5_lci','q5_uci')
write.csv(diet_mor_quintile,file='diet_quintile_mor.csv')

#diet index and mortality per 10th-90th -----
iqr_AHEI=quantile(handle_dat$AHEI_ALL, probs = 0.9)-quantile(handle_dat$AHEI_ALL, probs = 0.1)
iqr_DASH=quantile(handle_dat$DASH_ALL, probs = 0.9)-quantile(handle_dat$DASH_ALL, probs = 0.1)
iqr_DII=quantile(handle_dat$DII_ALL, probs = 0.9)-quantile(handle_dat$DII_ALL, probs = 0.1)
iqr_HEI2020=quantile(handle_dat$HEI2020_ALL, probs = 0.9)-quantile(handle_dat$HEI2020_ALL, probs = 0.1)
iqr_MED=quantile(handle_dat$MED_ALL, probs = 0.9)-quantile(handle_dat$MED_ALL, probs = 0.1)
#iqr_MEDI=quantile(handle_dat$MEDI_ALL, probs = 0.9)-quantile(handle_dat$MEDI_ALL, probs = 0.1)

handle_dat$AHEI_iqr=handle_dat$AHEI_ALL/iqr_AHEI
handle_dat$DASH_iqr=handle_dat$DASH_ALL/iqr_DASH
handle_dat$DII_iqr=handle_dat$DII_ALL/iqr_DII
handle_dat$HEI2020_iqr=handle_dat$HEI2020_ALL/iqr_HEI2020
handle_dat$MED_iqr=handle_dat$MED_ALL/iqr_MED

handle_dat_mor=merge(handle_dat,survNhanes4A2J_1999_2018,by='SEQN',all.x=T)
handle_dat_mor=handle_dat_mor[!is.na(handle_dat_mor$mortstat),]#34282


extcox_info <- function(model) {
  exp2=paste0(coef(model)[1]%>%exp()%>%round(2),
              '(',confint(model)[1,1]%>%exp()%>%round(2),
              ',',confint(model)[1,2]%>%exp()%>%round(2),')')
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[1,6]%>%round(3),hr=coef(model)[1]%>%exp(),lci=confint(model)[1,1]%>%exp(),uci=confint(model)[1,2]%>%exp())
  return(info2)
}

iqr_index=c('HEI2020_iqr','AHEI_iqr','DASH_iqr','MED_iqr','DII_iqr')

#
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~  MEC10YR,
    data = handle_dat_mor
  )#生成svy数据，格式固定

results <- list()
k=1
for (X_variable in iqr_index) {
  # 创建公式
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
  
  # 拟合 svycoxph 模型
  model <- svycoxph(formula, design = design_mor)
  
  # 提取信息
  info <- extcox_info(model) %>%data.frame()
  
  # 存储结果
  results[[X_variable]] <- info
  k=k+1
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
diet_mor=results_df
write.csv(diet_mor,file='association_mortality_exp.csv')


#subgroup----

source("~/NHANES_diet/diet_subgroup/svyscitb5cox22.R")



#RRR----
library(rrpack)
#install.packages('rrpack')

response_vars <- "HEI2020_iqr"
predictor_vars <- HEI_columns[!grepl("(_ALL|_iqr|_NOETOH)$", HEI_columns)]
#formula_text <- paste(response_vars, "+AHEI_iqr+DII_iqr+pat2I_iqr~", paste(predictor_vars, collapse = "+"))

# 2. 构造公式
#formula <- as.formula(formula_text)
#response_vars <- "HEI2020_iqr"
#HEI_columns <-names(handle_dat)[grep("HEI2020_", names(handle_dat))]
#predictor_vars <- HEI_columns[!grepl("(_ALL|_iqr|_NOETOH)$", HEI_columns)]
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",'MuscleAgeAccel'))])
#colSums(is.na(handle_dat[,append(predictor_vars,c("CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel", 'MuscAgeAccel'))]))
# 重新定义 X 和 Y 矩阵，以排除含有缺失值的观测
X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])


# 拟合 RRR 模型
rrr_obj <- rrr.fit(Y = Y, X = X,coefSVD=T, nrank =5)
# 

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵


#aging-related pattern score & mor----
#hei2020----
predictor_vars=HEI_columns[!grepl("(_ALL|_iqr|_NOETOH)$", HEI_columns)]
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",
                                                          'MuscleAgeAccel','cancer','cvd','diabetes_status','age'))])

X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])

rrr_obj <- rrr.fit(Y = Y, X = X, nrank =5 )

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵

A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv #%>% as.data.frame()
L
score=as.matrix(scale(handle_dat[,predictor_vars]))%*% L %>%data.frame()
names(score)[1]=c('HEI2020_score')
handle_dat_rrr=cbind(handle_dat,score[,1])


#ahei----
AHEI_columns <- names(handle_dat)[grep("AHEI_", names(handle_dat))]
predictor_vars <- AHEI_columns[!grepl("(_ALL|_iqr|_NOETOH)$", AHEI_columns)]
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",
                                                          'MuscleAgeAccel','cancer','cvd','diabetes_status','age'))])

X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])

rrr_obj <- rrr.fit(Y = Y, X = X, nrank =5 )

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵

A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv #%>% as.data.frame()
L
score=as.matrix(scale(handle_dat[,predictor_vars]))%*% L %>%data.frame()
names(score)[1]=c('AHEI_score')
handle_dat_rrr=cbind(handle_dat_rrr,score[,1])

#dash----

DASH_columns <-names(handle_dat)[grep("DASH_", names(handle_dat))]
predictor_vars <- DASH_columns[!grepl("(_ALL|_iqr|_NOETOH)$", DASH_columns)]
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",
                                                          'MuscleAgeAccel','cancer','cvd','diabetes_status','age'))])

X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])

rrr_obj <- rrr.fit(Y = Y, X = X, nrank =5 )

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵

A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv #%>% as.data.frame()
L
score=as.matrix(scale(handle_dat[,predictor_vars]))%*% L %>%data.frame()
names(score)[1]=c('DASH_score')
handle_dat_rrr=cbind(handle_dat_rrr,score[,1])

#med----
MED_columns <- names(handle_dat %>%   select(starts_with("MED_")))
predictor_vars <- MED_columns[!grepl("(_ALL|_iqr|_NOETOH)$", MED_columns)]
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",
                                                          'MuscleAgeAccel','cancer','cvd','diabetes_status','age'))])

X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])

rrr_obj <- rrr.fit(Y = Y, X = X, nrank =5 )

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵

A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv #%>% as.data.frame()
L
score=as.matrix(scale(handle_dat[,predictor_vars]))%*% L %>%data.frame()
names(score)[1]=c('MED_score')
handle_dat_rrr=cbind(handle_dat_rrr,score[,1])

#DII----

DII_columns <- names(handle_dat)[39:66]
#DII_columns<-setdiff(DII_columns,'KCAL')
predictor_vars <-DII_columns
clean_data <- na.omit(handle_dat[,append(predictor_vars,c('PhenoAgeAccel',
                                                          "CardiacAgeAccel","KidneyAgeAccel", "LiverAgeAccel",
                                                          'MuscleAgeAccel','cancer','cvd','diabetes_status','age'))])

X <- as.matrix(scale(clean_data[,predictor_vars]))
Y <- as.matrix(clean_data[,c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
                             "LiverAgeAccel" ,'MuscleAgeAccel')])

rrr_obj <- rrr.fit(Y = Y, X = X, nrank =5 )

beta_rrr <- rrr_obj$coef         # RRR的系数矩阵

A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv #%>% as.data.frame()
L

L[8,1]=0.5826228
L[6,1]=0.14407493
L[9,1]=0.37023998
L[21,1]=0.18539329
L[17,1]=0.06
score=as.matrix(scale(handle_dat[,predictor_vars]))%*% L %>%data.frame()
names(score)[1]=c('DII_score')
handle_dat_rrr=cbind(handle_dat_rrr,score[,1])

colnames(handle_dat_rrr)[(ncol(handle_dat_rrr)-4):ncol(handle_dat_rrr)] <-c('HEI2020_score','AHEI_score','DASH_score',
                                                                            'MED_score','DII_score')


iqr_HEI2020_score=quantile(handle_dat_rrr$HEI2020_score, probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$HEI2020_score, probs = 0.1,na.rm=T)
iqr_AHEI_score=quantile(handle_dat_rrr$AHEI_score, probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$AHEI_score, probs = 0.1,na.rm=T)
iqr_DASH_score=quantile(handle_dat_rrr$DASH_score, probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$DASH_score, probs = 0.1,na.rm=T)
iqr_MED_score=quantile(handle_dat_rrr$MED_score, probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$MED_score, probs = 0.1,na.rm=T)
iqr_DII_score=quantile(handle_dat_rrr$DII_score, probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$DII_score, probs = 0.1,na.rm=T)
iqr_dii_score=quantile(handle_dat_rrr$dii_score,probs = 0.9,na.rm=T)-quantile(handle_dat_rrr$dii_score,probs = 0.1,na.rm=T)


handle_dat_rrr$iqr_HEI2020_score=handle_dat_rrr$HEI2020_score/iqr_HEI2020_score
handle_dat_rrr$iqr_AHEI_score=handle_dat_rrr$AHEI_score/iqr_AHEI_score
handle_dat_rrr$iqr_DASH_score=handle_dat_rrr$DASH_score/iqr_DASH_score
handle_dat_rrr$iqr_MED_score=handle_dat_rrr$MED_score/iqr_MED_score
handle_dat_rrr$iqr_DII_score=handle_dat_rrr$DII_score/iqr_DII_score
handle_dat_rrr$iqr_dii_score=handle_dat_rrr$dii_score/iqr_dii_score


iqr_index=c('iqr_HEI2020_score','iqr_AHEI_score','iqr_DASH_score','iqr_MED_score','iqr_DII_score','iqr_dii_score')

handle_dat_rrr_mor=merge(handle_dat_rrr,survNhanes4A2J_1999_2018,by='SEQN',all.x=T)
handle_dat_rrr_mor=handle_dat_rrr_mor[!is.na(handle_dat_rrr_mor$mortstat),]#34282



#per10-90 取方向一致----
extcox_info <- function(model) {
  exp2=paste0((coef(model)[1]*(-1))%>%exp()%>%round(2),
              '(',(confint(model)[1,1]*(-1))%>%exp()%>%round(2),
              ',',(confint(model)[1,2]*(-1))%>%exp()%>%round(2),')')
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[1,6]%>%round(3),
                      hr=(coef(model)[1]*(-1))%>%exp(),
                      lci=(confint(model)[1,1]*(-1))%>%exp(),
                      uci=(confint(model)[1,2]*(-1))%>%exp())
  return(info2)
}
#
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~  MEC10YR,
    data = handle_dat_rrr_mor
  )#生成svy数据，格式固定

iqr_index=c('iqr_HEI2020_score','iqr_AHEI_score','iqr_DASH_score','iqr_MED_score','iqr_DII_score','iqr_dii_score')

results <- list()
k=1
for (X_variable in iqr_index) {
  # 创建公式
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke+ 
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
  
  # 拟合 svycoxph 模型
  model <- svycoxph(formula, design = design_mor)
  
  # 提取信息
  info <- extcox_info(model) %>%data.frame()
  
  # 存储结果
  results[[X_variable]] <- info
  k=k+1
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
aging_score_results=results_df[c(1:5),]

write.csv(aging_score_results,file='aging_score_association_mortality_exp.csv')


#quintiles----
score_index=c('HEI2020_score','AHEI_score','DASH_score','MED_score','DII_score')
handle_dat_rrr_mor_quit <- handle_dat_rrr_mor %>%
  mutate(across(all_of(score_index), ~ntile(., 5)))  # Create quintiles

names(handle_dat_rrr_mor_quit)[names(handle_dat_rrr_mor_quit) %in% score_index] <- paste0(names(handle_dat_rrr_mor_quit)[names(handle_dat_rrr_mor_quit) %in% score_index], "_quintile")
score_indexx<-c("HEI2020_score_quintile", "AHEI_score_quintile",
                "DASH_score_quintile", "MED_score_quintile","DII_score_quintile"
                )
handle_dat_rrr_mor_quit[score_indexx]=lapply(handle_dat_rrr_mor_quit[score_indexx], function(x) factor(x))
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_rrr_mor_quit
  )#生成svy数据，格式固定

results <- list()


# Loop over each combination of independent variables
extractcox_info <- function(model) {
  
  exp1=paste0( (coef(model)[1]*(-1)) %>%exp()%>%round(2),'(',(confint(model)*(-1))[1,1]%>%exp()%>%round(2),',',(confint(model)[1,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info1 <- data.frame(q2=exp1, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][1]%>%round(1))
  exp2=paste0((coef(model)[2]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[2,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[2,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][2]%>%round(3))
  
  exp3=paste0( (coef(model)[3]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[3,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[3,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info3 <- data.frame(q2=exp3, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][3]%>%round(3))
  
  exp4=paste0( (coef(model)[4]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[4,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[4,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info4 <- data.frame(q2=exp4, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][4]%>%round(4))
  info=cbind(info1,info2)%>%cbind(info3,info4) %>% cbind((coef(model)[1]*(-1)) %>%exp(),(confint(model)*(-1))[1,1]%>%exp(),(confint(model)[1,2]*(-1)) %>%exp(),
                                                         (coef(model)[2]*(-1))%>%exp(),(confint(model)[2,1]*(-1))%>%exp(),(confint(model)[2,2]*(-1)) %>%exp(),
                                                         (coef(model)[3]*(-1)) %>%exp(),(confint(model)[3,1]*(-1))%>%exp(),(confint(model)[3,2]*(-1)) %>%exp(),
                                                         (coef(model)[4]*(-1)) %>%exp(),(confint(model)[4,1]*(-1))%>%exp(),(confint(model)[4,2]*(-1)) %>%exp())
  return(info)
}

for (X_variable in score_indexx) {
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
  
  model <- svycoxph(formula, design = design_mor)
  # Extract information
  info <- extractcox_info(model)
  # Store results
  results[[paste(X_variable, collapse = "+")]] <- info
}


# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
aging_score_quintile=results_df
names(aging_score_quintile)<-c('q2','q2_p','q3','q3_p','q4','q4_p','q5','q5_p',
                               'q2_hr','q2_lci','q2_uci',
                               'q3_hr','q3_lci','q3_uci',
                               'q4_hr','q4_lci','q4_uci',
                               'q5_hr','q5_lci','q5_uci')
write.csv(aging_score_quintile,file='diet_score_quintile_mor.csv')



#fig 2a. diet & aging-related diet score & mortality ----
aging_score_quintile$grp='aging-related diet score'
diet_mor_quintile$grp='diet score'
tmp=rbind(diet_mor_quintile,aging_score_quintile)
tmp$diet=c('HEI2020','AHEI','DASH','MED','DII',
           'HEI2020','AHEI','DASH','MED','DII')

tmpp_hr=tmp[,c('q2_hr',#'q2_lci','q2_uci',
            'q3_hr',#'q3_lci','q3_uci',
            'q4_hr',
            'q5_hr')]
tmpp_lci=tmp[,c('q2_lci',
                 'q3_lci',
                 'q4_lci',
                 'q5_lci')]
tmpp_uci=tmp[,c('q2_uci',
                 'q3_uci',
                 'q4_uci',
                 'q5_uci')]

temp=cbind(tmpp_hr %>% melt(),tmpp_lci %>% melt()) %>% cbind(tmpp_uci %>% melt())
temp=temp[,c(1,2,4,6)]
names(temp)<-c('var','hr','lci','uci')
temp$diet=c('HEI2020','AHEI','DASH','MED','DII',
            'HEI2020','AHEI','DASH','MED','DII',
            'HEI2020','AHEI','DASH','MED','DII',
            'HEI2020','AHEI','DASH','MED','DII')

temp$group=c(rep('diet score',5),
             rep('aging-related diet score',5),
             rep('diet score',5),
            rep('aging-related diet score',5),
            rep('diet score',5),
            rep('aging-related diet score',5),
            rep('diet score',5),
            rep('aging-related diet score',5)
            )
temp$diet=factor(temp$diet,levels=c('HEI2020','AHEI','DASH','MED','DII'))
temp=rbind(data.frame(var=rep('q1_hr',10),hr=rep(1,10),lci=rep(1,10),uci=rep(1,10),
           diet=c('HEI2020','AHEI','DASH','MED','DII',
                  c('HEI2020','AHEI','DASH','MED','DII')),
           group=c(rep('diet score',5),
                   rep('aging-related diet score',5))
           ),temp)
temp$variable=c(rep('Q1',10),rep('Q2',10),rep('Q3',10),rep('Q4',10),rep('Q5',10))
temp$diet=factor(temp$diet,levels=c('HEI2020','AHEI','DASH','MED','DII'))

pa=ggplot(temp, aes(x = variable, y = hr, ymin = lci, ymax = uci, color = group, group = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed",color='red')+
  facet_wrap(~ diet,ncol=5,scales = "free_y") +
  labs(title = NULL, x = NULL, y = NULL,legend=NULL) +
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "white"),       # 设置面板背景为白色
    plot.background = element_rect(fill = "white"),        # 设置图形背景为白色
    strip.background = element_rect(fill = "#2b3990"),     # 设置分面标签背景
    strip.text = element_text(size = 10, colour = "white"),
    legend.position = "top"
  ) +
  scale_color_manual(values = c("#E3882F", '#49548A'))  # 设置颜色为半透明
#fig 2b. diet & aging-related diet score & mortality ----
aging_score_results$grp='aging-related diet score'
diet_mor$grp='diet score'
tmp=rbind(aging_score_results,diet_mor)
tmp$diet=c('HEI2020','AHEI','DASH','MED','DII',
           'HEI2020','AHEI','DASH','MED','DII')

#tmp$diet=rownames(tmp)
#melt(tmp)
# 绘制森林图
tmp$diet=factor(tmp$diet,levels=c('HEI2020','AHEI','DASH','MED','DII'))
tmp$grp=factor(tmp$grp,levels=c('aging-related diet score','diet score'))
p=ggplot(tmp, aes(y = grp, x = hr,color=grp,group=diet)) +
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.1,color='black') +  # 画误差条表示置信区间  
  geom_vline(xintercept = 1, linetype = "dashed",color='red') +  # 添加一条虚线表示HR=1
  theme_minimal() +
  facet_wrap(~ diet,ncol=5,scales = "free_x") +
  labs(x = "HR per 10-90th percentile", y = "", title = NULL) +
  geom_point(size = 3,#@color='#873963',
             shape = 15) +  # 画点表示HR
  theme_bw()+
  theme(##分面背景更改为红色
  strip.background=element_rect(fill=c("#2b3990")),
  ##分面字体更改为白色
  strip.text=element_text(size=10,colour="white"),
  axis.text.y = element_blank(),  # 去掉y轴的标注
 # axis.ticks.y = element_blank()  # 去掉y轴的刻度线
  legend.position = "none" )+
  #scale_x_continuous(breaks = seq(0.5, 3.5, by = 0.5), limits = c(0.5, 3.5)) +
  scale_color_manual(values = c( "#E3882F",'#49548A'))

ggarrange(pa,p, nrow = 2, ncol = 1,heights = c(1.6, 1),labels=c('a','b'))

ggsave("fig2.pdf", plot = p, width = 10, height = 4, units = "in")
write.csv(tmp,file='source_data_diet_mor_per10_90.csv')





#-----
A <- rrr_obj$A                   # 右奇异矩阵
# 右奇异矩阵的转置
A_t <- t(A)
# 求解 A^T A 的逆（可以使用广义逆）
A_inv <- solve(A_t %*% A)
# 计算 L 矩阵 - 就是loading 矩阵 
L <- beta_rrr %*% A %*% A_inv %>% as.data.frame()
L
#names(L)<-c('PhenoAgeAccel',"CardiacAgeAccel","KidneyAgeAccel", 
#            "LiverAgeAccel" ,'MuscAgeAccel')
rownames(L)<-predictor_vars
L
# 提取拟合的响应变量
fitted_values <- rrr_obj$fitted

# 计算correlation (response scores) between factors and response variables obtained from reduced rank regression ----
correlation_matrix <- cor(Y, fitted_values)
print(correlation_matrix)

# predicted values
X %*%  coef_rrr

#Variations of response variables and predictor variables explained by each Reduced Rank Regression factor
#降秩回归因子解释的response/predictors的变异
# 提取所需组件
fitted_values <- rrr_obj$fitted
coef_rrr <- rrr_obj$coef
Ad <- rrr_obj$Ad           # 奇异值向量
A <- rrr_obj$A             # 右奇异矩阵
rank_rrr <- rrr_obj$rank   # 降秩



# variance of response----
# 初始化累计解释方差和容器
cumulative_variation_y <- 0  # 累积的方差解释百分比
  
# 初始化结果框架
output <- data.frame(Factor = character(), `R2 for Each Y` = character(), Average = numeric(), Cumulative_Average = numeric(), stringsAsFactors = FALSE)
  
# 开始处理每个降秩回归因子
for (i in 1:rank_rrr) {
    # 临时变量
    variation_y <- 0
    r2_vals <- c()  # 各个响应变量的 R² 值
    
    # 针对每个响应变量逐个回归处理
    for (j in 1:ncol(Y) ) {
      # 将当前因子 f_i 作为解释变量
      X_temp <- fitted_values[, i, drop=FALSE]
      
      # 回归 Y 的列 j 对当前 f_i
      model <- lm(Y[, j] ~ X_temp)
      r2 <- summary(model)$r.squared  # 提取当前回归模型的 R²
      
      variation_y <- variation_y + r2  # 累加当前的 R²
      r2_vals <- c(r2_vals, round(r2, 6))  # 保留每个响应变量的 R² 并四舍五入到 6 位
    }
    
    # 平均 R² 值（方差解释比例）
    avg_variation_y <- variation_y / ncol(Y)
    
    # 更新累计的方差解释百分比
    cumulative_variation_y <- cumulative_variation_y + avg_variation_y
    
    # 格式化每个响应变量的 R² 为字符串
    r2_str <- paste(r2_vals, collapse = "  ")
    
    # 将结果存放入数据框
    output <- rbind(output, data.frame(Factor = paste0("f", i), `R2 for Each Y` = r2_str, Average = avg_variation_y, Cumulative_Average = cumulative_variation_y, stringsAsFactors = FALSE))
}
  
# 打印结果
cat("\nVariations of Response Variables Explained by Each RRR Factor\n")
cat("---------------------------------------------------------------------\n")
print(output)
# 保存当前因子的解释变异和累积解释变异
variations[[paste0("f", i)]] <- list(Variation = variationy, Cumulative = cumulativey)
  
# 打印当前因子的解释变异信息
cat("Factor f", i, ": ", vy, " Average: ", variationy, " Cumulative Average: ", cumulativey, "\n")


#variance of predictors----
# 初始化累计解释方差和结果框架
cumulative_variation_x <- 0  # 累计的解释方差
output <- data.frame(Factor = character(), `R2 for Each X` = character(), Average = numeric(), Cumulative_Average = numeric(), stringsAsFactors = FALSE)

# 开始为每个降秩回归因子计算 R²
for (i in 1:rank_rrr) {
  # 临时变量
  variation_x <- 0
  r2_vals <- c()  # 用于存储每个预测变量的 R²
  
  # 对于每个预测变量，使用当前因子作为“自变量”进行回归
  for (j in 1:ncol(X)) {
    # 提取当前因子的拟合值 f 的向量
    X_temp <- fitted_values[, i, drop=FALSE]  # 第 `i` 个降秩因子
    
    # 对 X 的列 `j` 进行线性回归，使用 R 的 `lm()` 函数
    model <- lm(X[, j] ~ X_temp)
    r2 <- summary(model)$r.squared  # 提取当前回归的 R² 值
    
    # 累积该因子的方差解释能力
    variation_x <- variation_x + r2
    r2_vals <- c(r2_vals, round(r2, 6))  # 保留每个键值的对应 R² (四舍五入至 6 位)
  }
  
  # 计算当前因子的 average R² (即平均解释能力)
  avg_variation_x <- variation_x / ncol(X)
  
  # 更新累计解释方差比例
  cumulative_variation_x <- cumulative_variation_x + avg_variation_x
  
  # 将每一个预测变量的 R² 列为字符串
  r2_str <- paste(r2_vals, collapse = "  ")
  
  # 将结果保存到结果框架中
  output <- rbind(output, data.frame(
    Factor = paste0("f", i), 
    `R2 for Each X` = r2_str, 
    Average = avg_variation_x, 
    Cumulative_Average = cumulative_variation_x, 
    stringsAsFactors = FALSE
  ))
}

# 结果输出
cat("\n*Variations of Predictors Explained by Each RRR Factor*\n")
cat("---------------------------------------------------------------\n")
print(output)

#PhenoAgeCRP----

diet_index=c('HEI2020_ALL',
             'AHEI_ALL',
             'DASH_ALL_quintile',
             'MED_ALL',
             'DII_ALL')
BA_index=c('PhenoAgeAccel_crp' )

#handle_dat_quit$WTMEC2YR7 <- handle_dat_quit$WTMEC2YR / 7

design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_quit
  )#生成svy数据，格式固定

# Create a list to store results
results <- list()

diet_indexx=c("HEI2020_ALL_quintile", "AHEI_ALL_quintile" ,
              "DASH_ALL_quintile"   , "MED_ALL_quintile",
              "DII_ALL_quintile"  )


for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in diet_indexx) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste('as.factor(',X_variable,')+age + sex +Smoke+ ethnicity + education3 + 
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- extract_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_quin9918_PhenoAge_crp.csv')



results <- list()
# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in iqr_index) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste(X_variable,'+age + sex + ethnicity + education3 + Smoke+
    poverty3 + PA_2018 + BMI3 + hbp + cvd + diabetes_status + cancer + KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- ext_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_per_iqr_PhenoAge_crp.csv')

#Sen analysis 24h recall usual----
BA_index=c('PhenoAgeAccel',"CardiacAgeAccel", "KidneyAgeAccel","LiverAgeAccel" ,'MuscleAgeAccel' )

handle_dat_quit=merge(handle_dat_quit,TotalNutrientIntakes_df,by='SEQN',all.x=T)
handle_dat_quit$include=ifelse(!is.na(handle_dat_quit$CompareYesterday) & handle_dat_quit$CompareYesterday=='Usual',
                                 1,
                                 ifelse((!is.na(handle_dat_quit$CompareYesterday1) & handle_dat_quit$CompareYesterday1=='Usual'& !is.na(handle_dat_quit$CompareYesterday2) & handle_dat_quit$CompareYesterday2=='Usual'),
                                        1,0))
handle_dat_remove=handle_dat_quit[handle_dat_quit$include==1,]#20926 


# (1) diet & BAOA quintiles----
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_remove
  )#生成svy数据，格式固定

# Create a list to store results
results <- list()

diet_indexx=c("HEI2020_ALL_quintile", "AHEI_ALL_quintile" ,
              "DASH_ALL_quintile"   , "MED_ALL_quintile",
              "DII_ALL_quintile"  )


for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in diet_indexx) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste('as.factor(',X_variable,')+age + sex +Smoke+ ethnicity + education3 + 
    poverty3 + PA_2018 + BMI3 + hbp+cvd+cancer+diabetes_status  + KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- extract_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_quin9918_sen.csv')

#(2) diet & BAOA per iqr----

results <- list()
# Loop over each dependent variable
for (Y_variable in BA_index) {
  # Loop over each combination of independent variables
  for (X_variable in iqr_index) {
    # Formulate the formula for svyglm
    formula <- as.formula(paste(Y_variable, "~", paste(X_variable,'+age + sex + ethnicity + education3 + Smoke+
    poverty3 + PA_2018 + BMI3 + hbp + cvd+cancer+diabetes_status+ KCAL_mean', collapse = "")))
    
    # Fit svyglm model
    model <- svyglm(formula, design = design)
    
    
    # Extract information
    info <- ext_info(model)
    
    
    # Store results
    results[[paste(Y_variable, paste(X_variable, collapse = "+"), sep = "_")]] <- info
  }
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
write.csv(results_df,file='association_per_iqr_sen.csv')

#(3) mortality----

handle_dat_quit=merge(handle_dat_quit,TotalNutrientIntakes_df,by='SEQN',all.x=T)
handle_dat_quit$include=ifelse(!is.na(handle_dat_quit$CompareYesterday) & handle_dat_quit$CompareYesterday=='Usual',
                               1,
                               ifelse((!is.na(handle_dat_quit$CompareYesterday1) & handle_dat_quit$CompareYesterday1=='Usual'& !is.na(handle_dat_quit$CompareYesterday2) & handle_dat_quit$CompareYesterday2=='Usual'),
                                      1,0))
handle_dat_remove=handle_dat_quit[handle_dat_quit$include==1,]#20926 


handle_dat_mor=merge(handle_dat_mor,TotalNutrientIntakes_df,by='SEQN',all.x=T)
handle_dat_mor$include=ifelse(!is.na(handle_dat_mor$CompareYesterday) & handle_dat_mor$CompareYesterday=='Usual',
                               1,
                               ifelse((!is.na(handle_dat_mor$CompareYesterday1) & handle_dat_mor$CompareYesterday1=='Usual'& !is.na(handle_dat_mor$CompareYesterday2) & handle_dat_mor$CompareYesterday2=='Usual'),
                                      1,0))


handle_dat_mor_sen=handle_dat_mor[handle_dat_mor$include==1,]

design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_mor_sen
  )#生成svy数据，格式固定

results <- list()

diet_indexx=c("HEI2020_ALL_quintile", "AHEI_ALL_quintile" ,
              "DASH_ALL_quintile"   , "MED_ALL_quintile",
              "DII_ALL_quintile"  )

# Loop over each combination of independent variables
extractcox_info <- function(model) {
  
  exp1=paste0(coef(model)[1] %>%exp()%>%round(2),'(',confint(model)[1,1]%>%exp()%>%round(2),',',confint(model)[1,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info1 <- data.frame(q2=exp1, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][1]%>%round(1))
  exp2=paste0(coef(model)[2] %>%exp()%>%round(2),'(',confint(model)[2,1]%>%exp()%>%round(2),',',confint(model)[2,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][2]%>%round(3))
  
  exp3=paste0(coef(model)[3] %>%exp()%>%round(2),'(',confint(model)[3,1]%>%exp()%>%round(2),',',confint(model)[3,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info3 <- data.frame(q2=exp3, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][3]%>%round(3))
  
  exp4=paste0(coef(model)[4] %>%exp()%>%round(2),'(',confint(model)[4,1]%>%exp()%>%round(2),',',confint(model)[4,2] %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info4 <- data.frame(q2=exp4, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][4]%>%round(4))
  info=cbind(info1,info2)%>%cbind(info3,info4) %>% cbind((coef(model)[1]*(1)) %>%exp(),(confint(model)*(1))[1,1]%>%exp(),(confint(model)[1,2]*(1)) %>%exp(),
                                                         (coef(model)[2]*(1))%>%exp(),(confint(model)[2,1]*(1))%>%exp(),(confint(model)[2,2]*(1)) %>%exp(),
                                                         (coef(model)[3]*(1)) %>%exp(),(confint(model)[3,1]*(1))%>%exp(),(confint(model)[3,2]*(1)) %>%exp(),
                                                         (coef(model)[4]*(1)) %>%exp(),(confint(model)[4,1]*(1))%>%exp(),(confint(model)[4,2]*(1)) %>%exp())
  
  return(info)
}

for (X_variable in diet_indexx) {
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp+ cvd+cancer+diabetes_status + KCAL_mean', collapse = "")))
  
  model <- svycoxph(formula, design = design_mor)
  # Extract information
  info <- extractcox_info(model)
  # Store results
  results[[paste(X_variable, collapse = "+")]] <- info
}

# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
diet_mor_quintile=results_df
names(diet_mor_quintile)<-c('q2','q2_p','q3','q3_p','q4','q4_p','q5','q5_p',
                            'q2_hr','q2_lci','q2_uci',
                            'q3_hr','q3_lci','q3_uci',
                            'q4_hr','q4_lci','q4_uci',
                            'q5_hr','q5_lci','q5_uci')
write.csv(diet_mor_quintile,file='diet_quintile_mor_sen_removedisease.csv')


#per iqr----

extcox_info <- function(model) {
  exp2=paste0(coef(model)[1]%>%exp()%>%round(2),
              '(',confint(model)[1,1]%>%exp()%>%round(2),
              ',',confint(model)[1,2]%>%exp()%>%round(2),')')
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[1,6]%>%round(3),hr=coef(model)[1]%>%exp(),lci=confint(model)[1,1]%>%exp(),uci=confint(model)[1,2]%>%exp())
  return(info2)
}

iqr_index=c('HEI2020_iqr','AHEI_iqr','DASH_iqr','MED_iqr','DII_iqr')

#
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~  MEC10YR,
    data = handle_dat_mor_sen
  )#生成svy数据，格式固定

results <- list()
k=1
for (X_variable in iqr_index) {
  # 创建公式
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp+cvd+cancer+diabetes_status +  KCAL_mean', collapse = "")))
  
  # 拟合 svycoxph 模型
  model <- svycoxph(formula, design = design_mor)
  
  # 提取信息
  info <- extcox_info(model) %>%data.frame()
  
  # 存储结果
  results[[X_variable]] <- info
  k=k+1
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
diet_mor=results_df
write.csv(diet_mor,file='association_mortality_exp_sen.csv')


#(4) aging-related diet score----

#quintiles----
handle_dat_rrr_mor_quit=merge(handle_dat_rrr_mor_quit,TotalNutrientIntakes_df,by='SEQN',all.x=T)
handle_dat_rrr_mor_quit$include=ifelse(!is.na(handle_dat_rrr_mor_quit$CompareYesterday) & handle_dat_rrr_mor_quit$CompareYesterday=='Usual',
                               1,
                               ifelse((!is.na(handle_dat_rrr_mor_quit$CompareYesterday1) & handle_dat_rrr_mor_quit$CompareYesterday1=='Usual'& !is.na(handle_dat_rrr_mor_quit$CompareYesterday2) & handle_dat_rrr_mor_quit$CompareYesterday2=='Usual'),
                                      1,0))
handle_dat_rrr_mor_sen=handle_dat_rrr_mor_quit[handle_dat_rrr_mor_quit$include==1,]#20903

score_indexx<-c("HEI2020_score_quintile", "AHEI_score_quintile",
                "DASH_score_quintile", "MED_score_quintile","DII_score_quintile"
)
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat_rrr_mor_sen
  )#生成svy数据，格式固定

results <- list()


# Loop over each combination of independent variables
extractcox_info <- function(model) {
  
  exp1=paste0( (coef(model)[1]*(-1)) %>%exp()%>%round(2),'(',(confint(model)*(-1))[1,1]%>%exp()%>%round(2),',',(confint(model)[1,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info1 <- data.frame(q2=exp1, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][1]%>%round(1))
  exp2=paste0((coef(model)[2]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[2,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[2,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][2]%>%round(3))
  
  exp3=paste0( (coef(model)[3]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[3,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[3,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info3 <- data.frame(q2=exp3, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][3]%>%round(3))
  
  exp4=paste0( (coef(model)[4]*(-1)) %>%exp()%>%round(2),'(',(confint(model)[4,1]*(-1))%>%exp()%>%round(2),',',(confint(model)[4,2]*(-1)) %>%exp()%>%round(2),')')
  # Combine eszimazes, CI, and p-values
  info4 <- data.frame(q2=exp4, q2_p=summary(model)$coefficients[, "Pr(>|z|)"][4]%>%round(4))
  info=cbind(info1,info2)%>%cbind(info3,info4) %>% cbind((coef(model)[1]*(-1)) %>%exp(),(confint(model)*(-1))[1,1]%>%exp(),(confint(model)[1,2]*(-1)) %>%exp(),
                                                         (coef(model)[2]*(-1))%>%exp(),(confint(model)[2,1]*(-1))%>%exp(),(confint(model)[2,2]*(-1)) %>%exp(),
                                                         (coef(model)[3]*(-1)) %>%exp(),(confint(model)[3,1]*(-1))%>%exp(),(confint(model)[3,2]*(-1)) %>%exp(),
                                                         (coef(model)[4]*(-1)) %>%exp(),(confint(model)[4,1]*(-1))%>%exp(),(confint(model)[4,2]*(-1)) %>%exp())
  return(info)
}

for (X_variable in score_indexx) {
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke +
    poverty3 + PA_2018 + BMI3 + hbp +cvd+cancer+diabetes_status+ KCAL_mean', collapse = "")))
  
  model <- svycoxph(formula, design = design_mor)
  # Extract information
  info <- extractcox_info(model)
  # Store results
  results[[paste(X_variable, collapse = "+")]] <- info
}


# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
aging_score_quintile=results_df
names(aging_score_quintile)<-c('q2','q2_p','q3','q3_p','q4','q4_p','q5','q5_p',
                               'q2_hr','q2_lci','q2_uci',
                               'q3_hr','q3_lci','q3_uci',
                               'q4_hr','q4_lci','q4_uci',
                               'q5_hr','q5_lci','q5_uci')
write.csv(aging_score_quintile,file='diet_quintile_mor_sen.csv')


iqr_index=c('iqr_HEI2020_score','iqr_AHEI_score','iqr_DASH_score','iqr_MED_score','iqr_DII_score','iqr_dii_score')


#per10-90 取方向一致----
extcox_info <- function(model) {
  exp2=paste0((coef(model)[1]*(-1))%>%exp()%>%round(2),
              '(',(confint(model)[1,1]*(-1))%>%exp()%>%round(2),
              ',',(confint(model)[1,2]*(-1))%>%exp()%>%round(2),')')
  info2 <- data.frame(q2=exp2, q2_p=summary(model)$coefficients[1,6]%>%round(3),
                      hr=(coef(model)[1]*(-1))%>%exp(),
                      lci=(confint(model)[1,1]*(-1))%>%exp(),
                      uci=(confint(model)[1,2]*(-1))%>%exp())
  return(info2)
}
#
design_mor <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~  MEC10YR,
    data = handle_dat_rrr_mor_sen
  )#生成svy数据，格式固定

iqr_index=c('iqr_HEI2020_score','iqr_AHEI_score','iqr_DASH_score','iqr_MED_score','iqr_DII_score')

results <- list()
k=1
for (X_variable in iqr_index) {
  # 创建公式
  formula <- as.formula(paste('Surv(permth_int,mortstat)', "~", paste(X_variable, '+age + sex + ethnicity + education3 + Smoke+ 
    poverty3 + PA_2018 + BMI3 + hbp +cvd+cancer+diabetes_status+ KCAL_mean', collapse = "")))
  
  # 拟合 svycoxph 模型
  model <- svycoxph(formula, design = design_mor)
  
  # 提取信息
  info <- extcox_info(model) %>%data.frame()
  
  # 存储结果
  results[[X_variable]] <- info
  k=k+1
}
# Convert results to a data frame
results_df <- do.call(rbind.data.frame, results)
aging_score_results=results_df[c(1:5),]

write.csv(aging_score_results,file='aging_score_association_mortality_exp_sen.csv')


# weighted correlation between diet scores----
library(wCorr)
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat
  )#生成svy数据，格式固定



# 例如，假设要处理变量 var1, var2, var3, ... n个变量
vars <- diet_index  # 变量名向量

# 将所有变量组合成公式对象
formula <- as.formula(paste("~", paste(vars, collapse="+")))
bstrat<- as.svrepdesign(design,type="subbootstrap")
# 计算方差协方差矩阵，返回协方差和每个bootstrap的重复样本的结果
v <- svyvar(formula, bstrat, return.replicates = TRUE)

# 将协方差矩阵转为相关系数矩阵
vcor <- cov2cor(as.matrix(v))
# 打印相关系数矩阵
cat("Original correlated pairs (Weighted Pearson correlation):\n")
print(vcor)

# 提取bootstrap 重采样的协方差估计
vreps <- v$replicates

# 定义矩阵的维度
n_vars <- length(vars)


# 使用 apply 对 bootstrap 结果进行处理，计算每次重采样的相关系数值
correps_matrix <- apply(vreps, 1, function(row) {
  # 将单行重新恢复为原来的协方差矩阵
  cov_matrix <- matrix(row, n_vars, n_vars)
  
  # 将重采样的协方差矩阵转换为相关系数矩阵
  cor_matrix <- cov2cor(cov_matrix)
  
  # 返回上三角部分的相关系数
  return(cor_matrix[upper.tri(cor_matrix, diag = FALSE)])
})

# 获取原始样本对应的相关系数上三角部分
vcor_upper <- vcor[upper.tri(vcor, diag = FALSE)]
cat("Upper triangle of original correlation matrix:\n")
print(vcor_upper)

# 利用vcov函数计算相关系数的方差基于 bootstrap 估计
cat("\nBootstrap Variance of each correlated pair:\n")
for (i in 1:length(vcor_upper)) {
  # 计算 i-th 配对的方差
  var_estimate <- vcov(bstrat, correps_matrix[i, ], centre = vcor_upper[i])
  cat(paste("Variance of correlation pair", i, ":", var_estimate), "\n")
}

# 生成相关性矩阵的热力图

# 绘制相关性矩阵
cat("\nPlotting correlation matrix...\n")

new_labels <- c("HEI2020", "AHEI",'DASH','aMED','DII')
rownames(vcor) <- new_labels
colnames(vcor) <- new_labels
corrplot(vcor, 
         method = "color",  # 使用颜色块的表示方法
         type = "upper",    # 只显示上三角矩阵
         addCoef.col = "black",  # 在图中显示相关系数数值
         tl.col = "black",  # 标签的颜色
         tl.srt = 45,       # 标签的倾斜角度

         number.cex = 1,  # 控制数值显示的字体大小
         diag = FALSE,
         tl.lab = new_labels)      # 不显示对角线项



# weighted correlation between acceleration aging ----

design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat
    #data = handle_dat[!is.na(handle_dat$MuscleAgeAccel)&!is.na(handle_dat$PhenoAgeAccel) & !is.na(handle_dat$CardiacAgeAccel) & !is.na(handle_dat$KidneyAgeAccel)&!is.na(handle_dat$LiverAgeAccel),]
  )#生成svy数据，格式固定

#12878

# 例如，假设要处理变量 var1, var2, var3, ... n个变量
vars <- BA_index  # 变量名向量

# 将所有变量组合成公式对象
formula <- as.formula(paste("~", paste(vars, collapse="+")))
bstrat<- as.svrepdesign(design,type="subbootstrap")
# 计算方差协方差矩阵，返回协方差和每个bootstrap的重复样本的结果
v <- svyvar(formula, bstrat,na.rm=T, return.replicates = TRUE)

# 将协方差矩阵转为相关系数矩阵
vcor <- cov2cor(as.matrix(v))
# 打印相关系数矩阵
cat("Original correlated pairs (Weighted Pearson correlation):\n")
print(vcor)

# 提取bootstrap 重采样的协方差估计
vreps <- v$replicates

# 定义矩阵的维度
n_vars <- length(vars)


# 使用 apply 对 bootstrap 结果进行处理，计算每次重采样的相关系数值
correps_matrix <- apply(vreps, 1, function(row) {
  # 将单行重新恢复为原来的协方差矩阵
  cov_matrix <- matrix(row, n_vars, n_vars)
  
  # 将重采样的协方差矩阵转换为相关系数矩阵
  cor_matrix <- cov2cor(cov_matrix)
  
  # 返回上三角部分的相关系数
  return(cor_matrix[upper.tri(cor_matrix, diag = FALSE)])
})

# 获取原始样本对应的相关系数上三角部分
vcor_upper <- vcor[upper.tri(vcor, diag = FALSE)]
cat("Upper triangle of original correlation matrix:\n")
print(vcor_upper)

# 利用vcov函数计算相关系数的方差基于 bootstrap 估计
cat("\nBootstrap Variance of each correlated pair:\n")
for (i in 1:length(vcor_upper)) {
  # 计算 i-th 配对的方差
  var_estimate <- vcov(bstrat, correps_matrix[i, ], centre = vcor_upper[i])
  cat(paste("Variance of correlation pair", i, ":", var_estimate), "\n")
}

# 生成相关性矩阵的热力图

# 绘制相关性矩阵
cat("\nPlotting correlation matrix...\n")

new_labels <- c("PhenoAgeAccel" ,  "CardiacAgeAccel" ,"KidneyAgeAccel"  ,"LiverAgeAccel",   "MuscleAgeAccel")
rownames(vcor) <- new_labels
colnames(vcor) <- new_labels
corrplot(vcor, 
         method = "color",  # 使用颜色块的表示方法
         type = "upper",    # 只显示上三角矩阵
         addCoef.col = "black",  # 在图中显示相关系数数值
         tl.col = "black",  # 标签的颜色
         tl.srt = 45,       # 标签的倾斜角度
         
         number.cex = 1,  # 控制数值显示的字体大小
         diag = FALSE,
         tl.lab = new_labels)      # 不显示对角线项

# weighted correlation between anti-inflammation/anti-oxidation----
anti<-names(NHANES_inflammation_index)[-1]
handle_dat=merge(handle_dat,NHANES_inflammation_index,by='SEQN',all.x=T)
design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    nest = TRUE,
    weight = ~ MEC10YR,
    data = handle_dat
    #data = handle_dat[!is.na(handle_dat$MuscleAgeAccel)&!is.na(handle_dat$PhenoAgeAccel) & !is.na(handle_dat$CardiacAgeAccel) & !is.na(handle_dat$KidneyAgeAccel)&!is.na(handle_dat$LiverAgeAccel),]
  )#生成svy数据，格式固定


# 例如，假设要处理变量 var1, var2, var3, ... n个变量
vars =anti  # 变量名向量
# 初始化结果存储
cor_results <- list()
ci_results <- list()
bstrat<- as.svrepdesign(design,type="subbootstrap")
# 遍历每一对 (var, diet_index) 变量
for (var_name in vars) {
  for (diet_name in diet_index) {
    
    # 创建公式
    formula <- as.formula(paste("~", paste(c(var_name, diet_name), collapse = "+")))
    
    # 计算协方差矩阵，返回协方差和每个 bootstrap 的重复样本
    v <- svyvar(formula, bstrat, na.rm = TRUE, return.replicates = TRUE)
    
    # 转换为相关系数矩阵
    vcor <- cov2cor(as.matrix(v))
    cor_value <- vcor[1, 2]  # 相关系数
    
    # 提取 bootstrap 重采样的协方差估计
    vreps <- v$replicates
    n_vars <- 2  # 每次只计算一对变量
    
    # 使用 apply 对 bootstrap 重采样结果处理，计算相关系数
    correps <- apply(vreps, 1, function(row) {
      cov_matrix <- matrix(row, n_vars, n_vars)  # 恢复为协方差矩阵
      cor_matrix <- cov2cor(cov_matrix)         # 转换为相关系数矩阵
      return(cor_matrix[1, 2])                  # 提取相关系数
    })
    
    # 计算 bootstrap 的置信区间
    cor_ci <- quantile(correps, probs = c(0.025, 0.975))
    
    # 保存结果
    cor_results[[paste(var_name, diet_name, sep = "_vs_")]] <- cor_value
    ci_results[[paste(var_name, diet_name, sep = "_vs_")]] <- cor_ci
  }
}
cat("Weighted Pearson Correlations:\n")
print(cor_results)
# 将 cor_results 转换为矩阵形式

var_diet_pairs <- names(cor_results)
var_names <- unique(sapply(var_diet_pairs, function(x) strsplit(x, "_vs_")[[1]][1]))
diet_names <- unique(sapply(var_diet_pairs, function(x) strsplit(x, "_vs_")[[1]][2]))

# 创建空矩阵
cor_matrix <- matrix(NA, nrow = length(var_names), ncol = length(diet_names),
                     dimnames = list(var_names, diet_names))

# 填充矩阵
for (pair in var_diet_pairs) {
  vars <- strsplit(pair, "_vs_")[[1]]
  cor_matrix[vars[1], vars[2]] <- cor_results[[pair]]
}

# 转换矩阵为数据框以便 ggplot2 使用
cor_df <- melt(cor_matrix, na.rm = TRUE)
colnames(cor_df) <- c("Variable", "DietIndex", "Correlation")

# 使用 ggplot2 生成热图，并添加数值
ggplot(cor_df, aes(x = DietIndex, y = Variable, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +  # 显示数值，保留两位小数
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Weighted Correlations",
       x = "Diet Index",
       y = "Variable")


