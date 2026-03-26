library(haven)
library(dplyr)

## Open data
BES <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/BES/BES2024_W30_Panel_v30.0.sav")

library(dplyr)

# ----------------------------------------------------------
# 1. PANEL + GEOGRAPHY
# ----------------------------------------------------------

panel_vars <- unique(c(
  "id",
  paste0("wave", 1:30),
  paste0("starttimeW", 1:30),  # fieldwork timing
  paste0("gorW", 1:30),        # Government Office Region
  paste0("pconW", 1:30),       # Parliamentary Constituency
  paste0("pcon_codeW", 1:30)   # Constituency codes (more stable)
))

panel_vars <- intersect(panel_vars, names(BES))

# ----------------------------------------------------------
# 2. MAIN SUBSTANTIVE VARIABLES (ATI + Nationalism)
# ----------------------------------------------------------

# ATI: economic, cultural, self (ALL WAVES)
ati_vars <- c(
  grep("^immigEconW\\d+$",     names(BES), value = TRUE),
  grep("^immigCulturalW\\d+$", names(BES), value = TRUE),
  grep("^immigSelfW\\d+$",     names(BES), value = TRUE)
)

# Britishness across waves
britishness_vars <- grep("^britishnessW\\d+$", names(BES), value = TRUE)

# Wave 11 civic & ethnic nationalism items
ethnic_items <- c("britBornHereW11","britCitizenW11","britLiveHereW11","britChristianW11")
civic_items  <- c("britSpeakEnglishW11","britCustomsW11","britRespectLawW11","britFeelBritishW11")
nat_items_wave11 <- intersect(c(ethnic_items, civic_items), names(BES))

# ----------------------------------------------------------
# 3. SOCIODEMOGRAPHICS
# ----------------------------------------------------------

# AGE — includes ageW1..W30 + "Age"
age_vars <- grep("^age(W\\d+)?$", names(BES), value = TRUE)

# GENDER — “gender” is stable across waves
gender_vars <- intersect("gender", names(BES))

# EDUCATION — respondent education only (filter out gov-handling, etc.)
educ_vars <- grep("^p_educationW\\d+$", names(BES), value = TRUE)

# ETHNICITY — respondent ethnicity (p_ethnicityWxx)
ethnicity_vars <- grep("^p_ethnicityW\\d+$", names(BES), value = TRUE)

# RELIGION — respondent’s religion**
religion_vars <- c(
  grep("^p_religionW\\d+$", names(BES), value = TRUE),
  intersect(c("ImpReligW14", "impReligionW15", "religImportantW23"), names(BES))
)

# ----------------------------------------------------------
# 4. MICRO-LEVEL ECONOMIC CONTROLS
# ----------------------------------------------------------

econ_micro <- unique(c(
  grep("householdIncome|HHIncome|income", names(BES), value = TRUE, ignore.case = TRUE),
  grep("employ|unemploy", names(BES), value = TRUE, ignore.case = TRUE),
  grep("cvEconW\\d+$", names(BES), value = TRUE),   # personal economic evals
  grep("fin", names(BES), value = TRUE, ignore.case = TRUE) # financial situation
))

# Clean out irrelevant false matches
econ_micro <- econ_micro[!grepl("party|contact|education|environment", econ_micro, ignore.case = TRUE)]

# ----------------------------------------------------------
# 5. MESO-LEVEL ECONOMIC CONTEXT
# ----------------------------------------------------------

econ_meso <- unique(c(
  grep("^regionEconW\\d+$", names(BES), value = TRUE),
  grep("^localEcon(W|Now|1520Yr)", names(BES), value = TRUE),
  grep("localUnemployment_a_1W\\d+$", names(BES), value = TRUE),
  grep("^statusArea", names(BES), value = TRUE),   # neighbourhood conditions
  grep("areaRichPoorW\\d+$", names(BES), value = TRUE),
  grep("areaCrimeW\\d+$", names(BES), value = TRUE)
))

# ----------------------------------------------------------
# 6. COMBINE AND SUBSET
# ----------------------------------------------------------

bes_core_vars <- unique(c(
  panel_vars,
  ati_vars,
  britishness_vars,
  nat_items_wave11,
  age_vars,
  gender_vars,
  educ_vars,
  ethnicity_vars,
  religion_vars,
  econ_micro,
  econ_meso
))

BES_core <- BES %>%
  select(all_of(bes_core_vars))

# Optional: inspect results
length(bes_core_vars)
str(BES_core)
