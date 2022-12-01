# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-05B
# #
# > Purpose of the script(s)
# # : For the baseline period, compute summary statistics.


# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "CER-Trial"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which dataset(s) and script(s) are loaded
# # 1.1. For the DT for regression analysis
FILE_TO.LOAD_CER_FOR.REG <-
  "CER_SubDT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")


FILE_TO.LOAD_CER_SURVEY_PRE <-
  "CER_Survey_Electricity_Pre.parquet"
PATH_TO.LOAD_CER_SURVEY_PRE <- paste(
  PATH_DATA_INTERMEDIATE_CER_SURVEY, FILE_TO.LOAD_CER_SURVEY_PRE, sep = "/"
)

FILE_TO.LOAD_CER_SURVEY_POST <-
  "CER_Survey_Electricity_Post.parquet"
PATH_TO.LOAD_CER_SURVEY_POST <- paste(
  PATH_DATA_INTERMEDIATE_CER_SURVEY, FILE_TO.LOAD_CER_SURVEY_POST, sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load dataset(s) and/or script(s) required to compute summary statistics
# ------------------------------------------------------------------------------
# ------- Load necessary dataset(s) and/or script(s) -------
# # 1. Load required DT(s)
# # 1.1. Load the DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)

# # 1.2. Load the DT for survey data
# # 1.2.1. For the baseline period
dt_survey_pre <-
  read_parquet(PATH_TO.LOAD_CER_SURVEY_PRE) %>%
    setDT(.)
# # 1.2.2. For the treatment period
dt_survey_post <-
  read_parquet(PATH_TO.LOAD_CER_SURVEY_POST) %>%
    setDT(.)
gc(reset = TRUE)


# ------------------------------------------------------------------------------
# Create DT(s) incl. consumption-related summary statistics
# ------------------------------------------------------------------------------
# ------- Create object(s) that will be used later -------
# # 1. An array that includes IDs
ids <-
  dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE, .N, by = .(id)
  ] %>%
    .$id
stopifnot(
  dt_survey_pre[id %in% ids, .N, by = .(id)][, .N] ==
    dt_survey_post[id %in% ids, .N, by = .(id)][, .N]
)


# ------- Add during-baseline-period consumption information -------
# # 1. Create a temporary DT that will be exploited to compute
# #    consumption-related summary statistics
tmp_dt_kwh <- dt_for.reg[
  id %in% ids & period == "Baseline",
  .(id, group, date, rate.period, kwh_per.hour)
][
  , .N, by = .(id, group, date, rate.period, kwh_per.hour)
][
  order(id)
]

# # 2. Compute Household-level consumption
# # 2.1. For hourly consumption
dt_kwh_hourly <- tmp_dt_kwh[
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group)
]
setnames(dt_kwh_hourly, "kwh_per.hour", "kwh_hourly")

# # 2.2. For daily consumption
dt_kwh_daily <- tmp_dt_kwh[
  ,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group, date)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group)
]
setnames(dt_kwh_daily, "kwh_per.hour", "kwh_daily")

# # 2.3. For consumption by rate period
# # 2.3.1. For the night rate period
dt_kwh_night <- tmp_dt_kwh[
  rate.period == "Night",
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group)
]
setnames(dt_kwh_night, "kwh_per.hour", "kwh_night.hourly")
# # 2.3.2. For the day rate period
dt_kwh_day <- tmp_dt_kwh[
  rate.period == "Day",
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group)
]
setnames(dt_kwh_day, "kwh_per.hour", "kwh_day.hourly")
# # 2.3.3. For the peak rate period
dt_kwh_peak <- tmp_dt_kwh[
  rate.period == "Peak",
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(id, group)
]
setnames(dt_kwh_peak, "kwh_per.hour", "kwh_peak.hourly")

# # 2.4. Create a DT by merging the DTs generated above
dt_kwh <-
  merge(x = dt_kwh_hourly, y = dt_kwh_daily, by = c("id", "group")) %>%
    merge(x = ., y = dt_kwh_night, by = c("id", "group")) %>%
    merge(x = ., y = dt_kwh_day, by = c("id", "group")) %>%
    merge(x = ., y = dt_kwh_peak, by = c("id", "group"))


# ------- Create a DT incl. consumption-related summary statistics -------
# # 1. Compute summary statistics
# # 1.1. For means
cols_to.compute <- names(dt_kwh)[str_detect(names(dt_kwh), "kwh")]
table_kwh_mean <- dt_kwh[
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_to.compute,
  by = .(group)
][
  ,
  lapply(.SD, round, digits = 3), .SDcols = cols_to.compute,
  by = .(group)
] %>%
  melt(., id.vars = "group") %>%
  dcast(., variable ~ group)

# # 1.2. For SEs
table_kwh_se <- dt_kwh[
  , N := .N, by = .(group)
][
  ,
  lapply(.SD, sd, na.rm = TRUE), .SDcols = cols_to.compute,
  by = .(group, N)
] %>%
  melt(., id.vars = c("group", "N")) %>%
  dcast(., variable + N ~ group) %>%
  .[
    ,
    `:=` (se_control = Control / sqrt(N), se_treatment = Treatment / sqrt(N))
  ] %>%
  melt(., id.vars = "variable", variable.name = "var") %>%
  .[var %in% c("se_control", "se_treatment") & !is.na(value)] %>%
  dcast(
    ., variable ~ var, value.var = "value", drop = TRUE
  ) %>%
  .[
    ,
    lapply(.SD, round, digits = 3), .SDcols = c("se_control", "se_treatment"),
    by = .(variable)
  ]

# # 1.3. For Differences
table_kwh_diff <- setDT(NULL)
for (col.name in cols_to.compute) {
  tmp_test.results <-
    t.test(formula(paste0(col.name, " ~ group")), data = dt_kwh)
  tmp_diff <- tmp_test.results$estimate %>% diff(.)
  tmp_se <- tmp_test.results$stderr
  tmp_p.value <- tmp_test.results$p.value
  tmp_dt <- data.table(
    variable = col.name,
    diff = round(tmp_diff, digits = 3),
    se = round(tmp_se, digits = 3),
    p.value = round(tmp_p.value, digits = 3)
  )
  table_kwh_diff <- rbind(table_kwh_diff, tmp_dt)
}


# # 2. Create a DT by merging the DTs created above
table_kwh <-
  merge(x = table_kwh_mean, y = table_kwh_se, by = "variable") %>%
    merge(x = ., y = table_kwh_diff, by = "variable")
names(table_kwh) <- names(table_kwh) %>% tolower(.)
setcolorder(table_kwh, c(1:2, 4))


# ------------------------------------------------------------------------------
# Create DT(s) incl. socioeconomic-condition-related summary statistics
# ------------------------------------------------------------------------------
# ------- Create object(s) that will be used later -------
# # 1. An empty DT that will utilized to add summary statistics
dt_summary <- setDT(NULL)
tmp_dt_summary <- dt_for.reg[id %in% ids, .N, by = .(id, group)][, .(id, group)]


# ------- Add demographic information -------
# # 1. Question ID 300: Age 65+, as an indicator variable
dt_age <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 300,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 300,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_age[
  ,
  `:=` (
    desc = "Age: 65+?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre == 6)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_age,
    by = "id",
    all.x = TRUE
  )
)


# # 2. Question ID 310: Employment, as an indicator variable
dt_employment <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 310,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 310,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_employment[
  ,
  `:=` (
    desc = "Employment: Unemployed?",
    # desc = "Employment: Employed?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre %in% 4:5)
    # value = as.numeric(answer_code_pre %in% 1:3)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_employment,
    by = "id",
    all.x = TRUE
  )
)


# # 3. Question ID 4021: Income, as an indicator variable
# # Note: Do not use income info. because many households did not answer.


# # 4. Question ID 420: No. of people (15+) in household
dt_no.of.15plus <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 420,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 420,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_no.of.15plus[
  ,
  `:=` (
    desc = "No. of 15+ People",
    is_indicator = FALSE,
    value = answer_code_pre
  )
]
dt_no.of.15plus[
  is.na(answer_code_pre) & !is.na(answer_code_post),
  value := answer_code_post
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_no.of.15plus,
    by = "id",
    all.x = TRUE
  )
)


# # 5. Question ID 43111: No. of people (15-) in household
dt_no.of.15minus <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 43111,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 43111,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_no.of.15minus[
  ,
  `:=` (
    desc = "No. of 15- People",
    is_indicator = FALSE,
    value = answer_code_pre
  )
]
dt_no.of.15minus[
  is.na(answer_code_pre) & !is.na(answer_code_post),
  value := answer_code_post
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_no.of.15minus,
    by = "id",
    all.x = TRUE
  )
)


# # 6. Question ID 5418: Education, as an indicator variable
# # 6.1. Primary or less
dt_education_primary <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 5418,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 5418,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_education_primary[
  ,
  `:=` (
    desc = "Education: Primary or less?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre %in% 1:2)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_education_primary,
    by = "id",
    all.x = TRUE
  )
)

# # 6.2. Secondary
dt_education_secondary <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 5418,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 5418,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_education_secondary[
  ,
  `:=` (
    desc = "Education: Secondary?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre %in% 3:4)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_education_secondary,
    by = "id",
    all.x = TRUE
  )
)


# ------- Add information about housing characteristics -------
# # 1. Question ID 452: Owner?, as an indicator variable
dt_owner <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 452,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 452,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_owner[
  ,
  `:=` (
    desc = "House: Owned?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre %in% 3:4)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_owner,
    by = "id",
    all.x = TRUE
  )
)


# # 2. Question ID 4531: 10+?, as an indicator variable
# # Note: Do not use this question because many households did not answer.


# # 3. Question ID 460: No. of bedrooms
dt_no.of.beds <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 460,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 460,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_no.of.beds[
  ,
  `:=` (
    desc = "House: No. of Bedrooms",
    is_indicator = FALSE,
    value = answer_code_pre
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_no.of.beds,
    by = "id",
    all.x = TRUE
  )
)


# # 4. Question ID 47001: Timer for space heating control?, as an indicator
# #    variable
dt_heating.control <- merge(
  x = dt_survey_pre[
    id %in% ids & question_id == 47001,
    .(id, question_id, answer_code)
  ],
  y = dt_survey_post[
    id %in% ids & question_id == 47001,
    .(id, question_id, answer_code)
  ],
  by = c("id", "question_id"),
  all.x = TRUE,
  suffixes = c("_pre", "_post")
)
dt_heating.control[
  ,
  `:=` (
    desc = "House: Timer for space heating control?",
    is_indicator = TRUE,
    value = as.numeric(answer_code_pre == 1)
  )
]

dt_summary <- rbind(
  dt_summary,
  merge(
    x = tmp_dt_summary,
    y = dt_heating.control,
    by = "id",
    all.x = TRUE
  )
)


# ------- Create a DT incl. summary statistics -------
# # 1. Compute summary statistics
# # 1.1. For means
table_summary_mean <- dt_summary[
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "value",
  by = .(desc, group)
][
  ,
  lapply(.SD, round, digits = 3), .SDcols = "value",
  by = .(desc, group)
] %>%
  dcast(., desc ~ group)

# # 1.2. For SEs
table_summary_se <- dt_summary[
  , N := .N, by = .(desc, group)
][
  ,
  lapply(.SD, sd, na.rm = TRUE), .SDcols = "value",
  by = .(desc, group, N)
] %>%
  .[, se := value / sqrt(N)] %>%
  dcast(., desc ~ group, value.var = "se") %>%
  .[
    ,
    lapply(.SD, round, digits = 3), .SDcols = c("Control", "Treatment"),
    by = .(desc)
  ]
names(table_summary_se) <- c("desc", "se_control", "se_treatment")

# # 1.3. For differences
table_summary_diff <- setDT(NULL)
for (desc_ in dt_summary[, .N, by = .(desc)]$desc) {
  tmp_test.results <- t.test(
    formula(value ~ group), data = dt_summary[desc == desc_]
  )
  tmp_diff <- tmp_test.results$estimate %>% diff(.)
  tmp_se <- tmp_test.results$stderr
  tmp_p.value <- tmp_test.results$p.value
  tmp_dt <- data.table(
    desc = desc_,
    diff = round(tmp_diff, digits = 3),
    se = round(tmp_se, digits = 3),
    p.value = round(tmp_p.value, digits = 3)
  )
  table_summary_diff <- rbind(table_summary_diff, tmp_dt)
}


# # 2. Create a DT by merging the DTs created above
table_summary <-
  merge(x = table_summary_mean, y = table_summary_se, by = "desc") %>%
    merge(x = ., y = table_summary_diff, by = "desc")
names(table_summary) <- names(table_summary) %>% tolower(.)
setcolorder(table_summary, c(1:2, 4))
