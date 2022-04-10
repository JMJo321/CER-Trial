# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, CER
# #
# > Script Number(s)
# # : B-01-07A
# #
# > Purpose of the script(s)
# # : Append data field(s) in the CER survey data to the DT for regression
# #   results.

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
FILE_TO.LOAD_CER_FOR.REG <- "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For Pre- and Post-Trial Data
# # 1.2.1. For Pre-Trial Data
FILE_TO.LOAD_CER_SURVEY_PRE <- "CER_Survey_Electricity_Pre.parquet"
PATH_TO.LOAD_CER_SURVEY_PRE <- paste(
  PATH_DATA_INTERMEDIATE_CER_SURVEY, FILE_TO.LOAD_CER_SURVEY_PRE, sep = "/"
)
# # 1.2.1. For Post-Trial Data
FILE_TO.LOAD_CER_SURVEY_POST <- "CER_Survey_Electricity_Post.parquet"
PATH_TO.LOAD_CER_SURVEY_POST <- paste(
  PATH_DATA_INTERMEDIATE_CER_SURVEY, FILE_TO.LOAD_CER_SURVEY_POST, sep = "/"
)


# # 2. Path(s) to which Ouputs will be stored
# # 2.1. For the DT created to run regressions
FILE_TO.SAVE_CER_DT <-
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.SAVE_CER_DT <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.SAVE_CER_DT, sep = "/")


# ------- Define parameter(s) -------
# # 1. Create lists incl. answer codes for heating-type-related questions
# # 1.1. For space heating type (i.e., `question_id == 470`)
heating.type_space <- list(
  `1` = "Electricity (Electric Central Heating/Storage Heating)",
  `2` = "Electricity (Plug-In Heaters)",
  `3` = "Gas",
  `4` = "Oil",
  `5` = "Solid Fuel",
  `6` = "Renewable (e.g., Solar)",
  `7` = "Other"
)
# # 1.2. For water heating type (i.e., `question_id == 4701`)
heating.type_water <- list(
  `1` = "Central Heating System",
  `2` = "Electricity (Immersion)",
  `3` = "Electricity (Instantaneous Heater)",
  `4` = "Gas",
  `5` = "Oil",
  `6` = "Solid Fuel",
  `7` = "Renewable (e.g., Solar)",
  `8` = "Other"
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Append Data Field(s) in CER Survey Data to the DT for Regression Analysis
# ------------------------------------------------------------------------------
# ------- Load DT(s) required -------
# # 1. Load CER dataset
# # 1.1. DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)

# # 1.2. DTs for pre- and post-trial data
# # 1.2.1. DT for the pre-trial
dt_survey_pre <-
  read_parquet(PATH_TO.LOAD_CER_SURVEY_PRE) %>%
    setDT(.)
# # 1.2.2. DT for the post-trial
dt_survey_post <-
  read_parquet(PATH_TO.LOAD_CER_SURVEY_POST) %>%
    setDT(.)


# ------- Create DTs by modifying DTs incl. survey data -------
# # 1. For the pre-trial survey data
dt_survey_pre[
  question_id %like% "^470", .N, keyby = .(question_id, question_desc)
]
# ## Note:
# ## In the pre-trail survey, Question 470 and Question 4701 are multiple
# ## selection. But in the post-trial survey, they are not.

# # 1.1. Create a DT incl. data in wide-form
# # 1.1.1. Create a DT by converting the pre-trial survey data to wideform
dt_survey_pre_wideform <- dcast(
  dt_survey_pre[
    question_id %like% "(470_)|(4701_)",
    # To include heating-type-related questions
    .(id, question_id, answer_code)
  ],
  formula = id ~ question_id,
  value.var = "answer_code"
)
# # 1.1.2. Rename data fields
names_old_space <- paste0("470_", 1:7)
names_new_space <- paste0("heating.type_space_", 1:7)
setnames(dt_survey_pre_wideform, names_old_space, names_new_space)
names_old_water <- paste0("4701_", 1:8)
names_new_water <- paste0("heating.type_water_", 1:8)
setnames(dt_survey_pre_wideform, names_old_water, names_new_water)

# # 1.2. Add data fields
dt_survey_pre_wideform[
  ,
  is_elec.heating_space :=
    heating.type_space_1 == 1 | heating.type_space_2 == 1
]
dt_survey_pre_wideform[
  ,
  is_elec.heating_water :=
    heating.type_water_2 == 1 | heating.type_water_3 == 1
]
dt_survey_pre_wideform[
  ,
  is_elec.heating_and :=
    is_elec.heating_space == TRUE & is_elec.heating_water == TRUE
]
dt_survey_pre_wideform[
  ,
  is_elec.heating_or :=
    is_elec.heating_space == TRUE | is_elec.heating_water == TRUE
]

# # 1.3. Create a DT by subsetting the DT above
cols_indicators <- c(
  "is_elec.heating_space", "is_elec.heating_water",
  "is_elec.heating_and", "is_elec.heating_or"
)
dt_survey_pre_wideform[, .N, keyby = cols_indicators]
subdt_survey_pre <-
  dt_survey_pre_wideform[, .SD, .SDcols = c("id", cols_indicators)]


# # 2. For the post-trial survey data
dt_survey_post[, .N, by = .(id, question_id)][N > 1][, .N, by = .(id)]
# ## Note:
# ## The result shows that there are duplicate rows for `id == 5059`.

# # 2.1. Create a DT incl. data in wide-form
# # 2.1.1. Create a DT by converting the post-trial survey data to wideform
dt_survey_post_wideform <- dcast(
  dt_survey_post[
    question_id %in% c("470", "4701"), .(id, question_id, answer_code)
  ] %>%
    unique(.),
  formula = id ~ question_id,
  value.var = "answer_code"
)
# # 2.1.2. Rename data fields
names_old <- c("470", "4701")
names_new <- c("heating.type_space", "heating.type_water")
setnames(dt_survey_post_wideform, names_old, names_new)

# # 2.2. Add data fields
dt_survey_post_wideform[
  ,
  is_elec.heating_space :=
    heating.type_space %in% c(1, 2)
]
dt_survey_post_wideform[
  ,
  is_elec.heating_water :=
    heating.type_water %in% c(2, 3)
]
dt_survey_post_wideform[
  ,
  is_elec.heating_and :=
    is_elec.heating_space == TRUE & is_elec.heating_water == TRUE
]
dt_survey_post_wideform[
  ,
  is_elec.heating_or :=
    is_elec.heating_space == TRUE | is_elec.heating_water == TRUE
]

# # 2.3. Create a DT by subsetting the DT above
dt_survey_post_wideform[, .N, keyby = cols_indicators]
subdt_survey_post <-
  dt_survey_post_wideform[, .SD, .SDcols = c("id", cols_indicators)]


# ------- Merge the DTs created, and then peform simple tests -------
# 1. Do simple tests before merging DTs
subdt_survey_pre[, .N] == subdt_survey_post[, .N]
# ## Note:
# ## This result implies that the number of respondents in the pre-trial survey
# ## is different from that in the post-trial survey.

n_both <- dt_for.reg[
  , .N, keyby = .(id)
][
  id %in% dt_survey_pre_wideform$id & id %in% dt_survey_post_wideform$id,
  .N
]
n_only.pre <- dt_for.reg[
  , .N, keyby = .(id)
][
  id %in% dt_survey_pre_wideform$id & (!id %in% dt_survey_post_wideform$id),
  .N
]
n_only.post <- dt_for.reg[
      , .N, keyby = .(id)
    ][
      (!id %in% dt_survey_pre_wideform$id) & id %in% dt_survey_post_wideform$id,
      .N
    ]
n_none <- dt_for.reg[
  , .N, keyby = .(id)
][
  (!id %in% dt_survey_pre_wideform$id) & (!id %in% dt_survey_post_wideform$id),
  .N
]
stopifnot(
  dt_for.reg[, .N, by = .(id)][, .N] ==
    n_both + n_only.pre + n_only.post + n_none
)


# # 2. Merge DTs
# # 2.1. Create a DT by merging the DTs in wide-form
dt_to.append <- merge(
  x = subdt_survey_pre,
  y = subdt_survey_post,
  by = "id",
  suffixes = c("_pre", "_post"),
  all = TRUE
)

# # 2.2. Perform simple tests
# # 2.2.1. Check the number of observations
stopifnot(dt_to.append[, .N, by = .(id)][N > 1] == 0)
stopifnot(
  dt_to.append[id %in% dt_for.reg[, .N, by = .(id)]$id, .N] ==
    n_both + n_only.pre + n_only.post
)
# # 2.2.2. Do simple tests after merging DTs to check
# # 2.2.2.1. With respect to space heating
dt_to.append[
  is_elec.heating_space_pre != is_elec.heating_space_post, .N
]
dt_to.append[
  is_elec.heating_space_pre == TRUE & is_elec.heating_space_post == TRUE, .N
]
dt_to.append[
  is_elec.heating_space_pre == TRUE & is_elec.heating_space_post == FALSE, .N
]
dt_to.append[
  is_elec.heating_space_pre == FALSE & is_elec.heating_space_post == TRUE, .N
]
dt_to.append[
  is_elec.heating_space_pre == FALSE & is_elec.heating_space_post == FALSE, .N
]
# # 2.2.2.2. With respect to water heating
dt_to.append[
  is_elec.heating_water_pre != is_elec.heating_water_post, .N
]
dt_to.append[
  is_elec.heating_water_pre == TRUE & is_elec.heating_water_post == TRUE, .N
]
dt_to.append[
  is_elec.heating_water_pre == TRUE & is_elec.heating_water_post == FALSE, .N
]
dt_to.append[
  is_elec.heating_water_pre == FALSE & is_elec.heating_water_post == TRUE, .N
]
dt_to.append[
  is_elec.heating_water_pre == FALSE & is_elec.heating_water_post == FALSE, .N
]

# ## Note:
# ## Those results mean:
# ## 1) Non-electric energy source are utilized for space or water heating.
# ## 2) With respect to water heating, many households change their way of
# ##    heating from electric to non-electric energy source.



# ------- Append the DT incl. survey data to the DT for reg. analysis -------
dt_for.reg_append <- merge(
  x = dt_for.reg,
  y = dt_to.append,
  by = "id",
  all.x = TRUE
)


# ------- Save the extended DT in .parquet format -------
write_parquet(
  dt_for.reg_append,
  sink = PATH_TO.SAVE_CER_DT,
  compression = "snappy",
  use_dictionary = TRUE
)
