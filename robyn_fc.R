library(Robyn)
packageVersion("Robyn")

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

create_files <- TRUE

data = read.csv("C:/Users/alvaro.trindade_ghfl/Desktop/R/robyn_mmm/FC/MMM_fc.csv")
head(dt_prophet_holidays)

robyn_directory <- "C:/Users/alvaro.trindade_ghfl/Desktop/R/robyn_mmm/FC"

# Input do modelo
InputCollect <- robyn_inputs(
  dt_input = data,
  dt_holidays = dt_prophet_holidays,
  date_var = "Dia", # date format must be "2020-01-01"
  dep_var = "Receita", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "BR", # input country code. Check: dt_prophet_holidays
  context_vars = c("Custo_mkt"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("Google", "Meta", "Criteo", "Blue", "Awin","Bing"), # mandatory input
  paid_media_vars = c("Google", "Meta", "Criteo", "Blue", "Awin","Bing"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = "Organic", # marketing activity without media spend
  #factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2023-10-01",
  window_end = "2024-07-07",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)


hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)

hyper_limits()

# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  Google_alphas = c(0.5, 3),
  Google_gammas = c(0.3, 1),
  Google_thetas = c(0, 0.3),
  Meta_alphas = c(0.5, 3),
  Meta_gammas = c(0.3, 1),
  Meta_thetas = c(0, 0.3),
  Criteo_alphas = c(0.5,3),
  Criteo_gammas = c(0.3, 1),
  Criteo_thetas = c(0, 0.3),
  Blue_alphas = c(0.5, 3),
  Blue_gammas = c(0.3, 1),
  Blue_thetas = c(0, 0.3),
  Awin_alphas = c(0.5, 3),
  Awin_gammas = c(0.3, 1),
  Awin_thetas = c(0, 0.3),
  Bing_alphas = c(0.5, 3),
  Bing_gammas = c(0.3, 1),
  Bing_thetas = c(0, 0.3),
  Organic_alphas = c(0.5, 3),
  Organic_gammas = c(0.3, 1),
  Organic_thetas = c(0, 0.3),
  train_size = c(0.5, 0.8)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

#### 2a-4: Fourth (optional), model calibration / add experimental input

## Guide for calibration

# 1. Calibration channels need to be paid_media_spends or organic_vars names.
# 2. We strongly recommend to use Weibull PDF adstock for more degree of freedom when
# calibrating Robyn.
# 3. We strongly recommend to use experimental and causal results that are considered
# ground truth to calibrate MMM. Usual experiment types are identity-based (e.g. Meta
# conversion lift) or geo-based (e.g. Meta GeoLift). Due to the nature of treatment
# and control groups in an experiment, the result is considered immediate effect. It's
# rather impossible to hold off historical carryover effect in an experiment. Therefore,
# only calibrates the immediate and the future carryover effect. When calibrating with
# causal experiments, use calibration_scope = "immediate".
# 4. It's controversial to use attribution/MTA contribution to calibrate MMM. Attribution
# is considered biased towards lower-funnel channels and strongly impacted by signal
# quality. When calibrating with MTA, use calibration_scope = "immediate".
# 5. Every MMM is different. It's highly contextual if two MMMs are comparable or not.
# In case of using other MMM result to calibrate Robyn, use calibration_scope = "total".
# 6. Currently, Robyn only accepts point-estimate as calibration input. For example, if
# 10k$ spend is tested against a hold-out for channel A, then input the incremental
# return as point-estimate as the example below.
# 7. The point-estimate has to always match the spend in the variable. For example, if
# channel A usually has $100K weekly spend and the experimental holdout is 70%, input
# the point-estimate for the $30K, not the $70K.
# 8. If an experiment contains more than one media variable, input "channe_A+channel_B"
# to indicate combination of channels, case sensitive.

# calibration_input <- data.frame(
# channel name must in paid_media_vars
# channel = c("Google", "Meta", "Criteo", "Blue", "Awin"),
# liftStartDate must be within input data range
# liftStartDate = as.Date(c("2018-05-01", "2018-04-03", "2018-07-01", "2017-12-01")),
# liftEndDate must be within input data range
# liftEndDate = as.Date(c("2018-06-10", "2018-06-03", "2018-07-20", "2017-12-31")),
# Provided value must be tested on same campaign level in model and same metric as dep_var_type
# liftAbs = c(400000, 300000, 700000, 200),
#   # Spend within experiment: should match within a 10% error your spend on date range for each channel from dt_input
# spend = c(421000, 7100, 350000, 0),
#   # Confidence: if frequentist experiment, you may use 1 - pvalue
# confidence = c(0.85, 0.8, 0.99, 0.95),
#   # KPI measured: must match your dep_var
# metric = c("revenue", "revenue", "revenue", "revenue"),
#   # Either "immediate" or "total". For experimental inputs like Meta Lift, "immediate" is recommended.
# calibration_scope = c("immediate", "immediate", "immediate", "immediate")
# )
# InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)


################################################################
#### Step 2b: For known model specification, setup in one single step

## Specify hyperparameters as in 2a-2 and optionally calibration as in 2a-4 and provide them directly in robyn_inputs()

# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly
#   ,dt_holidays = dt_prophet_holidays
#   ,date_var = "DATE"
#   ,dep_var = "revenue"
#   ,dep_var_type = "revenue"
#   ,prophet_vars = c("trend", "season", "holiday")
#   ,prophet_country = "DE"
#   ,context_vars = c("competitor_sales_B", "events")
#   ,paid_media_spends = c("tv_S", "ooh_S",	"print_S", "Meta_S", "search_S")
#   ,paid_media_vars = c("tv_S", "ooh_S", 	"print_S", "Meta_I", "search_clicks_P")
#   ,organic_vars = c("newsletter")
#   ,factor_vars = c("events")
#   ,window_start = "2016-11-23"
#   ,window_end = "2018-08-22"
#   ,adstock = "geometric"
#   ,hyperparameters = hyperparameters # as in 2a-2 above
#   ,calibration_input = calibration_input # as in 2a-4 above
# )

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

##### Manually save and import InputCollect as JSON file
# robyn_write(InputCollect, dir = "~/Desktop")
# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   json_file = "~/Desktop/RobynModel-inputs.json")

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "1", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

## 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/Metaexperimental/Robyn/blob/main/demo/schema.R
# pareto_hyperparameters.csv, hyperparameters per Pareto output model
# pareto_aggregated.csv, aggregated decomposition per independent variable of all Pareto output
# pareto_media_transform_matrix.csv, all media transformation vectors
# pareto_alldecomp_matrix.csv, all decomposition vectors of independent variables


################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "3_605_1" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = FALSE)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

# To check each of the one-pager's plots
# myOnePager[[select_model]]$patches$plots[[1]]
# myOnePager[[select_model]]$patches$plots[[2]]
# myOnePager[[select_model]]$patches$plots[[3]] # ...

################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition

# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends

# Scenario "max_response": "What's the max. return given certain spend?"
# Example 1: max_response default setting: maximize response for latest month
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = "all", # Default to "all"
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = c(1,1.2, 1.5, 1.5, 1.5, 1.5),
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = TRUE
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)

# Example 2: maximize response for latest 10 periods with given spend
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_10", # Last 10 periods, same as c("2018-10-22", "2018-12-31")
  total_budget = 5000000, # Total budget for date_range period simulation
  channel_constr_low = c(0.8, 0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.0,1.2, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 5, # Customise bound extension for wider insights
  scenario = "max_response",
  export = TRUE
)
print(AllocatorCollect2)
plot(AllocatorCollect2)

# Scenario "target_efficiency": "How much to spend to hit ROAS or CPA of x?"
# Example 3: Use default ROAS target for revenue or CPA target for conversion
# Check InputCollect$dep_var_type for revenue or conversion type
# Two default ROAS targets: 0.8x of initial ROAS as well as ROAS = 1
# Two default CPA targets: 1.2x and 2.4x of the initial CPA
AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  scenario = "target_efficiency",
  # target_value = 2, # Customize target ROAS or CPA value
  export = TRUE
)
print(AllocatorCollect3)
plot(AllocatorCollect3)

# Example 4: Customize target_value for ROAS or CPA (using json_file)
json_file = "~/Desktop/Robyn_202302221206_init/RobynModel-1_117_11.json"
AllocatorCollect4 <- robyn_allocator(
  json_file = json_file, # Using json file from robyn_write() for allocation
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  date_range = NULL, # Default last month as initial period
  scenario = "target_efficiency",
  target_value = 2, # Customize target ROAS or CPA value
  plot_folder = "~/Desktop/my_dir",
  plot_folder_sub = "my_subdir",
  export = create_files
)

## A csv is exported into the folder for further usage. Check schema here:
## https://github.com/Metaexperimental/Robyn/blob/main/demo/schema.R

## QA optimal response
# Pick any media variable: InputCollect$all_media
select_media <- "Gasto_Meta"
# For paid_media_spends set metric_value as your optimal spend
metric_value <- AllocatorCollect1$dt_optimOut$optmSpendUnit[
  AllocatorCollect1$dt_optimOut$channels == select_media
]; metric_value
# # For paid_media_vars and organic_vars, manually pick a value
metric_value <- 2000

## Saturation curve for adstocked metric results (example)
robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = select_media,
  metric_value = metric_value,
  date_range = "last_5"
)

################################################################
#### Step 6: Model refresh based on selected model and saved results

## Must run robyn_write() (manually or automatically) to export any model first, before refreshing.
## The robyn_refresh() function is suitable for updating within "reasonable periods".
## Two situations are considered better to rebuild model:
## 1. most data is new. If initial model has 100 weeks and 80 weeks new data is added in refresh,
## it might be better to rebuild the model. Rule of thumb: 50% of data or less can be new.
## 2. new variables are added.

# Provide JSON file with your InputCollect and ExportedModel specifications
# It can be any model, initial or a refresh model
json_file <- "~/Desktop/Robyn_202211211853_init/RobynModel-1_100_6.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 13,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)
# Now refreshing a refreshed model, following the same approach
json_file_rf1 <- "~/Desktop/Robyn_202208231837_init/Robyn_202208231841_rf1/RobynModel-1_12_5.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file_rf1,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 7,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)

# Continue with refreshed new InputCollect, OutputCollect, select_model values
InputCollectX <- RobynRefresh$listRefresh1$InputCollect
OutputCollectX <- RobynRefresh$listRefresh1$OutputCollect
select_modelX <- RobynRefresh$listRefresh1$OutputCollect$selectID

## Besides plots: there are 4 CSV outputs saved in the folder for further usage
# report_hyperparameters.csv, hyperparameters of all selected model for reporting
# report_aggregated.csv, aggregated decomposition per independent variable
# report_media_transform_matrix.csv, all media transformation vectors
# report_alldecomp_matrix.csv,all decomposition vectors of independent variables


################################################################
#### Step 7: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, newsletter sendings etc.) as well as plotting individual saturation curves. New
## argument names "metric_name" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.

## Recreate original saturation curve
Response <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "Meta_S"
)
Response$plot

## Or you can call a JSON file directly (a bit slower)
# Response <- robyn_response(
#   json_file = "your_json_path.json",
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   metric_name = "Meta_S"
# )

## Get the "next 100 dollar" marginal response on Spend1
Spend1 <- 20000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "Meta_S",
  metric_value = Spend1, # total budget for date_range
  date_range = "last_1" # last two periods
)
Response1$plot

Spend2 <- Spend1 + 100
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "Meta_S",
  metric_value = Spend2,
  date_range = "last_1"
)
# ROAS for the 100$ from Spend1 level
(Response2$response_total - Response1$response_total) / (Spend2 - Spend1)

## Get response from for a given budget and date_range
Spend3 <- 100000
Response3 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "Meta_S",
  metric_value = Spend3, # total budget for date_range
  date_range = "last_5" # last 5 periods
)
Response3$plot

## Example of getting paid media exposure response curves
# imps <- 10000000
# response_imps <- robyn_response(
#   InputCollect = InputCollect,
#   OutputCollect = OutputCollect,
#   select_model = select_model,
#   metric_name = "Meta_I",
#   metric_value = imps
# )
# response_imps$response_total / imps * 1000
# response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "newsletter",
  metric_value = sendings
)
# response per 1000 sendings
response_sending$response_total / sendings * 1000
response_sending$plot

################################################################
#### Optional: recreate old models and replicate results

# From an exported JSON file (which is created automatically when exporting a model)
# we can re-create a previously trained model and outputs. Note: we need to provide
# the main dataset and the holidays dataset, which are NOT stored in the JSON file.
# These JSON files will be automatically created in most cases.

############ WRITE ############
# Manually create JSON file with inputs data only
robyn_write(InputCollect, dir = "~/Desktop")

# Manually create JSON file with inputs and specific model results
robyn_write(InputCollect, OutputCollect, select_model)

############ READ ############
# Recreate `InputCollect` and `OutputCollect` objects
# Pick any exported model (initial or refreshed)
json_file <- "~/Desktop/Robyn_202208231837_init/RobynModel-1_100_6.json"

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file,
  export = create_files)

# Or re-create both by simply using robyn_recreate()
RobynRecreated <- robyn_recreate(
  json_file = "~/Desktop/Robyn_202303131448_init/RobynModel-1_103_7.json",
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)
InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

# Re-export or rebuild a model and check summary
myModel <- robyn_write(InputCollectX, OutputCollectX, export = FALSE, dir = "~/Desktop")
print(myModel)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX, OutputCollectX, export = FALSE)
# myModelPlot[[1]]$patches$plots[[7]]

# Refresh any imported model
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = InputCollectX$dt_input,
  dt_holidays = InputCollectX$dt_holidays,
  refresh_steps = 6,
  refresh_mode = "manual",
  refresh_iters = 1000,
  refresh_trials = 1
)

# Recreate response curves
robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  metric_name = "newsletter",
  metric_value = 50000
)