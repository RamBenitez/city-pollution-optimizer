#api.R
library(plumber)
library(jsonlite)

source("readData.R")
source("solver.R")

DATA <- read_csv_data(use_cache = TRUE)
assign("DATA", DATA, envir = .__DATA_CACHE__)

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* @get /projects
function(req, res) {
  out <- DATA$projects_df
  res$body <- toJSON(out, dataframe = "rows", auto_unbox = TRUE)
  res
}

#* @get /pollutants
function(req, res) {
  out <- DATA$pollutants_df
  res$body <- toJSON(out, dataframe = "rows", auto_unbox = TRUE)
  res
}

#* @post /solve
function(req, res) {

  body <- tryCatch(fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) {
    res$status <- 400
    return(list(error = "Invalid JSON body"))
  }

  selected     <- body$selected_projects %||% NULL
  return_trace <- body$return_trace %||% FALSE
  lower_bound  <- body$lower_bound  %||% 0
  upper_bound  <- body$upper_bound  %||% 20

  result <- solve_lp(
    A = DATA$A,
    costs = DATA$costs,
    targets = DATA$targets,
    project_names = DATA$projects_df$project_name,
    selected = selected,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    return_trace = return_trace
  )

  res$body <- toJSON(result, auto_unbox = TRUE, pretty = TRUE)
  return(res)
}
