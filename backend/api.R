# api.R
library(plumber) #fro creating Api route
library(jsonlite) #r  to json

#load helper scripts
source("readData.R")
source("solver.R")

#read data from csv
DATA <- read_csv_data(use_cache = TRUE)
#store data in  global so all endpoint can access it
assign("DATA", DATA, envir = .__DATA_CACHE__)

# @filter cors
function(req, res){
    #allow request from any url 
    res$setHeader("Access-Control-Allow-Origin", "*")
    #allow http methds
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
    #
    if (req&REQUEST_METHOD == "OPTIONS"){
        res$status <- 200
        return(list())
    }
    plumber::forward()
}

#get projects return all projects
function(req, res) {
  #retrieve the data
  data <- get("DATA", envir = .__DATA_CACHE__)
  #extract the projects data frame
  projects_df <- data$projects_df
  #keep the original columns
  out <- projects_df
  # add index column if no id column
  if (!("id" %in% tolower(names(out)))) {
    out$index <- seq_len(nrow(out))
  }
  #convert to row based json format
  res$body <- toJSON(out, dataframe = "rows", auto_unbox = TRUE)
  return(res)
}

#get pollutants return all the pollutants and targets
function(req, res) {
  #load the pollutant target data
  data <- get("DATA", envir = .__DATA_CACHE__)
  #convert to json
  res$body <- toJSON(data$pollutants_df, dataframe = "rows", auto_unbox = TRUE)

  return(res)
}

#Solve the LP using selected projects
#post solve
#parser json
function(req, res) {
  body <- tryCatch({
    fromJSON(req$postBody)
  }, error = function(e) {
    res$status <- 400 #bad request
    return(list(error = "Invalid JSON body"))
  })

  #default parameters
  selected <- NULL
  return_trace <- FALSE
  lower_bound <- 0
  upper_bound <- 20

  #read user parameters if provided
  if (!is.null(body$selected_projects)) selected <- as.integer(body$selected_projects)
  if (!is.null(body$return_trace)) return_trace <- as.logical(body$return_trace)
  if (!is.null(body$lower_bound)) lower_bound <- as.numeric(body$lower_bound)
  if (!is.null(body$upper_bound)) upper_bound <- as.numeric(body$upper_bound)

  #pull data
  data <- get("DATA", envir = .__DATA_CACHE__)

  # call solver
  res_solve <- tryCatch({
    solve_lp(
      A = data$A, 
      costs = data$costs, 
      targets = data$targets, 
      selected = selected, 
      lower_bound = lower_bound, 
      upper_bound = upper_bound, 
      return_trace = return_trace
    )
  }, error = function(e) {
    list(error = as.character(e))
  })

  # if solver returned erro, then http 500
  if (!is.null(res_solve$error)) {
    res$status <- 500
    return(list(error = res_solve$error))
  }
  #convert before sending
  if (!is.null(res_solve$solution)) res_solve$solution <- fromJSON(res_solve$solution)
  #final json 
  res$body <- toJSON(res_solve, auto_unbox = TRUE, pretty = TRUE)
  return(res)
}

# Run server if executed directly
if (identical(Sys.getenv("RUN_PLUMBER"), "1") || interactive()) {
  #load api from this 
  pr <- plumber::plumb("api.R")
  #then start server
  pr$run(host = "0.0.0.0", port = 8000)
}
