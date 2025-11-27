# Reads projects_reduction.csv and pollutants.csv  
# Produces: costs, A (pollutants x projects matrix), targets vector  
# Returns a list containing projects_df, pollutants_df, costs, A, targets

library(jsonlite)

.__DATA_CACHE__ <- new.env(parent = emptyenv())

clean_name <- function(x){
  x <- gsub("\\s+", "", x)# remove spaces
  x <- gsub("\\.", "", x) # remove dots
  x <- toupper(x) # uppercase
}

read_csv_data <- function(
  projectsPath = "projects_reduction.csv",
  pollutantsPath = "pollutants.csv",
  use_cache = TRUE) {

  if(use_cache && exists("DATA", envir= .__DATA_CACHE__)) {
      return(get("DATA", envir= .__DATA_CACHE__))
  }

  if (!file.exists(projectsPath)) 
    stop(paste("File not found:", projectsPath))

  if (!file.exists(pollutantsPath)) 
    stop(paste("File not found:", pollutantsPath))

  # Read CSV files
  projects_df <- read.csv(projectsPath, stringsAsFactors = FALSE, check.names = FALSE)
  pollutants_df <- read.csv(pollutantsPath, stringsAsFactors = FALSE, check.names = FALSE)


  # Ensure pollutants_df uses columns: Pollutant & Target
  if (!("Pollutant" %in% names(pollutants_df))) {
    names(pollutants_df)[1] <- "Pollutant"
  }

  if (!("Target" %in% names(pollutants_df))) {
    if (ncol(pollutants_df) < 2)
      stop("pollutants.csv must have at least two columns: Pollutant, Target")
    names(pollutants_df)[2] <- "Target"
  }

  pollutants <- as.character(pollutants_df$Pollutant)
  targets <- as.numeric(pollutants_df$Target)
  if (any(is.na(targets)))
    stop("Some pollutant targets are not numeric in pollutants.csv")

  # Detect Cost Column
  possible_costs <- c("cost","projectcost","price","costperunit","cost_each")
  lower_names <- tolower(names(projects_df))
  matches <- which(lower_names %in% possible_costs)

  if (length(matches) > 0) {
    cost_col <- matches[1]
  } else {
    if ("Cost" %in% names(projects_df)) {
      cost_col <- which(names(projects_df) == "Cost")
    } else {
      if (ncol(projects_df) >= 2) {
        cost_col <- 2
      } else {
        stop("Cannot detect cost column in projects_reduction.csv")
      }
    }
  }

  costs <- as.numeric(projects_df[[cost_col]])
  if (any(is.na(costs)))
    stop("Some costs are not numeric in projects_reduction.csv")

  # Identify pollutant columns in projects_df
  proj_cols <- names(projects_df)
  norm_proj <- sapply(proj_cols, clean_name)
  norm_pollutants <- sapply(pollutants, clean_name)

  pollutant_cols_idx <- sapply(norm_pollutants, function(np) {
    exact_idx <- which(norm_proj == np)

    if (length(exact_idx) == 0) {
      exact_idx <- which(grepl(np, norm_proj))
    }

    if (length(exact_idx) == 0)
      return(NA_integer_)

    return(exact_idx[1])
  })

  if (any(is.na(pollutant_cols_idx))) {
    missing <- pollutants[is.na(pollutant_cols_idx)]
    stop(
      paste(
        "Could not find columns for pollutants in projects_reduction.csv:",
        paste(missing, collapse = ", ")
      )
    )
  }

  # Build A Matrix
  A <- as.matrix(projects_df[, pollutant_cols_idx, drop = FALSE])
  A <- t(A)   # convert to pollutants x projects
  colnames(A) <- paste0("P", seq_len(ncol(A)))
  rownames(A) <- pollutants

DATA <- list(
  projects_df = projects_df,
  pollutants_df = pollutants_df,
  costs = costs,
  A=A,
  targets = setNames(targets, pollutants)
)
  assign("DATA", DATA, envir= .__DATA_CACHE__)
  return(DATA)
}
