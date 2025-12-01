#read data
library(jsonlite)

.__DATA_CACHE__ <- new.env(parent = emptyenv())

clean_name <- function(x){
  x <- gsub("\\s+", "", x)
  x <- gsub("\\.", "", x)
  toupper(x)
}

read_csv_data <- function(
  projectsPath = "projects_reductions.csv",
  pollutantsPath = "pollutants.csv",
  use_cache = TRUE
) {

  if(use_cache && exists("DATA", envir= .__DATA_CACHE__)) {
    return(get("DATA", envir= .__DATA_CACHE__))
  }

  if (!file.exists(projectsPath)) stop(paste("File not found:", projectsPath))
  if (!file.exists(pollutantsPath)) stop(paste("File not found:", pollutantsPath))

  projects_df <- read.csv(projectsPath, stringsAsFactors = FALSE, check.names = FALSE)
  pollutants_df <- read.csv(pollutantsPath, stringsAsFactors = FALSE, check.names = FALSE)

  if (!("Pollutant" %in% names(pollutants_df))) names(pollutants_df)[1] <- "Pollutant"
  if (!("Target" %in% names(pollutants_df))) names(pollutants_df)[2] <- "Target"

  pollutants <- pollutants_df$Pollutant
  targets <- pollutants_df$Target

  # detect cost column
  possible_costs <- c("cost","projectcost","price","costperunit","cost_each")
  lower_names <- tolower(names(projects_df))
  matches <- which(lower_names %in% possible_costs)

  if (length(matches) > 0)
    cost_col <- matches[1]
  else if ("Cost" %in% names(projects_df))
    cost_col <- which(names(projects_df) == "Cost")
  else
    cost_col <- 3

  costs <- as.numeric(projects_df[[cost_col]])

  proj_cols <- names(projects_df)
  norm_proj <- sapply(proj_cols, clean_name)
  norm_pollutants <- sapply(pollutants, clean_name)

  pollutant_cols_idx <- sapply(norm_pollutants, function(np) {
    idx <- which(norm_proj == np)
    if (length(idx) == 0) idx <- which(grepl(np, norm_proj))
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  })

  if (any(is.na(pollutant_cols_idx)))
    stop("Missing pollutant columns in projects_reductions.csv")

  A <- as.matrix(projects_df[, pollutant_cols_idx, drop = FALSE])
  A <- t(A)

  colnames(A) <- paste0("P", seq_len(ncol(A)))
  rownames(A) <- pollutants

  DATA <- list(
    projects_df = projects_df,
    pollutants_df = pollutants_df,
    costs = costs,
    A = A,
    targets = setNames(targets, pollutants)
  )

  assign("DATA", DATA, envir= .__DATA_CACHE__)
  return(DATA)
}
