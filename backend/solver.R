# solver.R
library(lpsolve) #lp solver
library(jsonlite) # convert R to JSON so we can send to frontend

# contsurcts and iterates dual simplex tableau
simplex_dual_trace <- function(A, costs, targets, max_iters = 300, eps = 1e-9) {
  #A - pollutant matrix mn
  #costs of each project
  #targets -required pollutang reductions
  #max_iters - safety stop to avoid infinite loops
  #eps - tolerance to treat values as zero

  m <- nrow(A) #number of pollutants
  n <- ncol(A) #number of projects

  #build dual tableau 
  D <- t(A)  # n x m
  rhs <- costs
  #add slack variables 
  tableau <- cbind(D, diag(n), rhs)

  obj_row <- c(-as.numeric(targets), rep(0, n), 0)
  T <- rbind(tableau, obj_row)


  colnames(T) <- c(paste0("y",1:m), paste0("s",1:n), "Solution")
  rownames(T) <- c(paste0("Cons",1:n), "Obj")

#store tabluea
  tableaux_list <- list()
  tableaux_list[[1]] <- T
  basis <- paste0("s",1:n)
  iter <- 1

  while(TRUE) {
    bottom <- T[nrow(T), -ncol(T)]
    min_val <- min(bottom)
    # if the most negative coefficint is non-negative, done
    if (min_val >= -eps) break
    #choose pivot column by selecting most negative coeff in objective row
    pivot_col_index <- which(bottom == min_val)[1]

    #only positive pivot colum entries allowed
    col_vals <- T[1:n, pivot_col_index] #column under pivot
    rhs_vals <- T[1:n, ncol(T)] #RHS Column
    positive_index <- which(col_vals > eps)
    if (length(positive_index) == 0) {
      stop("Unbounded dual in trace")
    }

    #smallest ratio = pivot row
    ratios <- rhs_vals[positive_index] / col_vals[positive_index]
    pivot_row_rel <- positive_index[which.min(ratios)]
    pivot_row_index <- pivot_row_rel
    #normalize the pivot row
    pivot_elem <- T[pivot_row_index, pivot_col_index]
    T[pivot_row_index, ] <- T[pivot_row_index, ] / pivot_elem
    #eliminate other rows
    for (r in setdiff(seq_len(nrow(T)), pivot_row_index)) {
      T[r, ] <- T[r, ] - T[r, pivot_col_index] * T[pivot_row_index, ]
    }
    #updated basis and tableau list
    basis[pivot_row_index] <- colnames(T)[pivot_col_index]
    tableaux[[length(tableaux)+1]] <- T
    iter <- iter + 1
    if (iter > max_iters) break
  }
  dual_obj <- T[nrow(T), ncol(T)]
  # derive primal x from slack columns s1..sn
  x <- numeric(n)
  for (i in 1:n) {
    col_name <- paste0("s", i)
    if (col_name %in% colnames(T)) {
      col_vec <- T[1:n, col_name]
      nz <- which(abs(col_vec) > 1e-8)
      if (length(nz) == 1 && abs(col_vec[nz] - 1) < 1e-8) {
        x[i] <- T[nz, ncol(T)]
      } else x[i] <- 0
    }
  }

# convert tableaux to JSON-friendly format
convert_tableau <- function(mat) {
    headers <- colnames(mat)
    rows <- lapply(seq_len(nrow(mat)), function(i) as.numeric(mat[i, ]))
    rownames_list <- rownames(mat)
    list(headers = headers, rownames = rownames_list, rows = rows)
  }
  #return full trace
  tableaux_json <- lapply(tableaux_list, convert_tableau)
  list(
    tableaux = tableaux_json,
    final_tableau = convert_tableau(T), 
    basis = basis, dual_obj = dual_obj, 
    primal_x = x
  )
}

# wraps lpsolve and optionally includes the trace
solve_lp <- function(A, costs, targets, selected = NULL, lower_bound = 0, upper_bound = 20, return_trace = FALSE) {
  # process the selected projects
  if (is.null(selected)) selected <- seq_len(ncol(A))
  selected <- as.integer(selected)
  selected <- selected[selected >= 1 & selected <= ncol(A)]
  if (length(selected) == 0) stop("No valid selected project indices")

  #subset A and costs
  A_sel <- A[, selected, drop = FALSE]
  cost_sel <- as.numeric(costs[selected])
  
  const_mat <- -A_sel
  const_dir <- rep("<=", nrow(A_sel))
  const_rhs <- -as.numeric(targets)

  lower <- rep(lower_bound, length(cost_sel))
  upper <- rep(upper_bound, length(cost_sel))
  
lp_res <- lp(direction = "min",
             objective.in = cost_sel,
             const.mat = const_mat,
             const.dir = const_dir,
             const.rhs = const_rhs,
             all.int = FALSE,
             lower = lower,
             upper = upper,)
  out <- list(lp_status = lp_res$status)

  if (lp_res$status == 0) { #success
    raw_x <- lp_res$solution
    x_opt <- pmin(pmax(raw_x, lower), upper)

    out$feasible <- TRUE
    out$total_cost <- sum(x_opt * cost_sel)
    out$solution <- data.frame(
      project_index = selected,
      project_name = if ("project_name" %in% names(get("DATA", envir = .__DATA_CACHE__)$projects_df)) {
                        get("DATA", envir = .__DATA_CACHE__) $projects_df$project_name[selected]
                     } else {
                        get("DATA", envir = .__DATA_CACHE__) $projects_df[[1]][selected]
                     },
      units = as.numeric(x_opt),
      cost_each = cost_sel,
      total_cost = as.numeric(x_opt * cost_sel),
      stringsAsFactors = FALSE
    )
  } else {
    out$feasible <- FALSE
    out$message <- ifelse(lp_res$status == 2, "Infeasible", paste("LP status", lp_res$status))
  }
  
  if (return_trace) {
    trace <- tryCatch(simplex_dual_trace(A_sel, cost_sel, targets), error = function(e) list(error = as.character(e)))
    out$tableau_trace <- trace
  }
  # convert json fro the frontend
  out$solution <- jsonlite::toJSON(out$solution, dataframe = "rows", auto_unbox = TRUE)
  out$tableau_trace <- if (!is.null(out$tableau_trace)) jsonlite::toJSON(out$tableau_trace, auto_unbox = TRUE) else NULL

  out
}
