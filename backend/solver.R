# solver.R 
library(lpSolve)
library(jsonlite)

# conver tableau to JSON friendly to easy understand for the frontend
convert_tableau <- function(tab, var_names, row_names, basic_vars, entering = NA, leaving = NA, pivot = NULL) {
  list(
    headers = c(var_names, "RHS"), #variable names + RHS
    rownames = row_names, #names of the rows
    basic_vars = basic_vars,        # names of basic variables per row
    tableau = lapply(seq_len(nrow(tab)), function(i) as.numeric(tab[i, ])),
    entering = entering,
    leaving = leaving,
    pivot = pivot
  )
}
#lp manually using simplex method
simplex_tableau_trace <- function(const_mat, const_dir, const_rhs, cost_vec, max_iters = 500, eps = 1e-9) {
 #convert into numeric matrix format
  A0 <- as.matrix(const_mat) * 1.0 
  b0 <- as.numeric(const_rhs) 
  dir0 <- as.character(const_dir) 
  c0 <- as.numeric(cost_vec)

  m <- nrow(A0) #number of constraints
  n <- ncol(A0) #number of decision variables

  # Step 1: normalize RHS positive: if b < 0, multiply row by -1 and flip direction
  for (i in seq_len(m)) {
    if (b0[i] < 0) {
      A0[i, ] <- -A0[i, ]
      b0[i] <- -b0[i]
      if (dir0[i] == "<=") dir0[i] <- ">="
      else if (dir0[i] == ">=") dir0[i] <- "<="
    }
  }

  # Step 2: convert to standard form 
  slack_cols <- rep(0, 0)
  art_cols <- rep(0, 0)
  slack_map <- integer(0)    
  art_map <- integer(0)      

  A_ext_rows <- list()
  for (i in seq_len(m)) {
    row <- A0[i, ]
    if (dir0[i] == "<=") {
      # add one slack variable (coefficient +1)
      slack_cols <- cbind(slack_cols, rep(0, length(slack_cols)))  
      slack_map[i] <- 1L  
      art_map[i] <- 0L
    } else if (dir0[i] == ">=") {
      row <- -row
      b0[i] <- b0[i]      
      slack_map[i] <- 0L
      art_map[i] <- 1L
      dir0[i] <- "<="    
    } else if (dir0[i] == "=") {
      slack_map[i] <- 0L
      art_map[i] <- 1L
    } else {
      stop("Unsupported constraint direction: ", dir0[i])
    }
    A_ext_rows[[i]] <- row
  }

  # Count slack and artificial columns
  #stack all rows together as matrix
  n_slack <- sum(slack_map == 1L)
  n_art <- sum(art_map == 1L)


  A_core <- do.call(rbind, lapply(A_ext_rows, function(r) r))
  #check if there are slack to create
  if (n_slack > 0) {
    # creates n m*n slack matrix
    S <- matrix(0, nrow = m, ncol = n_slack)
    s_idx <- 1 #counter
    for (i in seq_len(m)) { #lop through each
      if (slack_map[i] == 1L) { #if this row reques a slack
        S[i, s_idx] <- 1 #make it the basic variable
        s_idx <- s_idx + 1 #then move next
      }
    }
  } else { #else make iwth 0 colums
    S <- matrix(0, nrow = m, ncol = 0)
  }
  if (n_art > 0) {
    A_art <- matrix(0, nrow = m, ncol = n_art)
    a_idx <- 1
    for (i in seq_len(m)) {
      if (art_map[i] == 1L) {
        A_art[i, a_idx] <- 1
        a_idx <- a_idx + 1
      }
    }
  } else {
    A_art <- matrix(0, nrow = m, ncol = 0)
  }

  A_ext <- cbind(A_core, S, A_art)
  total_vars <- n + n_slack + n_art

  # Variable names
  x_names <- paste0("x", seq_len(n))
  s_names <- if (n_slack>0) paste0("s", seq_len(n_slack)) else character(0)
  a_names <- if (n_art>0) paste0("a", seq_len(n_art)) else character(0)
  var_names_full <- c(x_names, s_names, a_names)

  # Build initial tableau (m rows + 1 obj row). Tableau columns = total_vars + RHS
  tableau <- cbind(A_ext, matrix(b0, ncol = 1))
  obj_row <- rep(0, total_vars + 1)   # last element is RHS of obj (= objective value)
  tableau_full <- rbind(tableau, obj_row)

  # Determine initial basic variables: slack columns and artificial columns are basic
  basic_vars <- rep(NA_character_, m)
  basic_indices <- integer(m)  # which column index in tableau each row uses as basic var
  s_index <- n
  a_index <- n + n_slack
  # assign slacks first
  si <- 1
  ai <- 1
  for (i in seq_len(m)) {
    if (slack_map[i] == 1L) {
      s_index <- n + si
      basic_indices[i] <- s_index
      basic_vars[i] <- s_names[si]
      si <- si + 1
    } else if (art_map[i] == 1L) {
      a_index <- n + n_slack + ai
      basic_indices[i] <- a_index
      basic_vars[i] <- a_names[ai]
      ai <- ai + 1
    } else {
      basic_indices[i] <- NA_integer_
      basic_vars[i] <- NA_character_
    }
  }

  phase1_trace <- list()
  obj_phase1 <- rep(0, total_vars + 1)
  if (n_art > 0) {
    obj_phase1[(n + n_slack + 1):(n + n_slack + n_art)] <- 1
  }

  obj_row_current <- -obj_phase1 

  if (n_art > 0) {
    obj_row_current <- rep(0, total_vars + 1)
    for (i in seq_len(m)) {
      if (!is.na(basic_indices[i]) && basic_indices[i] > n + n_slack) {

        obj_row_current <- obj_row_current - tableau_full[i, ]
      }
    }
    for (j in seq_len(n_art)) {
      colj <- n + n_slack + j
      obj_row_current[colj] <- obj_row_current[colj] + 1
    }
  } else {
    obj_row_current <- rep(0, total_vars + 1)
  }

  tableau_full[nrow(tableau_full), ] <- obj_row_current

  row_names <- c(paste0("C", seq_len(m)), "Obj")
  phase1_trace[[1]] <- convert_tableau(tableau_full, var_names_full, row_names, basic_vars, entering = NA, leaving = NA, pivot = NULL)

  pivot_once <- function(tab, basic_indices_local, basic_vars_local) {
    m_local <- nrow(tab) - 1
    ncol_total <- ncol(tab)
    objrow <- tab[nrow(tab), 1:(ncol_total-1)]
    enter_idx <- which.min(objrow)
    if (length(enter_idx) == 0 || objrow[enter_idx] >= -eps) {
      return(list(tab = tab, ok = FALSE, entering = NA, leaving = NA, pivot = NULL, unbounded = FALSE))
    }
  
    col_vals <- tab[1:m_local, enter_idx]
    rhs_vals <- tab[1:m_local, ncol_total]
    ratios <- rep(Inf, m_local)
    pos_idx <- which(col_vals > eps)
    if (length(pos_idx) == 0) {
      
      return(list(tab = tab, ok = FALSE, entering = enter_idx, leaving = NA, pivot = NULL, unbounded = TRUE))
    }
    ratios[pos_idx] <- rhs_vals[pos_idx] / col_vals[pos_idx]
    leave_row <- which.min(ratios)
    pivot_val <- tab[leave_row, enter_idx]
    # perform pivot
    tab[leave_row, ] <- tab[leave_row, ] / pivot_val
    for (r in seq_len(nrow(tab))) {
      if (r != leave_row) {
        tab[r, ] <- tab[r, ] - tab[r, enter_idx] * tab[leave_row, ]
      }
    }
 
    basic_indices_local[leave_row] <- enter_idx
    basic_vars_local[leave_row] <- if (enter_idx <= n) x_names[enter_idx] else if (enter_idx <= n + n_slack) s_names[enter_idx - n] else a_names[enter_idx - n - n_slack]

    list(tab = tab, ok = TRUE, entering = enter_idx, leaving = leave_row, pivot = list(row = leave_row, col = enter_idx), unbounded = FALSE, basic_indices = basic_indices_local, basic_vars = basic_vars_local)
  }

 
  iter <- 1
  while (iter <= max_iters) {
    res <- pivot_once(tableau_full, basic_indices, basic_vars)
    if (res$unbounded) {
      return(list(status = "unbounded", message = "Unbounded during Phase 1", phase1.trace = phase1_trace, phase2.trace = NULL))
    }
    if (!res$ok) {

      break
    }
    tableau_full <- res$tab
    basic_indices <- res$basic_indices
    basic_vars <- res$basic_vars

    iter <- iter + 1
    phase1_trace[[length(phase1_trace) + 1]] <- convert_tableau(tableau_full, var_names_full, row_names, basic_vars, entering = res$entering, leaving = res$leaving, pivot = res$pivot)
  }


  phase1_obj_value <- tableau_full[nrow(tableau_full), ncol(tableau_full)]

  if (phase1_obj_value > eps) {
    # infeasible
    return(list(status = "infeasible", message = "Infeasible (Phase 1 objective > 0)", phase1.trace = phase1_trace, phase2.trace = NULL))
  }


  art_cols_idx <- if (n_art > 0) seq(n + n_slack + 1, n + n_slack + n_art) else integer(0)
  # Remove artificial columns from tableau
  if (length(art_cols_idx) > 0) {
    
    keep_cols <- setdiff(seq_len(total_vars), art_cols_idx)
    tableau_full <- tableau_full[, c(keep_cols, ncol(tableau_full)), drop = FALSE]  # keep RHS
    var_names_phase2 <- var_names_full[keep_cols]
    # update indices mapping (basic_indices need update to new column indices)
    # map old col -> new col index
    map_old_to_new <- integer(total_vars)
    new_idx <- 1
    for (j in seq_len(total_vars)) {
      if (!(j %in% art_cols_idx)) {
        map_old_to_new[j] <- new_idx
        new_idx <- new_idx + 1
      } else {
        map_old_to_new[j] <- NA_integer_
      }
    }
    for (i in seq_len(m)) {
      if (!is.na(basic_indices[i])) {
        basic_indices[i] <- map_old_to_new[basic_indices[i]]
      }
    }
    total_vars2 <- n + n_slack
  } else {
    var_names_phase2 <- var_names_full
    total_vars2 <- total_vars
  }

  c_ext <- c(c0, rep(0, n_slack))
  if (length(c_ext) != total_vars2) {
    stop("Internal error: cost vector length mismatch in phase2")
  }

  obj_row_phase2 <- rep(0, total_vars2 + 1)
  obj_row_phase2[1:total_vars2] <- c_ext
  obj_row_phase2[total_vars2 + 1] <- 0.0


  for (i in seq_len(m)) {
    bi <- basic_indices[i]
    if (is.na(bi)) next
    cb <- 0
    if (bi <= n) {
      cb <- c0[bi]
    } else {
      cb <- 0
    }
    if (abs(cb) > eps) {
      obj_row_phase2 <- obj_row_phase2 - cb * tableau_full[i, ]
    }
  }
  # set last row
  tableau_full[nrow(tableau_full), ] <- obj_row_phase2

  # record start of phase2
  phase2_trace <- list()
  row_names <- c(paste0("C", seq_len(m)), "Obj")
  phase2_trace[[1]] <- convert_tableau(tableau_full, var_names_phase2, row_names, basic_vars, entering = NA, leaving = NA, pivot = NULL)

  # iterate Phase2 simplex
  iter2 <- 1
  while (iter2 <= max_iters) {
    obj_row_now <- tableau_full[nrow(tableau_full), 1:total_vars2]
    if (all(obj_row_now >= -eps)) break  # optimal
    enter_idx <- which.min(obj_row_now)
    # ratio test
    col_vals <- tableau_full[1:m, enter_idx]
    rhs_vals <- tableau_full[1:m, total_vars2 + 1]
    pos_idx <- which(col_vals > eps)
    if (length(pos_idx) == 0) {
      return(list(status = "unbounded", message = "Unbounded during Phase 2", phase1.trace = phase1_trace, phase2.trace = phase2_trace))
    }
    ratios <- rep(Inf, m)
    ratios[pos_idx] <- rhs_vals[pos_idx] / col_vals[pos_idx]
    leave_row <- which.min(ratios)
    pivot_val <- tableau_full[leave_row, enter_idx]
    # pivot
    tableau_full[leave_row, ] <- tableau_full[leave_row, ] / pivot_val
    for (r in seq_len(nrow(tableau_full))) {
      if (r != leave_row) {
        tableau_full[r, ] <- tableau_full[r, ] - tableau_full[r, enter_idx] * tableau_full[leave_row, ]
      }
    }
    # update basic
    basic_indices[leave_row] <- enter_idx
    basic_vars[leave_row] <- if (enter_idx <= n) x_names[enter_idx] else s_names[enter_idx - n]
    iter2 <- iter2 + 1
    phase2_trace[[length(phase2_trace) + 1]] <- convert_tableau(tableau_full, var_names_phase2, row_names, basic_vars, entering = enter_idx, leaving = leave_row, pivot = list(row = leave_row, col = enter_idx))
  }

  # Collect final solution from tableau
  solution_x <- rep(0, n)
  for (i in seq_len(m)) {
    bi <- basic_indices[i]
    if (!is.na(bi) && bi <= n) {
      solution_x[bi] <- tableau_full[i, total_vars2 + 1]
    }
  }
  objective_value <- tableau_full[nrow(tableau_full), total_vars2 + 1]

  list(
    status = "optimal",
    message = "Optimal found via tableau",
    phase1.trace = phase1_trace,
    phase2.trace = phase2_trace,
    solution = solution_x,
    objective = objective_value
  )
}

solve_lp <- function(A, costs, targets,
                      project_names = NULL,
                     selected = NULL,
                     lower_bound = 0, #  minimum
                     upper_bound = 20, #maximum allowed units
                     return_trace = FALSE) {
  #if seected is not provided, condiser all
  if (is.null(selected)) selected <- seq_len(ncol(A))
  selected <- selected[selected >= 1 & selected <= ncol(A)]
  #stop if not valid prjects
  if (length(selected) == 0) stop("No valid selected project indices")

  
  #slect oly the chosen projects from A and costs
  A_sel <- A[, selected, drop = FALSE]
  cost_sel <- as.numeric(costs[selected])
  m <- nrow(A_sel) #constraints in 
  n_sel <- ncol(A_sel) #number of selected projects

  # Add variable bounds as constraints
  const_mat <- rbind(A_sel, diag(n_sel))
  const_dir <- c(rep(">=", m), rep("<=", n_sel))
  const_rhs <- c(targets, rep(upper_bound, n_sel))

  #  adds lower bounds if lowe bound >0
  if (lower_bound > 0) {
    const_mat <- rbind(const_mat, -diag(n_sel))
    const_dir <- c(const_dir, rep("<=", n_sel))
    const_rhs <- c(const_rhs, rep(-lower_bound, n_sel))
  }

  # Solve LP via lpSolve
  lp_res <- lp(
    direction = "min",
    objective.in = cost_sel,
    const.mat = const_mat,
    const.dir = const_dir,
    const.rhs = const_rhs,
    compute.sens = FALSE
  )

  # prapre output
  out <- list(lp_status = lp_res$status)

  if (lp_res$status == 0) {
    x <- lp_res$solution
    out$feasible <- TRUE
    out$total_cost <- sum(x * cost_sel)

    #select for projct names i  provided
    project_names_sel <- rep(NA, n_sel)
    if (!is.null(project_names)) {
      # ensures that  project_names has correct length
      if (length(project_names) != ncol(A)) stop("Length of project_names must match number of columns in A")
      project_names_sel <- project_names[selected]
    }

    # builds list of solution for each project
    solution <- lapply(seq_len(n_sel), function(i) {
      list(
        project_index = selected[i],
        project_name = project_names_sel[i],
        units = x[i],
        cost_each = cost_sel[i],
        total_cost = x[i] * cost_sel[i]
      )
    })
  #ouput the list
    out$solution <- solution

    #will show infeasible if not solvale
  } else {
    out$feasible <- FALSE
    out$message <- ifelse(lp_res$status == 2, "Infeasible", paste("LP status", lp_res$status))
    out$solution <- list()
    out$total_cost <- NULL
  }
  return(out)
}

# End of solver.R
