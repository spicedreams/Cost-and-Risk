# ==========================================
# utils.R
# ==========================================

get_safe_volumes <- function() {
  v <- c("Current Directory" = getwd())
  tryCatch({
    sf_v <- suppressWarnings(shinyFiles::getVolumes()())
    if (length(sf_v) > 0) {
      v <- c(v, sf_v)
    }
  }, error = function(e) NULL)
  return(v)
}

acquire_db_lock <- function(network_path, rv) {
  if (is.null(network_path) || network_path == "") return(FALSE)
  
  lock_dir <- paste0(network_path, ".lockdir")
  success <- suppressWarnings(dir.create(lock_dir))
  
  if (success) {
    user_info <- Sys.info()[["user"]]
    if (is.null(user_info) || user_info == "") user_info <- "UnknownUser"
    
    info_file <- file.path(lock_dir, paste0("locked_by_", user_info, ".txt"))
    try(writeLines(paste("Locked at:", Sys.time()), info_file), silent = TRUE)
    
    rv$lock_dir <- lock_dir
    return(TRUE)
  } else {
    occupant <- "another process"
    if (dir.exists(lock_dir)) {
      files <- list.files(lock_dir, pattern = "^locked_by_")
      if (length(files) > 0) {
        occupant <- gsub("locked_by_|.txt", "", files[1])
      }
    }
    showNotification(paste("Database is locked by:", occupant), type = "error", duration = 8)
    return(FALSE)
  }
}

release_db_lock <- function(rv) {
  if (!is.null(rv$lock_dir) && dir.exists(rv$lock_dir)) {
    unlink(rv$lock_dir, recursive = TRUE)
  }
  rv$lock_dir <- NULL
}

sync_to_network <- function(rv) {
  if (!is.null(rv$db_path) && !is.null(rv$network_db_path)) {
    file.copy(from = rv$db_path, to = rv$network_db_path, overwrite = TRUE)
    unlink(rv$db_path)
  }
}

rpert <- function(n, x.min, x.mode, x.max, lambda = 4) {
  if (is.na(x.min) || is.na(x.max) || is.na(x.mode)) return(rep(0, n))
  if (x.min == x.max) return(rep(x.min, n))
  x.range <- x.max - x.min
  alpha <- 1 + lambda * (x.mode - x.min) / x.range
  beta <- 1 + lambda * (x.max - x.mode) / x.range
  return(x.min + x.range * rbeta(n, alpha, beta))
}

fmt_dollar <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

generate_id <- function() {
  paste0(sample(c(letters, 0:9), 16, replace = TRUE), collapse = "")
}

extract_opened_ids <- function(tree) {
  opened <- c()
  if (is.list(tree) && length(tree) > 0) {
    for (name in names(tree)) {
      node <- tree[[name]]
      ststate <- attr(node, "ststate")
      stclass <- attr(node, "stclass")
      
      is_open <- FALSE
      if (!is.null(ststate) && isTRUE(ststate$opened)) {
        is_open <- TRUE
      } else if (isTRUE(attr(node, "stopened"))) {
        is_open <- TRUE
      }
      
      if (is_open && !is.null(stclass)) {
        node_id <- sub(" inactive-node", "", stclass)
        node_id <- sub("^node_", "", node_id)
        opened <- c(opened, node_id)
      }
      opened <- c(opened, extract_opened_ids(node))
    }
  }
  return(opened)
}

build_nested_tree <- function(df, parent = NA, focus_id = NULL, opened_ids = NULL, ancestor_ids = NULL) {
  if (is.na(parent)) {
    children <- df[is.na(df$parent_id) | df$parent_id == "", ]
  } else {
    children <- df[!is.na(df$parent_id) & df$parent_id == parent, ]
  }
  
  if (nrow(children) == 0) return("")
  
  res <- list()
  for (i in 1:nrow(children)) {
    node_name <- children$title[i]
    node_id <- children$id[i]
    el_type <- children$element_type[i]
    
    child_node <- build_nested_tree(df, node_id, focus_id, opened_ids, ancestor_ids)
    
    if (is.null(opened_ids)) {
      attr(child_node, "stopened") <- TRUE 
    } else {
      attr(child_node, "stopened") <- (node_id %in% opened_ids) || (node_id %in% ancestor_ids)
    }
    
    is_active <- ifelse(is.na(children$is_active[i]), 1, children$is_active[i])
    if (el_type == "Treatment" && is_active == 0) {
      attr(child_node, "stclass") <- paste0("node_", node_id, " inactive-node")
    } else {
      attr(child_node, "stclass") <- paste0("node_", node_id) 
    }
    
    icon_class <- switch(el_type,
                         "Cost" = "fa fa-tags",
                         "Risk" = "fa fa-exclamation-triangle",
                         "Issue" = "fa fa-fire",
                         "Benefit" = "fa fa-gift",
                         "Treatment" = "fa fa-medkit",
                         "Residual" = "fa fa-filter",
                         "fa fa-folder")
    
    attr(child_node, "sticon") <- icon_class
    if (!is.null(focus_id) && node_id == focus_id) {
      attr(child_node, "stselected") <- TRUE
    }
    res[[node_name]] <- child_node
  }
  return(res)
}

find_selected_node <- function(tree) {
  if (isTRUE(attr(tree, "stselected"))) {
    val <- sub(" inactive-node", "", attr(tree, "stclass"))
    return(sub("^node_", "", val)) 
  }
  if (is.list(tree)) {
    for (i in seq_along(tree)) {
      res <- find_selected_node(tree[[i]])
      if (!is.null(res)) {
        val <- sub(" inactive-node", "", res)
        return(sub("^node_", "", val))
      }
    }
  }
  return(NULL)
}

collect_descendants <- function(df, node_id) {
  if (is.null(node_id) || is.na(node_id) || node_id == "" || node_id == "root") return(character())
  ids <- character()
  current <- c(node_id)
  while (length(current) > 0) {
    next_ids <- df$id[!is.na(df$parent_id) & df$parent_id %in% current]
    next_ids <- next_ids[!(next_ids %in% ids)]
    if (length(next_ids) == 0) break
    ids <- c(ids, next_ids)
    current <- next_ids
  }
  unique(ids)
}

node_is_issue_in_risk_tree <- function(df, node_id) {
  if (is.null(node_id) || is.na(node_id) || node_id == "" || node_id == "root") return(FALSE)
  node_row <- df[df$id == node_id, , drop = FALSE]
  if (nrow(node_row) == 0 || node_row$element_type[1] != "Issue") return(FALSE)

  current <- node_id
  seen <- character()
  has_issue_ancestor <- FALSE
  while (!is.na(current) && current != "" && !(current %in% seen)) {
    seen <- c(seen, current)
    row <- df[df$id == current, , drop = FALSE]
    if (nrow(row) > 0) {
      if (row$element_type[1] == "Issue" && current != node_id) {
        has_issue_ancestor <- TRUE
      }
      current <- row$parent_id[1]
    } else {
      current <- NA
    }
  }
  !has_issue_ancestor
}

build_issue_candidate_tree <- function(df, parent = NA, excluded_ids = character()) {
  if (is.na(parent)) {
    children <- df[df$element_type == "Issue" & (is.na(df$parent_id) | df$parent_id == ""), , drop = FALSE]
  } else {
    children <- df[df$element_type == "Issue" & !is.na(df$parent_id) & df$parent_id == parent, , drop = FALSE]
  }

  if (nrow(children) == 0) return("")
  res <- list()
  for (i in seq_len(nrow(children))) {
    node_id <- children$id[i]
    if (node_id %in% excluded_ids) next

    child_node <- build_issue_candidate_tree(df, node_id, excluded_ids)
    attr(child_node, "stclass") <- paste0("node_", node_id)
    attr(child_node, "sticon") <- "fa fa-fire"
    attr(child_node, "stopened") <- TRUE
    res[[children$title[i]]] <- child_node
  }

  if (length(res) == 0) return("")
  res
}

generate_report <- function(df, n_iter = 10000, seed_val = NULL) {
  if (nrow(df) == 0) return(data.frame())
  if (!is.null(seed_val) && !is.na(seed_val)) set.seed(seed_val) else set.seed(NULL)
  
  traverse <- function(node_id, level) {
    node <- df[df$id == node_id, ]
    children <- df[!is.na(df$parent_id) & df$parent_id == node_id, ]
    
    val_mult <- ifelse(node$element_type == "Benefit", -1, 1)
    is_leaf <- ifelse(is.na(node$is_leaf), 0, node$is_leaf) == 1
    is_active <- ifelse(is.na(node$is_active), 1, node$is_active)
    
    has_active_treatment <- FALSE
    if (node$element_type %in% c("Risk", "Issue") && nrow(children) > 0) {
      has_active_treatment <- any(children$element_type == "Treatment" & 
                                  ifelse(is.na(children$is_active), 1, children$is_active) == 1)
    }
    
    mc_array <- rep(0, n_iter)
    child_rows <- list()
    
    if (is_leaf && !is.na(node$opt_val) && !is.na(node$likely_val) && !is.na(node$pess_val)) {
      if (!has_active_treatment) {
        calc_min <- min(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        calc_max <- max(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        calc_likely <- median(c(node$opt_val, node$likely_val, node$pess_val), na.rm = TRUE)
        prob_decimal <- ifelse(is.na(node$chance), 1, node$chance / 100)
        
        sev <- rpert(n_iter, calc_min, calc_likely, calc_max)
        occ <- rbinom(n_iter, size = 1, prob = prob_decimal)
        mc_array <- sev * occ * val_mult
      }
    }
    
    if (nrow(children) > 0) {
      leaf_arrays <- list()
      parent_arrays <- list()
      
      for (i in 1:nrow(children)) {
        c_res <- traverse(children$id[i], level + 1)
        child_rows <- c(child_rows, c_res$rows)
        
        child_is_leaf <- ifelse(is.na(children$is_leaf[i]), 0, children$is_leaf[i]) == 1
        if (child_is_leaf) {
          leaf_arrays <- c(leaf_arrays, list(c_res$mc_array))
        } else {
          parent_arrays <- c(parent_arrays, list(c_res$mc_array))
        }
      }
      
      if (length(leaf_arrays) > 0) mc_array <- mc_array + (Reduce("+", leaf_arrays) / length(leaf_arrays))
      if (length(parent_arrays) > 0) mc_array <- mc_array + Reduce("+", parent_arrays)
    }
    
    row_data <- list(
      Element_Type = node$element_type,
      Active = ifelse(is_active == 1, "Yes", "No"),
      Leaf_Opt = if(is_leaf && !is.na(node$opt_val)) round(node$opt_val) else NA, 
      Leaf_Likely = if(is_leaf && !is.na(node$likely_val)) round(node$likely_val) else NA,
      Leaf_Pess = if(is_leaf && !is.na(node$pess_val)) round(node$pess_val) else NA, 
      Leaf_Prob = if(is_leaf && !is.na(node$chance)) round(node$chance) else NA,
      Rollup_MC_P10 = round(quantile(mc_array, 0.10, names = FALSE)),
      Rollup_MC_P50 = round(quantile(mc_array, 0.50, names = FALSE)),
      Rollup_MC_P90 = round(quantile(mc_array, 0.90, names = FALSE)),
      Updated = if(!is.na(node$updated_at)) node$updated_at else "",
      Level = level, Title = node$title
    )
    
    return_mc_array <- if (node$element_type == "Treatment" && is_active == 0) rep(0, n_iter) else mc_array
    return(list(rows = c(list(row_data), child_rows), mc_array = return_mc_array))
  }
  
  roots <- df[is.na(df$parent_id) | df$parent_id == "", ]
  all_rows <- list()
  for (i in 1:nrow(roots)) {
    all_rows <- c(all_rows, traverse(roots$id[i], 1)$rows)
  }
  
  out_df <- do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))
  for (lvl in 1:8) {
    col_name <- paste0("L", lvl)
    out_df[[col_name]] <- ifelse(out_df$Level == lvl, out_df$Title, "")
  }
  
  col_order <- c("Element_Type", "Active", paste0("L", 1:8), "Leaf_Opt", "Leaf_Likely", "Leaf_Pess", "Leaf_Prob", "Rollup_MC_P10", "Rollup_MC_P50", "Rollup_MC_P90", "Updated")
  out_df <- out_df[, col_order]
  return(out_df)
}