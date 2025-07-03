presmooth_select_3cov <- function(data, max_power = 5, max_interaction_power = 3, x_var = "X", a_var = "A", ps_var = "PS") {
  models <- list()
  
  # Fit all possible models
  for (i in 1:max_power) {
    for (j in 1:max_power) {
      for (k in 1:max_power) {
        x_terms <- paste("I(", x_var, "^", 1:i, ")", collapse = " + ")
        a_terms <- paste("I(", a_var, "^", 1:j, ")", collapse = " + ")
        ps_terms <- paste("I(", ps_var, "^", 1:k, ")", collapse = " + ")
        
        formula <- paste0("frequency ~ ", x_terms, " + ", a_terms, " + ", ps_terms)
        model <- glm(as.formula(formula), family = poisson, data = data, x = TRUE)
        models[[paste0("Xglm", i, j, k)]] <- model
      }
    }
  }
  
  # Extract BIC
  model_info <- lapply(models, function(x) {
    c(BIC = BIC(x))
  })
  
  # Convert to data frame
  model_info_df <- do.call(rbind, model_info)
  rownames(model_info_df) <- names(models)
  
  # Find best model by BIC
  best_bic_model <- rownames(model_info_df)[which.min(model_info_df[, "BIC"])]
  
  # STEP 2: INTERACTION TERMS
  interaction_models <- list()
  
  # Create all lower order interactions
  create_lower_order_interactions <- function(i, j, var1, var2) {
    terms <- c()
    for (x_power in 1:i) {
      for (a_power in 1:j) {
        terms <- c(terms, paste0("I(", var1, "^", x_power, " * ", var2, "^", a_power, ")"))
      }
    }
    return(terms)
  }
  
  for (i in 1:max_interaction_power) {
    for (j in 1:max_interaction_power) {
      for (l in 1:max_interaction_power) {
        interaction_terms_x_a <- create_lower_order_interactions(i, j, x_var, a_var)
        interaction_terms_x_ps <- create_lower_order_interactions(i, l, x_var, ps_var)
        interaction_terms_a_ps <- create_lower_order_interactions(j, l, a_var, ps_var)
        
        interaction_terms <- c(create_lower_order_interactions(i, j, x_var, a_var),
                               create_lower_order_interactions(i, l, x_var, ps_var),
                               create_lower_order_interactions(j, l, a_var, ps_var))
        
        interaction_terms_str <- paste(interaction_terms, collapse = " + ")
        
        bic_formula_str <- paste("frequency ~", as.character(formula(models[[best_bic_model]]))[3], " + ", interaction_terms_str)
        
        interaction_model_bic <- glm(as.formula(bic_formula_str), family = poisson, data = data, x = TRUE)
        
        interaction_models[[paste0("XglmInteraction", i, j, l, "BIC")]] <- interaction_model_bic
      }
    }
  }
  
  # Extract BIC for interaction models
  interaction_model_info <- lapply(interaction_models, function(x) {
    c(BIC = BIC(x))
  })
  
  # Convert to data frame
  interaction_model_info_df <- do.call(rbind, interaction_model_info)
  rownames(interaction_model_info_df) <- names(interaction_models)
  
  # Find best interaction model by BIC
  best_bic_model_interaction <- rownames(interaction_model_info_df)[which.min(interaction_model_info_df[, "BIC"])]
  
  return(list(models = models, model_info = model_info_df, 
              bic_mod = best_bic_model, 
              best_bic_glm = models[[best_bic_model]],
              interaction_models = interaction_models, interaction_model_info = interaction_model_info_df, 
              bic_mod_interaction = best_bic_model_interaction,
              best_bic_glm_interaction = interaction_models[[best_bic_model_interaction]]))
}
