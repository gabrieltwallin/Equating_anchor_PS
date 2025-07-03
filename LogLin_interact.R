presmooth_select <- function(data, max_power = 5, max_interaction_power = 3, lrt_pvalue_threshold = 0.05, x_var = "X", a_var = "A") {
  models <- list()
#  lrt <- list()
  
  # Calculate significance threshold
#  adjusted_threshold <- function(threshold, num_models) {
#    1 - (1 - threshold)^(1 / (num_models - 1))
#  }
  
#  num_models <- max_power * max_power
#  adjusted_pvalue_threshold <- adjusted_threshold(lrt_pvalue_threshold, num_models)
  
  # Fit all possible models
  for (i in 1:max_power) {
    for (j in 1:max_power) {
      x_terms <- paste("I(", x_var, "^", 1:i, ")", collapse = " + ")
      a_terms <- paste("I(", a_var, "^", 1:j, ")", collapse = " + ")
      
      formula <- paste0("frequency ~ ", x_terms, " + ", a_terms)
      model <- glm(as.formula(formula),
                   family = poisson,
                   data = data,
                   x = TRUE)
      models[[paste0("Xglm", i, j)]] <- model
      
      # Perform LRT for nested models
  #    if (i > 1 || j > 1) {
  #      prev_model <- models[[paste0("Xglm", i - 1, j)]]
  #      if (is.null(prev_model)) {
  #        prev_model <- models[[paste0("Xglm", i, j - 1)]]
  #      }
        
  #      lrt_result <- anova(prev_model, model, test = "LRT")
  #      lrt[[paste0("Xglm", i, j)]] <- lrt_result$`Pr(>Chi)`[2]
  #    }
    }
  }
  
  # Remove Xglm11 from the LRT results
#  lrt <- lrt[-1]
  
  # Find the best model by LRT
 # best_lrt_model <- ""
#  for (name in names(lrt)) {
#    if (lrt[[name]] < adjusted_pvalue_threshold) {
#      best_lrt_model <- name
#    }
#  }
  
  # Extract AIC, BIC, and log-likelihood
  model_info <- lapply(models, function(x) {
    c(AIC = AIC(x),
      BIC = BIC(x)
      #LogLik = logLik(x)
      )
  })
  
  # Convert to data frame
  model_info_df <- do.call(rbind, model_info)
  rownames(model_info_df) <- names(models)
  
  # Find best models
  best_aic_model <- rownames(model_info_df)[which.min(model_info_df[, "AIC"])]
  best_bic_model <- rownames(model_info_df)[which.min(model_info_df[, "BIC"])]
  
  # STEP 2: INTERACTION TERMS
  interaction_models <- list()
  
  # Create all lower order interactions
  create_lower_order_interactions <- function(i, j, x_var, a_var) {
    terms <- c()
    for (x_power in 1:i) {
      for (a_power in 1:j) {
        terms <- c(terms, paste0("I(", x_var, "^", x_power, " * ", a_var, "^", a_power, ")"))
      }
    }
    return(paste(terms, collapse = " + "))
  }
  
  for (i in 1:max_interaction_power) {
    for (j in 1:max_interaction_power) {
      
      # Generate all interaction terms up to power i for X and j for A
      interaction_terms <- create_lower_order_interactions(i, j, x_var, a_var)
      
      aic_formula_str <- paste("frequency ~", as.character(formula(models[[best_aic_model]]))[3], "+", interaction_terms)
      bic_formula_str <- paste("frequency ~", as.character(formula(models[[best_bic_model]]))[3], "+", interaction_terms)
   #   lrt_formula_str <- paste("frequency ~", as.character(formula(models[[best_lrt_model]]))[3], "+", interaction_terms)
      
      interaction_model_aic <- glm(as.formula(aic_formula_str), family = poisson, data = data, x = TRUE)
      interaction_model_bic <- glm(as.formula(bic_formula_str), family = poisson, data = data, x = TRUE)
   #   interaction_model_lrt <- glm(as.formula(lrt_formula_str), family = poisson, data = data, x = TRUE)
      
      interaction_models[[paste0("XglmInteraction", i, j, "AIC")]] <- interaction_model_aic
      interaction_models[[paste0("XglmInteraction", i, j, "BIC")]] <- interaction_model_bic
    #  interaction_models[[paste0("XglmInteraction", i, j, "LRT")]] <- interaction_model_lrt
    }
  }
  
  # Extract AIC, BIC, and log-likelihood for interaction models
  interaction_model_info <- lapply(interaction_models, function(x) {
    c(AIC = AIC(x),
      BIC = BIC(x),
      LogLik = logLik(x))
  })
  
  # Convert to data frame
  interaction_model_info_df <- do.call(rbind, interaction_model_info)
  rownames(interaction_model_info_df) <- names(interaction_models)
  
  # Find best interaction models
  best_aic_model_interaction <- rownames(interaction_model_info_df)[which.min(interaction_model_info_df[, "AIC"])]
  best_bic_model_interaction <- rownames(interaction_model_info_df)[which.min(interaction_model_info_df[, "BIC"])]
#  best_lrt_model_interaction <- rownames(interaction_model_info_df)[which.max(interaction_model_info_df[, "LogLik"])]
  
  return(list(models = models, model_info = model_info_df, 
              aic_mod = best_aic_model, bic_mod = best_bic_model, 
             # lrt_mod = best_lrt_model, pvalue_lrt = lrt,
              best_aic_glm = models[[best_aic_model]],
              best_bic_glm = models[[best_bic_model]],
             # best_lrt_glm = models[[best_lrt_model]],
              interaction_models = interaction_models, interaction_model_info = interaction_model_info_df, 
              aic_mod_interaction = best_aic_model_interaction, 
              bic_mod_interaction = best_bic_model_interaction, 
            #  lrt_mod_interaction = best_lrt_model_interaction,
              best_aic_glm_interaction = interaction_models[[best_aic_model_interaction]],
              best_bic_glm_interaction = interaction_models[[best_bic_model_interaction]]))
            #  best_lrt_glm_interaction = interaction_models[[best_lrt_model_interaction]]))
}

                             