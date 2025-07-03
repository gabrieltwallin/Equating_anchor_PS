rm(list=ls())
graphics.off()

# Required R packages
l <- c("haven","ltm", "kequate", "ggplot2", "equateIRT", "Hmisc")
lapply(l, require, character.only = TRUE)

# Set working directory
setwd("C:/Users/walling1/Dropbox/Psychometrics/Equating/IngaMarie project/Propensity score anchor paper/Code/Simulation setup")
path <- getwd()

source(file = paste0(path,"/Updated_LogLin_Simulation_interact.R"))
source(file = paste0(path,"/model_select_3cov.R"))


# number of respondents (sample size)
N <- 1000

# total number of items, including anchor items
tot_item <- 60

# population size for P and Q, respectively
pop_size <- 200000

# number of anchor items
amax <- 20 # 

# Number of respective items in main tests X and Y
J <- tot_item - amax

# number of PS strata
nstrat <- 15 #15

# Number of simulation iterations
nrep <- 10

# Matrices to store results in
equated_scores_pop_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
equated_scores_pop_CE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_PS_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
equated_scores_PS_CE <- matrix(0,nrow = nrep, ncol = (J+1))

SEE_PS_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_PS_CE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_PS_anchor_PSE <- matrix(0,nrow = nrep, ncol= (J+1))
equated_scores_PS_anchor_CE <- matrix(0,nrow = nrep, ncol= (J+1))

SEE_PS_anchor_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_PS_anchor_CE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_PS_anchor_out_PSE <- matrix(0,nrow = nrep, ncol = (J+1))

SEE_PS_anchor_out_PSE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_neat_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_neat_PSE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_neat_CE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_neat_CE <- matrix(0,nrow = nrep, ncol = (J+1))




# Set the means and standard deviations for the two ability distributions
mean_p <- 0
sd_p <- 1
mean_q <- 0 #0.2
sd_q <- 1

# number of covariate categories (three categorical covariates)
cov1_lim <- 3 # Covariate 1 has 3 categories
cov2_lim <- 4 # Covariate 2 has 4 categories
cov3_lim <- 5 # Covariate 3 has 5 categories


set.seed(123)


# Generate the latent ability variable for each individual
# in population P and Q using normal distributions
ability_p <- rnorm(pop_size, mean = mean_p, sd = sd_p)
ability_q <- rnorm(pop_size, mean = mean_q, sd = sd_q)

# Combine the ability scores for the two populations into one vector
ability <- c(ability_p, ability_q)


###################################
#                                 #
# Generate data from an IRT model #
#                                 #
###################################

#--------------------------
# Generate item parameters
#--------------------------

# Discrimination for test X
akX <- runif(J, 0.5, 2)

# Difficulty for test X
bkX <- rnorm(J) 

# Guessing parameter for test X
ckX <- runif(J, 0, 0) # (currently setting it to 0 as we're considering the 2-PL)

# Discrimination for test Y
akY <- runif(J, 0.5, 2) 

# Difficulty for test Y
bkY <- rnorm(J) 

# Guessing parameter for test Y
ckY <- runif(J, 0, 0) # (currently setting it to 0 as we're considering the 2-PL)

# Discrimination for anchor test
akA <- runif(amax, 0.5, 2)

# Difficulty for anchor test 
bkA <- rnorm(amax)

# Guessing parameter for anchor test 
ckA <- runif(amax, 0, 0) # (currently setting it to 0 as we're considering the 2-PL)

#------------------------------------
# item parameters for the covariates
#------------------------------------

# Depending on if you want lower, moderate or higher correlation scenario, run one of the following:

# Lower correlation scenario
#akC1 <- runif(1, 0.1, 0.5)
#bkC1 <- rnorm(1)
#ckC1 <- runif(1, 0, 0)

#akC2 <- runif(1, 0.1, 0.5)
#bkC2 <- rnorm(1)
#ckC2 <- runif(1, 0, 0)

#akC3 <- runif(1, 0.1, 0.5)
#bkC3 <- rnorm(1)
#ckC3 <- runif(1, 0, 0)


# Moderate correlation scenario
akC1 <- runif(1, 0.5, 1.5)
bkC1 <- rnorm(1)
ckC1 <- runif(1, 0, 0)

akC2 <- runif(1, 0.5, 1.5)
bkC2 <- rnorm(1)
ckC2 <- runif(1, 0, 0)

akC3 <- runif(1, 0.5, 1.5)
bkC3 <- rnorm(1)
ckC3 <- runif(1, 0, 0)


# Higher correlation scenario
#akC1 <- runif(1, 1.5, 2)
#bkC1 <- rnorm(1)
#ckC1 <- runif(1, 0, 0)

#akC2 <- runif(1, 1.5, 2)
#bkC2 <- rnorm(1)
#ckC2 <- runif(1, 0, 0)

#akC3 <- runif(1, 1.5, 2)
#bkC3 <- rnorm(1)
#ckC3 <- runif(1, 0, 0)



dataP <- matrix(0, nrow = pop_size, ncol = (tot_item + cov1_lim+cov2_lim+cov3_lim))
dataQ <- matrix(0, nrow = pop_size, ncol = (tot_item + cov1_lim+cov2_lim+cov3_lim))

# Generate data from a 2-PL model
# First J columns are item responses for main test. 
# Thereafter anchor items, and thereafter the covariates

for(i in 1:nrow(dataP)){
  dataP[i, 1:J] <- ckX + ((1 - ckX) / (1 + exp(-akX * (ability_p[i] - bkX)))) > runif(J)
  dataP[i, (J + 1):tot_item] <- ckA + ((1 - ckA) / (1 + exp(-akA * (ability_p[i] - bkA)))) > runif(amax)
  dataP[i, (tot_item+1):(tot_item+cov1_lim)] <- ckC1 + ((1 - ckC1) / (1 + exp(-akC1 * (ability_p[i] - bkC1)))) > runif(cov1_lim)
  dataP[i, (tot_item+cov1_lim+1):(tot_item+cov1_lim+cov2_lim)] <- ckC2 + ((1 - ckC2) / (1 + exp(-akC2 * (ability_p[i] - bkC2)))) > runif(cov2_lim)
  dataP[i, (tot_item+cov1_lim+cov2_lim+1):(tot_item+cov1_lim+cov2_lim+cov3_lim)] <- ckC3 + ((1 - ckC3) / (1 + exp(-akC3 * (ability_p[i] - bkC3)))) > runif(cov3_lim)
}

for(i in 1:nrow(dataQ)){
  dataQ[i, 1:J] <- ckY + ((1 - ckY) / (1 + exp(-akY * (ability_q[i] - bkY)))) > runif(J)
  dataQ[i, (J + 1):tot_item] <- ckA + ((1 - ckA) / (1 + exp(-akA * (ability_q[i] - bkA)))) > runif(amax)
  dataQ[i, (tot_item+1):(tot_item+cov1_lim)]  <- ckC1 + ((1 - ckC1) / (1 + exp(-akC1 * (ability_q[i] - bkC1)))) > runif(cov1_lim)
  dataQ[i, (tot_item+cov1_lim+1):(tot_item+cov1_lim+cov2_lim)] <- ckC2 + ((1 - ckC2) / (1 + exp(-akC2 * (ability_q[i] - bkC2)))) > runif(cov2_lim)
  dataQ[i, (tot_item+cov1_lim+cov2_lim+1):(tot_item+cov1_lim+cov2_lim+cov3_lim)] <- ckC3 + ((1 - ckC3) / (1 + exp(-akC3 * (ability_q[i] - bkC3)))) > runif(cov3_lim)
}

# Calculate the sum score in the populations
sum_score_P <- rowSums(dataP[,1:J])
sum_score_Q <- rowSums(dataQ[,1:J])

# Add sum scores from P and Q into one vector
sum_score <- c(sum_score_P, sum_score_Q)

# Calculate the sum score for anchor items in the populations
anchor_score_P <- rowSums(dataP[,(J+1):tot_item])
anchor_score_Q <- rowSums(dataQ[,(J+1):tot_item])

# Add anchor sum scores from P and Q into one vector
anchor_score <- c(anchor_score_P, anchor_score_Q)

# Extract the three covariates generated from the 2-PL model
covariate1_score_P <- rowSums(dataP[, (tot_item + 1):(tot_item + cov1_lim)])
covariate1_score_Q <-  rowSums(dataQ[, (tot_item + 1):(tot_item + cov1_lim)])
covariate1_score <- c(covariate1_score_P, covariate1_score_Q)

covariate2_score_P <- rowSums(dataP[, (tot_item + cov1_lim+1):(tot_item + cov1_lim+cov2_lim)])
covariate2_score_Q <- rowSums(dataQ[, (tot_item + cov1_lim+1):(tot_item + cov1_lim+cov2_lim)])
covariate2_score <- c(covariate2_score_P, covariate2_score_Q)

covariate3_score_P <- rowSums(dataP[, (tot_item + cov1_lim+cov2_lim+1):(tot_item + cov1_lim+cov2_lim+cov3_lim)])
covariate3_score_Q <- rowSums(dataQ[, (tot_item + cov1_lim+cov2_lim+1):(tot_item + cov1_lim+cov2_lim+cov3_lim)])
covariate3_score <- c(covariate3_score_P, covariate3_score_Q)


#J=40, A=20
p <- 1 / (1 + exp(0.3 * anchor_score - 0.6 * covariate1_score - 0.7 * covariate2_score - 0.5 * covariate3_score))

#J=80, A=40
#p <- 1 / (1 + exp(0.2 * anchor_score - 0.3 * covariate1_score - 1 * covariate2_score - 0.5 * covariate3_score))
mean(p)

# Administer X/Y to test-takers
z <- rbinom(2*pop_size, 1, p)

df <- data.frame(sum_score = sum_score,
                 anchor_score = anchor_score,
                 covariate1_score = covariate1_score,
                 covariate2_score = covariate2_score,
                 covariate3_score = covariate3_score,
                 treatment = z)

# Add categorized propensity scores to the population-level data
df$cat_propscore_anchor_pop <- as.numeric(cut2(p, g = nstrat))

# Split the population-level data into two groups based on treatment
pop_P <- df[df$treatment == 0, ]
pop_Q <- df[df$treatment == 1, ]


# Calculate the frequency tables for the population-level data
XAfreq_PSwC_pop <- kefreq(pop_P$sum_score, 0:J, pop_P$cat_propscore_anchor_pop, 1:nstrat)
YAfreq_PSwC_pop <- kefreq(pop_Q$sum_score, 0:J, pop_Q$cat_propscore_anchor_pop, 1:nstrat)

names(XAfreq_PSwC_pop) <- c("X", "A", "frequency")
names(YAfreq_PSwC_pop) <- c("Y", "A", "frequency")

# Fit the log-linear models for the population-level data - using anchor and covariates
model_select_XA_PSwC_pop <- presmooth_select(data = XAfreq_PSwC_pop,
                                             max_power = 5,
                                             max_interaction_power = 2,
                                             lrt_pvalue_threshold = 0.05,
                                             x_var = "X",
                                             a_var = "A")

model_select_YA_PSwC_pop <- presmooth_select(data = YAfreq_PSwC_pop,
                                             max_power = 5,
                                             max_interaction_power = 2,
                                             lrt_pvalue_threshold = 0.05,
                                             x_var = "Y",
                                             a_var = "A")


############

# Calculate the frequency tables for the population-level data - for anchor-only method
#XAfreq_anchor_pop <- kefreq(pop_P$sum_score, 0:J, pop_P$anchor_score, 0:amax)
#YAfreq_anchor_pop <- kefreq(pop_Q$sum_score, 0:J, pop_Q$anchor_score, 0:amax)

#names(XAfreq_anchor_pop) <- c("X", "A", "frequency")
#names(YAfreq_anchor_pop) <- c("Y", "A", "frequency")

# Fit the log-linear models for the population-level data - using anchor and covariates
#model_select_XA_anchor_pop <- presmooth_select(data = XAfreq_anchor_pop,
#                                             max_power = 5,
#                                             max_interaction_power =2,
#                                             lrt_pvalue_threshold = 0.05,
#                                             x_var = "X",
#                                             a_var = "A")

#model_select_YA_anchor_pop <- presmooth_select(data = YAfreq_anchor_pop,
#                                             max_power = 5,
#                                             max_interaction_power = 2,
#                                             lrt_pvalue_threshold = 0.05,
#                                             x_var = "Y",
#                                             a_var = "A")



#----------------------------------------------------
# Perform equating using the population-level models
#----------------------------------------------------

# PSE
equating_pop_PSE <- kequate("NEAT_PSE",
                             0:J,
                             0:J,
                             model_select_XA_PSwC_pop$best_bic_glm_interaction,
                             model_select_YA_PSwC_pop$best_bic_glm_interaction,
                             kernel = "gaussian")

# The true equating function for PSE
equated_scores_pop_PSE <- t(matrix(rep(equating_pop_PSE@equating$eqYx, nrep), 
                               nrow = J+1, 
                               ncol = nrep))

# CE
equating_pop_CE <- kequate("NEAT_CE",
                            0:J,
                            0:J,
                            1:nstrat,
                            model_select_XA_PSwC_pop$best_bic_glm_interaction,
                            model_select_YA_PSwC_pop$best_bic_glm_interaction,
                            kernel = "gaussian")

# The true equating function for CE
equated_scores_pop_CE <- t(matrix(rep(equating_pop_CE@equating$eqYx, nrep), 
                                   nrow = J+1, 
                                   ncol = nrep))



######

# PSE - anchor only
#equating_anchor_pop_PSE <- kequate("NEAT_PSE",
#                            0:J,
#                            0:J,
#                            model_select_XA_anchor_pop$best_bic_glm_interaction,
#                            model_select_YA_anchor_pop$best_bic_glm_interaction,
#                            kernel = "gaussian")

# The true equating function for PSE
#equated_scores_anchor_pop_PSE <- t(matrix(rep(equating_anchor_pop_PSE@equating$eqYx, nrep), 
#                                   nrow = J+1, 
#                                   ncol = nrep))

# CE
#equating_anchor_pop_CE <- kequate("NEAT_CE",
#                           0:J,
#                           0:J,
#                           0:amax,
#                           model_select_XA_anchor_pop$best_bic_glm_interaction,
#                           model_select_YA_anchor_pop$best_bic_glm_interaction,
#                           kernel = "gaussian")

# The true equating function for CE
#equated_scores_anchor_pop_CE <- t(matrix(rep(equating_anchor_pop_CE@equating$eqYx, nrep), 
#                                  nrow = J+1, 
#                                 ncol = nrep))



for(k in 1:nrep){
  
  random_rows <- sample(1:nrow(df), 2*N)
  tot_samp <- df[random_rows, ]
  
  samp_P <- tot_samp[tot_samp$treatment == 0, ]
  samp_Q <- tot_samp[tot_samp$treatment == 1, ]
  
  tot_samp <- rbind(samp_P, samp_Q)
  
  ####################################################
  #                                                  #
  #   METHOD 1: Equating with propensity score       # 
  #   that includes only covariates and no anchor    #
  #                                                  #
  ####################################################
  
  # Estimate propensity score
  mod1 <- glm(treatment ~ covariate1_score + covariate2_score + covariate3_score, 
              data = tot_samp, family = binomial(logit))
  
  propscore_mod1 <- as.vector(mod1$fitted.values)
  
  tot_samp$PropensityScore_mod1 <- propscore_mod1
  
  # Categorize the propensity scores into "nstrat" groups
  tot_samp$cat_propscore_mod1 <- as.numeric(cut2(propscore_mod1, g = nstrat))
  
  # Split into P and Q sample, respectively
  samp_P$cat_ps <- tot_samp$cat_propscore_mod1[tot_samp$treatment == 0]
  samp_Q$cat_ps <- tot_samp$cat_propscore_mod1[tot_samp$treatment == 1]
  
  # Calculate frequency tables for main test score and categorised propensity score
  XAfreq_PS <- kefreq(samp_P$sum_score, 0:J, samp_P$cat_ps, 1:nstrat)
  YAfreq_PS <- kefreq(samp_Q$sum_score, 0:J, samp_Q$cat_ps, 1:nstrat)
  
  names(XAfreq_PS) <- c("X","A","frequency")
  names(YAfreq_PS) <- c("Y","A","frequency")
  
  # Best fitting log-linear models by AIC, BIC and LRT for XA:
  model_select_XA_PS <- presmooth_select(data = XAfreq_PS, 
                                         max_power = 5,
                                         max_interaction_power = 2, 
                                         lrt_pvalue_threshold = 0.05, 
                                         x_var = "X", 
                                         a_var = "A")
  
  
  # Best fitting log-linear models by AIC, BIC and LRT for YA:
  model_select_YA_PS <- presmooth_select(data = YAfreq_PS, 
                                         max_power = 5, 
                                         max_interaction_power = 2, 
                                         lrt_pvalue_threshold = 0.05, 
                                         x_var = "Y", 
                                         a_var = "A")
  
  # Equate test forms using post-stratification equating
  equating_PS_PSE <- kequate("NEAT_PSE", 
                         0:J, 
                         0:J, 
                         model_select_XA_PS$best_bic_glm_interaction, 
                         model_select_YA_PS$best_bic_glm_interaction, 
                         kernel = "gaussian") 
  
  # save the equated scores and the SEE
  equated_scores_PS_PSE[k,] <- equating_PS_PSE@equating$eqYx
  SEE_PS_PSE[k,] <- equating_PS_PSE@equating$SEEYx
  
  
  # Equate test forms using chained equating
  equating_PS_CE <- kequate("NEAT_CE", 
                         0:J, 
                         0:J, 
                         1:nstrat,
                         model_select_XA_PS$best_bic_glm_interaction, 
                         model_select_YA_PS$best_bic_glm_interaction, 
                         kernel = "gaussian") 
  
  # save the equated scores and the SEE
  equated_scores_PS_CE[k,] <- equating_PS_CE@equating$eqYx
  SEE_PS_CE[k,] <- equating_PS_CE@equating$SEEYx
  
  
  ######################################################################
  #                                                                    #
  #     METHOD 2: Equating with propensity score that includes         # 
  #     both covariates and anchor scores                              #
  #                                                                    #
  ######################################################################
  
  # Estimate propensity score
  ps_mod_anchor <- glm(treatment ~ anchor_score + covariate1_score + covariate2_score + covariate3_score,
                       data = tot_samp, family = binomial(logit))
  
  propscore_anchor <- as.vector(ps_mod_anchor$fitted.values)
  
  tot_samp$PropensityScore_anchor <- propscore_anchor
  
  # Add categorized propensity scores
  tot_samp$cat_propscore_anchor <- as.numeric(cut2(propscore_anchor, g = nstrat))
  
  samp_P$cat_ps_anchor <- tot_samp$cat_propscore_anchor[tot_samp$treatment == 0]
  samp_Q$cat_ps_anchor <- tot_samp$cat_propscore_anchor[tot_samp$treatment == 1]
  
  
  XAfreq_PSwC <- kefreq(samp_P$sum_score, 0:J, samp_P$cat_ps_anchor, 1:nstrat)
  YAfreq_PSwC <- kefreq(samp_Q$sum_score, 0:J, samp_Q$cat_ps_anchor, 1:nstrat)
  
  names(XAfreq_PSwC) <- c("X", "A", "frequency")
  names(YAfreq_PSwC) <- c("Y", "A", "frequency")
  
  model_select_XA_PSwC <- presmooth_select(data = XAfreq_PSwC,
                                           max_power = 5,
                                           max_interaction_power = 2,
                                           lrt_pvalue_threshold = 0.05,
                                           x_var = "X",
                                           a_var = "A")
  
  model_select_YA_PSwC <- presmooth_select(data = YAfreq_PSwC,
                                           max_power = 5,
                                           max_interaction_power = 2,
                                           lrt_pvalue_threshold = 0.05,
                                           x_var = "Y",
                                           a_var = "A")
  
  equating_PSwC_PSE <- kequate("NEAT_PSE",
                           0:J,
                           0:J,
                           model_select_XA_PSwC$best_bic_glm_interaction,
                           model_select_YA_PSwC$best_bic_glm_interaction,
                           kernel = "gaussian")
  
  equated_scores_PS_anchor_PSE[k,] <- equating_PSwC_PSE@equating$eqYx
  SEE_PS_anchor_PSE[k,] <- equating_PSwC_PSE@equating$SEEYx
  
  
  equating_PSwC_CE <- kequate("NEAT_CE",
                           0:J,
                           0:J,
                           1:nstrat,
                           model_select_XA_PSwC$best_bic_glm_interaction,
                           model_select_YA_PSwC$best_bic_glm_interaction,
                           kernel = "gaussian")
  
  equated_scores_PS_anchor_CE[k,] <- equating_PSwC_CE@equating$eqYx
  SEE_PS_anchor_CE[k,] <- equating_PSwC_CE@equating$SEEYx
  
  
  
  
  ####################################################################
  #                                                                  #
  # METHOD 3: Equating with anchor score outside of propensity score #
  #                                                                  #
  ####################################################################
  
   dat_X_ps_out <- as.data.frame(cbind(samp_P$sum_score, samp_P$cat_ps, samp_P$anchor_score)) 
   dat_Y_ps_out <- as.data.frame(cbind(samp_Q$sum_score, samp_Q$cat_ps, samp_Q$anchor_score))

   freq_P_anchor_out <- as.data.frame(table(factor(dat_X_ps_out$V1, levels = 0:J, ordered = TRUE),
                                             factor(dat_X_ps_out$V2, levels = 1:nstrat, ordered = TRUE),
                                             factor(dat_X_ps_out$V3, levels = 0:amax, ordered = TRUE)))
  
   freq_Q_anchor_out <- as.data.frame(table(factor(dat_Y_ps_out$V1, levels = 0:J, ordered = TRUE),
                                           factor(dat_Y_ps_out$V2, levels = 1:nstrat, ordered = TRUE),
                                           factor(dat_Y_ps_out$V3, levels = 0:amax, ordered = TRUE)))
  
  dataPnec_anchor_out <- data.frame(frequency=freq_P_anchor_out$Freq, 
                                    Var1=rep(0:J, nstrat), 
                                    Var2=rep(1:nstrat, each=(J+1)),
                                    Var3=rep(0:amax, each=nstrat*(J+1)))
  
  dataQnec_anchor_out <- data.frame(frequency=freq_Q_anchor_out$Freq, 
                                   Var1=rep(0:J, nstrat), 
                                    Var2=rep(1:nstrat, each=(J+1)),
                                    Var3=rep(0:amax, each=nstrat*(J+1)))
  
  # Reorder the columns to align with model-selection algorithm 
   dataPnec_anchor_out <- dataPnec_anchor_out[,c(2,3,4,1)]
   dataQnec_anchor_out <- dataQnec_anchor_out[,c(2,3,4,1)]
  
   names(dataPnec_anchor_out) <- c("X","PS","A", "frequency")
   names(dataQnec_anchor_out) <- c("Y","PS","A", "frequency")
  
  
  model_select_XA_PSwoA <- presmooth_select_3cov(data = dataPnec_anchor_out, 
                                                   max_power = 3, 
                                                   max_interaction_power = 2, 
                                                   x_var = "X",
                                                   ps_var = "PS",
                                                   a_var = "A")
  
  model_select_YA_PSwoA <- presmooth_select_3cov(data = dataQnec_anchor_out, 
                                                  max_power = 3, 
                                                   max_interaction_power = 2, 
                                                   x_var = "Y",
                                                   ps_var = "PS",
                                                   a_var = "A")
  
  equating_PSwoA_PSE <- kequate("NEAT_PSE", 
                              0:J, 
                              0:J, 
                              model_select_XA_PSwoA$best_bic_glm_interaction, 
                              model_select_YA_PSwoA$best_bic_glm_interaction, 
                              kernel = "gaussian") 
  
    equated_scores_PS_anchor_out_PSE[k,] <- equating_PSwoA_PSE@equating$eqYx
    SEE_PS_anchor_out_PSE[k,] <- equating_PSwoA_PSE@equating$SEEYx
  
  
  
  #################
  #               #
  # NEAT equating #
  #               #
  #################
  
  XAfreq_neat <- kefreq(samp_P$sum_score, 0:J, samp_P$anchor_score, 0:amax)
  YAfreq_neat <- kefreq(samp_Q$sum_score, 0:J, samp_Q$anchor_score, 0:amax)
  
  names(XAfreq_neat) <- c("X","A","frequency")
  names(YAfreq_neat) <- c("Y","A","frequency")
  
  # Best fitting log-linear models by AIC, BIC and LRT for XA:
  model_select_XA_neat <- presmooth_select(data = XAfreq_neat, 
                                           max_power = 5,
                                           max_interaction_power = 2, 
                                           lrt_pvalue_threshold = 0.05, 
                                           x_var = "X", 
                                           a_var = "A")
  
  # Best fitting log-linear models by AIC, BIC and LRT for YA:
  model_select_YA_neat <- presmooth_select(data = YAfreq_neat, 
                                           max_power = 5, 
                                           max_interaction_power = 2, 
                                           lrt_pvalue_threshold = 0.05, 
                                           x_var = "Y", 
                                           a_var = "A")
  
  equating_neat_PSE <- kequate("NEAT_PSE", 
                               0:J, 
                               0:J, 
                               model_select_XA_neat$best_bic_glm_interaction, 
                               model_select_YA_neat$best_bic_glm_interaction, 
                               kernel = "gaussian") 
  
  equated_scores_neat_PSE[k,] <- equating_neat_PSE@equating$eqYx
  SEE_neat_PSE[k,] <- equating_neat_PSE@equating$SEEYx
  
  
  equating_neat_CE <- kequate("NEAT_CE", 
                               0:J, 
                               0:J, 
                               0:amax,
                               model_select_XA_neat$best_bic_glm_interaction, 
                               model_select_YA_neat$best_bic_glm_interaction, 
                               kernel = "gaussian") 
  
  equated_scores_neat_CE[k,] <- equating_neat_CE@equating$eqYx
  SEE_neat_CE[k,] <- equating_neat_CE@equating$SEEYx
  
  
  print(k)
}

# Save the results to CSV files
write.csv(equated_scores_PS_PSE, "equated_scores_PS_PSE.csv", row.names = FALSE)
write.csv(SEE_PS_PSE, "SEE_PS_PSE.csv", row.names = FALSE)
write.csv(equated_scores_PS_CE, "equated_scores_PS_CE.csv", row.names = FALSE)
write.csv(SEE_PS_CE, "SEE_PS_CE.csv", row.names = FALSE)
write.csv(equated_scores_PS_anchor_PSE, "equated_scores_PS_anchor_PSE.csv", row.names = FALSE)
write.csv(SEE_PS_anchor_PSE, "SEE_PS_anchor_PSE.csv", row.names = FALSE)
write.csv(equated_scores_PS_anchor_CE, "equated_scores_PS_anchor_CE.csv", row.names = FALSE)
write.csv(SEE_PS_anchor_CE, "SEE_PS_anchor_CE.csv", row.names = FALSE)
write.csv(equated_scores_PS_anchor_out_PSE, "equated_scores_PS_anchor_out_PSE.csv", row.names = FALSE)
write.csv(SEE_PS_anchor_out_PSE, "SEE_PS_anchor_out_PSE.csv", row.names = FALSE)
write.csv(equated_scores_neat_PSE, "equated_scores_neat_PSE.csv", row.names = FALSE)
write.csv(SEE_neat_PSE, "SEE_neat_PSE.csv", row.names = FALSE)
write.csv(equated_scores_neat_CE, "equated_scores_neat_CE.csv", row.names = FALSE)
write.csv(SEE_neat_CE, "SEE_neat_CE.csv", row.names = FALSE)
write.csv(equated_scores_pop_PSE, "equated_scores_pop_PSE.csv", row.names = FALSE)
write.csv(equated_scores_pop_CE, "equated_scores_pop_CE.csv", row.names = FALSE)