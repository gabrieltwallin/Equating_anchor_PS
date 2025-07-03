rm(list=ls())
graphics.off()

library(ggplot2)

#####################
#                   #
#   bias function   #
#                   #
#####################

Bias <- function(results, parameter){
  rel.bias <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for(i in 1:nrow(results)){
    rel.bias[i,] <- (results[i,]-parameter[i,])
  }
  res <- apply(rel.bias, 2, mean)
  return(res) 
}



######################
#                    #    
#   RMSE function    #     
#                    # 
######################

RMSE <- function(results, parameter){
  rel.mse <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for (i in 1:nrow(results)){
    rel.mse[i,] <- (results[i,] - parameter[i,])^2
  }
  res <- sqrt(apply(rel.mse, 2, mean))
  return(res) 
}



#####################
#                   #    
#   SE function     #     
#                   # 
#####################

SE <- function(results){
  rel.se <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for (i in 1:nrow(results)){
    rel.se[i,] <- results[i,]
  }
  res <- apply(rel.se, 2, sd)
  return(res) 
}

# Load data
load("N1=5000_J40_A20.RData")


# Bias for all equating functions under consideration                             
bias_PS_PSE <- Bias(equated_scores_PS_PSE, equated_scores_pop_PSE)  
bias_PSwA_PSE <- Bias(equated_scores_PS_anchor_PSE, equated_scores_pop_PSE)  
bias_PSwoA_PSE <- Bias(equated_scores_PS_anchor_out_PSE, equated_scores_pop_PSE)
bias_neat_PSE <- Bias(equated_scores_neat_PSE, equated_scores_pop_PSE)

bias_PS_CE <- Bias(equated_scores_PS_CE, equated_scores_pop_CE)  
bias_PSwA_CE <- Bias(equated_scores_PS_anchor_CE, equated_scores_pop_CE)  
bias_neat_CE <- Bias(equated_scores_neat_CE, equated_scores_pop_CE)

bias_neat_CE2 <- Bias(equated_scores_neat_CE, equated_scores_pop_PSE)
bias_neat_CE <- (equating_neat_CE@equating$eqYx - 0:J)


# Simulation SE for all equating functions under consideration                             
SE_PS_PSE <- SE(equated_scores_PS_PSE)
SE_PSwA_PSE <- SE(equated_scores_PS_anchor_PSE)
SE_PSwoA_PSE <- SE(equated_scores_PS_anchor_out_PSE)
SE_neat_PSE <- SE(equated_scores_neat_PSE)

SE_PS_CE <- SE(equated_scores_PS_CE)
SE_PSwA_CE <- SE(equated_scores_PS_anchor_CE)
SE_neat_CE <- SE(equated_scores_neat_CE)

# RMSE for all equating functions under consideration                             
rmse_PS_PSE <- RMSE(equated_scores_PS_PSE, equated_scores_pop_PSE)
rmse_PSwA_PSE <- RMSE(equated_scores_PS_anchor_PSE, equated_scores_pop_PSE) 
rmse_PSwoA_PSE <- RMSE(equated_scores_PS_anchor_out_PSE, equated_scores_pop_PSE)
rmse_neat_PSE <- RMSE(equated_scores_neat_PSE, equated_scores_anchor_pop_PSE)

rmse_PS_CE <- RMSE(equated_scores_PS_CE, equated_scores_pop_CE)
rmse_PSwA_CE <- RMSE(equated_scores_PS_anchor_CE, equated_scores_pop_CE) 
rmse_neat_CE <- RMSE(equated_scores_neat_CE, equated_scores_anchor_pop_CE)


# SEE for all equating functions under consideration  
SEE_PS_PSE <- colMeans(SEE_PS_PSE)
SEE_PSwA_PSE <- colMeans(SEE_PS_anchor_PSE) 
SEE_PSwoA_PSE <- colMeans(SEE_PS_anchor_out_PSE)
SEE_neat_PSE <- colMeans(SEE_neat_PSE)

SEE_PS_CE <- colMeans(SEE_PS_CE)
SEE_PSwA_CE <- colMeans(SEE_PS_anchor_CE) 
SEE_neat_CE <- colMeans(SEE_neat_CE)

#############
#           #
# Plot bias #
#           #
#############
J <- 40
# Plot the bias for each estimator
Scores <- 0:J

bias_df_PSE <- data.frame(#bias_PS_PSE, 
  bias_PSwA_PSE, 
  bias_PSwoA_PSE, 
  bias_neat_PSE,
  Scores)

eq_labels <- c(#"bias_PS" = "solid", 
  "bias_PSwA" = "dotted", 
  "bias_PSwoA" = "dashed",
  "bias_neat" = "dotdash")


plot_bias_PSE <- ggplot(data = bias_df_PSE, aes(x=Scores)) +
  # geom_line(aes(y = bias_PS_PSE, linetype = "bias_PS")) + 
  geom_line(aes(y = bias_PSwA_PSE, linetype = "bias_PSwA")) +
  geom_line(aes(y = bias_PSwoA_PSE, linetype = "bias_PSwoA")) + 
  geom_line(aes(y = bias_neat_PSE, linetype = "bias_neat")) + 
  geom_hline(yintercept = 0) +
  labs(x = "Scores",
       y = "Bias",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = eq_labels) 

plot_bias_PSE


# Bias for the CE estimators
bias_df_CE <- data.frame(#bias_PS_PSE, 
  bias_PSwA_CE, 
#  bias_PSwoA_CE, 
  bias_neat_CE,
  Scores)

eq_labels <- c(#"bias_PS" = "solid", 
  "bias_PSwA" = "dotted", 
#  "bias_PSwoA" = "dashed",
  "bias_neat" = "dotdash")


plot_bias_CE <- ggplot(data = bias_df_CE, aes(x=Scores)) +
  # geom_line(aes(y = bias_PS_PSE, linetype = "bias_PS")) + 
  geom_line(aes(y = bias_PSwA_CE, linetype = "bias_PSwA")) +
  #geom_line(aes(y = bias_PSwoA_CE, linetype = "bias_PSwoA")) + 
  geom_line(aes(y = bias_neat_CE, linetype = "bias_neat")) + 
  geom_hline(yintercept = 0) +
  labs(x = "Scores",
       y = "Bias",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = eq_labels) 

plot_bias_CE

###########
#         #
# Plot SE #
#         #
###########

Scores <- 0:J
SE_df_PSE <- data.frame(#SE_PS_PSE, 
                      SE_PSwA_PSE, 
                      SE_PSwoA_PSE,
                      SE_neat_PSE,
                      Scores)

eq_labels <- c(#"SE_PS" = "solid", 
               "SE_PSwA" = "dotted", 
               "SE_PSwoA" = "dashed",
               "SE_neat" = "dotdash")


plot_SE_PSE <- ggplot(data = SE_df_PSE, aes(x=Scores)) +
  #geom_line(aes(y = SE_PS_PSE, linetype = "SE_PS")) + 
  geom_line(aes(y = SE_PSwA_PSE, linetype = "SE_PSwA")) +
  geom_line(aes(y = SE_PSwoA_PSE, linetype = "SE_PSwoA")) +
  geom_line(aes(y = SE_neat_PSE, linetype = "SE_neat")) +
  labs(x = "Scores",
       y = "SE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = eq_labels) 

plot_SE_PSE




#############
#           #
# Plot RMSE #
#           #
#############

Scores <- 0:J
rmse_df_PSE <- data.frame(#rmse_PS_PSE, 
                      rmse_PSwA_PSE, 
                      rmse_PSwoA_PSE, 
                      rmse_neat_PSE,
                      Scores)

eq_labels <- c(#"rmse_PS" = "solid", 
               "rmse_PSwA" = "dotted", 
               "rmse_PSwoA" = "dashed",
               "rmse_neat" = "dotdash")


plot_rmse_PSE <- ggplot(data = rmse_df_PSE, aes(x=Scores)) +
 # geom_line(aes(y = rmse_PS_PSE, linetype = "rmse_PS")) + 
  geom_line(aes(y = rmse_PSwA_PSE, linetype = "rmse_PSwA")) +
  geom_line(aes(y = rmse_PSwoA_PSE, linetype = "rmse_PSwoA")) + 
  geom_line(aes(y = rmse_neat_PSE, linetype = "rmse_neat")) + 
  labs(x = "Scores",
       y = "RMSE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = eq_labels) 

plot_rmse_PSE


############
#          #
# Plot SEE #
#          #
############

Scores <- 0:J
SEE_df_PSE <- data.frame(#SEE_PS_PSE, 
                      SEE_PSwA_PSE, 
                      SEE_PSwoA_PSE,
                     SEE_neat_PSE,
                      Scores)

eq_labels <- c(#"SEE_PS" = "solid", 
               "SEE_PSwA" = "dotted", 
               "SEE_PSwoA" = "dashed",
               "SEE_neat" = "dotdash")


plot_SEE_PSE <- ggplot(data = SEE_df_PSE, aes(x=Scores)) +
 # geom_line(aes(y = SEE_PS_PSE, linetype = "SEE_PS")) + 
  geom_line(aes(y = SEE_PSwA_PSE, linetype = "SEE_PSwA")) +
  geom_line(aes(y = SEE_PSwoA_PSE, linetype = "SEE_PSwoA")) +
  geom_line(aes(y = SEE_neat_PSE, linetype = "SEE_neat")) +
  labs(x = "Scores",
       y = "SEE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = eq_labels) 

plot_SEE_PSE



# --> CE plots are excluded but can be generated using the code above, 
# just replace the PSE estimators with the CE estimators