########################################################
# MORPHEME CONGRUENCY ANALYSIS
########################################################

###############################
# STEP 1. PACKAGES
###############################

# Install once if needed:

library(dplyr)
library(boot)
library(writexl)

###############################
# STEP 2. LOAD DATA
###############################

df <- read.csv(
  "accuracy_congruency.csv",
  na.strings="#DIV/0!"
)


###############################
# STEP 3. CREATE ACCURACY SCORES
###############################

# Number of obligatory contexts
df$OBC <- (
  df$correct +
    df$misformed +
    df$no_suppliance
)

# SOC
df$SOC <- (
  df$correct +
    (.5 * df$misformed)
) / df$OBC

# TLU
df$TLU <- (
  df$correct
) / (
  df$OBC + df$NOC
)

# SAC
df$SAC <- (
  df$correct +
    (.5 * df$misformed)
) / (
  df$OBC + df$NOC
)



###############################
# STEP 4. CEFR NUMERIC ENCODING
###############################

df$CEFR_num <- as.numeric(
  factor(
    df$CEFR,
    levels=c(
      "A2",
      "B1",
      "B2",
      "C1",
      "C2"
    )
  )
)


####################################
# STEP 5. CREATE TOKEN-LEVEL DATASETS
####################################

#############################
# SOC TOKEN DATA
#############################

expand_soc <- function(
    cefr,
    morph,
    correct,
    misformed,
    no_suppliance,
    binary,
    semantic,
    formal,
    CI
){
  
  scores <- c(
    rep(1, correct),
    rep(.5, misformed),
    rep(0, no_suppliance)
  )
  
  data.frame(
    CEFR = cefr,
    morpheme = morph,
    score = scores,
    binary = binary,
    semantic = semantic,
    formal = formal,
    CI = CI
  )
  
}


soc_token <- do.call(
  rbind,
  mapply(
    expand_soc,
    df$CEFR,
    df$morpheme,
    df$correct,
    df$misformed,
    df$no_suppliance,
    df$binary,
    df$semantic,
    df$formal,
    df$CI,
    SIMPLIFY=FALSE
  )
)



###################################
# TLU TOKEN DATA
###################################

expand_tlu <- function(
    cefr,
    morph,
    correct,
    misformed,
    no_suppliance,
    NOC,
    binary,
    semantic,
    formal,
    CI
){
  
  scores <- c(
    rep(1, correct),
    rep(0, misformed + no_suppliance + NOC)
  )
  
  data.frame(
    CEFR = cefr,
    morpheme = morph,
    score = scores,
    binary = binary,
    semantic = semantic,
    formal = formal,
    CI = CI
  )
  
}


tlu_token <- do.call(
  rbind,
  mapply(
    expand_tlu,
    df$CEFR,
    df$morpheme,
    df$correct,
    df$misformed,
    df$no_suppliance,
    df$NOC,
    df$binary,
    df$semantic,
    df$formal,
    df$CI,
    SIMPLIFY=FALSE
  )
)



###################################
# SAC TOKEN DATA
###################################

expand_sac <- function(
    cefr,
    morph,
    correct,
    misformed,
    no_suppliance,
    NOC,
    binary,
    semantic,
    formal,
    CI
){
  
  scores <- c(
    rep(1, correct),
    rep(.5, misformed),
    rep(0, no_suppliance + NOC)
  )
  
  data.frame(
    CEFR = cefr,
    morpheme = morph,
    score = scores,
    binary = binary,
    semantic = semantic,
    formal = formal,
    CI = CI
  )
  
}


sac_token <- do.call(
  rbind,
  mapply(
    expand_sac,
    df$CEFR,
    df$morpheme,
    df$correct,
    df$misformed,
    df$no_suppliance,
    df$NOC,
    df$binary,
    df$semantic,
    df$formal,
    df$CI,
    SIMPLIFY=FALSE
  )
)



####################################
# STEP 6. DESCRIPTIVE ACCURACY RANKS
####################################

###########################
# SOC
###########################

soc_ranks <- df %>%
  group_by(CEFR,morpheme) %>%
  summarise(
    Mean_SOC=mean(SOC),
    .groups="drop"
  ) %>%
  arrange(
    CEFR,
    desc(Mean_SOC)
  )

print(soc_ranks,n=39)



###################################
# TLU
###################################

tlu_ranks <- df %>%
  group_by(CEFR,morpheme) %>%
  summarise(
    Mean_TLU=mean(TLU),
    .groups="drop"
  ) %>%
  arrange(
    CEFR,
    desc(Mean_TLU)
  )

print(tlu_ranks,n=39)



###################################
# SAC
###################################

sac_ranks <- df %>%
  group_by(CEFR,morpheme) %>%
  summarise(
    Mean_SAC=mean(SAC),
    .groups="drop"
  ) %>%
  arrange(
    CEFR,
    desc(Mean_SAC)
  )

print(sac_ranks,n=39)



######################################
# STEP 7. MORPHEME ORDER CORRELATIONS
######################################


df$morpheme <- factor(
  df$morpheme,
  levels=c(
    "a",
    "an",
    "the",
    "plural_s",
    "poss_s",
    "irr_past",
    "past_ed",
    "3rd_s"
  )
)


###################################
# MORPHEME-LEVEL CONGRUENCY
# CORRELATIONS
###################################

###################################
# SOC MORPHEME ORDER
###################################

rank_soc <- df %>%
  group_by(morpheme) %>%
  summarise(
    mean_SOC = mean(SOC),
    mean_CI = mean(CI),
    mean_binary = mean(binary),
    mean_semantic = mean(semantic),
    mean_formal = mean(formal),
    .groups="drop"
  )

cor.test(
  rank_soc$mean_SOC,
  rank_soc$mean_CI,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_soc$mean_SOC,
  rank_soc$mean_binary,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_soc$mean_SOC,
  rank_soc$mean_semantic,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_soc$mean_SOC,
  rank_soc$mean_formal,
  method="spearman",
  exact=FALSE
)



###################################
# TLU MORPHEME ORDER
###################################

rank_tlu <- df %>%
  group_by(morpheme) %>%
  summarise(
    mean_TLU = mean(TLU),
    mean_CI = mean(CI),
    mean_binary = mean(binary),
    mean_semantic = mean(semantic),
    mean_formal = mean(formal),
    .groups="drop"
  )

cor.test(
  rank_tlu$mean_TLU,
  rank_tlu$mean_CI,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_tlu$mean_TLU,
  rank_tlu$mean_binary,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_tlu$mean_TLU,
  rank_tlu$mean_semantic,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_tlu$mean_TLU,
  rank_tlu$mean_formal,
  method="spearman",
  exact=FALSE
)



###################################
# SAC MORPHEME ORDER
###################################

rank_sac <- df %>%
  group_by(morpheme) %>%
  summarise(
    mean_SAC = mean(SAC),
    mean_CI = mean(CI),
    mean_binary = mean(binary),
    mean_semantic = mean(semantic),
    mean_formal = mean(formal),
    .groups="drop"
  )

cor.test(
  rank_sac$mean_SAC,
  rank_sac$mean_CI,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_sac$mean_SAC,
  rank_sac$mean_binary,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_sac$mean_SAC,
  rank_sac$mean_semantic,
  method="spearman",
  exact=FALSE
)

cor.test(
  rank_sac$mean_SAC,
  rank_sac$mean_formal,
  method="spearman",
  exact=FALSE
)

####################################
# STORE CORRELATION RESULTS
####################################

corr_results <- list(
  
  SOC_binary = cor.test(
    rank_soc$mean_SOC,
    rank_soc$mean_binary,
    method="spearman",
    exact=FALSE
  ),
  
  SOC_semantic = cor.test(
    rank_soc$mean_SOC,
    rank_soc$mean_semantic,
    method="spearman",
    exact=FALSE
  ),
  
  SOC_formal = cor.test(
    rank_soc$mean_SOC,
    rank_soc$mean_formal,
    method="spearman",
    exact=FALSE
  ),
  
  SOC_CI = cor.test(
    rank_soc$mean_SOC,
    rank_soc$mean_CI,
    method="spearman",
    exact=FALSE
  ),
  
  TLU_binary = cor.test(
    rank_tlu$mean_TLU,
    rank_tlu$mean_binary,
    method="spearman",
    exact=FALSE
  ),
  
  TLU_semantic = cor.test(
    rank_tlu$mean_TLU,
    rank_tlu$mean_semantic,
    method="spearman",
    exact=FALSE
  ),
  
  TLU_formal = cor.test(
    rank_tlu$mean_TLU,
    rank_tlu$mean_formal,
    method="spearman",
    exact=FALSE
  ),
  
  TLU_CI = cor.test(
    rank_tlu$mean_TLU,
    rank_tlu$mean_CI,
    method="spearman",
    exact=FALSE
  ),
  
  SAC_binary = cor.test(
    rank_sac$mean_SAC,
    rank_sac$mean_binary,
    method="spearman",
    exact=FALSE
  ),
  
  SAC_semantic = cor.test(
    rank_sac$mean_SAC,
    rank_sac$mean_semantic,
    method="spearman",
    exact=FALSE
  ),
  
  SAC_formal = cor.test(
    rank_sac$mean_SAC,
    rank_sac$mean_formal,
    method="spearman",
    exact=FALSE
  ),
  
  SAC_CI = cor.test(
    rank_sac$mean_SAC,
    rank_sac$mean_CI,
    method="spearman",
    exact=FALSE
  )
  
)


########################################################
# STEP 9. BOOTSTRAP MORPHEME ACCURACY DISTRIBUTIONS
########################################################

bootstrap_morpheme_means <- function(
    data,
    R=5000
){
  
  results <- list()
  
  for(level in unique(data$CEFR)){
    
    d_level <- subset(
      data,
      CEFR==level
    )
    
    morphemes <- unique(
      d_level$morpheme
    )
    
    morph_boot <- list()
    
    
    ###################################
    # BOOTSTRAP EACH MORPHEME
    ###################################
    
    for(m in morphemes){
      
      d_morph <- subset(
        d_level,
        morpheme==m
      )
      
      n <- nrow(d_morph)
      
      boot_means <- replicate(
        R,
        mean(
          sample(
            d_morph$score,
            size=n,
            replace=TRUE
          )
        )
      )
      
      morph_boot[[m]] <- boot_means
      
    }
    
    results[[level]] <- morph_boot
    
  }
  
  results
  
}



#######################################
# GENERATE BOOTSTRAP DISTRIBUTIONS
#######################################

soc_boot <- bootstrap_morpheme_means(
  soc_token,
  R=5000
)

tlu_boot <- bootstrap_morpheme_means(
  tlu_token,
  R=5000
)

sac_boot <- bootstrap_morpheme_means(
  sac_token,
  R=5000
)



###############################################
# STEP 10.  BOOTSTRAP-BASED ACCURACY CLUSTERS
###############################################

build_clusters <- function(
    boot_list,
    data,
    measure_name
){
  
  all_out <- list()
  
  
  ###################################
  # LOOP THROUGH CEFR LEVELS
  ###################################
  
  for(level in names(boot_list)){
    
    morphs <- names(
      boot_list[[level]]
    )
    
    
    ###################################
    # MEAN ACCURACIES
    ###################################
    
    means <- sapply(
      morphs,
      function(x)
        mean(
          boot_list[[level]][[x]]
        )
    )
    
    means <- sort(
      means,
      decreasing=TRUE
    )
    
    ordered_morphs <- names(means)
    
    
    ###################################
    # CLUSTERING
    ###################################
    
    clusters <- list()
    
    current_cluster <- c(
      ordered_morphs[1]
    )
    
    
    ###################################
    # BUILD CLUSTERS
    ###################################
    
    for(i in 2:length(ordered_morphs)){
      
      current_morph <- ordered_morphs[i]
      
      same_cluster <- TRUE
      
      
      ###################################
      # COMPARE AGAINST ALL MEMBERS
      ###################################
      
      for(existing_morph in current_cluster){
        
        diff_dist <- (
          boot_list[[level]][[existing_morph]] -
            boot_list[[level]][[current_morph]]
        )
        
        ci <- quantile(
          diff_dist,
          probs=c(.025,.975)
        )
        
        
        ####################################################
        # SIGNIFICANT DIFFERENCE IF THE 95% CI EXCLUDES ZERO
        ####################################################
        
        if(ci[1] > 0 ||
           ci[2] < 0){
          
          same_cluster <- FALSE
          
        }
        
      }
      
      
      ###################################
      # ASSIGN CLUSTER
      ###################################
      
      if(same_cluster){
        
        current_cluster <- c(
          current_cluster,
          current_morph
        )
        
      } else {
        
        clusters[[length(clusters)+1]] <-
          current_cluster
        
        current_cluster <- c(
          current_morph
        )
        
      }
      
    }
    
    
    ###################################
    # ADD FINAL CLUSTER
    ###################################
    
    clusters[[length(clusters)+1]] <-
      current_cluster
    
    
    ###################################
    # BUILD OUTPUT TABLE
    ###################################
    
    out <- data.frame()
    
    for(i in seq_along(clusters)){
      
      cluster_members <- clusters[[i]]
      
      for(m in cluster_members){
        
        out <- rbind(
          out,
          data.frame(
            CEFR=level,
            Morpheme=m,
            Accuracy=means[m],
            Cluster=i,
            Cluster_Members=paste(
              cluster_members,
              collapse=" = "
            )
          )
        )
        
      }
      
    }
    
    all_out[[level]] <- out
    
  }
  
  
  ###################################
  # FINAL OUTPUT
  ###################################
  
  final_out <- do.call(
    rbind,
    all_out
  )
  
  write.csv(
    final_out,
    paste0(
      "output/version1_results/",
      measure_name,
      "_accuracy_clusters.csv"
    ),
    row.names=FALSE
  )
  
  final_out
  
}



########################################################
# BUILD SOC CLUSTERS
########################################################

soc_clusters <- build_clusters(
  soc_boot,
  soc_token,
  "soc"
)



########################################################
# BUILD TLU CLUSTERS
########################################################

tlu_clusters <- build_clusters(
  tlu_boot,
  tlu_token,
  "tlu"
)



########################################################
# BUILD SAC CLUSTERS
########################################################

sac_clusters <- build_clusters(
  sac_boot,
  sac_token,
  "sac"
)



########################################################
# DISPLAY RESULTS
########################################################

print(soc_clusters)

print(tlu_clusters)

print(sac_clusters)


##################################
# SAVE HUMAN-READABLE CLUSTERS
##################################

sink(
  "output/version1_results/accuracy_clusters.txt"
)

cat("\n============================\n")
cat("SOC CLUSTERS\n")
cat("============================\n\n")

print(soc_clusters)

cat("\n============================\n")
cat("TLU CLUSTERS\n")
cat("============================\n\n")

print(tlu_clusters)

cat("\n============================\n")
cat("SAC CLUSTERS\n")
cat("============================\n\n")

print(sac_clusters)

sink()



########################################################
#GLOBAL ACCURACY CLUSTERS
########################################################

build_global_clusters <- function(
    token_data,
    measure_name,
    R=5000
){
  
  ###################################
  # BOOTSTRAP MORPHEME MEANS
  ###################################
  
  morphs <- unique(
    token_data$morpheme
  )
  
  boot_means <- list()
  
  for(m in morphs){
    
    scores <- token_data$score[
      token_data$morpheme==m
    ]
    
    boot_means[[m]] <- replicate(
      R,
      mean(
        sample(
          scores,
          replace=TRUE
        )
      )
    )
    
  }
  
  
  ###################################
  # ORDER MORPHEMES
  ###################################
  
  means <- sapply(
    boot_means,
    mean
  )
  
  means <- sort(
    means,
    decreasing=TRUE
  )
  
  ordered_morphs <- names(means)
  
  
  ###################################
  # ASSIGN CLUSTER
  ###################################
  
  clusters <- list()
  
  current_cluster <- c(
    ordered_morphs[1]
  )
  
  
  for(i in 2:length(ordered_morphs)){
    
    current_morph <- ordered_morphs[i]
    
    same_cluster <- TRUE
    
    
    ###################################
    # COMPARE AGAINST EXISTING MEMBERS
    ###################################
    
    for(existing_morph in current_cluster){
      
      diff_dist <- (
        boot_means[[existing_morph]] -
          boot_means[[current_morph]]
      )
      
      ci <- quantile(
        diff_dist,
        probs=c(.025,.975)
      )
      
      
      ###################################
      # IF ZERO NOT INSIDE CI
      ###################################
      
      if(ci[1] > 0 ||
         ci[2] < 0){
        
        same_cluster <- FALSE
        
      }
      
    }
    
    
    ###################################
    # ASSIGN CLUSTER
    ###################################
    
    if(same_cluster){
      
      current_cluster <- c(
        current_cluster,
        current_morph
      )
      
    } else {
      
      clusters[[length(clusters)+1]] <-
        current_cluster
      
      current_cluster <- c(
        current_morph
      )
      
    }
    
  }
  
  
  ###################################
  # FINAL CLUSTER
  ###################################
  
  clusters[[length(clusters)+1]] <-
    current_cluster
  
  
  ###################################
  # BUILD OUTPUT TABLE
  ###################################
  
  out <- data.frame()
  
  for(i in seq_along(clusters)){
    
    cluster_members <- clusters[[i]]
    
    for(m in cluster_members){
      
      out <- rbind(
        out,
        data.frame(
          Morpheme=m,
          Accuracy=means[m],
          Cluster=i,
          Cluster_Members=paste(
            cluster_members,
            collapse=" = "
          )
        )
      )
      
    }
    
  }
  
  
  ###################################
  # SAVE OUTPUT
  ###################################
  
  write.csv(
    out,
    paste0(
      "output/version1_results/global_",
      measure_name,
      "_order.csv"
    ),
    row.names=FALSE
  )
  
  out
  
}



########################################################
# BUILD GLOBAL SOC ORDER
########################################################

global_soc <- build_global_clusters(
  soc_token,
  "soc"
)



########################################################
# BUILD GLOBAL TLU ORDER
########################################################

global_tlu <- build_global_clusters(
  tlu_token,
  "tlu"
)



########################################################
# BUILD GLOBAL SAC ORDER
########################################################

global_sac <- build_global_clusters(
  sac_token,
  "sac"
)



########################################################
# DISPLAY RESULTS
########################################################

print(global_soc)

print(global_tlu)

print(global_sac)


#######################################################
# STEP 11. REGRESSION MODELS
#######################################################

########################
# SOC MODELS
########################

m1_soc <- lm(
  SOC ~ binary + CEFR_num,
  data=df
)

m2_soc <- lm(
  SOC ~ semantic + CEFR_num,
  data=df
)

m3_soc <- lm(
  SOC ~ formal + CEFR_num,
  data=df
)

m4_soc <- lm(
  SOC ~ CI + CEFR_num,
  data=df
)

m5_soc <- lm(
  SOC ~ semantic*formal + CEFR_num,
  data=df
)



####################################
# TLU MODELS
# Grouped quasibinomial GLMs
####################################

df$tlu_failure <- (
  df$misformed +
    df$no_suppliance +
    df$NOC
)

df$tlu_response <- cbind(
  df$correct,
  df$tlu_failure
)

m1_tlu <- glm(
  tlu_response ~ binary + CEFR_num,
  family=quasibinomial,
  data=df
)

m2_tlu <- glm(
  tlu_response ~ semantic + CEFR_num,
  family=quasibinomial,
  data=df
)

m3_tlu <- glm(
  tlu_response ~ formal + CEFR_num,
  family=quasibinomial,
  data=df
)

m4_tlu <- glm(
  tlu_response ~ CI + CEFR_num,
  family=quasibinomial,
  data=df
)

m5_tlu <- glm(
  tlu_response ~ semantic*formal + CEFR_num,
  family=quasibinomial,
  data=df
)


###############################
# SAC MODELS
###############################

m1_sac <- lm(
  SAC ~ binary + CEFR_num,
  data=df
)

m2_sac <- lm(
  SAC ~ semantic + CEFR_num,
  data=df
)

m3_sac <- lm(
  SAC ~ formal + CEFR_num,
  data=df
)

m4_sac <- lm(
  SAC ~ CI + CEFR_num,
  data=df
)

m5_sac <- lm(
  SAC ~ semantic*formal + CEFR_num,
  data=df
)



#######################################################
# STEP 12. MODEL COMPARISON
# + DIAGNOSTICS
#######################################################

##########################
# SOC
##########################

AIC(
  m1_soc,
  m2_soc,
  m3_soc,
  m4_soc,
  m5_soc
)

soc_r2 <- sapply(
  list(
    m1_soc,
    m2_soc,
    m3_soc,
    m4_soc,
    m5_soc
  ),
  function(x)
    summary(x)$adj.r.squared
)

print(soc_r2)



#############################
# SOC DIAGNOSTICS
#############################

par(mfrow=c(2,2))
plot(m4_soc)



##############################
# TLU
##############################

summary(m1_tlu)

summary(m2_tlu)

summary(m3_tlu)

summary(m4_tlu)

summary(m5_tlu)

########################################################
# TLU OVERDISPERSION
########################################################

summary(m4_tlu)$dispersion



########################################################
# SAC
########################################################

AIC(
  m1_sac,
  m2_sac,
  m3_sac,
  m4_sac,
  m5_sac
)

sac_r2 <- sapply(
  list(
    m1_sac,
    m2_sac,
    m3_sac,
    m4_sac,
    m5_sac
  ),
  function(x)
    summary(x)$adj.r.squared
)

print(sac_r2)



########################################################
# SAC DIAGNOSTICS
########################################################

par(mfrow=c(2,2))
plot(m4_sac)



########################################
# STEP 13. BOOTSTRAP REGRESSION ANALYSES
########################################

boot_model_lm <- function(
    formula,
    data,
    indices
){
  
  d <- data[indices,]
  
  agg <- d %>%
    group_by(
      CEFR,
      morpheme,
      binary,
      semantic,
      formal,
      CI
    ) %>%
    summarise(
      score=mean(score),
      .groups="drop"
    )
  
  agg$CEFR_num <- as.numeric(
    factor(
      agg$CEFR,
      levels=c(
        "A2",
        "B1",
        "B2",
        "C1",
        "C2"
      )
    )
  )
  
  model <- lm(
    formula,
    data=agg
  )
  
  coef(model)
  
}



boot_model_glm <- function(
    formula,
    data,
    indices
){
  
  d <- data[indices,]
  
  agg <- d %>%
    group_by(
      CEFR,
      morpheme,
      binary,
      semantic,
      formal,
      CI
    ) %>%
    summarise(
      correct=sum(correct),
      tlu_failure=sum(
        misformed +
          no_suppliance +
          NOC
      ),
      .groups="drop"
    )
  
  agg$CEFR_num <- as.numeric(
    factor(
      agg$CEFR,
      levels=c(
        "A2",
        "B1",
        "B2",
        "C1",
        "C2"
      )
    )
  )
  
  model <- glm(
    formula,
    family=quasibinomial,
    data=agg
  )
  
  coef(model)
  
}



set.seed(123)

########################################################
# GENERIC BOOTSTRAP WRAPPERS
########################################################

run_boot_lm <- function(
    token_data,
    formula,
    R=5000
){
  
  boot(
    token_data,
    statistic=function(data,indices)
      boot_model_lm(
        formula,
        data,
        indices
      ),
    R=R
  )
  
}



run_boot_glm <- function(
    raw_data,
    formula,
    R=5000
){
  
  boot(
    raw_data,
    statistic=function(data,indices)
      boot_model_glm(
        formula,
        data,
        indices
      ),
    R=R
  )
  
}

########################################################
# SOC BOOTSTRAPS
########################################################

boot_soc_binary <- run_boot_lm(
  soc_token,
  score ~ binary + CEFR_num
)

boot_soc_semantic <- run_boot_lm(
  soc_token,
  score ~ semantic + CEFR_num
)

boot_soc_formal <- run_boot_lm(
  soc_token,
  score ~ formal + CEFR_num
)

boot_soc_CI <- run_boot_lm(
  soc_token,
  score ~ CI + CEFR_num
)



########################################################
# TLU BOOTSTRAPS
########################################################

boot_tlu_binary <- run_boot_glm(
  df,
  cbind(correct,tlu_failure) ~
    binary + CEFR_num
)

boot_tlu_semantic <- run_boot_glm(
  df,
  cbind(correct,tlu_failure) ~
    semantic + CEFR_num
)

boot_tlu_formal <- run_boot_glm(
  df,
  cbind(correct,tlu_failure) ~
    formal + CEFR_num
)

boot_tlu_CI <- run_boot_glm(
  df,
  cbind(correct,tlu_failure) ~
    CI + CEFR_num
)



########################################################
# SAC BOOTSTRAPS
########################################################

boot_sac_binary <- run_boot_lm(
  sac_token,
  score ~ binary + CEFR_num
)

boot_sac_semantic <- run_boot_lm(
  sac_token,
  score ~ semantic + CEFR_num
)

boot_sac_formal <- run_boot_lm(
  sac_token,
  score ~ formal + CEFR_num
)

boot_sac_CI <- run_boot_lm(
  sac_token,
  score ~ CI + CEFR_num
)


########################################################
# STEP 14. CONFIDENCE INTERVALS
########################################################

#################################
# SOC
#################################

boot.ci(
  boot_soc_binary,
  type="bca",
  index=2
)

boot.ci(
  boot_soc_semantic,
  type="bca",
  index=2
)

boot.ci(
  boot_soc_formal,
  type="bca",
  index=2
)

boot.ci(
  boot_soc_CI,
  type="bca",
  index=2
)



#################################
# TLU
#################################

boot.ci(
  boot_tlu_binary,
  type="bca",
  index=2
)

boot.ci(
  boot_tlu_semantic,
  type="bca",
  index=2
)

boot.ci(
  boot_tlu_formal,
  type="bca",
  index=2
)

boot.ci(
  boot_tlu_CI,
  type="bca",
  index=2
)



########################################################
# SAC
########################################################

boot.ci(
  boot_sac_binary,
  type="bca",
  index=2
)

boot.ci(
  boot_sac_semantic,
  type="bca",
  index=2
)

boot.ci(
  boot_sac_formal,
  type="bca",
  index=2
)

boot.ci(
  boot_sac_CI,
  type="bca",
  index=2
)


########################################################
# SAVE ALL VERSION 1 RESULTS
########################################################

if(!dir.exists("output")){
  dir.create("output")
}

if(!dir.exists("output/version1_results")){
  dir.create("output/version1_results")
}



###############################
# SAVE MODEL SUMMARIES
###############################

save_lm_summary <- function(model, filename){
  
  s <- summary(model)
  
  coef_table <- as.data.frame(
    s$coefficients
  )
  
  coef_table$Predictor <- rownames(coef_table)
  
  coef_table$Adj_R2 <- s$adj.r.squared
  coef_table$AIC <- AIC(model)
  
  write.csv(
    coef_table,
    filename,
    row.names=FALSE
  )
  
}


save_glm_summary <- function(model, filename){
  
  s <- summary(model)
  
  coef_table <- as.data.frame(
    s$coefficients
  )
  
  coef_table$Predictor <- rownames(coef_table)
  
  coef_table$Dispersion <- summary(model)$dispersion
  
  coef_table$Odds_Ratio <- exp(
    coef_table$Estimate
  )
  
  write.csv(
    coef_table,
    filename,
    row.names=FALSE
  )
  
}



########################################################
# SAVE ALL SOC MODELS
########################################################

save_lm_summary(
  m1_soc,
  "output/version1_results/m1_soc_binary.csv"
)

save_lm_summary(
  m2_soc,
  "output/version1_results/m2_soc_semantic.csv"
)

save_lm_summary(
  m3_soc,
  "output/version1_results/m3_soc_formal.csv"
)

save_lm_summary(
  m4_soc,
  "output/version1_results/m4_soc_CI.csv"
)

save_lm_summary(
  m5_soc,
  "output/version1_results/m5_soc_interaction.csv"
)



########################################################
# SAVE ALL TLU MODELS
########################################################

save_glm_summary(
  m1_tlu,
  "output/version1_results/m1_tlu_binary.csv"
)

save_glm_summary(
  m2_tlu,
  "output/version1_results/m2_tlu_semantic.csv"
)

save_glm_summary(
  m3_tlu,
  "output/version1_results/m3_tlu_formal.csv"
)

save_glm_summary(
  m4_tlu,
  "output/version1_results/m4_tlu_CI.csv"
)

save_glm_summary(
  m5_tlu,
  "output/version1_results/m5_tlu_interaction.csv"
)



########################################################
# SAVE ALL SAC MODELS
########################################################

save_lm_summary(
  m1_sac,
  "output/version1_results/m1_sac_binary.csv"
)

save_lm_summary(
  m2_sac,
  "output/version1_results/m2_sac_semantic.csv"
)

save_lm_summary(
  m3_sac,
  "output/version1_results/m3_sac_formal.csv"
)

save_lm_summary(
  m4_sac,
  "output/version1_results/m4_sac_CI.csv"
)

save_lm_summary(
  m5_sac,
  "output/version1_results/m5_sac_interaction.csv"
)



########################################################
# SAVE SOC BOOTSTRAPS
########################################################

write.csv(
  boot_soc_binary$t,
  "output/version1_results/boot_soc_binary.csv",
  row.names=FALSE
)

write.csv(
  boot_soc_semantic$t,
  "output/version1_results/boot_soc_semantic.csv",
  row.names=FALSE
)

write.csv(
  boot_soc_formal$t,
  "output/version1_results/boot_soc_formal.csv",
  row.names=FALSE
)

write.csv(
  boot_soc_CI$t,
  "output/version1_results/boot_soc_CI.csv",
  row.names=FALSE
)



########################################################
# SAVE TLU BOOTSTRAPS
########################################################

write.csv(
  boot_tlu_binary$t,
  "output/version1_results/boot_tlu_binary.csv",
  row.names=FALSE
)

write.csv(
  boot_tlu_semantic$t,
  "output/version1_results/boot_tlu_semantic.csv",
  row.names=FALSE
)

write.csv(
  boot_tlu_formal$t,
  "output/version1_results/boot_tlu_formal.csv",
  row.names=FALSE
)

write.csv(
  boot_tlu_CI$t,
  "output/version1_results/boot_tlu_CI.csv",
  row.names=FALSE
)



########################################################
# SAVE SAC BOOTSTRAPS
########################################################

write.csv(
  boot_sac_binary$t,
  "output/version1_results/boot_sac_binary.csv",
  row.names=FALSE
)

write.csv(
  boot_sac_semantic$t,
  "output/version1_results/boot_sac_semantic.csv",
  row.names=FALSE
)

write.csv(
  boot_sac_formal$t,
  "output/version1_results/boot_sac_formal.csv",
  row.names=FALSE
)

write.csv(
  boot_sac_CI$t,
  "output/version1_results/boot_sac_CI.csv",
  row.names=FALSE
)


########################################################
# SAVE SUMMARY TABLES
########################################################

morpheme_means <- aggregate(
  cbind(SOC,TLU,SAC) ~ morpheme,
  data=df,
  mean
)

cefr_means <- aggregate(
  cbind(SOC,TLU,SAC) ~ CEFR,
  data=df,
  mean
)

write.csv(
  morpheme_means,
  "output/version1_results/morpheme_means.csv",
  row.names=FALSE
)

write.csv(
  cefr_means,
  "output/version1_results/cefr_means.csv",
  row.names=FALSE
)


########################################################
# SAVE COMPLETE EXCEL SUMMARY
########################################################

all_results <- list(
  
  morpheme_means =
    morpheme_means,
  
  cefr_means =
    cefr_means
  
)

write_xlsx(
  all_results,
  "output/version1_results/version1_summary_results.xlsx"
)

########################################################
# SAVE CORRELATION RESULTS
########################################################

sink(
  "output/version1_results/correlation_results.txt"
)

for(name in names(corr_results)){
  
  cat("\n\n========================\n")
  cat(name)
  cat("\n========================\n\n")
  
  print(corr_results[[name]])
  
}

sink()


########################################################
# SAVE BOOTSTRAP CONFIDENCE INTERVALS
########################################################

sink(
  "output/version1_results/bootstrap_confidence_intervals.txt"
)



#########################################
# SOC
#########################################

cat("\nSOC BINARY BCa CI\n")

print(
  boot.ci(
    boot_soc_binary,
    type="bca",
    index=2
  )
)

cat("\nSOC SEMANTIC BCa CI\n")

print(
  boot.ci(
    boot_soc_semantic,
    type="bca",
    index=2
  )
)

cat("\nSOC FORMAL BCa CI\n")

print(
  boot.ci(
    boot_soc_formal,
    type="bca",
    index=2
  )
)

cat("\nSOC CI BCa CI\n")

print(
  boot.ci(
    boot_soc_CI,
    type="bca",
    index=2
  )
)



########################################
# TLU
########################################

cat("\nTLU BINARY BCa CI\n")

print(
  boot.ci(
    boot_tlu_binary,
    type="bca",
    index=2
  )
)

cat("\nTLU SEMANTIC BCa CI\n")

print(
  boot.ci(
    boot_tlu_semantic,
    type="bca",
    index=2
  )
)

cat("\nTLU FORMAL BCa CI\n")

print(
  boot.ci(
    boot_tlu_formal,
    type="bca",
    index=2
  )
)

cat("\nTLU CI BCa CI\n")

print(
  boot.ci(
    boot_tlu_CI,
    type="bca",
    index=2
  )
)



########################################
# SAC
########################################

cat("\nSAC BINARY BCa CI\n")

print(
  boot.ci(
    boot_sac_binary,
    type="bca",
    index=2
  )
)

cat("\nSAC SEMANTIC BCa CI\n")

print(
  boot.ci(
    boot_sac_semantic,
    type="bca",
    index=2
  )
)

cat("\nSAC FORMAL BCa CI\n")

print(
  boot.ci(
    boot_sac_formal,
    type="bca",
    index=2
  )
)

cat("\nSAC CI BCa CI\n")

print(
  boot.ci(
    boot_sac_CI,
    type="bca",
    index=2
  )
)

sink()



########################################################
# SAVE FULL MODEL SUMMARIES
########################################################

sink(
  "output/version1_results/full_model_summaries.txt"
)

cat("\nSOC MODELS\n")

print(summary(m1_soc))
print(summary(m2_soc))
print(summary(m3_soc))
print(summary(m4_soc))
print(summary(m5_soc))

cat("\nTLU MODELS\n")

print(summary(m1_tlu))
print(summary(m2_tlu))
print(summary(m3_tlu))
print(summary(m4_tlu))
print(summary(m5_tlu))

cat("\nSAC MODELS\n")

print(summary(m1_sac))
print(summary(m2_sac))
print(summary(m3_sac))
print(summary(m4_sac))
print(summary(m5_sac))

sink()


###############################
# DONE
###############################

cat(
  "\n\nAll Version 1 results saved in:\n",
  "output/version1_results/\n\n"
)
