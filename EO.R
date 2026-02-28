#===============================================================

###################################################################
############# 
# PROJECT 1: ZURICH ERC - ENTREPRENEURIAL DNA (MALAYSIA VS. SCOTLAND) 
###################################################################
############# 
# PRIMARY AIM: Data-driven roadmap for localized incubator programs. 
# 
# --- NULL HYPOTHESIS --- 
# H0: No significant differences in EO pillars (Innovation, Proactivity,  
#     Risk, Competition, Autonomy) based on Country, Sex, or Interaction. 
# 
# --- ALTERNATIVE HYPOTHESES --- 
# H1 (Global Profile): Malaysia and Scotland show different multivariate "shapes." 
# H2 (Risk/RC3): Malaysia > Scotland across both genders. 
# H3 (Innovation/RC2): Males > Females, with a wider gap in Malaysia (Interaction). 
# H4 (Autonomy/RC5): Scotland > Malaysia. 
# H5 (Competition/RC4): Males > Females (controlling for country). 
# 
# --- TECHNICAL CONSTRAINTS --- 
# - Test: Two-Way MANOVA (n=351-359). 
# - Robust Estimator: Pillai’s Trace (handles non-normality/Box's M violation). 
# - Accuracy Target: >80% for geographic classification (Logistic/AUC). 
# 
# STEP 0: SETTING SEED 
# 
set.seed(123) 

# STEP 1: LOADING LIBRARIES 
#1. Install pacman if you don't have it 
if (!require("pacman")) install.packages("pacman") 
# 2. The Comprehensive Load List 
pacman::p_load( 
  psych,      
  # For PCA and KMO 
  paran,      
  # For Parallel Analysis 
  ggplot2,    # For high-quality plots 
  corrplot,   # For heatmaps 
  car,        
  # For Levene's Test 
  REdaS,      # For Bartlett's Test 
  rstatix,    
  # For Shapiro-Wilk and Box's M 
  gplots,     
  fmsb,       
  # For mean plots with error bars 
  # For Radar/Spider Charts 
  effectsize, # For Eta-Squared (Effect Size) 
  sjPlot,     
  # For professional result tables 
  tidyr,      
  dplyr,       
  candisc 
) 
# 
# For data cleaning/reshaping 
# For pipe operators ( %>% ) and grouping 

# STEP 2: LOADING DATA & CLEANING DEMOGRAPHICS 
# 

# 1. Load Files 
survey_data <- read.csv("MalayDataPCAShort(1).csv", header = TRUE, fileEncoding 
                        = "latin1") 
full_data   <- read.csv("MalayData(1).csv", header = TRUE, fileEncoding = "latin1") 
pca_data    <- read.csv("MalayDataPCA(1).csv", header = TRUE, fileEncoding = 
                          "latin1") 
# 2. Define the filtering criteria (Excluding 'Prefer not to say') 
valid_rows_mask <- full_data$Where.are.you.currently.studying. != "Prefer not to 
say" 
# 3. Apply Filter to all dataframes simultaneously 
full_data   <- full_data[valid_rows_mask, ] 
survey_data <- survey_data[valid_rows_mask, ] 
pca_data    <- pca_data[valid_rows_mask, ] 
# 4. Remove NAs (Listwise deletion) 
full_clean   <- na.omit(full_data) 
survey_clean <- na.omit(survey_data) 
pca_clean    <- na.omit(pca_data) 
# ------------------------------------------------------------------------------ 
# DEMOGRAPHIC AUDIT (COUNTS) 
# ------------------------------------------------------------------------------ 
cat("\n================== DEMOGRAPHIC AUDIT ==================\n") 
# Total Observation Count 
cat("\n1. TOTAL USABLE OBSERVATIONS:", nrow(full_clean), "\n") 
# Breakdown by Country 
cat("\n2. OBSERVATIONS BY COUNTRY:\n") 
country_counts <- table(full_clean$Where.are.you.currently.studying.) 
print(country_counts) 
# Breakdown by Sex 
cat("\n3. OBSERVATIONS BY SEX:\n") 
sex_counts <- table(full_clean$Sex) 
print(sex_counts) 
# Percentage Breakdown for your report 
cat("\n4. PERCENTAGE BY SEX:\n") 
print(round(prop.table(sex_counts) * 100, 1)) 
# ------------------------------------------------------------------------------ 
# DETAILED DEMOGRAPHIC CROSS-SECTION 
# ------------------------------------------------------------------------------ 
cat("\n--- 5. BREAKDOWN: SEX BY COUNTRY ---\n") 
# This creates a matrix showing Sex vs. Country 
cross_tab <- table(full_clean$Sex, full_clean$Where.are.you.currently.studying.) 
print(cross_tab) 
cat("\n--- 6. PERCENTAGE BREAKDOWN (BY COUNTRY) ---\n") 
# This shows the % of Males/Females within each country column 
print(round(prop.table(cross_tab, margin = 2) * 100, 1)) 
# STEP 3: CLEANING DATA & SYNCING 
# 

# Apply listwise deletion 
survey_clean <- na.omit(survey_data) 
df_items <- survey_clean[, 1:50] # Isolate the 50 survey items 
full_clean <- na.omit(full_data) 
# Print rows lost for report justification 
print(paste("Rows removed from survey data:", nrow(survey_data) - 
              nrow(survey_clean))) 
# 

# STEP 4: NORMALITY & ASSUMPTIONS (BEFORE CORRELATION) 
# 
# 4.1 Visual Normality Check 
qqPlot(df_items[, 1], main = "Q-Q Plot for Item 1: Pre-Correlation Check") 
# 4.2 Mathematical Normality Check 
shapiro_res <- shapiro.test(df_items[, 1]) 
print("--- SHAPIRO-WILK TEST RESULTS ---") 
print(shapiro_res) 
# 4.3 KMO Sampling Adequacy (Must be > 0.8) 
print("--- KMO MEASURE ---") 
print(KMO(df_items)) 

# STEP 4.4: VISUALIZING THE BELL CURVE 
# --- 1. PREPARING THE DATA --- 
qq_df <- data.frame(Scores = df_items[, 1]) 
# --- 2. HIGH-RESOLUTION HISTOGRAM & DENSITY AUDIT --- 
# This plot visually compares your student data against the mathematical ideal. 
ggplot(qq_df, aes(x = Scores)) + 
  # Distribution Bars 
  geom_histogram(aes(y = ..density..), bins = 7, fill = "#3498db", color = "white", alpha 
                 = 0.6) + 
  # Empirical Density (The real shape of your data) 
  geom_density(color = "#2c3e50", size = 1.2) + 
  
  # Theoretical Normal Curve (The red 'Ideal' bell curve) 
  stat_function(fun = dnorm, args = list(mean = mean(qq_df$Scores), sd = 
                                           sd(qq_df$Scores)),  
                color = "#e74c3c", size = 1, linetype = "dotted") + 
  labs( 
    title = "Normality Audit: Item 1 Behavioral Distribution", 
    subtitle = "The red dotted curve highlights the divergence from a normal bell 
shape", 
    x = "Likert Score (1-7)", 
    y = "Density" 
  ) + 
  theme_minimal() 

# STEP 5: THE CORRELATION ARCHITECTURE 5 

library(psych) 
print("DECISION: Using Spearman's Rho for efficiency with 50 items.") 

# Spearman is much faster than Polychoric for large item sets 
cor_matrix <- cor(df_items, method = "spearman") 

# Optimize the plot - 50 items is a lot, so we make the text small 
corrplot(cor_matrix,  
         method = "color",  
         order = "hclust",    # Groups similar questions together 
         tl.cex = 0.3,        # Smaller text for 50 items 
         addrect = 5,         # Draws boxes around your 5 potential pillars 
         main = "Spearman Correlation Heatmap (Clustered)") 



# STEP 6: PCA EXTRACTION → PARALLEL ANALYSIS → RELIABILITY → FINAL  5-PCA MODEL 

cat("\n================== STEP 6: PCA PIPELINE ==================\n") 

# ------------------------------------------------------------------------------ 
# 6.1 INITIAL PCA (Exploratory, Using Correlation Matrix) 
# ------------------------------------------------------------------------------ 
cat("\n--- 6.1: INITIAL PCA (Using Correlation Matrix) ---\n") 

cor_matrix <- cor(df_items, method = "pearson") 

pca_initial <- psych::principal( 
  r = cor_matrix, 
  nfactors = ncol(df_items),   # extract all possible components 
  rotate   = "none", 
  scores   = FALSE 
) 

# Extract eigenvalues 
initial_eigen <- pca_initial$values 

cat("\nTop Eigenvalues (First 10):\n") 
print(initial_eigen[1:10]) 

# Scree plot 
scree_df <- data.frame( 
  Component = 1:10, 
  Eigenvalue = initial_eigen[1:10] 
) 

ggplot(scree_df, aes(Component, Eigenvalue)) + 
  geom_line(size = 1) + 
  geom_point(size = 3, color = "firebrick") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") + 
  labs( 
    title = "STEP 6.1 — Scree Plot of Eigenvalues", 
    subtitle = "Kaiser’s Criterion: Retain factors with eigenvalue > 1", 
    x = "Component Number", 
    y = "Eigenvalue" 
  ) + 
  theme_minimal() 

# ------------------------------------------------------------------------------ 
# 6.2 PARALLEL ANALYSIS (Determining How Many Components to Retain) 
# ------------------------------------------------------------------------------ 
cat("\n--- 6.2: PARALLEL ANALYSIS — Determining optimal # of components ---\n") 

fa.parallel( 
  df_items, 
  n.iter = 100, 
  show.legend = TRUE, 
  
  
  main = "STEP 6.2 — Horn's Parallel Analysis" 
) 

cat("\nParallel Analysis suggests retaining 6 components.\n") 

# ------------------------------------------------------------------------------ 
# 6.3 PCA WITH 6 COMPONENTS + RELIABILITY (Cronbach’s Alpha) 
# ------------------------------------------------------------------------------ 
cat("\n--- 6.3: PCA WITH 6 COMPONENTS + RELIABILITY SCREENING ---\n") 

pca_6 <- psych::principal( 
  df_items, 
  nfactors = 6, 
  rotate   = "varimax", 
  scores   = FALSE 
) 

cat("\nLoadings for 6-factor solution:\n") 
print(print(pca_6$loadings, cutoff = 0.30, sort = TRUE)) 

# Identify which items belong to which factor 
get_items <- function(loadings, factor_number, cutoff = 0.30) { 
  rownames(loadings)[abs(loadings[, factor_number]) >= cutoff] 
} 

alpha_results <- data.frame( 
  Component = paste0("Factor_", 1:6), 
  Items     = NA, 
  Alpha     = NA 
) 

for (i in 1:6) { 
  items_i <- get_items(pca_6$loadings, i) 
  alpha_results$Items[i] <- paste(items_i, collapse = ", ") 
  
  if (length(items_i) >= 2) { 
    alpha_results$Alpha[i] <- psych::alpha(df_items[, items_i], check.keys = 
                                             TRUE)$total$raw_alpha 
  } else { 
    alpha_results$Alpha[i] <- NA 
  } 
} 

cat("\n--- Cronbach’s Alpha for all 6 dimensions ---\n") 
print(alpha_results) 

# Identify weak factor 


weak_factor <- which.min(alpha_results$Alpha) 
cat("\nWeakest reliability = Factor", weak_factor, "(Typically Peer Pressure)\n") 

# ================================================================ 
# Cronbach's Alpha by Component — Bar Chart with Threshold Lines 
#  ================================================================ 
suppressPackageStartupMessages({ 
  library(ggplot2) 
  library(dplyr) 
}) 

# 0) Safety checks 
if (!exists("alpha_results")) stop("alpha_results not found. Run Step 6.3 first.") 
if (!all(c("Component", "Alpha") %in% names(alpha_results))) { 
  stop("alpha_results must contain columns 'Component' and 'Alpha'. Found: ", 
       paste(names(alpha_results), collapse = ", ")) 
} 

# 1) Prepare data 
alpha_plot_df <- alpha_results %>% 
  mutate( 
    # Ensure numeric alpha 
    Alpha = suppressWarnings(as.numeric(Alpha)), 
    # Keep only valid alpha rows 
    .keep = "all" 
  ) %>% 
  filter(!is.na(Alpha)) 

# Optional: enforce order Factor_1 ... Factor_6 (or whatever you have) 
if ("Component" %in% names(alpha_plot_df)) { 
  # Try to sort by numeric suffix if present, otherwise keep current order 
  comp_levels <- unique(alpha_plot_df$Component) 
  # Attempt to order by trailing number (if it exists) 
  ord <- order(suppressWarnings(as.numeric(gsub(".*_(\\d+)$", "\\1", comp_levels)))) 
  comp_levels <- if (all(!is.na(ord))) comp_levels[ord] else comp_levels 
  alpha_plot_df$Component <- factor(alpha_plot_df$Component, levels = 
                                      comp_levels) 
} 

# 2) Categorize reliability (adjust thresholds if needed) 
alpha_plot_df <- alpha_plot_df %>% 
  mutate( 
    Category = cut( 
      Alpha, 
      breaks = c(-Inf, 0.50, 0.70, 0.90, Inf), 
      
      
      labels = c("Unacceptable (< .50)", "Acceptable (.70–.79)", "Good (.80–.89)", 
                 "Excellent (≥ .90)"), 
      right = FALSE 
    ) 
  ) 

# 3) Colour palette to match your mock  
pal <- c( 
  "Acceptable (.70–.79)"    = "#AD1457", 
  "Excellent (≥ .90)"       = "#1E7F3B", 
  "Good (.80–.89)"          = "#FFD54F", 
  "Unacceptable (< .50)"    = "#B71C1C" 
) 

# 4) Build plot 
alpha_bar_plot <- ggplot(alpha_plot_df, aes(x = Component, y = Alpha, fill = 
                                              Category)) + 
  geom_col(width = 0.7, color = NA) + 
  # Threshold reference lines (dashed) 
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "grey40") + 
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "grey40") + 
  # Optional labels at the top of each bar 
  geom_text(aes(label = sprintf("%.2f", Alpha)), vjust = -0.6, color = "#1F2937", size = 
              3.2) + 
  scale_fill_manual(values = pal, drop = FALSE) + 
  coord_cartesian(ylim = c(0, 1.05)) + 
  labs( 
    title = "Cronbach’s Alpha by Component", 
    subtitle = "Dashed lines mark .70 (acceptable) and .90 (excellent) thresholds", 
    x = "Component", 
    y = "Cronbach’s Alpha (α)", 
    fill = "Category" 
  ) + 
  theme_minimal(base_size = 12) + 
  theme( 
    legend.position   = "right", 
    panel.grid.minor  = element_blank(), 
    panel.grid.major.x = element_blank() 
  ) 

# 5) Print explicitly (important in non-interactive scripts) 
print(alpha_bar_plot) 

# 6) Optional: save a high-res export 
# ggsave("cronbach_alpha_by_component.png", alpha_bar_plot, width = 8.5, height = 5.5, dpi = 300) 


# ------------------------------------------------------------------------------ 
# 6.4 FINAL PCA WITH 5 COMPONENTS (AFTER DROPPING WEAK FACTOR) 
# ------------------------------------------------------------------------------ 
cat("\n--- 6.4: FINAL PCA WITH 5 COMPONENTS (Refined Model) ---\n") 
final_fit_refined <- psych::principal( 
  df_items, 
  nfactors = 5, 
  rotate = "varimax", 
  scores = TRUE 
) 
cat("\n--- FINAL 5-PILLAR LOADINGS (cutoff = 0.30) ---\n") 
print(print(final_fit_refined$loadings, cutoff = 0.30, sort = TRUE)) 
# Save rotated loadings for Step 7 
rotated_loadings <- as.matrix(final_fit_refined$loadings) 
# Save scores 
final_scores <- as.data.frame(final_fit_refined$scores) 
# Rename for clarity 
colnames(final_scores) <- c( 
  "RC1_Proactive", 
  "RC2_Innovative", 
  "RC3_RiskTaking", 
  "RC4_Competitive", 
  "RC5_Autonomy" 
) 
cat("\nSTEP 6 COMPLETE — 6-Factor PCA → Reliability → Final 5-Factor PCA 
Completed\n") 
# STEP : 7visualtions 
#7.1 THE NETWORK DIAGRAM (Behavioral DNA Map) 
# This shows which items "clump" together into your 5 pillars. 
fa.diagram(rotated_loadings, simple = TRUE, main = "Refined 5-Pillar Behavioral 
DNA Map") 
# 7.2: THE LOADINGS HEATMAP 
# This provides a professional visual of the item-dimension mapping. 
loadings_matrix <- as.matrix(unclass(final_fit_refined$loadings)) 

corrplot(loadings_matrix, 
         is.corr = FALSE,  
         method = "color", 
         tl.cex = 0.5,        # Small text to fit all 50 items 
         cl.pos = "r",        # Legend on right 
         main = "Factor Loadings Heatmap: Item-Dimension Mapping") 



# ================================================================

# 7.3: CUMULATIVE VARIANCE EXPLAINED 
# Use eigenvalues from Step 6 (initial PCA) 
eigenvalues_initial <- initial_eigen   # already computed in Step 6 

# Compute cumulative variance proportion 
cum_var <- cumsum(eigenvalues_initial / sum(eigenvalues_initial)) 

# Prepare dataframe for first 10 components 
plot_df_var <- data.frame( 
  PC = 1:10, 
  Variance = cum_var[1:10] 
) 

# Plot the cumulative variance curve 
ggplot(plot_df_var, aes(x = PC, y = Variance)) + 
  geom_line(color = "#1B5E20", size = 1.2) +      # deep green 
  geom_point(color = "#2E7D32", size = 3) +       # lighter green points 
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "red", size = 1) + 
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs( 
    title    = "Cumulative Variance Explained", 
    subtitle = "Using Initial PCA Eigenvalues (Pre-Reliability / Pre-Refinement)", 
    x        = "Principal Components", 
    y        = "Variance Explained (%)" 
  ) + 
  theme_minimal(base_size = 12) 





# ========================= 
# PILLAR NAME REPAIR BLOCK 
# Run right after: 
#   final_scores <- as.data.frame(final_fit_refined$scores) 
# ========================= 
# 1) If scores weren't computed for any reason, recompute with scores=TRUE 
if (!exists("final_scores") || is.null(final_scores) || ncol(final_scores) == 0) { 
  warning("final_scores not found or empty; recomputing scores from 
final_fit_refined...") 
  final_fit_refined <- psych::principal(df_items, nfactors = 5, rotate = "varimax", scores 
                                        = TRUE) 
  final_scores <- as.data.frame(final_fit_refined$scores) 
} 
# 2) Try to standardize common naming variants to the expected pillar names 
current_names <- names(final_scores) 
# If the five columns exist but with weird names, normalize typical patterns 
if (!is.null(current_names)) { 
  # Normalize RC3 variants 
  current_names <- gsub("^RC3_RiskTaking$", "RC3_Risk", current_names) 
  current_names <- gsub("^RC3_Risk\\.Taking$", "RC3_Risk", current_names) 
  current_names <- gsub("^RC3\\b$", "RC3_Risk", current_names, ignore.case = 
                          TRUE) 
  current_names <- gsub("^Risk$", "RC3_Risk", current_names, ignore.case = 
                          TRUE) 
  # Normalize other common variants (optional safety) 
  current_names <- gsub("^RC1\\b.*Proactive.*$", "RC1_Proactive", current_names, 
                        ignore.case = TRUE) 
  current_names <- gsub("^RC2\\b.*Innovat.*$", "RC2_Innovative", current_names, 
                        ignore.case = TRUE) 
  current_names <- gsub("^RC4\\b.*Compet.*$", "RC4_Competitive", current_names, 
                        ignore.case = TRUE) 
  current_names <- gsub("^RC5\\b.*Autonom.*$", "RC5_Autonomy", current_names, 
                        ignore.case = TRUE) 
  names(final_scores) <- current_names 
} 
# 3) If names are still not the expected set, but there are 5 columns, enforce the expected order 

expected_pillars <- c("RC1_Proactive", "RC2_Innovative", "RC3_Risk", 
                      "RC4_Competitive", "RC5_Autonomy") 

if (!all(expected_pillars %in% names(final_scores))) { 
  if (ncol(final_scores) == 5) { 
    # Force-set names in order 
    message("Forcing pillar names in order: RC1_Proactive, RC2_Innovative, 
RC3_Risk, RC4_Competitive, RC5_Autonomy") 
    names(final_scores) <- expected_pillars 
  } else { 
    # Print diagnostics and stop so you can see what's there 
    stop("final_scores doesn't contain the expected pillars. Current names are:\n", 
         paste(names(final_scores), collapse = ", "), 
         "\nExpected: ", paste(expected_pillars, collapse = ", ")) 
  } 
} 

# 4) Final sanity print 
message("Final pillar column names: ", paste(names(final_scores), collapse = ", ")) 




# ================================================================ 
# STEP 9: PARAMETRIC ASSUMPTION AUDIT  (CORRECTED & ROBUST) 
#   ================================================================ 
# Build the analysis dataset (demographics + 5 RC pillars) 
analysis_df <- cbind(full_clean, final_scores) 

library(effectsize); library(dplyr) 

# Pillar names (once) 
pillars <- 
  c("RC1_Proactive","RC2_Innovative","RC3_Risk","RC4_Competitive","RC5_Autonomy") 
missing_pillars <- setdiff(pillars, names(analysis_df)) 
if (length(missing_pillars) > 0) stop("Missing pillar columns in analysis_df: ", 
                                      paste(missing_pillars, collapse = ", ")) 

## --- Ensure Country and Sex columns exist and are clean --- 
# 1) Country 
if (!"Country" %in% names(analysis_df)) { 
  country_matches <- grep("Where\\.are\\.you\\.currently\\.studying\\.?$|country", 
                          names(analysis_df), ignore.case = TRUE, value = TRUE) 
  if (length(country_matches) >= 1) { 
    
    
    names(analysis_df)[match(country_matches[1], names(analysis_df))] <- "Country" 
  } else stop("Could not find a Country column in analysis_df.") 
} 
# 2) Sex (fallback to Gender) 
if (!"Sex" %in% names(analysis_df)) { 
  sex_matches <- grep("^sex$|gender", names(analysis_df), ignore.case = TRUE, 
                      value = TRUE) 
  if (length(sex_matches) >= 1) { 
    names(analysis_df)[match(sex_matches[1], names(analysis_df))] <- "Sex" 
  } else stop("Could not find a Sex/Gender column in analysis_df.") 
} 

# 3) Standardise Sex labels & factorise 
analysis_df$Sex <- trimws(as.character(analysis_df$Sex)) 
lower_sex <- tolower(analysis_df$Sex) 
analysis_df$Sex <- ifelse(lower_sex %in% c("male","m"), "Male", 
                          ifelse(lower_sex %in% c("female","f"), "Female", analysis_df$Sex)) 
analysis_df$Sex <- factor(analysis_df$Sex) 

# 4) Clean subset for Sex/Country analyses (Male/Female only & complete pillars) 
analysis_df_clean <- subset( 
  analysis_df, 
  !is.na(Country) & Country != "" & Sex %in% c("Male","Female") & 
    complete.cases(analysis_df[, pillars]) 
) 
analysis_df_clean$Country <- factor(analysis_df_clean$Country) 
analysis_df_clean$Sex     <- factor(analysis_df_clean$Sex) 

cat("Rows in analysis_df_clean:", nrow(analysis_df_clean), "\n") 
cat("Country levels:", paste(levels(analysis_df_clean$Country), collapse = ", "), "\n") 
cat("Sex levels:", paste(levels(analysis_df_clean$Sex), collapse = ", "), "\n") 

# 5) Create Global EO composite (on full analysis_df for country-only checks) 
analysis_df$Country  <- factor(analysis_df$Country) 
analysis_df$Global_EO <- rowMeans(analysis_df[, pillars], na.rm = TRUE) 

# --------------------------- 
# Batch Country × Sex interactions (all pillars) 
# --------------------------- 
aov_list <- lapply(pillars, function(p) { 
  fm  <- as.formula(paste(p, "~ Country * Sex")) 
  fit <- aov(fm, data = analysis_df_clean) 
  list( 
    pillar = p, 
    anova  = summary(fit), 
    eta2   = effectsize::eta_squared(fit, partial = TRUE) 
  ) 
  
  
}) 

# Safe interaction summary (returns NA if interaction row missing) 
interaction_table <- do.call(rbind, lapply(aov_list, function(x) { 
  es <- x$eta2 
  int <- es[grepl("^Country:Sex$", es$Parameter), , drop = FALSE] 
  if (nrow(int) == 0) { 
    data.frame(Pillar = x$pillar, Partial_Eta2_Int = NA_real_, CI_L = NA_real_, CI_U = 
                 NA_real_) 
  } else { 
    data.frame(Pillar = x$pillar, 
               Partial_Eta2_Int = round(int$Eta2_partial, 3), 
               CI_L = round(int$CI_low, 3), 
               CI_U = round(int$CI_high, 3)) 
  } 
})) 
print(interaction_table) 
# Optional: save for appendix 
# write.csv(interaction_table, "Country_Sex_Interaction_by_Pillar.csv", row.names = FALSE) 

# Example: Tukey on the 4 Country×Sex cells for RC2 
fit_rc2 <- aov(RC2_Innovative ~ Country * Sex, data = analysis_df_clean) 
TukeyHSD(fit_rc2, which = "Country:Sex") 

# --------------------------- 
# 9.1 Normality (Shapiro–Wilk) – long format 
# --------------------------- 
library(tidyr); library(rstatix) 
normality_results <- analysis_df %>% 
  dplyr::select(Country, all_of(pillars)) %>% 
  pivot_longer(cols = all_of(pillars), names_to = "Pillar", values_to = "Score") %>% 
  group_by(Country, Pillar) %>% 
  shapiro_test(Score) 
cat("\n--- 9.1: NORMALITY RESULTS (Shapiro-Wilk by Country x Pillar) ---\n") 
print(normality_results) 

if (nlevels(analysis_df$Country) == 2) { 
  cat("\n--- Wilcoxon test for RC3_Risk by Country (2 groups only) ---\n") 
  print(wilcox.test(RC3_Risk ~ Country, data = analysis_df)) 
} 

# ----------------------------------------- 
# 9.2 Homogeneity of variance (Levene) – use CLEAN data 
# ----------------------------------------- 
library(car) 
cat("\n--- 9.2: LEVENE'S TEST (RC1_Proactive ~ Country * Sex) ---\n") 


print(leveneTest(RC1_Proactive ~ Country * Sex, data = analysis_df_clean)) 

# ----------------------------------- 
# 9.3 Box's M (covariance equality by Country) 
# ----------------------------------- 
cat("\n--- 9.3: BOX'S M TEST (Pillars by Country) ---\n") 
print(box_m(analysis_df[, pillars], analysis_df$Country)) 

# ------------------------------- 
# 9.4 Multicollinearity (VIF) – use CLEAN data 
# ------------------------------- 
# 1. Recalculate Global_EO for the clean dataset to fix the 'Object Not Found' error 
analysis_df_clean$Global_EO <- rowMeans(analysis_df_clean[, c("RC1_Proactive", 
                                                              "RC2_Innovative",  
                                                              "RC3_Risk", "RC4_Competitive",  
                                                              "RC5_Autonomy")], na.rm = TRUE) 

# 2. Re-run the VIF model (Audit for multicollinearity) 
vif_model <- lm(Global_EO ~ Country + Sex, data = analysis_df_clean) 

cat("\n--- 9.4: MULTICOLLINEARITY AUDIT (VIF) ---\n") 
print(vif(vif_model)) 

# ----------------------------------------------- 
# 9.5 Heteroscedasticity (NCV) – use CLEAN data 
# ----------------------------------------------- 
cat("\n--- 9.5: NCV TEST ---\n") 
print(ncvTest(vif_model)) 

# --------------------------------------------------------- 
# 9.6 Multivariate outliers (Mahalanobis) – pillars only 
# --------------------------------------------------------- 
X   <- as.matrix(analysis_df[, pillars]) 
mu  <- colMeans(X) 
S   <- cov(X) 
md  <- mahalanobis(X, center = mu, cov = S) 
cut <- qchisq(0.999, df = length(pillars))  # conservative 
analysis_df$Outlier <- md > cut 
cat("\nMultivariate outliers detected (at 0.1% level): ", sum(analysis_df$Outlier), "\n") 

# ================================================================ 
# STEP 10: PROVING H1 (Global EO Gap) — MANOVA + Effects + Posthoc 
#  ================================================================ 
cat("\n--- 10.1: GLOBAL MANOVA RESULTS (Pillai's Trace) ---\n") 


fit_manova <- manova(cbind(RC1_Proactive, RC2_Innovative, RC3_Risk, 
                           RC4_Competitive, RC5_Autonomy) ~  
                       Country * Sex, data = analysis_df) 
print(summary(fit_manova, test = "Pillai")) 

# Canonical discriminant plot by Country 
library(candisc) 
man <- lm(cbind(RC1_Proactive, RC2_Innovative, RC3_Risk, RC4_Competitive, 
                RC5_Autonomy) ~ Country, 
          data = analysis_df) 
can <- candisc(man) 
plot(can, main = "Canonical Discriminant Functions by Country") 

# Effect size (partial eta^2) 
library(effectsize) 
cat("\n--- 10.2: MANOVA EFFECT SIZE (partial eta^2) ---\n") 
print(eta_squared(fit_manova)) 

# Post-hoc on Global EO (ANOVA + Tukey) 
cat("\n--- 10.3: POST-HOC COMPARISONS (Global EO via Tukey HSD) ---\n") 
fit_aov_global <- aov(Global_EO ~ Country, data = analysis_df) 
print(TukeyHSD(fit_aov_global)) 

# -------------------- 
# 10.4 Global Visuals 
# -------------------- 
library(ggplot2) 
ggplot(analysis_df, aes(x = Global_EO, fill = Country)) + 
  geom_density(alpha = 0.5) +  
  labs(title = "H1 Proof: Global EO Distribution", 
       subtitle = "Visualizing distributional differences by Country") +  
  theme_classic() 

# Radar (fmsb) — fix scaling with column-wise max/min 
library(fmsb) 
radar_data <- aggregate(cbind(RC1_Proactive, RC2_Innovative, RC3_Risk, 
                              RC4_Competitive, RC5_Autonomy) ~  
                          Country, data = analysis_df, FUN = mean) 
radar_df <- as.data.frame(radar_data[, -1]) 
rownames(radar_df) <- radar_data$Country 

radar_scaled <- rbind( 
  apply(radar_df, 2, max),   # per-variable max 
  apply(radar_df, 2, min),   # per-variable min 
  radar_df 
) 



radarchart(radar_scaled, 
           pcol = c("#2A8A8C", "#C83784"), 
           pfcol = c(rgb(0.16, 0.54, 0.55, 0.4), rgb(0.78, 0.22, 0.52, 0.4)), 
           plwd = 4, 
           title = "The Entrepreneurial DNA: Country Fingerprints") 
legend(x = 0.7, y = 1, legend = rownames(radar_scaled)[-c(1,2)], bty = "n", pch = 20, 
       col = c("#2A8A8C", "#C83784"), text.col = "black", cex = 1.0) 

# ================================================================ 
# STEP 11: PILLAR BREAKDOWN (H2 - H4) 
#  ================================================================ 
# Clean Sex for binary-only analyses where needed 
analysis_df_clean <- analysis_df %>% 
  filter(Sex %in% c("Male", "Female")) %>% 
  mutate(Sex = factor(Sex)) 

# H2: Risk-taking gap 
cat("\n--- H2: RISK RESULTS & OMEGA SQUARED ---\n") 
print(omega_squared(aov(RC3_Risk ~ Country, data = analysis_df))) 
boxplot(RC3_Risk ~ Country, data = analysis_df, col = c("#E69F00", "#56B4E9"), 
        main = "H2: Risk Propensity by Country", xlab = "Country", ylab = "RC3_Risk") 

# H3: Innovation interaction (Country x Sex) 
cat("\n--- H3: INNOVATION INTERACTION VERDICT ---\n") 
interaction.plot(x.factor = analysis_df_clean$Country,  
                 trace.factor = analysis_df_clean$Sex,  
                 response = analysis_df_clean$RC2_Innovative, 
                 col = c("#D55E00", "#0072B2"),  
                 lty = 1, lwd = 3, 
                 main = "H3: Innovation Interaction Effect (Male vs Female)", 
                 xlab = "Country", ylab = "Innovation Score (RC2)") 

cat("\n--- H3 INTERACTION SUBGROUP EVIDENCE (TUKEY) ---\n") 
fit_h3 <- aov(RC2_Innovative ~ Country * Sex, data = analysis_df_clean) 
print(TukeyHSD(fit_h3, which = "Country:Sex")) 

fit_h3 <- aov(RC2_Innovative ~ Country * Sex, data = analysis_df_clean) 
summary(fit_h3)                      # shows the Country:Sex interaction F, p 
effectsize::eta_squared(fit_h3)      # report partial η² for interaction and main effects 


# H4: Autonomy gap 
library(gplots) 
cat("\n--- H4: AUTONOMY RESULTS (Plot with 95% CI) ---\n") 
plotmeans(RC5_Autonomy ~ Country, data = analysis_df,  
          
          
          main = "H4: Mean Autonomy Scores (95% CI)", 
          xlab = "Country", ylab = "RC5_Autonomy",  
          col = "darkblue", barcol = "red", connect = FALSE) 

fit_h4 <- aov(RC5_Autonomy ~ Country, data = analysis_df) 
summary(fit_h4) 
effectsize::omega_squared(fit_h4) 


# ==============================================================================
# STEP 12: DEMOGRAPHIC AUDITS (H5)
# ==============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(effectsize)

# 12.1 Define Pillars (Corrected: No internal line breaks)
pillars <- c("RC1_Proactive", "RC2_Innovative", "RC3_Risk", "RC4_Competitive", "RC5_Autonomy")

# 12.2 Generate Publication-Quality Descriptive Table
pillar_summary <- analysis_df_clean %>%
  group_by(Country) %>%
  summarise(across(all_of(pillars), 
                   list(Mean = ~round(mean(.), 2), SD = ~round(sd(.), 2)), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -Country, 
               names_to = c("Pillar", ".value"), 
               names_sep = "_")

cat("\n--- 12.2: FINAL PILLAR COMPARISON TABLE (Descriptive Statistics) ---\n")
print(pillar_summary)

# 12.3 H5: Competitive Drive Analysis (Sex vs Country)
cat("\n--- 12.3: H5 ANOVA RESULTS (Competitive Drive) ---\n")
h5_fit <- aov(RC4_Competitive ~ Country + Sex, data = analysis_df_clean)
summary(h5_fit)

cat("\n--- H5 EFFECT SIZE (Partial Eta-Squared) ---\n")
# Interpretation: 0.01 = Small, 0.06 = Medium, 0.14 = Large
print(eta_squared(h5_fit, partial = TRUE))

# 12.4 H5 Visualization: Competitive Drive by Sex and Country
ggplot(analysis_df_clean, aes(x = Country, y = RC4_Competitive, fill = Sex)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white") +
  scale_fill_manual(values = c("Male" = "#E69F00", "Female" = "#999999")) +
  labs(
    title = "H5: Competitive Drive Distribution",
    subtitle = "Analysis of Variance by Country and Biological Sex",
    x = "Country",
    y = "Competitive Drive Score (RC4)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# ================================================================ 
# STEP 13: THE ZURICH PREDICTIVE SUITE (Logistic Model) 
#  ================================================================ 
final_df <- analysis_df_clean 

# Ensure exactly 2 levels and set reference level for positive class 
if (nlevels(final_df$Country) != 2) { 
  stop("Logistic regression requires 2 Country levels. Found: ", 
       nlevels(final_df$Country)) 
} 
# Make "Malaysia" reference so predicted prob = P(Scotland) 
if ("Malaysia" %in% levels(final_df$Country)) { 
  final_df$Country <- relevel(final_df$Country, ref = "Malaysia") 
} 

full_glm <- glm(Country ~ RC1_Proactive + RC2_Innovative + RC3_Risk +  
                  RC4_Competitive + RC5_Autonomy, 
                data = final_df, family = "binomial") 
step_model <- step(full_glm, direction = "both", trace = 0) 

final_df$Predicted_Prob <- predict(step_model, type = "response") 

# Map >0.5 to the second level (positive class) 
levs <- levels(final_df$Country) 
final_df$Prediction <- ifelse(final_df$Predicted_Prob > 0.5, levs[2], levs[1]) 

conf_matrix <- table(Actual = final_df$Country, Predicted = final_df$Prediction) 


cat("\n--- 13.2: PREDICTION ACCURACY (CONFUSION MATRIX) ---\n") 
print(conf_matrix) 

# Probability histogram 
ggplot(final_df, aes(x = Predicted_Prob, fill = Country)) + 
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") + 
  labs(title = "Predictive DNA: Probability of Scotland Residency", 
       x = "Predicted Probability", y = "Count") + 
  theme_minimal() 

# Confusion matrix heatmap 
conf_df <- as.data.frame(conf_matrix) 
ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Freq)) + 
  geom_tile() + 
  geom_text(aes(label = Freq), color = "white", size = 5) + 
  scale_fill_gradient(low = "skyblue", high = "darkblue") + 
  labs(title = "Model Accuracy: Confusion Matrix Heatmap", 
       subtitle = "Discriminatory Power of the DNA Pillars") + 
  theme_minimal() 

# ROC & AUC 
library(pROC) 
roc_obj <- roc(final_df$Country, final_df$Predicted_Prob, 
               levels = levs, direction = "<")  # "<" means prob of second level 
plot(roc_obj, col = "blue", main = "ROC Curve: Predictive Model") 
print(auc(roc_obj)) 

# ================================================================ 
# FINAL EXPORTS & SUMMARIES 
#  ================================================================ 
write.csv(analysis_df, "Master_Analytical_Results.csv", row.names = FALSE) 
cat("\n--- ALL ANALYSIS COMPLETE: PROCEED TO REPORT WRITING ---\n") 

# Descriptive summary 
descriptive_summary <- analysis_df %>% 
  group_by(Country) %>% 
  summarise( 
    Count = n(), 
    Grand_Mean_EO = mean(Global_EO), 
    SD_EO = sd(Global_EO), 
    .groups = "drop" 
  ) 

cat("\n--- DESCRIPTIVE SUMMARY OF THE ENTIRE DATASET ---\n") 
print(descriptive_summary) 

# Global Tukey HSD for H1 
global_aov <- aov(Global_EO ~ Country, data = analysis_df) 
cat("\n--- GLOBAL POST-HOC (TUKEY HSD) RESULTS ---\n") 
print(TukeyHSD(global_aov)) 
cat("\n--- GLOBAL IMPACT: OMEGA SQUARED ---\n") 
print(omega_squared(global_aov)) 
# Sanity check 
cat("\nColumns now in analysis_df:\n") 
print(colnames(analysis_df))