# Entire Dataset

# General R References: (R Core Team, 2020), (Stack Overflow, 2010),
# (Eremenko & Ponteves, 2017)

# Package References: -------------------
library(NbClust)
# (Charrad et al., 2014)
library(data.table)
# (Dowle & Srinivasan, 2021)
library(car)
# (Fox & Weisber, 2019)
library(fpc)
# (Hennig, 2020)
library(factoextra)
# (Kassambara & Mundt, 2020)
library(moments)
# (Komsta & Novomestky, 2015)
library(Rtsne)
# (Krijthe, 2015)
library(FactoMineR)
# (Le et al., 2008)
library(cluster)
# (Maechler et al., 2019)
library(psych)
# (Revelle, 2021)
library(clustMixType)
# (Szepannek, 2018)
library(caTools)
# (Tuszynski, 2021)
library(corrplot)
# (Wei & Simko, 2021)
library(stringr)
# (Wickham, 2019)
library(tidyverse)
# (Wickham et al., 2019)
library(readr)
# (Wickham & Hester, 2020)
library(dplyr)
# (Wickham et al., 2021)
library(clustertend)
# (Wright et al., 2021)

# Initial Dataset Analysis --------------------

#Importing using readr function read_csv
dataset = read_csv('online_shoppers_intention.csv')

# Print all tibble columns
options(tibble.width = Inf)

# Preliminary Data Cleaning  ----------------------------------------------

# Checking for NA values in entire dataset --------
dataset %>%
  select(everything()) %>%
  summarise_all(list(~sum(is.na(.))))

# (Simple boolean check)
any(is.na(dataset))

# Feature Name Simplifications --------
colnames(dataset)
dataset <- dataset %>% rename(Admin = Administrative, 
                                  Admin_Dur = Administrative_Duration, 
                                  Info = Informational,
                                  Info_Dur = Informational_Duration,
                                  Product = ProductRelated,
                                  Product_Dur = ProductRelated_Duration,
                                  BR = BounceRates,
                                  ER = ExitRates,
                                  PV = PageValues,
                                  Spec_Day = SpecialDay,
                                  OS = OperatingSystems,
                                  Traff_Type = TrafficType,
                                  Vis_Type = VisitorType)

## EDA --------------------

# Continuous Feature Analysis ----------------------------------
summary(dataset[1:10])

# Visualisations --------

# Page/ Page Duration Area Plots
ggplot(data = dataset, mapping = aes(x = Admin, fill = Revenue), alpha = 0.2) +
  geom_area(stat = "bin") + 
  ggtitle("Administrative Page Views per Session")

ggplot(data = dataset, mapping = aes(x = Info, fill = Revenue), alpha = 0.6) +
  geom_area(stat = "bin") +
  ggtitle("Informational Page Views per Session")

ggplot(data = dataset, mapping = aes(x = Product, fill = Revenue), alpha = 0.6) +
  geom_area(stat = "bin") +
  ggtitle("Product Page Views per Session")

ggplot(data = dataset, mapping = aes(x = Admin_Dur, fill = Revenue), alpha = 0.2) +
  geom_area(stat = "bin") + 
  ggtitle("Time Spent on Administrative Pages per Session")

ggplot(data = dataset, mapping = aes(x = Info_Dur, fill = Revenue), alpha = 0.6) +
  geom_area(stat = "bin") +
  ggtitle("Time Spent on Informational Pages per Session")

ggplot(data = dataset, mapping = aes(x = Product_Dur, fill = Revenue), alpha = 0.6) +
  geom_area(stat = "bin") +
  ggtitle("Time Spent on Product Pages per Session")

# BR, ER, PV
ggplot(dataset, aes(PV)) + 
  geom_line(aes(y = BR, colour = "BR"), alpha = 0.4) + 
  geom_line(aes(y = ER, colour = "ER"), alpha = 0.3) + 
  ylab("") +
  ggtitle("Bounce and Exit Rates in Relation to Page Values")

# Invesigating Correlations --------
corrplot.mixed(cor(dataset[1:9]),
               tl.pos = 'lt',
               tl.col = "black")

# Admin and Admin_Dur
ggplot(dataset, aes(x = Admin, y = Admin_Dur)) +
  geom_point() +
  geom_smooth(col = "darkolivegreen2") +
  ggtitle("Administrative Page Durations against Total Administrative Pages Viewed")
  
# Info and Info_Dur
ggplot(dataset, aes(x = Info, y = Info_Dur)) +
  geom_point() +
  geom_smooth(col = "darkorange") +
  ggtitle("Informational Page Durations against Total Informational Pages Viewed")

# Product and Product_Dur
ggplot(dataset, aes(x = Product, y = Product_Dur)) +
  geom_point() +
  geom_smooth(col = "darkorchid3") +
  ggtitle("Product Page Durations against Total Product Pages Viewed")

# Investigating Skew ----------

skeweness_value <- c(skewness(dataset$Admin), skewness(dataset$Admin_Dur),
            skewness(dataset$Info), skewness(dataset$Info_Dur),
            skewness(dataset$Product), skewness(dataset$Product_Dur),
            skewness(dataset$BR), skewness(dataset$ER),
            skewness(dataset$PV), skewness(dataset$Spec_Day))

skeweness_value
skew_table <- data.frame(skeweness_value, row.names = colnames(dataset[1:10]))
skew_table
# heavily skewed data

# Transforming Spec_Day --------
# Checking out Spec_Day
ggplot(dataset, aes(Spec_Day, fill = cut(Spec_Day, c(-2, 0, 0.2, 0.4, 0.6, 0.8, 1.0)))) + 
  geom_histogram() +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme(legend.position = "none") +
  ggtitle("The Proximity of Website Visits to Special Days")

# Making into factors
dataset$Spec_Day = factor(dataset$Spec_Day,
                          levels = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                          ordered = TRUE)

# Checking Spec_Day class
class(dataset$Spec_Day)

# Visualising changes
ggplot(dataset, aes(Spec_Day)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# Box plots for outlier detection --------
# Page and Page Duration Features:
ggplot(dataset_remov_extr) +
  aes(x = "", y = Admin) +
  geom_boxplot(fill = "darkorange") +
  theme_minimal() +
  ggtitle("Admin")

ggplot(dataset_remov_extr) +
  aes(x = "", y = Info) +
  geom_boxplot(fill = "deeppink3") +
  theme_minimal()+
  ggtitle("Info")

ggplot(dataset_remov_extr) +
  aes(x = "", y = Product) +
  geom_boxplot(fill = "chartreuse3") +
  theme_minimal() +
  ggtitle("Product")

ggplot(dataset_remov_extr) +
  aes(x = "", y = Admin_Dur) +
  geom_boxplot(fill = "darkorange") +
  theme_minimal() +
  ggtitle("Admin_Dur")

ggplot(dataset_remov_extr) +
  aes(x = "", y = Info_Dur) +
  geom_boxplot(fill = "deeppink3") +
  theme_minimal() +
  ggtitle("Info_Dur")

ggplot(dataset_remov_extr) +
  aes(x = "", y = Product_Dur) +
  geom_boxplot(fill = "chartreuse3") +
  theme_minimal() +
  ggtitle("Product_Dur")

# Remaining Features
ggplot(dataset_remov_extr) +
  aes(x = "", y = BR) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal() +
  ggtitle("BR")

ggplot(dataset_remov_extr) +
  aes(x = "", y = ER) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal() +
  ggtitle("ER")

ggplot(dataset_remov_extr) +
  aes(x = "", y = PV) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal() +
  ggtitle("PV")

# Categorical Feature Analysis ----------------------------------

# Checking categorical feature classes
cat_classes <- c(class(dataset$Spec_Day)[2], class(dataset$Month), 
                 class(dataset$OS), class(dataset$Browser), 
                 class(dataset$Region), class(dataset$Traff_Type), 
                 class(dataset$Vis_Type),class(dataset$Weekend), 
                 class(dataset$Revenue))

cat_class_df <- data.frame(cat_classes, row.names = colnames(dataset[10:18]))
cat_class_df

# Month Cleaning -------

# Changing June to Jun
dataset$Month <- recode_factor(dataset$Month, June = "Jun")

# reordering months into chronological order
dataset$Month <- factor(dataset$Month, levels = month.abb)

# Visualisations --------------
ggplot(dataset, aes(Month, fill = Spec_Day)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) + 
  ggtitle("Website Visits per Month")

ggplot(dataset, aes(Month, fill = Revenue)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) + 
  ggtitle("Website Visits per Month")

ggplot(dataset, aes(Vis_Type, fill = Vis_Type)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_x_discrete(drop = FALSE) + 
  ggtitle("Visitor Types")

ggplot(dataset, aes(OS)) +
  geom_bar(fill = "darkolivegreen") +
  ggtitle("Operating Systems")
 
ggplot(dataset, aes(Region)) +
  geom_bar(fill = "darkorange") +
  ggtitle("Regions")

ggplot(dataset, aes(Traff_Type)) +
  geom_bar(fill = "cadetblue3") +
  ggtitle("Traffic Types")

ggplot(dataset, aes(Weekend)) +
  geom_bar(fill = "darkorchid2") +
  ggtitle("Weekend Indicator")

ggplot(dataset, aes(Revenue)) +
  geom_bar(fill = "aquamarine2") +
  ggtitle("Outcome of Visit")

# Feature Engineering -----------------------------
# Categorical Feature Encoding ---------------------------------------------
# Code References: (Mangiafico, 2016), (Marsja, 2020)

dataset$OS = factor(dataset$OS,
                    levels = c('1', '2', '3', '4',
                               '5', '6', '7', '8'),
                    labels = c(1, 2, 3, 4, 5, 6, 7, 8))

dataset$Browser = factor(dataset$Browser,
                         levels = c('1', '2', '3', '4',
                                    '5', '6', '7', '8',
                                    '9', '10', '11', '12', '13'),
                         labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13))

dataset$Region = factor(dataset$Region,
                        levels = c('1', '2', '3', '4',
                                   '5', '6', '7', '8', '9'),
                        labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

dataset$Traff_Type = factor(dataset$Traff_Type,
                            levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', 
                                       '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 18, 19, 20))

dataset$Vis_Type = factor(dataset$Vis_Type,
                          levels = c('New_Visitor', 'Returning_Visitor', 'Other'),
                          labels = c("New", "Returning", "Other"))

dataset$Weekend = factor(dataset$Weekend,
                         levels = c(TRUE, FALSE),
                         labels = c(1, 0))

dataset$Revenue = factor(dataset$Revenue,
                         levels = c(TRUE, FALSE),
                         labels = c(1, 0))

# Transformations  ----------------------------------------
# Code References: (Zach, 2020)
# # Log(x+1) Transformation (Method 1): -----------------------------
log_data <- dataset
log_data[1:9] <- log1p(dataset[, 1:9])

# check log scaling effect:
log_skewed <- c(skewness(log_data$Admin), skewness(log_data$Admin_Dur),
                skewness(log_data$Info), skewness(log_data$Info_Dur),
                skewness(log_data$Product), skewness(log_data$Product_Dur),
                skewness(log_data$BR), skewness(log_data$ER),
                skewness(log_data$PV))

# # Square Root transformation (Method 2): -------------------------------
sqrt_data <- dataset
sqrt_data[1:9] <- sqrt(dataset[1:9])

sqrt_skewed <- c(skewness(sqrt_data$Admin), skewness(sqrt_data$Admin_Dur),
                 skewness(sqrt_data$Info), skewness(sqrt_data$Info_Dur),
                 skewness(sqrt_data$Product), skewness(sqrt_data$Product_Dur),
                 skewness(sqrt_data$BR), skewness(sqrt_data$ER),
                 skewness(sqrt_data$PV))

# # Cube Root transformation (Method 3): ----------------------------------------
cubrt_data <- dataset
cubrt_data[, 1:9] <- (dataset[, 1:9])^(1/3)

cubrt_skewed <- c(skewness(cubrt_data$Admin), skewness(cubrt_data$Admin_Dur),
                  skewness(cubrt_data$Info), skewness(cubrt_data$Info_Dur),
                  skewness(cubrt_data$Product), skewness(cubrt_data$Product_Dur),
                  skewness(cubrt_data$BR), skewness(cubrt_data$ER),
                  skewness(cubrt_data$PV))

# dropping Spec_Day from 'skewed' values
skewed <- skeweness_value[1:9]

# Skew statistics table
skewstats <- data.frame(skewed, log_skewed, sqrt_skewed, cubrt_skewed, 
                         row.names = colnames(dataset[1:9]))

# displaying the impact of the transformations on the skewness
skewstats

## Admin Histogram Comparison:
# Original
hist(dataset$Admin, xlab = 'Admin Pages', col='steelblue', main='Original Data')

# Log-transformed
hist(log_data$Admin, xlab = 'Admin Pages', col='coral2', main='Log Transformed Data')

# Square root transformed
hist(sqrt_data$Admin, xlab = 'Admin Pages', col='coral2', main='Square Root Transformed Data')

# Cubic root transformed
hist(cubrt_data$Admin, xlab = 'Admin Pages', col='coral2', main='Cubic Root Transformed Data')

# Feature Binning ------------------------------------------------
# Code References: (R CODER, 2021)

# Investigating continuous features from non-transformed dataset
summary(dataset[1:9])

# Non-zero Features:
non0_Admin <- dataset$Admin[dataset$Admin !=0 ]
non0_Admin_Dur <- dataset$Admin_Dur[dataset$Admin_Dur !=0]
non0_Info <- dataset$Info[dataset$Info !=0 ]
non0_Info_Dur <- dataset$Info_Dur[dataset$Info_Dur !=0 ]
non0_Product <- dataset$Product[dataset$Product !=0 ]
non0_Product_Dur <- dataset$Product_Dur[dataset$Product_Dur !=0 ]
non0_PV <- dataset$PV[dataset$PV !=0 ]

# Page and Page Duration Features: ---------------
tags <- factor(c("Zero", "Below Avg", "Above Avg", "High"))
BR_ER_tags <- factor(c("Low", "Below Avg", "Above Avg", "High"))

cat_data <- dataset %>% select(1:18)
cat_data <- as_tibble(cat_data) %>%
  mutate(Admin = case_when(
    Admin <= 0 ~ tags[1],
    Admin > 0 & Admin < unname(quantile(non0_Admin))[3] ~ tags[2],
    Admin >= unname(quantile(non0_Admin))[3] & Admin < unname(quantile(non0_Admin))[4] ~ tags[3],
    Admin >= unname(quantile(non0_Admin))[4] ~ tags[4]
  )) %>%
    mutate(Admin_Dur = case_when(
      Admin_Dur <= 0 ~ tags[1],
      Admin_Dur > 0 & Admin_Dur < unname(quantile(non0_Admin_Dur))[3] ~ tags[2],
      Admin_Dur >= unname(quantile(non0_Admin_Dur))[3] & Admin_Dur < unname(quantile(non0_Admin_Dur))[4] ~ tags[3],
      Admin_Dur >= unname(quantile(non0_Admin_Dur))[4] ~ tags[4]
  )) %>%
  mutate(Info = case_when(
    Info <= 0 ~ tags[1],
    Info > 0 & Info < unname(quantile(non0_Info))[3] ~ tags[2],
    Info >= unname(quantile(non0_Info))[3] & Info < unname(quantile(non0_Info))[4] ~ tags[3],
    Info >= unname(quantile(non0_Info))[4] ~ tags[4]
  )) %>%
  mutate(Info_Dur = case_when(
    Info_Dur <= 0 ~ tags[1],
    Info_Dur > 0 & Info_Dur < unname(quantile(non0_Info_Dur))[3] ~ tags[2],
    Info_Dur >= unname(quantile(non0_Info_Dur))[3] & Info_Dur < unname(quantile(non0_Info_Dur))[4] ~ tags[3],
    Info_Dur >= unname(quantile(non0_Info_Dur))[4] ~ tags[4]
  )) %>%
  mutate(Product = case_when(
    Product <= 0 ~ tags[1],
    Product > 0 & Product < unname(quantile(non0_Product))[3] ~ tags[2],
    Product >= unname(quantile(non0_Product))[3] & Product < unname(quantile(non0_Product))[4] ~ tags[3],
    Product >= unname(quantile(non0_Product))[4] ~ tags[4]
  )) %>%
  mutate(Product_Dur = case_when(
    Product_Dur <= 0 ~ tags[1],
    Product_Dur > 0 & Product_Dur < unname(quantile(non0_Product_Dur))[3] ~ tags[2],
    Product_Dur >= unname(quantile(non0_Product_Dur))[3] & Product_Dur < unname(quantile(non0_Product_Dur))[4] ~ tags[3],
    Product_Dur >= unname(quantile(non0_Product_Dur))[4] ~ tags[4]
  )) %>%
  mutate(BR = case_when(
    BR >= unname(quantile(BR))[1] & BR < unname(quantile(BR))[2] ~ BR_ER_tags[1],
    BR >= unname(quantile(BR))[2] & BR < unname(quantile(BR))[3] ~ BR_ER_tags[2],
    BR >= unname(quantile(BR))[3] & BR < unname(quantile(BR))[4] ~ BR_ER_tags[3],
    BR >= unname(quantile(BR))[4] ~ BR_ER_tags[4]
  )) %>%
  mutate(ER = case_when(
    ER >= unname(quantile(ER))[1] & ER < unname(quantile(ER))[2] ~ BR_ER_tags[1],
    ER >= unname(quantile(ER))[2] & ER < unname(quantile(ER))[3] ~ BR_ER_tags[2],
    ER >= unname(quantile(ER))[3] & ER < unname(quantile(ER))[4] ~ BR_ER_tags[3],
    ER >= unname(quantile(ER))[4] ~ BR_ER_tags[4]
  )) %>%
  mutate(PV = case_when(
    PV <= 0 ~ tags[1],
    PV > 0 & PV < unname(quantile(non0_PV))[3] ~ tags[2],
    PV >= unname(quantile(non0_PV))[3] & PV < unname(quantile(non0_PV))[4] ~ tags[3],
    PV >= unname(quantile(non0_PV))[4] ~ tags[4]
  ))

# Changing to ordered factors: -----------------------
cat_data$Admin <- factor(cat_data$Admin,
                    levels = tags,
                    ordered = TRUE)

cat_data$Admin_Dur <- factor(cat_data$Admin_Dur,
                    levels = tags,
                    ordered = TRUE)

cat_data$Info <- factor(cat_data$Info,
                        levels = tags,
                        ordered = TRUE)

cat_data$Info_Dur <- factor(cat_data$Info_Dur,
                   levels = tags,
                   ordered = TRUE)

cat_data$Product <- factor(cat_data$Product,
                   levels = tags,
                   ordered = TRUE)

cat_data$Product_Dur <- factor(cat_data$Product_Dur,
                   levels = tags,
                   ordered = TRUE)

cat_data$BR <- factor(cat_data$BR,
                 levels = BR_ER_tags,
                 ordered = TRUE)

cat_data$ER <- factor(cat_data$ER,
                 levels = BR_ER_tags,
                 ordered = TRUE)

cat_data$PV <- factor(cat_data$PV,
                 levels = tags,
                 ordered = TRUE)

# Categorical Binned Feature Visualisations: -----------
ggplot(cat_data, aes(Admin, fill = Admin)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Administrative Pages Viewed (Admin)")

ggplot(cat_data, aes(Admin_Dur, colour = Admin_Dur)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Time Spent on Administrative Pages (Admin_Dur)")

ggplot(cat_data, aes(Info, fill = Info)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Informational Pages Viewed (Info)")

ggplot(cat_data, aes(Info_Dur, colour = Info_Dur)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Time Spent on Informational Pages (Info_Dur)")

ggplot(cat_data, aes(Product, fill = Product)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Product Pages Viewed (Product)")
  
ggplot(cat_data, aes(Product_Dur, colour = Product_Dur)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Time Spent on Product Pages (Product_Dur)")

ggplot(cat_data, aes(BR, fill = BR)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Average Bounce Rate")

ggplot(cat_data, aes(ER, fill = ER)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Average Exit Rate")

ggplot(cat_data, aes(PV, fill = PV)) +
  theme(legend.position = "none") +
  xlab("") +
  geom_bar() +
  ggtitle("Average Page Values")

# Checking for NA values in entire cat dataset (sanity check) --------
any(is.na(cat_data))

# Unsupervised ML ----------------------------
# Dimensionality Reduction ---------------
# Code References: (Kassambara et al., 2021), (STHDA.com, 2017), 
# (STHDA.com, n.d), (Kassambara, 2017), (Kaggle.com, 2019)

# Testing Factor Analysis Adequacy (KMO Test) -------------
KMO(r = cubrt_data[, 1:9])
# Overall MSA = 0.68 

# Multiple Factor Analysis (MFA) -------------

# reorganising column numbers for MFA grouping:
MFA_data <- cubrt_data[, c(1,3,5,2,4,6,7,8,9,10,11,17,12,13,14,15,16,18)]

# checking ordered correctly:
colnames(MFA_data)
options(ggrepel.max.overlaps = Inf)
res.MFA <- MFA(base = MFA_data,
               group = c(3,3,3,3,2,3,1),
               type = c("s","s","s","n","n","n","n"),
               # the s indicates the features need standardising
               name.group = c("Page_Types", "Page_Durations", "Page_Stats",
                              "Visit_Time", "Device_Info", "Visitor_Info",
                              "Outcome"),
               graph = TRUE)

# showing results of the MFA
summary(res.MFA)

# drawing the scree plot
fviz_screeplot(res.mfa)

# Trying to understand the low % var:
# Checking correlations of numerical features (cubrt_data)
corrplot.mixed(cor(cubrt_data[1:9]),
               tl.pos = 'lt',
               tl.col = "black")

# Factor Analysis on Mixed Data (FAMD) -------------

options(ggrepel.max.overlaps = Inf)

res.FAMD <- FAMD(base = cubrt_data)
summary(res.FAMD)

# drawing the scree plot
fviz_screeplot(res.FAMD)

# Cluster Analysis -----------------------

# Checking necessity for scaling
summary(cubrt_data[1:9])

# Distance Measure: Gower (Filaire, 2018) ----------
set.seed(1997)

# Computing Gower distance (cubrt_data)
gower_dist <- daisy(cubrt_data, metric = c("gower"))
gower_matrix <- as.matrix(gower_dist)

# Print most similar visits
cubrt_data[which(gower_matrix == min(gower_matrix[gower_matrix != min(gower_matrix)]),
                  arr.ind = TRUE)[1, ], ]

# Print most dissimilar visits
cubrt_data[which(gower_matrix == max(gower_matrix[gower_matrix != max(gower_matrix)]),
                  arr.ind = TRUE)[1, ], ]

# Clustering Algorithm 1: PAM (cubrt_data) ----------------------------------------------------
# Code References: (Stack Overflow, 2016), (Daniels, 2018)

set.seed(1997)

# Average Silhouette width per cluster, per clustering solution
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

# Plotting the silhouette widths per cluster solution
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",
     main = "Determining Optimal Cluster Number k")
lines(1:8, sil_width)
abline(v = 3, col = "chartreuse3", lty = "dashed")

# Interpreting PAM Solution for k = 3:
k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- cubrt_data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# Visualising in a lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Reduced Dimension Visualisation", subtitle = "Mixed Dataset (cubrt_data)")

# Silhouette Analysis Graph 
# Code References: (Huang, 1998)
set.seed(1997)
fviz_silhouette(pam_fit)

# labelling observations with assigned PAM cluster number:
cubrt_data_clusters <- mutate(cubrt_data, PAM = pam_fit$clustering)

# graphs to analyse clustered observations:
ggplot(cubrt_data_clusters, aes(PAM, fill = Revenue)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Month)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Browser)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Spec_Day)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = OS)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Region)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Traff_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Vis_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(PAM, fill = Weekend)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(x = Admin, y = Admin_Dur, fill = PAM)) +
  geom_point(alpha = 0.2)

ggplot(cubrt_data_clusters, aes(x = Product, y = Product_Dur, fill = PAM)) +
  geom_point(alpha = 0.2)

ggplot(cubrt_data_clusters, aes(x = Info, y = Info_Dur, fill = PAM)) +
  geom_point(alpha = 0.2)

ggplot(cubrt_data_clusters, aes(Admin, fill = PAM)) + geom_histogram()

ggplot(cubrt_data_clusters, aes(PV, fill = PAM)) + geom_histogram()

# Clustering Algorithm 1: PAM (cat_data) ----------------------------------------------------
set.seed(1997)

# Computing Gower distance
cat_gower_dist <- daisy(cat_data, metric = c("gower"))
cat_gower_matrix <- as.matrix(cat_gower_dist)

# Average Silhouette Width per cluster, per clustering solution
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(cat_gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

# Plotting the silhouette widths per cluster solution
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",
     main = "Determining Optimal Cluster Number k")
lines(1:8, sil_width)
abline(v = 2, col = "chartreuse3", lty = "dashed")

# Interpreting PAM Solution
k <- 2
pam_fit <- pam(cat_gower_dist, diss = TRUE, k)
pam_results <- cat_data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# Visualising in a lower dimensional space
tsne_obj <- Rtsne(cat_gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Reduced Dimension Visualisation", subtitle = "Categorical Dataset (cat_data)")

# Silhouette Analysis Graph 
# interpretations: https://rstudio-pubs-static.s3.amazonaws.com/579984_6b9efbf84ee24f00985c29e24265d2ba.html
set.seed(1997)
pam_fit <- pam(cat_gower_dist, diss = TRUE, k=2)
fviz_silhouette(pam_fit)
pam_fit$id.med
# clustering shows how each of the observations have been clustered according

# labelling observations with assigned PAM cluster number:
cat_data_clusters <- mutate(cat_data, PAM = pam_fit$clustering)

# graphs to analyse clustered observations:
ggplot(cat_data_clusters, aes(PAM, fill = Spec_Day)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Month)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = OS)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Browser)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Region)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Traff_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Vis_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Weekend)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Revenue)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Admin)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Admin_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Info)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Info_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Product)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = Product_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = BR)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = ER)) +
  geom_bar() +
  ggtitle("")

ggplot(cat_data_clusters, aes(PAM, fill = PV)) +
  geom_bar() +
  ggtitle("")

# Clustering Algorithm 2: K-Prototypes (cubrt_data) ---------------------------
# Code References: (Szepannek, 2018), (Aschenbruck et al., 2020)
set.seed(1997)

# Creating TSS plot for elbow criterion:
kproto_tss <- numeric(8)
for(i in 1:8){
  kpres <- kproto(cubrt_data, k = i, verbose = FALSE)
  kproto_tss[i] <- kpres$tot.withinss
}

# Plotting the WCSS Method
plot(1:8, kproto_tss, type = "b", ylab = "Within Cluster Sum of Squares (WCSS)", 
     xlab = "Number of Clusters", main = "Optimal Number of Clusters k")
abline(v = 2, col = "chartreuse3", lty = "dashed")
abline(v = 4, col = "chartreuse3", lty = "dashed")

# k = 2
kproto_2 <- kproto(cubrt_data, k = 2, verbose = FALSE)

# k = 4
kproto_4 <- kproto(cubrt_data, k = 4, verbose = FALSE)

# Visualising in a lower dimensional space (k = 2):
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kproto_2$cluster))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Reduced Dimension Visualisation", subtitle = "k = 2")

# Visualising in a lower dimensional space (k = 4):
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kproto_4$cluster))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Reduced Dimension Visualisation", subtitle = "k = 4")

# Cluster validation statistics:
sil_method <- c(validation_kproto(method = "silhouette", kproto_2), validation_kproto(method = "silhouette", kproto_4))
sils <- data.frame(sil_method, row.names = c("k=2", "k=4"))
sils

# Center analysis:
kproto_2$centers

# Adding cluster labels to the observations:
cubrt_data_clusters <- mutate(cubrt_data_clusters, kproto2 = kproto_2$cluster)

# Visualisations for kproto_2
ggplot(cubrt_data_clusters, aes(kproto2, fill = Spec_Day)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Month)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = OS)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Browser)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Region)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Traff_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Vis_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Weekend)) +
  geom_bar() +
  ggtitle("")

ggplot(cubrt_data_clusters, aes(kproto2, fill = Revenue)) +
  geom_bar() +
  ggtitle("")

# sil plot for k=2 not possible (limitation of function)

# Additional Analysis: Changing lambda value to st.dev ----------------------------
set.seed(1997)

# Creating WCSS plot for elbow criterion:
kproto_tss <- numeric(8)
for(i in 1:8){
  kpres <- kproto(cubrt_data, k = i, verbose = FALSE, lambdaest(cubrt_data, num.method = 2))
  kproto_tss[i] <- kpres$tot.withinss
}

# Plotting the WCSS Method
plot(1:8, kproto_tss, type = "b", ylab = "Within Cluster Sum of Squares (WCSS)", 
     xlab = "Number of Clusters", main = "Optimal Number of Clusters k")
abline(v = 2, col = "chartreuse3", lty = "dashed")

kproto_2_sd <- kproto(cubrt_data, k = 2, verbose = FALSE, lambdaest(cubrt_data, num.method = 2))

# Center analysis:
kproto_2_sd$centers

summary(validation_kproto(method = "silhouette", kproto_2_sd))
# value = 0.4778555 (higher than lambda = variance solution)

# Clustering Algorithm 3: Hierarchical Clustering (cubrt_data) ----------------------------------------------------
# Code References: (Pathak, 2018), (Reusova, 2018)

#Smaller samples of cubrt_data and cat_data (same proportion of rev = true)
set.seed(1997)

split_cubrt = sample.split(cubrt_data$Revenue, SplitRatio = 3/4)
small_cubrt = subset(cubrt_data, split_cubrt == FALSE)

split_cat = sample.split(cat_data$Revenue, SplitRatio = 3/4)
small_cat = subset(cat_data, split_cat == FALSE)


# Dissimilarity Matrix (small_cubrt dataset) ----
set.seed(1997)
gower_dist <- daisy(small_cubrt, metric = c("gower"))


# Divisive Clustering ------
divisive_clust <- diana(as.matrix(gower_dist),
                        diss = TRUE, keep.diss = TRUE)

# Divisive Dendrogram
plot(divisive_clust, main = "Divisive Clustering (Mixed Data Sample)")

# Agglomerative Clustering (complete linkages) ------
set.seed(1997)
aggl_c <- hclust(gower_dist, method = 'complete')

# Agglomerative Dendrogram
plot(aggl_c,
     main = "Agglomerative Clustering with Complete Linkages (Mixed Data Sample)")

# Cluster Statistics Table Creation -------

cstats.table <- function(dist, tree, k) {
  clust.assess <-
    c("cluster.number", "n", "within.cluster.ss", "average.within", "average.between",
      "wb.ratio", "dunn2", "avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, "size")
  }
  
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k){
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  
  output.stats.df <- data.frame(output.stats)
  
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  
  rows.all <- c(clust.assess, row.clust)
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  
  output
}

# Cluster Table Comparison for Divisive Approach:
stats_cubrt_divisive <- cstats.table(gower_dist, divisive_clust, 8)
stats_cubrt_divisive

# Cluster Table Comparison for Agglomerative Approach:
stats_cubrt_aggl <- cstats.table(gower_dist, aggl_c, 8)
stats_cubrt_aggl

# Choosing the Number of Clusters
# Using the "Elbow' and Silhouette' methods to identify the best number of clusters

# Elbow Method ------------------
# Divisive
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        divisive_clust, 8))),
       aes(x = cluster.number, y = within.cluster.ss)) +
  geom_point() +
  geom_line() +
  ggtitle("Elbow Method for Divisive Clustering (Mixed Data Sample)") +
  labs(x = "Number of Clusters k", y = "Within Clusters Sum of Squares (WCSS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        aggl_c, 8))),
       aes(x = cluster.number, y = within.cluster.ss)) +
  geom_point() +
  geom_line() +
  ggtitle("Elbow Method for Agglomerative Clustering") +
  labs(x = "Number of Clusters k", y = "Within Clusters Sum of Squares (WCSS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette Method ----------------
#Divisive
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        divisive_clust, 8))),
       aes(x = cluster.number, y = avg.silwidth)) +
  geom_point() +
  geom_line() +
  ggtitle("Silhouette Method for Divisive Clustering") +
  labs(x = "Number of Clusters k", y = "Average Silhouette Width")
+
  theme(plot.title = element_test(hjust = 0.5))

#Agglomerative
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        aggl_c, 8))),
       aes(x = cluster.number, y = avg.silwidth)) +
  geom_point() +
  geom_line() +
  ggtitle("Silhouette Method for Agglomerative Clustering") +
  labs(x = "Number of Clusters k", y = "Average Silhouette Width")
+
  theme(plot.title = element_test(hjust = 0.5))

# Optimal k = 4 for divisive and agglomerative

# exploring divisive
div_k4 <- cutree(divisive_clust, k = 4)
small_cubrt_clusters <- mutate(small_cubrt, divisive_k4 = div_k4)
small_cubrt_clusters <- mutate(small_cubrt_clusters, sil_values = silhouette(div_k4, dist = gower_dist))

# Silhouette 
fviz_silhouette(silhouette(div_k4, dist = gower_dist))


# Visualisations of clusters and features --------------
ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Revenue)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Browser)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = OS)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Spec_Day)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Month)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Vis_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Weekend)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Traff_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cubrt_clusters, aes(divisive_k4, fill = Region)) +
  geom_bar() +
  ggtitle("")

# Clustering Algorithm 3: Hierarchical Clustering (cat_data) ----------------------------------------------------

# Dissimilarity Matrix (small_cat dataset) ----
set.seed(1997)
gower_dist <- daisy(small_cat, metric = c("gower"))

# Divisive Clustering ------
divisive_clust <- diana(as.matrix(gower_dist),
                        diss = TRUE, keep.diss = TRUE)

# Divisive Dendrogram
plot(divisive_clust, main = "Divisive Clustering (Categorical Data Sample)")

# Agglomerative Clustering (complete linkages) ------
set.seed(1997)
aggl_c <- hclust(gower_dist, method = 'complete')

# Agglomerative Dendrogram
plot(aggl_c,
     main = "Agglomerative Clustering with Complete Linkages (Categorical Data Sample)")

# Cluster Stats
# Divisive Approach:
stats_cat_divisive <- cstats.table(gower_dist, divisive_clust, 8)
stats_cat_divisive

# Agglomerative Approach:
stats_cat_aggl <- cstats.table(gower_dist, aggl_c, 8)
stats_cat_aggl

# Choosing the Number of Clusters
# Using the "Elbow' and Silhouette' methods to identify the best number of clusters

# Elbow Method ------------------
# Divisive
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        divisive_clust, 8))),
       aes(x = cluster.number, y = within.cluster.ss)) +
  geom_point() +
  geom_line() +
  ggtitle("Elbow Method for Divisive Clustering (Categorical Data Sample)") +
  labs(x = "Number of Clusters k", y = "Within Clusters Sum of Squares (WCSS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        aggl_c, 8))),
       aes(x = cluster.number, y = within.cluster.ss)) +
  geom_point() +
  geom_line() +
  ggtitle("Elbow Method for Agglomerative Clustering (Categorical Data Sample)") +
  labs(x = "Number of Clusters k", y = "Within Clusters Sum of Squares (WCSS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette Method ----------------
#Divisive
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        divisive_clust, 8))),
       aes(x = cluster.number, y = avg.silwidth)) +
  geom_point() +
  geom_line() +
  ggtitle("Silhouette Method for Divisive Clustering (Categorical Data Sample)") +
  labs(x = "Number of Clusters k", y = "Average Silhouette Width")
+
  theme(plot.title = element_test(hjust = 0.5))

#Agglomerative
ggplot(data = data.frame(t(cstats.table(gower_dist,
                                        aggl_c, 8))),
       aes(x = cluster.number, y = avg.silwidth)) +
  geom_point() +
  geom_line() +
  ggtitle("Silhouette Method for Agglomerative Clustering (Categorical Data Sample)") +
  labs(x = "Number of Clusters k", y = "Average Silhouette Width")
+
  theme(plot.title = element_test(hjust = 0.5))


# Investigating Clustering Solution (divisive k=2) ---------------
set.seed(1997)
sub_grp <- cutree(divisive_clust, k = 2)
table(sub_grp)

# add cluster label to dataset
small_cat_clusters <- small_cat
small_cat_clusters <- mutate(small_cat_clusters, divisive_k4 = sub_grp)

#silhouette plot
fviz_silhouette(silhouette(sub_grp, dist = gower_dist))

# Visualisations of clusters and features ---------------
ggplot(small_cat_clusters, aes(divisive_k4, fill = Revenue)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Browser)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = OS)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Spec_Day)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Month)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Vis_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Weekend)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Traff_Type)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Region)) +
  geom_bar() +
  ggtitle("")

# binned features
ggplot(small_cat_clusters, aes(divisive_k4, fill = Admin)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Admin_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Info)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Info_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Product)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = Product_Dur)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = BR)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = ER)) +
  geom_bar() +
  ggtitle("")

ggplot(small_cat_clusters, aes(divisive_k4, fill = PV)) +
  geom_bar() +
  ggtitle("")
