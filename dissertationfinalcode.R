library(tidyverse)

## CLEANING FMS DATA 

#load FMS data
fms_21 <- read.csv("fms_21.csv")
# there are 200,216 entries

# remove any values with an areacode from Northern Ireland
# To do this I filter out rows where areacode starts with '9'
fms_21 <- fms_21[!grepl("^9", fms_21$areacode), ]
# now there are 199,871 entries

# remove any columns that are not relevant columns from the dataset
fms_21 <- fms_21[c("areacode", "created")]

# Calculate the total number of complaints per MSOA
fms_21 <- aggregate(x = list(total_complaints = fms_21$areacode), 
                              by = list(MSOA = fms_21$areacode), 
                              FUN = length)
# there are 24,738 entries (LSOA code)

# rename MSOA column correctly to LSOA
names(fms_21)[names(fms_21) == "MSOA"] <- "LSOA"




## CLEANING LOOKUP DATA 
# load lookup data
lookupdata <- read.csv("lookupdata.csv")
# this has columns on LSOA code, LSOA name, MSOA code, MSOA name, LAD code and LAD name

# Remove columns that are not relevant from the dataset
lookupdata <- lookupdata[c("LSOA21CD", "MSOA21CD", "MSOA21NM", "LAD22CD", "LAD22NM")]

# Now, I determine the number of MSOA codes within the UK using the unique function
unique_msoa_codes <- unique(lookupdata$MSOA21CD)
# Count the number of unique MSOA codes
num_unique_msoa_codes <- length(unique_msoa_codes)
print(num_unique_msoa_codes)
# there are 7264 unique MSOA codes since this data includes codes from Wales.
# there are 408 welsh MSOA codes





## MERGING FMS WITH LOOKUPDATA
# merge datasets using merge function
fms_lookup <- merge(fms_21, lookupdata, by.x = "LSOA", by.y = "LSOA21CD", all.y = TRUE)

# rename MSOA column
names(fms_lookup)[names(fms_lookup) == "MSOA21CD"] <- "MSOA_Code"

# remove duplicate MSOA rows
fms_lookup <- fms_lookup %>% 
  distinct(MSOA_Code, .keep_all = TRUE)
# there are now 7,264 entries in total

# fill each NA value for total complaints with a 0 value.
fms_lookup$total_complaints[is.na(fms_lookup$total_complaints)] <- 0



## CLEANING + LOADING CENSUS DATA
# 1. Load population density data, Census 2021, last updated 16 February 2023.
pop_density <- read.csv("population_density.csv")
# change column names
names(pop_density)[names(pop_density) == "Observation"] <- "Population_Density"
names(pop_density)[names(pop_density) == "Middle.layer.Super.Output.Areas.Code"] <- "MSOA_Code"
# remove unnecessary columns 
pop_density <- pop_density[, !names(pop_density) %in% "Middle.layer.Super.Output.Areas"]


#2. Load Age data from 2021 Census
age <- read.csv("Age.csv")
# look at data structure 
head(age)
# Calculate the mean age for each MSOA
  # Create a column where each age is multiplied by the number of observations at that age
  age$Weighted_Age <- age$Age..101.categories..Code * age$Observation
  # Aggregate the weighted age sum and total observations for each MSOA
  sum_weighted_ages <- aggregate(Weighted_Age ~ Middle.layer.Super.Output.Areas.Code, data = age, FUN = sum)
  sum_observations <- aggregate(Observation ~ Middle.layer.Super.Output.Areas.Code, data = age, FUN = sum)
  # Merge the aggregated data
  age_data <- merge(sum_weighted_ages, sum_observations, by = "Middle.layer.Super.Output.Areas.Code")
  # calculate mean
  age_data$Mean_Age <- age_data$Weighted_Age / age_data$Observation
  # Subset to keep only MSOA and Mean Age columns
  age_data <- age_data[, c("Middle.layer.Super.Output.Areas.Code", "Mean_Age")]
  # Rename the MSOA column to MSOA_Code so I can merge this with other data sets later
  names(age_data)[names(age_data) == "Middle.layer.Super.Output.Areas.Code"] <- "MSOA_Code"


#3. Load Education data (highest qualification obtained) Census 2021
education <- read.csv("education1.csv")
# Calculate total population (sum across all education levels)
education$Total_Population <- rowSums(education[,!(names(education) %in% c("MSOA_Code", "MSOA_Name"))], na.rm = TRUE)
# Calculate the Percentage for Level 3 and Level 4 Qualifications
education$Level_3_and_4_Percentage <- (
  (education$`Level.3.qualifications..2.or.more.A.levels.or.VCEs..4.or.more.AS.levels..Higher.School.Certificate..Progression.or.Advanced.Diploma..Welsh.Baccalaureate.Advance.Diploma..NVQ.level.3..Advanced.GNVQ..City.and.Guilds.Advanced.Craft..ONC..OND..BTEC.National..RSA.Advanced.Diploma` +
     education$`Level.4.qualifications.or.above..degree..BA..BSc...higher.degree..MA..PhD..PGCE...NVQ.level.4.to.5..HNC..HND..RSA.Higher.Diploma..BTEC.Higher.level..professional.qualifications..for.example..teaching..nursing..accountancy.`)
  / education$Total_Population
) * 100
# remove irrelevant columns
education <- education[,c("MSOA_Code", "Level_3_and_4_Percentage")]

#4. Load Net Annual Household Income Before Housing Costs (Equivalised) data Census 2021
income <- read.csv("nethouseholdincome.csv")
# Rename columns 
names(income)[names(income) == "MSOA.code"] <- "MSOA_Code"
names(income)[names(income) == "Net.annual.income.before.housing.costs...."] <- "Net_Annual_Income"
# remove unnecessary columns
income <- income[, c("MSOA_Code", "Net_Annual_Income")]


#5. Loading and converting ethnic group data to EFI
library(readr)
ethnic_data <- read.csv("ethnic_MSOA.csv")
# Clean column names to remove spaces and special characters
colnames(ethnic_data) <- gsub(" ", "_", colnames(ethnic_data))
colnames(ethnic_data) <- gsub("[(]|[)]", "", colnames(ethnic_data))
colnames(ethnic_data) <- gsub("-", "_", colnames(ethnic_data))

# remove the 'does not apply' ethnic group category 
DNA_ethnicgroup <- subset(ethnic_data, Ethnic_group_20_categories == "Does not apply")
# To see the total observations for "Does Not Apply" category
total_observations_dna <- sum(DNA_ethnicgroup$Observation, na.rm = TRUE)
print(total_observations_dna)
# 0 entries for 'does not apply' so i will remove values in this category.
# Remove rows where 'Ethnic_group_20_categories' contains 'Does not apply'
ethnic_data <- subset(ethnic_data, Ethnic_group_20_categories != "Does not apply")

# calculate EFI for each MSOA
library(dplyr)
# Calculating the total population per MSOA
total_population <- ethnic_data %>%
  group_by(Middle_layer_Super_Output_Areas_Code) %>%
  summarise(Total = sum(Observation))
# Merging total population back to the original data
ethnic_data <- merge(ethnic_data, total_population, by = "Middle_layer_Super_Output_Areas_Code")
# Calculating proportion and squared proportion
ethnic_data$Proportion <- ethnic_data$Observation / ethnic_data$Total
ethnic_data$Proportion_Squared <- ethnic_data$Proportion^2
# Summing squared proportions by MSOA
squared_sums <- ethnic_data %>%
  group_by(Middle_layer_Super_Output_Areas_Code) %>%
  summarise(Sum_Squares = sum(Proportion_Squared))
# Calculating EFI - by subtracting the sum of the squared proportions from 1
squared_sums$EFI <- 1 - squared_sums$Sum_Squares
EFI_data <- squared_sums
# remove unnecessary columns 
EFI_data <- EFI_data[, c("Middle_layer_Super_Output_Areas_Code", "EFI")]
# Rename the MSOA column to MSOA_Code for merging with other datasets later
names(EFI_data)[names(EFI_data) == "Middle_layer_Super_Output_Areas_Code"] <- "MSOA_Code"


# calculate ethnic polarisation measure for each MSOA
ethnic_pt2_data <- read.csv("ethnic_MSOA.csv")
# Calculate total population per area
total_population <- aggregate(Observation ~ Middle.layer.Super.Output.Areas, ethnic_pt2_data, sum)
# Rename columns for clarity
names(total_population)[2] <- "Total.Population"
# Merge total population back to the original data
ethnic_pt2_data <- merge(ethnic_pt2_data, total_population, by = "Middle.layer.Super.Output.Areas")
# Calculate the proportion of each ethnic group
ethnic_pt2_data$Proportion <- with(ethnic_pt2_data, Observation / Total.Population)
# Function to calculate the RQ index for a vector of proportions
calculate_rq <- function(proportions) {
  1 - sum((proportions * (0.5 - proportions) / 0.5) ^ 2)
}
# Calculate the RQ index per area
rq_index <- aggregate(Proportion ~ Middle.layer.Super.Output.Areas, ethnic_pt2_data, calculate_rq)
# Rename the RQ column for clarity
names(rq_index)[2] <- "RQ"
# Print the results
print(rq_index)







# MERGE ALL DATASETS
# using left_join function to merge data sets - this keeps all rows from the first dataset and matching rows from subsequent datasets
census_fms <- fms_lookup %>%
  left_join(pop_density, by = "MSOA_Code") %>%
  left_join(age_data, by = "MSOA_Code") %>%
  left_join(education, by = "MSOA_Code") %>%
  left_join(income, by = "MSOA_Code") %>%
  left_join(EFI_data, by = "MSOA_Code")

# remove wales from the dataset
# obtain names of LAD in Wales 2021 https://geoportal.statistics.gov.uk/documents/d1fab2d9fb0a4576a7e08f89ac7e0b72/about
# LAD W06000001 to W06000024 are welsh LAD codes
wales_lad_codes <- c("W06000001", "W06000002", "W06000003", "W06000004", "W06000005",
                     "W06000006", "W06000008", "W06000009", "W06000010", "W06000011",
                     "W06000012", "W06000013", "W06000014", "W06000015", "W06000016",
                     "W06000018", "W06000019", "W06000020", "W06000021", "W06000022",
                     "W06000023", "W06000024")
# filter out wales rows
census_fms <- census_fms %>% 
  filter(!LAD22CD %in% wales_lad_codes)
# now there are 6,856 entries which is the total number of MSOAs in England

# remove unnecessary columns
census_fms <- subset(census_fms, select = -c(LSOA, MSOA21NM))

# check for missing values (ie 0) for total complaints
num_zero_complaints_msoas <- length(unique(census_fms$MSOA_Code[census_fms$total_complaints == 0]))
print(num_zero_complaints_msoas)
# Filter data for entries with zero complaints and count them by LAD
zero_complaints_count <- census_fms %>%
  filter(total_complaints == 0) %>%   # Filter rows where total_complaints is zero
  group_by(LAD22NM) %>%               # Group by Local Authority District Name
  summarise(Count = n())              # Count the number of zero complaint entries in each LAD
ggplot(zero_complaints_count, aes(x = reorder(LAD22NM, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Local Authority District", y = "Number of MSOAs with Zero Complaints",
       title = "Zero Complaints by Local Authority District") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for better visibility

# check for NA/missing values for each column/ variable
missing_counts <- colSums(is.na(census_fms))
print(missing_counts)
  # 179 missing values for income out of 6,856 rows
# Calculate mean
mean_income <- mean(census_fms$Net_Annual_Income, na.rm = TRUE)
# Impute missing values
census_fms$Net_Annual_Income <- ifelse(is.na(census_fms$Net_Annual_Income), mean_income, census_fms$Net_Annual_Income)


## Descriptive statistics.
install.packages("kableExtra")
library(knitr)
library(kableExtra)
install.packages("officer")
install.packages("flextable")
library(flextable)
# select variables
variables <- c("EFI", "total_complaints","Population_Density", "Mean_Age", "Level_3_and_4_Percentage", "Net_Annual_Income")
# Create a data frame of means and standard deviations
ds_df <- data.frame(
  Variable = variables,
  Mean = sapply(census_fms[variables], mean, na.rm = TRUE),
  SD = sapply(census_fms[variables], sd, na.rm = TRUE)
)
# Create the table
ds_table <- flextable(ds_df)
ds_table <- set_header_labels(ds_table, Mean = "Mean (M)", SD = "Standard Deviation (SD)")
ds_table <- autofit(ds_table)
# Create a new Word document and add the table
doc <- read_docx()
doc <- body_add_flextable(doc, ds_table)
print(doc, target = "Descriptive_Statistics.docx")



### STATISTICAL TESTING
## making FE model
install.packages("plm")
library(plm)
library(dplyr)
install.packages("fixest")
library(fixest)
install.packages("ggplot2")
library(ggplot2)
install.packages("modelsummary")
library(modelsummary)
install.packages("stargazer")
library(stargazer)

# first model including fixed effects for Local Authority District
fe_model <- feols(total_complaints ~ EFI + Population_Density + Mean_Age + Level_3_and_4_Percentage + Net_Annual_Income | LAD22CD, data = census_fms)
summary(fe_model)

## checking assumptions
# Assumption: checking for normal distribution of the residuals
# checking with histogram 
model_residuals = fe_model$residuals
hist(model_residuals)
# checking with Q-Q plot
par(mfrow=c(1, 2))
qqnorm(resid(fe_model), main="Original Q-Q Plot")
qqline(resid(fe_model), col = "steelblue", lwd = 2)

# transforming dependent variable + Refitting the model with the transformed variable
census_fms$total_complaints_log <- log(census_fms$total_complaints + 1) # +1 to avoid log(0)
fe_model_log <- feols(total_complaints_log ~ EFI + Net_Annual_Income + Population_Density + Mean_Age + Level_3_and_4_Percentage | LAD22CD, data = census_fms)
# Summarize the model
summary(fe_model_log)
## rechecking assumptions for normal distribution of the residuals
# Q-Q plot for residuals to check normality
qqnorm(resid(fe_model_log), main="New Q-Q Plot")
qqline(resid(fe_model_log), col="red")

# checking for homoscedasticity - the variance of the residuals should be constant across the range of predicted values.
# Plotting residuals
plot(fitted(fe_model_log), residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
# Fit the model with White's robust standard errors
fe_model_robust <- feols(total_complaints_log ~ EFI + Population_Density + Mean_Age + Level_3_and_4_Percentage + Net_Annual_Income | LAD22CD, data = census_fms, vcov = "white")
summary(fe_model_robust)



# Calculate VIF to check for multicollinearity
library(car)
vif_model <- lm(total_complaints_log ~ EFI + Population_Density + Mean_Age + Level_3_and_4_Percentage + Net_Annual_Income, data = census_fms)
vif(vif_model)
# Create table of VIf values
vif_values <- vif(vif_model)
vif_table <- data.frame(Variable = names(vif_values), VIF = vif_values)
print(vif_table)

# Plotting residuals vs fitted values
plot(fitted(fe_model_robust), residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
# Adding a loess smoothed line
smoothed_line <- loess(residuals ~ fitted_values)
lines(fitted_values, predict(smoothed_line), col = "red")


##  ROBUSTNES CHECKS
library(MASS) 

# Calculate standardized residuals
std_resids <- resid(fe_model_robust, type = "pearson")
# Plot histogram of standardized residuals to check their distribution
hist(std_resids, breaks = 50, main = "Histogram of Standardized Residuals")
# Identifying outliers based on standardized residuals
outliers <- which(abs(std_resids) > 2)  # Assuming outliers are those with a z-score > 2 or < -2
print(outliers)
# Plotting residuals vs. fitted values
plot(fitted(fe_model_robust), std_resids, xlab = "Fitted Values", ylab = "Standardized Residuals", main = "Residuals vs. Fitted Values Plot")
abline(h = c(-2, 2), col = "red")
# Excluding outliers and refitting the model
clean_data <- census_fms[-outliers, ]
fe_model_revised <- feols(total_complaints_log ~ EFI + Population_Density + Mean_Age + Level_3_and_4_Percentage + Net_Annual_Income | LAD22CD, data = clean_data)
summary(fe_model_revised)

# Boxplot of residuals
boxplot(residuals(fe_model_robust, horizontal=TRUE, main="Boxplot of Residuals"))


# presenting results
# Create the modelsummary table and store it as a gt object
gt_table <- modelsummary(fe_model_robust, 
                         stars = TRUE,
                         statistic = "std.error",
                         output = "gt")

# Customize the gt table
gt_table <- gt_table %>%
  tab_header(
    title = "Regression Results",
    subtitle = "Dependent variable: Log Total Complaints"
  ) %>%
  cols_label(
    `(1)` = "Log Total Complaints"
  )

print(gt_table)
