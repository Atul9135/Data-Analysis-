install.packages("WDI", repos = "https://cloud.r-project.org/")
install.packages("tidyverse", repos = "https://cloud.r-project.org/")
install.packages("corrplot", repos = "https://cloud.r-project.org/")
setwd("D:/R Projects Atul")
life_exp <- read.csv("D:/R Projects Atul/LIFE_EXPECTANCY.csv",  check.names = FALSE)
infant_mor <- read.csv("D:/R Projects Atul/INFANT_MORTALITY_RATE.csv",  check.names = FALSE)
gni_ppp <- read.csv("D:/R Projects Atul/GNI_PER_CAPITA.csv",  check.names = FALSE)
gini_coeff <- read.csv("D:/R Projects Atul/GINI_COEFFICINT.csv",  check.names = FALSE)
head(wb_data)
str(wb_data)
names(wb_data)
clean_life_exp <- wb_wide_to_long(life_exp, "Life_Expectancy")
clean_infant_mor <- wb_wide_to_long(infant_mor, "INFANT_MORTALITY_RATE")
clean_gni_ppp <- wb_wide_to_long(gni_ppp, "GNI_PER_CAPITA")
clean_gini_coeff <- wb_wide_to_long(gini_coeff, "GINI_COEFFICINT")
head(clean_life_exp)
wb_wide_to_long <- function(df, value_name) {
  
  year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
  
  long_list <- lapply(year_cols, function(yr) {
    data.frame(
      country = df$`Country Name`,
      year = as.integer(yr),
      value = as.numeric(df[[yr]])
    )
  })
  
  long_df <- do.call(rbind, long_list)
  names(long_df)[3] <- value_name
  
  return(long_df)
}
# 2. Put all clean dataframes into a list
list_of_dfs <- list(clean_life_exp, clean_infant_mor, clean_gni_ppp, clean_gini_coeff)
# 3. Merge them all automatically
# 'all = TRUE' ensures you keep rows even if data is missing in one file (Full Join)
master_df <- Reduce(function(x, y) merge(x, y, by = c("country", "year"), all = TRUE), list_of_dfs)
head(master_df)
focus_countries <- c("India", "United States", "China", 
                     "United Kingdom", "Germany", "Brazil")

wb_focus <- master_df[
  master_df$country %in% focus_countries &
    master_df$year == 2023,
]

wb_focus <- na.omit(wb_focus)
summary(wb_focus)
names(wb_focus)
library(corrplot)
head(wb_focus)
# 1. Select only the numeric columns for correlation
# (Assuming your columns are: life_exp, gni_ppp, infant_mortality, gini_index)
# Check names(wb_focus) to get the exact index numbers if 3:6 is wrong.
numeric_data <- wb_focus[c("Life_Expectancy", "INFANT_MORTALITY_RATE", "GNI_PER_CAPITA", "GINI_COEFFICINT")]
M <- cor(numeric_data)
# 3. Plot it
png("outputs/plots/correlation_matrix.png", 600, 600)
corrplot(M, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black") # Add numbers to the circles
dev.off()
png("outputs/plots/country_dashboard.png", 1200, 800)

# Set up a 2x2 grid for plots
par(mfrow = c(2, 2), mar = c(4, 10, 4, 2)) 

# Plot 1: Life Expectancy
barplot(sort(wb_focus$Life_Expectancy), horiz = TRUE, 
        names.arg = wb_focus$country[order(wb_focus$Life_Expectancy)], 
        las = 1, col = "lightgreen", main = "Life Expectancy (Years)")

# Plot 2: GNI (Wealth)
barplot(sort(wb_focus$GNI_PER_CAPITA), horiz = TRUE, 
        names.arg = wb_focus$country[order(wb_focus$GNI_PER_CAPITA)], 
        las = 1, col = "steelblue", main = "GNI per Capita (Wealth)")

# Plot 3: Infant Mortality (Lower is better!)
barplot(sort(wb_focus$INFANT_MORTALITY_RATE, decreasing = TRUE), horiz = TRUE, 
        names.arg = wb_focus$country[order(wb_focus$INFANT_MORTALITY_RATE, decreasing = TRUE)], 
        las = 1, col = "salmon", main = "Infant Mortality (Deaths/1000)")

# Plot 4: GINI (Inequality - Lower is better)
barplot(sort(wb_focus$GINI_COEFFICINT, decreasing = TRUE), horiz = TRUE, 
        names.arg = wb_focus$country[order(wb_focus$GINI_COEFFICINT, decreasing = TRUE)], 
        las = 1, col = "orange", main = "GINI Index (Inequality)")

dev.off()

png("outputs/plots/inequality_vs_mortality.png", 800, 600)

plot(wb_focus$GINI_COEFFICINT, wb_focus$INFANT_MORTALITY_RATE,
     xlab = "Inequality (GINI Index)",
     ylab = "Infant Mortality Rate",
     main = "Does Inequality Kill? (2023)",
     pch = 19, col = "red", cex = 1.5)

# Add labels
text(wb_focus$GINI_COEFFICINT, wb_focus$INFANT_MORTALITY_RATE, 
     labels = wb_focus$country, pos = 3)

# Add a trend line
abline(lm(INFANT_MORTALITY_RATE ~ GINI_COEFFICINT, data = wb_focus), 
       col = "blue", lty = 2)

dev.off()
# Save the 2022 snapshot
write.csv(wb_focus, "outputs/wb_focus_2022.csv", row.names = FALSE)

# Save the big master file (All countries, all years)
write.csv(wb_master, "outputs/wb_master_clean.csv", row.names = FALSE)