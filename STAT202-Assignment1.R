
# 0. conditionally install (synchronise between home and uni laptops)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("conflicted", quietly = TRUE)) {
  install.packages("conflicted")
}

# 0.1  Load lib (and address conflicts)
library(tidyverse)
library(conflicted) 
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

# 1. Load dataset
data(starwars)
df <- starwars
View(starwars)

# 2. Create  scatter plot
ggplot(df, aes(x = height, y = mass)) +
  geom_point() +
  labs(title = "Scatter plot -  Mass vs. Height", 
       x = "Height (cm)", y = "Mass (kg)")

# weak and positive. if the outlyer was removed then I would suggest stronger and positive

#3. missing data
na_rows   <- starwars %>% filter(if_any(everything(), is.na))
na_graph  <- starwars %>% filter(is.na(mass) | is.na(height))
starwars |> select (mass,height) |> summary()

star_no <- starwars |> drop_na(mass, height) #drop na
star_no <- star_no  |> filter (mass <= 250)
star_no |> select (mass,height) |> summary()

# 5. Student_ID as value
Student_ID = 82171165
set.seed(Student_ID)
my_starno <-star_no |> sample_n(50)

# 6. scatter plot of filtered data
ggplot(my_starno, aes(x = height, y = mass)) +
  geom_point() +
  labs(title = "Scatter plot -  Mass vs. Height (filtered)", 
       x = "Height (cm)", y = "Mass (kg)")

# not weak, not strong, MODERATE and positive 

# 7. linear regression model
model_1 <- lm(mass ~ height, data = my_starno)
summary(model_1)
coefficients <- summary(model_1)$coefficients

# The intercept is where the height is zero and is useless on its own but is 
# required to maintain the slope of the line to properly fit a curve relative
# to the observed data points. 
#
# the slope shows how the mass of a character is expected to increase for every
# centimetre of highght increase as per the group of observed data points. 
#

#lm(formula = mass ~ height, data = my_starno)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-37.201  -5.908   1.214   5.379  50.679 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -28.39482   12.41314  -2.287   0.0266 *  
#  height        0.58858    0.07023   8.381 5.89e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 17.76 on 48 degrees of freedom
#Multiple R-squared:  0.594,	Adjusted R-squared:  0.5856 
#F-statistic: 70.24 on 1 and 48 DF,  p-value: 5.885e-11

# 8. centre the height 

mystar_no <- my_starno %>% mutate(cent_height = height - mean(height, na.rm = TRUE))
model_2 <- lm(mass ~ cent_height, data = mystar_no)
summary(model_2)
coefficients_2 <- summary(model_2)$coefficients

#Call:
#  lm(formula = mass ~ cent_height, data = mystar_no)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-37.201  -5.908   1.214   5.379  50.679 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 73.48800    2.51138  29.262  < 2e-16 ***
#  cent_height  0.58858    0.07023   8.381 5.89e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 17.76 on 48 degrees of freedom
#Multiple R-squared:  0.594,	Adjusted R-squared:  0.5856 
#F-statistic: 70.24 on 1 and 48 DF,  p-value: 5.885e-11

# 8.1 scatter plot of mystar_no data
ggplot(mystar_no, aes(x = height, y = mass)) +
  geom_point() +
  labs(title = "Scatter plot -  Mass vs. Height (mystar_no)", 
       x = "Height (cm)", y = "Mass (kg)")


# differences:
# primary difference is the intercept, r, r-squared and adjusted r-square
# remains the same. Indicating to me that the slope is the same and that the
# data shifted and this shift is primarily indicated in the intercept. 

