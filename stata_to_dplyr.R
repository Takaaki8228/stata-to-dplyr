# --------------------------------------------
# Note for this script
# R code are written after #
# Stata code are are written after ###
# name of our dataframe is data
# names of our variables are var`i', where i=1,...,n
# In some cases, I show multiple corresponding R code 
# --------------------------------------------

# install packages if not already installed
library(tidyverse)
library(skimr)

# --------------------------------------------
#################### table ###################
# --------------------------------------------

# Stata 
table var1
# R 
data %>% dplyr::count(var1)

# --------------------------------------------
################### summary ##################
# --------------------------------------------

# Stata 
sum 
# R
skim(data)
skim(data) %>% summary()


# Stata
sum var1
# R
data %>% 
  summarise_each(funs(n(), mean, sd, min, max), 
                 var1)


# Stata
sum var1 var2 var3
# R
skim(var1, var2, var3)
# R
data %>%
  dplyr::summarise(var1_m = mean(var1),
                   var1_sd = sd(var1),
                   observation = dplyr::n()
  )


# Stata
############ check later
# R
data %>% 
  group_by(var2) %>%
  summarise_each(funs(n(), mean, sd, min, max), 
                 var1)

# --------------------------------------------
#################### rename ##################
# --------------------------------------------

# Stata
rename var1 new_var1
rename var2 new_var2
# R
data <- data %>%
  dplyr::rename(new_var1 = var1,
                new_var2 = var2)

# --------------------------------------------
############# generate & replace #############
# --------------------------------------------

# case of dummy variable
# Stata
gen new_var = 1 if var1 >= 6
replace new_var = 0 if var1 < 6
# R
data <- data %>%
  dplyr::mutate(new_var = ifelse(var1 >= 6,1,0))

# Stata
gen new_var = 1 if var1 == 24
replace new_var = 0 if var1 != 24
# R
data <- data %>%
  dplyr::mutate(new_var = ifelse(var1 == 24,1,0))

# Stata
gen new_var1 = var1 * 100
gen new_var2 = var1 / obs
# R
data <- data %>%
  dplyr::mutate(new_var1 = var1 * 100,
                new_var2 = var1 / obs)

# --------------------------------------------
##################### drop ###################
# --------------------------------------------

# Stata 
drop var1
# R
data <- data %>%
  dplyr::select(-var1)


# Stata 
drop var1 var2 var3
# R
data <- data %>%
  dplyr::select(-var1, -var2, -var3)


# Stata
drop if var1 == "NA"
# R
data <- data %>%
  dplyr::filter(var1 != "NA")

# --------------------------------------------
##################### keep ###################
# --------------------------------------------

# Stata 
keep var1 var2 var3
# R
data <- data %>%
  dplyr::select(var1, var2, var3)


# Stata 
keep var1 == 4
# R
data <- data %>%
  dplyr::filter(var1 == 4)


# Stata 
keep if var1 > 0.2 & var1 < 0.4
# R
data <- data %>%
  dplyr::filter(var1 > 0.2, var1 < 0.4)

# --------------------------------------------
#################### impute ##################
# --------------------------------------------

# Stata
gen one = 1 
impute var1 one, gen(new_var1)
drop var1
rename new_var1 var1
# R
data <- data %>%
  dplyr::mutate_at(vars(var1),
            funs(if_else(is.na(.), mean(., na.rm = T), .)))

# impute all variables except for var1
data <- data %>%
  dplyr::mutate_at(vars(-var1),
            funs(if_else(is.na(.), mean(., na.rm = T), .)))

