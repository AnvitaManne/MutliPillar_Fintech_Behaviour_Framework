#DATASET LOADING

library(tidyverse)
library(lubridate)

# Load household transaction dataset
household_raw <- read_csv("Daily Household Transactions.csv")

# Load demographic and credit profile dataset
demographics <- read_csv("Credit_Card_Dataset.csv")

# Load global spending behavior dataset
spending_habits <- read_csv("Spending_patterns_detailed.csv")

# Load mobile application financial datasets
app_expenses <- read_csv("Expenses_clean.csv")
app_income <- read_csv("Income_clean.csv")


#INITIAL PREPROCESSING AND DATASET PREPRATION

# Currency conversion rates
USD_TO_INR <- 95.72
BYN_TO_INR <- 34.36

# Pillar 1: Household transaction dataset

household_clean <- household_raw %>%
  
  mutate(
    Clean_Date = dmy_hms(Date, truncated = 3),
    
    Hour = hour(Clean_Date),
    
    Month = month(Clean_Date, label = TRUE),
    
    Day_Type = ifelse(
      wday(Clean_Date) %in% c(1, 7),
      "Weekend",
      "Weekday"
    ),
    
    Amount_INR = as.numeric(Amount),
    
    Transaction_Type = as.factor(`Income/Expense`)
  ) %>%
  
  filter(!is.na(Clean_Date)) %>%
  
  select(
    Clean_Date,
    Transaction_Type,
    Category,
    Subcategory,
    Note,
    Amount_INR,
    Day_Type,
    Hour
  )

household_final <- household_clean %>%
  
  mutate(
    Budget_Type = case_when(
      Category %in% c(
        "Food",
        "Transportation",
        "Household",
        "Health",
        "Education"
      ) ~ "Essential",
      
      Category %in% c(
        "subscription",
        "Festivals",
        "Entertainment",
        "Gifts",
        "Apparel"
      ) ~ "Lifestyle",
      
      TRUE ~ "Other"
    )
  )

# Pillar 2: Demographic and financial profile dataset

demographics_clean <- demographics %>%
  
  rename(
    User_ID = Customer_ID
  ) %>%
  
  mutate(
    Income_Group = ntile(Annual_Income, 5),
    
    Life_Stage = case_when(
      Age < 25 ~ "Gen Z / Student",
      
      Age >= 25 & Age < 40 ~ "Young Professional",
      
      Age >= 40 & Age < 60 ~ "Mid-Career",
      
      TRUE ~ "Senior"
    ),
    
    Risk_Profile = ifelse(
      Credit_Utilization_Ratio > 0.7 |
        Number_of_Late_Payments > 1,
      
      "High Risk",
      "Stable"
    )
  ) %>%
  
  mutate(
    across(
      c(
        Gender,
        Marital_Status,
        Employment_Status,
        Risk_Profile,
        Life_Stage
      ),
      as.factor
    )
  )

# Pillar 3: Global spending behavior dataset

spending_habits_clean <- spending_habits %>%
  
  rename(
    User_ID = `Customer ID`
  ) %>%
  
  mutate(
    Transaction_Date = as_date(`Transaction Date`),
    
    Amount_INR = `Total Spent` * USD_TO_INR,
    
    Price_Per_Unit_INR = `Price Per Unit` * USD_TO_INR
  ) %>%
  
  mutate(
    Budget_Type = case_when(
      Category %in% c(
        "Groceries",
        "Housing and Utilities",
        "Transportation",
        "Medical/Dental",
        "Education"
      ) ~ "Essential",
      
      Category %in% c(
        "Shopping",
        "Travel",
        "Entertainment",
        "Gifts",
        "Fitness",
        "Friend Activities",
        "Personal Hygiene"
      ) ~ "Lifestyle",
      
      TRUE ~ "Other"
    )
  )

# Pillar 4: Mobile application financial dataset

app_expenses_clean <- app_expenses %>%
  
  mutate(
    Clean_Date = as_datetime(date_time),
    
    Amount_INR = amount * BYN_TO_INR,
    
    Transaction_Type = "Expense"
  ) %>%
  
  mutate(
    Budget_Type = case_when(
      category %in% c(
        "Food",
        "Public transport",
        "Health",
        "House",
        "Education"
      ) ~ "Essential",
      
      category %in% c(
        "Cafe",
        "Gifts",
        "Entertainment",
        "Taxi",
        "Clothes"
      ) ~ "Lifestyle",
      
      TRUE ~ "Other"
    )
  )

app_income_clean <- app_income %>%
  
  mutate(
    Clean_Date = as_datetime(date_time),
    
    Amount_INR = amount * BYN_TO_INR,
    
    Transaction_Type = "Income",
    
    Budget_Type = "Income"
  )

mobile_cashflow <- bind_rows(
  
  app_expenses_clean %>%
    
    select(
      Clean_Date,
      Transaction_Type,
      Category = category,
      Amount_INR,
      Budget_Type,
      account
    ),
  
  app_income_clean %>%
    
    select(
      Clean_Date,
      Transaction_Type,
      Category = category,
      Amount_INR,
      Budget_Type,
      account
    )
)


# Household transaction dataset
p1_master <- household_final %>%
  mutate(
    User_ID = "India_Household_User",
    Date = Clean_Date,
    Source = "Pillar_1_Local_Household",
    Transaction_Type = as.character(Transaction_Type)
  ) %>%
  select(User_ID, Date, Category, Amount_INR, Budget_Type, Transaction_Type, Source)

# Demographic and financial profile dataset
p2_master <- demographics_clean %>%
  mutate(
    Source = "Pillar_2_Risk_Profiles",
    Transaction_Type = "Profile_Only"
  ) %>%
  select(User_ID, Source, Transaction_Type, Age, Annual_Income, Credit_Score, Risk_Profile, Life_Stage)

# Global spending behavior dataset
p3_master <- spending_habits_clean %>%
  mutate(
    Date = as_datetime(Transaction_Date),
    Source = "Pillar_3_Global_Habits",
    Transaction_Type = "Expense"
  ) %>%
  select(User_ID, Date, Category, Amount_INR, Budget_Type, Transaction_Type, Source)

# Mobile application cashflow dataset
p4_master <- mobile_cashflow %>%
  mutate(
    User_ID = account,
    Date = Clean_Date,
    Source = "Pillar_4_Mobile_App"
  ) %>%
  select(User_ID, Date, Category, Amount_INR, Budget_Type, Transaction_Type, Source)

# Merge all processed datasets
unified_master <- bind_rows(p1_master, p2_master, p3_master, p4_master)

# Verify final dataset structure
cat("Confirmed Columns:\n")
print(colnames(unified_master))