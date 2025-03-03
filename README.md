# R-programming
---
title: "MS5130 Assignment 1"
author: "24242479 - Richa Kandpal"
format: 
  html:
    embed-resources: true
    toc: true
---

# INTRODUCTION

In this report we delve into an Online Retail dataset, which contains the following columns: InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country, and obtain insights by using steps of data processing, visualization and predictive modelling, that can benefit business. The analysis includes Data cleaning with missing value treatment, duplicates removal,outlier detection. Exploratory Data Analysis (EDA) is done using bar charts and heatmaps, to find the sales trends, highest selling, least selling products, and customer behaviour. Moreover, a linear regression model investigates the correlation between quantity, pricing and time factors. By using statistical programming techniques, this study shows the practical use of data analytics in business decision-making

**Data Source:** Dua, D. and Graff, C. (2019). Online Retail Data Set. UCI Machine Learning Repository. Available at: https://archive.ics.uci.edu/dataset/352/online+retail [Accessed 24 February 2025]


# OVERVIEW
The mermaid code was written with the help of Lecture Notes and modified using ChatGPT.

```{mermaid}

flowchart LR
    x1["Upload Excel File"]
    x2["Check & Fix Data Types"]
    x3["Handle Missing Values"]
    x4["Remove Duplicate Values"]
    x5["Data Transformation"]
    x6["Add New Columns: Sales & Year_Month"]
    x7["Data Visualization"]
    x8["Return Analysis"]
    x9["Top 5 Countries"]
    x10["Top 5 Products"]
    x11["Sales Trend"]
    x12["Customer Behaviour Analysis"]
    x13["Predictive Modeling"]
    x14["Linear Regression: Sales ~ Price + Weekday"]

    x1 --> x2
    x2 --> x3
    x2 --> x4
    x2 --> x5
    x5 --> x6
    x6 --> x7
    x7 --> x8
    x7 --> x9
    x7 --> x10
    x7 --> x11
    x7 --> x12
    x6 --> x13
    x13 --> x14

```

# RUNNING CODE

### Loading Libraries and Data

```{r}
# Loading relevant libraries
suppressWarnings(library(tidyverse))
library(dplyr)
library(readxl)
library(gridExtra)
library(patchwork)

# Loading the dataset using read_excel from readxl library
online_data = read_excel("Online Retail.xlsx")
head(online_data)

```

### Check and Handle Data Types

```{r}
# Check the datatypes of all columns in the dataset
str(online_data)

```

The datatype for InvoiceNo is chr, so we will now convert InvoiceNo datatype to factor
InvoiceDate has timestamp, so we will convert InvoiceNo to Date only

```{r}
# Using mutate function from dplyr to change datatypes

online_data <- online_data %>% mutate(InvoiceNo = as.factor(InvoiceNo),
                                      CustomerID = as.factor(CustomerID),
                                      InvoiceDate = as.Date(InvoiceDate))

# Check the datatypes of all columns in the dataset again
str(online_data)

```
### Handling Missing Values

```{r}
summary(online_data)

# Checking NA in all columns
colSums(is.na(online_data))

```
The description has 1454 missing values and CustomerID has 135080 misssing values
```{r}
# Dropping the missing value rows for both Description and CustomerID Column
online_data_new <- drop_na(online_data)

# Rechecking NA in all columns
colSums(is.na(online_data_new))

```

### Removing Duplicate Rows

```{r}
# Checking for duplicate rows
sum(duplicated(online_data_new))

# Remove duplicate rows
data_unique <- online_data_new %>% distinct()

# Print the data frame without duplicate rows
head(data_unique)

```

# DATA TRANSFORMATION

```{r}
# Creating a New columns for Sales
data_unique$Sales <- data_unique$UnitPrice * data_unique$Quantity

#Adding Month and Year Column
data_unique <- data_unique %>%
  mutate(Month = format(InvoiceDate, "%m"),
         Year= format(InvoiceDate, "%Y"),
         Year_Month = as.factor(paste0(Year, "-", Month)))

#Separating Data with returns and no returns
data_no_returns <- data_unique %>% filter(Quantity > 0)
returns_data <- data_unique %>% filter(Quantity < 0 )

```

# DATA ANALYSIS AND VISUALIZATION

```{mermaid}

graph LR;
    A[Data] --> B[Group by Step]
    B --> B1[Apply group_by on selected variable] 
    B --> C[Summarization]
    C --> C1[Calculate sum, or count using summarise] 
    C --> D[Summarized Data]
    D --> E[ggplot - Use geom_bar/geom_line/geom_point for visualization]
    E --> G[Final Output]
    
```
### Returns Analysis

Most Returns are made in December and January which may be due to Holiday season.

```{r}
#Group by Year_Month to find total returns
returns_by_yearmonth <- returns_data %>% group_by(Year_Month) %>%
                        summarize( total_returns = sum(Quantity))
returns_by_yearmonth

```

```{r}

#Plot the returns
ggplot(returns_by_yearmonth, aes(x=Year_Month, y= abs(total_returns), group = 1)) +
       geom_point()+
       geom_line(colour = "red") +
       labs (title = "Returns by Year_Month", x= "Year_Month", y="Returns") +
       theme_minimal()
```

### Sales Heatmap by Year_Month and Country

Most of the Sales coming from UK since it's a UK ecommerce website.

```{r}
# Create dataset for heatmap
heatmap_data <- data_no_returns %>%
  group_by(Year_Month, Country) %>%
  summarise(TotalSales = sum(Sales), .groups = "drop")

heatmap_data
```

```{r}

# Create heatmap
ggplot(heatmap_data, aes(x = Year_Month, y = Country, fill = TotalSales)) +
  geom_tile() +
  scale_fill_gradient(low = "orange", high = "red")
  labs(title = "Sales Heatmap by Month & Country", x = "Year_Month", y = "Country", fill = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))  # Rotate x-axis labels
```

### Top 5 Countries with Maximum Sales

Netherlands, Germany, France and Ireland are have max sales after UK.

```{r}
# Group by Country to find number of invoices and max sales
country_df <- data_no_returns %>%
  group_by(Country) %>%
  summarize( total_sales = sum(Sales),
             distinct_invoices = n_distinct(InvoiceNo)) %>% 
  arrange(desc(total_sales), desc(distinct_invoices)) %>% 
  head(5)
```

```{r}
# Plot the column chart
ggplot(country_df, aes(x = Country, y = total_sales)) +
  geom_col(fill = "orange") +
  labs( title = "Total Sales by Country", x = "Country", y = "Total Sales") +
  theme_minimal()
```

### Top 5 and Bottom 5 Selling Products

This can help identify which products can benefit more with promotions or which products can be removed if there is no sales.

```{r}
# Group by Description to find top 5 selling products
top_5_products <- data_no_returns %>%
  group_by(Description) %>%
  summarise(total_product_sales = sum(Sales)) %>%
  arrange(desc(total_product_sales)) %>%
  head(5)

high_prod <- ggplot(top_5_products, aes(x = reorder(Description, total_product_sales), y = total_product_sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 5 Best-Selling Products", x = "Product", y = "Total Sales by Product")

# Identify the bottom 5 lowest-selling products
low_5_products <- data_no_returns %>%
  group_by(Description) %>%
  summarise(total_product_sales = sum(Sales)) %>%
  arrange(total_product_sales) %>%  # Sort in ascending order
  head(5)  # Get the bottom 5

# Plot the lowest-selling products
low_prod <- ggplot(low_5_products, aes(x = reorder(Description, total_product_sales), y = total_product_sales)) +
  geom_bar(stat = "identity", fill = "red") +  # Different color for low sales
  coord_flip() +
  labs(title = "Bottom 5 Lowest-Selling Products", x = "Product", y = "Total Sales by Product")

high_prod / low_prod

```


### Sales Trend Over Time

Most of the sales during November, which can be due to some promotional activities or holidays like Black Friday or Christmas shopping.

```{r}
# Aggregate daily sales
daily_sales <- data_no_returns %>%
  group_by(Year_Month) %>%
  summarise(sales_by_date = sum(Sales),
            qty_by_date = sum(Quantity)) %>% head(5)

daily_sales %>%
  gather(key,value, sales_by_date, qty_by_date) %>%
  ggplot(aes(x=Year_Month, y=value, colour=key, group = key)) +
  geom_point()+
  geom_line()
```

### Customer Purchase Behaviour

RFM Analysis can be used to identify High-Value and Low-value Customers and help in targetted campaigns.

```{r}
customer_summary <- data_no_returns %>%
  group_by(CustomerID) %>%
  summarise(
    Total_Spend = sum(Sales),
    Total_Transactions = n_distinct(InvoiceNo),
    Last_Purchase = max(InvoiceDate)
  )%>%
  mutate(Is_Repeat_Buyer = ifelse(Total_Transactions > 1, "Yes", "No"))

head(customer_summary)
```

# PREDICTIVE MODELLING

### Linear Regression

```{r}

# Adding a weekday column to use in linear regression assuming there might be some relationship between quantity, price, day of the week

data_no_returns$weekday <- weekdays(data_no_returns$InvoiceDate) 

#Removing outliers From Sales and UnitPrice column
data_no_outliers <- data_no_returns %>%
  filter(Quantity %in% boxplot.stats(Quantity)$out == FALSE)

data_no_outliers <- data_no_outliers %>%
  filter(UnitPrice %in% boxplot.stats(UnitPrice)$out == FALSE)
```

### Histogram to check distribution of Sales and UnitPrice

The Histogram for Quantity below shows that it is positively-skewed and does not have normal distribution.


```{r}
hist(data_no_outliers$Quantity, col ='violet')
```

The Histogram for UnitPrice is positively-skewed and does not have normal distribution.

```{r}
hist(data_no_outliers$UnitPrice, col = 'pink')
```

```{r}

# Scatter Plot Sales with Unit Price along with a fitted line

ggplot(mapping = aes(x = UnitPrice, y = Quantity), data = data_no_outliers) +
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)

```

By the above scatter plot we can see that there is a lot of variance in the data. Most of the data is concentrated near zero, which means that most of the sales occur at lower price points. The blue line which indicates a a moderate negative linear relationship, which supports the correlation analysis.

To explore further, Linear Regression is run below to check if Unitprice and Day of the week have significant effect on Quantity.

```{r}
# Running Multiple linear regression model
model <- lm(Quantity ~ UnitPrice + weekday , data = data_no_outliers)

# Model summary
summary(model)
```

The results show that UnitPrice, Monday, Tuesday, Wednesday and Sunday are negative and are highly significant. The Adjusted R-square is only 14% which tells that the model is not robust as only 14% of variability in Quantity is explained by independent variables. Other factors like seasonality, customer type, product category can be included to improve model accuracy.


# THANK YOU
