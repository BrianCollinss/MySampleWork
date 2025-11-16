## ----------------------------------------------------------
## Dummy E-commerce Star Schema (data.table)
## Date range: 2020-01-01 to 2025-12-31
## Fiscal year: July–June (FY ends in June)
## Extra dimension: DimPayment
## ----------------------------------------------------------

library(data.table)

set.seed(123)  # for reproducibility

## ==========================================================
## 1. Dimension tables
## ==========================================================

## --------------------------
## DimCity
## --------------------------

DimCity <- data.table(
  CityKey = 1:10,
  City = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Canberra", "Hobart", "Darwin", "Gold Coast", "Newcastle"),
  State = c("NSW", "VIC", "QLD", "WA", "SA", "ACT", "TAS", "NT", "QLD", "NSW"),
  Country = "Australia",
  Latitude = c(-33.8688, -37.8136, -27.4698, -31.9505, -34.9285, -35.2809, -42.8821, -12.4634, -28.0167, -32.9283),
  Longitude = c(151.2093, 144.9631, 153.0251, 115.8605, 138.6007, 149.1300, 147.3272, 130.8456, 153.4000, 151.7817),
  Region = c("East Coast", "South", "East Coast", "West", "South", "Capital Territory", "South", "North", "East Coast", "East Coast")
)

setkey(DimCity, CityKey)

## --------------------------
## DimCustomer
## --------------------------
n_customers <- 1000L

first_names <- c("Alex", "Jordan", "Taylor", "Casey", "Sam", "Chris", "Morgan",
                 "Jamie", "Riley", "Drew", "Bailey", "Cameron", "Logan", "Avery")
last_names  <- c("Smith", "Jones", "Brown", "Wilson", "Taylor", "Anderson",
                 "Martin", "Thompson", "White", "Lee", "Walker", "Hall")

cities  <- c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide",
             "Canberra", "Hobart", "Darwin", "Gold Coast", "Newcastle")
states  <- c("NSW", "VIC", "QLD", "WA", "SA", "ACT", "TAS", "NT")
country <- "Australia"

customer_city <- sample(DimCity$CityKey, n_customers, replace = TRUE)

DimCustomer <- data.table(
  CustomerKey     = 1L:n_customers,
  CustomerName    = paste(
    sample(first_names, n_customers, replace = TRUE),
    sample(last_names,  n_customers, replace = TRUE)
  ),
  Gender          = sample(c("Male", "Female", "Other"), n_customers, replace = TRUE,
                           prob = c(0.48, 0.48, 0.04)),
  AgeGroup        = sample(c("18-24","25-34","35-44","45-54","55-64","65+"),
                           n_customers, replace = TRUE),
  CustomerSegment = sample(c("Retail", "SMB", "Enterprise"), n_customers,
                           replace = TRUE, prob = c(0.7, 0.25, 0.05)),
  CityKey         = customer_city,
  Country         = "Australia",
  JoinDate        = sample(seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"),
                           n_customers, replace = TRUE)
)

setkey(DimCustomer, CustomerKey)
       
## --------------------------
## DimProduct
## --------------------------
n_products <- 200L

categories    <- c("Electronics", "Home & Garden", "Fashion", "Sports", "Beauty")
subcategories <- list(
  Electronics     = c("Phone", "Laptop", "Tablet", "Headphones", "Camera"),
  `Home & Garden` = c("Furniture", "Kitchen", "Bedding", "Decor", "Garden"),
  Fashion         = c("Menswear", "Womenswear", "Footwear", "Accessories"),
  Sports          = c("Fitness", "Outdoor", "Cycling", "Team Sports"),
  Beauty          = c("Skincare", "Makeup", "Haircare", "Fragrance")
)
brands <- c("Acme", "NovaTech", "GreenLeaf", "UrbanStyle", "Skyline",
            "Pulse", "Aurora", "Zenith", "Summit", "Mosaic")

pick_subcategory <- function(cat_vec) {
  vapply(cat_vec, function(cat) {
    choices <- subcategories[[cat]]
    sample(choices, 1)
  }, character(1L))
}

prod_category <- sample(categories, n_products, replace = TRUE)
prod_subcat   <- pick_subcategory(prod_category)

base_cost <- runif(n_products, min = 10, max = 500)
margin    <- runif(n_products, min = 0.1, max = 0.6)
price     <- round(base_cost * (1 + margin), 2)

DimProduct <- data.table(
  ProductKey   = 1L:n_products,
  ProductName  = paste(prod_category, prod_subcat, "Item", 1L:n_products),
  Category     = prod_category,
  Subcategory  = prod_subcat,
  Brand        = sample(brands, n_products, replace = TRUE),
  UnitCost     = round(base_cost, 2),
  UnitPrice    = price
)

setkey(DimProduct, ProductKey)

## --------------------------
## DimDate (2020-2025 with Fiscal Year July–June)
## --------------------------
all_dates <- seq.Date(from = as.Date("2020-01-01"),
                      to   = as.Date("2025-12-31"),
                      by   = "day")

calendar_year <- as.integer(format(all_dates, "%Y"))
month_number  <- as.integer(format(all_dates, "%m"))

## FiscalYear: if month >= 7 (Jul–Dec) -> FY = year + 1
##             if month <= 6 (Jan–Jun) -> FY = year
fiscal_year <- ifelse(month_number >= 7,
                      calendar_year + 1L,
                      calendar_year)

## FiscalQuarter (FYQ1 = Jul–Sep, FYQ2 = Oct–Dec, FYQ3 = Jan–Mar, FYQ4 = Apr–Jun)
fiscal_quarter <- character(length(all_dates))
fiscal_quarter[month_number %in% 7:9]   <- "Q1"
fiscal_quarter[month_number %in% 10:12] <- "Q2"
fiscal_quarter[month_number %in% 1:3]   <- "Q3"
fiscal_quarter[month_number %in% 4:6]   <- "Q4"

DimDate <- data.table(
  DateKey           = as.integer(format(all_dates, "%Y%m%d")),
  Date              = all_dates,
  Year              = calendar_year,
  MonthNumber       = month_number,
  MonthName         = format(all_dates, "%B"),
  DayOfWeek         = format(all_dates, "%A"),
  Quarter           = paste0("Q", ceiling(month_number / 3)),
  FiscalYear        = fiscal_year,
  FiscalYearLabel   = paste0("FY", fiscal_year),
  FiscalQuarter     = fiscal_quarter,
  FiscalYearQuarter = paste0("FY", fiscal_year, " ", fiscal_quarter)
)

setkey(DimDate, DateKey)

## --------------------------
## DimChannel
## --------------------------
DimChannel <- data.table(
  ChannelKey   = 1:4,
  ChannelName  = c("Online Store", "Mobile App", "Marketplace", "In-store Pickup"),
  OnlineFlag   = c(TRUE, TRUE, TRUE, FALSE)
)

setkey(DimChannel, ChannelKey)

## --------------------------
## DimPromotion
## --------------------------
n_promos <- 30L   # across 6 years

promo_types <- c("Percentage Discount", "Buy One Get One", "Free Shipping", "Bundle Deal")

promo_start  <- sample(seq(as.Date("2020-01-01"), as.Date("2025-11-30"), by = "day"),
                       n_promos, replace = TRUE)
promo_length <- sample(7:60, n_promos, replace = TRUE)
promo_end    <- promo_start + promo_length

DimPromotion <- data.table(
  PromotionKey  = 1L:n_promos,
  PromotionName = paste("Promo", 1L:n_promos),
  PromotionType = sample(promo_types, n_promos, replace = TRUE),
  DiscountPct   = round(runif(n_promos, min = 5, max = 40), 1),
  StartDate     = promo_start,
  EndDate       = promo_end
)

setkey(DimPromotion, PromotionKey)

## --------------------------
## NEW: DimPayment
## --------------------------
payment_methods <- c("Credit Card", "Debit Card", "PayPal", "Bank Transfer",
                     "Buy Now Pay Later")
providers <- list(
  "Credit Card"        = c("Visa", "Mastercard", "Amex"),
  "Debit Card"         = c("Visa Debit", "Mastercard Debit"),
  "PayPal"             = c("PayPal"),
  "Bank Transfer"      = c("Direct Deposit", "OSKO"),
  "Buy Now Pay Later"  = c("Afterpay", "Zip", "Klarna")
)

pick_provider <- function(method_vec) {
  vapply(method_vec, function(m) {
    sample(providers[[m]], 1)
  }, character(1L))
}

n_payment <- length(payment_methods)

pm_methods   <- payment_methods
pm_providers <- pick_provider(pm_methods)
pm_online    <- pm_methods != "Bank Transfer"

DimPayment <- data.table(
  PaymentKey        = 1L:n_payment,
  PaymentMethod     = pm_methods,
  PaymentProvider   = pm_providers,
  IsOnlinePayment   = pm_online,
  IsBuyNowPayLater  = pm_methods == "Buy Now Pay Later"
)

setkey(DimPayment, PaymentKey)

## ==========================================================
## 2. Fact table (FactSales)
## ==========================================================

n_fact <- 40000L   # still under 50k

# Random foreign keys across full 2020–2025 range
fact_dates    <- sample(DimDate$DateKey,         n_fact, replace = TRUE)
fact_cust     <- sample(DimCustomer$CustomerKey, n_fact, replace = TRUE)
fact_prod     <- sample(DimProduct$ProductKey,   n_fact, replace = TRUE)
fact_channel  <- sample(DimChannel$ChannelKey,   n_fact, replace = TRUE)
fact_payment  <- sample(DimPayment$PaymentKey,   n_fact, replace = TRUE)

# Assign promotions to ~35% of rows
has_promo  <- rbinom(n_fact, size = 1, prob = 0.35) == 1
promo_keys <- rep(NA_integer_, n_fact)
promo_keys[has_promo] <- sample(DimPromotion$PromotionKey, sum(has_promo), replace = TRUE)

# Quantity and product prices
quantity       <- sample(1:10, n_fact, replace = TRUE)
unit_price_vec <- DimProduct$UnitPrice[fact_prod]

# Convert DateKey to Date for checking promotion validity
date_for_fact <- as.Date(as.character(fact_dates), format = "%Y%m%d")

discount_pct_vec <- numeric(n_fact)
valid_promo_idx  <- which(!is.na(promo_keys))

if (length(valid_promo_idx) > 0) {
  promo_info <- DimPromotion[.(promo_keys[valid_promo_idx])]
  
  valid_date <- date_for_fact[valid_promo_idx] >= promo_info$StartDate &
    date_for_fact[valid_promo_idx] <= promo_info$EndDate
  
  discount_pct_vec[valid_promo_idx[valid_date]] <- promo_info$DiscountPct[valid_date]
}

gross_amount    <- quantity * unit_price_vec
discount_amount <- round(gross_amount * (discount_pct_vec / 100), 2)
net_amount      <- round(gross_amount - discount_amount, 2)

# Simulated order IDs (multiple lines per order)
n_orders  <- round(n_fact / 2)
order_ids <- sample(1:n_orders, n_fact, replace = TRUE)

FactSales <- data.table(
  SalesKey       = 1L:n_fact,
  OrderID        = order_ids,
  DateKey        = fact_dates,
  CustomerKey    = fact_cust,
  ProductKey     = fact_prod,
  ChannelKey     = fact_channel,
  PromotionKey   = promo_keys,
  PaymentKey     = fact_payment,
  Quantity       = quantity,
  UnitPrice      = round(unit_price_vec, 2),
  GrossAmount    = round(gross_amount, 2),
  DiscountPct    = discount_pct_vec,
  DiscountAmount = discount_amount,
  NetAmount      = net_amount
)

setkey(FactSales, SalesKey)

## ==========================================================
## 3. Write to CSV for Power BI
## ==========================================================

output_folder <- "C:/Users/brcol/My Drive/Documents/!Resume/Sample Work/PowerBI - Test"

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

fwrite(DimCustomer,  file.path(output_folder, "DimCustomer.csv"))
fwrite(DimCity,      file.path(output_folder, "DimCity.csv"))
fwrite(DimProduct,   file.path(output_folder, "DimProduct.csv"))
fwrite(DimDate,      file.path(output_folder, "DimDate.csv"))
fwrite(DimChannel,   file.path(output_folder, "DimChannel.csv"))
fwrite(DimPromotion, file.path(output_folder, "DimPromotion.csv"))
fwrite(DimPayment,   file.path(output_folder, "DimPayment.csv"))
fwrite(FactSales,    file.path(output_folder, "FactSales.csv"))
