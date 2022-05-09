# Import Grocery Store Datasets
safeway <- read.csv("datasets/safeway.csv")
walmart <- read.csv("datasets/walmart.csv")
s.market <- read.csv("datasets/streets_market.csv")

# Function to create Sample Baskey from Safeway 
# Input: df.basket
# Output: df.basket
get.basket <- function(calories, weight) {
  calories.sum <- 0
  
  df <- safeway
  # Return DF of randomly selected items
  df.basket <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for (group in names(weight)){
    
    # Define threshold for calories
    threshold <- weight[group] * calories
    
    # Subset group
    df.group <- df[df$Food.Group == group, ]
    min_calories <- min(df.group$Calories) 
    
    # Sample until threshold
    while(TRUE){
      if(threshold <= 0){
        break
      }
      else{
        # Randomly select item
        chosen <- sample(unique(df.group$Item.Name), 1)
        record <- subset(df.group, Item.Name %in% chosen)
        if(length(record) > 1){
          record <- sample_n(record, 1)
        }
        if(threshold - record$Calories.per.serving > -min_calories ){
          threshold <- threshold - record$Calories.per.serving
          df.basket[nrow(df.basket) + 1,] = c(record$Food.Group, record$Item.Name, 1, record$Price/record$Servings, as.double(record$Calories.per.serving), record$Brand)
        }
      }
    }
  }
  colnames(df.basket) <- c('Food.Group', 'Item.Name', 'Serving', "Price", "Calories", "Brand")
  df.basket$Price <- as.numeric(as.character(df.basket$Price))
  df.basket$Calories <- as.numeric(as.character(df.basket$Calories))
  df.basket$Serving <- as.numeric(as.character(df.basket$Serving))
  df.basket <- aggregate(df.basket[,sapply(df.basket,is.numeric)],df.basket[c("Item.Name","Brand","Food.Group")],sum)
  return(df.basket)
}

# Function to fetch prices from competing stores - Walmart and Streets Market
# Input: df.basket
# Output: df.basket
get.competing.prices <- function(df.basket){
  
  # Walmart
  basket.walmart <- walmart[walmart$Item.Name %in% df.basket$Item.Name & walmart$Brand %in% df.basket$Brand, c("Item.Name","Price","Servings","Food.Group", "Brand")]
  basket.walmart$Price <- (basket.walmart$Price/basket.walmart$Servings)
  basket.walmart <- subset(basket.walmart, select = c("Item.Name", "Brand", "Food.Group", "Price"))
  names(basket.walmart)[names(basket.walmart) == 'Price'] <- 'Price.Walmart'
  basket.walmart
  df.basket <- merge(df.basket,basket.walmart,by=c("Item.Name", "Brand", "Food.Group"))  
  df.basket$Price.Walmart <- df.basket$Price.Walmart * df.basket$Serving
  df.basket <- aggregate(df.basket[,sapply(df.basket,is.numeric)],df.basket[c("Item.Name","Food.Group")],sum)
  
  # Streets Market
  basket.smart <- s.market[s.market$Item.Name %in% df.basket$Item.Name, c("Item.Name","Price","Servings")]
  basket.smart$Price <- (basket.smart$Price/basket.smart$Servings)
  basket.smart <- subset(basket.smart, select = c("Item.Name", "Price"))
  names(basket.smart)[names(basket.smart) == 'Price'] <- 'Price.SMarket'
  basket.smart
  df.basket <- merge(df.basket,basket.smart,by="Item.Name")
  df.basket$Price.SMarket <- df.basket$Price.SMarket * df.basket$Serving
  
  names(df.basket) <- c("Item Name", "Food Group", "Servings", "Safeway", "Calories", "Walmart", "Streets Market")
  df.basket <- df.basket %>% mutate_if(is.numeric, round, digits = 2)
  
  return(df.basket)
}
# Function to fetch basket of processed vs raw goods
# Output: df.processed.raw
compare_processed.raw <- function(){
  cols.processed <- c("Food.Group", "Main.Ingredient", "Item.Name", "Price")
  cols.raw <- c("Item.Name", "Price")
  
  # Walmart
  walmart.processed <- subset(walmart, is_Processed == 'Y' & Brand == "National" )
  walmart.processed$Price <- walmart.processed$Price / walmart.processed$Servings
  walmart.processed <- walmart.processed[cols.processed];walmart.processed
  walmart.raw <- subset(walmart, Item.Name  %in% walmart.processed$Main.Ingredient & Brand == "National")
  walmart.raw$Price <- walmart.raw$Price / walmart.raw$Servings
  walmart.raw <- walmart.raw[cols.raw]
  basket.walmart <- merge(walmart.processed, walmart.raw, by.x=c("Main.Ingredient"),by.y="Item.Name")
  colnames(basket.walmart) <- c('Raw', 'Food Group', 'Processed', "Processed Price", "Raw Price")
  basket.walmart$Difference = basket.walmart$`Raw Price` - basket.walmart$`Processed Price`
  basket.walmart$store <- "Walmart"
  
  # Safeway 
  safeway <- read.csv("datasets/safeway.csv")
  safeway.processed <- subset(safeway, is_Processed == 'Y' & Brand == "National" )
  safeway.processed$Price <- safeway.processed$Price / safeway.processed$Servings
  safeway.processed <- safeway.processed[cols.processed];safeway.processed
  safeway.raw <- subset(safeway, Item.Name  %in% safeway.processed$Main.Ingredient & Brand == "National")
  safeway.raw$Price <- safeway.raw$Price / safeway.raw$Servings
  safeway.raw <- safeway.raw[cols.raw]
  basket.safeway <- merge(safeway.processed, safeway.raw, by.x=c("Main.Ingredient"),by.y="Item.Name")
  colnames(basket.safeway) <- c('Raw', 'Food Group', 'Processed', "Processed Price", "Raw Price")
  basket.safeway$Difference = basket.safeway$`Raw Price` - basket.safeway$`Processed Price`
  basket.safeway$store <- "Safeway"
  
  # Streets Market
  market.processed <- subset(s.market, is_Processed == 'Y' )
  market.processed$Price <- market.processed$Price / market.processed$Servings
  market.processed <- market.processed[cols.processed]
  market.raw <- subset(s.market, Item.Name  %in% market.processed$Main.Ingredient)
  market.raw$Price <- market.raw$Price / market.raw$Servings
  market.raw <- market.raw[cols.raw]
  basket.market <- merge(market.processed, market.raw, by.x=c("Main.Ingredient"),by.y="Item.Name")
  colnames(basket.market) <- c('Raw', 'Food Group', 'Processed', "Processed Price", "Raw Price")
  basket.market$Difference = basket.market$`Raw Price` - basket.market$`Processed Price`
  basket.market$store <- "Streets Market"
  
  # Merge Dataframes
  df.processed.raw <- do.call("rbind", list(basket.walmart, basket.safeway, basket.market))
  col_order <- c("Food Group","Raw", "Raw Price","Processed", "Processed Price", "Difference", "store")
  df.processed.raw <- df.processed.raw[, col_order]
  df.processed.raw <- df.processed.raw %>% mutate_if(is.numeric, round, digits = 2)
  return(df.processed.raw)
}

# Function to fetch basket of private vs national products
# Output: df.private.national
compare_private.national <- function(){
  cols.brand <- c("Food.Group", "Item.Name", "Price")
  
  # Walmart
  walmart.national <- subset(walmart, Compare.Brand == TRUE & Brand == "National" )
  walmart.national$Price <- walmart.national$Price / walmart.national$Servings
  walmart.national <- walmart.national[cols.brand]
  colnames(walmart.national) <- c("Food Group", 'Item Name', 'National Price')
  walmart.national
  
  walmart.private <- subset(walmart, Compare.Brand == TRUE & Brand == "Private" )
  walmart.private$Price <- walmart.private$Price / walmart.private$Servings
  walmart.private <- walmart.private[cols.brand]
  colnames(walmart.private) <- c("Food Group", 'Item Name', 'Private Price')
  walmart.private
  
  basket.walmart <- merge(walmart.national, walmart.private, by=c('Item Name','Food Group') )
  basket.walmart$Difference <- basket.walmart$`Private Price` - basket.walmart$`National Price`
  basket.walmart$Store <- "Walmart"
  
  # Safeway
  safeway.national <- subset(safeway, Compare.Brand == TRUE & Brand == "National" )
  safeway.national$Price <- safeway.national$Price / safeway.national$Servings
  safeway.national <- safeway.national[cols.brand]
  colnames(safeway.national) <- c("Food Group", 'Item Name', 'National Price')
  safeway.national
  
  safeway.private <- subset(walmart, Compare.Brand == TRUE & Brand == "Private" )
  safeway.private$Price <- safeway.private$Price / safeway.private$Servings
  safeway.private <- safeway.private[cols.brand]
  colnames(safeway.private) <- c("Food Group", 'Item Name', 'Private Price')
  
  basket.safeway <- merge(safeway.national, safeway.private, by=c('Item Name','Food Group') )
  basket.safeway$Difference <- basket.safeway$`Private Price` - basket.safeway$`National Price`
  basket.safeway$Store <- "Safeway"
  basket.safeway
  
  df.private.national <- do.call("rbind", list(basket.walmart, basket.safeway))
  col_order <- c("Food Group","Item Name", "Private Price","National Price", "Difference", "Store")
  df.private.national <- df.private.national[, col_order]
  
  df.private.national <- df.private.national %>% mutate_if(is.numeric, round, digits = 2)
  
  return(df.private.national)
}

# Function: fetch total price per calories for regression plot
# Input: cals.l - list of calories
#        weight - food group weight
# Output: cal.v - vector of basket price per calories given diet(weight)
get_diet_cals <- function(cals.l, weight){
  cals.v <- c()
  for(cal in cals.l){
    basket <- get.basket( cal, weight) 
    cals.v <- c(cals.v,sum(basket$Price))
  }
  return(cals.v)
}
