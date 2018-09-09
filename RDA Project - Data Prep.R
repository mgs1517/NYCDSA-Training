#########################################
# RDA PROJECT                           #   
# - Purpose: Prepare data for modeling  #
# - Michael Sullivan                    #
# - Dec 2017                            # 
# Note: LINEAR MOMS' TV FILTERED OUT    #
#########################################

#seasonality indices#

library(readxl)
DUPLO_Seasonality_Indices <- read_excel(
  "~/NYCDSA Training/RDA Project/DUPLO Seasonality Indices.xlsx")

colnames(DUPLO_Seasonality_Indices) = c(
  "2016_indice", "2017_indice", "2018_indice")

DUPLO_Seasonality_Indices = DUPLO_Seasonality_Indices %>%
  mutate(avg_seasonality = rowMeans(.[,1:3]))

DUPLO_Seasonality_Indices = data.frame(
  DUPLO_Seasonality_Indices, seq(1,52,1)) %>%
  select(5,4) %>%
  rename(weekid = seq.1..52..1.)
           
#fulldataset#

library(readxl)
full.data.set <- read_excel(
  "~/NYCDSA Training/RDA Project/RDA_Project-Data_Validation/data/LEGO RDA Project - Mike Sullivan.xlsx",
  sheet = "DUPLO Data",
  col_types = c("text", "text", "text",
                "text", "text", "text", 
                "text", "numeric"))

full.data.set$Metric[full.data.set$Activity == "D2C SAH Promotions"] =
  "Cost"

full.data.set = full.data.set %>%
  mutate(Week = as.integer(Week)) %>%
  filter(Metric != "Spend" & Metric != "spend") %>%
  rename(Variable_Type = `Variable Type`,
         Sub_Activity = `Sub Activity`,
         Sales_Channel = `Sales Channel`) %>%
  filter(Sub_Activity != "Linear" | is.na(Sub_Activity))

full.data.set$Sales_Channel[full.data.set$Sales_Channel == "WalMart"] = "Walmart"
full.data.set$Sales_Channel[full.data.set$Sales_Channel == "WalMart.Com"] = "Walmart.com"
full.data.set$Variable_Type[full.data.set$Variable_Type == "Marketing "] = "Marketing"
full.data.set$Metric[full.data.set$Metric == "Circulation "] = "Circulation"
full.data.set$Metric[full.data.set$Metric == "TRP"] = "TRPs"
full.data.set$Metric[full.data.set$Metric == "Store Counts"] = "Store Count"

#unique values in data#

unique_values = list()
for (i in 1:length(colnames(full.data.set))-1) {
  output = unique(full.data.set[,i])
  unique_values[i] = output
}
unique_values

#retailer & theme list#

theme.name.list = c("DUPLO", "CITY", "Super Heroes", "Ninjago",
               "Older Core IP", "Nexo Knights", "Star Wars",
               "Constraction", "Oldest Core", "All Others",
               "Juniors", "Girls NIP", "DC Super Heroes Girls",
               "Disney Princess")

retailer.name.list = c("Brand Retail", "Walmart", "Target",
                  "Toys R Us", "All Other", "Shop at Home",
                  "Amazon", "Walmart.com", "Target.com", 
                  "Toysrus.com")

activity.name.list =
  unique(full.data.set$Activity)[!is.na(unique(full.data.set$Activity))]

#adstocks#

adstock.list = data.frame(lapply(adstocks.byactivity.byretailer,
                                 as.character),
                          stringsAsFactors = FALSE)

adstock.list$adstock = as.numeric(adstock.list$adstock)
adstock.list$rsquared = as.numeric(adstock.list$rsquared)
adstock.list$retailer[adstock.list$retailer == "WalMart"] = "Walmart"
adstock.list$retailer[adstock.list$retailer == "WalMart.Com"] = "Walmart.com"
############# FUNCTIONS #############

###ADSTOCK###

#Create a list of unique weeks#

Every.week.generator = function(weekstart.g, weekend.g) {
  every.week = sort(unique(full.data.set$Week))
  every.week = data.frame(Week = as.integer(every.week)) %>%
    filter(Week >= weekstart.g, Week <= weekend.g)
  return(every.week)
}

#create a data set that includes data for every week for each activity#

every.week.each.activity = function(a) {
  transformed.marketing.data = data.frame()
  for (i in 1:length(activity.name.list)) {
    output = 
      filter(a, Activity == activity.name.list[i])
    output2 = 
      left_join(every.week, output, by = "Week")
    output2$Variable_Type[is.na(output2$Variable_Type)] = 
      output2$Variable_Type[!is.na(output2$Variable_Type)][1]
    output2$Activity[is.na(output2$Activity)] = 
      output2$Activity[!is.na(output2$Activity)][1]
    output2$Sub_Activity[is.na(output2$Sub_Activity)] =
      output2$Sub_Activity[!is.na(output2$Sub_Activity)][1]
    output2$Sales_Channel[is.na(output2$Sales_Channel)] =
      output2$Sales_Channel[!is.na(output2$Sales_Channel)][1]
    output2$Theme[is.na(output2$Theme)] =
      output2$Theme[!is.na(output2$Theme)][1]
    output2$Metric[is.na(output2$Metric)] =
      output2$Metric[!is.na(output2$Metric)][1]
    output2$Data[is.na(output2$Data)] = 0
    transformed.marketing.data = 
      rbind(transformed.marketing.data, output2)
  }
  transformed.marketing.data$Data =
    round(
    as.numeric(
      format(transformed.marketing.data$Data, scientific = FALSE)
    ), 
    digits = 2)
  transformed.marketing.data = 
    transformed.marketing.data %>% filter(!is.na(Activity))
  return(transformed.marketing.data)
}

#transforms single activities#

adstock.transformation = function(act, rate) {
  transformed = act[1]
  for (i in 2:length(act)) {
    y = act[i] + (transformed[i - 1] * rate)
    transformed[i] = y
  }
  transformed = round(as.numeric(
    format(transformed, scientific = FALSE)), digits = 2)
  return(transformed)
}

#loops adstock.transformation#

adstock.list.transformation = function(x, y) {
  final.marketing.data = vector("list", 14)
  for (i in 1:length(theme.name.list)) {
    for(j in 1:length(retailer.name.list)){
      temp.data = data.frame()
      for(k in 1:length(unique(x[[i]][[j]]$Activity))) {
        #filter on indiv activity#
        activity.filter = unique(
          x[[i]][[j]]$Activity)[k]
        output =
          x[[i]][[j]] %>%
          filter(Activity == activity.filter)
      
        #ID correct adstcok#
        act.ret = data.frame(
          activity = unique(output$Activity),
          retailer = retailer.name.list[j],
          stringsAsFactors = FALSE)
        act.ret = act.ret %>% filter(retailer == retailer.name.list[j])
        adstock = left_join(
          act.ret,
          y,
          by = c("activity", "retailer"))
        adstock = as.numeric(as.character(adstock[,3][1]))
      
        #trans act and replace orig data#
        transformed.act = 
          adstock.transformation(output$Data, adstock)
        output2 = cbind(output, transformed.act) %>%
          select(2:7, 1, 9) %>%
          rename(Data = transformed.act)
      
        #recreate list with all dataframe of all trans activities#
        temp.data = rbind(temp.data, output2)
        }
      final.marketing.data[[i]][[j]] = temp.data
      final.marketing.data[[i]][[j]]$Data[is.na(final.marketing.data[[i]][[j]]$Data)] = 0
      }
  return(final.marketing.data)
  }
}

##DEBUGGING#
final.marketing.data = vector("list", 14)
for (i in 1:length(theme.name.list)) {
for(j in 1:length(retailer.name.list)){
temp.data = data.frame()
for(k in 1:length(unique(
  full.theme.by.retailer[[i]][[j]]$Activity))) {
  
#filter on indiv activity#
activity.filter = unique(
  full.theme.by.retailer[[1]][[5]]$Activity)[1]
output =
  full.theme.by.retailer[[1]][[5]] %>%
  filter(Activity == activity.filter)

#ID correct adstock#
act.ret = data.frame(
  activity = unique(output$Activity),
  retailer = unique(retailer.name.list[5]),
  stringsAsFactors = FALSE)
adstock = left_join(
  act.ret,
  adstock.list,
  by = c("activity", "retailer"))
adstock = as.numeric(as.character(adstock[,3][1]))

#trans act and replace orig data#
transformed.act = 
  adstock.transformation(output$Data, adstock)
output2 = cbind(output, transformed.act) %>%
  select(2:7, 1, 9) %>%
  rename(Data = transformed.act)

#recreate list with all dataframe of all trans activities#
temp.data = rbind(temp.data, output2)
}
final.marketing.data[[i]][[j]] = temp.data
final.marketing.data[[i]][[j]]$Data[is.na(final.marketing.data[[i]][[j]]$Data)] = 0
}
}

a = c()
for (i in 1:length(retailer.name.list)) {
  aa = 
    unique(theme.by.retailer[[1]][[i]]$Sales_Channel)
  a[[i]] = aa
}

final.marketing.data[[1]][[1]]$Data[is.na(final.marketing.data[[1]][[1]]$Data)] = 0


glimpse(final.marketing.data)
View(final.marketing.data[[2]])
unique(final.marketing.data[[10]]$Sales_Channel)

###SPLITTING DATA BY THEME AND RETAILER (functions cont)###

theme.marketing.data.split = function(a, x) {
  data = filter(
    a,
    Variable_Type != "Sales",
    Theme == x)
  return(data)
}

retailer.data.split = function(b, y) {
  data = filter(
    b,
    str_detect(Sales_Channel, y) == TRUE |
      is.na(Sales_Channel))
  return(data)
}

sales.data.split = function(b, y) {
  data = filter(
    b,
    Sales_Channel == y
  )
  return(data)
}

sales.data.separator = function(c) {
  temp.data = 
    filter(
      c,
      Variable_Type == "Sales"
    )
  temp.data$Metric[temp.data$Metric == "Dollars"] = "sales"

  theme.sales.data.temp = list()
  for (i in 1:length(theme.name.list)) {
    output = 
      filter(temp.data, 
             Theme == theme.name.list[i])
    
    sales.data.by.retailer = list()
    for (j in 1:length(retailer.name.list)) {
    output2 = 
      sales.data.split(
        b = output,
        y = retailer.name.list[j])
    sales.data.by.retailer[[j]] = output2
  }
    theme.sales.data.temp[[i]] = sales.data.by.retailer
  }
  return(theme.sales.data.temp)
}

############# DATA PREP #############

#create a data set in which every activity has a row for each week#

every.week = Every.week.generator(weekstart.g = 201401, weekend.g = 201752)


#SEPARATE SALES DATA#

sales.data.byretailer = sales.data.separator(c = full.data.set)

#unique values in Sales data#

unique_values = list()
for (i in 1:length(colnames(sales.data.byretailer[[1]][[1]]))-1) {
  output = unique(sales.data.byretailer[[1]][[1]][,i])
  unique_values[i] = output
}
unique_values

#spreading sales data to tidy format#

sales.data.byretailer.tidy = vector("list", 14)
for(i in 1:length(theme.name.list)) {
  for(j in 1:length(retailer.name.list)) {
    output =
      spread(sales.data.byretailer[[i]][[j]], key = "Metric", value = "Data")
    sales.data.byretailer.tidy[[i]][[j]] = output
  }
  return(theme.by.retailer.tidy)
}


###SEPARATE MARKETING DATA BY THEME AND RETAILERS###
#loop data.split over all themes and then all retailers to create all data sets#

#spread across themes
theme.data = list()
  for (i in 1:length(theme.name.list)) {
  output = 
    theme.marketing.data.split(
      a = full.data.set,
      x = theme.name.list[i])
  theme.data[[i]] = output
  }

#break out theme data by retailer#

theme.by.retailer = list()
for (i in 1:length(theme.name.list)) {
  onetheme.data = theme.data[[i]]
  
  templist = list()
  for (j in 1:length(retailer.name.list)) {
  output = retailer.data.split(
    b = onetheme.data,
    y = retailer.name.list[j])
  templist[[j]] = output
  }
   theme.by.retailer[[i]] = templist
}

#insert all weeks into final theme/retail long data set#

full.theme.by.retailer = vector("list", 14)
for (i in 1:length(theme.name.list)) {
  for(j in 1:length(retailer.name.list)) {
    output =
      every.week.each.activity(theme.by.retailer[[i]][[j]])
    full.theme.by.retailer[[i]][[j]] = output
  }
  return(full.theme.by.retailer)
}

#unique values in theme by retailer data#

unique_values = list()
for (i in 1:(length(colnames(full.theme.by.retailer[[1]][[9]])) - 1)) {
  output = unique(full.theme.by.retailer[[1]][[9]][,i])
  unique_values[[i]] = output
}
unique_values

###transform marketing activity based on optimum adstocks###

final.marketing.data =
  adstock.list.transformation(full.theme.by.retailer, adstock.list)

###REFORMATTING MARKETING DATA###

final.marketing.data.tidy = vector("list", 14)
for(i in 1:length(theme.name.list)) {
  for(j in 1:length(retailer.name.list)) {
    output =
      spread(final.marketing.data[[i]][[j]],
             key = "Metric",
             value = "Data")
    final.marketing.data.tidy[[i]][[j]] = output
    }
  return(final.marketing.data.tidy)
}


###combine marketing and sales data###

final.rda.data = vector("list", 14)
for (i in 1:length(theme.name.list)) {
  for(j in 1:length(retailer.name.list)) {
    output = right_join(final.marketing.data.tidy[[i]][[j]],
                   sales.data.byretailer.tidy[[i]][[j]][ , c("sales", "Week")],
                   by = "Week")
    output = output %>%
      select(length(names(output)),
             1:length(names(output))
      ) %>% 
      mutate(Sales_Channel = retailer.name.list[j])
    final.rda.data[[i]][[j]] = output
  }
}

#gather columns to allow rbind
###Needs to modify first loop based on # of themes ###
final.rda.data.short = data.frame()
for (i in 1:1) {
  for (j in 1:length(retailer.name.list)) {
    output = gather(final.rda.data[[i]][[j]],
                    key="metric", value="value", -1:-7)
    final.rda.data.short = rbind(
      final.rda.data.short, output)
    final.rda.data.short =
      final.rda.data.short %>%
      filter(!is.na(value)) %>%
      select(2:9, 1)
    }
}

##spread variables to columns and add weekly indice###

final.final.rda.data =
  final.rda.data.short %>%
  select(-Variable_Type, -Sub_Activity, -metric) %>%
  spread(key = Activity,
         value = value) %>%
  mutate(weekid = as.numeric(substr(Week,5,6))) %>%
  left_join(DUPLO_Seasonality_Indices, by = "weekid") %>%
  select(-29) %>% 
  mutate(seasonid =
           ifelse(avg_seasonality >= 1, 1, 0)
         )

final.final.rda.data[is.na(final.final.rda.data)] = 0

#save file#

write.csv(x = final.final.rda.data,
            file = paste(
              "/efs/home/usmicsul/final_rda_data.csv")
  )


