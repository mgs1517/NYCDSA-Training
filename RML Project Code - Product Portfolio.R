#################
# RML Project
# Project Portfolio Analysis
# Date: Apr to Jun 2018
# Notes: 
#

library(tidyverse)
library(purrr)
library(readxl)
library(readr)
library(ggplot2)
library(mice)
library(scales)
library(VIM)
library(kknn)
library(dummies)
library(magrittr)
library(zoo)
library(cowplot)
library(randomForest)
library(glmnet)
library(car)
library(psych)
library(flexclust)
library(factoextra)

## LOAD RAW DATASETS ##
raw_product_data <- read_excel(
  "2014-2019 Product Asst Comparison v10.2.xlsx",
  sheet = "Aggregated Product",
  skip = 1) %>% select(-44:-47)
dim(raw_product_data)
head(raw_product_data)
summary(raw_product_data)

raw_sales_data = 
  read_excel("consumer_sales_data_monthly_byitem.xlsx",
             col_names = c(
               "product_no", "super_theme",
               "month_year", "nip", "units"),
             col_types = c("text", "text",
                           "text", "numeric",
                           "numeric"),
             skip = 1)
dim(raw_sales_data)
summary(raw_sales_data)

monthly_sales_seasonality = read_excel(
  "consumer_sales_monthly_seasonality.xlsx",
  col_names = c("month_year", "nip", "units"),
  col_types = c("text", "numeric", "numeric"),
  skip = 1)

#jddata#

movies <- read_csv("movies.csv")

unique_elements <- read_csv("sets_dedup.csv")

VBS <- read_csv("VBS_ALL_V2.csv") %>% select(-3, -5)  #remove item description due to unrecongizable characters

## PREP PRODUCT DATA ##

names(raw_product_data)
lapply(raw_product_data, unique)

product_data = raw_product_data %>%
  mutate(product_no = as.numeric(`Product No`),
         sap_product_no = as.numeric(`SAP Product No`)) %>%
  select(-2, -4, -43, -13:-20, -30:-31) %>%
  select(1, 31, 32, 2:29) %>%
  filter(Year != 2019) %>%
  rename(year = Year) %>%
  mutate(year_id = as.numeric(paste(product_no, year, sep = "")))

names(product_data) = gsub(" ", "_", names(product_data))

product_data$New_Novelty[is.na(product_data$New_Novelty)] = "N"
product_data$New_Novelty[
  product_data$New_Novelty == "New Novelty"] = "Y"

#dupllicate IDs
dim(product_data)[1] - length(unique(product_data$year_id))

product_data = product_data %>%
  filter(!duplicated(year_id, fromLast = TRUE))

dim(product_data)[1] - length(unique(product_data$year_id))

#jddata#

#isolating set numbers and converrting to numeric. NAs are for impulse, bags, etc. Remove them
#unique_elements$set_num = 
#  as.numeric(
#    sapply(strsplit(unique_elements$set_num,"-"), `[`, 1))
    
unique_elements.data = unique_elements %>%
  filter(!is.na(set_num)) %>%
  rename(product_no = set_num) %>%
  mutate(year_id = as.numeric(paste(product_no, year, sep = ""))) %>% 
  filter(num_parts != 0 & product_no != 0) %>% 
  group_by(product_no) %>% summarise(num_parts = sum(num_parts)) %>% 
  as.data.frame %>% mutate(product_no = as.numeric(product_no))

#movie data - combining month & year

movies.data = movies %>% mutate(wk_yr_id = as.numeric(paste(movies$Month, movies$Year, sep = ""))) %>%
  select(-1)

movies.data = movies.data %>% rename(product_no = Product.Number, year = Year) %>%
  mutate(year_id = as.numeric(paste(product_no, year, sep = "")))

#VBS

VBS.data = VBS %>%
  rename(product_no = `PLM Comm. No.`, year = `Calendar year`) %>%
  mutate(year_id = as.numeric(paste(product_no, year, sep = "")))

#combined jddata with product data#

product_data_comb = 
  left_join(
    product_data, unique_elements.data, by = "product_no") %>%
  left_join(VBS.data, by = "year_id") %>%
  select(-34:-36) %>%
  left_join(movies.data, by = "year_id") %>%
  select(-43, -44, -46)

##PREP SALES DATA##

#calc monthly seasonality#

avg_annual_sales_for_seasonality =
  monthly_sales_seasonality %>% 
  mutate(year = sapply(strsplit(month_year,"\\."), `[`, 2)) %>%
  group_by(year) %>% summarise(avg_nip = mean(nip),
                               avg_units = mean(units))

monthly_sales_seasonality = monthly_sales_seasonality %>% 
  mutate(year = sapply(strsplit(month_year,"\\."), `[`, 2))

monthly_sales_seasonality = left_join(
  monthly_sales_seasonality, 
  avg_annual_sales_for_seasonality,
  by = "year") %>%
  mutate(nip_seasonality = nip / avg_nip,
         units_seasonality = units / avg_units) 
#setting 2018 seasonality to 2017 seasonality#

months_2018 = c("01.2018", "02.2018", "03.2018", "04.2018", "05.2018", "06.2018")
months_2017 = c("01.2017", "02.2017", "03.2017", "04.2017", "05.2017", "06.2017")

for (i in 1:length(months_2018)) {
    monthly_sales_seasonality$nip_seasonality[
      monthly_sales_seasonality$month_year == months_2018[i]] = 
      monthly_sales_seasonality$nip_seasonality[
        monthly_sales_seasonality$month_year == months_2017[i]]
}

for (i in 1:length(months_2018)) {
  monthly_sales_seasonality$units_seasonality[
    monthly_sales_seasonality$month_year == months_2018[i]] = 
    monthly_sales_seasonality$units_seasonality[
      monthly_sales_seasonality$month_year == months_2017[i]]
} 

# de-seasonalizing sales & removing 06.2018

sales_data = left_join(raw_sales_data, monthly_sales_seasonality,
          by = "month_year") %>%
  select(-6:-7, -9:-10) %>%
  filter(month_year != "06.2018") %>%
  rename(nip = nip.x,
         units = units.x) %>%
  mutate(nip_adjusted = nip / nip_seasonality,
         units_adjusted = units / units_seasonality)

plot.data = sales_data %>% filter(product_no == "10828")
qplot(x = month_year, y = nip_adjusted, data = plot.data) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(plot.data$nip_adjusted)

sales_data_annual = sales_data %>%
  filter(nip >= 0) %>%
  group_by(product_no, super_theme, year) %>% 
  summarise(
    avg_monthly_nip = sum(nip) / sum(nip_seasonality),
    avg_monthly_units = sum(units) / sum(units_seasonality),
    annual_nip = sum(nip),
    annual_units = sum(units)) %>%
  mutate(year_id = as.numeric(paste(product_no, year, sep = "")))

#validation chart#

set.seed(2)
test = sample(sales_data_annual$product_no, 10)
test.plot = sales_data_annual %>% filter(product_no %in% test)

ggplot(test.plot, aes(x = year, y = avg_monthly_nip)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ product_no, scales = "free")

# COMBINING ANNUAL SALES & PRODUCT #

product.sales.annual.data = left_join(
  product_data_comb, sales_data_annual, by = "year_id") %>%
  filter(!is.na(avg_monthly_nip)) %>%
  select(-44:-46) %>%
  rename(year = year.x, product_no = product_no.x,
         theme = Theme.x) %>%
  select(32, 1:31, 33:47)

names(product.sales.annual.data) =
  gsub(" ", "_", names(product.sales.annual.data))

product.sales.annual.data = product.sales.annual.data %>%
  distinct()

##IMPUTATTION##

#Completely missing columns: raw_product_data$`Country Area`
na.counter = function(x) {
  counts = NULL
  for (i in 1:dim(x)[2]) {
    Column = names(x)[i]
    NA_Count = sum(is.na(x[,i]))
    NA_Percent = percent(sum(is.na(x[,i]))/dim(x)[1])
    counts = rbind(counts, data.frame(Column, NA_Count, NA_Percent))
  }
  return(counts)
}

na.counter(product.sales.annual.data)
# imputing with theme means for VBS columns

#problem part 1: use map instead of for loop to iteratively 

co_names = c("Total_Score", "Product_Attributes", "Marketing_Support", 
             "Theme_Relevance", "Theme_Execution", "Play_Promise",
             "Visual_Appeal")
final.list = NULL
unique_productnos = unique(product.sales.annual.data$product_no)
list_values = c(1:2, 4:7)

for (i in 1:length(unique_productnos)) {  
output = product.sales.annual.data %>%
  filter(product_no == unique_productnos[i])
for(j in list_values) {
    need.fix = output %>% filter_(paste("is.na(", co_names[j],")", sep = ""))
    repl.value = output %>% filter_(paste("!is.na(", co_names[j],")", sep = ""))
    if(length(need.fix$product_no) == 0) {
      output2 = repl.value
    } else if (length(repl.value$product_no) == 0) {
      output2 = need.fix
    } else {
      need.fix[ , 34 + j] = repl.value[ , 34 + j] 
      output2 = rbind(need.fix, repl.value)
    }
    output = output2
    }
final.list = rbind(final.list, output)
}

product.sales.annual.data = final.list

x = length(unique(product.sales.annual.data$Super_Theme))

total.score.means = c()
product.attribute.means = c()
marketing.support.means = c() #### CAN IMPUTE BASED ON AVG O or C product status?
theme.relevance.means = c()
theme.execution.means = c()
play.promise.means = c()
visual.appeal.means = c()

for (i in 1:x) {
  output = product.sales.annual.data %>%
    filter(Super_Theme == unique(product.sales.annual.data$Super_Theme)[i])
  
  total.score.means[[i]] = summary(output$Total_Score)[[4]]
  total.score.means[is.na(total.score.means)] = 
    summary(product.sales.annual.data$Total_Score)[[4]]
  
  product.attribute.means[[i]] = summary(output$Product_Attributes)[[4]]
  product.attribute.means[is.na(product.attribute.means)] = 
    summary(product.sales.annual.data$Product_Attributes)[[4]]
  
  marketing.support.means[[i]] = summary(output$Marketing_Support)[[4]]
  marketing.support.means[is.na(marketing.support.means)] = 
    summary(product.sales.annual.data$Marketing_Support)[[4]]
  
  theme.relevance.means[[i]] = summary(output$Theme_Relevance)[[4]]
  theme.relevance.means[is.na(theme.relevance.means)] = 
    summary(product.sales.annual.data$Theme_Relevance)[[4]]
  
  theme.execution.means[[i]] = summary(output$Theme_Execution)[[4]]
  theme.execution.means[is.na(theme.execution.means)] = 
    summary(product.sales.annual.data$Theme_Execution)[[4]]
  
  play.promise.means[[i]] = summary(output$Play_Promise)[[4]]
  play.promise.means[is.na(play.promise.means)] = 
    summary(product.sales.annual.data$Play_Promise)[[4]]
  
  visual.appeal.means[[i]] = summary(output$Visual_Appeal)[[4]]
  visual.appeal.means[is.na(visual.appeal.means)] = 
    summary(product.sales.annual.data$Visual_Appeal)[[4]]
  
  }

product.sales.annual.imp = NULL

for (i in 1:x) {
  output = product.sales.annual.data %>%
    filter(Super_Theme == unique(product.sales.annual.data$Super_Theme)[i])
  
  output$Total_Score[
    is.na(output$Total_Score)] = total.score.means[i]
  
  output$Product_Attributes[
    is.na(output$Product_Attributes)] = product.attribute.means[i]
  
  output$Marketing_Support[
    is.na(output$Marketing_Support)] = marketing.support.means[i]
  
  output$Theme_Relevance[
    is.na(output$Theme_Relevance)] = theme.relevance.means[i]
  
  output$Theme_Execution[
    is.na(output$Theme_Execution)] = theme.execution.means[i]
  
  output$Play_Promise[
    is.na(output$Play_Promise)] = play.promise.means[i]
  
  output$Visual_Appeal[
    is.na(output$Visual_Appeal)] = visual.appeal.means[i]
  
  product.sales.annual.imp =
    rbind(product.sales.annual.imp, output)
  }

summary(product.sales.annual.imp)  #any NAs?

na.counter(product.sales.annual.imp)

#other NAs#

product.sales.annual.imp$Embargo_Date[
  is.na(product.sales.annual.imp$Embargo_Date)] = as.Date("2000-01-01")

product.sales.annual.imp$LPT_ID[
  is.na(product.sales.annual.imp$LPT_ID)] = "0000000000"

product.sales.annual.imp$Lead_Planning_Theme[
  is.na(product.sales.annual.imp$Lead_Planning_Theme)] = "missing"

product.sales.annual.imp$Restrict_Street[
  is.na(product.sales.annual.imp$Restrict_Street)] = "N"

product.sales.annual.imp$Novelty_Launch[
  is.na(product.sales.annual.imp$Novelty_Launch)] = as.Date("2000-01-01")

product.sales.annual.imp$Global_Launch_Status[
  is.na(product.sales.annual.imp$Global_Launch_Status)] = "missing"

product.sales.annual.imp$RRP_Local_Price[
  is.na(product.sales.annual.imp$RRP_Local_Price)] = 0.00

product.sales.annual.imp$movie_tie_in[
  is.na(product.sales.annual.imp$movie_tie_in)] = "no_movie"

product.sales.annual.imp = product.sales.annual.imp %>% 
  left_join(unique_elements.data, by = "product_no") %>%
  select(-47:-49, -51)

product.sales.annual.imp %<>% rename(num_parts = num_parts.x)
  
product.sales.annual.imp$num_parts[
  is.na(product.sales.annual.imp$num_parts)] = 1

write.csv(x = product.sales.annual.imp, file = "annual.product.sales.csv")

####### CREATE MONTHLY THEME DATA SET ######

### David's code revised ###

d = read_excel("annual.product.sales.revisedbyDRv2.xlsx")

d = d %>% left_join(unique_elements.data, by = "product_no") 

n = dim(d)[1]

mylist = list()
j = 0
for (i in 1:n){
  print(i/n)
  dt = d[i,]
  for (i_year in 1992:2018){
    for (i_month in 1:12){
      if(i_year > dt$launchyear &
         i_year < dt$exityear &
         i_year == dt$activeyear) {
        dt$monthly_date = paste0(i_month,".",i_year)
        j = j + 1
        mylist[[j]] = dt
      } else if (
        i_year == dt$launchyear &
        i_year < dt$exityear &
        i_month >= dt$launchmonth &
        i_year == dt$activeyear) { 
        dt$monthly_date = paste0(i_month,".",i_year) 
        j = j + 1
        mylist[[j]] = dt
      } else if (
        i_year == dt$launchyear &
        i_year == dt$exityear &
        i_month >= dt$launchmonth &
        i_month <= dt$exitmonth &
        i_year == dt$activeyear) { 
        dt$monthly_date = paste0(i_month,".",i_year) 
        j = j + 1
        mylist[[j]] = dt
      } else if (
        i_year > dt$launchyear &
        i_year == dt$exityear &
        i_month <= dt$exitmonth &
        i_year == dt$activeyear) { 
        dt$monthly_date = paste0(i_month,".",i_year) 
        j = j + 1
        mylist[[j]] = dt
        }
    }
  }
}


nlist = length(mylist)
D = mylist[[1]]
for (i in 2:nlist){
  print(i/nlist)
  D[i,] = mylist[[i]]
}

for (i in 1:dim(D)[1]) {
  if (nchar(D[i, dim(D)[2]]) == 6) {
    D[i, dim(D)[2]] = paste0(0,D[i, dim(D)[2]])
    }
}

View(D %>% filter(product_no == 9449))

product_data_monthly = D %>% 
  mutate(month_year_id = paste0(product_no, monthly_date)) %>% 
  select(54, 1:53)

# join product with sales #

sales_data_joinprep = 
  sales_data %>%
  mutate(month_year_id = paste0(product_no, month_year))

product.sales.monthly.data = 
  product_data_monthly %>% 
  left_join(sales_data_joinprep, by = "month_year_id") %>% 
  filter(!is.na(product_no.y)) %>% 
  select(-2:-7,-49:-52, -55:-57, -60:-64) %>% 
  rename(year = year.x, product_no = product_no.x)

product.sales.monthly.data$movie_tie_in[
  product.sales.monthly.data$movie_tie_in ==
    unique(product.sales.monthly.data$movie_tie_in)[1]] = 0

product.sales.monthly.data$movie_tie_in[
  product.sales.monthly.data$movie_tie_in !=
    unique(product.sales.monthly.data$movie_tie_in)[1]] = 1

#imputation for monthly data#

na.counter(product.sales.monthly.data)

product.sales.monthly.data$num_parts[
  is.na(product.sales.monthly.data$num_parts)] = 1

#Feature Engineering#

product.sales.monthly.data = data.frame(product.sales.monthly.data)
cols = c("Price_Point", names(product.sales.monthly.data[30]), "New_Novelty",
         "Status", "movie_tie_in")
product.sales.monthly.data %<>% mutate_at(cols, funs(factor(.)))

product.sales.monthly.data.dm =
  dummy.data.frame(product.sales.monthly.data, 
                   dummy.classes = "factor")

product.sales.monthly.data.dm =
  product.sales.monthly.data.dm %>% 
  mutate(month_year = 
           as.yearmon(
             product.sales.monthly.data$monthly_date, "%m.%Y"))

write.csv(x = product.sales.monthly.data.dm, file = "product_portfolio_item_data.csv")

product.data.2017_2018 = product.sales.monthly.data.dm %>% filter(year >= 2017)
write.csv(x = product.data.2017_2018, file = "product_portfolio_item_data - 2017 & 2018.csv")

#aggregating to theme level#

data.short = product.sales.monthly.data.dm

data.short = data.short %>% select(-month_year_id,-sap_product_no, -Group, -Product_BU, -Product_Group, -Global_Status_Year_ID,
                      -Global_Marketing_Exit_Date, -Market_Status_ID, -Product_Category, -Lead_Planning_Theme,
                      -Global_Launch_Date, -Local_Launch_Date, -Local_Exit_Date, -Embargo_Date,
                      -Super_Category, -LPT_ID, -Restrict_Street, -Novelty_Launch, -Global_Launch_Status) 

data.short %>% group_by(Market_Product_Category) %>% summarise(count = n())

product_categories = c("Standard Retail")

data.short.sr.ka = data.short %>%
  filter(Market_Product_Category == product_categories)

#validation
theme.sales.monthly.test = data.short.sr.ka %>% 
  group_by(Super_Theme, month_year, Super_Segment) %>%
  summarise(n = n())
View(theme.sales.monthly.test)

View(product.sales.monthly.data.dm %>%
       filter(Super_Theme == "Ninjago"))

########################################################
theme.sales.monthly = data.short.sr.ka %>% 
  group_by(Super_Theme, month_year, Super_Segment) %>%
  summarise(n = n(),
            retail_price = mean(Retail_Price),
            rm_per = mean(USD_RM.),
            pp9.99 = sum(`Price_Point$0 - $9.99`)/n,
            pp19.99 = sum(`Price_Point$10.00 - $19.99`)/n,
            pp29.99 = sum(`Price_Point$20.00 - $29.99`)/n,
            pp49.99 = sum(`Price_Point$30.00 - $49.99`)/n,
            pp59.99 = sum(`Price_Point$50.00 - $59.99`)/n,
            pp79.99 = sum(`Price_Point$60.00 - $79.99`)/n,
            pp99.99 = sum(`Price_Point$80.00 - $99.99`)/n,
            pp100 = sum(`Price_Point$100 +`)/n,
            IP = sum(IP_._NIPIP)/n,
            NIP = sum(IP_._NIPNIP)/n,
            not_new_novelty = sum(New_NoveltyN)/n,
            new_novelty = sum(New_NoveltyY)/n,
            continuing = sum(StatusContinuing)/n,
            novelty = sum(StatusNovelty)/n,
            promotional = sum(StatusPromotional)/n,
            outgoing = sum(StatusOutgoing)/n,
            vbs.total_score = mean(Total_Score),
            vbs.product_attributes = mean(Product_Attributes),
            vbs.theme_relevance = mean(Theme_Relevance),
            vbs.theme_execution = mean(Theme_Execution),
            vbs.play_promise = mean(Play_Promise),
            vbs.visual_appeal = mean(Visual_Appeal),
            vbs.marketing = mean(Marketing_Support),
            movie_tie_in = sum(movie_tie_in1)/n,
            unique_elements = mean(num_parts),
            nip_wk_avg = sum(nip)/n,
            units_wk_avg = sum(units)/n) %>% 
  mutate(season = if (month(month_year) %in% c(11, 12)) {
              1
            } else {
              0
            },
         random = sample(n, 1))

#total theme monthly
total.sales.monthly = data.short.sr.ka %>% 
  group_by(month_year, ) %>% 
  summarise(total_nip = sum(nip),
            avg_nip = sum(nip)/n())

write.csv(x = theme.sales.monthly, file = "theme.sales.monthly.csv")
write.csv(x = total.sales.monthly, file = "total.sales.monthly.csv")

#isolating top themes

View(data.short.sr.ka %>%
  filter(year(month_year) == 2017) %>% 
  group_by(Super_Theme, Super_Segment) %>% 
  summarise(nip = sum(nip)) %>% arrange(desc(nip)))

top.number = 14
top.themes = data.short.sr.ka %>%
  filter(year(month_year) == 2017) %>% 
  group_by(Super_Theme, Super_Segment) %>% 
  summarise(nip = sum(nip)) %>% arrange(desc(nip)) %>% 
  "["(1:top.number, ) %>%
  select(1)

duplo.themes = data.short.sr.ka %>%
  filter(year(month_year) == 2017, Super_Segment == "Preschool") %>% 
  select(6) %>% unique() %>% select(1)

select.themes = c(top.themes[[1]], duplo.themes[[1]])

theme.sales.monthly.topthemes = theme.sales.monthly %>% 
  filter(Super_Theme %in% select.themes)

write.csv(x = theme.sales.monthly.topthemes, file = "theme.sales.monthly.topthemes.csv")
  
################################################################
# VISUALIZATION
#
#

#EDA - check variances and trends. Could transform variables
#       or do feature engineering...such as lag
#       if very variable average aren't rep or if skewed...everything
#       should be normal.


#avg nip trend: going down...

a = total.sales.monthly %>%
  ggplot() +
  geom_smooth(aes(x = month_year, y = avg_nip),
              method = "lm", se = FALSE) +
  geom_smooth(aes(x = month_year, y = avg_nip), se = FALSE)
  
b = total.sales.monthly %>%
  ggplot() +
  geom_smooth(aes(x = month_year, y = total_nip),
              method = lm, se = FALSE, colour = "red") +
  geom_smooth(aes(x = month_year, y = total_nip), se = FALSE,
              colour = "red")

plot_grid(a, b, nrow = 2)

#check lm results

ggplot(train.data, aes(x = vbs.total_score, y = nip_wk_avg)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(train.data, aes(x = vbs.total_score, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot(train.data, aes(x = pp100, y = nip_wk_avg)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(train.data, aes(x = pp100, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot(train.data, aes(x = pp9.99, y = nip_wk_avg)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(train.data, aes(x = pp9.99, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot(train.data, aes(x = IP, y = nip_wk_avg)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(train.data, aes(x = IP, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot(train.data, aes(x = novelty, y = nip_wk_avg)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(train.data, aes(x = novelty, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

train.data %>%
  filter(
    Super_Theme %in% c("LEGO City", "LEGO Friends", "LEGO Creator",
                       "Star Wars", "Super Heroes")) %>% 
  ggplot(aes(x = novelty, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

train.data %>%
  filter(
    Super_Theme %in% c("LEGO City", "LEGO Friends", "LEGO Creator",
                       "Star Wars", "Super Heroes")) %>% 
  ggplot(aes(x = pp100, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

train.data %>%
  filter(
    Super_Theme %in% c("LEGO City", "LEGO Friends", "LEGO Creator",
                       "Star Wars", "Super Heroes")) %>% 
  ggplot(aes(x = pp100, y = nip_wk_avg, col = Super_Theme)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

pcaclusters.summary %>% ggplot(aes(x = avg_price, y = avg_sales)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

pcaclusters.summary %>%
  filter(IP > .5) %>% 
  ggplot(aes(x = avg_price, y = avg_sales)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

pcaclusters.summary %>%
  filter(Super_Segment == "boys") %>% 
  ggplot(aes(x = avg_price, y = avg_sales)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

  
#################################################################
# MODELING
#
#

theme.sales.monthly.topthemes <- 
  read_csv("~/NYCDSA Training/RML/RML Project/theme.sales.monthly.topthemes.csv")

#Random Forest for feature selection
cols = c("Super_Theme", "Super_Segment")

train.data = theme.sales.monthly.topthemes %>% 
  dplyr::select(-1, -units_wk_avg, -month_year) %>% data.frame() %>%
  mutate_at(cols, funs(factor(.)))

set.seed(0)
rf.fit = randomForest(nip_wk_avg ~ .,
                      data = train.data,
                      importance = TRUE, 
                      ntree = 2500)
rf.fit
importance(rf.fit)
varImpPlot(rf.fit)

#sorted importance table
imp = importance(rf.fit)
order_MSE = order(imp[,1],decreasing = T)
imp.2 = data.frame(imp)
imp.df = data.frame(variables = rownames(imp.2)[order_MSE],
                    MSE = imp[order_MSE, 1])
imp.df

rf.variables.2 = "outgoing + IP + unique_elements + rm_per + novelty +
retail_price + pp19.99 + n + vbs.total_score + vbs.marketing +
vbs.theme_relevance + movie_tie_in + vbs.play_promise + pp9.99 +
vbs.visual_appeal + vbs.theme_execution + vbs.product_attributes +
pp100 + Super_Segment + promotional + pp29.99 +
pp59.99 + pp79.99 + NIP"


############ USE XGBOOST OR REGULAR GRADIENT BOOSTING? #####

############ CREATE HISTOGRAM ################

#Lasso for feature selection

rf.variables
#Dropping the intercept column and included all variables more important than "random"
x = model.matrix(nip_wk_avg ~ outgoing + IP + unique_elements + rm_per + novelty +
                   retail_price + pp19.99 + n + vbs.total_score + vbs.marketing +
                   vbs.theme_relevance + movie_tie_in + vbs.play_promise + pp9.99 +
                   vbs.visual_appeal + vbs.theme_execution + vbs.product_attributes +
                   pp100 + Super_Segment + promotional + pp29.99 +
                   pp59.99 + pp79.99 + NIP, train.data)[, -1] 
y = train.data$nip_wk_avg
grid = 10^seq(5, -2, length = 100)

#Approach 1. for choosing best lambda: elbow of plot

lasso.fit = glmnet(x, y, alpha = 1, lambda = grid)
lasso.fit
plot(lasso.fit, xvar = "lambda", label = TRUE, main = "Lasso Regression")
plot(lasso.fit)
coef(lasso.fit)[, 80]
plot(x = lasso.fit$lambda, y = lasso.fit$dev.ratio)
plot(x = lasso.fit$df, y = lasso.fit$dev.ratio) #useful
plot(x = lasso.fit$lambda, y = lasso.fit$df)

lasso.fit$lambda[11] #extract lambda
coef(lasso.fit)[, 11]  #how do I extract the variables with non-zero coefs?

lasso.fit$lambda[24] #extract lambda
coef(lasso.fit)[, 24]  #how do I extract the variables with non-zero coefs?

differences = "n, pp9.99, vbs.play_promise, Super_Segment,
promotional, pp29.99, NIP" 

#Approach 2. to ID best Lambda: lowest MSE using CV

train.data.shuffled =
  train.data[sample(nrow(train.data)), ]

train.index = sample(nrow(train.data),
                     .7*nrow(train.data))

train.x.cv = x[train.index, ]
train.y.cv = y[train.index]
test.x.cv = x[-train.index, ]
test.y.cv = y[-train.index]

lambda.results = data.frame(matrix(ncol=2, nrow=30))
n = 1000
for (i in 1:n) {
cv.lasso.fit =
  cv.glmnet(train.x.cv, train.y.cv,
          lambda = grid, alpha = 1, nfolds = 10)
best.lambda = cv.lasso.fit$lambda.min
lambda.results[i,1] = best.lambda
lasso.predict = predict(lasso.fit, s = best.lambda, newx = test.x.cv)
lambda.results[i, 2] = mean((lasso.predict - test.y.cv)^2)
print(i/n)
}

best.best.lambda = 
  lambda.results %>% arrange(X2) %>% filter(X2 == min(X2)) %>% 
  select(1) %>% slice(1)

lasso.fit.bestL = 
  glmnet(x, y, alpha = 1, lambda = best.best.lambda[[1]])
coef(lasso.fit.bestL)


lasso.coefs = list()
list.index = 0
for (i in seq_along(coef(lasso.fit.bestL))) {
  if (abs(coef(lasso.fit.bestL)[i]) > 0) {
    list.index = list.index + 1
    lasso.coefs[[list.index]] = coef(lasso.fit.bestL)[, 1][i]
    }
}
lasso.coefs

# MLR backward stepwise feature selection #
model.empty = lm(nip_wk_avg ~ 1, data = train.data)
model.full = lm(nip_wk_avg ~ outgoing + IP + unique_elements +
                  novelty + pp19.99 + vbs.total_score +
                  movie_tie_in + vbs.play_promise +
                  vbs.visual_appeal + vbs.product_attributes +
                  pp100 + Super_Segment + pp9.99 + n,
                data = train.data)
scope = list(lower = formula(model.empty), upper = formula(model.full))
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

# MLR #

lm.fit = lm(nip_wk_avg ~ movie_tie_in + pp9.99 + novelty + n +
              outgoing + pp100 + unique_elements +
              vbs.product_attributes + IP + Super_Segment +
              vbs.total_score,
            data = train.data)
summary(lm.fit)

plot(lm.fit)
vif(lm.fit)
influencePlot(lm.fit)
avPlots(lm.fit)

#Results:  
# 1. pp100 has the largest impact on sales, but p-value is almost too high
# 2. vbs scores are negatively related to sales or have no relationship
# 3. Higher the % of outgoing items, the lower the avg sales
# 4. Higher % IPs results in higher avg sales
# 5. % novelty, movie_tie_in, unique_elements and pp9.99 are 
#      positively correlated with sales, but have p values
#      slightly below 0.05

# Principal Component Analysis

pca.train.data = train.data %>% select(-1:-3, -32) %>% scale() %>% 
  data.frame()
map(pca.train.data, mean) #confirming normalization
map(pca.train.data, sd) #confirming normalization

fa.parallel(pca.train.data)  #select k = 8 due to elbow near 1 eigenvalue on fa.parallel chart
pca.output = principal(pca.train.data,
                       nfactors = 8,
                       rotate = "none")
pca.output
attributes(pca.output)

pca.output
pca.output$scores

pca.lm.fit2 =
  lm(train.data$nip_wk_avg ~ pca.output$scores[, 1] + 
       pca.output$scores[, 2] + pca.output$scores[, 3] +
       pca.output$scores[, 4] + pca.output$scores[, 5] + 
       pca.output$scores[, 6] + pca.output$scores[, 7] + 
       pca.output$scores[, 8])

summary(pca.lm.fit2)

#PCA interpretation: 74% of variance. PC4 53%
#PC1 = vbs, $99.99, $100 (same $100 as PC2)
#PC2 = IP, price related: retail price, -$9.99, -$19.99, $49.99, $79.99,
#       pp100
#PC3 = vbs (2nd)
#PC4 = new novelty, -outgoing
#PC6 = continuing
#PC5 = continuing, 19.99, movie_tie_in
#PC7 = novelty

pca.output$scores

pca.lm.fit2 =
  lm(train.data$nip_wk_avg ~ pca.output$scores[, 1] + 
       pca.output$scores[, 2] + pca.output$scores[, 3] +
       pca.output$scores[, 4] + pca.output$scores[, 5] + 
       pca.output$scores[, 6] + pca.output$scores[, 7] + 
       pca.output$scores[, 8])

summary(pca.lm.fit2)

# Results
# 1. vbs no definitive relationship or invesrse to sales
# 2. higher pp have a positive relationship to sales while lower
#      pp have an inverse relationship
# 3. novelty and new novelty strong positive relationship,
#      outgoing negative
# 4. unique_elements have somewhat positive relatinoship with sales
#      (1 of 2 PCs)
# 5. $19.99 positively related to sales?

#prcomp - approach #2
prcomp.output = prcomp(pca.train.data, rank = 8)
prcomp.output

pca.lm.fit =
  lm(train.data$nip_wk_avg ~ prcomp.output$x[, 1] + 
     prcomp.output$x[, 2] + prcomp.output$x[, 3] +
     prcomp.output$x[, 4] + prcomp.output$x[, 5] + 
     prcomp.output$x[, 6] + prcomp.output$x[, 7] + 
     prcomp.output$x[, 8])
   
summary(pca.lm.fit)


#COMBINED conclusions from PCA and MLR after feature selection
# 1. Higher % novelty/new novelty and lower % outgoing is significantly associated with higher sales
#     - outgoing lack of inventory and shipment problem?
# 2. PCA shows definite relationship between high pps and sales
#     MLR reinforces $100
# 3. vbs has no relationship to avg theme sales
# 4. high % IP are related to sales
# 5. movie_tie_in and unique elements have a less definite 
#     positive relationship with sales
# 6. Tony finding:  if replacing high pp, double up on low pps to compensate
#                    
# These explain 33% of monthly sales variance and have very 
# low p-values.
# Notes: - future steps: PP lag effects does high % low pp result in higher total performance
#         later within the same season. LT implications
#       - scale avg sales by year to eliminate trend  
#           use CER or inflation index to scale?

#cluster analysis

train.data.scaled = pca.train.data
#id number of centers
center.max = 20
rss.vector = NULL
for (i in 1:center.max) {
  rss.vector[i]=
    kmeans(train.data.scaled, centers = i)$tot.withinss
}

plot(1:center.max, rss.vector)

#10 centers is the low, 14 is lower
cluster10 =
  kmeans(train.data.scaled, centers = 10, nstart = 100)

train.data.clusters =
  train.data %>% mutate(clusters = cluster10$cluster)
glimpse(train.data.clusters)

View(train.data.clusters)
                      
#clustering based on PCA: only PCs per David's reco re issues with
#  clustering at high dimensiontality

center.max = 20
rss.vector = NULL
for (i in 1:center.max) {
  rss.vector[i]=
    kmeans(pca.output$scores[, 1:4], centers = i)$tot.withinss
}

plot(1:center.max, rss.vector)

cluster.pca.center6 =
  kmeans(pca.output$scores[, 1:4], centers = 6, nstart = 100)

fviz_nbclust(pca.output$scores, kmeans, method = "wss") #6 or 8
fviz_nbclust(pca.output$scores, kmeans, method = "silhouette") #6 or 9

#clusters:
# 1: NIP DUPLO, Classic, & Creator / lowest pps / med negative PC2 =
#      Lowest performing
# 2: DUPLO IP, 2014 Minecraft / low vbs & pps / high negative PC1 =
#     2nd lowest performing
# 3: Minecraft x 2018 / lowest vbs, highest pp / med neg PC1, high PC2 =
#     highest performing
# 4: Girls IP DUPLO & system...some SH / med vbs & pp / -2:3 across all 4 = 
#     4th highest performer
# 5: mix of IP & NIP system / high vbs & price / high pos PC1 = 
#     2nd highest performer
# 6: novelty / avg pp & vbs / high pos PC4, med neg PC1, med pos PC3 =
#     3rd highest


train.data.pcaclusters =
  train.data %>% mutate(clusters = cluster.pca.center6$cluster)

pcaclusters.summary = train.data.pcaclusters %>% group_by(clusters) %>% 
  summarise(n = n(),
            avg_sales = mean(nip_wk_avg),
            avg_vbs = mean(vbs.total_score),
            avg_price = mean(retail_price),
            novelty = mean(novelty),
            IP = mean(IP))

glimpse(train.data.pcaclusters)
View(train.data.pcaclusters)

#heirarchical clustering: not very useful at obs level.  Too many

train.data.dist = dist(train.data.scaled)

#hclust = hclust(train.data.dist, method = "single")
hclust = hclust(train.data.dist, method = "complete")
#hclust = hclust(train.data.dist, method = "average")

plot(hclust)

clusters.average = cutree(hclust, k = 16)
table(clusters.average)

plot(hclust, hang = -1, main = "Dendrogram of Average Linkage")
rect.hclust(hclust, k = 16)

fviz_nbclust(train.data.scaled, hcut, method = "wss")
fviz_nbclust(train.data.scaled, hcut, method = "silhouette",
             k.max = 50)

#hclust based on pca
train.data.pcadist = dist(pca.output$scores)
train.data.pcadist

hclustpca = hclust(train.data.pcadist, method = "average")

plot(hclustpca)

clusters.average.pca = cutree(hclustpca, k = 8)
table(clusters.average.pca)

plot(hclustpca, hang = -1, main = "Dendrogram of Average Linkage")
rect.hclust(hclustpca, k = 8)

fviz_nbclust(pca.output$scores, hcut, method = "wss")
fviz_nbclust(pca.output$scores, hcut, method = "silhouette",
             k.max = 10)
#Range of Silhouette Interpretation
#0.71-1.0	A strong structure has been found
#0.51-0.70	A reasonable structure has been found
#0.26-0.50	The structure is weak and could be artificial
#< 0.25	No substantial structure has been found


train.data.bothclusters =
  train.data %>% mutate(kmeans.clusters = cluster.pca.center6$cluster,
                        hier.clusters = clusters.average.pca)

train.data.bothclusters %>% group_by(hier.clusters) %>% 
  summarize(n = n(),
            avg_sales = mean(nip_wk_avg),
            avg_vbs = mean(vbs.total_score),
            avg_price = mean(retail_price),
            novelty = mean(novelty),
            IP = mean(IP))

columns = c(5, 15, 20, 23, 31, 33, 32)

aggregate(train.data[, columns], 
          by = list(cluster = clusters.average.pca), mean)


##################################################################
# Modeling By Theme
#
#

# %deviance explained for each theme
theme.rf.deviance = list()
for (i in seq_along(top.10)) {
  cols = c("Super_Theme", "Super_Segment")
  train.data = theme.sales.monthly %>% 
    select(-units_wk_avg) %>% data.frame() %>%
    filter(Super_Theme == top.10[i]) %>% 
    mutate_at(cols, funs(factor(.)))
  theme.rf.deviance[[i]] = randomForest(nip_wk_avg ~ .,
                                        data = train.data,
                                        importance = TRUE)
}

names(theme.rf.deviance) = top.10
theme.rf.deviance
varImpPlot(rf.list[[3]])

#importance results by theme
theme.rf.results = list()
for (i in seq_along(top.10)) {
  cols = c("Super_Theme", "Super_Segment")
  train.data = theme.sales.monthly %>% 
    select(-units_wk_avg) %>% data.frame() %>%
    filter(Super_Theme == top.10[i]) %>% 
    mutate_at(cols, funs(factor(.)))
  fit = randomForest(nip_wk_avg ~ .,
                     data = train.data,
                     importance = TRUE)
  imp = importance(fit)
  order_MSE = order(imp[,1],decreasing = T)
  imp.2 = data.frame(imp)
  imp.df = data.frame(variables = rownames(imp.2)[order_MSE],
                      MSE = imp[order_MSE, 1])
  theme.rf.results[[i]] = imp.df
}

names(theme.rf.results) = top.10
theme.rf.results



theme.lm = list()
for (i in seq_along(top.themes)) {
  cols = c("Super_Theme", "Super_Segment")
  train.data = theme.sales.monthly %>% 
    select(-units_wk_avg) %>% data.frame() %>%
    filter(Super_Theme == top.themes[i]) 
  lm.fit.theme = lm(nip_wk_avg ~ movie_tie_in + n + novelty +
                      outgoing + unique_elements + pp100 + vbs.total_score +
                      rm_per + vbs.product_attributes + pp9.99,
                    data = train.data)
  theme.lm[[i]] = summary(lm.fit.theme)
}
glimpse(train.data)

names(theme.lm) = top.themes
theme.lm  #most theme results aren't stat significant, 
#but there are similar patterns
