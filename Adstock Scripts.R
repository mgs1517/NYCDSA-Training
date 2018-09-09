####ADSTOCK FUNCTIONS: data prep, IDing optimal adstocks, looping over
####retailers and activities


library(readxl)
LEGO_RDA_Project_Mike_Sullivan <- read_excel(
  "~/NYCDSA Training/RDA Project/RDA_Project-Data_Validation/data/LEGO RDA Project - Mike Sullivan.xlsx",
  sheet = "DUPLO Data",
  col_types = c("text", "text", "text",
                "text", "text", "text", 
                "text", "numeric"))

View(LEGO_RDA_Project_Mike_Sullivan)

###preping sales and marketing data####

sales.data = LEGO_RDA_Project_Mike_Sullivan %>%
  filter(`Variable Type` == "Sales", Week >= "201418") %>%
  select(`Sales Channel`, Week, Data)
sales.data$Week = as.integer(sales.data$Week)

potential.adstocks = seq(from = 0.01, to = 0.8, by = 0.01)

#Function: creating a list of unique weeks

Every.week.generator = function(weekstart.g, weekend.g) {
  every.week = sort(unique(LEGO_RDA_Project_Mike_Sullivan$Week))
  every.week = data.frame(Week = as.integer(every.week)) %>%
    filter(Week >= weekstart.g, Week <= weekend.g)
  return(every.week)
}

#functions preps one activity

Marketing.data.oneactivity = function(
  activity.1, theme.1, weekstart.1 = "201418", weekend.1 = "201717", 
  metricexcluded1.1 = "Spend", metricexcluded2.1 = "spend",
  metricexcluded3.1 = "TRPs", metricexcluded4.1 = "TRP"
  ) {
  marketing.data = LEGO_RDA_Project_Mike_Sullivan %>%
    filter(
      Activity == activity.1, Theme == theme.1,
      Week >= weekstart.1, Week <= weekend.1, 
      Metric != metricexcluded1.1, Metric != metricexcluded2.1,
      Metric != metricexcluded3.1, Metric != metricexcluded4.1 
      ) %>%
    select(7, 2, 3, 6, 8) %>%
  mutate(Week = as.integer(Week))

    every.week = Every.week.generator(weekstart.g = weekstart.1,
                                    weekend.g = weekend.1)
  marketing.data.everyweek = 
    left_join(every.week, marketing.data, by = "Week") %>% 
    mutate(
      Activity = activity.1,
      Metric = if(is.na(unique(Metric)[1])) { 
        unique(Metric)[2]
        } else {
            unique(Metric)[1]
          },
      Data = ifelse(
        is.na(Data), 0, Data
      )
    ) %>%
    filter(Week != "201453")
  return(marketing.data.everyweek)
}


#function: preps all activities

Marketing.data.allactivities = function(
  theme, weekstart = "201418", weekend = "201717", 
  metricexcluded1 = "Spend", metricexcluded2 = "spend",
  metricexcluded3 = "TRPs", metricexcluded4 = "TRP") {
  
  alldata = data.frame()
  marketing.data.1 = 
    LEGO_RDA_Project_Mike_Sullivan %>% filter(
      `Variable Type` == "Marketing" | `Variable Type` == "Marketing "
      )
  for (i in 1:length(unique(marketing.data.1$Activity))) {
    output = Marketing.data.oneactivity(
      theme.1 = theme, 
      activity.1 = unique(marketing.data.1$Activity)[i],
      weekstart.1 = weekstart,
      weekend.1 = weekend,
      metricexcluded1.1 = metricexcluded1,
      metricexcluded2.1 = metricexcluded2,
      metricexcluded3.1 = metricexcluded3,
      metricexcluded4.1 = metricexcluded4
      ) 
    alldata = rbind(alldata, output)
  }
    exclude = alldata %>%
    group_by(Activity) %>% summarise(Data = sum(Data)) %>%
    filter(Data == 0) %>% select(Activity)
  alldata = alldata %>% filter(!(Activity %in% exclude$Activity))
  return(alldata)
}

exclude =  marketing.data.allactivities %>%
  group_by(Activity) %>% summarise(Data = sum(Data)) %>%
  filter(Data == 0) %>% select(Activity)
alldata = marketing.data.allactivities %>%
  filter(!(Activity %in% exclude$Activity))
View(alldata)


##Adstock function that transforms activity##

Adstock.calc = function(act, rate) {
  transformed = act[1]
  for (i in 2:length(act)) {
    y = act[i] + (transformed[i - 1] * rate)
    transformed[i] = y
  }
  transformed = round(as.numeric(format(transformed, scientific = FALSE)), digits = 2)
  return(transformed)
}

##Functions: Identify optimal adstock##

#IDes the one optimal adstock

Adstock.Selector = function(activ, adstock.ops, sales) {
  r_squareds = c()
  for (i in 1:length(adstock.ops)) {
  trans.var = Adstock.calc(
    act = activ, rate = adstock.ops[i])
  lm.summary = summary(lm(sales ~ trans.var))
  r_squareds[i] = lm.summary[[9]]
  }
  Adstock.Optimizer.Output =
    as.data.frame(cbind(potential.adstocks, r_squareds))
  Optimal.Adstocks = Adstock.Optimizer.Output %>%
    filter(r_squareds == max(r_squareds))
  return(Optimal.Adstocks)
  }

#Creates list of rsquareds for all adstock#
Adstocks.List.Creator = function(activ, adstock.ops, sales) {
  r_squareds = c()
  for (i in 1:length(adstock.ops)) {
    trans.var = Adstock.calc(act = activ$Data, rate = adstock.ops[i])
    lm.summary = summary(lm(sales$Data ~ trans.var))
    r_squareds[i] = lm.summary[[9]]
  }
  output = as.data.frame(cbind(potential.adstocks, r_squareds))
  return(output)
}

##Loops "Adstock Optimizer" function (IDs one adstock) over all sales channels

Retailer.Adstock.Loop = function(sales1, marketing1, adstocks1) {
  adstock = c()
  rsquared = c()
  activity = c()
  retailer = c()
  for (i in 1:length(unique(sales1$`Sales Channel`))) {
    sales.by.channel = 
      sales1 %>% filter(`Sales Channel` ==
                        unique(sales1$`Sales Channel`)[i])
    output = 
      Adstock.Selector(activ = marketing1$Data,
                       adstock.ops = adstocks1, 
                       sales = sales.by.channel$Data)
    adstock[i] = output[[1]]
    rsquared[i] = round(output[[2]], digits = 2)
    activity[i] = unique(marketing1$Activity)
    retailer[i] = unique(sales.by.channel$`Sales Channel`)
  }
  adstock.byretailer = cbind(activity, retailer, adstock, rsquared)
  return(adstock.byretailer)
}

#Loops adstock by retailer for all activities
Adstock.Tool = function(sales, marketing, adstocks) {
  finaloutput = data.frame()
  for (i in 1:length(unique(marketing$Activity))) {
    marketing.data = 
      marketing %>% filter(
        Activity == unique(marketing$Activity)[i])
    output = 
      Retailer.Adstock.Loop(
        sales1 = sales, marketing1 = marketing.data, 
        adstocks1 = adstocks)
  finaloutput = rbind(finaloutput, output)
  }
  return(finaloutput)
}

######RUNNING FUNCTIONS & IDENTIFYING OPTIMUM ADSTOCKS FOR ALL ACTIVITIES & RETAILERS#####

#prep marketing data for ALL activities

marketing.data.allactivities = 
  Marketing.data.allactivities(theme = "DUPLO")
marketing.data.allactivities = 
  marketing.data.allactivities %>% filter(Activity != "Ecommerce Digital Display")

###FINAL LIST OF ALL ADSTOCKS BY ACTIVITY AND BY RETAILER###

adstocks.byactivity.byretailer = Adstock.Tool(
  sales = sales.data, 
  marketing = marketing.data.allactivities,
  adstocks = potential.adstocks)
View(adstocks.byactivity.byretailer)


#### TEST RUNS: Function Development ####
marketing.data.1activity = Marketing.data.oneactivity(
  activity.1 = "LEGO Life Magazine", theme.1 = "DUPLO")
View(marketing.data.1activity)

#Adstock transformation example

test.campaign = c(20,20,20,20,0,0,0,0,0,0,0)
test.rate = .5

test.transform = Adstock.calc(test.campaign, test.rate)

View(test.transform)

#Adstock transform on actual data
test.comparison = cbind(marketing.data.1activity$Data,
                        Adstock.calc(marketing.data.1activity$Data, .5))
View(test.comparison)

##Every.week example###

Every.week.generator(201601, 201652)

#identify optimal adstock for ONE activity across all retailers#

Adstocks_LEGOLife = Retailer.Adstock.Loop(sales1 = sales.data,
                                          marketing1 = marketing.data.1activity, 
                                          adstocks1 = potential.adstocks)
View(Adstocks_Display)

###creates a list of all adstocks and rsquareds
retailer =  unique(sales.data$`Sales Channel`)[5]
sales1 = sales.data %>% filter(`Sales Channel` == retailer)

adstock.rsq.list = Adstocks.List.Creator(
  activ = marketing.data.1activity, adstock.ops = potential.adstocks,
  sales = sales1)

title = paste(marketing.data.1activity$Activity, " ", retailer)
ggplot(adstock.rsq.list, aes(x = potential.adstocks, y = r_squareds)) + geom_line() +
  ggtitle(title)
