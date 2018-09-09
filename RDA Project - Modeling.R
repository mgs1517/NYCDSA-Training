#########################################
# RDA PROJECT                           #   
# - Purpose: modeling                   #
# - Michael Sullivan                    #
# - Feb 2018                            # 
#                                       #
#########################################

library(readr)
final_rda_data <- read_csv("~/final_rda_data.csv")

final_rda_data = final_rda_data %>% select(-1, -3) %>%
  filter(Sales_Channel == "Walmart") %>% select(-1)

summary(final_rda_data)
glimpse(final_rda_data)
class(final_rda_data)

final_rda_data[is.na(final_rda_data)] = 0

#review distribution#

ggplot(final_rda_data,
       aes(x = avg_seasonality, y = sales)) + geom_point() +
  geom_smooth(method = "lm")

hist(final_rda_data$`Brand Paid Search`, breaks = 20)

pairs(vis.data)

# model #

lm.fit.all = lm(
  sales ~ . -avg_seasonality, data = final_rda_data)

summary(lm.fit.all)

