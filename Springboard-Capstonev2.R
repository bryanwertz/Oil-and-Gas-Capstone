library(dplyr)
library(tidyr)
library(ggplot2)

prod_data <- read.csv(file = "Prod_Time_Series.csv")
prod_headers <- read.csv(file = "Prod_Headers.csv")

prod_table <- left_join(prod_headers, prod_data, by = "API.UWI")

prod_table <- tbl_df(prod_table)

# data wrangling steps
prod_table <- prod_table %>% 
  select(API.UWI, Operator.Alias.x, Drill.Type, Gross.Perforated.Interval, Monthly.Production.Date, Daily.Avg.Oil, Daily.Avg.Gas) %>% 
  rename(API = API.UWI) %>% 
  mutate(API = API/1000) %>% 
  rename(Operator = Operator.Alias.x) %>% 
  rename(Lat.Len = Gross.Perforated.Interval) %>% 
  rename(Prod.Date = Monthly.Production.Date) %>% 
  rename(Oil.Rate = Daily.Avg.Oil) %>%  
  rename(Gas.Rate = Daily.Avg.Gas) %>% 
  mutate(Prod.Date = as.Date(Prod.Date, format = "%m/%d/%Y"))

# calculation of new variables
prod_table <- prod_table %>% 
  filter(!is.na(Lat.Len)) %>% 
  filter(Lat.Len >= 4000) %>% 
  mutate(Norm.Oil.Rate = Oil.Rate/Lat.Len*7500) %>% 
  mutate(Norm.Gas.Rate = Gas.Rate/Lat.Len*7500) %>%
  group_by(API) %>% 
  mutate(Norm.Peak.Oil = max(Norm.Oil.Rate)) %>% 
  mutate(Norm.Peak.Gas = max(Norm.Gas.Rate)) %>% 
  mutate(Peak.Oil = max(Oil.Rate)) %>%
  mutate(Peak.Gas = max(Gas.Rate)) %>%
  mutate(GOR = Peak.Gas/Peak.Oil*1000) %>% 
  mutate(t = dense_rank(Prod.Date)) %>%
  filter(max(t) > 2) %>% 
  mutate(First.Prod = min(Prod.Date)) %>% 
  mutate(First.Prod = format(as.Date(First.Prod),"%Y"))
  
View(prod_table)

# Set up additional dataframe with one entry per well

singlewelldata <- prod_table %>% 
  group_by(API) %>% 
  summarise(Peak.Oil = max(Peak.Oil), Peak.Gas = max(Peak.Gas), Norm.Peak.Oil = max(Norm.Peak.Oil), Norm.Peak.Gas = max(Norm.Peak.Gas),
            First.Prod = min(First.Prod), tmax = max(t), Lat.Len = max(Lat.Len), GOR = max(GOR))

View(singlewelldata)

# EDA Section

# freq of peak oil and gas levels

ggplot(singlewelldata, aes(Peak.Gas)) +
  geom_histogram(binwidth = 200)
ggplot(singlewelldata, aes(Peak.Oil)) +
  geom_histogram(binwidth = 200)

# look at potential linear relationship between peak oil and gas rates

ggplot(singlewelldata, aes(Peak.Oil, Peak.Gas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, fill = NA) +
  xlab("Peak Oil (bbl/d)") +
  ylab("Peak Gas (mcf/d)")
lm_raw <- lm(Peak.Gas ~ Peak.Oil, singlewelldata)
summary(lm_raw)

ggplot(singlewelldata, aes(Peak.Oil, Peak.Gas)) +
  geom_point(aes(color = First.Prod), alpha = 0.4) +
  geom_smooth(aes(color = First.Prod), method = "lm", forumula = y~x, fill = NA) +
  xlab("Peak Oil (bbl/d)") +
  ylab("Peak Gas (mcf/d)")

# determine if GOR is changing over time

ggplot(singlewelldata, aes(First.Prod, GOR)) +
  geom_boxplot() +
  ylim(0,50000)

# Conclusion - Gas peak rates much more skewed than oil peak rates. Relatively consistent oil/gas ratio trends between 2016-2018.

# Vintage analysis. Create additional dataframe for vintage analysis.

vintagedata <- prod_table %>% 
  group_by(First.Prod, t) %>% 
  summarise(Oil.Rate = mean(Oil.Rate, na.rm = TRUE), Gas.Rate = mean(Gas.Rate, na.rm = TRUE), 
            Norm.Oil.Rate = mean(Norm.Oil.Rate, na.rm = TRUE), Norm.Gas.Rate = mean(Norm.Gas.Rate, na.rm = TRUE))

# Understand sample size in each year
singlewelldata %>% count(First.Prod)

# plot time-normalize production grouped by vintage
ggplot(na.omit(vintagedata), aes(t, Oil.Rate, color = First.Prod)) +
  geom_smooth() +
  xlab("Months on Production") +
  ylab("Oil Rate (bbl/d)")
ggplot(na.omit(vintagedata), aes(t, Gas.Rate, color = First.Prod)) +
  geom_smooth() +
  xlab("Months on Production") +
  ylab("Gas Rate (mcf/d)")

# Understand average lateral length trend by year
ggplot(singlewelldata, aes(First.Prod, Lat.Len)) +
  geom_boxplot() +
  ylab("Lateral Length (ft)")

# Conclusion - more variability in 2017 data, higher rates on both oil and gas that settle back down to 2016 rates.
# 2018 gas rates trending higher than 2017. This may be attributable to the increasing lateral length.

# Seasonality analysis

# pick random well to examine
well <- sample(prod_table$API, 1)
seasondata <- filter(prod_table, API == well)

ggplot(na.omit(seasondata), aes(Prod.Date, Oil.Rate)) +
  geom_line()

# Conclusion - seasonality is difficult to measure in this application because of the declining nature of the wells
# and the limited data points throughout time. A dip in the winter may be expected operationally, but with monthly
# data it is difficult to validate and identify trends.

# Study the impact of lateral length on production, if not 1:1, see if GOR has any correlation

ggplot(singlewelldata, aes(Lat.Len, Peak.Gas)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, fill = NA) +
  xlab("Lateral Length (feet)") +
  ylab("Peak Gas Rate (mcf/d)")
  

ggplot(singlewelldata, aes(Lat.Len, Peak.Oil)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, fill = NA) +
  xlab("Lateral Length (feet)") +
  ylab("Peak Oil Rate (bbl/d)")

lmOil <- lm(Peak.Oil ~ Lat.Len, singlewelldata)
summary(lmOil)
lmGas <- lm(Peak.Gas ~ Lat.Len, singlewelldata)
summary(lmGas)

lmOilGOR <- lm(Peak.Oil ~ Lat.Len + GOR, singlewelldata)
summary(lmOilGOR)
lmGasGOR <- lm(Peak.Gas ~ Lat.Len + GOR, singlewelldata)
summary(lmGasGOR)

# Bucket singlewelldata by GOR group

ggplot(singlewelldata, aes(GOR)) +
  geom_histogram(binwidth = 1000) +
  xlab("GOR (cf/bbl)") +
  xlim(0,50000)

# add GOR bucket tag to singlewelldata dataframe
GOR1000 <- singlewelldata %>% 
  filter(GOR < 2000) %>%
  mutate(GORtag = "<2000") %>% 
  select(API, GORtag)
GOR3000 <- singlewelldata %>% 
  filter(GOR <= 4000 & GOR > 2000) %>% 
  mutate(GORtag = "2000-4000") %>% 
  select(API, GORtag)
GOR5000 <- singlewelldata %>% 
  filter(GOR <= 6000 & GOR > 4000) %>% 
  mutate(GORtag = "4000-6000") %>% 
  select(API, GORtag)
GOR6000plus <- singlewelldata %>% 
  filter(GOR > 6000) %>% 
  mutate(GORtag = ">6000") %>% 
  select(API, GORtag)
GORtags <- bind_rows(GOR1000, GOR3000)
GORtags <- bind_rows(GORtags, GOR5000)
GORtags <- bind_rows(GORtags, GOR6000plus)
singlewelldata <- left_join(singlewelldata, GORtags, by = "API")
singlewelldata$GORtag <- as.factor(singlewelldata$GORtag)

# Count outcomes in each bucket as a check
singlewelldata %>% count(GORtag)

# plot peak rates with linear models for each GOR bucket
ggplot(na.omit(singlewelldata), aes(Lat.Len, Peak.Oil, color = GORtag)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y~x, fill = NA) +
  xlab("Lateral Length (feet)") +
  ylab("Peak Oil Rate (bbl/d)")

ggplot(na.omit(singlewelldata), aes(Lat.Len, Peak.Gas, color = GORtag)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y~x, fill = NA) +
  xlab("Lateral Length (feet)") +
  ylab("Peak Gas rate (mcf/d)")

# Reset bucketed data frames to do lm's
GOR1000 <- singlewelldata %>% 
  filter(GOR < 2000)
GOR3000 <- singlewelldata %>% 
  filter(GOR <= 4000 & GOR > 2000)
GOR5000 <- singlewelldata %>% 
  filter(GOR <= 6000 & GOR > 4000)
GOR6000plus <- singlewelldata %>% 
  filter(GOR > 6000)

lmOil1000 <- lm(Peak.Oil ~ Lat.Len, GOR1000)
lmOil3000 <- lm(Peak.Oil ~ Lat.Len, GOR3000)
lmOil5000 <- lm(Peak.Oil ~ Lat.Len, GOR5000)
lmOil6000plus <- lm(Peak.Oil ~ Lat.Len, GOR6000plus)
summary(lmOil1000)
summary(lmOil3000)
summary(lmOil5000)
summary(lmOil6000plus)

lmGas1000 <- lm(Peak.Gas ~ Lat.Len, GOR1000)
lmGas3000 <- lm(Peak.Gas ~ Lat.Len, GOR3000)
lmGas5000 <- lm(Peak.Gas ~ Lat.Len, GOR5000)
lmGas6000plus <- lm(Peak.Gas ~ Lat.Len, GOR6000plus)
summary(lmGas1000)
summary(lmGas3000)
summary(lmGas5000)
summary(lmGas6000plus)

#Conclusion - length scaling does not seem to be 1:1 - look at summaries of lm's