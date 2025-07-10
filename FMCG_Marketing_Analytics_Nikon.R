#Project Scope: FMCG Marketing Analytics
#Marina Nikonchuk

#Objectives:
#1.	To identify which Channel partners responded and who to target
#first in the next planned campaign (EDA/Statistical Modeling/ML)
#2.	To identify the most effective communication channel/s for them 
#3.	To analyze the responses collected from end consumers and see 
#if minor tweaks can be done to improve the product 


#Install the packages if required and call for the corresponding library
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("knitr")
install.packages("car")
install.packages("shiny")

library(dplyr)
library(ggplot2)
library(gmodels)
library(knitr)
library(car) #vif
library(caret) #confusionMatrix


#------------------------------
# Phase 1 
# 1.	Understand all data sets
#------------------------------

# Load the CampaignDetails data
CampaignDetails<-read.csv(file.choose(), header = TRUE)
head(CampaignDetails) # View first 6 rows
dim(CampaignDetails) # Check the dimension of the dataset
summary(CampaignDetails) #Summarizing data and checking for missing values
colnames(CampaignDetails)
str(CampaignDetails) # Check the structure of the dataset
anyNA(CampaignDetails) # Check for missing values

#Observations:
#There are no missing values
# Since mean < median for email, sms, and call, it suggests a 
#left-skewed (negatively skewed) distribution.
# Since max > 3rd Quartile for email, sms, and call, it suggests a 
#right-skewed (positively skewed) distribution.

# Check unique values
unique(CampaignDetails$email)
unique(CampaignDetails$sms)
unique(CampaignDetails$call)


# Load the CampaignResponse data
CampaignResponse<-read.csv(file.choose(), header = TRUE)
head(CampaignResponse) # View first 6 rows
dim(CampaignResponse) # Check the dimension of the dataset
summary(CampaignResponse) #Summarizing data and checking for missing values
colnames(CampaignResponse)
str(CampaignResponse) # Check the structure of the dataset
anyNA(CampaignResponse) # Check for missing values

#Observations:
#There are no missing values, but there are some categorical variables
#that should be converted into "factor" variables
# Since mean < median for n_comp, loyalty, portal, it suggests a 
#left-skewed (negatively skewed) distribution.
# Since max > 3rd Quartile for n_comp, nps, and n_yrs, it suggests a 
#right-skewed (positively skewed) distribution. 
#response has a median of 0 and mean of 0.4007, meaning most responses
#are likely to be 0 (non-response).
#rewards  has a median of 0 and mean of 0.4422, meaning most responses
#are likely to be 0 (non-response).


# Check unique values
unique(CampaignResponse$response)
unique(CampaignResponse$loyalty)
unique(CampaignResponse$portal)
unique(CampaignResponse$rewards)
unique(CampaignResponse$n_comp)
unique(CampaignResponse$nps)
unique(CampaignResponse$n_yrs)

#Convert numerically coded categorical variables (portal, 
# rewards, loyalty) into factors
CampaignResponse$response<-as.factor(CampaignResponse$response)
CampaignResponse$loyalty<-as.factor(CampaignResponse$loyalty)
CampaignResponse$portal<-as.factor(CampaignResponse$portal)
CampaignResponse$rewards<-as.factor(CampaignResponse$rewards)

str(CampaignResponse)


# Load the Region data
Region<-read.csv(file.choose(), header = TRUE)
head(Region) # View first 6 rows
dim(Region) # Check the dimension of the dataset
summary(Region) #Summarizing data and checking for missing values
colnames(Region)
str(Region) # Check the structure of the dataset
anyNA(Region) # Check for missing values

# Check unique values
unique(Region$Region)
#Observations:
#There are no missing values, but Region
#should be converted into "factor" variables

#Convert Region into factors
Region$Region<-as.factor(Region$Region)
str(Region)

#Check how many Channel Partners belong to each region 
table(Region$Region)
#Observations:
# East North South  West 
#22450 22537 22501 22512
# The distribution is quite balanced
# North has highest number of partners, and the East has the lowest


# Load the Transaction data 
TransactionData<-read.csv(file.choose(), header = TRUE)
head(TransactionData) # View first 6 rows
dim(TransactionData) # Check the dimension of the dataset
summary(TransactionData) #Summarizing data and checking for missing values
colnames(TransactionData)
str(TransactionData) # Check the structure of the dataset
anyNA(TransactionData) # Check for missing values

# Check unique values
unique(TransactionData$Month)
unique(TransactionData$Year)
unique(TransactionData$Brand)
unique(TransactionData$Sales)

#Observations:
#There are no missing values, but some categorical variables
#(Month, Year, Brand) should be converted into "factor" variables

#Convert categorical variables into factors
TransactionData$Month<-as.factor(TransactionData$Month)
TransactionData$Year<-as.factor(TransactionData$Year)
TransactionData$Brand<-as.factor(TransactionData$Brand)

str(TransactionData)

#--------------------------------------------
# 2.	Create master data for further analysis
#--------------------------------------------

# Merge Campaign Response Data and Campaign Details and call it master1
master1<-merge(CampaignResponse, CampaignDetails, by="ChannelPartnerID", all.x = T)
head(master1)


# Merge master1 and Region and call it as master2
master2<-merge(master1, Region, by="ChannelPartnerID", all.x = TRUE)
head(master2)


# Derive one variable at a time from transaction data and then merge
# with the previous master data.

# Total sales in 2021 for each Channel Partner
TotalSales_2021<-aggregate(Sales~ChannelPartnerID, 
                           data = TransactionData[TransactionData$Year == "2021",], sum) # filter by Year = 2021, then aggregate
colnames(TotalSales_2021)[2]<- "TotalSales2021" # Rename column for clarity

# Merge with master2 and call it as master3
master3<-merge(master2, TotalSales_2021, by="ChannelPartnerID", all.x = TRUE)
head(master3)


# Total sales in 2022 for each Channel Partner
TotalSales_2022<-aggregate(Sales~ChannelPartnerID, 
                           data = TransactionData[TransactionData$Year == "2022",], sum) # filter by Year = 2022, then aggregate
# Rename column for clarity
colnames(TotalSales_2022)[2]<- "TotalSales2022" 

# Merge with master3 and call it as master4
master4<-merge(master3, TotalSales_2022, by="ChannelPartnerID", all.x = TRUE)

# Replace NA (partners who did not sale in 2022) with 0
master4$TotalSales2022[is.na(master4$TotalSales2022)]<- 0
head(master4)


# Brand B1 sales in 2022 for each Channel Partner
B1_sales_2022 <- aggregate(Sales~ChannelPartnerID, 
                          data = TransactionData[TransactionData$Brand == "B1" & TransactionData$Year == "2022",], sum) # filter by Brand B1 & Year = 2022, then aggregate

# Rename column for clarity
colnames(B1_sales_2022)[2]<- "B1_sales2022" 

  
# Merge with master4 and call it as master5
master5<-merge(master4, B1_sales_2022, by="ChannelPartnerID", all.x = TRUE)

# Replace NA (partners who did not buy Brand B1 in 2022) with 0
master5$B1_sales2022[is.na(master5$B1_sales2022)]<- 0
head(master5)


#	Brand B1 contribution to total sales in 2022
master5$B1_contribution2022<-(master5$B1_sales2022 / master5$TotalSales2022) * 100
head(master5)


# Buying Frequency - number of unique months of purchase in 2022
Buying_Frequency<-aggregate(Month~ChannelPartnerID,
                            data = TransactionData[TransactionData$Year == "2022",],
                            function(x) length(unique(x)))
colnames(Buying_Frequency)[2]<-"Buying_Frequency_2022"
head(Buying_Frequency)

# Merge Buying Frequency (unique months of purchase in 2022) 
master6<-merge(master5, Buying_Frequency, by="ChannelPartnerID", all.x = TRUE)
head(master6)


# Brand Engagement - number of unique brands purchase in 2022
Brand_Engagement<-aggregate(Brand~ChannelPartnerID,
                            data = TransactionData[TransactionData$Year == "2022",],
                            function(x) length(unique(x)))
colnames(Brand_Engagement)[2]<-"Brand_Engagement_2022"
head(Brand_Engagement)

# Merge Brand Engagement (unique brands purchase in 2022)
master7<-merge(master6, Brand_Engagement, by="ChannelPartnerID", all.x = TRUE)
head(master7)


# Buying Frequency for brand B1 in 2022 by each Channel Partner
Buying_Frequency_B1<-aggregate(Month~ChannelPartnerID,
                            data = TransactionData[TransactionData$Year == "2022" & 
                                                     TransactionData$Brand == "B1",],
                            function(x) length(unique(x)))
colnames(Buying_Frequency_B1)[2]<-"Buying_Frequency_B1_2022"
head(Buying_Frequency_B1)


# Merge Buying Frequency for brand B1 in 2022
master8<-merge(master7, Buying_Frequency_B1, by="ChannelPartnerID", all.x = TRUE)
head(master8)


# Active in the last quarter (Yes/No)
Active_Last_Q<-TransactionData %>%
  filter(Year =="2022" & Month %in% c("10", "11", "12")) %>% #Filter purchases in Q4, 2022
  distinct(ChannelPartnerID) %>% #Get unique ChannelPartnerID (only one entry per partner)
  mutate(Active_Last_Q = "Yes")
head(Active_Last_Q)

# Merge active in last quarter (Yes/No)
master9<-merge(master8, Active_Last_Q, by="ChannelPartnerID", all.x = TRUE)
head(master9)

# Replace NA (partners who did not buy in Q4) with "No"
master9$Active_Last_Q[is.na(master9$Active_Last_Q)]<-"No"
head(master9)


# Active in last quarter (Yes/No) for brand B1 by each Channel Partner 
Active_Last_Q_B1<-TransactionData %>%
  filter(Year =="2022" & Month %in% c("10", "11", "12") & Brand == "B1") %>% #Filter purchases in Q4, 2022 for brand B1
  distinct(ChannelPartnerID) %>% #Get unique ChannelPartnerID (only one entry per partner)
  mutate(Active_Last_Q_B1 = "Yes")
head(Active_Last_Q_B1)


# Merge active in last quarter for brand B1 (Yes/No)
master10<-merge(master9, Active_Last_Q_B1, by="ChannelPartnerID", all.x = TRUE)
head(master10)

# Replace NA (partners who did not buy brand B1 in Q4) with "No"
master10$Active_Last_Q_B1[is.na(master10$Active_Last_Q_B1)]<-"No"


# View the merged dataset master10
head(master10)
str(master10)
summary(master10)
dim(master10)
colSums(is.na(master10))  # Count missing values in each column

#Observations:
# 736 (=0) partners did not respond, and 492 (=1) responded on the campaign
#There are some missing values (NA) in the dataset master10
#TotalSales2021: 64 missing values
#B1_contribution2022,Buying_Frequency_2022, Brand_Engagement_2022: 91 missing values each
#In Buying_Frequency_B1_2022 there are 674 missing values (partners 
#never purchased Brand B1 in 2022)


# Check distribution of response variable / how many partners responded
table(master10$response) # Count number of responses (0 and 1) /
#how many channel partners responded
#0   1 
#736 492 

prop.table(table(master10$response)) *100 # Convert to percentages
#       0        1 
#59.93485 40.06515 
#Observation: 
# Imbalance in distribution of response variable does not observed


#Replace NA with 0
#Create a copy of dataset master10 before replacing NAs
master10_clean<-master10

#Use is.na() to check if a value is NA. Then, replace the NA values with 0:
master10_clean[is.na(master10_clean)] <- 0


# Check if NAs are removed
sum(is.na(master10_clean)) # Should return 0 if all NAs are handled

head(master10_clean)
str(master10_clean)
summary(master10_clean)
dim(master10_clean)
colSums(is.na(master10_clean))  # Count missing values in each column

#Observations:
# 736 (=0) partners did not respond, and 492 (=1) responded on the campaign
# It means there is a relatively low engagement rate

# Some partners had no complaints, while others had 5 complaints in last 
# 3 months (min=0, max=5)
# On average there were 2-3 complaints from partners in last 3 months
# (median = 3, mean=2.55)

# About half of the partners participated in loyalty programs, used
# portals or engaged in rewards
# loyalty portal  rewards
#  0:522   0:600   0:685  
#  1:706   1:628   1:543 

# Net Promoter Score is low.
# median = 4, mean = 4.4, means that 50% of respondents
# are below 4 and 50% are above 4.
# 1st Qu.=2, meaning 25% of partners gave 2 or lower
# 3rd Qu.=7, meaning 25% of partners gave 7 or higher

# median=0 for B1_sales2022 and B1_contribution2022 meaning that half of the 
# partners did not buy Brand B1


#--------------------------------------------------------------
# Phase 2 is about data analysis using graphs, tables etc (EDA)
#--------------------------------------------------------------

#__________________________________________________
#Box-Whisker plots for numeric variables by Response
#__________________________________________________
#Box-Whisker plot for NPS by “Response”
boxplot(nps~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), #Different colors for each response category
        main = "Box-Whisker Plot for NPS by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Net Promoter Score (NPS)")

#Observations:
#The box-whisker plot shows that the median NPS is slightly 
#higher for those who responded 'Yes' compared to 'No' response , 
#indicating that partners who responded to the campaign are more 
#satisfied / loyal compared to those who did not respond to the campaign.
#'No' response is distributed symmetrically.
#'Yes' response can be assumed slightly negatively skewed


#Box-Whisker plot for variable n_comp by “Response”
boxplot(n_comp~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Number of complaints\nin last 3 months by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Number of complaints (n_comp)")

#Observations:
#The box-whisker plot shows that the median change in number of complaints 
#in last 3 months is higher for responses 'No' than for 'Yes', 
#indicating that the partners who did not respond to the campaign tend to
#have more complaints compared to those who responded to the campaign.
#Both responses groups are distributed symmetrically.


#Box-Whisker plot for variable n_yrs by “Response”
boxplot(n_yrs~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for number of years\nin business with the company by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Number of years (n_yrs)")

#Observations:
#The box-whisker plot shows that the median change in number of years in business 
#with the company is slightly  higher for response 'Yes' than for 'No', 
#indicating that long-term partners are more likely to respond to the campaign
#Response group with answer "No" is positively skewed
#Both response groups are distributed symmetrically.


#Box-Whisker plot for variable TotalSales2021 by “Response”
boxplot(TotalSales2021~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Total Sales\nin 2021 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Total Sales in 2021")

#Observation:
#The box-whisker plot shows that the median in Total Sales in 2021 
# is slightly  higher for response 'Yes' than for 'No', 
#but this median difference is not clearly noted
#Both response groups are highly positively skewed and include a significant number
#of outliers, meaning that some partners (both who responded and did 
#not responded on the campaign in 2021) sold more then the others


#Box-Whisker plot for variable TotalSales2022 by “Response”
boxplot(TotalSales2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Total Sales\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Total Sales in 2022")

#Observation: 
#The box-whisker plot shows that the median in Total Sales in 2022 
#is slightly  higher for response 'Yes' than for 'No', 
#indicating that customers who responded to the campaign sold slightly more,  
#then those who did not respond. 
#Both response groups are highly positively skewed and include a significant
#number of outliers, meaning that some partners (both who responded 
#and did not responded on the campaign in 2022) sold more then the others.


#Box-Whisker plot for variable B1_sales2022 by “Response”
boxplot(B1_sales2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for B1 Sales\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "B1 Sales in 2022")

#Observation:
#The box-whisker plot shows that the median in B1 Sales in 2022 
#is very close to zero for both response groups, indicating that a 
#large number of the channel partners had little or no B1 sales.
#Both response groups are extremely positively skewed and include a significant
#number of outliers, meaning that some partners have very high B1
#sales compared to majority. It looks that response "Yes" has higher 
#outliers, than response "No".
#There does not appear to be a major difference in B1 Sales distribution, 
#both responses have a large number of low-value sales and a few
#outliers


#Box-Whisker plot for variable B1_contribution2022 by “Response”
boxplot(B1_contribution2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for B1 contribution\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "B1 contribution in 2022")

#Observation:
#The box-whisker plot shows that the median for B1 contribution in 2022 
#is very close to zero, indicating that a large number of the channel 
#partners had little or no B1 contribution.
#Both response groups are extremely positively skewed. Response "No" include a significant
#number of outliers, meaning that some partners have very high B1
#contribution compared to majority.


#Box-Whisker plot for variable Buying_Frequency_2022 by “Response”
boxplot(Buying_Frequency_2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Buying Frequency\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Buying Frequency in 2022")

#Observation:
#The median is similar for both response groups, it suggests that, on
#average, the buying frequency does not differ significantly
#between these two response groups.
#Both groups have a few high-value outliers (above 6 purchases),
#indicating that some channel partners made significantly more
#purchases then the majority.
#Response group with answer "No" is positively skewed
#The spread of buying frequency is nearly identical for both
#responses. The range of values is slightly wider for response "No",
#but the difference is minimal.


#Box-Whisker plot for variable Brand_Engagement_2022 by “Response”
boxplot(Brand_Engagement_2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Brand Engagement\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Brand Engagement in 2022")

#Observation:
#The box-whisker plot shows that the median change in Brand Engagement 
#looks symmetrically for both response groups. And also both responses are 
#distributed symmetrically and identically. Both response groups are positively skewed. 


#Box-Whisker plot for variable Buying_Frequency_B1_2022 by “Response”
boxplot(Buying_Frequency_B1_2022~response, data=master10_clean,
        col= c("lightblue", "lightcoral"), 
        main = "Box-Whisker Plot for Buying Frequency B1\nin 2022 by Response",
        xlab = "Response (0 = No, 1 = Yes)",
        ylab = "Buying Frequency B1 in 2022")

#Observation:
#The box-whisker plot shows that the median in Buying Frequency B1 
#is very close to zero, indicating that a large number of the channel 
#partners had bought little or no at all of B1.
#Both response groups are extremely positively skewed and include a couple of
#outliers, meaning that some partners have bought more B1 
#compared to majority. Both responses are distributed symmetrically
#and identically. Just response "Yes" has only on one outlier
#more, than response "No".


#____________________________________________
#Chi-Square Test (for categorical variables): 
#____________________________________________

#Communication Channel Effectiveness
#Check if email, sms, and call influence response rate
chisq.test(table(master10_clean$email, master10_clean$response))  # Email vs Response
#X-squared = 85.434, df = 4, p-value < 2.2e-16 - Significant relationship
chisq.test(table(master10_clean$sms, master10_clean$response))    # SMS vs Response
#X-squared = 14.758, df = 4, p-value = 0.00523 -No strong relationship
chisq.test(table(master10_clean$call, master10_clean$response))   # Call vs Response
#X-squared = 105.12, df = 5, p-value < 2.2e-16 - Significant relationship


#Chi-Square Test: region-wise response
#Check if response rate varies across different regions
chisq.test(table(master10_clean$Region, master10_clean$response))  
#X-squared = 4.0181, df = 3, p-value = 0.2595 - no strong relationship


#Chi-Square Test: Does loyalty or rewards influence response?
chisq.test(table(master10_clean$loyalty, master10_clean$response)) #Loyalty vs Response
#X-squared = 38.451, df = 1, p-value = 5.614e-10 - Significant relationship
chisq.test(table(master10_clean$rewards, master10_clean$response))   #Rewards vs Response
#X-squared = 6.0943, df = 1, p-value = 0.01356 - Strong relationship


#Chi-Square Test: activity on web
chisq.test(table(master10_clean$portal, master10_clean$response))  
#X-squared = 0.010772, df = 1, p-value = 0.9173 - no strong relationship

#Chi-Square Test: activity last quarter
chisq.test(table(master10_clean$Active_Last_Q, master10_clean$response))  
#X-squared = 0.57312, df = 1, p-value = 0.449 - no strong relationship

#Chi-Square Test: activity last quarter for B1
chisq.test(table(master10_clean$Active_Last_Q_B1, master10_clean$response))  
#X-squared = 0, df = 1, p-value = 1 - no strong relationship


#____________________________________________________
#2.	Table of Response Rate:  for categorical variable 
#____________________________________________________

# N: Number of channel partners with “loyalty = 0/1” 
# N+: Number of channel partners with “Response=1” within “Loyalty=0/1”

library(dplyr)
library(knitr)

# Convert response to numeric
master10_clean$response <- as.numeric(as.character(master10_clean$response))


# Response Rate by email engagement(No. of emails)
response_email <-master10_clean %>%
  group_by(email) %>% # Group by email (0 or 1)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_email, format = "pipe", align = "c") # Print response table
#print(response_email) # Print response table
#Observations:
#  | email |  N  | N_plus |    RR     |
#  |:-----:|:---:|:------:|:---------:|
#  |   0   | 418 |  107   | 25.59809  |
#  |   1   | 737 |  329   | 44.64043  |
#  |   2   | 66  |   49   | 74.24242  |
#  |   3   |  6  |   6    | 100.00000 |
#  |   4   |  1  |   1    | 100.00000 |
# As the groups with large emails number are so small, the response rate may 
#not be reliable. Email Engagement was grouped like:
#Low Engagement - 0 emails
#Medium Engagement - 1 emails
#High Engagement - 2+ emails

#Create temporary column email_engagement in master10_clean dataset
master10_clean$email_engagement <- ifelse(master10_clean$email == 0, "Low",
                                          ifelse(master10_clean$email == 1, "Medium", "High"))

# Response Rate by email
response_email_grouped <-master10_clean %>%
  group_by(email_engagement) %>% # Group by email (0 - 4)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_email_grouped, format = "pipe", align = "c") # Print response table
#print(response_email_grouped) # Print response table
#Observations:
#  | email_engagement |  N  | N_plus |    RR    |
#  |:----------------:|:---:|:------:|:--------:|
#  |       High       | 73  |   56   | 76.71233 |
#  |       Low        | 418 |  107   | 25.59809 |
#  |      Medium      | 737 |  329   | 44.64043 | 

#Create the bar chart for response rate by email
ggplot(response_email_grouped, aes(x = email_engagement, y = RR, fill = email_engagement)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "orange", "High" = "red")) +  
  labs(title = "Response Rate by Email Engagement Level",
       x = "Email Engagement Level",
       y = "Response Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate % on bars
#Observations:
#Higher email engagement correlates with higher response rates (76.7%)
#Partners who received no emails had lowest response rates (25.6%)
#Partners who received 1 email had moderate response rates (44.6%)


# Response Rate by sms (No. of sms)
response_sms <-master10_clean %>%
  group_by(sms) %>% # Group by sms (0 - 4)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_sms, format = "pipe", align = "c") # Print response table

#Observations:
#  | sms |  N  | N_plus |    RR     |
#  |:---:|:---:|:------:|:---------:|
#  |  0  | 416 |  172   | 41.34615  |
#  |  1  | 754 |  285   | 37.79841  |
#  |  2  | 51  |   29   | 56.86275  |
#  |  3  |  4  |   4    | 100.00000 |
#  |  4  |  3  |   2    | 66.66667  |
# As the groups with large sms number are so small, the response rate may 
#not be reliable. SMS Engagement was grouped like:
#Low Engagement - 0 sms
#Medium Engagement - 1 sms
#High Engagement - 2+ sms

#Create temporary column sms_engagement in master10_clean dataset
master10_clean$sms_engagement <- ifelse(master10_clean$sms == 0, "Low",
                                        ifelse(master10_clean$sms == 1, "Medium", "High"))

# Response Rate by sms
response_sms_grouped <-master10_clean %>%
  group_by(sms_engagement) %>% # Group by sms (0 or 4)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_sms_grouped, format = "pipe", align = "c") # Print response table
#Observations:
#  | sms_engagement |  N  | N_plus |    RR    |
#  |:--------------:|:---:|:------:|:--------:|
#  |      High      | 58  |   35   | 60.34483 |
#  |      Low       | 416 |  172   | 41.34615 |
#  |     Medium     | 754 |  285   | 37.79841 |

#Create the bar chart for response rate by sms
ggplot(response_sms_grouped, aes(x = sms_engagement, y = RR, fill = sms_engagement)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "orange", "High" = "red")) +  
  labs(title = "Response Rate by sms Engagement Level",
       x = "Sms Engagement Level",
       y = "Response Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate % on bars
#Observations:
#Higher sms engagement correlates with higher response rates (60.3%)
#Partners who received no sms had moderate response rates (41.3%)
#Partners who received 1 sms had lowest response rates (37.8%)


# Response Rate by call (No. of call)
response_call <-master10_clean %>%
  group_by(call) %>% # Group by call (0 - 5)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_call, format = "pipe", align = "c") # Print response table
#Observations: 
#  | call |  N  | N_plus |    RR     |
#  |:----:|:---:|:------:|:---------:|
#  |  0   | 527 |  159   | 30.17078  |
#  |  1   | 640 |  274   | 42.81250  |
#  |  2   | 47  |   45   | 95.74468  |
#  |  3   | 12  |   12   | 100.00000 |
#  |  4   |  1  |   1    | 100.00000 |
#  |  5   |  1  |   1    | 100.00000 |
# As the groups with large call number are so small, the response rate may 
#not be reliable. Call Engagement was grouped like:
#Low Engagement - 0 call
#Medium Engagement - 1 call
#High Engagement - 2+ call

#Create temporary column call_engagement in master10_clean dataset
master10_clean$call_engagement <- ifelse(master10_clean$call == 0, "Low",
                                         ifelse(master10_clean$call == 1, "Medium", "High"))

# Response Rate by call
response_call_grouped <-master10_clean %>%
  group_by(call_engagement) %>% # Group by call (0 or 5)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_call_grouped, format = "pipe", align = "c") # Print response table
#Observations: 
#  | call_engagement |  N  | N_plus |    RR    |
#  |:---------------:|:---:|:------:|:--------:|
#  |      High       | 61  |   59   | 96.72131 |
#  |       Low       | 527 |  159   | 30.17078 |
#  |     Medium      | 640 |  274   | 42.81250 |

#Create the bar chart for response rate by call
ggplot(response_call_grouped, aes(x = call_engagement, y = RR, fill = call_engagement)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "orange", "High" = "red")) +  
  labs(title = "Response Rate by call Engagement Level",
       x = "Sms Engagement Level",
       y = "Response Rate (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate % on bars
#Observations:
#Higher call engagement correlates with higher response rates (96.7%)
#Partners who received no call had lowest response rates (30.2%)
#Partners who received 1 call had moderate response rates (42.8%)


# Response Rate by Region
response_region<-master10_clean %>%
  group_by(Region) %>% # Group by Region (East, North, South, West)
  summarise(N = n(), # Count total partners in each region
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_region, format = "pipe", align = "c") # Print response table
#Observations:
#  | Region |  N  | N_plus |    RR    |
#  |:------:|:---:|:------:|:--------:|
#  |  East  | 340 |  132   | 38.82353 |
#  | North  | 316 |  130   | 41.13924 |
#  | South  | 274 |   99   | 36.13139 |
#  |  West  | 298 |  131   | 43.95973 |

# Create the bar chart for response rate by region
ggplot(response_region, aes(x = Region, y = RR, fill = Region)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("East" = "lightblue", "North" = "orange", "South" = "lightgreen", "West" = "red")) +
  labs(
    title = "Response Rate by Region",
    x = "Region",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# Response rate varies not significantly by region
#West has the highest response rate (44.0%), suggesting stronger 
#partners engagement in this region 
#South has the lowest response rate (36.1%), indicating lower 
#partners engagement in this region
#North and East have similar response rates (38.8% and 41.1%), 
#meaning engagement is fairly consistent across these regions


# Response Rate by loyalty (member of loyalty program)
response_loyalty <-master10_clean %>%
  group_by(loyalty) %>% # Group by loyalty (0 or 1)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_loyalty, format = "pipe", align = "c") # Print response table

#Observations:
#  | loyalty |  N  | N_plus |    RR    |
#  |:-------:|:---:|:------:|:--------:|
#  |    0    | 522 |  156   | 29.88506 |
#  |    1    | 706 |  336   | 47.59207 |

# Create the bar chart for response rate by loyalty 
ggplot(response_loyalty, aes(x = loyalty, y = RR, fill = loyalty)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen")) +
  labs(
    title = "Response Rate by Loyalty",
    x = "Loyalty",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# Higher Response Rate has partners who are members of loyalty program(Yes=1), (47.6%),
#compared to non-loyal partners (29.9)
# This suggests that loyal partners are more engaged and responsive to interactions.
# Nearly half of the loyal partners responded (336 out of 706)
# But only about 30% of non-loyal partners responded (156 out of 522)


# Response Rate by rewards (redeemed reward points last month)
# Convert response to numeric
response_rewards <-master10_clean %>%
  group_by(rewards) %>% # Group by rewards (0 or 1)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_rewards, format = "pipe", align = "c") # Print response table
#Observations:
# | rewards |  N  | N_plus |    RR    |
# |:-------:|:---:|:------:|:--------:|
# |    0    | 685 |  296   | 43.21168 |
# |    1    | 543 |  196   | 36.09576 |

# Create the bar chart for response rate by rewards 
ggplot(response_rewards, aes(x = rewards, y = RR, fill = rewards)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen")) +
  labs(
    title = "Response Rate by Rewards",
    x = "Rewards",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# Unrewarded partners have higher Response Rate (43.2%)
# This suggests that partners, who did not redeem reward points
#responded more frequently than those who did redeem rewards (36.1%).


# Response Rate by portal
response_portal <-master10_clean %>%
  group_by(portal) %>% # Group by portal (0 or 1)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_portal, format = "pipe", align = "c") # Print response table
#print(response_portal) # Print response table
#Observations:
# | portal |  N  | N_plus |    RR    |
# |:------:|:---:|:------:|:--------:|
# |   0    | 600 |  239   | 39.83333 |
# |   1    | 628 |  253   | 40.28662 |

# Create the bar chart for response rate by portal 
ggplot(response_portal, aes(x = portal, y = RR, fill = portal)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen")) +
  labs(
    title = "Response Rate by portal",
    x = "Portal",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# Partners who is active on web portal (40.3%) have a slightly higher Response 
#Rate than those who does not use the portal (39.8%).
#The difference is very small (only 0.5%), suggesting that portal usage 
#does not significantly impact Response Rates.


# Response Rate by activity in last quarter
response_Active_Last_Q<-master10_clean %>%
  group_by(Active_Last_Q) %>% # Group by activity in last quarter ( Yes/No)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_Active_Last_Q, format = "pipe", align = "c") # Print response table
#Observations:
#  | Active_Last_Q |  N  | N_plus |    RR    |
#  |:-------------:|:---:|:------:|:--------:|
#  |      No       | 614 |  239   | 38.92508 |
#  |      Yes      | 614 |  253   | 41.20521 |

# Create the bar chart for response rate by activity in last quarter
ggplot(response_Active_Last_Q, aes(x = Active_Last_Q, y = RR, fill = Active_Last_Q)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("No" = "orange", "Yes" = "lightgreen")) +
  labs(
    title = "Response Rate by activity in last quarter",
    x = "Activity in last quarter",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# 41.2% of partners were active in last quarter
# 38.9% of partners were not active in last quarter


# Response Rate by activity for brand B1 in last quarter
response_Active_Last_Q_B1<-master10_clean %>%
  group_by(Active_Last_Q_B1) %>% # Group by activity in last quarter for brand B1 ( Yes/No)
  summarise(N = n(), # Count total partners
            N_plus = sum(response), # Count partners with response = 1
            RR = (N_plus / N) * 100) # Calculate response rate, %

kable(response_Active_Last_Q_B1, format = "pipe", align = "c") # Print response table
#Observations:
#  | Active_Last_Q_B1 |  N   | N_plus |    RR    |
#  |:----------------:|:----:|:------:|:--------:|
#  |        No        | 1067 |  427   | 40.01874 |
#  |       Yes        | 161  |   65   | 40.37267 |

# Create the bar chart for response rate by activity for brand B1 in last quarter
ggplot(response_Active_Last_Q_B1, aes(x = Active_Last_Q_B1, y = RR, fill = Active_Last_Q_B1)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart with borders
  scale_fill_manual(values = c("No" = "orange", "Yes" = "lightgreen")) +
  labs(
    title = "Response Rate by activity for brand B1 in last quarter",
    x = "Activity for brand B1",
    y = "Response Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(RR, 1), "%")), vjust = -0.5, size = 5)  # Add response rate labels
#Observations:
# 40.4% of partners were active in last quarter
# 40.0% of partners were not active in last quarter
#Percentage of activity and non-activity for brand B1 in last
#quarter is almost equal between active and non active partners


#________________
# 3.	Bar Charts
#________________

# Which communication channel (email, sms, call) was most effective
em_Resp <- table(master10_clean$email_engagement, master10_clean$response)
colnames(em_Resp) <-c("Not Responded", "Responded")
print(em_Resp)
#Observations:
#       Not Responded Responded
#High              17        56
#Low              311       107
#Medium           408       329


#Compute row-wise proportion (percentage)
prop.table(em_Resp, margin = 1) * 100
#Observations:
#       Not Responded Responded
#High        23.28767  76.71233
#Low         74.40191  25.59809
#Medium      55.35957  44.64043
#Response rates increase with higher email engagement

sms_Resp <- table(master10_clean$sms_engagement, master10_clean$response)
colnames(sms_Resp) <-c("Not Responded", "Responded")
print(sms_Resp)
#Observations:
#       Not Responded Responded
#High              23        35
#Low              244       172
#Medium           469       285

#Compute row-wise proportion (percentage)
prop.table(sms_Resp, margin = 1) * 100
#Observations:
#       Not Responded Responded
#High        39.65517  60.34483
#Low         58.65385  41.34615
#Medium      62.20159  37.79841
#SMS has an inconsistent impact, with less clear differentiation than email.
#Not as effective overall


call_Resp <- table(master10_clean$call_engagement, master10_clean$response)
colnames(call_Resp) <-c("Not Responded", "Responded") # Rename columns for clarity
print(call_Resp)
#Observations:
#       Not Responded Responded
#High               2        59
#Low              368       159
#Medium           366       274

#Compute row-wise proportion
prop.table(call_Resp, margin = 1) * 100
#         Not Responded Responded
#High        3.278689 96.721311
#Low        69.829222 30.170778
#Medium     57.187500 42.812500
#Call engagement is the most effective channel

#...................................................................
#Bar charts to visualize the impact of each channel on response rate
#...................................................................

# Convert response to factor (if not already done)
master10_clean$response <- factor(master10_clean$response, 
                                  levels = c(0, 1), 
                                  labels = c("Not Responded", "Responded"))

#Email plot with discrete colors
ggplot(master10_clean, aes(x = email_engagement, fill = response)) + 
  geom_bar(position = "dodge") +  # Side-by-side bars
  labs(title = "Response Count by email Engagement", 
       x = "Email Engagement Level", 
       y = "Count") +
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))


#Sms plot with discrete colors
ggplot(master10_clean, aes(x = sms_engagement, fill = response)) + 
  geom_bar(position = "dodge") +  # Side-by-side bars
  labs(title = "Response Count by SMS Engagement", 
       x = "SMS Engagement Level", 
       y = "Count") +
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))


#Call plot with discrete colors
ggplot(master10_clean, aes(x = call_engagement, fill = response)) + 
  geom_bar(position = "dodge") +  # Side-by-side bars
  labs(title = "Response Count by call Engagement", 
       x = "Call Engagement Level", 
       y = "Count") +
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))
#Observations:
#Calls are the most effective (96.7% response rate for high engagement)
#Emails are the second most effective (76.7% response rate for high engagement).
#Sms has a lower response rate overall


#How response rate vary by region
Reg_table <- table(master10_clean$Region, master10_clean$response)
colnames(Reg_table) <-c("Not Responded", "Responded") # Rename columns for clarity
print(Reg_table)
#Observations:
#      Not Responded Responded
#East            208       132
#North           186       130
#South           175        99
#West            167       131

#Compute row-wise proportion
prop.table(table(master10_clean$Region, master10_clean$response), margin = 1) * 100
#Observations:
#      Not Responded Responded
#East       61.17647  38.82353
#North      58.86076  41.13924
#South      63.86861  36.13139
#West       56.04027  43.95973
#West has the highest response rate (44%). South has the lowest (36%)

#To visualize:
  ggplot(master10_clean, aes(x = Region, fill = response)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Response Rate by Region", x = "Region", y = "Proportion")+
    scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                                 "Responded" = "lightblue"))
#Observations:
#Differences are not extreme. West and North regions perform slightly better

  
#Does customer loyalty or rewards impact response?
loyal_table <- table(master10_clean$loyalty, master10_clean$response)
colnames(loyal_table) <-c("Not Responded", "Responded") # Rename columns for clarity
print(loyal_table)
#Observations:
#    Not Responded Responded
#0           366       156
#1           370       336


rewar_table <- table(master10_clean$rewards, master10_clean$response)
colnames(rewar_table) <-c("Not Responded", "Responded") # Rename columns for clarity
print(rewar_table)
#Observations:
#   Not Responded Responded
# 0           389       296
# 1           347       196  
  
#To visualize:
ggplot(master10_clean, aes(x = loyalty, fill = response)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Response Rate by Loyalty", x = "Loyalty", y = "Proportion")+
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))

ggplot(master10_clean, aes(x = rewards, fill = response)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Response Rate by Rewards", x = "Rewards", y = "Proportion")+
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))
#Observations:  
#User without rewards responded at a higher rate  

#Do customers with higher NPS respond more?
nps_table <- table(master10_clean$nps, master10_clean$response)
colnames(nps_table) <-c("Not Responded", "Responded") # Rename columns for clarity
print(nps_table)
#Observations:
#   Not Responded Responded
#0             45        24
#1             73        55
#2            128        41
#3            109        53
#4             82        37
#5             81        44
#6             81        56
#7             73        48
#8             64        56
#9              0        51
#10             0        27

ggplot(master10_clean, aes(x = nps, fill = response)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Response Rate by NPS", x = "NPS", y = "Proportion")+
  scale_fill_manual(values = c("Not Responded" = "lightcoral", 
                               "Responded" = "lightblue"))
#Observations:
#Low NPS (0-5) shows lower response rates. High NPS (8-10) has much higher
#response rates. 9 and 10 are 100% responded. It means engaged customers
#are more likely to respond  
  
  

###If the data is skewed, a median is a better representation than mean


# Bar Chart: Median Buying Frequency by Response
#Calculate median Buying Frequency for each response group
buying_freq_median<-master10_clean%>%
  group_by(response)%>%
  summarise(Median_Buying_Freq = median(Buying_Frequency_2022, na.rm = TRUE))

#Create bar chart
ggplot(buying_freq_median,aes(x=as.factor(response), y=Median_Buying_Freq, fill = as.factor(response))) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("lightblue", "lightcoral") )+
  labs(title = "Median Buying Frequency by Response ",
       x = "Response (0=No, 1=Yes)",
       y = "Median Buying Frequency",
       fill = "Response") +
  theme_minimal()
#Observation:
#The buying frequency does not differ between these two response groups.


# Bar Chart: Median Brand Engagement by Response
#Calculate median Brand Engagement for each response group
brand_engagement_median<-master10_clean%>%
  group_by(response)%>%
  summarise(Median_Brand_Engagement = median(Brand_Engagement_2022, na.rm = TRUE))

#Create bar chart
ggplot(brand_engagement_median,aes(x=as.factor(response), y=Median_Brand_Engagement, fill = as.factor(response))) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("lightblue", "lightcoral") )+
  labs(title = "Median Brand Engagement by Response ",
       x = "Response (0=No, 1=Yes)",
       y = "Median Brand Engagement",
       fill = "Response") +
  theme_minimal()
#There is no difference in median Brand Engagement between these two response groups.



#---------------------------------------------------------------------
# Phase 3 is about developing a model using Binary Logistic Regression
#(using final master data)
#---------------------------------------------------------------------

#Estimating the response rate
counts  <-data.frame(table(master10_clean$response))
colnames(counts)[1] <- "Responded"
counts$Percent <- counts$Freq / sum(counts$Freq)*100
counts
#Observation:
#Responded Freq  Percent
#1       0  736  59.93485
#2       1  492  40.06515
#Around 40% of customers responded 

#Set threshold to match the proportion of positive cases. 
#This value will be used as threshold for PredY
threshold <- 0.4 


library(GGally)

# 1.	Create data partition into train and test data sets ( 80/20) 

str(master10_clean) #Check the structure of the dataset

# Ensure all categorical variables are factors
categorical_vars <- c("response", "email_engagement", "sms_engagement", "call_engagement",
                    "Active_Last_Q", "Active_Last_Q_B1")

master10_clean[categorical_vars] <- lapply(master10_clean[categorical_vars], as.factor)


# Drop raw numeric versions as they are duplicated with columns - email_engagement,
#sms_engagement, call_engagement
master10_clean$email <- NULL
master10_clean$sms <- NULL
master10_clean$call <- NULL


#Double check
str(master10_clean)
    

# Convert response into binary factor: 0 = Not Responded, 1 = Responded
master10_clean$response <- factor(ifelse(master10_clean$response == "Responded", 1,0), levels = c(0,1))
library(caret)
#Splitting data into Train and Test data sets
set.seed(123) #to use the same traindata
index<-createDataPartition(master10_clean$response, p=0.8, list=FALSE)
head(index)
dim(index)

traindata<-master10_clean[index,]
testdata<-master10_clean[-index,]

dim(traindata) #dimension of training set
dim(testdata) #dimension of testing set


#____________________________________
# Model 1. Binary Logistic Regression
#____________________________________

#2.	Run Binary Logistic Regression with response as dependent variable
#and all others as independent variables.
master10_clean_model<-glm(response~., data=traindata, family = "binomial")
summary(master10_clean_model)
#Observation:
#Since p-value is <0.05 for loyalty1, rewards1, nps, email_engagementLow,
#sms_engagementLow, call_engagementLow, call_engagementMedium these independent
#variables are statistically significant.
#n_yrs (0.089324) and TotalSales2022 (0.074505) are on a border line, they were retained
#AIC: 929.32 


# 3.	Check which variables are significant (revise the model if needed)
#Rerun the model after removing the insignificant variables
master10_clean_model<-glm(response~loyalty+rewards+nps+n_yrs+TotalSales2022+
                            email_engagement+sms_engagement+call_engagement, data=traindata, family = "binomial")
summary(master10_clean_model)
#Observation:
#Since p-value is <0.05 for all variables, all these independent variables are 
#significant and signs of the coefficients are also logical.
#AIC: 918.82


#4.	Relate results to EDA
#Observation:
#Email, SMS, and Call Engagement: These are strong predictors in both EDA 
#and logistic regression (model), confirming that higher engagement significantly  
#increases the likelihood of a response.

#Region: EDA showed that response rates vary very slightly across 
#regions (36.1%-44%). Region also was not significant in the model 
#(p-value for RegionNorth is 0.175939    
#             RegionSouth is 0.267272    
#              RegionWest is 0.235108)
#Both EDA and the model agree that Region has no strong influence.

#Loyalty, rewards and nps showed significant and strong relationship with 
#response in EDA. All three were confirmed as significant predictors in
#the model, confirming their importance in predicting partner behavior. 

#EDA suggests that there is relationship between higher complaint and lower response_call_grouped
#but the model does not identify n_comp as a significant predictor.

#EDA shows that partners with higher n-yrs are more likely to respond. The model shows
#a slight (borderline), but positive effect of number of years in business on response
#For TotalSales_2022 both EDA and the models confirmed the small effect of higher 
#sales on the response

#The rest of the variables (sales, contribution, frequency and so) also were
#equally confirmed by both EDA and the model that these variables not significantly
#affect response and have no significant or small predictive power


# 5.	Check multicollinearity 
vif(master10_clean_model)          
#Observation: 
# None of the GVIF values are greater than 5, indicating “No Multicollinearity”


# 6.	Obtain ROC curve and AUC for train data
library(ROCR)
traindata$predprob<-predict(master10_clean_model, traindata, type = "response")
predtrain_glm<-prediction(traindata$predprob,traindata$response)
perftrain_glm<-performance(predtrain_glm,"tpr","fpr")
plot(perftrain_glm, col="orchid", main="ROC Curve (Train Data, glm)")
abline(0,1, col="blue")


#Checking area under the ROC curve
auctrain_glm<-performance(predtrain_glm,"auc")
auctrain_glm@y.values
#Observations:
#0.8492886 - good 
#Anything about 70 is a good auc value


# 7.	Obtain confusion matrix and optimum threshold to balance sensitivity and specificity
#Setting the threshold
response_counts<-data.frame(table(master10_clean$response))
colnames(response_counts)[1] <- "Response"
response_counts$Percent <- response_counts$Freq / sum(response_counts$Freq)
#Count the occurrences of each value in the "Response" column
response_counts
#Observations:
#Response Freq   Percent
#1      0  736 0.5993485
#2      1  492 0.4006515

#This value is used as threshold for PredY
threshold <- 0.4


#Predicted Y for train data
traindata$predY_train_glm<-as.factor(ifelse(traindata$predprob>threshold,1,0))

#Confusion matrix for train data    
confusionMatrix(traindata$predY_train_glm,traindata$response,positive="1")
#Observation:
#          Reference
#Prediction   0   1
#         0 475 112
#         1 114 282
#Accuracy : 0.7701
#Sensitivity : 0.7157          
#Specificity : 0.8065

# Accuracy : 0.7701 - means the model correctly predicts 77% of the time on the train set 
# Sensitivity : 0.7157 - the model correctly identifies 72% of responses         
# Specificity : 0.8065 - the model correctly identifies 81% of non-responses        


#Go to step 8 only if you are satisfied with model on train data
# 8.	Obtain ROC curve and AUC for test data( compare with step 6)
testdata$predprob<-predict(master10_clean_model,testdata,type='response')
predtest<-prediction(testdata$predprob,testdata$response)
perftest<-performance(predtest,"tpr","fpr")
plot(perftest, col="cyan3", main="ROC Curve (Test Data, glm)")
abline(0,1, col="blue")

auctest_glm<-performance(predtest,"auc")
auctest_glm@y.values
#Observation:
#0.835277 - good 


# 9.	Use above threshold to obtain sensitivity and specificity for test data(compare with step 7)
#Predicted Y for test data
testdata$predY_test_glm<-as.factor(ifelse(testdata$predprob>threshold,1,0))

#Confusion matrix for test data    
confusionMatrix(testdata$predY_test_glm,testdata$response,positive="1")
cm_glm <- confusionMatrix(testdata$predY_test_glm,testdata$response,positive="1") #store to use for the comparison table
#Observation:
#          Reference
#Prediction   0   1
#         0 118  32
#         1  29  66
# Accuracy : 0.751 - means the model correctly predicts 75% of the time on the TEST set (good)
# Sensitivity : 0.6735 - the model correctly identifies 67% of responses (good)        
# Specificity : 0.8027 - the model correctly identifies 80% of non-responses (good)       
#In conclusion the model performs reasonably well. The performance is
#quite similar between the training and test set.
         
         
# 10.	Finalize the model 
#The finalized logistic regression model includes only statistically
#significant predictors and performs consistently across training
#and testing data, with good AUC and balanced sensitivity/specificity.

#Loyalty, reward participation, and communication engagement 
#(especially email and calls) are key predictors of campaign 
#response. High engagement and loyalty drive higher campaign 
#response. Low communication engagement predicts low likelihood 
#of response.

#The business can use this model to score future customers and 
#prioritize outreach.



#-------------------------------------------------------------------------------
# Phase 4 is about using ML methods and compare with Binary Logistic Regression 
#-------------------------------------------------------------------------------

#1.	Create data partition into train and test data sets ( 80/20)
#(already done for Logistic Regression)

#___________________________
#Model 2. Naïve Bayes Method
#___________________________

#2.	Apply Naïve Bayes Method on train data (with response as dependent variables and all others as independent variables)
install.packages("e1071")
library(e1071)
set.seed(123)
master10_clean_nb<-naiveBayes(response~., data=traindata)
master10_clean_nb


#3.	Obtain ROC curve and AUC for train data
predprob_nb_train<- predict(master10_clean_nb, traindata, type = "raw")

# Computing AUC value for ROC curve
predtrain_nb<-prediction(predprob_nb_train[,2],traindata$response)
perftrain_nb<-performance(predtrain_nb,"tpr","fpr")
plot(perftrain_nb, col="orchid", main="ROC Curve (Train Data, nb)")
abline(0,1, col="blue")


#Checking area under the ROC curve
auctrain_nb<-performance(predtrain_nb,"auc")
auctrain_nb@y.values
#Observations:
# 0.8491938


#4.	Obtain Confusion Matrix for train data

#Predicted Y for train data
traindata$predY_train_nb<-as.factor(ifelse(predprob_nb_train[,2]>threshold,1,0))

#Confusion matrix for train data    
confusionMatrix(traindata$predY_train_nb,traindata$response,positive="1")
#Observations:
#          Reference
#Prediction   0   1
#         0 468 107
#         1 121 287
#Accuracy : 0.7681
#Sensitivity : 0.7284          
#Specificity : 0.7946


#5.	Obtain ROC curve and AUC for test data
predprob_nb_test<-predict(master10_clean_nb,testdata,type='raw')

# Computing AUC value for ROC curve
predtest_nb<-prediction(predprob_nb_test[,2],testdata$response)
perftest_nb<-performance(predtest_nb,"tpr","fpr")
plot(perftest_nb, col="cyan3", main="ROC Curve (Test Data, nb)")
abline(0,1, col="blue")

#Test AUC
auctest_nb<-performance(predtest_nb,"auc")
auctest_nb@y.values
#Observations:
#0.8420103


#6.	Obtain Confusion Matrix for test data
testdata$predY_test_nb<-as.factor(ifelse(predprob_nb_test[,2] > threshold,1,0))
confusionMatrix(testdata$predY_test_nb,testdata$response, positive="1")
cm_nb <- confusionMatrix(testdata$predY_test_nb,testdata$response, positive="1") #store to use for the comparison table
#Observation:
#          Reference
#Prediction   0   1
#         0 122  33
#         1  25  65
#Accuracy : 0.7633
#Sensitivity : 0.6633          
#Specificity : 0.8299
#Observation: 
#The overall model accuracy is 76%. The area under ROC curve is 84.20
#Conclusion: 
#Naïve Bayes method has higher AUC (0.8420) and slightly higher accuracy
#(76.33%), but slightly lower sensetivity (66.33%) compare to Logistic 
#Regression method



#______________________________
#Model 3. Decision Tree Method
#______________________________

# 7.	Repeat steps 3 to 6 for Decision Tree

install.packages("partykit")
library(partykit)

head(master10_clean) # View first 6 rows
str(master10_clean) #Check the structure of the dataset
#Observation:
#According to the structure of the dataset all necessary variables are 
#converted to the factor, so the next step could be omited in the present dataset

#Converting data types (Required for modelling) is not necessary for the present dataset
#data <- data %>%
#  mutate_at(vars(c("response","loyalty","portal","rewards", "Region",
#                   "Active_Last_Q", "Active_Last_Q_B1", "email_engagement",
#                   "sms_engagement", "call_engagement")), ~as.factor(.))


set.seed(123)
master10_clean_ctree<-partykit::ctree(response ~ n_comp + loyalty + portal + rewards + nps + 
                          n_yrs + Region + TotalSales2021 + TotalSales2022 + 
                          B1_sales2022 + B1_contribution2022 + Buying_Frequency_2022 + 
                          Brand_Engagement_2022 + Buying_Frequency_B1_2022 + 
                          Active_Last_Q + Active_Last_Q_B1 + email_engagement + 
                          sms_engagement + call_engagement, data=traindata)
#Visualize the tree
plot(master10_clean_ctree,type="simple", gp=gpar(cex=0.6))
print(master10_clean_ctree)


#3.	Obtain ROC curve and AUC for train data <<<<<<<<<<<<
train_pred_tree <- predict(master10_clean_ctree,traindata,type="prob")

predtrain_tree <- prediction(train_pred_tree[,2], traindata$response)
perftrain_tree<-performance(predtrain_tree,"tpr","fpr")
plot(perftrain_tree, col="violet", main="ROC Curve (Train Data, Dtree)")
abline(0,1, col="blue")

## Area under ROC Curve in R (AUC)
auctrain_tree<-performance(predtrain_tree,"auc")
auctrain_tree@y.values
#Observation:
#0.850256 


#4.	Obtain Confusion Matrix for train data 
traindata$predY_train_tree<- predict(master10_clean_ctree,traindata,type="response")
confusionMatrix(traindata$predY_train_tree,traindata$response,positive="1") #store to use for the comparison table
#Observation:
#           Reference
#Prediction   0   1
#         0  562 156
#         1   27 238
#Accuracy : 0.8138
#Sensitivity : 0.6041          
#Specificity : 0.9542


#5.	Obtain ROC curve and AUC for test data 
test_pred_tree <- predict(master10_clean_ctree,testdata,type="prob")

predtest_tree <- prediction(test_pred_tree[,2], testdata$response)
perftest_tree<-performance(predtest_tree,"tpr","fpr")
plot(perftest_tree, col="cyan", main="ROC Curve (Test Data, Dtree)")
abline(0,1, col="blue")
 
## Area under ROC Curve in R (AUC)
auctest_tree<-performance(predtest_tree,"auc")
auctest_tree@y.values
#Observation:
#0.8173678


#6.	Obtain Confusion Matrix for test data
testdata$predY_test_tree<- predict(master10_clean_ctree,testdata,type="response")
confusionMatrix(testdata$predY_test_tree,testdata$response,positive="1")
cm_tree <- confusionMatrix(testdata$predY_test_tree,testdata$response,positive="1") #store to use for the comparison table
#Observation:
#           Reference
#Prediction   0   1
#         0 136  44
#         1  11  54
#Accuracy : 0.7755
#Sensitivity : 0.5510         
#Specificity : 0.9252 
#Observation: 
#The overall model accuracy is 76%. The area under ROC curve is 81.74
#Conclusion: 
#The Decision Tree model has the highest accuracy(77.55%), but at the same time 
#has a very high specificity (92.52%) and lower sensitivity (55.10%) compare 
#to Logistic Regression method. It could misses many actual responders, which 
#could hurt marketing targeting. Logistic Regression is more balanced
#and practical for outreach.



#_____________________________
#Model 4. Random Forest Method
#_____________________________
  
# 8.	Repeat steps 3 to 6 for Random Forest Method
install.packages("randomForest")
library(randomForest)
set.seed(123)
#Building model Random Forest
master10_clean_rf<-randomForest(response ~ n_comp + loyalty + portal + rewards + nps + 
                          n_yrs + Region + TotalSales2021 + TotalSales2022 + 
                          B1_sales2022 + B1_contribution2022 + Buying_Frequency_2022 + 
                          Brand_Engagement_2022 + Buying_Frequency_B1_2022 + 
                          Active_Last_Q + Active_Last_Q_B1 + email_engagement + 
                          sms_engagement + call_engagement, data=traindata,
                          mtry = 3, ntree=100, importance=TRUE)
master10_clean_rf
#Observation:
#        OOB estimate of  error rate: 22.18%
#Confusion matrix:
#    0   1 class.error
#0 530  59   0.1001698
#1 159 235   0.4035533


#Variable Importance Plot
master10_clean_rf$importance
#Observation:
#Top predictors
#nps                             62.907030
#TotalSales2021                  42.648131
#TotalSales2022                  40.473674
#call_engagement                 38.601068

#Low importance variables #### (matching those excluded from the logistic model)
#portal                           7.963521
#Active_Last_Q                    7.112834
#Buying_Frequency_B1_2022         6.714350
#Active_Last_Q_B1                 3.836360


#Visualize the tree
varImpPlot(master10_clean_rf, col="turquoise3")


#3.	Obtain ROC curve and AUC for train data
predrf_train <- predict(master10_clean_rf,traindata, type = "vote", norm.votes = TRUE)

pred_rf_train<-prediction(predrf_train[,2],traindata$response)
perf_rf_train<-performance(pred_rf_train,"tpr","fpr")
plot(perf_rf_train, col="deeppink", main="ROC Curve (Train Data, RF)")
abline(0,1, col="blue")

# Area under ROC Curve in R (AUC)
auctrain_rf<-performance(pred_rf_train,"auc")
auctrain_rf@y.values
#Observation:
#1


#4.	Obtain Confusion Matrix for train data
traindata$predY_train_rf<- predict(master10_clean_rf,traindata, type="response")
confusionMatrix(traindata$predY_train_rf,traindata$response, positive="1")
#Observation:
#          Reference
#Prediction   0   1
#         0 589   6
#         1   0 388
#Accuracy : 0.9939
#Sensitivity : 0.9848          
#Specificity : 1.0000


#5.	Obtain ROC curve and AUC for test data
predrf_test <- predict(master10_clean_rf,testdata, type = "vote", norm.votes = TRUE)

pred_rf_test<-prediction(predrf_test[,2],testdata$response)
perf_rf_test<-performance(pred_rf_test,"tpr","fpr")
plot(perf_rf_test, col="salmon", main="ROC Curve (Test Data, RF)")
abline(0,1, col="blue")

# Area under ROC Curve in R (AUC)
auctest_rf<-performance(pred_rf_test,"auc")
auctest_rf@y.values
#Observation:
#0.8190337


#6.	Obtain Confusion Matrix for test data
testdata$predY_rf_test<- predict(master10_clean_rf,testdata,type="response")
confusionMatrix(testdata$predY_rf_test,testdata$response,positive="1")
cm_rf <- confusionMatrix(testdata$predY_rf_test,testdata$response,positive="1") #store to use for the comparison table
#Observation:
#          Reference
#Prediction   0   1
#         0 141  49
#         1   6  49
#Accuracy : 0.7755 
#Sensitivity : 0.5000          
#Specificity : 0.9592
#Observation: 
#The overall model accuracy is 76%. The area under ROC curve is 81.90
#Conclusion: 
#The Random Forest model has the high accuracy(77.55%), and very high
#specificity (95.92%) and lowest sensitivity (55.%) compare to Logistic
#Regression method. It could misses many actual responders, which 
#could hurt marketing targeting. Logistic Regression is more balanced
#and practical for outreach.



#9.	Compare AUC for test data in case of 4 methods and finalize the method
# Create a comparison table
auc_comparison <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest"),
  Test_AUC = round (c(auctest_glm@y.values[[1]],
                      auctest_nb@y.values[[1]],
                      auctest_tree@y.values[[1]],
                      auctest_rf@y.values[[1]]),4),
  
  Accuracy = round (c(cm_glm$overall["Accuracy"],
                        cm_nb$overall["Accuracy"],
                        cm_tree$overall["Accuracy"],
                        cm_rf$overall["Accuracy"]),4),
  
  Sensitivity = round(c(cm_glm$byClass["Sensitivity"],
                        cm_nb$byClass["Sensitivity"],
                        cm_tree$byClass["Sensitivity"],
                        cm_rf$byClass["Sensitivity"]),4),
  
  Specificity = round(c(cm_glm$byClass["Specificity"],
                        cm_nb$byClass["Specificity"],
                        cm_tree$byClass["Specificity"],
                        cm_rf$byClass["Specificity"]),4)
)

print(auc_comparison)
#                Model Test_AUC Accuracy Sensitivity Specificity
#1 Logistic Regression   0.8353   0.7510      0.6735      0.8027
#2         Naive Bayes   0.8420   0.7633      0.6633      0.8299
#3       Decision Tree   0.8174   0.7755      0.5510      0.9252
#4       Random Forest   0.8190   0.7755      0.5000      0.9592


#Plot ROC curves for all models on test data
plot(perftest, col="coral", lwd=3, 
     main="ROC Curves with AUC (Test Data)",
     xlab="False Positive Rate",
     Ylab="True Positive Rate",
     cex.main=1.8, cex.lab=1.5)
plot(perftest_nb, col="orchid", lwd=3, add=TRUE)
plot(perftest_tree, col="turquoise", lwd=3, add=TRUE)
plot(perf_rf_test, col="slateblue", lwd=3, add=TRUE)

#Add diagonal reference line
abline(0, 1, lwd=2, col="gray")

#Add legend with AUC values
legend(x=0.7, y = 0.65, #Manual position
       legend = c(
         paste ("Logistic Regression (", round(auctest_glm@y.values[[1]], 3),")"),
         paste ("Naive Bayes (", round(auctest_nb@y.values[[1]], 3), ")"),
         paste ("Decision Tree (", round(auctest_tree@y.values[[1]], 3), ")"),
         paste ("Random Forest (", round(auctest_rf@y.values[[1]], 3), ")")
       ),

       col=c("coral", "orchid", "turquoise", "slateblue"),
lwd=3, #Line thickness
title= expression(bold("AUC Scores")), 
title.adj = 0.2, #Title position ajustment
cex=0.7, #Text size
x.intersp = 0.5, #Horizontal spacing
y.intersp = 0.5, #Vertical spacing
bty = "n", #No legend box
seg.len = 0.6 #Line segment length
)
#Conclusion:
#Logistic Regression offers the best balance of accuracy, AUC, interpretability
#and prediction for both responders and non-responders.



#-------------------------------------------------------
#Phase 5 is about text mining of consumer feedback data
#-------------------------------------------------------

#________________________________________
#1.	Clean the corpus-text pre-processing
#________________________________________

#Import the data
responses <- readLines(file.choose()) 

head(responses)
length(responses)
str(responses)


#Convert "responses" into ‘Corpus’
#install.packages("tm")
library(tm)

#Convert data into corpus
corp <- Corpus(VectorSource(responses))
class(responses) 
class(corp)

## Inspect Corpus (displays first 3 textlines).
inspect(corp[1:3])


#Insert space after punctuation.
#Define a custom function and add space
fix_punctuation_space <-content_transformer(function(x) {
  x <- gsub("([[:punct:]])([A-Za-z])", "\\1 \\2", x)
  x
})

#Apply it to the corpus
corp <- tm_map(corp, fix_punctuation_space)


# Convert Textdata to lowercase.
corp <- tm_map(corp, tolower)

#Remove punctuation from Textdata.
corp <- tm_map(corp, removePunctuation)

#Remove numbers from Textdata.
corp <- tm_map(corp, removeNumbers)


# Remove stop words like: i, me, our and, the, is, etc.
corp <- tm_map(corp, removeWords, stopwords("en"))


# Remove words that are obvious.
custom_stopwords <- c("coffee", "milk", "cup", "cups", "drink", "drinks", "hot", "add", "make", "this")
corp <- tm_map(corp, removeWords, custom_stopwords)
#The words "sugar", "flavor", "taste" were not deleted as it reflects preference and taste of
#coffee, and it is good to keep them for sentiment analysis.

#Remove extra whitespace
corp <- tm_map(corp, stripWhitespace)


#Checking "responses" before and after cleaning
head(responses, 7) #before cleaning
sapply(corp[1:7], as.character) #after cleaning


#________________________________________________________
#2.	Obtain word cloud (exclude obvious words like coffee)
#________________________________________________________

# Install and load package “wordcloud”
#install.packages("wordcloud")
library(wordcloud)


# Convert to term-document matrix format 
tdm <- TermDocumentMatrix(corp)


# Find terms with frequency of at least 10
findFreqTerms(tdm,10)


#Convert tdm object to a matrix
m <- as.matrix(tdm) 
m 


# Calculate the total frequency of words & create a data frame of it
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
head(d)


# Set color palette
#install.packages("RColorBrewer")
library(RColorBrewer)
#display.brewer.all() #palettes
palette <- brewer.pal(8,"Accent")

# Get Word Cloud
set.seed(123)
wordcloud(d$word, d$freq, random.order = FALSE , min.freq = 1, colors=palette)

#Observation:
#Words "taste" and "like" have largest size, indicating most frequent words
#followed by flavor, and then by "chocolate" and "great". 


#Plotting frequent terms as a bar plot
term.freq <- rowSums(m)
term.freq <- subset(term.freq, term.freq >= 5)

# Transform as a dataframe
df <- data.frame(term = names(term.freq), freq = term.freq)

# Horizontal bar plot
#install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = term, y = freq))+ 
  geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


#________________________________
# 3.	Perform Sentiment Analysis
#________________________________

# Install and Load package “sentimentr”
#install.packages("sentimentr")
library(sentimentr)


# Calculate Sentiment Score (by sentences)
sentiment(responses)

# Calculate Avg Sentiment Score (by entire response)
sentiment_by(responses)

# Calculate average sentiment per response
sent_scores <- sentiment_by(responses)

#Combine into one dataframe
sent_scores$response <- responses

# Most positive responses
head(sent_scores[order(-sent_scores$ave_sentiment), c("ave_sentiment", "response")], 5)

# Most negative responses
head(sent_scores[order(sent_scores$ave_sentiment), c("ave_sentiment", "response")], 5)


#View sentiment as Positive / Neutral / Negative
sent_scores$sentiment_tag <- ifelse(sent_scores$ave_sentiment > 0.3, "Positive",
                                 ifelse(sent_scores$ave_sentiment < -0.3, "Negative", "Neutral"))
# View number of each sentiment type
table(sent_scores$sentiment_tag)
#Observations:
#Negative  Neutral Positive 
#       7       45       22 
#The majority of responses (45) are neutral. A small number (7) are negative.


# Bar plot of average sentiment 
ggplot(sent_scores, aes(x = factor(element_id), y = ave_sentiment, fill = ave_sentiment > 0)) +
  geom_col() +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  labs(title = "Average Sentiment per Response",
       x = "Response ID",
       y = "Average Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Hide x labels if too many
#Observations:
#The majority of response are positive, including some very negative and very
#positive sentiments. Some responses have score close to zero, meaning neutral
#or mixed feeling.

# Histogram of average sentiment distribution
sent_scores <- sentiment_by(responses)
hist(sent_scores$ave_sentiment,
     main = "Distribution of Average Sentiment",
     xlab = "Average Sentiment Score",
     col = "lightblue",
     breaks = 20)
#Observations:
#There is a higher frequency of positive sentiment values between 0.1 and 0.5.
#Many responses are near neutral sentiment (0.0) suggesting mixed
#or balanced emotional tone.
#The distribution suggest most user responses are slightly to moderately positive.

#Conclusion:
#Sentiment successfully captures emotional tone. Positive and negative responses
#are sharply differentiated by their emotional content. The majority of responses
#are neutral, suggesting that most respondents provided informative 
#comments, possibly without strong emotion. Negative sentiment is relatively low
#but clearly indicating dissatisfaction that should be addressing.




