# Credit risk classification



#Data Import
#File Reader Package Import
install.packages("readr")
library(readr)
# Import data
customer_data=read.csv("C:\\Users\\shani\\OneDrive - Asia Pacific University\\Documents\\Degree\\Semester 1\\Programming for Data Analysis\\assignment\\credit_risk.csv"
                       ,header = TRUE, sep = ",")
customer_data

#converting data type
str(customer_data)

#duplicating data frame
new_data = customer_data

---------------------------------------------------------------------------------------------------------------------------
  #Data Cleaning
  #Assigning column names
  names(new_data) = c("Cust_ID", "Checking_Status", "Duration", "Credit_History", "Purpose", "Credit_Amount", 
                      "Saving_Status", "Employment", "Installment_Commitment", "Personal_Status", 
                      "Other_parties", "Residence_since", "Property_Magnitude", "Age", 
                      "Other_Payment_Plans", "Housing", "Existing_Credits", "Job", 
                      "Number_of_Dependents", "Own_Telephone", "Foreign_Worker", "Class")

---------------------------------------------------------------------------------------------------
  #setting more cleaning conditions
  #cleaning class
  replace(new_data$Class, new_data$Class == "",NA)

---------------------------------------------------------------
  #cleaning credit amount
  #Check for NA
  No_of_Null_NoCA <- colSums(is.na(new_data[6]))
No_of_Null_NoCA

#check for number of empty column
No_of_Null_NoCA <- sum(new_data$Own_Telephone == "")
No_of_Null_NoCA

#accepting the range of 1000 to 10000
new_data=new_data[(new_data$Credit_Amount>1000) & (new_data$Credit_Amount<10000), ]

#change data type to integer
new_data$Credit_Amount=as.integer(new_data$Credit_Amount)
str(new_data)
--------------------------------------------------------------
  #cleaning saving status
  #Check for NA
  No_of_Null_NoSS <- colSums(is.na(new_data[6]))
No_of_Null_NoSS

#check for number of empty column
No_of_Null_NoSS <- sum(new_data$Own_Telephone == "")
No_of_Null_NoSS  

#only accept data 100>x<100
new_data=new_data[(new_data$Saving_Status<100) , ]

#replace >=1000 to >100
new_data$Saving_Status[new_data$Saving_Status == ">=1000"] <- ">100"

#replace <100 and >100 
new_data$Saving_Status[new_data$Saving_Status == "<100"] <- 0
new_data$Saving_Status[new_data$Saving_Status == ">100"] <- 1

#change data type to integer
new_data$Saving_Status=as.integer(new_data$Saving_Status)
str(new_data)

-------------------------------------------------------
  #cleaning housing
  #Check for NA
  No_of_Null_NoH <- colSums(is.na(new_data[6]))
No_of_Null_NoH

#check for number of empty column
No_of_Null_NoH <- sum(new_data$Own_Telephone == "")
No_of_Null_NoH 


-------------------------------------------------------
  #cleaning own telephone
  #Check for NA
  No_of_Null_NoOT <- colSums(is.na(new_data[20]))
No_of_Null_NoOT

#check for number of empty column
No_of_Null_NoOT <- sum(new_data$Own_Telephone == "")
No_of_Null_NoOT

--------------------------------------------------------------------------------------------------
  #removing all NA values
  cleandata = new_data[complete.cases(new_data), ]

--------------------------------------------------------------------------------------------------------------------------------------
  #Pre-Processing
  #comparing dimensions
  length(new_data)
ncol(new_data)
nrow(new_data)

length(cleandata)
ncol(cleandata)
nrow(cleandata)

--------------------------------------------------------------------------------------------------------------------
  
  #data analysis
  #filtering subset of data with required columns
  final_data=subset(cleandata, select=c("Credit_Amount","Saving_Status","Housing","Own_Telephone","Class"))
nrow(final_data)

----------------------------------------------------------------------------------------------------------------
  
  # Recommendations for "Good" Customers
  # Initial Assumptions for Filtering Good Customers
  ap_val = good_customers %>%
  filter(Credit_Amount < 5000) %>%  # Less than 5000
  filter(Saving_Status == 0) %>%  # Saving Status 0 only
  filter(Housing == "own") %>%  # Homeowners only
  filter(Own_Telephone == "yes") %>%  # Own Telephone "yes" only
  nrow()

# Calculate the initial proportion
ap = ap_val / pfval * 100
ap

# Adjusted Criteria for Good Customers
final_val = good_customers %>%
  filter(Credit_Amount < 6000) %>%  # Less than 6000
  filter(Saving_Status %in% c(0, 1)) %>%  # Include both Saving Status 0 and 1
  filter(Housing == "own") %>%  # Homeowners only
  filter(Own_Telephone %in% c("yes", "none")) %>%  # Include both yes and none for Own Telephone
  nrow()

# Calculate the adjusted proportion
fv = final_val / pfval * 100
fv

#data analysis
#filtering subset of data with required columns
final_data=subset(cleandata, select=c("Credit_Amount","Saving_Status","Housing","Own_Telephone","Class"))
nrow(final_data)

------------------------------------------------------------
  
  
  ##Credit Amount
  
  
  # Filter to separate customers with Credit Amount < 5000 and >= 5000
  filtered_data_under_5000 <- final_data %>%
  filter(Credit_Amount < 5000)

filtered_data_over_5000 <- final_data %>%
  filter(Credit_Amount >= 5000)
----------------------------------------------------------------------------
  # Filter data to include only "good" customers and categorize by Credit Amount
  filtered_data <- final_data %>%
  filter(Class == "good") %>%
  mutate(CreditCategory = ifelse(Credit_Amount < 5000, "Less than 5000", "5000 or more"))  
-----------------------------------------------------------------------------
  # Filter data for good customers  own housing by credit Amount
  filtered_data <- final_data %>%
  filter(Class == "good", Housing == "own") %>%
  mutate(CreditCategory = ifelse(Credit_Amount < 5000, "Less than 5000", "5000 or more")) 
---------------------------------------------------------------------------------
  # Filter data for customers with "good" credit score, credit amount < 5000, and have a telephone
  filtered_data <- final_data %>%
  filter(Class == "good", Credit_Amount < 5000, Own_Telephone == "yes")
---------------------------------------------------------------------------------------
  # Filter for "good" customers with Savings Status < 100 and Credit Amount < 5000
  filtered_data <- final_data %>%
  filter(Class == "good", Saving_Status == 0, Credit_Amount < 5000)
-------------------------------------------------------------------------------------
  # Filter final_data for "good" customers with savings less than 100, credit amount < 5000, housing owned, and telephone yes
  filtered_data <- final_data %>%
  filter(Saving_Status == 0,
         Class == "good",
         Credit_Amount < 5000,
         Housing == "own",
         Own_Telephone == "yes") %>%
  mutate(CreditCategory = "Less than 5000")  
----------------------------------------------------------------------------
  
  # packages 
  install.packages("plotrix")  
install.packages("dplyr")     
install.packages("ggplot2")   
install.packages("ggthemes")
install.packages("ggalt")
insatll.packages("tidyr") 

-----------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  #Analyse.1How many customer with credit amount less than 5000 and 5000 or more?
  
  library(plotrix)
library(dplyr)

# Filter to separate customers with Credit Amount < 5000 and >= 5000
filtered_data_under_5000 <- final_data %>%
  filter(Credit_Amount < 5000)

filtered_data_over_5000 <- final_data %>%
  filter(Credit_Amount >= 5000)

# Calculate the total number of customers in each category
total_customers_under_5000 <- nrow(filtered_data_under_5000)
total_customers_over_5000 <- nrow(filtered_data_over_5000)

# Calculate the total number of customers and the percentages for each category
total_customers <- nrow(final_data)
percentage_under_5000 <- (total_customers_under_5000 / total_customers) * 100
percentage_over_5000 <- (total_customers_over_5000 / total_customers) * 100

# Create a vector for the counts and labels
counts <- c(total_customers_under_5000, total_customers_over_5000)
labels <- c(paste0("Customers (< 5000)\n", round(percentage_under_5000, 2), "% (", total_customers_under_5000, ")"),
            paste0("Customers (>= 5000)\n", round(percentage_over_5000, 2), "% (", total_customers_over_5000, ")"))

# Create a 3D pie chart
pie3D(counts, labels = labels, explode = 0.1, main = "3D Pie Chart of Customers by Credit Amount",
      col = c("lightblue", "darkblue"))
























##Analyse.2 How many customers are classified as good, with a credit amount of less than 5000?

# Load the actual data
file_path <- "C:/Users/Dell/Downloads/5. credit_risk_classification.csv"  # Adjust the file path as necessary
customer_data <- read.csv(file_path)

library(dplyr)
library(ggplot2)

# Filter data to include only "good" customers and categorize by Credit Amount
filtered_data <- final_data %>%
  filter(Class == "good") %>%
  mutate(CreditCategory = ifelse(Credit_Amount < 5000, "Less than 5000", "5000 or more"))  

# Calculate total matches and percentage for customers based on total count
total_count <- nrow(final_data)  
good_credit_count_under_5000 <- nrow(filtered_data %>% filter(Credit_Amount < 5000))  
good_credit_count_5000_or_more <- nrow(filtered_data %>% filter(Credit_Amount >= 5000))  

# Calculate matched percentages
percentage_good_credit_under_5000 <- (good_credit_count_under_5000 / total_count) * 100
percentage_good_credit_5000_or_more <- (good_credit_count_5000_or_more / total_count) * 100

# Create a summary data frame for the heatmap
credit_summary <- data.frame(
  CreditCategory = c("Less than 5000", "5000 or more"),
  Percentage = c(percentage_good_credit_under_5000, percentage_good_credit_5000_or_more)
)

# Create a heatmap to show the percentage of good customers in each credit category
ggplot(credit_summary, aes(x = CreditCategory, y = "", fill = Percentage)) +
  geom_tile(color = "white") +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), color = "black", vjust = -0.5, size = 6) +  
  labs(title = "Percentage of Good Customers by Credit Amount Category",
       x = "Credit Amount Category",
       y = "",
       fill = "Percentage") +
  scale_fill_gradient(low = "lightblue", high = "darkorange") + 
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),  
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


















##Analyse.3 How many customers with a housing ownership,classified as good and  have a credit amount of less than 5000 ?
library(dplyr)
library(ggplot2)

# Load the actual data
file_path <- "C:/Users/Dell/Downloads/5. credit_risk_classification.csv"  
final_data <- read.csv(file_path)

# Filter data for customers with "good" credit score and own housing
filtered_data <- final_data %>%
  filter(Class == "good", Housing == "own") %>%
  mutate(CreditCategory = ifelse(Credit_Amount < 5000, "Less than 5000", "5000 or more"))  

# Calculate total matches and percentage for customers with Credit Amount < 5000
total_count <- nrow(cleandata %>% filter(Housing == "own"))  
good_credit_count_under_5000 <- nrow(filtered_data %>% filter(Credit_Amount < 5000)) 
good_credit_count_5000_or_more <- nrow(filtered_data %>% filter(Credit_Amount >= 5000))  

# Calculate matched percentages
percentage_good_credit_under_5000 <- (good_credit_count_under_5000 / total_count) * 100
percentage_good_credit_5000_or_more <- (good_credit_count_5000_or_more / total_count) * 100

# Display the matches and percentages
cat("Total Matches with Good Credit Score and Credit Amount < 5000:", good_credit_count_under_5000, "\n")
cat("Percentage of Customers with Good Credit Score and Credit Amount < 5000:", round(percentage_good_credit_under_5000, 2), "%\n")
cat("Total Matches with Good Credit Score and Credit Amount >= 5000:", good_credit_count_5000_or_more, "\n")
cat("Percentage of Customers with Good Credit Score and Credit Amount >= 5000:", round(percentage_good_credit_5000_or_more, 2), "%\n")

# Create a histogram 
ggplot(filtered_data, aes(x = Credit_Amount, fill = CreditCategory)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +  
  scale_fill_manual(values = c("orange", "blue")) +  # Custom colors for each category
  labs(title = "Credit Amount for Good Customers who Own Housing",
       subtitle = paste("Matched Percentage of Good Customers who Own a House and Credit Amount < 5000:", round(percentage_good_credit_under_5000, 2), "%", 
                        "\nMatched Percentage of Good Customers who Own a House and Amount >= 5000:", round(percentage_good_credit_5000_or_more, 2), "%"),
       x = "Credit Amount",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12)
  )
















##Analyse4. How many good customers have a credit amount of less than 5000 and have a telephone?
# Load necessary libraries

library(dplyr)
library(ggplot2)

# Filter data for customers with "good" credit score, credit amount < 5000, and have a telephone
filtered_data <- final_data %>%
  filter(Class == "good", Credit_Amount < 5000, Own_Telephone == "yes")

# Calculate total matches and percentage
total_count <- nrow(final_data %>% filter(Own_Telephone == "yes"))  # Total customers with a telephone
good_credit_count <- nrow(filtered_data)  # Total customers with "good" credit score and credit amount < 5000
percentage_good_credit <- (good_credit_count / total_count) * 100  # Percentage calculation

# Display the matches and percentage
cat("Total Matches with Good Credit Score and Credit Amount < 5000:", good_credit_count, "\n")
cat("Percentage of Customers with Good Credit Score and Credit Amount < 5000:", round(percentage_good_credit, 2), "%\n")

# Create a scatter plot to visualize the count of customers with good credit
ggplot(filtered_data, aes(x = Credit_Amount)) +
  geom_jitter(aes(y = 1), width = 0.1, height = 0.1, size = 3, alpha = 0.7, color = "purple") +  
  labs(
    title = " Good Customers who have a Telphone with Credit Amount < 5000",
    subtitle = paste("Percentage of Good Customers who have a Telphone with Credit Amount < 5000:", round(percentage_good_credit, 2), "%"),
    x = "Credit Amount",
    y = "Count of Good Customers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12)
  )
















##Analyse.5 How many good customer with status less than 100 and credit amount less than 5000?

library(dplyr)
library(ggplot2)

# Filter for "good" customers with Savings Status < 100 and Credit Amount < 5000
filtered_data <- final_data %>%
  filter(Class == "good", Saving_Status == 0, Credit_Amount < 5000)

# Calculate total matches
total_count <- nrow(final_data %>% filter(Class == "good"))  
good_credit_count <- nrow(filtered_data)  

# Calculate the matched percentage based on the total number of good customers
matched_percentage <- (good_credit_count / total_count) * 100

# Print the results
cat("Total Matches with Good Customers (Savings < 100 and Credit < 5000):", good_credit_count, "\n")
cat("Matched Percentage of Good Customers based on Total:", round(matched_percentage, 2), "%\n")

# Create a summary data frame for the bar chart
credit_summary <- data.frame(
  Category = c("Good Customers\n(Savings < 100 & Credit < 5000)"),
  Count = good_credit_count,
  Percentage = matched_percentage
)

# Create bar chart
ggplot(credit_summary, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +  # Bar chart with narrower bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 6, fontface = "bold", color = "white") +  
  labs(title = "Count of Good Customers with Savings < 100 and Credit Amount < 5000",
       subtitle = paste("Matched Percentage based on Total:", round(matched_percentage, 2), "%"),
       x = "Customer Category",
       y = "Count") +
  scale_fill_manual(values = c("orange")) +  
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()
  ) +
  coord_flip()  












##Analyse.6 How many good customers have a credit amount of less than 5000, savings less than 100, own a house, and have a telephone 
library(dplyr)
library(ggplot2)

# Filter final_data for "good" customers with savings less than 100, credit amount < 5000, housing owned, and telephone yes
filtered_data <- final_data %>%
  filter(Saving_Status == 0,
         Class == "good",
         Credit_Amount < 5000,
         Housing == "own",
         Own_Telephone == "yes") %>%
  mutate(CreditCategory = "Less than 5000")  

# Calculate total matches and percentage for customers based on total count
total_count <- nrow(final_data %>% filter(Class == "good", Saving_Status < 100, Housing == "own", Own_Telephone == "yes"))  
good_credit_count <- nrow(filtered_data) 
# Calculate matched percentage
percentage_good_credit <- (good_credit_count / total_count) * 100  

# Print the results
cat("Total Matches with Good Credit Score and Credit Amount < 5000:", good_credit_count, "\n")
cat("Percentage of Customers with Good Credit Score and Credit Amount < 5000:", round(percentage_good_credit, 2), "%\n")

# Create a summary data frame for the box plot
credit_summary <- filtered_data %>%
  group_by(CreditCategory) %>%
  summarise(CustomerCount = n(), .groups = 'drop')

# Create a box plot for credit amounts based on categories
box_plot <- ggplot(filtered_data, aes(x = CreditCategory, y = Credit_Amount, fill = CreditCategory)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +  
  scale_fill_manual(values = c("grey")) +  
  labs(title = "Good customers with savings less than 100, credit amount < 5000, housing owned, and telephone ",
       subtitle = paste("Total Matches:", good_credit_count, 
                        "| Percentage:", round(percentage_good_credit, 2), "%"),
       x = "Credit Amount Category",
       y = "Credit Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12))

# Print the box plot
print(box_plot)







#own telephone data analysis
phone_counts = table(ifelse(good_data$Own_Telephone == "yes", "yes", "none"))

#create a data frame for visualization
phone_data <- data.frame(
  phone = names(phone_counts),
  frequency = as.numeric(phone_counts),
  percentage = as.numeric(phone_counts) / sum(phone_counts) * 100
)

#Plotting bar chart
ggplot(phone_data, aes(x = phone, y = frequency, fill = phone, label = paste0(frequency, "\n (", round(percentage, 1), "%)"))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position = position_stack(vjust = 0.5), color = "black") +
  labs(title = "Frequency Distribution of Own Telephone",
       fill = "Status", y = "frequency", x = "with/without own telephone") + 
  scale_fill_manual(values = c("yes" = "cornsilk", "none" = "lavender")) +
  theme_minimal()

#plotting pie chart
ggplot(phone_data, aes(x = "", y = frequency, fill = phone, label = paste0(frequency, " (", round(percentage, 1), "%)"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), color = "black") +
  labs(title = "Frequency Distribution of Own Telephone",
       fill = "Status", y = "frequency") + 
  scale_fill_manual(values = c("yes" = "cornsilk", "none" = "lavender")) +
  coord_polar(theta = "y") +
  theme_void()

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #own telephone and credit amount data analysis
  #calculate count for own telephone
  WithPhone_lessthan5000 <- nrow(CreditAmount_WithPhone)
NoWithPhone_Notlessthan5000 <- nrow(CreditAmount_NoWithPhone)
WithNoPhone_lessthan5000 <- nrow(CreditAmount_WithNoPhone)
NotWithNoPhone_Notlessthan5000 <- nrow(CreditAmount_NotWithNoPhone)

#create a data frame with counts
amount_data <- data.frame(
  Credit_Amount = c("<5000", ">=5000"),
  phone = c(rep("yes", 2)),
  freq = c(WithPhone_lessthan5000, NoWithPhone_Notlessthan5000)
)

amount_data2 <- data.frame(
  Credit_Amount = c("<5000", ">=5000"),
  phone = c(rep("none", 2)),
  freq = c(WithNoPhone_lessthan5000, NotWithNoPhone_Notlessthan5000)
)

#Calculate percentages
total_yes <- sum(amount_data$freq)
amount_data$percentage <- amount_data$freq / total_yes * 100

total_none <- sum(amount_data2$freq)
amount_data2$percentage <- amount_data2$freq / total_none * 100

combined_amount_data <- rbind(amount_data, amount_data2)

#plotting stacked bar chart
ggplot(combined_amount_data, aes(x = phone, y = freq, fill = Credit_Amount)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Relationship Between Own Telephone and Credit Amount",
       x = "With and Without Own Telephone", y = "frequency",
       fill = "Credit Amount",  ) + 
  scale_fill_manual(values = c("aquamarine", "cyan")) +
  theme_minimal() +
  geom_text(aes(label = paste(freq, "(", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5), color = "black", size = 4)

#create a treemap
treemapify_data <- combined_amount_data

#rename columns for better visualization
colnames(treemapify_data) <- c("Credit_Amount", "Own_Telephone", "Value", "percentage")

#plotting treemap
ggplot(treemapify_data, aes(area = Value, label = paste(Credit_Amount, "\n", Value, "(", round(percentage, 1), "%)"))) +
  geom_treemap(aes(fill = Own_Telephone)) +
  geom_treemap_text(color = "black", place = "centre") +
  scale_fill_manual(values = c("aquamarine", "cyan"), name = "With/Without phone") +
  labs(title = "Treemap of Own Telephone and Credit Amount") + 
  theme_void()

--------------------------------------------------------------------------------------------------------
  
  #own telephone, credit amount and saving status
  #count frequencies for each subset
  #create a data frame with counts for with telephone and amount less than 5000
  With_Tele_less5000 <- data.frame(
    Saving_Status = c("<100", ">100"),
    freq = c(
      nrow(SavingStatus_WithPhone),
      nrow(SavingStatus_NoWithPhone)
    ),
    phone = "yes"
  )

#create a data frame with counts for no telephone and amount more than 5000
No_Tele_more5000 <- data.frame(
  Saving_Status = c("<100", ">100"),
  freq = c(
    nrow(SavingStatus_WithNoPhone),
    nrow(SavingStatus_NotWithNoPhone)
  ),
  phone = "none"
)

#calculate total frequency for each group
total_freq_less5000 <- sum(With_Tele_less5000$freq)
total_freq_more5000 <- sum(No_Tele_more5000$freq)

#calculate percentage for each group
With_Tele_less5000$percentage <- With_Tele_less5000$freq / total_freq_less5000 * 100
No_Tele_more5000$percentage <- No_Tele_more5000$freq / total_freq_more5000 * 100

#combine data frames
combined_saving_data <- rbind(With_Tele_less5000, No_Tele_more5000)

#plotting line graph
ggplot(combined_saving_data, aes(x = Saving_Status, y = freq, group = phone, color = phone)) +
  geom_line() + 
  geom_point() +
  labs(title = "Relationship Between Own Telephone, Credit Amount and Saving Status",
       x = "Saving Status", y = "frequemcy", 
       color = "With/Without phone") + 
  scale_color_manual(values = c("yes" = "lightblue", "none" = "lightpink")) +
  theme_minimal()

#plotting lollipop chart
ggplot(combined_saving_data, aes(x = Saving_Status, y = freq, group = phone, color = phone)) +
  geom_segment(aes(x = Saving_Status, xend = Saving_Status, y = 0, yend = freq), color = "lightgreen") + 
  geom_point(size = 3) +
  labs(title = "Relationship Between Own Telephone, Credit Amount and Saving Status",
       x = "Saving Status", y = "frequemcy", 
       color = "With/Without phone") + 
  scale_color_manual(values = c("yes" = "burlywood2", "none" = "burlywood4")) +
  theme_minimal()

------------------------------------------------------------------------------------------------------------------------------
  
  #own telephone, credit amount, saving status and housing
  #housing
  house_tele_data <- data.frame(
    phone = c(rep("With Telephone",2), rep("Without Telephone", 2)),
    Credit_Amount = c("Less Than 5000", "Less Than 5000", "More Than or Equal 5000", "More Than or Equal 5000"),
    Saving_Status = c("<100", "<100", ">100", ">100"),
    Housing = c("own", "rent/free", "own", "rent/free"),
    frequency = c(
      nrow(Housing_withPhone),
      nrow(Housing_NowithPhone),
      nrow(Housing_withNoPhone),
      nrow(Housing_NotwithNoPhone)
    )
  )

#calculate percentages for each title
house_tele_data <- house_tele_data %>%
  group_by(phone, Credit_Amount, Saving_Status) %>%
  mutate(percentage = frequency / sum(frequency) * 100)

#plotting bar chart
ggplot(house_tele_data, aes(x = Housing, y = frequency, fill = Saving_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = frequency), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  geom_text(aes(label = paste0("(", round(percentage, 1), "%)")),
            position = position_dodge(width = 0.9), vjust = 1.25, size = 3, color = "black") +
  facet_wrap(~Credit_Amount + phone) + 
  labs(title = "Relationship Between Own Telephone, Credit Amount, Saving Status and Housing",
       x = "Type of House",y = "frequency", fill = "Status") + 
  scale_fill_manual(values = c("darkseagreen", "green")) +
  theme_minimal()

#plotting heatmap
ggplot(house_tele_data, aes(x = Housing, y= Credit_Amount, fill = frequency)) + 
  geom_tile() + 
  geom_text(aes(label = paste0(frequency, "\n (", round(percentage, 1), "5)")), color = "black") +
  facet_wrap(~Saving_Status + phone) + 
  labs(title = "Relationship Between Own Telephone, Credit Amount, Saving Status and Housing",
       x = "Type of House",y = "Credit Amount",
       fill = "frequency") +
  scale_fill_gradient(low = "lightgoldenrod1", high = "gold4") +
  theme_minimal()

-----------------------------------------------------------------------------------------------------------------------------------
  #chi-square test 
  #subset the data to include only cases where customer with their credit amount and is a good customer.
  subset_data <- final_data[final_data$Credit_Amount <5000 & final_data$Class == "good", ]

#construct the contingency table
chiresult <- table(subset_data$Own_Telephone, subset_data$Class)

#perform the chi-square test
chisq_result <- chisq.test(chiresult)
print(chisq_result)

--------------------------------------------------------------------------------------------------------------------------
  
  #additional features
  #Own Telephone
  tele1 = nrow(subset(own_phone, Own_Telephone == "yes"))
tele2 = nrow(subset(no_phone, !(Own_Telephone == "yes")))

phone_data <- data.frame(tele = c("yes", "Others"),
                         frequency = c(tele1, tele2))

total_freq <- sum(phone_data$frequency)

total_freq <- tele1 + tele2

#proportions to fit the squares
scaled_tele1 <- round((tele1 / total_freq) * 100)
scaled_tele2 <- round((tele2 / total_freq) * 100)

#creating data frame for data visualization
tele_data <- data.frame(
  group = c("yes", "Others"),
  value = c(scaled_tele1, scaled_tele2)
)

#waffle
ggplot(tele_data, aes(fill = group, values = value)) +
  geom_waffle(rows = 8, size = 0.33, color = "black") +
  scale_fill_manual(values = c("yes" = "pink", "Others" = "purple"),
                    labels = c("Others", "yes")) +
  coord_equal() +
  theme_void() +
  labs(title = "Frequency Distribution of Own Telephone")

--------------------------------------------------------------------------------------------------------------------------------------------
  
  #own telephone , credit amount
  # bubble plot
  ggplot(good_data, aes(x = Credit_Amount, y = Own_Telephone)) +
  geom_count(color = "blue") + 
  labs(title = "Frequency of Credit Amount and Own Telephone",
       x = "Credit Amount", y = "Own Telephone",
       size = "frequency") + 
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 45, hjust = 0.5))

----------------------------------------------------------------------------------------------------------------------
  
  #own telephone, credit amount, saving amount
  #plot scatterplot
  ggplot(good_data, aes(x = Credit_Amount, y = Own_Telephone, colour = Saving_Status)) + 
  geom_point() + 
  labs(title = "Own Telephone vs. Credit Amount vs. Savings Status",
       x = "Credit Amount", y = "Own Telephone", color = "Saving Status") +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8))

----------------------------------------------------------------------------------------------------------------------
  
  #own telephone, credit amount, saving status, housing
  #create contingency table
  contingency_data <- xtabs(frequency ~ phone + Housing, data = house_tele_data)

#create spine plot
spineplot(contingency_data, main = "Spineplot of Own Telephone, Credit Amount, Saving Status and Housing",
          xlab = "Own Telephone", ylab = "Housing Status",
          col = c("gold", "tan"))



#data analysis
#filtering subset of data with required columns
final_data=subset(cleandata, select=c("Credit_Amount","Saving_Status","Housing","Own_Telephone","Class"))
nrow(final_data)

-----------------------------------------------------
  #Housing
  
  #Filtering good and bad customer
  Good_cus <- subset(final_data, Class == "good")

#Filtering own, for free and rent data
hown <- subset(Good_cus, Housing == "own")
hff <- subset(Good_cus, Housing == "for free")
hr <- subset(Good_cus, Housing == "rent")

owntrue <- subset(Good_cus, Housing == "own")
ownfalse <- subset(Good_cus, !(Housing == "own"))
------------------------------------------
  #own_telephone filter
  ownteletrue <- subset(owntrue, Own_Telephone == "yes")
owntelefalse <- subset(owntrue, !(Own_Telephone == "yes"))
--------------------------------------------
  #Credit amount filtering
  lt5k_true <- subset(ownteletrue, Credit_Amount < 5000)
lt5k_false <- subset(ownteletrue, !(Credit_Amount < 5000))
--------------------------------------------
  #Saving stats filtering
  lt1_true <- subset(lt5k_true, Saving_Status == 0)
lt1_false <- subset(lt5k_true, Saving_Status == 1)
lt1_true2 <- subset(lt5k_false, Saving_Status == 0)
lt1_false2 <- subset(lt5k_false, Saving_Status == 1)

--------------------------------------------
  
  install.packages("ggplot2")
library(ggplot2)

install.packages("treemapify")
library(treemapify)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggrepel")
library(ggrepel)

install.packages("plotrix")
library(plotrix)

install.packages("waffle")
library(waffle)

install.packages("lessR")
library(lessR)
---------------------------------------------
  #housing
  Own = nrow(subset(hown , Housing == "own"))
Rent = nrow(subset(hr , Housing == "rent"))
Ffree = nrow(subset(hff , Housing == "for free"))
house_data <- data.frame(house = c("Own a house", "Renting a house", " Free house"), 
                         frequency = c(Own, Rent, Ffree))

#Bar chart
ggplot(house_data, aes(x = house, y = frequency, fill = house)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("lightpink", "lightgreen", "lightskyblue")) +
  theme_minimal() +
  labs(
    title = "Information on people's housing status",
    subtitle = "Based on good customer",
    x = "Housing Status",
    y = "Frequency",
    fill = "Status"
  ) +
  geom_text(aes(label = frequency), position = position_stack(vjust = 1), size = 6, color = "black")

#Pie chart
Own2 = nrow(subset(owntrue, Housing == "own"))
Rent_Free = nrow(subset(ownfalse , Housing != "own"))

house_data2 <- data.frame(house2 = c("Own", "Others"),
                          frequency = c(Own2, Rent_Free))
housecolor = c("Own" = "lightskyblue", "Others" = "lightpink")

#calculate total frequency
total_freq <- sum(house_data2$frequency)
house_data2 <- cbind(house_data2, Percentage = round((house_data2$frequency / total_freq) * 100, 1))
labels <- paste0(house_data2$Percentage, "% (", house_data2$frequency, ")")

#create pie chart
pie(house_data2$frequency,
    labels = labels,
    col = housecolor[house_data2$house2],
    theta = 1,
    main = "Housing destribution")

legend("topright", legend = house_data2$house2, fill = housecolor[house_data2$house2])


------------------------------------------------
  
  #housing and own telephone
  tele1 = nrow(subset(owntrue, Own_Telephone == "yes"))
tele2 = nrow(subset(owntrue, !(Own_Telephone == "yes")))

#create data frame far stacked bar chart
phone_data <- data.frame(
  other_housing = "Others",
  Own_Telephone = c("yes", "Others"),
  frequency = c(tele1, tele2)
)

#create the stack bar chart
ggplot(phone_data, aes(x = other_housing, y = frequency, fill = Own_Telephone)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("skyblue", "lightpink")) + 
  theme_minimal() +
  labs(
    title = "Information on People's Housing Status with Their Own Telephone",
    subtitle = "Based on Good Credit Score",
    x = "Others housing status",
    y = "frequency",
    fill = "Own Telephone"
  ) +
  geom_text(aes(label = frequency), position = position_stack(vjust = 0.5), size = 5, color = "black")

-----------------------------------------------------------------------------------------------------------
  
  #housing own telephone and credit amount
  Credit1 = nrow(subset(ownteletrue, Credit_Amount < 5000))
Credit2 = nrow(subset(ownteletrue, !(Credit_Amount < 5000)))

#create data frame for bar chart
credit_data <- data.frame(
  other_housing = "Others",
  Own_Telephonr = "Other",
  credit = c("<5000", ">=5000"),
  frequency = c(Credit1, Credit2)
)

#plotting bar chart
ggplot(credit_data, aes(x = credit, y = frequency, fill = credit)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("skyblue", "pink")) + 
  geom_text(aes(label = frequency), vjust = -0.1) + 
  theme_minimal() + 
  labs(
    title = "Information on People's Credit Amount Against Others Housing Status and Own Telephone",
    subtitle = "Based on Good Credit Score",
    x = "Others Housing Status and Own Telephone",
    y = "Frequency",
    fill = "Credit Amount"
  )

------------------------------------------------------------------------------------------------------------------------
  
  #housing own telephone credit amount and saving status less than 100
  saving1 = nrow(subset(lt5k_true, Saving_Status == 0))
saving2 = nrow(subset(lt5k_true, Saving_Status == 1))

#create data frame for saving status for credit amount with 360
saving_data <- data.frame(
  housing = rep("Others", 2),
  own_telephone = rep("Others", 2),
  credit_amount = rep("<5000", 2),
  saving = c("Less than 100", "More than 100"),
  frequency = c(saving1, saving2)
)

#plotting horizontal chart
ggplot(saving_data, aes(x = reorder(saving, frequency), y = frequency, fill = saving)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "pink")) + 
  geom_text(aes(label = frequency), vjust = -1, size = 4, color = "black") + 
  xlab("Saving Status") + 
  ylab("Frequency") + 
  labs(
    title = "Information on People's Saving Status Against Housing, Own Telephone and Credit Amount",
    subtitle = "Credit amount with <5000",
  ) +
  coord_flip() + 
  theme_minimal()

#create exploding pie chart
total_freq <- sum(saving_data$frequency)
label <- paste0(round((saving_data$frequency / total_freq) * 100, 1), "% (", saving_data$frequency, ")")

#plotting pie
pie(saving_data$frequency,
    labels = label, 
    explode = 1, 
    col = c("skyblue", "pink"), 
    theta = 0.5, 
    main = "Information on People's Saving Status Against Housing, Own Telephone and Credit Amount",
    sub = "Credit amount with <5000")
legend("topright", legend = saving_data$saving, fill = c("skyblue", "pink"))

------------------------------------------------------------------------------------------------------------------------------
  
  #housing own telephone credit amount and saving status more than 100  
  saving3 = nrow(subset(lt5k_false, Saving_Status == 0))
saving4 = nrow(subset(lt5k_false, Saving_Status == 1)) 

#create data frame for saving for credit amount with 57
saving_data_2 <- data.frame(
  housing = rep("Others", 2),
  own_telephone = rep("Others", 2),
  credit_amount = rep(">5000", 2),
  saving = c("Less than 100", "More than 100"),
  frequency = c(saving3, saving4)
)

#plotting horizontal chart
ggplot(saving_data_2, aes(x = reorder(saving, frequency), y = frequency, fill = saving)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("skyblue", "pink")) + 
  geom_text(aes(label = frequency), vjust = -1, size = 4, color = "black") + 
  xlab("Saving Status") + 
  ylab("Frequency") + 
  labs(
    title = "Information on People's Saving Status Against Housing, Own Telephone and Credit Amount",
    subtitle = "Credit amount with >=5000",
  ) +
  coord_flip() + 
  theme_minimal()

#create exploding pie chart
total_freq <- sum(saving_data_2$frequency)
label <- paste0(round((saving_data_2$frequency / total_freq) * 100, 1), "% (", saving_data_2$frequency, ")")

#plotting pie
pie(saving_data_2$frequency,
    labels = label, 
    explode = 1, 
    col = c("skyblue", "pink"), 
    theta = 0.5, 
    main = "Information on People's Saving Status Against Housing, Own Telephone and Credit Amount",
    sub = "Credit amount with >=5000")
legend("topright", legend = saving_data$saving, fill = c("skyblue", "pink"))

----------------------------------------------------------------------------------------------------------------
  
  #combined data frame together
  combined_data <- rbind(
    transform(saving_data, Credit_Amount = "<5000"),
    transform(saving_data_2, Credit_Amount = "Others")
  )

#plotting bar chart
ggplot(combined_data, aes(x = saving, y = frequency, fill = Credit_Amount)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("skyblue", "pink"), name = "Credit Amount", 
                    labels = c("<5000", "Others")) +
  geom_text(aes(label = frequency), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, color = "black") +
  xlab("Saving Status") +
  ylab("Frequency") +
  labs(
    title = "Information on People's Saving Status Against Housing, Own Telephone and Credit Amount",
    subtitle = "Comparison Between Saving Status on Good Credit Score"
  ) + 
  theme_minimal()

---------------------------------------------------------------------------------------------------------------------
  
  #additional 
  #housing against telephone and credit amount
  Own = nrow(subset(hown , Housing == "own"))
Rent = nrow(subset(hr , Housing == "rent"))
Ffree = nrow(subset(hff , Housing == "for free"))

house_data_3 <- data.frame(
  Own_telephone = "others",
  Credit_amount = "others",
  housing = c("own", "rent", "for free"),
  frequency = c(Own, Rent, Ffree))

# create lollipop chart
ggplot(house_data_3, aes(x = housing, y = frequency)) + 
  geom_segment(aes(x = housing, xend = housing, y = 0, yend = frequency), color = "black", lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = frequency), color = "black", size = 4, vjust = -1.5) +
  scale_x_discrete(labels = c("own", "rent", "for free")) +
  coord_flip() + 
  theme_minimal() + 
  labs(
    title = "Information on housing status with own telephone and credit amount",
    subtitle = "Based on Good Credit Score",
    x = "Own telephone and Credit amount", 
    y = "frequency",
    fill = "Housing Status"
  )

--------------------------------------------------------------------------------------------------------------------  
  
  #create a contingency_table
  contingency_table <- xtabs(frequency ~ saving + Credit_Amount, data = combined_data)

#create the spine plot
spineplot(contingency_table, main = "Information on Saving Status Against others Housing, Telephone and Amount",
          xlab = "Saving Status", ylab = "Proportion", 
          col = c("skyblue", "pink"))

-------------------------------------------------------------------------------------------------------------------------------
  
  #waffle chart
  total_freq <- Own2 + Rent_Free

#Proportions to fit into 100 squares
scaled_Own2 <- round((Own2 / total_freq) * 100)
scaled_rentfree <- round((Rent_Free / total_freq) * 100)

#create data frame
house_waffle_data <- data.frame(
  group = c("own", "Others"),
  value = c(scaled_Own2, scaled_rentfree)
)

#create waffle chart
ggplot(house_waffle_data, aes(fill = group, values = value)) +
  geom_waffle(rows = 8, size = 0.33, color = "black") +
  scale_fill_manual(name = NULL,
                    values = c("own" = "skyblue", "Others" = "pink"),
                    labels = c("Others", "own")) + 
  coord_equal() + 
  theme_void() +
  labs(title = "Information on people's housing status")

-------------------------------------------------------------------------------------------------------
  
  #donut chart
  hsize <- 3

#create donut chart
ggplot(phone_data, aes(x = hsize, y = frequency, fill = Own_Telephone)) + 
  geom_col(color = "black") + 
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) + 
  scale_fill_manual(values = c("skyblue", "pink")) +
  theme_minimal() +
  labs(
    title = "Information on People with Own Telephone Against Housing Status",
    subtitle = "Based on Good Credit Score", 
    x = "Others housing status",
    y = "frequency",
    fill = "Own Telephone"
  ) + 
  geom_text(aes(label = frequency), position = position_stack(vjust = 0.5), size = 5, color = "black")



#data analysis
#filtering subset of data with required columns
final_data=subset(cleandata, select=c("Credit_Amount","Saving_Status","Housing","Own_Telephone","Class"))
nrow(final_data)

#-------------------------------------------------------------------------------------------------------------------
# Load necessary libraries and install if needed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

library(dplyr)
library(ggplot2)
library(plotly)

# Data analysis
# Filtering subset of data with required columns
final_data <- subset(cleandata, select = c("Credit_Amount", "Saving_Status", "Housing", "Own_Telephone", "Class"))
nrow(final_data)

# Step 1: Filter to keep only 'good' customers
# Assuming 'Class' has values like "good" and "bad"
good_customers <- subset(final_data, Class == "good")

# Step 2: Check the count of 0s and 1s in the 'Saving_Status' column
count_0 <- sum(good_customers$Saving_Status == 0)
count_1 <- sum(good_customers$Saving_Status == 1)

# Determine which value is more frequent and filter accordingly
if (count_0 > count_1) {
  # If there are more 0s, remove rows with Saving_Status == 1
  filtered_data <- subset(good_customers, Saving_Status == 0)
} else {
  # If there are more 1s, remove rows with Saving_Status == 0
  filtered_data <- subset(good_customers, Saving_Status == 1)
}

# View the filtered data
print(head(filtered_data))

# Step 3: Check the count of 'own' vs. other values in the 'Housing' column
count_own <- sum(filtered_data$Housing == "own")
count_other <- sum(filtered_data$Housing != "own")

# Determine which value is more frequent and filter accordingly
if (count_own > count_other) {
  # If 'own' is more frequent, keep only rows where Housing is 'own'
  final_filtered_data <- subset(filtered_data, Housing == "own")
} else {
  # If 'other' is more frequent, keep only rows where Housing is not 'own'
  final_filtered_data <- subset(filtered_data, Housing != "own")
}

# View the final filtered data
print(head(final_filtered_data))

# Step 4: Check the count of 'yes' vs. 'no' in the 'Own_Telephone' column
count_yes <- sum(final_filtered_data$Own_Telephone == "yes")
count_no <- sum(final_filtered_data$Own_Telephone == "no")

# Determine which value is more frequent and filter accordingly
if (count_yes > count_no) {
  # If 'yes' is more frequent, keep only rows where Own_Telephone is 'yes'
  final_filtered_data <- subset(final_filtered_data, Own_Telephone == "yes")
} else {
  # If 'no' is more frequent, keep only rows where Own_Telephone is 'no'
  final_filtered_data <- subset(final_filtered_data, Own_Telephone == "no")
}

# Step 5: Check the count of Credit_Amount less than 5000 vs. more than 5000
count_less_than_5000 <- sum(final_filtered_data$Credit_Amount < 5000)
count_more_than_5000 <- sum(final_filtered_data$Credit_Amount >= 5000)

# Determine which value is more frequent and filter accordingly
if (count_less_than_5000 > count_more_than_5000) {
  # If Credit_Amount less than 5000 is more frequent, keep those rows
  final_filtered_data <- subset(final_filtered_data, Credit_Amount < 5000)
} else {
  # If Credit_Amount 5000 or more is more frequent, keep those rows
  final_filtered_data <- subset(final_filtered_data, Credit_Amount >= 5000)
}

# View the final filtered data
print(head(final_filtered_data))

# Additional Feature 1: Boxplot of Credit Amount by Saving Status
ggplot(final_filtered_data, aes(x = factor(Saving_Status), y = Credit_Amount, fill = factor(Saving_Status))) +
  geom_boxplot() +
  labs(title = "Boxplot of Credit Amount by Saving Status",
       x = "Saving Status (0: <100, 1: >=100)",
       y = "Credit Amount") +
  theme_minimal()

# Additional Feature 2: Violin Plot of Credit Amount by Saving Status
ggplot(final_filtered_data, aes(x = factor(Saving_Status), y = Credit_Amount, fill = factor(Saving_Status))) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Credit Amount by Saving Status",
       x = "Saving Status (0: <100, 1: >=100)",
       y = "Credit Amount") +
  theme_minimal()



