library(readr)
library(dplyr)
#Question 1 INST 314 Brendan Conners
HW_dataset_county <- R_hw1_dataset_22$County
sorted_county_dataframe <- data.frame(sort(table(HW_dataset_county), decreasing = TRUE)[1:2])
colnames(sorted_county_dataframe) <- c("Counties", "Frequency")
sorted_county_dataframe


#Question 2 INST 314 Brendan Conners

hwdata = read.csv(file= 'R_hw1_dataset_22.csv')
MD_inds = hwdata$State == "MD"
md = hwdata[MD_inds,]
sorted_md_countys = sort(table(md$County), decreasing = TRUE) [1:2]
sorted_md_countys

#Question 3 INST 314 Brendan Conners
hwdata = read.csv(file= 'R_hw1_dataset_22.csv')
HW_dataset_states = hwdata$State
sorted_state_df = data.frame(sort(table(HW_dataset_states), decreasing = TRUE)[1:2])
colnames(sorted_state_df) = c("States", "Frequency")
sorted_state_df
#Question 4 INST 314 Brendan Conners
library(dplyr)
hwdata = read.csv(file= 'R_hw1_dataset_22.csv')
four_inds = hwdata$Severity == "4"
st = hwdata[four_inds,]
sort_st = table(st$State)
sortst_df = data.frame(sort_st)
colnames(sortst_df) = c("States", "Frequency")

hw1data = read.csv(file= 'R_hw1_dataset_22.csv')
not_four_inds = (hw1data$Severity == "1") | (hw1data$Severity == "2") | (hw1data$Severity == "3") 
nt = hw1data[not_four_inds,]
sort_nt = table(nt$State)
sortnt_df = data.frame(sort_nt)
colnames(sortnt_df) = c("States", "Frequency")
commonID <- intersect(sortst_df$States,sortnt_df$States)
sortnt_df[!sortnt_df$States %in% commonID,]
acommon <-sortst_df[sortst_df$States %in% commonID,]
bcommon <- sortnt_df[sortnt_df$States %in% commonID,]

combined = merge(data.frame(acommon, row.names = NULL), data.frame(bcommon, row.names = NULL),
                 by = 0, all = TRUE)[-1]
                
combined[order(combined$States.x),]
combined[order(combined$States.y),]
combined$Fraction <- combined$Frequency.x / combined$Frequency.y
x = -combined$Fraction
indices_2 = order(x)
combined[indices_2[1:3],]

#Question 5 INST 314 Brendan Conners
hwdata = read.csv(file= 'R_hw1_dataset_22.csv')
hwdata %>%
  group_by(State) %>%
  summarize(temp=mean(Temperature.F.)) %>%
  arrange(temp) %>%
  print(n = 4) 


