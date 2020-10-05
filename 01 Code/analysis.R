# Preliminaries ----
library("readxl")
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(caret)

main_path = "/Users/paulmora/Documents/projects/polarized"
raw_path = paste(main_path, "/00 Raw", sep="")
code_path = paste(main_path, "/01 Code", sep="")
data_path = paste(main_path, "/02 Data", sep="")
output_path = paste(main_path, "/03 Output", sep="")

# Importing data ----
house_data = read_excel(paste(data_path, "/house_data.xlsx", sep=""))
senate_data = read_excel(paste(data_path, "/senate_data.xlsx", sep=""))

# Functions ----
polarising_data = function(data, type) {
  "
  This function calculates the relative number of democratic and republicans
  who vote with yes to any given bill. Afterwards the absolute difference
  of these two numbers are calculated and labelled as the polarization factor.
  Lastly, in order to identify whether this function was applied to the house
  or the senate, a variable called type is created and added as a column.
  "
  data = data %>% 
    mutate(rel_yes_dem = democratic_yea / democratic_seats,
           rel_yes_rep = republican_yea / republican_seats) %>%
    mutate(polarized_factor = abs(rel_yes_dem - rel_yes_rep)) %>%
    select(polarized_factor, democratic_yea, democratic_nay,
           democratic_seats, republican_yea, republican_nay,
           republican_seats, year, rel_yes_dem, rel_yes_rep)
  data[, "type"] = type
  return(data)
}

# House data cleaning ----
"
The goal is to assess polarization for democrats and republicans. This is done
by checking how many people from a certain party voted for a bill if it was
proposed by the same party.

"

num_cols = c("democratic", "republican", "total")
party_cols = grep(paste(num_cols, collapse="|"), colnames(house_data))
house_data[, party_cols] = sapply(house_data[, party_cols], as.numeric)

for (party in c("democratic", "republican")) {
  cols = grep(party, colnames(house_data))
  column_name = paste(party, "seats", sep="_")
  house_data[, column_name] = rowSums(house_data[, cols])
}

cleaned_house_data = polarising_data(house_data, "house")

# Senate data cleaning ----
"
We start by inputting the year variable. This is done by looking at the number
of congress. We know for a fact that in 1989 we had the first 101st congress.
"

congresses = sort(unique(senate_data$congress))
start_year = 1989
end_year = start_year + length(congresses) - 1
years = seq(start_year, end_year)
senate_data$year = mapvalues(senate_data$congress, from=congresses, to=years)

type_list = c("Guilty"="Yea", "Not Guilty"="Nay")
for (party in c("D_", "R_")) {
  for (type in names(type_list)) {
    guilty_col = paste(party, type, sep="")
    party_col = paste(party, type_list[type], sep="")
    bool_guilty = !is.na(senate_data[, guilty_col])
    senate_data[bool_guilty, party_col] = senate_data[bool_guilty, guilty_col] 
  }
}

"

"

senate_data[is.na(senate_data)] = 0
voting_types = c("Yea", "Nay", "Not Voting")
party_types = c("D"="democratic", "R"="republican")
for (party in names(party_types)) {
  # Calculating the sum of people being able to give a vote
  total_sum = rowSums(senate_data[, paste(party, voting_types, sep="_")])
  senate_data[, paste(party_types[party], "seats", sep="_")] = total_sum
  
  # Changing names to be in line with house data
  for (answer in c("Yea", "Nay")) {
    old_name = paste(party, answer, sep="_")
    new_name = paste(party_types[party], tolower(answer), sep="_")
    colnames(senate_data)[which(names(senate_data) == old_name)] = new_name
  } 
}

cleaned_senate_data = polarising_data(senate_data, "senate")

# Appending Data ----

"
Here we put both now cleaned datasets together
"

polarized_data = rbind(cleaned_house_data, cleaned_senate_data)
polarized_data$year = as.numeric(polarized_data$year) 
polarized_data$type = str_to_title(polarized_data$type)

# Number of seats over time ----

"
We start by looking how many seats we have for house and senate over time.
"

# Getting data-frame with the number of average seats
average_seats = tidyr::gather(polarized_data, party, seats, 
                              democratic_seats, republican_seats) %>%
  group_by(year, type, party) %>%
  summarise(Average_Seats = mean(seats, na.rm=TRUE))

# Assigning sensible variable names
average_seats$party = mapvalues(average_seats$party,
                                from=c("democratic_seats", "republican_seats"),
                                to=c("Democrats", "Republicans"))

ggplot(data=average_seats, aes(x=year, y=Average_Seats,
                               group=interaction(party, type),
                               color=party, linetype=type)) +
  geom_point() + 
  geom_line() + 
  ylab("Average Number of Seats") + xlab("Years") + 
  scale_color_manual(values=c("#0015BC", "#FF0000")) +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave(paste(output_path, "seats.png", sep="/"), bg="transparent")

# Correlation between seats of the house and senate ----
average_seats$combination = paste(average_seats$type,
                                  average_seats$party, sep=" ")
long_corr_data = average_seats %>% 
  ungroup() %>% 
  select(Average_Seats, combination, year)
wide_corr_data = spread(long_corr_data, combination, Average_Seats) %>%
  drop_na()

cormat = round(cor(wide_corr_data),2)
# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low="red", high="blue", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Pearson\nCorrelation") +
  theme_tufte()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 8) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=20, angle=45, hjust=1),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=20, hjust=1),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                               label.theme = element_text(size=20),
                               title.position = "top", title.hjust = 0.5))
ggsave(paste(output_path, "correlation.png", sep="/"))

# Polarization ----

# Number of decisions over time
"
We first show how many decisions are available for each year for house and
senate.
"

number_decisions_per_year = polarized_data %>% select(year, type) %>% 
  group_by(year, type) %>%
  tally()

ggplot(data=number_decisions_per_year, aes(x=year, y=n,
                               group=type,
                               color=type)) +
  geom_point() + 
  geom_line() + 
  ylab("Number of Decisions made") + xlab("Years") +
  scale_color_manual(values=c("#82B446", "#7846B4")) +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste(output_path, "num_decisions.png", sep="/"))

# Polarization over time 

"
Here we show the average polarization over time.
"

avg_pol_data = polarized_data %>% select(polarized_factor, year, type) %>%
  group_by(year, type) %>%
  summarize(mean_pol = mean(polarized_factor, na.rm=TRUE))

ggplot(data=avg_pol_data, aes(x=year, y=mean_pol,
                              group=type,
                              color=type)) +
  geom_point() + 
  geom_line() + 
  ylab("Average Polarization") + xlab("Years") +
  scale_color_manual(values=c("#82B446", "#7846B4")) +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste(output_path, "avg_pol.png", sep="/"))

"
In order to see whether the polarization factor is dependent on which party
is in power, we graph both lines in a separate plot while shading the background
according the majority power of the respective institution - House/ Senate
"

data = average_seats %>% select(-one_of("combination"))
wide_data = spread(data, party, Average_Seats) %>%
  mutate(majority = if_else(Democrats < Republicans,
                            "Democratic", "Republican"))
avg_pol_data$majority = wide_data$majority

ggplot(data=avg_pol_data, aes(x=year, y=mean_pol,
                                group=type)) +
  geom_point() + 
  geom_line() + 
  geom_tile(aes(fill=majority),
            width = 1, height = Inf, alpha = 0.3) +
  scale_fill_manual(values=c(Democratic="#0015BC", Republican="#FF0000")) +
  ylab("Average Polarization") + xlab("Years") +
  theme_tufte() +
  facet_grid(.~ type) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste(output_path, "avg_pol_majority.png", sep="/"))

dummy_data = avg_pol_data %>% 
  mutate(majority_dummy = if_else(majority=="Democratic", 1, 0)) %>%
  select(majority_dummy, mean_pol )

col_names = c("year", "Republican Dummy", "Average Polarization")
colnames(dummy_data) = col_names
cormat = round(cor(dummy_data[, -(1)]),2)
# Get upper triangle of the correlation matrix
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low="red", high="blue", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Pearson\nCorrelation") +
  theme_tufte()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 20, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=20, angle=45, hjust=1),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=20, hjust=1),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                               label.theme = element_text(size=20),
                               title.position = "top", title.hjust = 0.5))
ggsave(paste(output_path, "correlation_pol_marjority.png", sep="/"))

# Regression Line

"
The question is now whether the growing effect of polarization is significant.
For that we fit a regression line through all observations and report the
statistics
"
avg_pol_data$type = str_to_title(avg_pol_data$type)
ggplot(data=avg_pol_data, aes(x=year, y=mean_pol)) +
  geom_point(colour = "#7846B4") +
  geom_smooth(method='lm', formula= y~x, color="#82B446") +
  ylab("Average Polarization") + xlab("Years") +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste(output_path, "regression_line.png", sep="/"))

lmodel <- lm(mean_pol ~ year, data=avg_pol_data)
summary(lmodel)

# Party Unity ----

"
Now we assess the party unity. This works similiarly to the polarization but
focuses on inner-party behaviour. 
"

# Calculation of unity
party_unity_data = polarized_data %>% 
  mutate(Democrats = abs(rel_yes_dem - (1-rel_yes_dem)),
         Republican = abs(rel_yes_rep - (1-rel_yes_rep))) %>%
  group_by(type, year) %>%
  summarise(across(c("Democrats", "Republican"), mean, na.rm= TRUE))

# Wide to long data-frame
long_party_unity = tidyr::gather(party_unity_data, party, unity, 
                                 Democrats, Republican)

# Plotting the results
house_data = long_party_unity[long_party_unity$type=="House",]
senate_data = long_party_unity[long_party_unity$type=="Senate",]

ggplot(data=long_party_unity, aes(x=year, y=unity,
                                  group=interaction(party, type),
                                  color=party)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x) +
  ylab("Average Unity") + xlab("Years") + 
  scale_color_manual(values=c("#0015BC", "#FF0000")) +
  theme_tufte() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=45, hjust=1)) +
  facet_grid(.~ type)
ggsave(paste(output_path, "unity.png", sep="/"))

lmodel = lm(unity ~ year, data=house_data)
summary(lmodel)
lmodel = lm(unity ~ year, data=senate_data)
summary(lmodel)

