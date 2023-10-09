
df<- read.csv("C:/Users/suhan/Downloads/Data Set- Inc5000 Company List_2014.csv")
view(df)
head(df)
tail(df)
type_sum(df)

#shape
dim(df)

#datatypes
str(df)

#missing values
colSums(is.na(df))

#subset columns
df$city
df$city[0:5]
df$state_l

df[df$state_l=="Florida",]
df$state_l[5:10]

#average revenue 
mean(df$revenue)

#average number of workers
mean(df$workers)

mean(df$yrs_on_list)

#check duplicates
duplicated(df$company)

#no of duplicates in company
sum(duplicated(df$company))
sum(duplicated(df$X_input))
sum(duplicated(df$id))

sum(duplicated(df)) #check row wise

#total missing valuesin each column
colSums(is.na(df))

#drop null values
na.omit(df) #temprory drop

#size after drop
dim(na.omit(df)) # it will drop all rows having null

#drop all rows
df= na.omit(df) #permanent drop

#reload dataset

df<- read.csv("C:/Users/suhan/Downloads/Data Set- Inc5000 Company List_2014.csv")

names(df) # give all the names of the columns

#drop column X_input df(rows, column)
df = df[,!names(df) %in% c("X_input")] #exclude X_input

dim(df)

#check missing values
colSums(is.na(df))

#removing multiple columns 
df = df[,!names(df) %in% c("X_input", "X_num", "X_pageUrl")] #exclude X_input

#checking duplicates
sum(duplicated(df$X_widgetName))

#rounding growth column
round(df$growth,2)

df$growth = round(df$growth,2) # permanent change

#import library
#library(ggplot2)

#drawing boxplot and labeling outliers
ggplot(df, aes(x=revenue, y=growth)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

Q1_growth= quantile(df$growth, 0.25) #1st quantile = 84.21
Q3_growth= quantile(df$growth, 0.75) #3rd quantile = 347.65

IQR_growth = Q3_growth-Q1_growth
print(IQR_growth)

max(df$growth)
min(df$growth)

347.65+1.5*347.65 # upper range for IQR
84.21-1.5*84.21 # lower range for IQR

# dropping outliers
df_cleaned= subset(df, df$growth> (Q1_growth - 1.5*IQR_growth) & df$growth< (Q3_growth + 1.5*IQR_growth))

#summary statistics

summary(df_cleaned)

dim(df_cleaned)
view(df_cleaned)

#bivariate analysis

ggplot(df_cleaned, aes(x=rank, y=workers)) + 
  geom_point()+ scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim = c(0, 300))

#bar plat

ggplot(df_cleaned, aes(x=yrs_on_list)) + 
  geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15)) #sshows negative correlation

#correlation between growth and revenue

cor(df_cleaned$growth, df_cleaned$revenue)
cor(df_cleaned$yrs_on_list, df_cleaned$rank)

#export cleaned data

write.csv(df_cleaned, "C:/Users/suhan/Desktop/Data Technicican/R/cleaned_df.csv")









