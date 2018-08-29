library("ggplot2")
library(tidyr)
library("ggthemes")
library(plyr)
library(dplyr)
library(scales)
library("data.table")

#### Reading Data and Correcting Data Types ####
getwd()

RawData <- read.csv("GloMain.csv", stringsAsFactors = FALSE)
RawData[2:3,2:12]

NewData2 <- transpose(RawData)
colnames(NewData2) <- NewData2[1,]
NewData2 <- NewData2[-1,]
rownames(NewData2) <- NULL

YearName <- data.frame(5:15)
colnames(YearName) <- "Year"
NewData2 <- cbind(YearName, NewData2)
NewData2 <- as.data.frame(NewData2)


for(i in 2:4){
  NewData2[,i] <- as.double(NewData2[,i])
}

NewData2[,5] <- factor(NewData2[,5],levels(NewData2[,5])[c(4,2,1,3)])

for(i in 6:9){
  NewData2[,i] <- as.numeric(NewData2[,i])
}


#### Credit Rating ####
ggplot(NewData2, aes(Year,`Credit Rating`)) + 
  geom_col(fill = "#DA70D6", color = "black",alpha = .6, lwd = .9) + 
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Credit Rating") +
  theme_economist(base_size = 15)

#### Stock Price ####
ggplot(NewData2, aes(Year,`Stock Price`)) + 
  geom_col(fill = "Green", color = "black",alpha = .6, lwd = 1) + 
  geom_text(aes(label = paste0("$",`Stock Price`), y = ifelse(`Stock Price` == 12, `Stock Price`+35,`Stock Price`*.5)), size = 4.4) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(0,875,125)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Year-End Stock Price") +
  theme_economist(base_size = 15) 

#### EPS ####
ggplot(NewData2, aes(Year, EPS)) + 
  geom_col(fill = "#DEB887", color = "black",alpha = .6, lwd = 1) + 
  geom_text(aes(label = paste0("$",EPS), y = ifelse(EPS == .75, EPS+1.2, EPS*.5)), size = 4.8) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(0,32,4)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Year-End EPS") +
  theme_economist(base_size = 15) 

#### Image Rating ####
ggplot(NewData2, aes(Year, `Image Rating`)) + 
  geom_col(fill = "#6495ed", color = "black",alpha = .6, lwd = 1) + 
  geom_text(aes(label = `Image Rating`, y = `Image Rating`*.5), color = "White", size = 8) +
  scale_y_continuous(breaks = seq(0,100,15)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Image Rating") +
  theme_economist(base_size = 15)

#### ROE ####
ggplot(NewData2, aes(Year, ROE)) + 
  geom_col(fill = "#D35400", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = paste0(100*ROE,"%"), y = ROE*.5), color = "White", size = 5) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.7,.1)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Year-End ROE") +
  theme_economist(base_size = 15)

#### Prep for Stacked Revenue Charts ####
A <- data.frame(NewData2$Year, NewData2$`AC Camera Revenue`)
A$Type <- for(i in 1:11){A[i,3] = "AC Camera"}
colnames(A) <- c("Year", "Revenue", "Revenue Stream")

B <- data.frame(NewData2$Year, NewData2$`UAV Revenue`)
B$Type <- for(i in 1:11){B[i,3] = "UAV"}
colnames(B) <- c("Year", "Revenue", "Revenue Stream")

C <- rbind(A,B)

D <- group_by(C, as.character(C$Year)) %>% summarise(totalRev = sum(Revenue))
D <- D[c(7:11, 1:6),]

Percent <- round(100*(C$Revenue/D$totalRev),1)
df.summary <- C %>% group_by(Year, `Revenue Stream`) %>% 
  summarise(Revenue = sum(Revenue)) %>%   # Within each Brand, sum all values in each Category
  mutate(percent = Revenue/sum(Revenue),
         pos = cumsum(percent) - 0.5*percent)

df.summary <- df.summary[c(2,1,4,3,6,5,8,7,10,9,12,11,14,13,16,15,18,17,20,19,22,21),]


#### Total Revenue Plot ####
ggplot(C, aes(Year, Revenue, fill = `Revenue Stream`)) + 
  geom_col(color = "black",alpha = .6, lwd = 1, position = "Stack") +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(0,1800000,300000)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Trend in Annual Revenue") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank())

#### Percent Rev Plot ####
ggplot(df.summary) + 
  geom_bar(color = "black",alpha = .6, lwd = 1, aes(y = percent, x = Year, fill = `Revenue Stream`), stat = "identity") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.25)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Where Our Revenue Comes From") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(y = "Percentage of Total Revenue")



#### Demand/Market General Setup ####
AC_Dem_Raw <- read.csv("AC_Dem-Cost-Marg.csv", stringsAsFactors = FALSE)
UAV_Dem_Raw <- read.csv("UAV_Dem-Cost-Marg.csv", stringsAsFactors = FALSE)


AC_Dem <- transpose(AC_Dem_Raw)
colnames(AC_Dem) <- AC_Dem[1,]
AC_Dem <- AC_Dem[-1,]
rownames(AC_Dem) <- NULL

YearName <- data.frame(5:15)
colnames(YearName) <- "Year"
AC_Dem <- cbind(YearName, AC_Dem)
AC_Dem <- as.data.frame(AC_Dem)


UAV_Dem <- transpose(UAV_Dem_Raw)
colnames(UAV_Dem) <- UAV_Dem[1,]
UAV_Dem <- UAV_Dem[-1,]
rownames(UAV_Dem) <- NULL

YearName <- data.frame(5:15)
colnames(YearName) <- "Year"
UAV_Dem <- cbind(YearName, UAV_Dem)
UAV_Dem <- as.data.frame(UAV_Dem)


Total_Dem <- rbind(AC_Dem, UAV_Dem)

Total_Dem[,1] <- as.character(Total_Dem[,1])
AC_Dem[,1] <- as.character(AC_Dem[,1])
UAV_Dem[,1] <- as.character(UAV_Dem[,1])

for(i in 1:11){
  Total_Dem[,i] <- as.numeric(Total_Dem[,i])
  AC_Dem[,i] <- as.numeric(AC_Dem[,i])
  UAV_Dem[,i] <- as.numeric(UAV_Dem[,i])
}


#### P/Q Above Average, AC ####
ggplot(AC_Dem, aes(Year, `PQ Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = `PQ Rating`, y = ifelse(`PQ Above Industry Average` == 0, `PQ Above Industry Average`+.02,`PQ Above Industry Average`*.5)), size = 4.8) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.45,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("P/Q Rating for AC Camera") +
  theme_economist(base_size = 15)


#### P/Q Above Average, UAV ####
ggplot(UAV_Dem, aes(Year, `PQ Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = `PQ Rating`, y = ifelse(`PQ Above Industry Average` == 0, `PQ Above Industry Average`+.02,`PQ Above Industry Average`*.5)), size = 4.8) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("P/Q Rating for UAV") +
  theme_economist(base_size = 15)


#### Setup for AC Camera Market Share ####
A <- data.frame(AC_Dem$Year, AC_Dem$`Actual Overall Demand`)
A$Type <- for(i in 1:11){A[i,3] = "Actual Demand"}
colnames(A) <- c("Year", "Market Share", "Demand Type")

B <- data.frame(AC_Dem$Year, AC_Dem$`Units Gained`)
B$Type <- for(i in 1:11){B[i,3] = "Units Gained"}
colnames(B) <- c("Year", "Market Share", "Demand Type")

C <- rbind(A,B)

D <- group_by(C, as.character(C$Year)) %>% summarise(totalMS = sum(`Market Share`))
D <- D[c(7:11, 1:6),]

Percent <- round(100*(C$`Market Share`/D$totalMS),1)
df.summary <- C %>% group_by(Year, `Demand Type`) %>% 
  summarise(`Market Share` = sum(`Market Share`)) %>%   # Within each Brand, sum all values in each Category
  mutate(percent = `Market Share`/sum(`Market Share`),
         pos = cumsum(percent) - 0.5*percent)


#### AC Market Share Charts ####
ggplot(C, aes(Year, `Market Share`, fill = `Demand Type`)) + 
  geom_col(color = "black",alpha = .6, lwd = 1, position = "Stack") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("AC Camera Market Share") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank())


ggplot(df.summary) + 
  geom_bar(color = "black",alpha = .6, lwd = 1, aes(y = percent, x = Year, fill = `Demand Type`), stat = "identity") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.25)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("AC Camera Market Share Breakdown") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(y = "Percentage of Market Share")


#### Setup for UAV Market Share ####
A <- data.frame(UAV_Dem$Year, UAV_Dem$`Actual Overall Demand`)
A$Type <- for(i in 1:11){A[i,3] = "Actual Demand"}
colnames(A) <- c("Year", "Market Share", "Demand Type")

B <- data.frame(UAV_Dem$Year, UAV_Dem$`Units Gained`)
B$Type <- for(i in 1:11){B[i,3] = "Units Gained"}
colnames(B) <- c("Year", "Market Share", "Demand Type")

C <- rbind(A,B)

D <- group_by(C, as.character(C$Year)) %>% summarise(totalMS = sum(`Market Share`))
D <- D[c(7:11, 1:6),]

Percent <- round(100*(C$`Market Share`/D$totalMS),1)
df.summary <- C %>% group_by(Year, `Demand Type`) %>% 
  summarise(`Market Share` = sum(`Market Share`)) %>%   # Within each Brand, sum all values in each Category
  mutate(percent = `Market Share`/sum(`Market Share`),
         pos = cumsum(percent) - 0.5*percent)


#### UAV Market Share Charts ####
ggplot(C, aes(Year, `Market Share`, fill = `Demand Type`)) + 
  geom_col(color = "black",alpha = .6, lwd = 1, position = "Stack") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("UAV Market Share") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank())


ggplot(df.summary) + 
  geom_bar(color = "black",alpha = .6, lwd = 1, aes(y = percent, x = Year, fill = `Demand Type`), stat = "identity") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.25)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("UAV Market Share Breakdown") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(y = "Percentage of Market Share")


#### Operating Margins ####
ggplot(Total_Dem, aes(Year, `Operating Profit Margin`, fill = Product)) + 
  geom_col(color = "black",alpha = .6, lwd = 1, position = "Dodge") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("Operating Profit Margins by Product") +
  theme_economist(base_size = 15) +
  theme(legend.position = "right", legend.title = element_blank())


#### Setup for Price Chart ####
PricesTotal <- melt(Total_Dem[,1:3], id = 'Year')
PricesAC <- melt(AC_Dem[,1:3], id = 'Year')
PricesUAV <- melt(UAV_Dem[,1:3], id = 'Year')


#### AC Price Change ####
ggplot(PricesAC, aes(x = Year, y = value, color = variable)) + 
  geom_line(size = 1.4) +
  geom_point(size=4, shape = 22, fill = "white")+
  scale_x_continuous(breaks = seq(5,15,1)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(225, 450, 25)) +
  theme_economist(base_size = 15) +
  ggtitle("Price of AC Cameras") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = "Price")


#### UAV Price Change ####
ggplot(PricesUAV, aes(x = Year, y = value, color = variable)) + 
  geom_line(size = 1.4) +
  geom_point(size=4, shape = 22, fill = "white")+
  scale_x_continuous(breaks = seq(5,15,1)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(1000, 2750, 250)) +
  theme_economist(base_size = 15) +
  ggtitle("Price of UAVs") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = "Price")


#### Demand/Market General Setup, Compensation/Productivity ####
AC_Prod_Raw <- read.csv("AC_Prod-Comp.csv", stringsAsFactors = FALSE)
UAV_Prod_Raw <- read.csv("UAV_Prod-Comp.csv", stringsAsFactors = FALSE)


AC_Prod <- transpose(AC_Prod_Raw)
colnames(AC_Prod) <- AC_Prod[1,]
AC_Prod <- AC_Prod[-1,]
rownames(AC_Prod) <- NULL

YearName <- data.frame(5:15)
colnames(YearName) <- "Year"
AC_Prod <- cbind(YearName, AC_Prod)
AC_Prod <- as.data.frame(AC_Prod)


UAV_Prod <- transpose(UAV_Prod_Raw)
colnames(UAV_Prod) <- UAV_Prod[1,]
UAV_Prod <- UAV_Prod[-1,]
rownames(UAV_Prod) <- NULL

YearName <- data.frame(5:15)
colnames(YearName) <- "Year"
UAV_Prod <- cbind(YearName, UAV_Prod)
UAV_Prod <- as.data.frame(UAV_Prod)


Total_Prod <- rbind(AC_Prod, UAV_Prod)

Total_Prod[,1] <- as.character(Total_Prod[,1])
AC_Prod[,1] <- as.character(AC_Prod[,1])
UAV_Prod[,1] <- as.character(UAV_Prod[,1])

for(i in 1:13){
  Total_Prod[,i] <- as.numeric(Total_Prod[,i])
  AC_Prod[,i] <- as.numeric(AC_Prod[,i])
  UAV_Prod[,i] <- as.numeric(UAV_Prod[,i])
}


#### Compensation Above Average, AC ####
ggplot(AC_Prod, aes(Year, `Compensation Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = dollar(Compensation), y = ifelse(`Compensation Above Industry Average` == 0, `Compensation Above Industry Average`+.04,`Compensation Above Industry Average`*.5)), size = 4.2) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.7,.1)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("B Company Compensation for AC Camera Workers (per PAT Member)") +
  theme_economist(base_size = 15)


#### Compensation Above Average, UAV ####
ggplot(UAV_Prod, aes(Year, `Compensation Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = dollar(Compensation), y = ifelse(`Compensation Above Industry Average` == 0, `Compensation Above Industry Average`+.04,`Compensation Above Industry Average`*.5)), size = 4.2) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.70,.1)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("B Company Compensation for UAV Workers (per PAT Member)") +
  theme_economist(base_size = 15)


#### Productivity Above Average, AC ####
ggplot(AC_Prod, aes(Year, `Productivity Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = dollar(`PAT Productivity`), y = ifelse(`Productivity Above Industry Average` == 0, `Productivity Above Industry Average`+.02,`Productivity Above Industry Average`*.5)), size = 4.2) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.45,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("B Company Productivity for AC Camera Workers (per PAT)") +
  theme_economist(base_size = 15)


#### Productivity Above Average, UAV ####
ggplot(UAV_Prod, aes(Year, `Productivity Above Industry Average`)) + 
  geom_col(fill = "lightgreen", color = "black", alpha = .6, lwd = 1) + 
  geom_text(aes(label = dollar(`PAT Productivity`), y = ifelse(`Productivity Above Industry Average` == 0, `Productivity Above Industry Average`+.02,`Productivity Above Industry Average`*.5)), size = 4.2) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,.5,.05)) +
  scale_x_continuous(breaks = seq(5,15,1)) +
  ggtitle("B Company Productivity for UAV Workers (per PAT)") +
  theme_economist(base_size = 15)


#### Setup for Compensation ####
Total <- melt(Total_Prod[,c(1,8:9)], id = 'Year')
AC <- melt(AC_Prod[,c(1,8:9)], id = 'Year')
UAV <- melt(UAV_Prod[,c(1,8:9)], id = 'Year')


#### AC Cost of Labor ####
ggplot(AC, aes(x = Year, y = value, color = variable)) + 
  geom_line(size = 1.4) +
  geom_point(size=4, shape = 22, fill = "white")+
  scale_x_continuous(breaks = seq(5,15,1)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(30, 45, 1)) +
  theme_economist(base_size = 15) +
  ggtitle("Cost of Labor, AC Cameras") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = "Labor Cost ($/Unit Assembled)")


#### UAV Cost of Labor ####
ggplot(UAV, aes(x = Year, y = value, color = variable)) + 
  geom_line(size = 1.4) +
  geom_point(size=4, shape = 22, fill = "white")+
  scale_x_continuous(breaks = seq(5,15,1)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = seq(60, 80, 2)) +
  theme_economist(base_size = 15) +
  ggtitle("Cost of Labor, UAVs") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = "Labor Cost ($/Unit Assembled)")
