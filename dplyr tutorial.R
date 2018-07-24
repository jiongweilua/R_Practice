install.packages('data.table', dependencies = TRUE)
library(data.table)
setwd('/Users/luajiongwei/Documents/GitHub/R tutorials')
data <- fread("combined_data.csv")
# general syntax data[i, j, by]
# i = where , j = select / update, by = groupby
x = data[town == "ANG MO KIO" & resale_price > 100000]
head(x)
x = x[order(month, floor_area_sqm, -resale_price)]
head(x)
?order()
x = data[town=="ANG MO KIO", .(time = month , price = resale_price)] #can rename columns as per defining a list, .() is just an alias for .list()
head(x)

x = data[town == "ANG MO KIO", .(mprice = mean(resale_price))] # subset in i and computation in j
head(x)

data[town == "ANG MO KIO", .N] # .N is a special symbol that identifies the number of extension matching 

x = data[town =="ANG MO KIO", c("month", "resale_price") ]
head(x)
select_cols = c("resale_price", "month", "floor_area_sqm")
x = data[, ..select_cols]
head(x)


data[, .(.N), by = .(town)]
x= data[month > 2000, .(mean(resale_price)), by =.(town, flat_type)] # i subsetting, updating/ computation, by = town 
View(x)

x = data[month > 2000, .(.N), by = .(town, flat_type)]
sort(x, on = "N", decreasing = FALSE)

data_matrix = as.matrix(data)
head(data_matrix)
head(data_matrix)

head(x)
for (i in unique(x$town)){ 
  town = i
  subset = x[x$town == i]$N
  max_price = max(subset)
  room  = x[x$town ==i & x$N == max_price]$flat_type
  print(paste(i ,room, toString(max_price)))
  }
  
install.packages('dplyr')
install.packages('downloader')
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- fread("msleep_ggplot2.csv")
head(msleep)
colnames(msleep)
msleep %>% select(name, genus, vore, order) %>% group_by(genus)




library(dplyr)
sleepdata <- select(msleep, name, sleep_total, genus)
filter(msleep, sleep_total > 12, bodywt > 5)
head(select(msleep, name:sleep_total))

head(data)
class(data)
data[ town!= 'ANG MO KIO' | resale_price > 300000]

y = data[town != 'ANG MO KIO' | town != 'BUKIT TIMAH', .(mean(resale_price)), by = .(storey_range, flat_type)]
fwrite(y, 'group.csv')

x2 <- fread('group.csv')
colnames(x2)
x2[storey_range == '10 TO 12']
head(x2)

