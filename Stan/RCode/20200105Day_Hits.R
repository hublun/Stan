library(readr)

pu <- read_csv("Dropbox/Stan/Data/page_usage.csv", 
                col_types = cols(day = col_integer(), 
                time = col_time(format = "%H:%M:%S")))

head(pur)
pu$u <- as.integer(str_extract(pu$userID, "\\d+"))
pu$p <- as.integer(str_extract(pu$pageID, "\\d+"))
#=================== ================================
library(tidyr)
library(dplyr)
pu <- pu %>%
  separate(time, c("h","m","s"), ":")
#===================================================
pu$h <- as.integer(pu$h)
pu$m <- as.integer(pu$m)
pu$s <- as.integer(pu$s)
#==================================================
pud <- pu %>%
  group_by(u, p, day, h) %>%
  count()
sum(pud$n)
puu <- pud %>%
  group_by(u,p) %>%
  count()
sum(puu$n)
puu[puu$u==1120,]
#==================================================
u1120 = which(pud$u==1120&pud$p==49)
u1120
pu1120 = pud[u1120,]
pu1120[order(pu1120$day),]

ggplot(data=pu1120, aes(x=day), y=counts) + geom_bar()

       