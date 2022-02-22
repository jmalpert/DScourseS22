library(jsonlite)
library(tidyverse)
system('wget -O dates.json "http://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=202220219&lang=en"')
system('cat dates.json')
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)%>%print
head(mydf$date)%>%print

