yes <- levels(as.factor(db$o.indoor.))[1]
no <- levels(as.factor(db$o.indoor.))[2]
blank <- levels(as.factor(db$o.indoor.))[3]
standards <- c(yes,no,blank)
english.standards <- c("yes","no","blank")
correct <- function(input,standards,english.standards){
  if(is.vector(input)){
    this.1 <- as.factor(input)
    if (levels(this.1) %in% standards %>% sum() == 3){
      levels(this.1) <- english.standards
      return(this.1)
    }
  } else {
    print("your are not good to go")
  }
}

listofindexes <- list()
for (i in LETTERS[1:26]){
  listofindexes[[i]]<- paste0(i,LETTERS[1:26])
}
transition <- c(LETTERS[1:26],unlist(listofindexes))
which(transition=="G") # cities starting column 7
which(transition=="AK") # cities end column 37
# base <- db %>% 
#   select(g.city.,g.province.) %>% 
#   filter(g.province.=="فارس")
# ggplot(base,aes(x=g.city.))+
#   geom_bar(stat = "count")+
#   stat_count(geom = "text", colour = "red", size = 4,
#              aes(label = ..count..),position=position_stack(vjust=0.25))+
#   coord_flip()
# secon <- s %>% select(everything()) %>% filter(a<=15)
# test <- db
# test <- lapply(X = test,FUN = function(x) as.factor(x)) %>% as.data.frame()
# test <-lapply(test, function(x) gsub(x,pattern = "بلی",replacement = "yes"))
# test <-lapply(test, function(x) gsub(x,pattern = "بله",replacement = "yes"))
# test <-lapply(test, function(x) gsub(x,pattern = "خیر",replacement = "no"))

# p.url <- "https://cran.r-project.org/src/contrib/Archive/ConvCalendar/ConvCalendar_1.2.tar.gz"

#### working on dates ----
# p.url <- "https://cran.r-project.org/src/contrib/Archive/ConvCalendar/ConvCalendar_1.2.tar.gz"
# install.packages()
# install.packages(p.url,repos = NULL,type = "source")
# library(ConvCalendar)
# starting <- db$starting_date %>%
#   str_split_fixed(string = .,pattern = "-",n = 2)
# starting_date <- as_tibble(starting[,1])
# completion<- db$completion_date %>%
#   str_split_fixed(string = .,pattern = "-",n=2)
# test.begin <- as_datetime(db$starting_date)
# test.end <- as_datetime(db$completion_date)
# test.interval <- interval(start = test.begin,end = test.end)
# test.length <- int_length(test.interval)
# test.length[test.length<(15*60)] %>% plot.ecdf(x = .,pch = 1,cex=0.1)
test[str_which(test,"[:alpha:]")] <- NA
test[test=="4444"]<-NA
test[test=="6262"]<-NA

Encoding(test)
test %>% as.factor() %>% levels()
helper <- data.frame(v=test,e=Encoding(test))


View(helper)

str_which(test,"[:digit:]")

levels(db$p.p.code.)
levels(db$p.p.code.)[1] <- "1104"
levels(db$p.p.code.)[2] <- "1234"
levels(db$p.p.code.)[3] <- "1400"
levels(db$p.p.code.)[5] <- "1401"
levels(db$p.p.code.)[6] <- "1401"
levels(db$p.p.code.)[7] <- "1402"
levels(db$p.p.code.)[8] <- "1402"
levels(db$p.p.code.)[9] <- "1403"
levels(db$p.p.code.)[10] <- "1404"
levels(db$p.p.code.)[11] <- "1405"
levels(db$p.p.code.)[12] <- "1406"
levels(db$p.p.code.)[13:20] <- "9999"