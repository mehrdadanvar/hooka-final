#### importing libraries and setting the working directory----
# on your machine the working directory is different

required.packaes <- c("tidyverse","readxl","stringi")
for(i in required.packaes){
  if(i %in% as.data.frame(installed.packages())$Package){
    print(paste0("Package ",i," is installed"))
  } else {
      install.packages(i,dependencies = T)
    }
}
lapply(required.packaes, require, character.only = TRUE)
setwd("/home/mehrdad/Desktop/firstclone/hooka-linux/")
#### importing the source excel spread sheet and constructing names----
hooka <- read_excel(path = "./raw/06-06-2021.xlsx"
                    ,sheet = "Results",col_names = T)
                    #could be any directory depending on your machine
namesonly <- read_excel(path ="./raw/06-06-2021.xlsx",
                        sheet = "Results",
                        col_names = F)[1:3,]
# reading all cells from the first three rows of the data set
#and merging all of them and removing the NAs
all.names <- paste0(namesonly[1,],
                    namesonly[2,],
                    namesonly[3,]) %>% gsub(x = .,pattern = "NA",replacement = "")
#### moving from excel AA AB col indexes to proper numbers----
listofindexes <- list()
for (i in LETTERS[1:26]){
  listofindexes[[i]]<- paste0(i,LETTERS[1:26])
}
transition <- c(LETTERS[1:26],unlist(listofindexes))
which(transition=="G") # cities starting column 7
which(transition=="AK") # cities end column 37
#### removing the first three rows using the function cleanDataSet----
cleanDataSet <- function(input){
  startingpoint <- 3
  stoppingpoint <- dim(input)[1]
  output <- input[startingpoint:stoppingpoint,]
  names(output)[1] <- "ID"
  na.per.row <- data.frame(ID=output[,1],
                           nas=apply(output, 1, function(x) sum(is.na(x))))
  #state.1
  this.state.1 <- left_join(x = output,y = na.per.row,by="ID")
  #state.2
  #now remove respondents with all na answers
  this.state.2 <- this.state.1 %>% 
    select(everything()) %>% 
    filter(nas<170)
  names(this.state.2) <- c(all.names,"nas")
  # now get two clean columns of provinces and cities to be attached at the end of the data-set
  #state.3
  cities <- this.state.2[,7:37] #indexes for cities
  positions<- apply(cities,1, function(z) which(!is.na(z)))
  cities <- as_tibble(cbind(cities,positions=positions))
  newcitis <- list()
  for (i in 1:nrow(cities)){
    newcitis[[i]] <- cities[i,positions[i]]
  }
  final.city.col<- unlist(newcitis) %>% as.vector()
  cbind(province=this.state.2[,6],city=final.city.col) %>% as_tibble() -> cleanedCities
  rm(final.city.col,i,newcitis,cities)
  this.state.3 <- cbind(this.state.2,cleanedCities)
  #write.csv(x=this.state.3,file = paste0(getwd(),"/","this.state.3",".csv"))
  #state.4
  #removing columns 7 to 37 as they are the spread from of cities and no longer needed
  this.state.4_part.one <- this.state.3[,c(1,2,3)]
  this.state.4_part.two <- this.state.3[,c(177,178,179)]
  this.state.4_part.three <- this.state.3[,-c(1:5,6:37,177:179)]
  this.state.4 <- cbind(this.state.4_part.one,this.state.4_part.two,this.state.4_part.three)
  #write.csv(x=this.state.4,file = paste0(getwd(),"/","this.state.4",".csv"))
  #state.5
  #sets the names to english
  this.state.5 <- this.state.4
  farsi.names <- data.frame(farsi=names(this.state.4),
                      some=as.data.frame(str_split_fixed(names(this.state.4),
                                                         pattern = "-",n = 3)))
  english.names <-read.csv(file = "./needs/mehrdad.csv",header = T)
  repaired.both <- cbind(english.names,farsi.names)
  final.names <- paste0(repaired.both[,3],".",repaired.both[,4],".",repaired.both[,5])
  names(this.state.5) <- final.names
  out <- names(this.state.5)
  remover <- ifelse(test=stri_endswith_fixed(out,"."),
                    yes = str_sub(out,1,end = str_length(out)-1),
                    no = out)
  names(this.state.5) <- remover
  #state.6
  #state.6
  #removing entries in questioner area with invalid or smaller than 10 inputs
  # this.state.4 %>%
  #   select(everything()) %>% 
  #   filter()
  return(this.state.5)
}
db <- cleanDataSet(hooka)

#recoding from Perian to English gender variable
db$g.gender <- as.factor(db$g.gender)
levels(db$g.gender)[1] <- "F"
levels(db$g.gender)[2] <- "M"
#converting the age variable from character to integer
db$g.age <- as.integer(db$g.age)
# education
db$g.education <- as.factor(db$g.education)
levels(db$g.education) <- c("elementary","No education","pre-university","High School","PhDs and above","diplomas",
                    "middle","Bachelors","Masters","movement","Conservatory")
print("DATA CLEANING WAS SUCCESSFULL, NEGLECT ALL THE ABOVE GENERATED WARNINGS")
#write.csv(db,file = "cleaned.csv",row.names = F)

#####slicing translated labels from google ----
new.list <- discard(lapply(db, function(z) levels(as.factor(z))),function(y) length(y)>11)
write(x = unlist(new.list),file = "./needs/test.txt")
# upload this test.txt file to google and translate from farsi to English
#save the google's out put in csv format
#name = google.translated.csv and load it in to R environment 
translated.levels <- read.csv(file = "./needs/googletranslated.csv",header = F)$V1
mehrdad <- c(sapply(new.list,length))
stops <- c()
steps <- length(mehrdad)-1
for(i in 1:steps){stops[i]<-sum(mehrdad[1:i])}
starts <- c()
for(i in 1:steps){starts[i]<-sum(mehrdad[1:i])+1}
starts <- c(1,starts)
mapper <- data.frame(starts=starts,stops=c(stops,length(translated.levels)))
####apply new lables ----
db.good <- list()
for(i in 1:dim(mapper)[1]){
  db.good[[i]] <- translated.levels[mapper[i,1]:mapper[i,2]]
}
db.bad <- discard(lapply(db, function(z) levels(as.factor(z))),function(y) length(y)>11)


names(db.good)<-names(db.bad)
for(i in 1:135){
  db.bad[i]<-db.good[i]
}
test <- lapply(db[names(db.good)],as.factor)
for (i in 1:length(test)){
  levels(test[[i]])<-db.good[[i]]
}
test <- as.data.frame(test)
db.small <- db[which(!names(db) %in% names(db.good))]
db.final <- cbind(test,db.small)
db.final <- db.final[names(db)]
db.final$h.age <- as.integer(db.final$h.age)
db.final$c.age <- as.integer(db.final$c.age)
rm(db.good,db.bad,db.small)

# removing intermediate objects no longer needed
rm(test,
   new.list,
   mapper,
   listofindexes,
   names.farsi,
   names.repaired.both,
   names.repaired.english,
   stops,
   starts,
   transition,
   translated.levels,mehrdad,db)


#### function for returning compact and combined variables
cc <- function(input){
  #step one is to cut a slice of the parent data to work on, we call it cake.slice  
  cake.slice <- db.final[,str_which(names(db.final),pattern = as.character(input))]
  # we then define the number of columns across which choices are spread
  choices <- apply(cake.slice, 1,function(x) sum(!is.na(x)))
  # combine all the columns across which the data was spread
  cake.slice %>% 
    unite(combined, c(names(cake.slice)), sep = "-", remove = T) -> cake.slice
  # then replace NA with nothing for future convenience
  cake.slice$combined <- gsub(cake.slice$combined,pattern = "NA",replacement = "")
  # if the respondent was not in this category at all we make NA
  inter.cake <- ifelse(choices==0,NA,cake.slice$combined)
  cake.slice$combined <- inter.cake
  # define the output for this function and polish the names
  output <- data.frame(cake.slice$combined,choices)
  names(output)[1] <- paste0(input,".combined")
  names(output)[2] <- paste0(input,".choices")
  return(output)
}


#### working on dates ----
# step one is splitting the date component from the time component for further user
m.start <-db.final$p.date.start
m.start <- str_split_fixed(m.start,"-",2) %>% as.data.frame()
m.start$ID <- db.final$g.id
names(m.start) <- c("start.date","start.time","ID")
m.finish <- db.final$p.date.finish
m.finish <- str_split_fixed(m.finish,"-",2) %>% as.data.frame()
m.finish$ID <- db.final$g.id
names(m.finish) <- c("finish.date","finish.time","ID")
# rejoin them and paste at the end of the output object
rejoined <- bind_cols(m.start,m.finish)
rejoined <- rejoined[,-c(3,6)]
# step two calculate the time difference between start completion of the answer and append
library(lubridate)
as_datetime(db.final$p.date.start)->test.s
as_datetime(db.final$p.date.finish)->test.f
test.interval <- interval(test.s,test.f)
test.len <- int_length(test.interval)


#### reconstructing 
cutters <-c("n.why.","c.reason.use","cq.reason.quit","cq.quit.negative","h.main.reason","hq.reason.quit","hq.quit.negative")
db.out <- db.final %>%
  select(starts_with(c("g","o","n.previous.use"))) %>%
  bind_cols(.,cc("n.why")) %>% 
  bind_cols(.,db.final %>% select(n.friend.use:n.offer.accept)) %>% 
  bind_cols(.,db.final %>% select(c.age)) %>% 
  bind_cols(.,cc("c.reason.use")) %>% 
  bind_cols(.,db.final %>% select(c.use.quantity:c.suggest.family.friends)) %>% 
  bind_cols(.,db.final %>% select(cq.willing.quit:cq.quit.commit)) %>% 
  bind_cols(.,cc("cq.reason.quit")) %>% 
  bind_cols(.,db.final %>% select(cq.quit.times:cq.quit.methods)) %>% 
  bind_cols(.,cc("cq.quit.negative")) %>% 
  bind_cols(.,db.final %>% select(cq.ca.quit.again:h.age)) %>%
  bind_cols(.,cc("h.main.reason")) %>% 
  bind_cols(.,db.final %>% select(h.use.quantity:hq.quit.commit)) %>% 
  bind_cols(.,cc("hq.reason.quit")) %>% 
  bind_cols(.,db.final %>% select(hq.quit.times:hq.quit.methods)) %>% 
  bind_cols(.,cc("hq.quit.negative")) %>% 
  bind_cols(.,db.final %>% select(hq.ca.quit.again:p.date.finish))
#attaching previous date variables
db.out <- cbind(db.out,test.s,test.f,test.len,rejoined)
save(db.out,file = "hooka.Rdata")
save(db.out,file = "shiny/hooka.Rdata")




