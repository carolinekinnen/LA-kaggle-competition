library(tidyverse)
library(lubridate)
library(formattable)

directory <- "/Users/carolinekinnen/Downloads/CityofLA/Job Bulletins"
ndirectory <- "/Users/carolinekinnen/Desktop/R2/R2 project/Job Bulletins"

# Identify Files
file_name <- list.files(directory, pattern = ".txt")

files.to.read <- paste(directory, file_name, sep="/") 
files.to.write <- paste(ndirectory, paste0(sub(".txt","", file_name),".csv"), sep="/")



NAMES <- read.table(files.to.read[17], nrow = 1, stringsAsFactors = FALSE, sep = ",")
DATA <- read.table(files.to.read[17], 
                   skip = 1, 
                   stringsAsFactors = FALSE,
                  # sep = ",",
                  fill = TRUE)
DATA <- DATA[,1:51]
names(DATA) <- NAMES 



# Function to extract information
readin <- function(x) {
  #file <- read.delim(files.to.read[x], stringsAsFactors = FALSE)
  
  # file <- file %>%
  #   filter(grepl("Open Date: |must be received by|\\$|must be submitted on-line by |without prior notice|alary to be determined",
  #                file[[1]]),
  #          !grepl("he salary in the Department",
  #                 file[[1]]))
  # 
  # name <- file
  
  file <- file %>%
    mutate(value = mdy(file[1,1]),
           value2 = if_else(is.na(mdy(file[3,1])), 
                            "No notice", 
                            as.character(mdy(file[3,1]))))
  
  file[[1,1]] <- as.character(file[[1,2]])
  file[[3,1]] <- as.character(file[[3,3]])

  file <- file %>%
    select(-c(value, value2)) %>%
    mutate(key = c("open_date", "salary_range", "close_date"))

    file <- file %>%
    spread(key = key, value = colnames(name)) %>%
    mutate(job = colnames(name),
           close_date = if_else(is.na(ymd(close_date)),
                                "No Notice",
                                as.character(ymd(close_date))),
           open_date = ymd(as_date(open_date))) %>%
    mutate(salary_low = if_else(grepl("[[:digit:]]", salary_range),
                                as.character(as.numeric(str_remove(
                                  str_remove(word(salary_range),
                                             ","), "\\$"))),
                                "No salary"),
           salary_high = if_else(grepl("[[:digit:]]", salary_range),
                                 as.character(as.numeric(str_remove(
                                   str_remove(word(salary_range, str_count(salary_range, "\\S+")), ","), "\\$"))),
                                 "No salary")) %>%
    select(-salary_range) %>%
    mutate(median_salary = if_else(grepl("[[:digit:]]", salary_low),
                                     as.character(((as.double(salary_high) + as.double(salary_low))/2)),
                                     "No salary")) %>%
    mutate(days_open = as.period(as_date(close_date) - open_date))
    
    file
}


readin2 <- function(x) {
  
  file <- read.delim(files.to.read[x], stringsAsFactors = FALSE)
  
  file <- file %>%
    filter(grepl("Open Date: |must be received by|\\$|must be submitted on-line by |without prior notice|alary to be determined|a.m. Friday, ",
                 file[[1]]),
           !grepl("he salary in the Department",
                  file[[1]]))
  
  name <- file
  
  ifelse(nrow(file) == 3 , 
          readin(file),
          print("too big"))
  
}

# need it to throw a warning for multiple open dates
# if file has more than 5 rows -> extract differently

list <- map(10:18, readin)



three <- read.delim(files.to.read[3], stringsAsFactors = FALSE)

three <- three %>%
  filter(grepl("Open Date: |must be received by|\\$|must be submitted on-line by |without prior notice|alary to be determined|a.m. Friday, ",
              three[[1]])) 
# select the rows we care about

three <- three %>%
  filter(!(grepl("he salary in the Department", 
                 three[[1]])))
# unselect info about flat rated Department salary

name <- three
# leave this to extract the job title from later

three <- three %>%
  mutate(value = mdy(three[1,1]),  # to add open date 
         # works as long as Open Date is in this format -> 
         # Open Date:  04-18-14
         value2 = if_else(is.na(mdy(three[3,1])), 
                             "No notice", 
                             as.character(mdy(three[3,1]))))  # to add close date
# works as long as close date is in this format -> 
# Applications must be received by THURSDAY, MAY 1, 2014.

three[[1,1]] <- as.character(three[[1,2]])
# moves open date to be in first column with close date and salary
three[[3,1]] <- as.character(three[[3,3]])
# moves close date to be in first column with salary

three <- three %>%
  select(-c(value, value2)) %>%
  mutate(key = c("open_date", "salary_range", "close_date"))

three <- three %>%
  spread(key = key, value = colnames(name)) %>%
  mutate(job = colnames(name),
         close_date = if_else(is.na(ymd(close_date)),
                              "No Notice",
                              as.character(ymd(close_date))),
         open_date = ymd(as_date(open_date))) %>%
  mutate(salary_low = if_else(grepl("[[:digit:]]", salary_range),
                              as.character(as.numeric(str_remove(
                                str_remove(word(salary_range),
                                           ","), "\\$"))),
                              "No salary"),
         salary_high = if_else(grepl("[[:digit:]]", salary_range),
                               as.character(as.numeric(str_remove(
                                 str_remove(word(salary_range, str_count(salary_range, "\\S+")), ","), "\\$"))),
                               "No salary")) %>%
  select(-salary_range) %>% 
  mutate(median_salary = if_else(grepl("[[:digit:]]", salary_low),
                                 as.character(((as.double(salary_high) + as.double(salary_low))/2)),
         "No salary")) %>%
  mutate(days_open = as.period(as_date(close_date) - open_date))





# could potentially be used to read multiple files in
one <- read.csv(files.to.read[1], skip = 4, 
                #nrows = 49, 
                fill = TRUE, 
                blank.lines.skip = TRUE, 
                col.names = c(1,2,3,4,5))

for (i in 1:length(files.to.read)) {
  temp <- (read.csv(files.to.read[i], header = TRUE, skip = 11, fill = TRUE))
  write.csv(temp, file = files.to.write[i])
}

