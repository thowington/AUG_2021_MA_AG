# evaluate changes in WMA reporting update
library(readxl)
library(dplyr)

oldfile_4b = "C:/Users/Tim/Downloads/Documents/business_docs/SMB_work/Aug_2021_MA_AG/Eversource_raw_responses/Attachment AG-Oversight-4(b) CONFIDENTIAL.xlsx"
newfile_4b = "C:/Users/Tim/Downloads/Documents/business_docs/SMB_work/Aug_2021_MA_AG/Eversource_raw_responses/Attachment AG-Oversight-4(b) CONFIDENTIAL - Refile 01.21.22.xlsx"


############ competitive suppliers ################
old_4b_cs <- read_excel(oldfile_4b,sheet = " Q4 Competitive Supplier a")
dim(old_4b_cs)
# remove one false column
old_4b_cs <- old_4b_cs[,1:8]
# remove blank lines
old_4b_cs <- old_4b_cs[complete.cases(old_4b_cs),]
dim(old_4b_cs)


new_4b_cs <- read_excel(newfile_4b,sheet = " Q4 Competitive Supplier a")
dim(new_4b_cs)
new_4b_cs <- new_4b_cs[complete.cases(new_4b_cs),]
dim(new_4b_cs)

changes_4b_cs <- old_4b_cs %>% setdiff(new_4b_cs)
dim(changes_4b_cs)

# summary of changes to 4b_cs
summarize_cs <-function(test_file){
  total_accts <- sum(test_file$`Number of Accounts`)
  total_kwh <- sum(test_file$`Total KWH`)
  total_billings <- sum(test_file$`Supplier Rate`*test_file$`Total KWH`)
  number_suppliers <- length(unique(test_file$`Competitive Supplier`))
  number_towns <- length(unique(test_file$'Town'))
  print(c(as.character(total_accts), 
          as.character(total_kwh), 
          as.character(total_billings), 
          as.character(number_suppliers), 
          as.character(number_towns)))
  return(total_accts)
}

summarize_cs(old_4b_cs)
summarize_cs(new_4b_cs)
summarize_cs(changes_4b_cs)

# Q4 Municipal Aggregator b
old_4b_agg <- read_excel(oldfile_4b,sheet = "Q4 Municipal Aggregator b")
dim(old_4b_agg)
colnames(old_4b_agg)

new_4b_agg <- read_excel(newfile_4b,sheet = "Q4 Municipal Aggregator b")
dim(new_4b_agg)
# remove one false column
new_4b_agg <- new_4b_agg[,1:5]
colnames(new_4b_agg)

changes_4b_agg <- old_4b_agg %>% setdiff(new_4b_agg)
#  aggregator name was filled in for the revised response
# the additions to the agg tab were Whhtely (456 accts) and South Deerfield (1010 accts)

summarize_agg <-function(test_file){
  total_accts <- sum(test_file$`Number of Accounts`)
  number_aggregators <- length(unique(test_file$`Aggregator`))
  number_towns <- length(unique(test_file$'Town'))
  print(c("total_accts:", as.character(total_accts), 
          "number_aggregators", as.character(number_aggregators), 
          "number_towns", as.character(number_towns)))
  return(total_accts)
} 

summarize_agg(old_4b_agg)
summarize_agg(new_4b_agg)
summarize_agg(changes_4b_agg)


# Q4 Basic Service c
old_4b_basic <- read_excel(oldfile_4b,sheet = "Q4 Basic Service c")
dim(old_4b_basic)
colnames(old_4b_basic)

new_4b_basic <- read_excel(newfile_4b,sheet = "Q4 Basic Service c")
dim(new_4b_basic)
colnames(new_4b_basic)

changes_4b_basic <- old_4b_basic %>% setdiff(new_4b_basic)
dim(changes_4b_basic)

assertthat::are_equal(x=dim(old_4b_basic),y=dim(new_4b_basic))

# determine if old totals of cs and agg equal new totals of cs and agg
assertthat::are_equal(x=(summarize_cs(old_4b_cs)+summarize_agg(old_4b_agg)),
                      y=(summarize_cs(new_4b_cs)+summarize_agg(new_4b_agg)))
