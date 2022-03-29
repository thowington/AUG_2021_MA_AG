# This begins with the final code of March 5, 2021.

# data input 
library(readr)
library(readxl)

# data mgmt
library(dplyr)
library(stringr)
library(tidyr)
library(zipcodeR)
library(reshape2)
library(data.table)
library(dtplyr)
library(lubridate)


# data visuals
library(lfe)
library(ggplot2)
library(scales)
library(stargazer)
library(ggrepel)

setwd("../cleaned_responses")

# custom functions ----
# clean_muni_names ----
clean_muni_names=function(df){
  df %>% 
    mutate(municipality=str_to_title(municipality)) %>% 
    left_join(fread("clean_muni_names.csv")) %>% 
    mutate(municipality=ifelse(!is.na(clean_muni),clean_muni,municipality)) %>% 
    select(-clean_muni)
}

# clean_supplier_names ----
clean_supplier_names=function(df){
  df %>% 
    left_join(fread("clean_supplier_names.csv")) %>% 
    mutate(supplier=ifelse(!is.na(corrected_supplier),corrected_supplier,supplier)) %>% 
    select(-corrected_supplier)
}


# # change neighborhoods to municipalities ----
neighborhood_to_municipality=function(df){
  df %>%
    left_join(fread("neighborhood_to_municipality.csv")) %>%
    mutate(neighborhood = municipality,
           municipality=ifelse(!is.na(corrected_municipality),corrected_municipality,municipality)) %>%
    select(-corrected_municipality)
}

today = today()

# full year summary ----
# get the name of all files in the folder, and then the ones matching my raw data filenames
all_files=list.files()
monthly_cs_files=grep("^\\w+_q[1,2]_cs.xlsx",all_files,value=T)
monthly_basic_files=grep("^\\w+_q[1,2]_basic.xlsx",all_files,value=T)
monthly_agg_files=grep("^\\w+_q[1,2]_agg.xlsx",all_files,value=T)

# get basic rates from excel
basic_rates=read_excel("Basic_Rates_TH.xlsx")

# neighborhood-municipality correspondence
#neighborhoods<-read_excel("Municipality_Neighborhoods.xlsx")

# for (i in (1 : length(monthly_cs_files))) {
#   thisfile = read_excel(monthly_cs_files[i],col_types = c("date","text","numeric","numeric","numeric","numeric","numeric","text","text","numeric"))
#   print(monthly_cs_files[i])
#   print(colnames(thisfile))
#   print(summary(thisfile))
# }
# # 
# for (i in (1 : length(monthly_basic_files))) {
#   thisfile = read_excel(monthly_basic_files[i],col_types = c("date","numeric","numeric","numeric","text","text"))
#   print(monthly_basic_files[i])
#   print(colnames(thisfile))
#   print(summary(thisfile))
# }
# for (i in (1 : length(monthly_agg_files))) {
#   thisfile = read_excel(monthly_agg_files[i],col_types = c("date","text","text","numeric","numeric","numeric","numeric","text","text"))
#   print(monthly_agg_files[i])
#   print(colnames(thisfile))
#   print(summary(thisfile))
# }
  

# read each monthly CS excel into one big file
monthly_cs=lapply(monthly_cs_files,function(x){
  return(read_excel(x,col_types = c("date","text","numeric","numeric","numeric","numeric","numeric","text","text","numeric")))
})


## 
monthly_cs=bind_rows(monthly_cs) %>% 
  filter(date > '2020-06-01') %>%
  group_by(date,supplier,rate,region,income) %>%
  summarize(kwh=sum(kwh),
            amt_billed=sum(amt_billed),
            no_accts=sum(no_accts),
            new_accts=sum(new_accts)) 

# Remove records where kwh = 0 and number of accts = 0
monthly_cs <- monthly_cs %>% filter(kwh>0 & no_accts > 0)

# Some records have kwh>0, but no accts.  
# Change no_accts from 0 to 1 where it is currently 0 but kwh>0
monthly_cs[which(monthly_cs$no_accts ==0 & monthly_cs$kwh >0),]$no_accts <- 1

# this is used to fill a gap in reporting.
# Unitil did not provide the kwh in the zip code files, so I use these averages
# to estimate it down below.
FIT_low_avg_kwh_Sept20<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2020-09-01") %>%
  filter(income == "low") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_low_avg_kwh_Sept20 <- as.integer(FIT_low_avg_kwh_Sept20)

FIT_all_avg_kwh_Sept20<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2020-09-01") %>%
  filter(income == "all") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_all_avg_kwh_Sept20 <- as.integer(FIT_all_avg_kwh_Sept20)


# create monthly_cs_welfare ----
monthly_cs_welfare=left_join(monthly_cs,basic_rates, by = c("date", "region")) %>%
  mutate(basic_bill=kwh*basic_rate,  
         bill_difference=amt_billed-basic_bill,  
         bill_difference_pp=bill_difference/no_accts,  
         rate_difference=rate-basic_rate,
         kwh_per_acct=ifelse(kwh==0|no_accts==0,0,kwh/no_accts))

# check for NAs
summary(monthly_cs_welfare)


# read each basic customer excel into one big file
monthly_basic=lapply(monthly_basic_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","numeric","numeric","text","text")))
})
monthly_basic=bind_rows(monthly_basic)

colnames(monthly_basic)=c("date","basic_kwh","basic_billed","basic_accts","region","income")

# check for NAs
summary(monthly_basic)


# read each agg excel into one big file
# requires date, region, total_billed, kwh, number_accts, region, income
monthly_agg=lapply(monthly_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","numeric","numeric","numeric","numeric","text","text")))
})
monthly_agg=bind_rows(monthly_agg) %>% 
  group_by(date,region,income) %>% 
  summarize(kwh=sum(kwh),
            amt_billed=sum(amt_billed),
            no_accts=sum(no_accts)) 

colnames(monthly_agg)=c("date","region","income","agg_kwh","agg_billed","agg_accts")

# check for NAs
summary(monthly_agg)



## lineplot of CS rates over time, and basic tiers  ----
yearly_summary=monthly_cs_welfare %>% 
  group_by(date,income) %>%
  summarize(avg_rate=sum(kwh*rate)/sum(kwh),
            avg_basic_rate=sum(kwh*basic_rate)/sum(kwh),
            kwh=sum(kwh),
            amt_billed=sum(amt_billed)) 

yearly_summary_o=yearly_summary %>% 
  group_by(date) %>% 
  summarize(kwh=max(kwh)-min(kwh),
            amt_billed=max(amt_billed)-min(amt_billed),
            income="other") %>% 
  mutate(avg_rate=amt_billed/kwh)

yearly_summary=bind_rows(yearly_summary,yearly_summary_o) %>% 
  ungroup() %>% 
  mutate(rate=ifelse(income=="all",avg_basic_rate,avg_rate),
         income=factor(income,levels=c("low","other","all"))) %>%
  ungroup()


plot_dir = "../plots/"

p2<- ggplot(yearly_summary,aes(x=date,y=rate,color=factor(income))) +
  geom_step(size=2) +
  scale_y_continuous(labels=dollar,limits=c(0.075,.16)) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL,
                       labels=c("\nCompetitive: \nLow income \n",
                                "\nCompetitive: \nNon-low \nincome \n",
                                "\nBasic \nservice \n")) +
  labs(x=NULL,y="State-wide average rate (dollars per kWh)") 
file_name = paste(plot_dir,"Statewide_average_rate.png", sep = "")
png(file_name, width = 1500, height = 1000)
print(p2)
dev.off()
p2

write.csv(yearly_summary, "../output/rates_data.csv")




# annual summary ----

net=monthly_cs_welfare %>% 
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="net")

over=filter(monthly_cs_welfare,bill_difference>0) %>%  
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="over")

under=filter(monthly_cs_welfare,bill_difference<0) %>%  
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="under")

filename = paste0("../output/annual_summary_",today,".csv")
write_csv(bind_rows(net,under,over),filename)

# cs_vs_basic_agg ----
# estimate participation in competitive supply relative to basic and agg
monthly_basic <- monthly_basic[,c(1,2,3,4,5,6)]

cs_vs_basic_agg=monthly_cs_welfare %>% 
  group_by(region,income,date) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            cs_kwh=sum(kwh)) %>%
  left_join(monthly_basic) %>% 
  left_join(monthly_agg) %>% 
  group_by(income) %>% 
  summarize(amt_billed=sum(amt_billed),
            no_accts=sum(no_accts),
            cs_kwh=sum(cs_kwh),
            tot_bill_difference=sum(tot_bill_difference),
            basic_billed=sum(basic_billed),
            basic_accts=sum(basic_accts),
            basic_kwh=sum(basic_kwh),
            agg_billed=sum(agg_billed),
            agg_accts=sum(agg_accts),
            agg_kwh=sum(agg_kwh)) %>% 
  mutate(pct_cs_accts=no_accts/(no_accts+basic_accts+agg_accts),
         tot_accts=no_accts+basic_accts+agg_accts,
         pct_cs_billed=amt_billed/(amt_billed+basic_billed+agg_billed),
         pct_cs_kwh=cs_kwh/(cs_kwh+basic_kwh+agg_kwh),
         tot_kwh=cs_kwh+basic_kwh+agg_kwh)

filename = paste0("../output/cs_vs_basic_agg_",today,".csv")
write_csv(cs_vs_basic_agg,filename)




# supplier key ----
supplier_key=monthly_cs_welfare %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  select(supplier,supplier_id) %>%
  arrange(supplier_id) %>% 
  distinct()
filename = paste0("../output/supplier_key_",today,".csv")
write_csv(supplier_key,filename)

supplier_key_low=monthly_cs_welfare %>%
  filter(income == "low") %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  select(supplier,supplier_id) %>%
  arrange(supplier_id) %>% 
  distinct()

monthly_cs_welfare %>% 
  group_by(supplier) %>% 
  summarize(avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            no_accts=sum(no_accts,na.rm=T)) %>% 
  mutate(df=avg_rate_by_kwh-avg_basic_by_kwh) 

#rm(supplier_key)

# supplier summary ----
suppliers=monthly_cs_welfare %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
  group_by(supplier_id,income) %>%
  summarize(no_months=length(unique(date)),
            first_month=min(date),
            last_month=max(date),
            avg_rate_by_accts=sum(rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
            avg_basic_by_accts=sum(basic_rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
            avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            net_loss=sum(bill_difference,na.rm=T),
            sum_overchargebills=sum(bill_difference[which(bill_difference>0)]),
            sum_underchargebills=sum(bill_difference[which(bill_difference<0)]))%>%
  ungroup() %>%
  mutate(avg_premium_by_accts=net_loss/no_accts,
         avg_premium_by_kwh=net_loss/kwh) %>% 
  group_by(income) %>%
  mutate(share_of_accts=no_accts/sum(no_accts),
         share_of_kwh=kwh/sum(kwh),
         share_of_overcharge=sum_overchargebills/sum(sum_overchargebills),
         share_of_undercharge=sum_underchargebills/sum(sum_underchargebills))
filename = paste0("../output/supplier_summary_",today,".csv")
write_csv(suppliers,filename)


# rates by top three suppliers ----
# Previously the top three supplier were hard-coded as VIRID, CON E, and CONSO.
# This was based on total number of accounts by supplier.
# Now the top three are different.

supplier_totals <- suppliers %>% group_by(supplier_id) %>% summarise(total_accts = sum(no_accts))
top_suppliers_ordered <- supplier_totals %>% arrange(desc(total_accts))
top_three_suppliers <- top_suppliers_ordered[1:3,]
top_three_suppliers
                                     
# In 2019 they were NRG R, DIRECT, & CONST.
# VIRID dropped well down, with only 242907 accounts.
# 2020 top three are CONST, NRG R, CLEAN





# ---- census data ----
# Note that some census rows have NA in the raw data

census_english=read_csv("../ACS_data/percent_lep_hh_2019.csv",na="-") %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         pct_lep_hh = as.numeric(pct_lep_hh))
         
census_race=read_csv("../ACS_data/percent_nonwhite_2019.csv")  %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         pct_nonwhite = as.numeric(pct_nonwhite))

census_poverty=read_csv("../ACS_data/percent_poverty_2019.csv",na=c("-","(X)")) %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         pct_poverty = as.numeric(pct_poverty)) 

census_income=read_csv("../ACS_data/median_HH_Income_2019.csv",na=c("-","(X)")) %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         Median_HH_Income = as.numeric(Median_HH_Income)) 

census_disability=read_csv("../ACS_data/percent_disablility_2019.csv",na=c("-","(X)")) %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         pct_disability = as.numeric(pct_disability)) 

census_age=read_csv("../ACS_data/age_65_plus_2019.csv",na=c("-","(X)")) %>%
  mutate(zcta = paste0("0",as.character(zcta)),
         pct_65_plus = as.numeric(pct_65_plus)) 

xwalk=read_excel("zip_to_zcta.xlsx") 


# ---- zipcode analysis - most disaggregated ----

all_files=list.files()
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

# 
# for (i in (1 : length(zipcode_agg_files))) {
#   thisfile = read_excel(zipcode_agg_files[i],col_types = c("date","text","text","text","numeric","text","text"))
#   print(zipcode_agg_files[i])
#   print(colnames(thisfile))
#   print(summary(thisfile))
# }
# 
# for (i in (1 : length(zipcode_cs_files))) {
#   thisfile = read_excel(zipcode_cs_files[i],col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text"))
#   print(zipcode_cs_files[i])
#   print(colnames(thisfile))
#   print(summary(thisfile))
# }


basic_rates=read_excel("Basic_Rates_TH.xlsx")


municipal_light_plants <- read.csv("../other_inputs/municipal_light_plants.csv", header = TRUE)
light_plant_municipalities <- unique(municipal_light_plants$Town.Name)
light_plant_municipalities



zipcode_basic=lapply(zipcode_basic_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  rename(no_basic_accts=no_accts) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality() %>%
  group_by(zip,municipality,region,income) %>%
  summarize(no_basic_accts = sum(no_basic_accts)) %>%
  #select(-date) %>%
  clean_muni_names()

summary(zipcode_basic)


zipcode_agg=lapply(zipcode_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  group_by(zip,municipality,region,income) %>%
  summarize(no_agg_accts=sum(no_accts)) 

summary(zipcode_agg)

# TH added a col for kwh, and changed calculation of avg_cs_rate to use kwh for weighting, rather than no_accts
zipcode_cs=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

# fill in gaps for FIT region
zipcode_cs[which(zipcode_cs$region == "FIT" & zipcode_cs$income == "low"),]$kwh <- zipcode_cs[which(zipcode_cs$region == "FIT" & zipcode_cs$income == "low"),]$no_accts*FIT_low_avg_kwh_Sept20
zipcode_cs[which(zipcode_cs$region == "FIT" & zipcode_cs$income == "all"),]$kwh <- zipcode_cs[which(zipcode_cs$region == "FIT" & zipcode_cs$income == "all"),]$no_accts*FIT_all_avg_kwh_Sept20

zipcode_cs = zipcode_cs %>%  
  filter(kwh>0) %>%
  filter(no_accts>0) %>%
  group_by(zip,municipality,region,income) %>%
  summarize(avg_cs_rate=sum(rate*kwh)/sum(kwh),
            no_cs_accts=sum(no_accts),
            new_cs_accts=sum(new_accts)) 

summary(zipcode_cs)

suppliers_by_zip=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality),
         supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  filter(income=="all") %>% 
  neighborhood_to_municipality() %>%
  group_by(zip,municipality,region,supplier_id) %>%
  summarize(suppliers=1) %>% 
  group_by(zip,municipality,region) %>%
  summarize(suppliers=n()) 

summary(suppliers_by_zip)

zips=full_join(zipcode_basic,zipcode_cs,by=c("zip","municipality","region","income")) %>%
  full_join(zipcode_agg,by=c("zip","municipality","region","income")) %>%
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date) 

summary(zips)
# Above generates some NAs because not every ZIP has all kinds of accounts

zips[is.na(zips$no_basic_accts),"no_basic_accts"]=0
zips[is.na(zips$no_cs_accts),"no_cs_accts"]=0
zips[is.na(zips$new_cs_accts),"new_cs_accts"]=0
zips[is.na(zips$no_agg_accts),"no_agg_accts"]=0

zips=zips %>%
  mutate(rate_dif=avg_cs_rate-basic_rate,
         tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) 


zips=recast(zips,zip+municipality+region~income+variable, sum) %>%
  rename(basic_rate=all_basic_rate) %>%
  mutate(low_basic_rate=NULL)

zips[is.na(zips$low_tot_accts),"low_tot_accts"]=0
zips[is.na(zips$low_no_basic_accts),"low_no_basic_accts"]=0
zips[is.na(zips$low_no_cs_accts),"low_no_cs_accts"]=0
zips[is.na(zips$low_new_cs_accts),"low_new_cs_accts"]=0
zips[is.na(zips$low_no_agg_accts),"low_no_agg_accts"]=0

# write_csv(zips, "../output/zips2.csv")



zips=zips %>%
  neighborhood_to_municipality() %>%
  group_by(zip,municipality,region) %>%
  summarize(basic_rate=sum(basic_rate*all_tot_accts)/sum(all_tot_accts),
            all_avg_cs_rate=sum(all_avg_cs_rate*all_no_cs_accts)/sum(all_no_cs_accts),
            all_rate_dif=ifelse(sum(all_no_cs_accts)>0,
                                    sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
                                    NA),
            all_tot_accts=sum(all_tot_accts),
            all_no_cs_accts=sum(all_no_cs_accts),
            all_no_agg_accts=sum(all_no_agg_accts),
            all_no_basic_accts=sum(all_no_basic_accts),
            low_avg_cs_rate=ifelse(sum(low_no_cs_accts)>0,
                                   sum(low_avg_cs_rate*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts),
                                   NA),
            low_avg_rate_dif=ifelse(sum(low_no_cs_accts)>0,
                                    sum(low_rate_dif*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts),
                                    NA),
            low_tot_accts=sum(low_tot_accts),
            low_no_cs_accts=sum(low_no_cs_accts),
            low_no_agg_accts=sum(low_no_agg_accts),
            low_no_basic_accts=sum(low_no_basic_accts)) %>%
  ungroup() %>%
  mutate(other_tot_accts=all_tot_accts-low_tot_accts,
         other_no_cs_accts=all_no_cs_accts-low_no_cs_accts,
         other_no_agg_accts=all_no_agg_accts-low_no_agg_accts,
         other_no_basic_accts=all_no_basic_accts-low_no_basic_accts,
         other_avg_cs_rate=ifelse(low_no_cs_accts>0&other_no_cs_accts>0,
                                  (all_no_cs_accts*all_avg_cs_rate-low_no_cs_accts*low_avg_cs_rate)/
                                    other_no_cs_accts,
                                  ifelse(low_no_cs_accts==0,all_avg_cs_rate,NA)),
         other_avg_rate_dif=other_avg_cs_rate-basic_rate,
         all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         other_pct_cs=ifelse(other_tot_accts>0,
                             other_no_cs_accts/other_tot_accts,
                             NA),
         agg_present=ifelse(all_no_agg_accts>0,1,0),
         pct_low_accts=low_tot_accts/all_tot_accts) %>%
  left_join(suppliers_by_zip,by = c("zip","municipality","region")) %>%
  left_join(xwalk) %>%
  left_join(census_english) %>%
  left_join(census_race) %>%
  left_join(census_income) %>%
  left_join(census_poverty) %>%
  left_join(census_age) %>%
  left_join(census_disability) %>%
  filter(!municipality %in% light_plant_municipalities)
filename = paste0("../output/master_zipcode_",today,".csv")
write_csv(zips,filename)




# make plot of participation rate versus low-income accounts ----
plotdata=filter(zips,low_tot_accts>9,
                municipality %in% c("Boston","Worcester",
                                    "East Boston","South Boston","S Boston",
                                    "Allston","Brighton","Charlestown",
                                    "Dorchester","Hyde Park","Jamaica Plain",
                                    "Mattapan","Roslindale","Roxbury","Roxbry Xng",
                                    "West Roxbury",
                                    "Springfield","Indian Orchard",
                                    "Worcester")) %>% 
  mutate(municipality=ifelse(municipality %in% c("East Boston","South Boston","S Boston",
                                                 "Allston","Brighton","Charlestown",
                                                 "Dorchester","Hyde Park","Jamaica Plain",
                                                 "Mattapan","Roslindale","Roxbury","Roxbry Xng",
                                                 "West Roxbury"),
                             "Boston",
                             municipality),
         municipality=ifelse(municipality %in% c("Indian Orchard"),
                             "Springfield",
                             municipality)) %>% 
  select(pct_low_accts,all_pct_cs,zip,municipality)
filename = paste0("../output/scatter_plot_",today,".csv")
fwrite(plotdata,file=filename)


# large for printing
theme_set(theme_gray(base_size = 50))
png(filename="../plots/low_income_vs_participation_rate.png",
    width=1.35*1250,
    height=1*1250)
ggplot(data=plotdata,aes(x=pct_low_accts,y=all_pct_cs,label=zip,color=municipality))+
  geom_text_repel(size=12,fontface="bold")+
  scale_x_continuous(labels=percent)+
  scale_y_continuous(labels=percent)+
  scale_color_discrete(name="Municipality",
                       labels=c("Boston","Springfield","Worcester")) +
  labs(x="Share of low income customers",y="Participation in the competitive supply market")+
  theme(legend.position = c(.89,.2),
        legend.margin=margin(2,1,1,1,"line"),
        legend.key.height=unit(4,"line"))
dev.off()


# ---- summaries by top 20 ----
maj_min=zips %>%
  filter(pct_nonwhite>.5,
         all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_nonwhite,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) %>%
  arrange(desc(pct_nonwhite)) 
summary=zips %>%
  filter(pct_nonwhite>.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Majority Minority",
            pct_nonwhite=sum(pct_nonwhite*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts,na.rm=T)/sum(other_tot_accts,na.rm=T))
rest_of_state=zips %>%
  filter(!(is.na(pct_nonwhite)), # 46 rows missing pct_nonwhite
         pct_nonwhite<=.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_nonwhite=sum(pct_nonwhite*all_tot_accts,na.rm=T)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/sum(all_tot_accts),
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts,na.rm=T)/sum(other_tot_accts,na.rm=T),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
maj_min=bind_rows(summary,rest_of_state,maj_min) %>% 
  mutate(other_pct_cs=ifelse(other_pct_cs<0,0,other_pct_cs))
filename = paste0("../output/majority_minority_",today,".csv")
write_csv(maj_min,filename)



top_25_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  head(.,n=25) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,Median_HH_Income,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  head(.,n=25) %>%
  summarize(zip="-",
            municipality="Top 25: Med HH Inc",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  filter(!is.na(Median_HH_Income)) %>% # 63 rows missing Median_HH_Income
  tail(.,n=nrow(.)-25) %>%
  summarize(zip="-",
            municipality="Rest of State",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_25_income=bind_rows(summary,rest_of_state,top_25_income)
filename = paste0("../output/top_25_income_",today,".csv")
write_csv(top_25_income,filename)


bottom_25_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  head(.,n=25) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,Median_HH_Income,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts)  
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  head(.,n=25) %>%
  summarize(zip="-",
            municipality="Bottom 25: Med HH Inc",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  filter(!is.na(Median_HH_Income)) %>% 
  tail(.,n=nrow(.)-25) %>%
  summarize(zip="-",
            municipality="Rest of State",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
bottom_25_income=bind_rows(summary,rest_of_state,bottom_25_income)
filename = paste0("../output/bottom_25_income_",today,".csv")
write_csv(bottom_25_income,filename)


top_20_lep=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_lep_hh,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct Lim English",
            pct_lep_hh=sum(pct_lep_hh*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  filter(!is.na(pct_lep_hh)) %>% # 48 rows missing pct_lep_hh
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_lep_hh=sum(pct_lep_hh*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_lep=bind_rows(summary,rest_of_state,top_20_lep)
filename = paste0("../output/top_20_lep_",today,".csv")
write_csv(top_20_lep,filename)



top_20_65_plus=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_65_plus)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_65_plus,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_65_plus)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct 65 Plus",
            pct_65_plus=sum(pct_65_plus*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_65_plus)) %>%
  filter(!is.na(pct_65_plus)) %>% 
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_65_plus=sum(pct_65_plus*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_65_plus=bind_rows(summary,rest_of_state,top_20_65_plus)
filename = paste0("../output/top_20_65_plus_",today,".csv")
write_csv(top_20_65_plus,filename)


top_20_poverty=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_poverty)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_poverty,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_poverty)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct families in poverty",
            pct_poverty=sum(pct_poverty*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_poverty)) %>%
  filter(!is.na(pct_poverty)) %>% 
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_poverty=sum(pct_poverty*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_poverty=bind_rows(summary,rest_of_state,top_20_poverty)
filename = paste0("../output/top_20_poverty_",today,".csv")
write_csv(top_20_poverty,filename)


top_20_disability=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_disability)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_disability,
         all_tot_accts,all_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_disability)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct population with disability",
            pct_disability=sum(pct_disability*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_disability)) %>%
  filter(!is.na(pct_disability)) %>% 
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_disability=sum(pct_disability*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_disability=bind_rows(summary,rest_of_state,top_20_disability)
filename = paste0("../output/top_20_disability_",today,".csv")
write_csv(top_20_disability,filename)

# muni_level2 -  grouping across EDCS, with suppliers ----

zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("Basic_Rates_TH.xlsx")

muni_basic_neighborhoods = lapply(zipcode_basic_files,function(x){
  read_excel(paste0(x)) %>%  
    #select(-date) %>%
    clean_muni_names() %>%
    neighborhood_to_municipality() %>%
    rename(no_basic_accts=no_accts) 
}) %>%
  bind_rows()

muni_basic = muni_basic_neighborhoods %>%
  group_by(municipality,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T)) %>%
  filter(!municipality %in% light_plant_municipalities) %>%
  ungroup()

muni_basic_neighborhoods = muni_basic_neighborhoods %>%
  group_by(neighborhood,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))

muni_basic_zips = lapply(zipcode_basic_files,function(x){
  read_excel(paste0(x)) %>%  
    #select(-date) %>%
    clean_muni_names() %>%
    neighborhood_to_municipality() %>%
    rename(no_basic_accts=no_accts) 
}) %>%
  bind_rows()
  group_by(zip,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))
muni_basic_zips


muni_agg_neighborhoods = lapply(zipcode_agg_files,function(x){
  read_excel(paste0(x)) %>% 
    #select(-date) %>% 
    clean_muni_names() %>%
    neighborhood_to_municipality()
}) %>%
  bind_rows()

muni_agg = muni_agg_neighborhoods %>%
  group_by(municipality,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) %>%
  filter(!municipality %in% light_plant_municipalities) %>%
  ungroup()

muni_agg_neighborhoods = muni_agg_neighborhoods %>%
  group_by(neighborhood,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) 


muni_cs_neighborhoods = lapply(zipcode_cs_files,function(x){
  read_excel(paste0(x)) %>% 
    #select(-date) %>%
    clean_muni_names() %>%
    neighborhood_to_municipality()
}) %>%
  bind_rows() 

september_rates <- basic_rates %>% filter(date == as.Date("2020-09-01 UTC"))
september_rates

muni_cs = muni_cs_neighborhoods %>%  
  left_join(september_rates, by = "region") %>% 
  #select(-date) %>% 
  clean_supplier_names() %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
  filter(!municipality %in% light_plant_municipalities) 


#  fill in gaps in kwh from calculations above
muni_cs[which(is.na(muni_cs$kwh) == T &
                muni_cs$income == "low"),]$kwh <-
  muni_cs[which(is.na(muni_cs$kwh) == T &
                  muni_cs$income == "low"),]$no_accts*FIT_low_avg_kwh_Sept20

muni_cs[which(is.na(muni_cs$kwh) == T &
                muni_cs$income == "all"),]$kwh <-
  muni_cs[which(is.na(muni_cs$kwh) == T &
                  muni_cs$income == "all"),]$no_accts*FIT_all_avg_kwh_Sept20

muni_cs_2=muni_cs %>% 
  mutate(amt_billed=kwh*rate) %>% 
  group_by(municipality,income) %>%
  summarize(weighted_basic_rate=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            multiple_EDCs=ifelse(n_distinct(region)>1,1,0),
            no_cs_accts=sum(no_accts,na.rm=T),
            new_cs_accts=sum(new_accts,na.rm=T),
            cs_kwh=sum(kwh,na.rm=T),
            cs_amt_billed=sum(amt_billed,na.rm=T),
            suppliers=paste0(sort(unique(supplier_id)),collapse=", ")) %>% 
  mutate(avg_cs_rate=ifelse(cs_kwh!=0,cs_amt_billed/cs_kwh,NA))



muni=full_join(muni_basic,muni_cs_2) %>%
  full_join(muni_agg) 

muni[is.na(muni$no_basic_accts),"no_basic_accts"]=0
muni[is.na(muni$no_cs_accts),"no_cs_accts"]=0
muni[is.na(muni$new_cs_accts),"new_cs_accts"]=0
muni[is.na(muni$no_agg_accts),"no_agg_accts"]=0

suppliers_present=muni %>%
  filter(income=="all") %>% 
  select(municipality,suppliers,multiple_EDCs) %>% 
  rename(all_suppliers=suppliers)

muni=muni %>%
  ungroup() %>% 
  mutate(rate_dif=avg_cs_rate-weighted_basic_rate,
         tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) %>% 
  select(-suppliers,-multiple_EDCs)

muni=recast(muni,municipality~income+variable) 

muni[is.na(muni$low_tot_accts),"low_tot_accts"]=0
muni[is.na(muni$low_no_basic_accts),"low_no_basic_accts"]=0
muni[is.na(muni$low_no_cs_accts),"low_no_cs_accts"]=0
muni[is.na(muni$low_new_cs_accts),"low_new_cs_accts"]=0
muni[is.na(muni$low_no_agg_accts),"low_no_agg_accts"]=0

muni=muni %>%
  ungroup() %>% 
  mutate(all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         agg_present=ifelse(all_no_agg_accts>0,1,0),
         pct_low_accts=low_tot_accts/all_tot_accts) %>% 
  left_join(suppliers_present)

# added 1/7 to get non-low-income components
muni = muni %>%
  mutate(nonlow_tot_accts = all_tot_accts -low_tot_accts,
        nonlow_no_basic_accts = all_no_basic_accts - low_no_basic_accts,
        nonlow_no_cs_accts = all_no_cs_accts - low_no_cs_accts,
        nonlow_no_agg_accts = all_no_agg_accts - low_no_agg_accts,
        nonlow_cs_kwh = all_cs_kwh - low_cs_kwh,
        nonlow_cs_amt_billed = all_cs_amt_billed - low_cs_amt_billed,
        )


muni = muni %>%
  mutate(nonlow_pct_cs = nonlow_no_cs_accts / nonlow_tot_accts,
         nonlow_avg_cs_rate = nonlow_cs_amt_billed / nonlow_cs_kwh)

muni_small=muni %>% 
  mutate(all_cs_loss_over_basic=all_cs_amt_billed-all_weighted_basic_rate*all_cs_kwh,
         all_loss_per_cs_hh=all_cs_loss_over_basic/all_no_cs_accts,
         low_cs_loss_over_basic=low_cs_amt_billed-low_weighted_basic_rate*low_cs_kwh,
         low_loss_per_cs_hh=low_cs_loss_over_basic/low_no_cs_accts,
         nonlow_cs_loss_over_basic = nonlow_cs_amt_billed - all_weighted_basic_rate*nonlow_cs_kwh,
         nonlow_loss_per_cs_hh = nonlow_cs_loss_over_basic / nonlow_no_cs_accts,
         nonlow_rate_diff = nonlow_avg_cs_rate - all_weighted_basic_rate
         )%>% 
  select(municipality,
         all_rate_dif,all_cs_loss_over_basic,
         all_loss_per_cs_hh,all_no_cs_accts,
         all_tot_accts,all_pct_cs,
         low_rate_dif,low_cs_loss_over_basic,
         low_loss_per_cs_hh,low_no_cs_accts,
         low_tot_accts,low_pct_cs,
         nonlow_rate_diff, nonlow_cs_loss_over_basic,
         nonlow_loss_per_cs_hh, nonlow_no_cs_accts,
         nonlow_tot_accts, nonlow_pct_cs,
         pct_low_accts,agg_present,
         multiple_EDCs,
         all_suppliers) 
filename = paste0("../output/muni_level2_",today,".csv")
write_csv(muni_small,filename)


# correlations ----
# used this 3/4/2021
filename = paste0("../output/cor1_",today,".txt")
sink(filename)
cor1 <- cor.test(filter(zips,low_tot_accts>9)$all_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)
print("correlation between percent competitive supply and percent low income accounts by ZIP code")
print(cor1)
sink()

filename = paste0("../output/cor2_",today,".txt")
sink(filename)
cor2 <- cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,filter(zips,low_tot_accts>9)$pct_low_accts)
print("correlation between rate differential and percent low income accounts by ZIP code")
print(cor2)
sink()



const <- monthly_cs %>% filter(supplier %like% "CONST")
const_low <- const %>% filter(income == "low") %>% group_by(date) %>% summarise(tot_accts = sum(no_accts))
filename = paste0("../output/const_low_",today,".txt")
write.csv(const_low, filename, row.names = FALSE)

# check for aggregator in competitive supplier lists
wma_q4_agg <- read_excel("Eversource2021_WMA_q4_agg.xlsx")
wma_q4_aggregators <- unique(wma_q4_agg$aggregator)

wma_q5_agg <- read_excel("Eversource2021_WMA_q5_agg.xlsx")
wma_q5_aggregators <- unique(wma_q5_agg$aggregator)

# wma_q4_cs <- read_excel("Eversource2021_WMA_q4_cs.xlsx")
# wma_q4_cs_check_supplier <- wma_q4_cs %>% filter(supplier %in% wma_q4_aggregators)
# wma_q4_cs_check_supplier_unique <- unique(wma_q4_cs_check_supplier[,c("municipality","supplier")])
# write.csv(wma_q4_cs_check_supplier_unique,"../output/wma_q4_cs_check_supplier.csv", 
#                           row.names = FALSE, quote = FALSE)
# 
# wma_q5_cs <- read_excel("Eversource2021_WMA_q5_cs.xlsx")
# wma_q5_cs_check_supplier <- wma_q5_cs %>% filter(supplier %in% wma_q5_aggregators)
# wma_q5_cs_check_supplier_unique <- unique(wma_q5_cs_check_supplier[,c("municipality","supplier")])
# write.csv(wma_q5_cs_check_supplier_unique,"../output/wma_q5_cs_check_supplier.csv", 
#                           row.names = FALSE, quote = FALSE)


# top 10 cities by overall consumer loss, all incomes ----
max_total_loss_all_inc <- muni_small %>% slice_max(all_cs_loss_over_basic, n=10) %>% 
  select(municipality,all_cs_loss_over_basic)
filename = paste0("../output/max_total_loss_all_inc",today,".csv")
write_csv(max_total_loss_all_inc,filename)


# top 10 cities by average consumer loss per hh, all incomes ----
max_loss_per_hh_all_inc <- muni_small %>% slice_max(all_loss_per_cs_hh, n=10) %>% 
  select(municipality,all_loss_per_cs_hh)
filename = paste0("../output/max_loss_per_hh_all_inc",today,".csv")
write_csv(max_loss_per_hh_all_inc,filename)


#### top 10 cities by overall consumer loss, low incomes ----
max_total_loss_low_inc <- muni_small %>% slice_max(low_cs_loss_over_basic, n=10) %>% 
  select(municipality,low_cs_loss_over_basic)
filename = paste0("../output/max_total_loss_low_inc",today,".csv")
write_csv(max_total_loss_low_inc,filename)


# top 10 cities by average consumer loss per hh, low incomes ----
max_loss_per_hh_low_inc <- muni_small %>% slice_max(low_loss_per_cs_hh, n=10) %>% 
  select(municipality,all_loss_per_cs_hh)
filename = paste0("../output/max_loss_per_hh_low_inc",today,".csv")
write_csv(max_loss_per_hh_low_inc,filename)

# which competitive suppliers save consumers the most money?
# in monthly_cs_welfare, bill_difference = difference betw what was paid and what would have been
# paid under basic (non-competitive) billing.
# savings_by_supplier <- monthly_cs_welfare %>% 
#   filter(income == "all") %>%
#   group_by(supplier, date) %>% 
#   summarize(aggregate_savings = -sum(bill_difference),
#             savings_per_hh = -sum(bill_difference_pp))
# filename = paste0("../output/savings_by_supplier_by_month_",today,".csv")
# write_csv(savings_by_supplier, filename )
# 
# savings_over_year <- savings_by_supplier %>% 
#   summarize(aggr_savings_over_year = -sum(aggregate_savings))
# filename = paste0("../output/savings_by_supplier_whole_year_",today,".csv")
# write_csv(savings_over_year, filename )

## do aggregate savings / premiums make sense, ocrrespond to other accountings?
## are all aggregators out of cs?


# difference in premium, participation between town with and without aggregators ----
part_rate_agg_no_agg_towns <- muni_small %>%
  group_by(agg_present) %>%
  summarize(total_accounts_all = sum(all_tot_accts),
            cs_accounts_all = sum(all_no_cs_accts),
            total_accounts_low = sum(low_tot_accts),
            cs_accounts_low = sum(low_no_cs_accts),
            pct_cs_all = cs_accounts_all / total_accounts_all,
            pct_cs_low = cs_accounts_low / total_accounts_low) %>%
  filter(is.na(agg_present) == FALSE) %>%
  mutate(aggregator_present = ifelse(agg_present==0, "Towns with no aggregator", "Towns with aggregator")) %>%
  mutate(total_accounts_all = scales::number(total_accounts_all, big.mark = ","),
         cs_accounts_all = scales::number(cs_accounts_all, big.mark = ","),
         total_accounts_low = scales::number(total_accounts_low, big.mark = ","),
         cs_accounts_low = scales::number(cs_accounts_low, big.mark = ","),
         pct_cs_all = scales::percent(pct_cs_all),
         pct_cs_low = scales::percent(pct_cs_low)) %>%
  select(aggregator_present, total_accounts_all, cs_accounts_all, pct_cs_all,
         total_accounts_low, cs_accounts_low, pct_cs_low) 
part_rate_agg_no_agg_towns
filename = paste0("../output/participation_rate_agg_no_agg_towns_",today,".csv")
write_csv(part_rate_agg_no_agg_towns, filename )

prem_diff_agg_no_agg_towns <- muni_small %>%
  group_by(agg_present) %>%
  filter(all_no_cs_accts>0) %>%
  filter(low_no_cs_accts>0) %>%
  summarize(ave_premium_all = sum(all_rate_dif*all_no_cs_accts) / sum(all_no_cs_accts),
            ave_premium_low = sum(low_rate_dif*low_no_cs_accts) / sum(low_no_cs_accts)) %>%
  mutate(ave_premium_all = scales::number(ave_premium_all, accuracy = 0.0001, prefix = "$", suffix = "/kwh"),
         ave_premium_low = scales::number(ave_premium_low, accuracy = 0.0001, prefix = "$", suffix = "/kwh")) %>%
  mutate(aggregator_present = ifelse(agg_present==0, "Towns with no aggregator", "Towns with aggregator")) %>%
  select(aggregator_present, ave_premium_all, ave_premium_low)
prem_diff_agg_no_agg_towns
filename = paste0("../output/premium_difference_agg_no_agg_towns_",today,".csv")
write_csv(prem_diff_agg_no_agg_towns, filename )

# net loss / gain, number of customers, ave gain / loss ----
gain_loss <- monthly_cs_welfare %>%
  filter(income == "all") %>%
  mutate(net = ifelse(bill_difference>0,"loss","gain")) %>%
  group_by(net) %>%
  summarize(total_cs_accounts = sum(no_accts),
            total_bill_difference = sum(bill_difference),
            total_kwh = sum(kwh),
            average_gain_loss_per_year = 12*total_bill_difference/total_cs_accounts,
            average_gain_loss_per_kwh = total_bill_difference / total_kwh,
            wtd_avg_rate = sum(rate*kwh)/sum(kwh)) %>%
  select(net, total_cs_accounts, total_bill_difference, average_gain_loss_per_year, average_gain_loss_per_kwh, wtd_avg_rate)
gain_loss
filename = paste0("../output/gain_loss_by_consumers",today,".csv")
write_csv(gain_loss, filename )
  
# appendix 2b ----
#head(muni_small)
appendix_2b <-muni_small %>%
  select(municipality, all_loss_per_cs_hh, all_cs_loss_over_basic, all_rate_dif, all_pct_cs, all_no_cs_accts) %>%
  mutate("Municipality" = municipality,
         "Total Consumer Loss in Month" = scales::number(all_cs_loss_over_basic, prefix = "$", big.mark = ",", accuracy = 1),
         "Average Per Household Loss in Month" = scales::number(all_loss_per_cs_hh, prefix = "$", big.mark = ",", accuracy = 0.01),
         "Premium (per kWh)" = scales::number(all_rate_dif,accuracy = 0.0001,prefix = "$"),
         "% Households Participating in Competitive Supply Market" = scales::percent(all_pct_cs, accuracy = 1),
         "# Competitive Supply Accounts" = scales::number(all_no_cs_accts, big.mark = ",", accuracy = 1)) %>%
  select("Municipality", 
         "Total Consumer Loss in Month",
         "Average Per Household Loss in Month",
         "Premium (per kWh)",
         "% Households Participating in Competitive Supply Market",
         "# Competitive Supply Accounts")
head(appendix_2b)
filename = paste0("../output/appendix_2b_",today,".csv")
write_csv(appendix_2b, filename )

# appendix 2c ----
appendix_2c <-muni_small %>%
  select(municipality, low_loss_per_cs_hh, low_cs_loss_over_basic, low_rate_dif, low_pct_cs, low_no_cs_accts) %>%
  mutate("Municipality" = municipality,
         "Total Consumer Loss in Month" = scales::number(low_cs_loss_over_basic, prefix = "$", big.mark = ",", accuracy = 1),
         "Average Per Household Loss in Month" = scales::number(low_loss_per_cs_hh, prefix = "$", big.mark = ",", accuracy = 0.01),
         "Premium (per kWh)" = scales::number(low_rate_dif,accuracy = 0.0001,prefix = "$"),
         "% Households Participating in Competitive Supply Market" = scales::percent(low_pct_cs, accuracy = 1),
         "# Competitive Supply Accounts" = scales::number(low_no_cs_accts, big.mark = ",", accuracy = 1)) %>%
  select("Municipality", 
         "Total Consumer Loss in Month",
         "Average Per Household Loss in Month",
         "Premium (per kWh)",
         "% Households Participating in Competitive Supply Market",
         "# Competitive Supply Accounts")
head(appendix_2c)
filename = paste0("../output/appendix_2c_",today,".csv")
write_csv(appendix_2c, filename )

# boston neighborhood analysis ----
boston_basic_all <- muni_basic_neighborhoods %>% 
  filter(income == "all")
boston_basic_all

boston_basic_low <- muni_basic_neighborhoods %>% 
  filter(income == "low")
boston_basic_low

boston_neighborhoods_all <- full_join(muni_cs_neighborhoods, muni_basic_neighborhoods) %>%
  #full_join(muni_agg_neighborhoods) %>%
  filter(municipality == "Boston") %>%
  filter(income == "all") %>%
  #filter(is.na(no_accts) == FALSE) %>%
  left_join(basic_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(neighborhood) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(neighborhood, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(boston_basic_all, by.x = "neighborhood", by.y = "neighborhood") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts)) %>%
  select(-no_basic_accts, -income)
boston_neighborhoods_all
filename = paste0("../output/boston_neighborhoods_all_",today,".csv")
write_csv(boston_neighborhoods_all, filename )


# boston neighborhoods all income - by ZIP
boston_neighborhoods_zip_all <- full_join(muni_cs_neighborhoods, muni_basic_neighborhoods) %>%
  #full_join(muni_agg_neighborhoods) %>%
  filter(municipality == "Boston") %>%
  filter(income == "all") %>%
  #filter(is.na(no_accts) == FALSE) %>%
  left_join(basic_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(zip) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(zip, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(muni_basic_zips, by.x = "zip", by.y = "zip") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts)) %>%
  select(-no_basic_accts, -date, -region, -neighborhood) %>%
  filter(income == "all") %>%
  select(-income, -municipality)
boston_neighborhoods_zip_all
filename = paste0("../output/boston_neighborhoods_zip_all_",today,".csv")
write_csv(boston_neighborhoods_zip_all, filename )

# boston neighborhoods - low income
boston_neighborhoods_low <- full_join(muni_cs_neighborhoods, muni_basic_neighborhoods) %>%
  #full_join(muni_agg_neighborhoods) %>%
  filter(municipality == "Boston") %>%
  filter(income == "low") %>%
  #filter(is.na(no_accts) == FALSE) %>%
  left_join(basic_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(neighborhood) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(neighborhood, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(boston_basic_low, by.x = "neighborhood", by.y = "neighborhood") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts)) %>%
  select(-no_basic_accts, -income)
boston_neighborhoods_low
filename = paste0("../output/boston_neighborhoods_low_",today,".csv")
write_csv(boston_neighborhoods_low, filename )


# boston neighborhoods low income - by ZIP
boston_neighborhoods_zip_low <- full_join(muni_cs_neighborhoods, muni_basic_neighborhoods) %>%
  #full_join(muni_agg_neighborhoods) %>%
  filter(municipality == "Boston") %>%
  filter(income == "low") %>%
  #filter(is.na(no_accts) == FALSE) %>%
  left_join(basic_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(zip) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(zip, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(muni_basic_zips, by.x = "zip", by.y = "zip") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts)) %>%
  select(-no_basic_accts, -date, -region, -neighborhood) %>%
  filter(income == "low") %>%
  select(-income, -municipality)
boston_neighborhoods_zip_low
filename = paste0("../output/boston_neighborhoods_zip_low_",today,".csv")
write_csv(boston_neighborhoods_zip_low, filename )
  

# Resident ----
zipcode_res=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

# fill in gaps for FIT region
zipcode_res[which(zipcode_res$region == "FIT" & zipcode_res$income == "low"),]$kwh <- zipcode_res[which(zipcode_res$region == "FIT" & zipcode_res$income == "low"),]$no_accts*FIT_low_avg_kwh_Sept20
zipcode_res[which(zipcode_res$region == "FIT" & zipcode_res$income == "all"),]$kwh <- zipcode_res[which(zipcode_res$region == "FIT" & zipcode_res$income == "all"),]$no_accts*FIT_all_avg_kwh_Sept20

zipcode_res = zipcode_res %>%  
  filter(kwh>0) %>%
  filter(no_accts>0) %>%
  filter(supplier %like% "RESID") %>%
  filter(income == "low") %>%
  left_join(basic_rates) %>%
  group_by(municipality, zip, region, basic_rate) %>%
  summarize(no_cs_accts=sum(no_accts),
            avg_rate_charged = sum(rate*kwh)/sum(kwh),
            avg_premium = sum((rate-basic_rate)*kwh)/sum(kwh)) %>%
  select(municipality, zip, no_cs_accts, avg_rate_charged, basic_rate, avg_premium, region)
summary(zipcode_res)
filename = paste0("../output/RESIDENT_ENERGY_LowIncome_Sept2020_",today,".csv")
write_csv(zipcode_res, filename )

# highest rate charged ----
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_cs_high_rate=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

zipcode_cs_high_rate = zipcode_cs_high_rate %>%  
  filter(kwh>0) %>%
  filter(no_accts>0) %>%
  filter(rate >.25) %>%
  arrange(desc(rate))
filename = paste0("../output/highest_rates_",today,".csv")
write_csv(zipcode_cs_high_rate, filename )


summary(zipcode_cs_high_rate)


print("Finished.")