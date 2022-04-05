# Sept 2021 data update

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


# custom functions ----
# clean_muni_names ----
# this function substitutes standard names for abbreviated and nonstandard town names.
clean_muni_names=function(df){
  df %>% 
    mutate(municipality=str_to_title(municipality)) %>% 
    left_join(fread("../cleaned_responses/clean_muni_names.csv")) %>% 
    mutate(municipality=ifelse(!is.na(clean_muni),clean_muni,municipality)) %>% 
    select(-clean_muni)
}

# clean_supplier_names ----
clean_supplier_names=function(df){
  df %>% 
    left_join(fread("../cleaned_responses/clean_supplier_names.csv")) %>% 
    mutate(supplier=ifelse(!is.na(corrected_supplier),corrected_supplier,supplier)) %>% 
    select(-corrected_supplier)
}


# # change neighborhoods to municipalities ----
# this function creates separate fields for neigborhood and municipality based on a lookup.
neighborhood_to_municipality=function(df){
  df %>%
    left_join(fread("../cleaned_responses/neighborhood_to_municipality.csv")) %>%
    mutate(neighborhood = municipality,
           municipality=ifelse(!is.na(corrected_municipality),corrected_municipality,municipality)) %>%
    select(-corrected_municipality)
}

today = today()

# muni_level2  ----
# loading and cleaning data ----
# Eversource East ALL income ----
EMA_all_cs <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(a) CONFIDENTIAL.xlsx",
                             sheet = "Q1 Alternate Supply",
                             skip = 2,
                             col_names = TRUE)
# dim(EMA_all_cs)
# sum(EMA_all_cs$Billed)
str(EMA_all_cs)
EMA_all_cs <- EMA_all_cs %>% rename(zip = Zip, 
                                    municipality = Town,
                                    supplier = Supplier,
                                    rate = Price,
                                    kwh = Use,
                                    no_accts = `Num Customers`,
                                    new_accts = `New Customers`) %>%
  select(-Date, -`Supplier Rate`, -Billed) %>%
  mutate(region = "EMA",
         income = "all")
str(EMA_all_cs)


EMA_all_muni <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(a) CONFIDENTIAL.xlsx",
                        sheet = "Q1 Aggregation",
                        skip = 1,
                        col_names = TRUE)
# dim(EMA_all_muni)
# sum(EMA_all_muni$`Num Customers`)
str(EMA_all_muni)
EMA_all_muni <- EMA_all_muni %>% rename(zip = Zip,
                                        municipality = Town,
                                        no_accts = `Num Customers`) %>%
  select(-Date, -Aggregation) %>%
  mutate(region = "EMA",
         income = "all")
str(EMA_all_muni)


EMA_all_basic <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(a) CONFIDENTIAL.xlsx",
                          sheet = "Q1 Basic Service",
                          skip = 0,
                          col_names = TRUE)
# dim(EMA_all_basic)
# sum(EMA_all_basic$`Num of Customers`)
str(EMA_all_basic)
EMA_all_basic <- EMA_all_basic %>% rename(zip = Zip,
                                          municipality = Town,
                                          no_accts = `Num of Customers`) %>%
  select(-Date) %>%
  mutate(region = "EMA",
         income = "all")
str(EMA_all_basic)


# Eversource East LOW income ----
EMA_low_cs <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(a) CONFIDENTIAL.xlsx",
                        sheet = "Q2 Low Inc Alternate Supply",
                        skip = 0,
                        col_names = TRUE)
# dim(EMA_low_cs)
# sum(EMA_low_cs$Billed)
str(EMA_low_cs)
EMA_low_cs <- EMA_low_cs %>% rename(zip = Zip, 
                                    municipality = Town,
                                    supplier = Supplier,
                                    rate = Price,
                                    kwh = Use,
                                    no_accts = `Num Customers`,
                                    new_accts = `Num New Customers`) %>%
  select(-Date, -`Supplier Rate`, -Billed) %>%
  mutate(region = "EMA",
         income = "low")
str(EMA_low_cs)


EMA_low_muni <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(a) CONFIDENTIAL.xlsx",
                          sheet = "Q2 Low Inc Aggregation",
                          skip = 0,
                          col_names = TRUE)
# dim(EMA_low_muni)
# sum(EMA_low_muni$`Num of Customers`)
str(EMA_low_muni)
EMA_low_muni <- EMA_low_muni %>% rename(zip = Zip,
                                        municipality = Town,
                                        no_accts = `Num of Customers`) %>%
  select(-Date, -Agg, -`Low Income`) %>%
  mutate(region = "EMA",
         income = "low")
str(EMA_low_muni)


EMA_low_basic <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(a) CONFIDENTIAL.xlsx",
                           sheet = "Q2 Low Income Basic Service",
                           skip = 0,
                           col_names = TRUE)
# dim(EMA_low_basic)
# sum(EMA_low_basic$`Num of Customers`)
str(EMA_low_basic)
EMA_low_basic <- EMA_low_basic %>% rename(zip = Zip,
                                          municipality = Town,
                                          no_accts = `Num of Customers`) %>%
  select(-Date, -`Low Income`) %>%
  mutate(region = "EMA",
         income = "low")
str(EMA_low_basic)



# Eversource western ALL income ----
WMA_all_cs <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(b) CONFIDENTIAL.xlsx",
                        sheet = "1A",
                        skip = 0,
                        col_names = TRUE)
dim(WMA_all_cs)
sum(WMA_all_cs$PRICE)
WMA_all_cs <- WMA_all_cs %>%rename(zip = ZIP,
                                   municipality = TOWN,
                                   supplier = `SUPPLIER NAME`,
                                   rate = PRICE,
                                   kwh = `TOTAL KWH`,
                                   no_accts = `TOTAL COUNT`,
                                   new_accts = `NEW ACCTS`) %>%
  mutate(region = "WMA",
         income = "all")
str(WMA_all_cs)

WMA_all_muni <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(b) CONFIDENTIAL.xlsx",
                          sheet = "1B",
                          skip = 0,
                          col_names = TRUE)
# dim(WMA_all_muni)
# sum(WMA_all_muni$`TOTAL COUNT`)
str(WMA_all_muni)
WMA_all_muni <- WMA_all_muni %>% rename(zip = ZIP,
                                    municipality = TOWN,
                                    supplier = `SUPPLIER NAME`,
                                    no_accts = `TOTAL COUNT`) %>%
  mutate(region = "WMA",
         income = "all")
str(WMA_all_muni)

WMA_all_basic <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 1(b) CONFIDENTIAL.xlsx",
                           sheet = "1C",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(WMA_all_basic)
# sum(WMA_all_basic$NUM_ACCTS_BILLED)
str(WMA_all_basic)
WMA_all_basic <- WMA_all_basic %>% rename(zip = ZIP,
                                      municipality = TOWN,
                                      no_accts = `NUM_ACCTS_BILLED`) %>%
  select(-DATE) %>%
  mutate(region = "WMA",
         income = "all")
str(WMA_all_basic)


# Eversource western LOW income ----
WMA_low_cs <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(b) CONFIDENTIAL.xlsx",
                        sheet = "2A",
                        skip = 0,
                        col_names = TRUE)
# dim(WMA_low_cs)
# sum(WMA_low_cs$PRICE)
str(WMA_low_cs)
WMA_low_cs <- WMA_low_cs %>%rename(zip = ZIP,
                                   municipality = TOWN,
                                   supplier = `SUPPLIER NAME`,
                                   rate = PRICE,
                                   kwh = `TOTAL KWH`,
                                   no_accts = `TOTAL COUNT`,
                                   new_accts = `NEW ACCTS`) %>%
  mutate(region = "WMA",
         income = "low")
str(WMA_low_cs)

WMA_low_muni <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(b) CONFIDENTIAL.xlsx",
                          sheet = "2B",
                          skip = 0,
                          col_names = TRUE)
# dim(WMA_low_muni)
# sum(WMA_low_muni$`TOTAL COUNT`)
str(WMA_low_muni)
WMA_low_muni <- WMA_low_muni %>% rename(zip = ZIP,
                                        municipality = TOWN,
                                        supplier = `SUPPLIER NAME`,
                                        no_accts = `TOTAL COUNT`)%>%
  mutate(region = "WMA",
         income = "low")
str(WMA_low_muni)



WMA_low_basic <-read_excel("../sept_2021_update/Eversource_raw/Attachment AG Oversight 2(b) CONFIDENTIAL.xlsx",
                           sheet = "2C",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(WMA_low_basic)
# sum(WMA_low_basic$NUM_ACCTS_BILLED)
str(WMA_low_basic)
WMA_low_basic <- WMA_low_basic %>% rename(zip = ZIP,
                                          municipality = TOWN,
                                          no_accts = `NUM_ACCTS_BILLED`) %>%
  select(-DATE) %>%
  mutate(region = "WMA",
          income = "low")
str(WMA_low_basic)




# MECO ALL income data ----
MECO_all_cs <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Rsponse_MECOQ1 - CONFIDENTIAL.xlsx",
                        sheet = "Q1 (MECO) comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text","numeric",
                                      "numeric","numeric","numeric","numeric"))
# dim(MECO_all_cs)
# sum(MECO_all_cs$`Number of residential accounts billed`)
str(MECO_all_cs)
MECO_all_cs <- MECO_all_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date, -`Customer Charge`) %>%
  mutate(region = "MECO",
         income = "all")
str(MECO_all_cs)


MECO_all_muni <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Rsponse_MECOQ1 - CONFIDENTIAL.xlsx",
                          sheet = "Q1 (MECO) muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(MECO_all_muni)
# sum(MECO_all_muni$`Number of residential accounts billed`)
str(MECO_all_muni)
MECO_all_muni <- MECO_all_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`)%>%
  mutate(region = "MECO",
         income = "all")
str(MECO_all_muni)


MECO_all_basic <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Rsponse_MECOQ1 - CONFIDENTIAL.xlsx",
                           sheet = "Q1 (MECO) basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(MECO_all_basic)
# sum(MECO_all_basic$`Number of residential accounts billed`)
str(MECO_all_basic)
MECO_all_basic <- MECO_all_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date)%>%
  mutate(region = "MECO",
         income = "all")
str(MECO_all_basic)


# MECO LOW income ----
MECO_low_cs <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_MECO_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                        sheet = "Q2 (MECO-LI) comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text","numeric",
                                      "numeric","numeric","numeric","numeric"))
# dim(MECO_low_cs)
# sum(MECO_low_cs$`Number of NEW residential accounts billed`)
str(MECO_low_cs)
MECO_low_cs <- MECO_low_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date, -`Customer Charge`) %>%
  mutate(region = "MECO",
         income = "low") %>%
  filter(kwh > 0)
str(MECO_low_cs)


MECO_low_muni <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_MECO_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                          sheet = "Q2 (MECO-LI) muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(MECO_low_muni)
# sum(MECO_low_muni$`Number of residential accounts billed`)
str(MECO_low_muni)
MECO_low_muni <- MECO_low_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`) %>%
  mutate(region = "MECO",
         income = "low")
str(MECO_low_muni)


MECO_low_basic <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_MECO_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                           sheet = "Q2 MECO-LI) basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(MECO_low_basic)
# sum(MECO_low_basic$`Number of residential accounts billed`)
str(MECO_low_basic)
MECO_low_basic <- MECO_low_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "MECO",
         income = "low")
str(MECO_low_basic)




# Nantucket ALL income data ----
NAN_all_cs <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_NANTQ1 - CONFIDENTIAL.xlsx",
                        sheet = "Q1 (NANT) comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text","numeric",
                                      "numeric","numeric","numeric","numeric"))
# dim(NAN_all_cs)
# sum(NAN_all_cs$`Number of residential accounts billed`)
str(NAN_all_cs)
NAN_all_cs <- NAN_all_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date, -`Customer Charge`) %>%
  mutate(region = "NAN",
         income = "all")
str(NAN_all_cs)


NAN_all_muni <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_NANTQ1 - CONFIDENTIAL.xlsx",
                          sheet = "Q1 (NANT) muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(NAN_all_muni)
# sum(NAN_all_muni$`Number of residential accounts billed`)
str(NAN_all_muni)
NAN_all_muni <- NAN_all_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`) %>%
  mutate(region = "NAN",
         income = "all")
str(NAN_all_muni)


NAN_all_basic <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG response_NANTQ1 - CONFIDENTIAL.xlsx",
                           sheet = "Q1 (NANT) basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(NAN_all_basic)
# sum(NAN_all_basic$`Number of residential accounts billed`)
str(NAN_all_basic)
NAN_all_basic <- NAN_all_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "NAN",
         income = "all")
str(NAN_all_basic)


# Nantucket LOW income data ----
NAN_low_cs <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Response_NANT_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                        sheet = "Q2 (NANT) comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text","numeric",
                                      "numeric","numeric","numeric","numeric"))
# dim(NAN_low_cs)
# sum(NAN_low_cs$`Total kWh Billed to Residential Accounts `)
str(NAN_low_cs)
NAN_low_cs <- NAN_low_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date, -`Customer Charge`) %>%
  mutate(region = "NAN",
         income = "low")
str(NAN_low_cs)


NAN_low_muni <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Response_NANT_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                          sheet = "Q2 (NANT) muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(NAN_low_muni)
# sum(NAN_low_muni$`Number of residential accounts billed`)
str(NAN_low_muni)
NAN_low_muni <- NAN_low_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`) %>%
  mutate(region = "NAN",
         income = "low")
str(NAN_low_muni)


NAN_low_basic <-read_excel("../sept_2021_update/NationalGrid_raw/MA AG Response_NANT_LowIncomeQ2 - CONFIDENTIAL.xlsx",
                           sheet = "Q2 (NANT) basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(NAN_low_basic)
# sum(NAN_low_basic$`Number of residential accounts billed`)
str(NAN_low_basic)
NAN_low_basic <- NAN_low_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "NAN",
         income = "low")
str(NAN_low_basic)


# Unitil ALL income data ----
FIT_all_cs <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                        sheet = "Q1 comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text",
                                      "numeric","numeric","numeric","numeric"))
# dim(FIT_all_cs)
# sum(FIT_all_cs$`Total kWh Billed to Residential Accounts`)
str(FIT_all_cs)
FIT_all_cs <- FIT_all_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "FIT",
         income = "all") %>%
  filter(!supplier %in% c("ASHBYAGG-FIRSTPO","CONSTELLATION LU"))
str(FIT_all_cs)


FIT_all_muni <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                          sheet = "Q1 muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(FIT_all_muni)
# sum(FIT_all_muni$`Number of residential accounts billed`)
str(FIT_all_muni)
FIT_all_muni <- FIT_all_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`) %>%
  mutate(region = "FIT",
         income = "all")
str(FIT_all_muni)


FIT_all_basic <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                           sheet = "Q1 basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(FIT_all_basic)
# sum(FIT_all_basic$`Number of residential accounts billed`)
str(FIT_all_basic)
FIT_all_basic <- FIT_all_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "FIT",
         income = "all")
str(FIT_all_basic)


# Unitil LOW income ----
FIT_low_cs <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                        sheet = "Q2 comp_suppliers",
                        skip = 0,
                        col_names = TRUE,
                        col_types = c("text","numeric","text","text",
                                      "numeric","numeric","numeric","numeric"))
# dim(FIT_low_cs)
# sum(FIT_low_cs$`Number of residential accounts billed`)
str(FIT_low_cs)
FIT_low_cs <- FIT_low_cs %>% rename(zip = Zip, 
                                    municipality = Municipality,
                                    supplier = Supplier,
                                    rate = `Rate class ($/kwh)`,
                                    kwh = `Total kWh Billed to Residential Accounts`,
                                    no_accts = `Number of residential accounts billed`,
                                    new_accts = `Number of NEW residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "FIT",
         income = "low") %>%
  filter(!supplier %in% c("ASHBYAGG-FIRSTPO","CONSTELLATION LU"))
str(FIT_low_cs)


FIT_low_muni <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                          sheet = "Q2 muni_agg",
                          skip = 0,
                          col_names = TRUE,
                          col_types = c("text","numeric","text","text","numeric"))
# dim(FIT_low_muni)
# sum(FIT_low_muni$`Number of residential accounts billed`)
str(FIT_low_muni)
FIT_low_muni <- FIT_low_muni %>% rename(zip = Zip,
                                        municipality = Municipality,
                                        no_accts = `Number of residential accounts billed`) %>%
  select(-Date, -`Municipal Aggregator`) %>%
  mutate(region = "FIT",
         income = "low")
str(FIT_low_muni)


FIT_low_basic <-read_excel("../sept_2021_update/Unitil_raw/FGE CONFIDENTIAL MA AG response_format_Sept 2021 Data (1).xlsx",
                           sheet = "Q2 basic_service",
                           skip = 0,
                           col_names = TRUE,
                           col_types = c("text","numeric","text","numeric"))
# dim(FIT_low_basic)
# sum(FIT_low_basic$`Number of residential accounts billed`)
str(FIT_low_basic)
FIT_low_basic <- FIT_low_basic %>% rename(zip = Zip,
                                          municipality = Municipality,
                                          no_accts = `Number of residential accounts billed`) %>%
  select(-Date) %>%
  mutate(region = "FIT",
         income = "low")
str(FIT_low_basic)



basic_rates=read_excel("../cleaned_responses/Basic_Rates_TH.xlsx")
municipal_light_plants <- read.csv("../other_inputs/municipal_light_plants.csv", header = TRUE)
light_plant_municipalities <- unique(municipal_light_plants$Town.Name)

# clean names and add neighborhoods ----
zipcode_basic_files <- bind_rows(EMA_all_basic, EMA_low_basic, 
                         WMA_all_basic, WMA_low_basic,
                         MECO_all_basic, MECO_low_basic,
                         NAN_all_basic, NAN_low_basic,
                         FIT_all_basic, FIT_low_basic)

muni_basic_neighborhoods <- zipcode_basic_files %>%
    clean_muni_names() %>%
    neighborhood_to_municipality() %>%
    rename(no_basic_accts=no_accts) 


muni_basic = muni_basic_neighborhoods %>%
  group_by(municipality,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T)) %>%
  filter(!municipality %in% light_plant_municipalities) %>%
  ungroup()

muni_basic_neighborhoods = muni_basic_neighborhoods %>%
  group_by(neighborhood,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))

muni_basic_zips = zipcode_basic_files %>%  
  clean_muni_names() %>%
  neighborhood_to_municipality() %>%
  rename(no_basic_accts=no_accts) %>%
  group_by(zip,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))
muni_basic_zips


zipcode_agg_files = bind_rows(EMA_all_muni, EMA_low_muni, 
                              WMA_all_muni, WMA_low_muni,
                              MECO_all_muni, MECO_low_muni,
                              NAN_all_muni, NAN_low_muni,
                              FIT_all_muni, FIT_low_muni)


muni_agg_neighborhoods = zipcode_agg_files %>%
    clean_muni_names() %>%
    neighborhood_to_municipality()


muni_agg = muni_agg_neighborhoods %>%
  group_by(municipality,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) %>%
  filter(!municipality %in% light_plant_municipalities) %>%
  ungroup()

muni_agg_neighborhoods = muni_agg_neighborhoods %>%
  group_by(neighborhood,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) 

muni_agg_zips = zipcode_agg_files %>%  
  clean_muni_names() %>%
  neighborhood_to_municipality() %>%
  rename(no_agg_accts=no_accts) %>%
  group_by(zip,income) %>% 
  summarize(no_agg_accts=sum(no_agg_accts,na.rm=T))
muni_agg_zips


zipcode_cs_files = bind_rows(EMA_all_cs, EMA_low_cs, 
                             WMA_all_cs, WMA_low_cs,
                             MECO_all_cs, MECO_low_cs,
                             NAN_all_cs, NAN_low_cs,
                             FIT_all_cs, FIT_low_cs)

muni_cs_neighborhoods = zipcode_cs_files %>% 
    clean_muni_names() %>%
    neighborhood_to_municipality()

# september 2021 rates ----
september_rates <- basic_rates %>% filter(date == as.Date("2021-09-01 UTC"))
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
filename = paste0("../sept_2021_update/output/muni_level2_",today,".csv")
write_csv(muni_small,filename)


# correlations ----
# used this 3/4/2021
# filename = paste0("../sept_2021_update/output/cor1_",today,".txt")
# sink(filename)
# cor1 <- cor.test(filter(zips,low_tot_accts>9)$all_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)
# print("correlation between percent competitive supply and percent low income accounts by ZIP code")
# print(cor1)
# sink()
# 
# filename = paste0("../sept_2021_update/output/cor2_",today,".txt")
# sink(filename)
# cor2 <- cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,filter(zips,low_tot_accts>9)$pct_low_accts)
# print("correlation between rate differential and percent low income accounts by ZIP code")
# print(cor2)
# sink()




# top 10 cities by overall consumer loss, all incomes ----
max_total_loss_all_inc <- muni_small %>% slice_max(all_cs_loss_over_basic, n=10) %>% 
  select(municipality,all_cs_loss_over_basic)
filename = paste0("../sept_2021_update/output/max_total_loss_all_inc",today,".csv")
write_csv(max_total_loss_all_inc,filename)


# top 10 cities by average consumer loss per hh, all incomes ----
max_loss_per_hh_all_inc <- muni_small %>% slice_max(all_loss_per_cs_hh, n=10) %>% 
  select(municipality,all_loss_per_cs_hh)
filename = paste0("../sept_2021_update/output/max_loss_per_hh_all_inc",today,".csv")
write_csv(max_loss_per_hh_all_inc,filename)


#### top 10 cities by overall consumer loss, low incomes ----
max_total_loss_low_inc <- muni_small %>% slice_max(low_cs_loss_over_basic, n=10) %>% 
  select(municipality,low_cs_loss_over_basic)
filename = paste0("../sept_2021_update/output/max_total_loss_low_inc",today,".csv")
write_csv(max_total_loss_low_inc,filename)


# top 10 cities by average consumer loss per hh, low incomes ----
max_loss_per_hh_low_inc <- muni_small %>% slice_max(low_loss_per_cs_hh, n=10) %>% 
  select(municipality,all_loss_per_cs_hh)
filename = paste0("../sept_2021_update/output/max_loss_per_hh_low_inc",today,".csv")
write_csv(max_loss_per_hh_low_inc,filename)



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
filename = paste0("../sept_2021_update/output/participation_rate_agg_no_agg_towns_",today,".csv")
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
filename = paste0("../sept_2021_update/output/premium_difference_agg_no_agg_towns_",today,".csv")
write_csv(prem_diff_agg_no_agg_towns, filename )



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
filename = paste0("../sept_2021_update/output/appendix_2b_",today,".csv")
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
filename = paste0("../sept_2021_update/output/appendix_2c_",today,".csv")
write_csv(appendix_2c, filename )

# boston neighborhood analysis ----
boston_basic_all <- muni_basic_neighborhoods %>% 
  filter(income == "all") 

boston_basic_low <- muni_basic_neighborhoods %>% 
  filter(income == "low")

# muni_basic_neighborhoods_all <- muni_basic_neighborhoods %>% filter(income == "all")
# muni_basic_neighborhoods_low <- muni_basic_neighborhoods %>% filter(income == "low")

boston_agg_all <- muni_agg_neighborhoods %>%
  filter(income == "all")

boston_agg_low <- muni_agg_neighborhoods %>%
  filter(income == "low")

boston_neighborhoods_all <- muni_cs_neighborhoods %>%
  filter(municipality == "Boston") %>%
  filter(income == "all") %>%
  left_join(september_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(neighborhood) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(neighborhood, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(boston_basic_all, by.x = "neighborhood", by.y = "neighborhood") %>%
  left_join(boston_agg_all, by.x = "neigborhood", by.y = "neighborhood") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts + no_agg_accts)) %>%
  select(-no_basic_accts, -income, -no_agg_accts)
boston_neighborhoods_all
filename = paste0("../sept_2021_update/output/boston_neighborhoods_all_",today,".csv")
write_csv(boston_neighborhoods_all, filename )


# boston neighborhoods all income - by ZIP
boston_neighborhoods_zip_all <- muni_cs_neighborhoods %>%
  filter(municipality == "Boston") %>%
  filter(income == "all") %>%
  left_join(september_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(zip) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(zip, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(muni_basic_zips[which(muni_basic_zips$income == "all"),], by.x = "zip", by.y = "zip") %>%
  left_join(muni_agg_zips[which(muni_agg_zips$income == "all"),], by.x = "zip", by.y = "zip") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts + no_agg_accts)) %>%
  select(-no_basic_accts, -no_agg_accts) %>%
  filter(income == "all") %>%
  select(-income)
boston_neighborhoods_zip_all
filename = paste0("../sept_2021_update/output/boston_neighborhoods_zip_all_",today,".csv")
write_csv(boston_neighborhoods_zip_all, filename )

# boston neighborhoods - low income
boston_neighborhoods_low <- muni_cs_neighborhoods %>%
  filter(municipality == "Boston") %>%
  filter(income == "low") %>%
  left_join(september_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(neighborhood) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(neighborhood, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(boston_basic_low, by.x = "neighborhood", by.y = "neighborhood") %>%
  left_join(boston_agg_low, by.x = "neigborhood", by.y = "neighborhood") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts + no_agg_accts)) %>%
  select(-no_basic_accts, -income, -no_agg_accts)
boston_neighborhoods_low
filename = paste0("../sept_2021_update/output/boston_neighborhoods_low_",today,".csv")
write_csv(boston_neighborhoods_low, filename )


# boston neighborhoods low income - by ZIP
boston_neighborhoods_zip_low <- muni_cs_neighborhoods%>%
  filter(municipality == "Boston") %>%
  filter(income == "low") %>%
  left_join(september_rates) %>%
  mutate(rate_premium = rate - basic_rate) %>%
  select(-new_accts, -Company) %>%
  group_by(zip) %>%
  summarize(total_consumer_loss_in_month = sum(rate_premium*kwh),
            average_per_hh_loss_in_month = total_consumer_loss_in_month / sum(no_accts),
            premium_per_kwh = total_consumer_loss_in_month/ sum(kwh),
            number_cs_accts = sum(no_accts)) %>%
  select(zip, total_consumer_loss_in_month, average_per_hh_loss_in_month, premium_per_kwh, number_cs_accts) %>%
  left_join(muni_basic_zips[which(muni_basic_zips$income == "low"),], by.x = "zip", by.y = "zip") %>%
  left_join(muni_agg_zips[which(muni_agg_zips$income == "low"),], by.x = "zip", by.y = "zip") %>%
  mutate(percent_hh_in_cs_mkt = number_cs_accts /(number_cs_accts + no_basic_accts + no_agg_accts)) %>%
  select(-no_basic_accts, -no_agg_accts) %>%
  filter(income == "low") %>%
  select(-income)
boston_neighborhoods_zip_low
filename = paste0("../sept_2021_update/output/boston_neighborhoods_zip_low_",today,".csv")
write_csv(boston_neighborhoods_zip_low, filename )



# highest rate charged ----
zipcode_cs_high_rate=zipcode_cs_files %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

zipcode_cs_high_rate = zipcode_cs_high_rate %>%  
  filter(kwh>0) %>%
  filter(no_accts>0) %>%
  filter(rate >.25) %>%
  arrange(desc(rate))
filename = paste0("../sept_2021_update/output/highest_rates_",today,".csv")
write_csv(zipcode_cs_high_rate, filename )



# demographic analyses - appendices 3b-3e ----

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

xwalk=read_excel("../cleaned_responses/zip_to_zcta.xlsx",
                 col_types = c("numeric","text"))



suppliers_by_zip=zipcode_cs_files %>%
  mutate(municipality=str_to_title(municipality),
         supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  filter(income=="all") %>% 
  neighborhood_to_municipality() %>%
  group_by(zip,municipality,region,supplier_id) %>%
  summarize(suppliers=1) %>% 
  group_by(zip,municipality,region) %>%
  summarize(suppliers=n()) 

summary(suppliers_by_zip)

zipcode_cs_files <- zipcode_cs_files %>%
  mutate(municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

zipcode_basic_files <- zipcode_basic_files %>%
  mutate(municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

zipcode_agg_files <- zipcode_agg_files %>%
  mutate(municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  neighborhood_to_municipality()

zips=zipcode_cs_files %>%
  left_join(september_rates, by = "region") %>% 
  select(-date, -new_accts, -supplier, -region, -Company) %>%
  rename(no_cs_accts = no_accts) %>%
  mutate(premium_billed_over_basic = kwh*(rate-basic_rate)) %>%
  group_by(zip, municipality, income) %>%
  summarize(tot_cs_accts = sum(no_cs_accts),
            tot_premium_billed = sum(premium_billed_over_basic),
            tot_kwh_billed = sum(kwh)) %>%
  left_join(zipcode_basic_files, by = c("zip","municipality","income")) %>%
  rename(tot_basic_accts = no_accts) %>%
  select(-region) %>%
  left_join(zipcode_agg_files, by = c("zip","municipality","income")) %>%
  rename(tot_agg_accts = no_accts) %>%
  select(-region, -supplier)

write_csv(zips, "../sept_2021_update/output/zips2.csv")


# Above generates some NAs because not every ZIP has all kinds of accounts

zips[is.na(zips$tot_basic_accts),"tot_basic_accts"]=0
zips[is.na(zips$tot_cs_accts),"tot_cs_accts"]=0
zips[is.na(zips$tot_agg_accts),"tot_agg_accts"]=0

write_csv(zips, "../sept_2021_update/output/zips3.csv")

zips_with_neighborhoods <- zips  %>% 
  filter(municipality == "Boston") %>% 
  filter(municipality != neighborhood.x) %>%
  select(zip, neighborhood.x) %>%
  mutate(city_neighborhood = paste(municipality, "-",neighborhood.x),
         zip = paste0("0", zip)) %>%
  unique() %>%
  select(-municipality, -neighborhood.x)

zips_with_neighborhoods[which(zips_with_neighborhoods$city_neighborhood == "Boston - Roxbry Xng"),]$city_neighborhood = "Boston - Roxbury Crossing"


zips4=zips %>%
  mutate(tot_accts=tot_basic_accts+tot_cs_accts+tot_agg_accts,
         avg_markup = tot_premium_billed / tot_cs_accts) %>%
  select(-neighborhood.x, -neighborhood.y)

zips4=recast(zips4,zip+municipality~income+variable, 
            id.var = c("zip", "municipality","income"), sum) 

zips4[is.na(zips$low_tot_accts),"low_tot_accts"]=0
zips4[is.na(zips$low_tot_basic_accts),"low_tot_basic_accts"]=0
zips4[is.na(zips$low_tot_cs_accts),"low_tot_cs_accts"]=0
zips4[is.na(zips$low_tot_agg_accts),"low_tot_agg_accts"]=0

write_csv(zips4, "../sept_2021_update/output/zips4.csv")

zips4 <- zips4 %>%
  select(zip, municipality, 
         all_tot_cs_accts, all_tot_basic_accts, all_tot_agg_accts, all_tot_accts, all_tot_premium_billed, all_tot_kwh_billed , 
         low_tot_cs_accts, low_tot_basic_accts, low_tot_agg_accts, low_tot_accts, low_tot_premium_billed, low_tot_kwh_billed ) %>%
  mutate(pct_low_inc_accts = low_tot_accts / all_tot_accts,
         avg_markup = ((all_tot_cs_accts * all_tot_premium_billed / all_tot_kwh_billed) + 
           (all_tot_cs_accts * all_tot_premium_billed / all_tot_kwh_billed)) / (all_tot_cs_accts + all_tot_cs_accts),
         nonlow_tot_cs_accts = all_tot_cs_accts - low_tot_cs_accts,
         nonlow_tot_accts = all_tot_accts - low_tot_accts,
         all_pct_cs=all_tot_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_tot_cs_accts/low_tot_accts,
                           0),
         nonlow_pct_cs=ifelse(nonlow_tot_accts>0,
                              nonlow_tot_cs_accts/nonlow_tot_accts,
                             0),
         agg_present=ifelse(all_tot_agg_accts>0,1,0)) %>%
left_join(suppliers_by_zip,by = c("zip","municipality")) %>%
  left_join(xwalk) %>%
  left_join(census_english) %>%
  left_join(census_race) %>%
  left_join(census_income) %>%
  left_join(census_poverty) %>%
  left_join(census_age) %>%
  left_join(census_disability) %>%
  filter(!municipality %in% light_plant_municipalities)
filename = paste0("../sept_2021_update/output/master_zipcode_",today,".csv")
write_csv(zips4,filename)



# make plot of participation rate versus low-income accounts ----
plotdata=filter(zips4,low_tot_cs_accts>9,
                municipality %in% c("Boston","Worcester","Springfield")) %>% 
  mutate(zip = paste0("0",zip)) %>% 
  select(pct_low_inc_accts,all_pct_cs,zip,municipality)
filename = paste0("../sept_2021_update/output/scatter_plot_",today,".csv")
fwrite(plotdata,file=filename)


theme_set(theme_gray(base_size = 50))
filename = paste0("../sept_2021_update/output/low_income_vs_participation_rate_",today,".png")
png(filename=filename,
    width=1.35*1250,
    height=1*1250)
ggplot(data=plotdata,aes(x=pct_low_inc_accts,y=all_pct_cs,label=zip,color=municipality))+
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
maj_min=zips4 %>%
  filter(pct_nonwhite>.5,
         all_tot_accts>9) %>%
  group_by(zip, municipality) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_nonwhite,
         all_tot_accts,
         pct_low_accts,
         avg_markup,
         all_pct_cs,
         low_pct_cs,
         nonlow_pct_cs) %>%
  arrange(desc(pct_nonwhite)) %>%
  mutate(zip = paste0("0",as.character(zip))) %>%
  left_join(zips_with_neighborhoods, by.x = zip, by.y = zip) %>%
  mutate(municipality = case_when(is.na(city_neighborhood) == TRUE ~ municipality, 
                                  is.na(city_neighborhood) == FALSE ~ city_neighborhood)) %>%
  select(-city_neighborhood)
summary=zips4 %>%
  filter(pct_nonwhite>.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Majority Minority",
            pct_nonwhite=sum(pct_nonwhite*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup), 
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts,na.rm=T)/sum(nonlow_tot_accts,na.rm=T)) %>%
  unique()
rest_of_state=zips4 %>%
  filter(!(is.na(pct_nonwhite)), # 46 rows missing pct_nonwhite
         pct_nonwhite<=.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_nonwhite=sum(pct_nonwhite*all_tot_accts,na.rm=T)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup), 
            all_pct_cs=sum(all_tot_cs_accts)/sum(all_tot_accts),
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts,na.rm=T)/sum(nonlow_tot_accts,na.rm=T)) %>%
  unique()
maj_min=bind_rows(summary,rest_of_state,maj_min) %>% 
  mutate(nonlow_pct_cs=ifelse(nonlow_pct_cs<0,0,nonlow_pct_cs))
filename = paste0("../sept_2021_update/output/majority_minority_",today,".csv")
write_csv(maj_min,filename)



top_25_income=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  head(.,n=25) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,Median_HH_Income,
         all_tot_accts,
         pct_low_accts,
         avg_markup,
         all_pct_cs,
         low_pct_cs,
         nonlow_pct_cs) %>%
  mutate(zip = paste0("0",as.character(zip))) %>%
  left_join(zips_with_neighborhoods, by.x = zip, by.y = zip) %>%
  mutate(municipality = case_when(is.na(city_neighborhood) == TRUE ~ municipality, 
                                  is.na(city_neighborhood) == FALSE ~ city_neighborhood)) %>%
  select(-city_neighborhood)
summary=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  head(.,n=25) %>%
  summarize(zip="-",
            municipality="Top 25: Med HH Inc",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
rest_of_state=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(Median_HH_Income)) %>%
  filter(!is.na(Median_HH_Income)) %>% # 63 rows missing Median_HH_Income
  tail(.,n=nrow(.)-25) %>%
  summarize(zip="-",
            municipality="Rest of State",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
top_25_income=bind_rows(summary,rest_of_state,top_25_income)
filename = paste0("../sept_2021_update/output/top_25_income_",today,".csv")
write_csv(top_25_income,filename)


bottom_25_income=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  head(.,n=25) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,Median_HH_Income,
         all_tot_accts,
         pct_low_accts,
         avg_markup,
         all_pct_cs,
         low_pct_cs,
         nonlow_pct_cs) %>%
  mutate(zip = paste0("0",as.character(zip))) %>%
  left_join(zips_with_neighborhoods, by.x = zip, by.y = zip) %>%
  mutate(municipality = case_when(is.na(city_neighborhood) == TRUE ~ municipality, 
                                  is.na(city_neighborhood) == FALSE ~ city_neighborhood)) %>%
  select(-city_neighborhood)
summary=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  head(.,n=25) %>%
  summarize(zip="-",
            municipality="Bottom 25: Med HH Inc",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
rest_of_state=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(Median_HH_Income) %>%
  filter(!is.na(Median_HH_Income)) %>% 
  tail(.,n=nrow(.)-25) %>%
  summarize(zip="-",
            municipality="Rest of State",
            Median_HH_Income=sum(Median_HH_Income*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
bottom_25_income=bind_rows(summary,rest_of_state,bottom_25_income)
filename = paste0("../sept_2021_update/output/bottom_25_income_",today,".csv")
write_csv(bottom_25_income,filename)


top_20_lep=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_lep_hh,
         all_tot_accts,
         pct_low_accts,
         avg_markup,
         all_pct_cs,
         low_pct_cs,
         nonlow_pct_cs)  %>%
  mutate(zip = paste0("0",as.character(zip))) %>%
  left_join(zips_with_neighborhoods, by.x = zip, by.y = zip) %>%
  mutate(municipality = case_when(is.na(city_neighborhood) == TRUE ~ municipality, 
                                  is.na(city_neighborhood) == FALSE ~ city_neighborhood)) %>%
  select(-city_neighborhood)
summary=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct Lim English",
            pct_lep_hh=sum(pct_lep_hh*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
rest_of_state=zips4 %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep_hh)) %>%
  filter(!is.na(pct_lep_hh)) %>% # 48 rows missing pct_lep_hh
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_lep_hh=sum(pct_lep_hh*all_tot_accts)/(sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            avg_markup = ave(avg_markup),
            all_pct_cs=sum(all_tot_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_tot_cs_accts)/sum(low_tot_accts),
            nonlow_pct_cs=sum(nonlow_tot_cs_accts)/sum(nonlow_tot_accts)) %>%
  unique()
top_20_lep=bind_rows(summary,rest_of_state,top_20_lep)
filename = paste0("../sept_2021_update/output/top_20_lep_",today,".csv")
write_csv(top_20_lep,filename)



print("Finished.")