# evaluate fixed charges

library(readxl)
library(dplyr)

Nan_all_file = "C:/Users/Tim/Downloads/Documents/business_docs/SMB_work/Aug_2021_MA_AG/NationalGrid_raw_responses/CONFIDENTIAL ATTACHMENT MA AG RESPONSES NANTUCKET QUESTIONS 1 AND 4 2021.xlsx"


############ Nantucket ################
Nan_all <- read_excel(Nan_all_file ,sheet = "Q1 (a) - (f) comp suppliers")
dim(Nan_all)
colnames(Nan_all)
all_suppliers <- unique(Nan_all$`(a)`)
num_suppliers <- length(all_suppliers) - 1

suppliers_fixed <- Nan_all %>% filter(`...8` > 0)
suppliers_fixed <- unique(suppliers_fixed$`(a)`)
num_suppliers_fixed <- length(suppliers_fixed) - 1

############ Eversource East ############
Ever_all_file = "C:/Users/Tim/Downloads/Documents/business_docs/SMB_work/Aug_2021_MA_AG/Eversource_raw_responses/Attachment AG-Oversight 1(a) CONFIDENTIAL- With Customer Charge.xlsx"
Ever_all <- read_excel(Ever_all_file, sheet = "Q1 Competitive Supply" )
colnames(Ever_all)
dim(Ever_all)

ever_all_supps <- unique(Ever_all$`Competitive Supplier`)
length(ever_all_supps)

ever_sups_fixed <- Ever_all %>% filter(`Customer Charge`>0)
num_ever_sups_fixed <- length(unique(ever_sups_fixed$`Competitive Supplier`))
num_ever_sups_fixed

############ NationalGrid (MECO) ########
MECO_file = "C:/Users/Tim/Downloads/Documents/business_docs/SMB_work/Aug_2021_MA_AG/NationalGrid_raw_responses/CONFIDENTIAL ATTACHMENT MA AG RESPONSES MECO QUESTIONS 1 AND 4 2021.xlsx"

MECO_all <- read_excel(MECO_file, sheet = "Q1 (a) - (f) comp suppliers" )
colnames(MECO_all)
dim(MECO_all)

MECO_suppliers <- unique(MECO_all$`(a)`)
MECO_suppliers
num_MECO_suppliers <- length(MECO_suppliers) - 1
num_MECO_suppliers

MECO_sup_fixed <- MECO_all %>% filter("...8"  == "$2.99 ")
num_MECO_fixed <- length(unique(MECO_sup_fixed$`(a)`))-1
num_MECO_fixed


