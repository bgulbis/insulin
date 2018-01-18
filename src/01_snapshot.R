library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"
tz <- "US/Central"

insulin <- med_lookup("insulin") 

mbo_insulin <- concat_encounters(insulin$med.name)
    
# run MBO query
#   * Patients - All During Dates

all_pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients(extras = list("admit.datetime" = "`Date and Time - Admit`",
                              "encounter.status" = "`Encounter Status`",
                              "encounter.class" = "`Encounter Class Type`")) %>%
    mutate_at(c("admit.datetime"), ymd_hms) %>%
    mutate_at(c("admit.datetime"), with_tz, tzone = tz) %>%
    filter(!(encounter.status == "Discharged" & is.na(discharge.datetime)),
           encounter.class != "Emergency",
           floor_date(admit.datetime, "day") < mdy("1/16/2018", tz = tz),
           (is.na(discharge.datetime) | floor_date(discharge.datetime, "day") == mdy("1/15/2018", tz = tz)))
    
mbo_pts <- concat_encounters(all_pts$millennium.id)

# run MBO queries
#   * Medications - Inpatient - All
#   * Orders

orders <- read_data(dir_raw, "orders", FALSE) %>%
    distinct() %>%
    rename(millennium.id = `Encounter Identifier`,
           order.datetime = `Date and Time - Original (Placed)`,
           stop.datetime = `Date and Time - Discontinue Effective`,
           order = `Mnemonic (Primary Generic) FILTER ON`,
           order.id = `Order Id`,
           parent.order.id = `Parent Order Id`,
           order.unit = `Nurse Unit (Order)`) %>%
    mutate_at(c("order.datetime", "stop.datetime"), ymd_hms) %>%
    mutate_at(c("order.datetime", "stop.datetime"), with_tz, tzone = tz) %>%
    filter(is.na(stop.datetime) | floor_date(stop.datetime, "day") >= mdy("1/16/2018", tz = tz))

active_orders <- orders %>%
    distinct(millennium.id) %>%
    mutate(order = TRUE)

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() 

active_meds <- meds %>%
    filter(med.datetime > mdy("1/13/2018", tz = tz),
           med.datetime < mdy("1/16/2018", tz = tz)) %>%
    distinct(millennium.id) %>%
    mutate(dose = TRUE)

data_insulin <- all_pts %>%
    left_join(active_orders, by = "millennium.id") %>%
    left_join(active_meds, by = "millennium.id")

n <- all_pts %>%
    count(facility)

d <- data_insulin %>%
    group_by(facility) %>%
    summarize_at(c("order", "dose"), sum, na.rm = TRUE) %>%
    left_join(n, by = "facility") %>%
    mutate(pct = dose / order,
           pct_order = order / n)
