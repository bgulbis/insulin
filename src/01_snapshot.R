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

insulin_pts <- read_data(dir_raw, "pts-meds", FALSE) %>%
    as.patients()


# run MBO queries
#   * Location History
#   * Medications - Inpatient - All
#   * Orders

orders <- read_data(dir_raw, "orders_insulin", FALSE) %>%
    distinct() %>%
    rename(millennium.id = `Encounter Identifier`,
           order.datetime = `Date and Time - Original (Placed)`,
           stop.datetime = `Date and Time - Discontinue Effective`,
           order = `Mnemonic (Primary Generic) FILTER ON`,
           prn = `PRN Indicator`,
           order.id = `Order Id`,
           order.parent.id = `Parent Order Id`,
           order.unit = `Nurse Unit (Order)`) %>%
    mutate_at(c("order.datetime", "stop.datetime"), ymd_hms) %>%
    mutate_at(c("order.datetime", "stop.datetime"), with_tz, tzone = tz) %>%
    filter(is.na(stop.datetime) | floor_date(stop.datetime, "day") >= mdy("1/16/2018", tz = tz)) %>%
    mutate_at("order.parent.id", funs(na_if(., 0))) %>%
    mutate(orig.order.id = coalesce(order.parent.id, order.id))

active_orders <- orders %>%
    distinct(millennium.id) %>%
    mutate(order = TRUE)

all_orders <- read_data(dir_raw, "orders-parent", FALSE) %>%
    distinct() %>%
    select(millennium.id = `Encounter Identifier`,
           order.datetime = `Date and Time - Original (Placed)`,
           stop.datetime = `Date and Time - Discontinue Effective`,
           order = `Mnemonic (Primary Generic) FILTER ON`,
           prn = `PRN Indicator`,
           order.id = `Order Id`,
           order.parent.id = `Parent Order Id`,
           order.unit = `Nurse Unit (Order)`) %>%
    mutate_at(c("order.datetime", "stop.datetime"), ymd_hms) %>%
    mutate_at(c("order.datetime", "stop.datetime"), with_tz, tzone = tz) %>%
    # mutate_at("order", str_to_lower) %>%
    filter((is.na(stop.datetime) | floor_date(stop.datetime, "day") >= mdy("1/16/2018", tz = tz)),
           str_detect(order, regex("insulin", ignore_case = TRUE))) %>%
    mutate_at("order.parent.id", funs(na_if(., 0))) %>%
    mutate(orig.order.id = coalesce(order.parent.id, order.id))

active_orders_all <- all_orders %>%
    distinct(millennium.id) %>%
    mutate(order = TRUE)

distinct(all_orders, order)

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med.datetime > mdy("1/13/2018", tz = tz),
           med.datetime < mdy("1/16/2018", tz = tz)) %>%
    mutate_at("order.parent.id", funs(na_if(., 0))) %>%
    mutate(orig.order.id = coalesce(order.parent.id, order.id))

mbo_orders <- concat_encounters(meds$orig.order.id)

# run MBO query
#   * Orders Meds - Details - by Order Id

details <- read_data(dir_raw, "orders-details", FALSE) %>%
    as.order_detail()

x <- anti_join(meds, details, by = c("orig.order.id" = "order.id"))
y <- anti_join(orders, details, by = c("orig.order.id" = "order.id"))


active_meds <- meds %>%
    distinct(millennium.id) %>%
    mutate(dose = TRUE)

locations <- read_data(dir_raw, "location", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter(floor_date(arrive.datetime, "day") <= mdy("1/15/2018", tz = tz),
           floor_date(depart.datetime, "day") >= mdy("1/15/2018", tz = tz),
           !(location %in% c("HH EDTR", "HH EDHH", "HH VUHH", "HH EREV", 
                             "HH OBEC", "HH ADMT", "HH APAC", "HH PAHH", 
                             "HH PreAdmit OU", "HH DSU", "HH AMSA", "HH WCOR")),
           !str_detect(location, "^CY"))

insulin_location <- locations %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, location)

data_insulin <- all_pts %>%
    left_join(active_orders, by = "millennium.id") %>%
    left_join(active_meds, by = "millennium.id") %>%
    left_join(insulin_location, by = "millennium.id") %>%
    mutate_at("order", funs(coalesce(., dose))) %>%
    filter(facility != "HC Childrens",
           age > 14)

n <- all_pts %>%
    count(facility)

d <- data_insulin %>%
    group_by(facility) %>%
    summarize_at(c("order", "dose"), sum, na.rm = TRUE) %>%
    left_join(n, by = "facility") %>%
    mutate(pct_order = order / n,
           pct_dose_order = dose / order,
           pct_dose_total = dose / n)

active <- filter(all_pts, encounter.status == "Active") %>%
    count(facility)

d2 <- data_insulin %>%
    filter(encounter.status == "Active") %>%
    group_by(facility) %>%
    summarize_at(c("order", "dose"), sum, na.rm = TRUE) %>%
    left_join(n, by = "facility") %>%
    mutate(pct_order = order / n,
           pct_dose_order = dose / order,
           pct_dose_total = dose / n)

write.csv(data_insulin, "data/external/2018-01-15_insulin.csv", row.names = FALSE)

data_doses <- meds %>%
    full_join(orders, by = c("millennium.id", "orig.order.id"))
