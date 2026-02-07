install.packages("tidyverse")
library(tidyverse)
install.packages("haven")
library(haven)
getwd()
setwd("C:/Users/pavan/Desktop/R learning/AILENS/sdtm")
dm=read_xpt("dm.xpt")
subseted=dm |>
 filter(AGE>80)


getwd()

select (dm,starts_with("RF"))
select (dm,ends_with("DTC"))
getwd()
setwd("C:/Users/pavan/Desktop/R learning/R session/sdtm")

lb=read_xpt("lb.xpt")
lb1 = lb %>%
  mutate(
    SHIFT = case_when(
      LBBLFL == "Y" ~ NA_character_,
      LBSTRESN < LBSTNRLO ~ "LOW",
      LBSTRESN > LBSTNRHI ~ "HIGH",
      TRUE ~ "NORMAL"
    )
  ) %>%
  select(USUBJID, LBTESTCD, SHIFT, LBSTNRLO, LBSTNRHI, LBSTRESN)

lbtemp = lb %>%
  group_by(USUBJID,LBTEST) %>%
  mutate(
    maxaval=max(LBSTRESN,na.rm=TRUE)
  )

lbtemp1 = lb %>%
  group_by(USUBJID,LBTEST) %>%
  summarise(
    maxaval=max(LBSTRESN,na.rm=TRUE)
  )


lbtemp2 <- lb %>%
  group_by(USUBJID, LBTEST) %>%
  mutate(
    maxaval = if (all(is.na(LBSTRESN))) {
      NA_real_                     # no numeric values → return NA
    } else {
      max(LBSTRESN, na.rm = TRUE)  # numeric values → compute max
    },
    .groups = "drop"
  )

getwd()
setwd("C:/Users/pavan/Desktop/R learning/R session/UpdatedCDISCPilotData/UpdatedCDISCPilotData/ADAM")

adsl=read_xpt("adsl.xpt")
adlb=read_xpt("adlbc.xpt")

adlc_sl = adlb |>
          left_join(adsl,by=c("USUBJID"))

tfl <- dm |>
  group_by(ARM)|>
  summarise(
    MEAN=mean(AGE,na.rm=TRUE),
    SD=sd(AGE,na.rm=TRUE),
    .groups = "drop"
    ) |>
  pivot_longer(
    cols=c(MEAN,SD),
    names_to="STAT",
    values_to="VALUE"
  ) |>
  pivot_wider(
    names_from = ARM,
    values_from = c(VALUE)
  )
 
