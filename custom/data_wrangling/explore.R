    #  Explore MR data
    #  Josh Nowak
    #  08/2016
################################################################################
    require(dplyr)
    require(tidyr)

    #  Set working directory
    setwd("C:/Users/josh.nowak/Documents/BirdMR")
################################################################################
    #  Initial data handling - 2 data sets
    raw <- read.csv(
      file.path(getwd(),
        "/data/originals/MASTER_BANDING DATA 2007_FALL 2015_for Josh.csv"), 
      as.is = T)

    #  Subset to yellow warbler
    yw <- raw %>%
      filter(AOU == "YEWA")
      
    #  Define mpg related sites
    mpg <- c("MPGR", "SHEP", "RDGE")
    galen <- "GKNP"
    mthaggin <- "MHAG"
    moon <- "MOON"

    #  Encounter history
    ywd <- yw %>%
      select(BAND, DATE, TIME, NET, STATION, SEASON, STATUS, WEIGHT, AGE, SEX, 
        DISP)

    #  A look at the EH of an often encountered bird - same year
    filter(ywd, BAND == "234027729")
    filter(ywd, BAND == "277026196")
    filter(ywd, BAND == "234027780")

    #  Were any birds caught in multiple years?
    ywd <- ywd %>%
      mutate(
        Date = as.Date(DATE, "%d-%b-%y"),
        Year = as.numeric(format(Date, "%Y"))
      ) %>%
      select(-DATE)

    ywd %>%
      group_by(BAND) %>%
      summarise(
        nyr = length(unique(Year))
      ) %>%
      filter(nyr > 1)

    