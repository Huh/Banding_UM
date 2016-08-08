    #  Mark-recapture of birds in Broot Valley
    #  Josh Nowak
    #  08/2016
################################################################################
    #  Load Packages
    require(tidyr)
    require(dplyr)

    #  Set working directory
    setwd("C:/Users/josh.nowak/Documents/BirdMR")
################################################################################
    #  Initial data handling - 2 data sets, but USFS set is only effort
    raw <- read.csv(
      file.path(getwd(),
        "/data/originals/MASTER_BANDING DATA 2007_FALL 2015_for Josh.csv"), 
      as.is = T)
    colnames(raw) <- tolower(colnames(raw))

    #  Create lookup of species codes and common names
    #  This only retains the species code for which there are Common-names given
    dics <- list()
    dics$species <- raw %>%
      select(aou, common_name) %>%
      distinct() %>%
      filter(common_name != "")

    #  Subset to necessary columns and rename them
    morph <- raw %>%
      select(
        band, date, time, net, 
        station, season, status, weight, 
        age, sex, disp, aou
      ) %>%
      rename("id" = band) %>%
      mutate(
        date = as.Date(date, "%d-%b-%y"),
        year = as.numeric(format(date, "%Y"))
      )

    #  Clean morphed data
    clean <- morph %>%
      filter(
        !is.na(id),
        !is.na(date),
        time > 0,
        station != "",
        !is.na(aou)
      )
################################################################################
    #  Load effort data
    eff <- read.csv(
        file.path(
          getwd(),
          "/data/originals/Effort_ALLYears_BEVP&FALL_For Josh.csv"
        ), as.is = T
      ) %>%
      .[,1:6] %>%
      rename(
        "year" = YEAR,
        "season" = LOC,
        "station" = STATION,
        "date" = DATE,
        "net" = NET.,
        "net_hours" = NET.HOURS    
      ) %>%
      mutate(
        date = as.Date(date, "%m/%d/%Y"),
        net = as.character(net)
      )
      
     #  Combine each capture with effort data
     band <- left_join(morph, eff)
################################################################################
    #  Summarise number of birds by station, season and year
    summ_caps <- band %>% 
      group_by(station, season, year) %>% 
      summarise(
        birds = n(), 
        males = sum(sex == "M"), 
        females = sum(sex == "F")
      ) %>%
      arrange(station, year, season)
################################################################################
    #  Summarise species by time
    summ_sptime <- morph %>%
      filter(time > 0) %>%
      count(time, aou)

    #  Plot yellow warbler captures by time
    plot_ly(filter(summ_sptime, aou == "YEWA"), 
      x = time, 
      y = n, 
      colors = "dodgerblue",
      type = "bar"
    )

    #  Alternative for single species - ugly
    plot_ly(filter(summ_sptime, aou == "YEWA"),
      x = time, 
      y = n,
      fill = "tozerox"
    )
################################################################################
    #  Summarise captures by species
    cap_sp <- band %>%
      count(aou)

    plot_ly(
      cap_sp,
      x = aou,
      y = n,
      type = "bar"
    )
    
    #  Sumarrise captures by species at each station, season, year
    cap_spstyr <- band %>%
      count(aou, station, season, year)
################################################################################
    #  Effort
    
################################################################################
    #  Weight
    wt_id <- morph %>%
      filter(!is.na(id), weight > 0) %>%
      group_by(id) %>%
      summarise(
        mu = mean(weight, na.rm = T),
        aou = aou[1]
      ) %>%
      arrange(aou)
    
    wt_season <- morph %>%
      filter(aou == "YEWA", !is.na(id), weight > 0)

    #  Boxplot of inidividual values
    plot_ly(wt_id,
      color = aou,
      y = mu,
      type = "box",
      showlegend = F
    )
    
    #  Boxplot of weight by season
    plot_ly(wt_season,
      color = season,
      y = weight,
      type = "box",
      showlegend = F
    )

################################################################################