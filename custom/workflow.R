    #  Banding survival analysis
    #  Josh Nowak
    #  08/2016
################################################################################
    #  Load packages
    require(R2jags)
    require(tidyr)
    require(dplyr)
    
    #  Load data
    load(
      file.path(
        "C:/Users/josh.nowak/Documents/BirdMR",
        "/data/working/band_data.RData"
      )
    )
    
    #  Load effort data
    eff <- read.csv(
        file.path(
          "C:/Users/josh.nowak/Documents/BirdMR",
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

    #  Source functions
    source(
      file.path(
        "C:/Users/josh.nowak/Documents/GitHub/Banding_UM",
        "custom/band_utility_funs.R"
      )
    )
################################################################################
    #  Set inputs
    recaps <- function(x){
      x %>%
        group_by(id, aou) %>%
        summarise(nyr = length(unique(year))) %>%
        ungroup() %>%
        filter(nyr > 1) %>%
        group_by(aou) %>%
        summarise(
          nind = n(),
          nrecap = sum(nyr, na.rm = T)
        )
    }

    #  Calculate the number of recaptures spanning more than 1 year
    recaps(band) %>% as.data.frame

    #  Species
    band_sp <- "GRCA"
    #  Station
    band_stn <- NULL
    #  Year(s)
    band_yr <- NULL
    #  Season
    band_seasn <- NULL
    #  Parameters to monitor
    parms <- c("survival", "mean.phi", "mean.p", "detection")
################################################################################
    #  Subset data
    subd <- band_subset(band, band_sp, band_stn, band_yr, band_seasn)

    #  Create encounter history
    eh <- band_eh(subd)

    #  Create summary of individual level & time constant covariates
    sex <- band_sex(subd)

    #  Summary of annual effort
    fill_eff <- expand.grid(
      year = min(eff$year, na.rm = T):max(eff$year, na.rm = T),
      net_hours = 0
    )
    
    effort <- eff %>% 
      group_by(year) %>%
      full_join(., fill_eff) %>%
      summarise(
        hrs = sum(net_hours, na.rm = T)
      )
    
    #  Get first observation
    get.first <- function(x) min(which(x == 1))
    f <- apply(eh[,-1], 1, get.first)
################################################################################
    y <- eh[f < max(f),-1]
    f <- f[f < max(f)]
    
    #  Replace NA after capture with 0
    for(i in 1:length(f)){
      for(j in (f[i] + 1):ncol(y)){
        y[i,j] <- ifelse(is.na(y[i,j]), 0, y[i,j])
      }
    }

    inits <- replicate(3, gen_inits(as.matrix(y)), simplify = F)

    fit <- jags(
      data = list(y = y, f = f, nind = nrow(y), n.occasions = ncol(y)),
      inits = inits,
      parameters.to.save = parms,
      model.file = "C:/Users/josh.nowak/Documents/GitHub/Banding_UM/custom/models/c_c.txt",
      n.chains = 3,
      n.iter = 3000,
      n.burnin = 1000,
      n.thin = 1, 
      jags.module = c("glm", "dic")
    )
################################################################################