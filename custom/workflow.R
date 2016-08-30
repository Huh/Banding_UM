    #  Banding survival analysis
    #  Josh Nowak
    #  08/2016
################################################################################
    #  Load packages
    require(R2jags)
    require(tidyr)
    require(dplyr)
################################################################################
    #  Load data
    #  Avian ecology lab's data
    load(
      file.path(
        "C:/Users/josh.nowak/Documents/GitHub/Banding_UM/data",
        "megan.RData"
      )
    )
    #  USFS data
    load(
      file.path(
        "C:/Users/josh.nowak/Documents/GitHub/Banding_UM/data",
        "dave.RData"
      )
    )
################################################################################
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
################################################################################
    #  Source functions
    source(
      file.path(
        "C:/Users/josh.nowak/Documents/GitHub/Banding_UM",
        "custom/band_utility_funs.R"
      )
    )
################################################################################
    #  Options for subsetting and the like
    #  Species
    dave_sp <- recaps(dave) %>% filter(nind > 9) %>% .$aou
    megan_sp <- recaps(megan) %>% filter(nind > 9) %>% .$aou
    #  Station
    band_stn <- NULL
    #  Year(s)
    band_yr <- NULL
    #  Season
    band_seasn <- NULL
    #  Parameters to monitor
    parms <- c("survival", "mean.phi", "mean.p", "detection")
################################################################################
    #  Run it by calling wrapper
    mg <- megan %>%
      filter(aou %in% megan_sp) %>%
      group_by(aou) %>%
      do(fit = try(band_wrapper(., NULL, NULL, NULL, NULL, parms)))

    dv <- dave %>%
      filter(aou %in% dave_sp) %>%
      group_by(aou) %>%
      do(fit = try(band_wrapper(., NULL, NULL, NULL, NULL, parms)))
################################################################################
    #  Plot output
    p_fun <- function(x, sp){
      par(mfrow = c(1, 2))
      hist(x[,"detection"], 
        freq = F,
        breaks = 100,
        col = "black",
        main = paste(sp, "P(Detection)"),
        xlab = "Estimate"
      )
      lines(density(x[,"detection"]), col = "gray40", lwd = 4)
      hist(x[,"survival"],
        freq = F,
        breaks = 100,
        col = "dodgerblue", 
        border = "dodgerblue",
        main = paste(sp, "P(Survival)"),
        xlab = "Estimate"
      )
      lines(density(x[,"survival"]), col = "#DC143C", lwd = 4)
    }
    
    s_plot <- function(x, sp){
      par(mfrow = c(1,1))
      hist(x[,"survival"],
        freq = F,
        breaks = 100,
        col = "dodgerblue",
        border = "dodgerblue",
        main = paste(sp, "P(Survival)"),
        xlab = "Estimate"
      )
      lines(density(x[,"survival"]), col = "#DC143C", lwd = 4)
    }

    #  Plot megan's data
    lapply(1:nrow(mg), function(i){
      tmp <- do.call(rbind, as.mcmc(mg$fit[[i]]))

      png(
        file.path(
          "C:/Users/josh.nowak/Documents/BirdMR/plots",
          paste0("mg_", mg[i,1], ".png")
        ),
        width = 9,
        height = 9,
        units = "in",
        res = 600
      )
      
      p_fun(tmp, mg[i,1])

      dev.off()
      
      png(
        file.path(
          "C:/Users/josh.nowak/Documents/BirdMR/plots",
          paste0("mg_surv_", mg[i,1], ".png")
        ),
        width = 9,
        height = 9,
        units = "in",
        res = 600
      )
      
      s_plot(tmp, mg[i,1])
      
      dev.off()
    })

    #  Plot Dave's data
    lapply(1:nrow(dv), function(i){
      tmp <- do.call(rbind, as.mcmc(dv$fit[[i]]))

      png(
        file.path(
          "C:/Users/josh.nowak/Documents/BirdMR/plots",
          paste0("dv_", dv[i,1], ".png")
        ),
        width = 9,
        height = 9,
        units = "in",
        res = 600
      )
      
      p_fun(tmp, dv[i,1])

      dev.off()
      
      png(
        file.path(
          "C:/Users/josh.nowak/Documents/BirdMR/plots",
          paste0("dv_surv_", dv[i,1], ".png")
        ),
        width = 9,
        height = 9,
        units = "in",
        res = 600
      )
      
      s_plot(tmp, dv[i,1])
      
      dev.off()
    })
################################################################################
    #  Table the results
    lapply(1:nrow(mg), function(i){
      write.csv(
        mg[i,2][[1]][[1]]$BUGS$summary,
        file =
          file.path(
            "C:/Users/josh.nowak/Documents/BirdMR/tables",
            paste0("mg_table_", mg[i,1], ".csv")
          )
      )
    })
    lapply(1:nrow(dv), function(i){
      write.csv(
        dv[i,2][[1]][[1]]$BUGS$summary,
        file =
          file.path(
            "C:/Users/josh.nowak/Documents/BirdMR/tables",
            paste0("dv_table_", dv[i,1], ".csv")
          )
      )
    })
################################################################################
    #  End