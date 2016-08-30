    #  Banding survival analysis utility functions
    #  Josh Nowak
    #  08/2016
################################################################################
    band_subset <- function(x, sp = NULL, stn = NULL, yr = NULL, seasn = NULL){
      #  A function to subset banding data
      #  Takes 
      #  x - data.frame
      #  sp - 4 letter species code, one or many
      #  stn - 4 letter station code, one or many 
      #  yr - 4 character year, one or many
      #  seasn - name of the season, one or many
      
      if(!is.null(sp)){
        x <- x %>%
          filter(aou %in% toupper(sp))
      }

      if(!is.null(stn)){
        x <- x %>%
          filter(station %in% toupper(stn))
      }

      if(!is.null(yr)){
        x <- x %>%
          filter(year %in% yr)
      }

      if(!is.null(seasn)){
        x <- x %>%
          filter(season %in% seasn)
      }

    return(x)
    }
################################################################################
    band_eh <- function(x){
      #  A function to create encounter histories from long data
      #  Takes a data frame with a column date used for new column headings
      #  Returns a data frame with one row per individual

      filler <- expand.grid(
        id = 000000,
        year = min(x$year, na.rm = T):max(x$year, na.rm = T),
        stringsAsFactors = F
      )
      
      out <- x %>%
        select(id, year) %>%
        distinct() %>%
        full_join(., filler) %>%
        mutate(y = 1) %>%
        tidyr::spread(year, y) %>%
        filter(id != 000000)

    return(out)
    }
################################################################################
    band_sex <- function(x){
      #  A function to collect and format individual sex covariate
      #  Takes output of subd
      #  Returns a data.frame

      fix_sex <- function(x){
        #  A function to standardize sex where 1 is female and 0 is male
        #  Takes sex column of data frame grouped by id
        x[!x %in% c("F", "M")] <- NA
        x[x == "M"] <- 0
        x[x == "F"] <- 1

        out <- suppressWarnings(max(x, na.rm = T))
      return(out)
      }

      samp_sex <- function(x){
        #  A function to sample missing values of sex from observed proportions
        #  Takes an ungrouped sex column (vector)
        #  Returns vector of imputed sex values, same length as input
        
        p <- x[!is.na(x)]
        p <- c(sum(p == 0)/length(p), sum(p == 1)/length(p))
        
        x[is.na(x)] <- sample(0:1, sum(is.na(x)), replace = T, prob = p)
      return(x)
      }

      #  Impute missing values
      out <- x %>% 
        group_by(id) %>%
        mutate(
          sex = fix_sex(sex)
        ) %>% 
        ungroup() %>% 
        mutate(sex = samp_sex(sex)) %>%
        select(id, sex)

    return(out)
    }
################################################################################
    gen_inits <- function(eh_data){
      known.state.cjs <- function(ch){
        state <- ch
        for (i in 1:dim(ch)[1]){
          n1 <- min(which(ch[i,]==1))
          n2 <- max(which(ch[i,]==1))
          state[i,n1:n2] <- 1
          state[i,n1] <- NA
          }
        state[state==0] <- NA
      return(state)
      }
      
      list(
        mean.phi = runif(1, 0, 1), 
        mean.p = runif(1, 0, 1),
        z = known.state.cjs(eh_data)
      )
    }
################################################################################
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
################################################################################
    band_wrapper <- function(x, band_sp, band_stn, band_yr, band_seasn, parms){
      #  A function to wrap the analysis into a single step
      #  Takes banding data (x), species to consider (band_sp), 
      #   station to consider (band_stn), year to consider (band_yr), 
      #   season to consider (band_seasn) and the parameters to monitor (parms).  
      #   Most inputs are defined by the data, but they may also be NULL.  
      #   The function expects the data to have columns:
      #   id, date, time, net, station, season, status, weight, age, sex, disp,
      #   aou, year
      #  Returns rjags object
      #  Note: this wrapper is a total hack and should be massaged into a nicer
      #   function
      
      #  Subset data
      subd <- band_subset(x, band_sp, band_stn, band_yr, band_seasn)

      #  Create encounter history
      eh <- band_eh(subd)

      #  Create summary of individual level & time constant covariates
      #sex <- band_sex(subd)

      # #  Summary of annual effort
      # fill_eff <- expand.grid(
        # year = min(eff$year, na.rm = T):max(eff$year, na.rm = T),
        # net_hours = 0
      # )

      # effort <- eff %>% 
        # group_by(year) %>%
        # full_join(., fill_eff) %>%
        # summarise(
          # hrs = sum(net_hours, na.rm = T)
        # )

      #  Get first observation
      get.first <- function(x) min(which(x == 1))
      f <- apply(eh[,-1], 1, get.first)

      y <- eh[f < max(f),-1]
      f <- f[f < max(f)]

      #  Replace NA after capture with 0
      for(i in 1:length(f)){
        for(j in (f[i] + 1):ncol(y)){
          y[i,j] <- ifelse(is.na(y[i,j]), 0, y[i,j])
        }
      }

      inits <- replicate(3, gen_inits(as.matrix(y)), simplify = F)

      out <- jags(
        data = list(y = y, f = f, nind = nrow(y), n.occasions = ncol(y)),
        inits = inits,
        parameters.to.save = parms,
        model.file = "C:/Users/josh.nowak/Documents/GitHub/Banding_UM/custom/models/c_c.txt",
        n.chains = 3,
        n.iter = 30000,
        n.burnin = 10000,
        n.thin = 1, 
        jags.module = c("glm", "dic")
      )
    
    return(out)
    }