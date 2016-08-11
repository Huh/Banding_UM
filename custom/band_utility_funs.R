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