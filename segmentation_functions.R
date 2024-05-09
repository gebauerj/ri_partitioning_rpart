### Libraries ###

library(rpart)
library(rpart.plot)
library(reflimR)
library(tidyverse)
library(ggplot2)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

### Function at work ###

##function to draw reflims

draw_ri <- function(gx, x){
  g <- gx
  
  for (i in 1:nrow(x)){
    xx <- x[i,]
    
    if (xx$unisex == FALSE & xx$SEX == 'M'){
      ri_colour <- 'cornflowerblue'
    } else if (xx$unisex == FALSE & xx$SEX == 'F'){ 
      ri_colour <- 'indianred'
    } else {ri_colour <- 'black'}
    
    if (xx$method == 'reflimR'){
      ri_line <- 'solid'
    } else {ri_line = 'dashed'}
    
    g <- g + geom_rect(alpha = 0, colour = ri_colour, linetype = ri_line, xmin = xx$min_age, xmax = xx$max_age,
                       ymin = xx$lower.lim,
                       ymax = xx$upper.lim)
  }
  return(g)
}

##function to calculate parameters for Harris&Boyd
calc_stats <- function(segmentation_df, data_df, var_analyte, sex){
  # 
  if(sex == 'U'){
    ## here only the M RI are used if unisex == TRUE. MAKE SURE M/F RI ARE THE SAME OR IMPLEMENT SPECIFIC FUNCTION!
    xdf <- segmentation_df %>% filter(analyte == var_analyte & SEX %in% c('M') & unisex == TRUE)
  } else{
    xdf <- segmentation_df %>% filter(analyte == var_analyte & SEX == sex & unisex == FALSE)
  }
  
  #generate a template df to populate with data trough iteration
  template_df <- tibble(lower.lim = numeric(), 
                            upper.lim = numeric(), 
                            mean = numeric(), 
                            sd = numeric(),
                            n.trunc = numeric())
  
  # iterate through each row of the segmentation df
  for (i in 1:nrow(xdf)){
    x <- xdf[i,]
    # split data intp female/male or not depending on specific segment
    if(sex == 'U'){
      a_df <- data_df %>% filter(between(AGE_DAYS, x$min_age, x$max_age) & SEX %in% c('M','F'))
      actual_sex <- 'unisex'
    } else {
      a_df <- data_df %>% filter(between(AGE_DAYS, x$min_age, x$max_age) & SEX == sex)
      actual_sex <- x$SEX
    }
    
    #reset variable during the loop
    method_used <- 'init'
    lower <- NULL
    upper <- NULL
    
    
    # Versuche, a_ri zu berechnen
    tryCatch({
      main_title <- paste0(x$analyte, ' ', actual_sex, ' AGE: ', x$min_age, " - ", x$max_age)
      a_ri <- a_df$VALUE %>% reflim(n.min = 40, plot.all = TRUE, main = main_title)
      
      # use reflim RI if there is no error
      method_used <- 'reflimR'
      lower <- unname(a_ri$limits['lower.lim'])
      upper <- unname(a_ri$limits['upper.lim'])
    }, 
    # use quantile if reflim is not working
    
    ## is this not working??
    error = function(e) {
      print(paste("Fehler aufgetreten:", e))
      
      lower <- quantile(a_df$VALUE, 0.025)
      upper <- quantile(a_df$VALUE, 0.975)
    })
    
    if (is.na(a_ri$limits['lower.lim'])){
    method_used <- 'quantile'
    lower <- quantile(a_df$VALUE, 0.025)
    upper <- quantile(a_df$VALUE, 0.975)
    }
    
    # create output vector with lower und upper lim from 
    create_df <- tibble(lower.lim = lower,
                        upper.lim = upper,
                        mean = mean(a_df$VALUE),
                        sd = sd(a_df$VALUE),
                        n.trunc = length(a_df$VALUE),
                        method = method_used)
    
    template_df <- bind_rows(template_df, create_df)
    
  }
  df_out <- bind_cols(xdf, template_df)
  return(df_out)
}

# Function to calculate Harris & Boyd z-value and critical value according to Lahti 2004
calc_HarrisBoyd <- function(df1, df2){
  z <- abs((df1$mean-df2$mean)/sqrt((df1$sd^2/df1$n.trunc)+(df2$sd^2/df2$n.trunc)))
  print(paste0('z-value: ', z))
  critical_value <- 3*sqrt((df1$n.trunc+df2$n.trunc)/240)
  print(paste0('critical value: ', critical_value))
}

# Function to calculate permissible uncertainty for the RI
calc_permissible_uncertainty <- function(vec_lower, vec_upper){
  v <- NULL
  for (i in 1:length(vec_lower)){
    vi <- as_tibble_row(permissible_uncertainty(vec_lower[i], vec_upper[i]))
    if (is.null(v)){
      v <- vi
    } else{
      v <- bind_rows(v, vi)
    }
  }
  return(v)
}