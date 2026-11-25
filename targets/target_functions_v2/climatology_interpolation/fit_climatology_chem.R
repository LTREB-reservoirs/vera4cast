# Title: DOY models with and without interpolation: FluoroProbe
# Author: Mary Lofton
# Date: 19SEP23

#Purpose: fit DOY model for FluoroProbe from 2014-present

library(tidyverse)
library(lubridate)
library(mgcv)
library(zoo)
library(cowplot)

#'Function to fit day of year model for chla
#'@param data data frame with columns datetime, site_id, depth_m, observation, variable
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period
#'@param plot_file filepath to where you want plot output written
#'
#'Output:
#'for each variable, side-by-side plots of the fitted GAM for interpolated-to-daily
#'and not interpolated data, with interpolated points a different color

fit_DOY_chem <- function(data, cal_dates, plot_file){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    mutate(Date = date(datetime)) %>%
    filter(Date >= start_cal & Date <= stop_cal) %>%
    mutate(doy = yday(Date)) 
  
  #get vector of variables
  vars <- unique(df$variable)
  
  #loop thru variables
  for(i in 1:length(vars)){
    
    #subset data
    sub <- df %>%
      filter(variable == vars[i]) %>%
      select(Date, doy, observation)
    
    #create data frame for interpolation and interpolate
    daily_dates <- tibble(seq.Date(from = as.Date(start_cal), to = as.Date(stop_cal), by = "day"))
    colnames(daily_dates) <- "Date"
    interp_df <- left_join(daily_dates, sub) %>%
      mutate(observation = ifelse(is.nan(observation), NA, observation), 
             doy = yday(Date), 
             interp_flag = ifelse((is.na(observation)),"interpolated","obs"))
    
    interp_df$observation[1] <- 2.2 # set first value to first actual value
    
    interp_df <- interp_df |>
      mutate(observation = na.approx(observation))
    
    #format for GAM
    colnames(sub) <- c("Date","x","y")
    colnames(interp_df) <- c("Date","x","y","interp_flag")
    
    #fit GAM following methods in ggplot()
    my.gam <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                        data = sub, method = "REML")
    my.gam1 <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                        data = interp_df, method = "REML")
    
    #get GAM predictions
    GAM_predicted <- mgcv::predict.gam(my.gam, data.frame(x=sub$x))
    GAM_predicted_interp <- mgcv::predict.gam(my.gam1, data.frame(x=interp_df$x))
    sub$pred <- GAM_predicted
    interp_df$pred <- GAM_predicted_interp
    
    #plot GAM predictions and data
    GAM_plot <- ggplot()+
      xlab("DOY")+
      ylab(vars[i])+
      geom_point(data = sub, aes(x = x, y = y, fill = "obs"))+
      geom_line(data = sub, aes(x = x, y = pred, color = "DOY model"), linewidth = 1)+
      theme_classic()+
      scale_color_manual(values = c("obs" = "black","DOY model" = "blue"))+
      labs(color = NULL, fill = NULL)+
      ggtitle(paste0(vars[i],": Observed only"))
    #GAM_plot
    
    GAM_plot_interp <- ggplot()+
      xlab("DOY")+
      ylab(vars[i])+
      geom_point(data = interp_df, aes(x = x, y = y, color = interp_flag))+
      geom_line(data = interp_df, aes(x = x, y = pred, color = "DOY model"), linewidth = 1)+
      theme_classic()+
      scale_color_manual(values = c("obs" = "black","interpolated" = "gray","DOY model" = "blue"))+
      labs(color = NULL, fill = NULL)+
      ggtitle(paste0(vars[i],": Interpolated"))
    #GAM_plot_interp
    
    #combine plots and write to file
    p <- plot_grid(GAM_plot, GAM_plot_interp)
    ggsave(p, filename = paste0(plot_file,vars[i],".png"),
           device = "png", height = 3, width = 10, units = "in")
    
  }
  
}
