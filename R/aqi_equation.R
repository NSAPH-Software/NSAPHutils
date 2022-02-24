
#' @title Calculate Air Quality Index (AQI) and classification
#' @description takes in a vector of concentrations (of either PM2.5 or ozone)
#' and calculates the AQI, classification (ranging from "Good" to "Hazardous"),
#' and associated color (ranging from Green to Maroon)
#'
#' @param pollutant Either "pm2.5" or "ozone"
#' @param concentration A vector of concentrations of that pollutant,
#' in ug/m^3 for pm2.5 or in ppb for ozone
#'
#' @return A data frame with the columns Concentration, AQI, Class, and Color
#' @export
#'
#' @examples
#' aqi_equation("pm2.5", seq(0,300,10))
#' aqi_equation("ozone", seq(0,0.2,0.0075))


aqi_equation<- function(pollutant, concentration){

  #### Set up reference tables:

  ### PM2.5:

  PM25.Class<- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                 "Unhealthy", "Very Unhealthy", "Hazardous")
  PM25.Color<- c("Green", "Yellow", "Orange", "Red", "Purple", "Maroon")
  PM25.Conc_low<- c(0, 12.1, 35.5, 55.5, 150.5, 250.5)
  PM25.Conc_high<- c(12.099999, 35.499999, 55.499999, 150.499999, 250.499999, 5000)
  PM25.AQI_low<- c(0, 51, 101, 151, 201, 301)
  PM25.AQI_high<- c(50, 100, 150, 200, 300, 500)

  PM25.AQI_table<- data.frame(PM25.Color, PM25.Class,
                              PM25.Conc_low, PM25.Conc_high,
                              PM25.AQI_low, PM25.AQI_high)


  ### Ozone:

  O3.Class<- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
               "Unhealthy", "Very Unhealthy")
  O3.Color<- c("Green", "Yellow", "Orange", "Red", "Purple")
  O3.Conc_low<- c(0, 0.055, 0.071, 0.086, 0.106)
  O3.Conc_high<- c(0.05499, 0.07099, 0.08599, 0.10599, 0.200)
  O3.AQI_low<- c(0, 51, 101, 151, 201)
  O3.AQI_high<- c(50, 100, 150, 200, 300) # set this max value higher??

  ## Note: the reference said that AQI values >300 are calculated using 1-hour ozone breakpoints...
  ## ... unsure what this means?

  O3.AQI_table<- data.frame(O3.Color, O3.Class,
                            O3.Conc_low, O3.Conc_high,
                            O3.AQI_low, O3.AQI_high)

  #### Do the calculations:

  if(!is.numeric(concentration)){
    print("Please use numeric value for concentration")
    return(NULL)
  }else{

    y<- round(concentration, 4)

    if(pollutant %in% c("PM2.5", "pm2.5", "PM25", "pm25")){
      df<- PM25.AQI_table
    }else if(pollutant %in% c("ozone", "o3", "O3", "Ozone")){
      df<- O3.AQI_table
    }else{
      print("Please specify pollutant")
      return(NULL)
    }

    DF<- sapply(y, function(x){
      n_r<- which((df[,3] <= x)&(df[,4] >= x))[1]
      if(is.na(n_r)){
        # print("Please specify realistic concentration value")
        return(NULL)
      }else{
        Class<- df[n_r, 2]
        Color<- df[n_r, 1]
        AQI<- round( (df[n_r,6]-df[n_r,5])/(df[n_r,4]-df[n_r,3])*(x-df[n_r,3])+df[n_r,5] )
        Concentration<- x

        return(data.frame(Concentration, AQI, Class, Color))
      }
    })

    return( t(DF) )
  }
}
