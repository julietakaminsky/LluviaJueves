fixChlorophyllData <- function(chla){
  
  actualYear <- chla$Year[1]          # definir actualYear
  
  chla$Date <- ymd("1900-01-01")      # agrega una variable y le asigna una fecha inicial al data frame

  for(i in 1:nrow(chla)) {             # va a recorrer cada registro del data frame chla. i es una variable. para for siempre va i. todos los valores que puede tomar i. 
    
    if(is.na(chla$Year[i])) {          # si hay NA en la columna Year TRUE
      
      chla$Year[i] <- actualYear       # que agregue el valor de actualYEAR
      
    } else {                            # sino
      
      actualYear <- chla$Year[i]        # que el valor de actualYear sea el de Year
    }
    
    fechaTmp <- ymd( paste(chla$Year[i],chla$Month[i],1 ) )   # para que complete la fecha en la columna DATE, le decimos que tome el valor de año de Year, el de mes de MOnth y que ponga 1 como día, siguiendo el formato ymd = year month day
    
    if(is.na(fechaTmp)) {             # si en la fecha queda NA, entonces
      
     if(chla$Month[i]=="Mar" ) {      # y si en el mes hay "Mar"
        
      fechaTmp <- ymd( paste(chla$Year[i],3,1 ) )     # entonces que arme la fecha en Date siguiendo Year, poniendo 3 para marzo, y 1 para día
    
     } else if (chla$Month[i]=="Ago" ) {        # sino, si es Agosto
    
        fechaTmp <- ymd( paste(chla$Year[i],8,1 ) ) # Verdadero Agosto, poné
      
    } else {            # Si no
      
       fechaTmp <- dmy(chla$Month[i]) # armar la fecha siguiendo el formato day, month, year
      }
      
    } 
    
    chla$Date[i] <- fechaTmp # lo ponemos en el data frame
    
  }
  

  chla$IntegE1 <- round(abs(chla$IntegE1),2)
  
  chla$IntegE2 <- abs(as.numeric(chla$IntegE2))
   
  return(chla)
}