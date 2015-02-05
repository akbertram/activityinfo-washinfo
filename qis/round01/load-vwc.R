
vwc <- readRDS("qis/round01/vwc.rds")
vwcTypeId <- 51036

for(i in 1:nrow(vwc)) {
  
  row <- vwc[i, ]
  
  location <- list(
    id=row$locationId, 
    locationTypeId=vwcTypeId,
    name=row$vwc,
    E1267=row$divisionId,
    E1523=row$districtId,
    E1534=row$upazilaId,
    longitude=row$LOCATION_LNG,
    latitude=row$LOCATION_LAT)
  
  
  executeCommand("CreateLocation", properties = location)
}
