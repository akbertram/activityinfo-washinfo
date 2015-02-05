# A script to create a table of "QIS VWC" locations in Bangladesh as available 
# in ActivityInfo.

require("activityinfo")

# get locations of type "QIS VWC" in Bangladesh:
locations <- getLocations(51036)

# define a helper function:
getEntityName <- function(loc, id) {
  if (!is.null(loc$adminEntities[[id]])) {
    loc$adminEntities[[id]]$name
  } else {
    NA_character_
  }
}

# create a handy table with the VWC names, their AI indentifier and the
# administrative entities where they are located:
location.table <-
  do.call(rbind, lapply(locations,
                        function(loc) {
                          data.frame(name = loc$name,
                                     locationId = loc$id,
                                     division = getEntityName(loc, "1267"),
                                     district = getEntityName(loc, "1523"),
                                     upazila  = getEntityName(loc, "1534"),
                                     stringsAsFactors = FALSE)
                        }))