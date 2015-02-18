# A script to create a table of "QIS VWC" locations in Bangladesh as available 
# in ActivityInfo.

require("activityinfo")

# get locations of type "QIS VWC" in Bangladesh:
cat("...Downloading 'QIS VWC' locations from ActivityInfo...")
locations <- getLocations(51036)

# define helper functions:
getEntityId <- function(loc, id) {
  if (!is.null(loc$adminEntities[[id]])) {
    loc$adminEntities[[id]]$id
  } else {
    NA_integer_
  }
}

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
                                     divisionId = getEntityId(loc, "1267"),
                                     division = getEntityName(loc, "1267"),
                                     districtId = getEntityId(loc, "1523"),
                                     district = getEntityName(loc, "1523"),
                                     upazilaId = getEntityId(loc, "1534"),
                                     upazila  = getEntityName(loc, "1534"),
                                     stringsAsFactors = FALSE)
                        }))