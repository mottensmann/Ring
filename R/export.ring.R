#' Export ringing data for exchange with the IFV
#'
#' @description
#' This functions enables to export ringing data in a format that is compatible
#' with output of the software [Ring 2.0](https://www.ifv-vogelwarte.de/en/home-ifv/bird-ringing-centre/information-for-ringers/software-ring-20.html). The main intention is to avoid manual data entries in a situation where the ringing data has been already entered in another format. Hence, familiarity with the format of Ring 2.0 is required as well as some familiarity to code in `R` in order to achieve the desired output. The manual of Ring 2.0 is accessible [online](https://www.ifv-vogelwarte.de/fileadmin/resources/Beringerzentrale/RING_9.3-Programmdoku.pdf). Additionally, `Lookups` are available [here](https://www.ifv-vogelwarte.de/fileadmin/resources/Beringerzentrale/Lookups.mde)
#'
#' @details
#' There are some prerequisites:
#' R must be executed as as 32 bit version, to ensure proper connectivity with the package '\code{\link{RODBC}}.
#' In RStudio, this can be set under \code{Tools} > \code{Global options}. Secondly, it is required to set up a Microsoft Access Drivers (.mdb) linking to databases that are specified as parameters \code{DNS1} and \code{DNS2}. To work with these channels, databases must be created **before** running this function. On windows, you can use the `ODBC Data Source Administrator 32-bit` for this purpose. It is required to have two such databases, whereby `DNS1` is used as a template that therefore needs to be a valid output of `Ring 2.0`, with correctly specified data structure. `DNS2` is simply a copy of `DNS1`, which will be changed when running the function. To set up, simply search for the keyword `odbc` and launch `ODBC Data Sources 32-bit`. Then, on the `User DSN` page, click on `Add` and select `Driver to Microsoft Access (*.mdb)`. Choose a name e.g. `'Ring2018'` and select as database the previously created file `'Ring2018.mde'`. Note, you might need to select the option to show all file types, as '.mde' is not shown by default.
#'
#'It may happen that the export failes due to a variety of errors related to SQL operations. Then, the best way might be to restart with a freshly created DNS2 connection. Further, it might help to play with the function parameters in \code{\link[RODBC]{sqlDrop}} and \code{\link[RODBC]{sqlSave}}
#' @param DNS1
#' name of odbc database 32-bit with Driver to Microsoft access (*.mdb). Must contain correct VarTypes.
#' @param DNS2 name of odbc database 32-bit with Driver to Microsoft access (*.mdb).
#' @param rings data frame with these columns: \code{Ring} (Ring number, character), \code{Species} (EURING species code, character), \code{Date} (Date), \code{Age} (Age of individual, character), \code{Sex} (0 = Male, 1 = Female), \code{Weight} (Body mass in g, numeric), \code{Wing} (Wing chord length in mm, numeric), \code{Tarus} (Tarsus length in mm), \code{Brood_ID} (Brood identifier, character), \code{Brood_Size} (Brood size, numeric), \code{Wingtag} (wingtag code, character), \code{Lon} (Longitude in decimal degrees), \code{Lat} (Latitude in decimal degrees), \code{PlaceCode] (factor). **Note**: Missing values must be specified only as NA!
#'
#' @param PlaceCode data frame with place codes in EURING notation. Coordinates are centroids of the corresponding spatial polygons. Five columns: \code{Code} (place code in EURING notation), \code{Lon} (longitude in decimal degrees) and \code{Lat} (latidude in decimal degrees), \code{strTK25} (TK25 number), \code{strPlaceCode} (four letter EURING place code).
#'
#' @param RingerNr Ringer Identity (as issued by Vogelwarte Helgoland)

#'
#' @import RODBC magrittr
#' @export
#'
export.ring <- function(DNS1 = NULL, DNS2 = NULL, rings = NULL, PlaceCode = NULL, RingerNr = NULL) {

  ## Check function call
  ## ===========================================================================
  if (is.null(DNS1)) stop("Specify name of DNS1")
  if (is.null(DNS2)) stop("Specify name of DNS2")
  if (is.null(rings)) stop("Specify rings")
  if (is.null(RingerNr)) stop("Specify Ringer identity")
  if (is.null(PlaceCode)) stop("Place code not provided")

  ## check rings structure
  ## ===========================================================================
  if (ncol(rings) != 14) warning("Expected 14 columns in rings, got", ncol(rings))
  if (!all(names(rings) %in% c("Ring", "Species", "Date", "Age",
                              "Sex", "Weight", "Wing", "Tarsus",
                              "Brood_ID", "Brood_Size", "Wingtag",
                              "Lon", "Lat", "Code"))) warning("Unexpected column names in rings")
  if (ncol(PlaceCode) != 5) warning("Expected 3 columns in rings, got", ncol(PlaceCode))
  if (!all(names(PlaceCode) %in% c("Lon.centroid", "Lat.centroid", "Code", "strTK25", "strPlaceCode"))) warning("Unexpected column names in PlaceCode")

  ## ===========================================================================
  ### Check if running 32 bit
  ### ==========================================================================
  cat("Check system requirements ... \n")
  if (R.version$arch == "x86_64") stop(paste0("Change to 32-bit version of R. R.version$arch = ", R.version$arch))
  ### ==========================================================================

  ### Connect with access drivers
  ## ===========================================================================
  # template
  ch <- RODBC::odbcConnect(DNS1)
  cat('Connect to DNS1 ... \n')

  # export
  ch2 <- RODBC::odbcConnect(DNS2)
  cat('Connect to DNS2 ... \n')
  ## ===========================================================================

  ## retrieve data as data.frame
  ## ===========================================================================
  # tblRinging <- RODBC::sqlFetch(ch2, "tblRinging")
  tblRefer.old <- RODBC::sqlFetch(ch, "tblRefer")
  tblGeoTab <- RODBC::sqlFetch(ch, "tblGeoTab", as.is = T)
  # tblOpen <- RODBC::sqlFetch(ch, "tblOpen")



  ### Match ringing entries to closed GeoTab reference
  ## ===========================================================================
  tblGeoTab2 <- data.frame(idGeoTab = 1:nrow(PlaceCode), #+ 99
                           lngLong = PlaceCode$Lon.centroid %>% as.numeric(),
                           lngLat = PlaceCode$Lat.centroid %>% as.numeric(),
                           strPlace = PlaceCode$Code %>% as.character(),
                           dtmLastUpdateDate = Sys.time() %>% as.character(),
                           strTK25 = PlaceCode$strTK25,
                           strPlaceCode = PlaceCode$strPlaceCode)
  # tblGeoTab2 <- tblGeoTab
  # tblGeoTab2[["strTK25"]] <- lapply(tblGeoTab2[["strPlace"]], function(x) {
  #   as.character(x) %>%
  #     stringr::str_sub(., start = nchar(.) - 4, end = nchar(.) - 1) %>% trimws()
  # }) %>% unlist()
  #
  # tblGeoTab2[["strPlaceCode"]] <- lapply(tblGeoTab2[["strPlace"]], function(x) {
  #   as.character(x) %>%
  #     stringr::str_sub(., start = nchar(.) - 12, end = nchar(.) - 9) %>% trimws()
  # }) %>% unlist()

  ## determine row in GeoTab with the closest coordinate for all ringed individuals
  # rings[["idGeoTab"]] <- lapply(1:nrow(rings), function(x) {
  #   lapply(1:nrow(tblGeoTab), function(y) {
  #     geosphere::distGeo(p1 = rings[x ,c("Lon","Lat")],
  #                        p2 = tblGeoTab[y ,c("lngLong", "lngLat")])/1000
  #   }) %>% unlist() %>% order(., decreasing = F) %>% .[1]
  # }) %>% unlist %>% tblGeoTab[., "idGeoTab"]

  #rings <- dplyr::left_join(rings, tblGeoTab2, by = "idGeoTab")
  rings <- dplyr::left_join(rings, tblGeoTab2, by = c("Code" = "strPlace"))
  ## ===========================================================================

  ### Create tblRinging
  ## ===========================================================================
  cat("Create tblRinging\n")
  tblRinging <- lapply(1:nrow(rings), function(i) {
    result <- data.frame(
      strRingScheme = 'DEW',
      strRingNr =
        rep(".", 10 - nchar(rings[["Ring"]][i])) %>% paste0(., collapse = "") %>%
        paste0(., rings[["Ring"]][i]) %>% as.factor,
      strSpecies = as.factor(rings[[i ,"Species"]]) %>% as.factor,
      strRecoveryChances = ifelse(!is.na(rings[[i, "Wingtag"]]), 6, 1) %>% as.factor,
      strSex = lapply(rings[[i, "Sex"]], function(x) {
        if (is.na(x)) {
          out <- 0 # unknown
        } else if (x == 0) {
          out <- 2 # female
        } else {
          out <- 1 # male
        }
        return(out)
      }) %>% unlist() %>% as.factor,
      strAge = ifelse(rings[[i, "Age"]] %in% c("juv", "juvenile"), 1, 4) %>% as.factor,
      strStatusBroodsize = ifelse(rings[[i, "Age"]] %in% c("juv", "juvenile"), rings[[i, "Brood_Size"]],0) %>% as.factor,
      lngPullusAge = as.integer(0),
      lngAccPullusAge = as.integer(0),
      dtmDate = paste(rings[[i, "Date"]], "00:00:00"),
      strAccuracyDate = as.integer(0),
      strPlaceCode = rings[[i, "strPlaceCode"]] %>% as.factor,
      idGeoTab = rings[[i, "idGeoTab"]] %>% as.integer,
      strAccuracyPlace = 1 %>% as.factor,
      strTK25 = rings[[i, "strTK25"]] %>% as.factor,
      strRingerNr = as.factor(RingerNr),
      strCatchingMethods = as.factor("1"),
      strCatchingLures = as.factor("8"),
      lngWingLength = ifelse(is.na(rings[[i, "Wing"]]), 0.0, rings[[i, "Wing"]]) %>% as.numeric(),
      lngLengthP8 = 0,
      lngWeight = ifelse(is.na(rings[[i, "Weight"]]), 0.0, rings[[i, "Weight"]]) %>% as.numeric(),
      lngTarsus = ifelse(is.na(rings[[i, "Tarsus"]]), 0.0, rings[[i, "Tarsus"]]) %>% as.numeric(),
      dtmLastUpdateDate = Sys.time() %>% as.character(),
      strRemarks = rings[[i, "Wingtag"]],
      strNet = NA,
      strProject = NA,
      lngLong = rings[[i, "lngLong"]],
      lngLat = rings[[i, "lngLat"]]
    )
  }) %>%
    do.call("rbind",.)
  ## NA mean status in unknown i.e. birds from rehabilitation center
  tblRinging[["strStatusBroodsize"]][is.na(tblRinging[["strStatusBroodsize"]])] <- "O"
  ## ===========================================================================

  ### Create tblRefer
  ## ===========================================================================

  ## subset to all rings with sibships
  with_sibs <- rings[["Brood_ID"]] %>% stats::na.omit() %>% as.character() %>%
    .[duplicated(.)] %>%
    unique

  rings <- dplyr::filter(rings, Brood_ID %in% with_sibs) %>%
    .[with(., order(Brood_ID, Ring)),]

  ## Build data frame
  tblRefer <- lapply(with_sibs, function(x) {

    # find sibs of focal ind
    relatives <- dplyr::filter(rings, Brood_ID == x)
    # N - 1 rows are required to express relations
    out <- lapply(2:nrow(relatives), function(j) {
      out <- data.frame(
        idRefer = 999, # last number of old project, changed below
        strRingScheme1 = "DEW",
        strRingNr1 =
          rep(".", 10 - nchar(relatives[[1, "Ring"]])) %>% paste0(., collapse = "") %>% paste0(., relatives[[1, "Ring"]]),
        strRingScheme2 = "DEW",
        strRingNr2 =
          rep(".", 10 - nchar(relatives[[j, "Ring"]])) %>% paste0(., collapse = "") %>% paste0(., relatives[[j, "Ring"]]),
        dtmDate = relatives[[1, "Date"]],
        strRelation = "4" %>% as.factor,
        dtmLastUpdateDate = Sys.time())
    }) %>% do.call("rbind", .)
  }) %>% do.call("rbind",.)

  tblRefer$idRefer <- max(tblRefer.old[["idRefer"]]) + 1:nrow(tblRefer) # keep order of all previous entries
  ## ===========================================================================

  ### Write to database
  ## ===========================================================================

  ## retrieve info about all tables
  tables.ch2 <- RODBC::sqlTables(channel = ch2)

  cat('Export to DNS2 ... \n')
  ## Try to drop tables if they exsist ...
  if ("tblRinging" %in% tables.ch2[["TABLE_NAME"]]) {
    RODBC::sqlDrop(ch2, "tblRinging", errors = FALSE)

  }
  if ("tblRefer" %in% tables.ch2[["TABLE_NAME"]]) {
    RODBC::sqlDrop(ch2, "tblRefer", errors = FALSE)
  }

  if ("tblGeoTab" %in% tables.ch2[["TABLE_NAME"]]) {
    #RODBC::sqlDrop(ch2, "tblGeoTab", errors = FALSE)
    RODBC::sqlClear(ch2, "tblGeoTab", errors = FALSE)

  }

  ## get vartypes
  tmp <- RODBC::sqlColumns(ch, "tblRinging", as.is = T)
  varTypes <- as.character(tmp$TYPE_NAME)
  names(varTypes) <- as.character(tmp$COLUMN_NAME)
  RODBC::sqlSave(channel = ch2,
          dat = tblRinging,
          tablename = "tblRinging",
          varTypes = varTypes,
          rownames = FALSE,
          safer = FALSE)

  ## get vartypes
  tmp <- RODBC::sqlColumns(ch, "tblRefer", as.is = T)
  varTypes <- as.character(tmp$TYPE_NAME)
  names(varTypes) <- as.character(tmp$COLUMN_NAME)
  RODBC::sqlSave(ch2, tblRefer, varTypes = varTypes, rownames = FALSE)

  ## save data frame GeoTab to database
  tmp <- RODBC::sqlColumns(ch, "tblGeoTab", as.is = T)
  varTypes <- as.character(tmp$TYPE_NAME)
  names(varTypes) <- as.character(tmp$COLUMN_NAME)
  tblGeoTab <- subset(tblGeoTab2, select = -c(strTK25, strPlaceCode))
  RODBC::sqlSave(ch2, tblGeoTab, varTypes = varTypes, rownames = FALSE, append = FALSE, safer = FALSE, fast = FALSE) #, append = TRUE

  ## ===========================================================================

  ### Close connection to database
  ## ===========================================================================
  cat('Close connection to DNS1 ... \n')
  odbcClose(ch)
  cat('Close connection to DNS2 ... \n')
  odbcClose(ch2)
  # odbcClose(ch3)
  ## ===========================================================================
  return(list(tblRinging = tblRinging,
              tblRefer = tblRefer,
              tblGeoTab = tblGeoTab))
  cat("Finished\n")
}
