#' format data
#'
#' @param buzzard path to buzzard_db.RData
#' @param kite path to Kite ringing list.xlsx
#' @param others path to Goshawk and others ringing list.xlsx
#' @param owls path to Eagle owl ringing list
#' @param year vector of years to export
#' @param exclude ring numbers to exclude from export (ie individuals without Vogelwartenring)

#' @import magrittr
#'
formatR <- function(
  buzzard = "~/01-PhD/00-Raw/RData/buzzard_db.RData",
  kite = "~/../Dropbox/Fieldsaison_2020/Kite ringing list 2020-10-28 MO.xlsx",
  owls = "~/../Dropbox/Fieldsaison_2020/Eagle Owl ringing list 2020-08-28 MO.xlsx",
  others = "~/../Dropbox/Fieldsaison_2020/Goshawk and others ringing list 2020-08-28 MO.xlsx",
  year = 2020,
  exclude = 3419178) {

  ## check function call
  if (is.null(buzzard)) stop("Specify path to 'buzzard_db.RData'")
  if (is.null(kite)) stop("Specify path to 'Kite ringing list.xlsx'")
  if (is.null(others)) stop("Specify path to 'Goshawk and others ringing list.xlsx'")
  if (is.null(year)) stop("Specify a vector of years to select data to submit")

  ### Load ringing data
  ### =========================================================================================================
  ### 1. Buteo buteo
  load(buzzard)
  cat("Load Buteo buteo data\n")
  rings <- dplyr::filter(buzzard_db[["ring_db"]], Year %in% year, last == 1) %>%
    .[, c("Territory", "Terr_ID","Date","ID", "Ring", "Sex", "Age", "Weight",
          "Wing", "Tarsus", "Brood_ID", "Brood_Size")]

  # Euring code for Buteo buteo
  rings$species <- "02871"

  # add coords to list
  rings <- dplyr::left_join(
    rings,
    dplyr::filter(buzzard_db[["repro_fledge_db"]], Year %in% year)[, c("Terr_ID", "N", "E")], by = "Terr_ID")

  # test that ring number is numeric, if not discard the entry, removes unringed
  rings <- rings[sapply(rings[["Ring"]], function(x) !grepl("\\D", x)) ,]

  ## 2. Milvus milvus & milvus migrans
  ### =========================================================================================================
  cat("Load milvus data\n")

  ## careful, the table contains also Ravens!
  kites <- readxl::read_xlsx(kite) %>%
    dplyr::filter(., format(Date, "%Y") %in% year,
                  Species %in% c("Red kite", "black kite"))

  ## Get factor denoting territory + Brood_ID
  kites[["Territory"]] <- lapply(kites[["Territory name"]], function(x) {
    ## checks for numbers and remove them from territory names
    if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x)) {
      out <- stringr::str_sub(x, start = 1, end = nchar(x) - 1) %>% trimws()
    } else {
      out <- trimws(x)
    }
    out
  }) %>% unlist() %>% as.factor()
  names(kites)[ which(names(kites) == "Ring no")] <- "Ring"

  # take last handling event if there were multiple
  kites <- kites[with(kites, order(Ring, Date, decreasing = T)),] %>% .[!duplicated(.[["Ring"]]),]

  ## Add Brood_Size
  kites <- dplyr::left_join(kites, data.frame(Territory = levels(kites$Territory),
                                              Brood_Size = kites[["Territory"]] %>% summary() %>% as.numeric()),
                            by = "Territory")

  kites <- data.frame(Territory = kites[["Territory"]],
                      Terr_ID = kites[["Territory"]],
                      Date = kites[["Date"]],
                      ID = kites[["Identification"]],
                      Ring = kites[["Ring"]] %>% as.character,
                      Sex = kites[["Sex (1=male 0=fem)"]],
                      Age = kites[["Age"]],
                      Weight = kites[["Weight"]] %>% as.character %>% as.numeric,
                      Wing = kites[["Wing"]],
                      Tarsus = kites[["Tarsus"]],
                      Brood_ID = kites[["Territory"]],
                      Brood_Size = kites[["Brood_Size"]],
                      species = dplyr::case_when(
                        kites$Species == 'Red kite' ~ "02390",
                        kites$Species == 'Black kite' ~ "02380"),
                      N = kites[["Geogr Breite"]],
                      E = kites[["Geogr Länge"]])

  ## remove unringed samples such as 3418736a
  kites <- kites[sapply(kites[["Ring"]], function(x) !grepl("\\D", x)) ,]


  ## 3. Accipiter gentilis, A. nisus & Pernis apivorus
  ### =========================================================================================================
  cat("Load Goshawk data\n")

  others <- readxl::read_xlsx(others) %>%
    dplyr::filter(., format(Date, "%Y") %in% year)

  ## Get factor denoting territory + Brood_ID
  others[["Territory"]] <- lapply(others[["Territory name"]], function(x) {
    ## checks for numbers and remove them from territory names
    if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x)) {
      out <- stringr::str_sub(x, start = 1, end = nchar(x) - 1) %>% trimws()
    } else {
      out <- trimws(x)
    }
    out
  }) %>% unlist() %>% paste0(., others[["Species"]]) %>% as.factor()

  ## Add Brood_Size
  others <- dplyr::left_join(others,
                             data.frame(Territory = levels(others$Territory),
                                        Brood_Size = others[["Territory"]] %>% summary() %>% as.numeric()),
                             by = "Territory")

  others <- data.frame(Territory = others[["Territory"]],
                       Terr_ID = others[["Territory"]],
                       Date = others[["Date"]],
                       ID = others[["Identification"]],
                       Ring = others[["Ring no"]] %>% as.character,
                       Sex = others[["Sex (1=male 0=fem)"]],
                       Age = others[["Age"]],
                       Weight = others[["Weight"]] %>% as.character %>% as.numeric,
                       Wing = others[["Wing"]],
                       Tarsus = others[["Tarsus"]],
                       Brood_ID = others[["Territory"]],
                       Brood_Size = others[["Brood_Size"]],
                       species = dplyr::case_when(
                         others[["Species"]] == "Goshawk" ~ "02670",
                         others[["Species"]] == "Sparrowhawk" ~ "02690",
                         others[["Species"]] == "Honey Buzzard" ~ "02310",
                         others[["Species"]] == "Kestrel" ~ "03040"),
                       N = others[["Geogr Breite"]],
                       E = others[["Geogr Länge"]])
  # take last handling event if there were multiple
  others <- others[with(others, order(Ring, Date, decreasing = T)),] %>% .[!duplicated(.[["Ring"]]),]

  ## remove unringed samples such as 3418736a
  if (any(others[["Ring"]] %>% as.character %>% stringr::str_detect(., c("a")))) {
    stop("Check Ring numbers of others")
  }
  ################################################################################
  ## 4. Eagle owl
  ### ==========================================================================
  cat("Load Eagle owl data\n")

  eagleowl <- readxl::read_xlsx(owls) %>%
    dplyr::filter(., format(Date, "%Y") %in% year)  %>%
    subset(., select = c(`Territory name`, Lat, Lon, Date, `Ring no`, Age, Species,
                         Weight, Wing, Tarsus))

  ## Get factor denoting territory + Brood_ID
  eagleowl[["Territory"]] <- lapply(eagleowl[["Territory name"]], function(x) {
    ## checks for numbers and remove them from territory names
    if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",x)) {
      out <- stringr::str_sub(x, start = 1, end = nchar(x) - 1) %>% trimws()
    } else {
      out <- trimws(x)
    }
    out
  }) %>% unlist() %>% paste0(., others[["Species"]]) %>% as.factor()

  ## Add Brood_Size
  eagleowl <- dplyr::left_join(eagleowl,
                               data.frame(Territory = levels(eagleowl$Territory),
                                          Brood_Size = eagleowl[["Territory"]] %>% summary() %>% as.numeric()),
                               by = "Territory")

  eagleowl <- data.frame(Territory = eagleowl[["Territory"]],
                         Terr_ID = eagleowl[["Territory"]],
                         Date = eagleowl[["Date"]],
                         ID = NA,
                         Ring = eagleowl[["Ring no"]] %>% as.character,
                         Sex = NA,
                         Age = eagleowl[["Age"]],
                         Weight = eagleowl[["Weight"]] %>% as.character %>% as.numeric,
                         Wing = eagleowl[["Wing"]],
                         Tarsus = eagleowl[["Tarsus"]],
                         Brood_ID = eagleowl[["Territory"]],
                         Brood_Size = eagleowl[["Brood_Size"]],
                         species = dplyr::case_when(
                           eagleowl[["Species"]] == "Eagle owl" ~ "07440"),
                         N = eagleowl[["Lat"]],
                         E = eagleowl[["Lon"]])

  ## remove unringed samples such as 3418736a
  if (any(eagleowl[["Ring"]] %>% as.character %>% stringr::str_detect(., c("a")))) {
    stop("Check Ring numbers of eagleowl")
  }
  ###############################################################################

  ## combine
  rings <- rbind(rings, kites, others, eagleowl)

  rm(list = c("kites", "others", "buzzard_db", "eagleowl"))

  # exclude 3418746, in captivity VHF
  if (!is.null(exclude)) rings <- rings[-which(rings[["Ring"]] %in% exclude),]

  # split id column to get wingtag and rewrite them
  rings[["wingtag"]] <- lapply(rings[["ID"]], function(x) {
    out <- strsplit(x, split = ",")[[1]][-1] %>% trimws()
    return(ifelse(length(out) == 1, out, NA))
  }) %>% unlist()

  ## remove wingtag if it is a black kite [2380]
  rings[["wingtag"]][which(rings[["species"]] == "02380")] <- NA
  rings[["wingtag"]] <- stringr::str_replace(rings[["wingtag"]], "wingtag", "Flügelmarken")
  rings[["wingtag"]] <- stringr::str_replace(rings[["wingtag"]], "yellow", "Gelbe")
  rings[["wingtag"]] <- stringr::str_replace(rings[["wingtag"]], "blue", "Blaue")
  rings[["wingtag"]] <- stringr::str_replace(rings[["wingtag"]], "white", "Weiße")

  ## add coordinate to individuals released as adults
  for (i in which(rings$Territory == "Freilassung Wittenbrock")) {
    rings[i,c("N", "E")] <- c(52.06479, 8.44390)
    rings[i, "Brood_ID"] <- NA
  }

  for (i in which(rings$Territory == "Freilassung Adlerwarte")) {
    rings[i,c("N", "E")] <- c(51.892262, 8.872981)
    rings[i, "Brood_Size"] <- 0
    rings[i, "Brood_ID"] <- NA
  }

   ## output
  rings <- data.frame(
    Ring = rings$Ring %>% as.character(),
    Species = rings$species %>% as.character(),
    Date = rings$Date,
    Age = rings$Age %>% as.character(),
    Sex = rings$Sex %>% as.numeric(),
    Weight = rings$Weight %>% as.numeric(),
    Wing = rings$Wing %>% as.numeric(),
    Tarsus = rings$Tarsus %>% as.numeric(),
    Brood_ID = rings$Brood_ID %>% as.character(),
    Brood_Size = rings$Brood_Size %>% as.numeric(),
    Wingtag = rings$wingtag %>% as.character(),
    Lon = rings$E %>% as.numeric(),
    Lat = rings$N %>% as.numeric())

  return(rings)

}

