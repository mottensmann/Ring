#' Get place info for a coordinate
#'
#' @description
#' Executes the QGIS algorithm `qgis:joinattributesbylocation` via \code{\link[RQGIS3]{run_qgis}} to obtain place information from two vector layers. For example, administrative levels from [gadm.org](https://gadm.org/)) and corresponding TK25 numbers (e.g. available from [here](https://www.ornitho.de/index.php?m_id=1118&item=15)).
#'
#' @param lat latitude
#' @param lon longitude
#' @param GADM path to GADM ESRI shape file (.shp). By default GADM-NRW-NDS.shp, supplied as extdata with the package
#' @param Shape2 path to a ESRI shape file (.shp) with TK25. By default T25qdr.shp supplied as extdata with the package
#' @param root location of RQGIS3
#' @import reticulate
#' @export
#'
place.suggest <- function(lat = NULL, lon = NULL, root = 'C:/OSGeo4W64', Shape1 = NULL, Shape2 = NULL) {
  if ("RQGIS3" %in% rownames(utils::installed.packages())) {
   library("RQGIS3", quietly = T)
  } else {
    stop("Install RQGIS3 to proceed")
  }

  ## ===========================================================================
  ### Check if running 32 bit
  ### ==========================================================================
  #cat("Check system requirements ... \n")
  if (!R.version$arch == "x86_64") stop(paste0("Change to 64-bit version of R. R.version$arch = ", R.version$arch))
  ### ==========================================================================

  ## check that Shape1 file is there
  if (is.null(Shape1)) {
    if (file.exists(system.file("extdata", "GADM-NRW-NDS-2.shp", package = "RingR"))) {
      Shape1 <- system.file("extdata", "GADM-NRW-NDS-2.shp", package = "RingR")
    } else {
      stop("GADM-NRW-NDS-2.shp not found in extdata")
    }

  }

  if (is.null(Shape2)) {
    if (file.exists(system.file("extdata", "TK-VT.shp", package = "RingR"))) {
      Shape2 <- system.file("extdata", "TK-VT.shp", package = "RingR")
    } else {
      stop("TK-VT.shp not found in extdata")
    }

  }

  ## Where to find RQGIS
  if (!is.null(root)) {
    RQGIS3::set_env(root = root)
  } else {
    RQGIS3::set_env()
  }

  ## Turn coordinates into a shape file
  shape <- data.frame(N = lat, E = lon)

  ## set coords
  sp::coordinates(shape) =~E+N

  ## set CRS
  sp::proj4string(shape) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  ## convert to SpatialPointsDataFrame
  shape <- methods::as(shape,"SpatialPointsDataFrame")
  attr(shape,"data") <- data.frame(name = "coordinate", id = 1)

  ## write a temp file
  shape.temp <- temp.shp()

  rgdal::writeOGR(obj = shape, dsn = shape.temp, driver = "ESRI Shapefile", layer = "coord")

  ## Query algorithm

  joined.temp <- temp.shp()

  ## Shape1
  place <- RQGIS3::run_qgis(alg = "qgis:joinattributesbylocation",
                            INPUT = Shape1,
                            JOIN = shape.temp,
                            METHOD = 1,
                            PREDICATE = 0,
                            DISCARD_NONMATCHING = T,
                            OUTPUT = joined.temp,
                            load_output = T,
                            show_output_paths = F)


  ## Shape2
  tk <- RQGIS3::run_qgis(alg = "qgis:joinattributesbylocation",
                         INPUT = Shape2,
                         JOIN = shape.temp,
                         METHOD = 1,
                         PREDICATE = 0,
                         DISCARD_NONMATCHING = T,
                         OUTPUT = joined.temp,
                         load_output = T,
                         show_output_paths = F)

  ## reformat
  place <- data.frame(
    Lat = lat,
    Lon = lon,
    State = place[['NAME_1']],
    District = place[['NAME_2']],
    Municipality = place[['NAME_3']],
    TK25 = tk[["TK25_NR"]],
    TK25_quad = tk[["TK25_VIERT"]],
    #TK25_min = tk[["TK25_MIN_1"]],
    Lon.centroid = tk[["x_wgs84"]],
    Lat.centroid = tk[["y_wgs84"]],
    strTK25 = tk[["TK25_NR"]] %>% as.character(),
    strPlaceCode = ifelse(place[["NAME_1"]] == "Nordrhein-Westfalen", "DEFL", "DEGW")
  )


  place[["Code"]] <-
    paste0(place[["District"]], "; ",
           ifelse(place[["Municipality"]] != place[["District"]], place[["Municipality"]], ""),
           " TK", place[["TK25"]], "-", place[["TK25_quad"]],
           #" TK", place[["TK25"]], "-", place[["TK25_quad"]], " MF ", place[["TK25_min"]],
           " (", ifelse(place[["State"]] == "Nordrhein-Westfalen", "DEFL,", "DEGW,"),
           " TK", place[["TK25"]], ")"
  )

  ## discard temp files
  unlink.shp(shape.temp)
  unlink.shp(joined.temp)
  return(place)
}
