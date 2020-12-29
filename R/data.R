#' #' ANAE dialect regions
#' #'
#' #' An sf object containing polygons delineating the dialect regions from the Atlas of North American English.
#' #'
#' #' @format A data frame/sf object with 17 rows and 6 variables:
#' #' \describe{
#' #'   \item{OBJECTID}{ID for GIS, I think?}
#' #'   \item{SHAPE_Leng}{Not sure what this is}
#' #'   \item{Dialect_Re}{Name of the dialect region}
#' #'   \item{Shape_Le_1}{Not sure what this is}
#' #'   \item{Shape_Area}{Area of the dialect region, not sure what units}
#' #'   \item{geometry}{sf geometry column}
#' #'   ...
#' #' }
#' #' @source YGDP-generated polygons
#' "anae"
#'
#' #' Carver dialect regions
#' #'
#' #' An sf object containing polygons delineating the dialect regions defined in Carver (1987)
#' #'
#' #' @format A data frame/sf object with 26 rows and 5 variables:
#' #' \describe{
#' #'   \item{ID}{ID for GIS, I think?}
#' #'   \item{Region}{Name of the region}
#' #'   \item{Sub_Region}{Name of the sub-region}
#' #'   \item{SubSub_Reg}{Name of the sub-sub-region}
#' #'   \item{geometry}{sf geometry column}
#' #'   ...
#' #' }
#' #' @source YGDP-generated polygons
#' "carver"
#'
#' #' NSW dialect regions
#' #'
#' #' An sf object containing polygons dividing the country into North/South/West regions.
#' #'
#' #' @format A data frame/sf object with 3 rows and 5 variables:
#' #' \describe{
#' #'   \item{OBJECTID}{ID for GIS, I think?}
#' #'   \item{SHAPE_Leng}{I don't know what this is}
#' #'   \item{SHAPE_Area}{Area of the polygon}
#' #'   \item{Dialect_Re}{Name of the region (north, south, or west)}
#' #'   \item{geometry}{sf geometry column}
#' #'   ...
#' #' }
#' #' @source YGDP-generated polygons
#' "nsw"
#'
#' #' US counties shapefiles
#' #'
#' #' An sf object containing county polygons, from the US census
#' #'
#' #' @format A data frame/sf object with 3142 rows and 56 variables:
#' #' @source \url{https://www.arcgis.com/home/item.html?id=a00d6b6149b34ed3b833e10fb72ef47b}
#' "counties"
#'
#' #' US urban areas shapefiles (census)
#' #'
#' #' An sf object containing urban/rural area polygons, from the US census
#' #'
#' #' @format A data frame/sf object with 3601 rows and 13 variables:
#' #' @source \url{https://hub.arcgis.com/datasets/1eb393576fb748fc9c4cc445018b30f2_0}
#' "counties"

#' Cars data
#'
#' Description
#'
#' @format description
"mtcars"
