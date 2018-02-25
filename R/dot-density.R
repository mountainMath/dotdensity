#' Dot density data merge function
#' Cleans the data and prepares them for dot-density maps
#'
#' Data at lower aggregation levels may not add up to the more accurate aggregate counts.
#' This function distributes the aggregate level counts proprtionally (by population) to the containing lower
#' leve geographic regions.
#'
#' @param data The base geographic data (sp or sf format)
#' @param parent_data Higher level geographic data (sp format)
#' @param geo_match A named string informing on what column names to match data and parent_data
#' @param categories Vector of column names to re-aggreagte
#' @param base Column name to use for proportional weighting when re-aggregating
#' @keywords dot-density reaggregate
#' @export
#' @examples
#' dot_density.proportional_re_aggregate(data=geo_db@data,parent_data=geo_da@data,geo_match=setNames("GeoUID","DA_UID"),categories=categories)
dot_density.proportional_re_aggregate <- function(data,parent_data,geo_match,categories,base="Population"){
  d1=data
  d2=parent_data
  vectors=categories
  #set NA to zero
  d1 <- d1 %>% replace(is.na(.), 0)
  d2 <- d2 %>% replace(is.na(.), 0)
  # create zero vectors if we don't have them on base (for example DB geo)
  missing=setdiff(vectors,names(d1))
  for (v in missing) {
    d1 <- d1 %>% mutate(!!v := 0)
  }
  ## compute the weights
  basex=as.name(paste(base,'x',sep="."))
  basey=as.name(paste(base,'y',sep="."))
  d1 <- d1 %>% inner_join(select(d2 %>% as.data.frame,c(vectors,c(as.character(geo_match),base))), by=geo_match) %>%
    mutate(weight = !!quo(UQ(basex) / UQ(basey)))  %>%
    replace(is.na(.), 0)
  ## aggregate variables up and down
  for (v in vectors) {
    vss=paste(v,'s',sep=".")
    vs=as.name(vss)
    vx=as.name(paste(v,'x',sep="."))
    vy=as.name(paste(v,'y',sep="."))
    d1 <- d1 %>% group_by(!!as.name(names(geo_match))) %>%
      mutate(!!vss := sum(!!vx)) %>%
      ungroup() %>%
      mutate(!!v := !!quo(UQ(vx) + weight * (UQ(vy) - UQ(vs))))
  }
  d1 <- d1 %>% select(-ends_with('.y')) %>%
    mutate(!!base := !!basex) %>%
    select(-!!basex)  %>%
    replace(is.na(.), 0)
  return(d1)
}

#' Dot density scale and compute dot data
#' Performs random rounding when scaling, randomly distributes dots in geographic areas and
#' randomizes the order.
#'
#' Some of the code is inspired by https://www.blog.cultureofinsight.com/2017/06/building-dot-density-maps-with-uk-census-data-in-r/
#'
#' @param geo_data The base geographic data (sp or sf format)
#' @param categories Vector of column names to re-aggreagte
#' @param scale How many units should be represented by 1 dot
#' @keywords dot-density
#' @export
#' @examples
#' dot_density.compute_dots(ge_data=geo_db,categories=categories, scale=25)
dot_density.compute_dots <- function(geo_data,categories,scale=1,datum=NA){
  geo_data <- geo_data  %>% sf::st_as_sf()
  orig_datum <- sf::st_crs(geo_data)$epsg
  if (is.na(datum)) datum=orig_datum

  random_round <- function(x) {
    v=as.integer(x)
    r=x-v
    test=runif(length(r), 0.0, 1.0)
    add=rep(as.integer(0),length(r))
    add[r>test] <- as.integer(1)
    value=v+add
    ifelse(is.na(value) | value<0,0,value)
    return(value)
  }

  geo_data <- geo_data %>%
    mutate_at(categories,funs(./scale)) %>%
    mutate_at(categories,random_round) %>%
    sf::st_as_sf() %>% sf::st_transform(datum)

  # # testing performance
  # x=categories[1]
  # d<-as(geo_data,"Spatial")
  # system.time(maptools::dotsInPolys(d, as.integer(geo_data[[x]]), f="random"))
  # system.time(suppressMessages(sf::st_sample(geo_data, geo_data[[x]])))

  d<-as(geo_data %>% sf::st_as_sf() ,"Spatial")
  dfs <- lapply(categories, function(x) {
    y<-maptools::dotsInPolys(d, geo_data[[x]], f="random") %>%
      sf::st_as_sf() %>%
      select(-ID) %>%
      mutate(Category=x)
  })
  # st_sample is really slow...
  # dfs <- lapply(categories, function(x) {
  #   tibble(geometry=suppressMessages(sf::st_sample(geo_data, geo_data[[x]])),
  #          Category=x) %>% sf::st_sf()
  # })

  # all colors in single vector and randomize order
  # randomize order so as not to draw one color at a time, with last color on top.

  dots <- do.call(rbind,dfs) %>%
    mutate(Category=factor(Category, levels = categories)) %>%
    sample_n(nrow(.)) %>%
    sf::st_sf(crs=datum) %>%
    sf::st_transform(orig_datum)

  return(dots )
}


#' Convenience function to produce dot-density input for ggplot2
#' Uses geom_point instead of geom_sf to get proper legend labelling
#' @param dots Dot data, e.g. output from dot_density.compute_dots
#' @param size Size of each dot
#' @param alpha Alpha value for the dots
#' @keywords dot-density
#' @export
#' @examples
#' dot_density.dots_map(dots,size=0.01,alpha=0.5)
dot_density.dots_map <- function(dots,size=0.01,alpha=0.5){
  sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
    ret <- sf::st_coordinates(x)
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    x <- x[ , !names(x) %in% names]
    ret <- setNames(ret,names)
    dplyr::bind_cols(x,ret)
  }
  ggplot2::geom_point(data = dots %>% sfc_as_cols,
                      ggplot2::aes(x, y, colour = Category),
                      shape=20,
                      size=size,
                      alpha=alpha)
}



