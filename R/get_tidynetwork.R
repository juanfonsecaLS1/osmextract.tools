#' Obtain a sfnetwork object from OpenStreetMap data
#'
#' @param ... parameters passed to `osmextract::oe_get_network()`
#' @param simplify_highway logical, whether to simplify the highway values by removing the "_link" suffix and filtering by `highway_filter`
#' @param highway_filter character vector of highway types to keep, if `simplify_highway` is TRUE
#' @param directed logical, whether to return a directed sfnetwork object (default is FALSE)
#'
#' @returns a sfnetwork object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_area <- sf::st_point(c(-1.6005470549372385,53.836053590512215)) |>
#'   sf::st_sfc(crs = 4326) |>
#'   sf::st_buffer(units::set_units(1, "km"))
#'
#' sfnet_directed <- oe_get_sfnetwork(place = my_area, mode = "driving", directed = TRUE)
#'
#' sfnet_undirected <- oe_get_sfnetwork(place = my_area, mode = "driving", directed = FALSE)
#' }

oe_get_sfnetwork <- function(
  ...,
  directed = FALSE,
  simplify_highway = TRUE,
  highway_filter = c(
    "motorway",
    "trunk",
    "primary",
    "secondary",
    "tertiary",
    "unclassified",
    "residential"
  )
) {
  checkmate::assert_logical(directed, len = 1)

  net <- get_tidynetwork(
    ...,
    simplify_highway = simplify_highway,
    highway_filter = highway_filter
  )

  # Basic simplification using sfnetworks with the undirected graph
  net <- net_2_sfnet_undirected(net)

  if (directed) {
    # Prepare directed graph
    net <- prepare_directed(net)
  }

  net
}

net_2_sfnet_undirected <- function(net_sf) {
  sfnet <- sfnetworks::as_sfnetwork(
    x = net_sf,
    directed = FALSE
  )

  # Creating junctions where road segments overlap
  # This converts implicit intersections into explicit nodes
  sf_net_subdiv <- tidygraph::convert(sfnet, sfnetworks::to_spatial_subdivision)

  # Simplifying the interstitial nodes segments keeping
  # the oneway attribute and concatenating the other fields

  tidygraph::convert(
    sf_net_subdiv,
    sfnetworks::to_spatial_smooth,
    summarise_attributes = list(collapse_function),
    require_equal = "oneway"
  )
}

prepare_directed <- function(sfnet_und) {
  net_raw <- sfnetworks::activate(sfnet_und, "edges") |>
    dplyr::as_tibble() |>
    dplyr::select(-.data$from, -.data$to, -.data$z_order)

  # Reversing the geometries of bidirectional links
  net_rev <- sf::st_reverse(net_raw[net_raw$oneway == "no", ])

  # Binding the duplicated geometries
  dplyr::bind_rows(net_rev, net_raw) |>
    sfnetworks::as_sfnetwork(directed = TRUE)
}

#' Obtain a tidy sf from OpenStreetMap data
#'
#' @param ... parameters passed to `osmextract::oe_get_network()`
#' @param simplify_highway logical, whether to simplify the highway values by removing the "_link" suffix and filtering by `highway_filter`
#' @param highway_filter character vector of highway types to keep, if `simplify_highway` is TRUE
#'
#' @returns sf object with standardized highway and oneway values
#'
#' @export
#' @examples
#' \dontrun{
#' my_area <- sf::st_point(c(-1.6005470549372385,53.836053590512215)) |>
#'   sf::st_sfc(crs = 4326) |>
#'  sf::st_buffer(units::set_units(1, "km"))
#' tidynet_sf <- get_tidynetwork(place = my_area, mode = "driving")
#' }
get_tidynetwork <- function(
  ...,
  simplify_highway = TRUE,
  highway_filter
) {
  checkmate::assert_logical(simplify_highway, len = 1)

  args <- list(...)

  if ("extra_tags" %in% names(args)) {
    args$extra_tags <- c(args$extra_tags, "junction")
  } else {
    args$extra_tags <- "junction"
  }

  # Get network
  net <- do.call(osmextract::oe_get_network, args)

  # Tidy highway values
  if (simplify_highway) {
    net <- tidy_highway(net, highway_filter = highway_filter)
  }

  # Tidy oneway
  net <- tidy_oneway(net)

  net
}
