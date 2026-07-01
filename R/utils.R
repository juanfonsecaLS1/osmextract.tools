#' @importFrom rlang .data

# This function simplifies the highway values by removing the "_link" suffix and filtering by `highway_filter` if specified.

tidy_highway <- function(net, highway_filter) {
  net$highway <- gsub(
    pattern = "_link",
    replacement = "",
    x = net$highway
  )

  if (!is.null(highway_filter)) {
    net <- net[net$highway %in% highway_filter, ]
  }
  net
}

#' Tidy the oneway values in a osm network
#'
#' This helper function standardises the oneway values in a osm network.
#' It also applies the implied `oneway` tag restriction based on the `junction`
#' tag values if specified.
#'
#' @param net_raw a `sf` object representing a spatial network with the `oneway` and `junction` columns
#' @param implied_oneway logical, whether to apply the implied `oneway` restriction
#'
#' @returns a `sf` object with standardised oneway values
#'
#' @export
#'
#' @details See [oneway tag Wiki](https://wiki.openstreetmap.org/wiki/Key:oneway#Implied_oneway_restriction).
#'
#'
#' @examples
#' \dontrun{
#' my_area <- sf::st_point(c(-1.6005470549372385,53.836053590512215)) |>
#'   sf::st_sfc(crs = 4326) |>
#'  sf::st_buffer(units::set_units(1, "km"))
#' sf_net <- oe_get_network(place = my_area, mode = "driving")
#' sf_net_tidy <- tidy_oneway(sf_net, implied_oneway = TRUE)
#' }
tidy_oneway <- function(
  net_raw,
  implied_oneway = TRUE
) {
  checkmate::assert_logical(implied_oneway)

  # Simplifying the bi-directional tags
  net_raw$oneway[
    is.na(net_raw$oneway) | net_raw$oneway %in% c("alternating", "reversible")
  ] <- "no"

  # Reversing the geometries with -1
  sf::st_geometry(net_raw[
    net_raw$oneway == "-1",
  ]) <- sf::st_reverse(sf::st_geometry(net_raw[net_raw$oneway == "-1", ]))

  net_raw$oneway[net_raw$oneway == "-1"] <- "yes"

  if ("junction" %in% names(net_raw) && implied_oneway) {
    message("The implied oneway restriction is applied.")

    net_raw$oneway[
      net_raw$junction %in% c("roundabout", "motorway") & net_raw$oneway == "no"
    ] <- "yes"
  } else {
    message(
      "The junction column is not present in the network. The implied oneway restriction is not applied."
    )
  }

  net_raw
}


collapse_function <- function(x) {
  paste(unique(x), collapse = ",")
}
