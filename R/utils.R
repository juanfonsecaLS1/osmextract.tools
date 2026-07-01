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
#' @details For more information on the implied oneway restriction, see [wiki.openstreetmap.org](https://wiki.openstreetmap.org/wiki/Key:oneway#Implied_oneway_restriction).
#'
#'
#' @examples
#' \dontrun{
#' sf_net <- osmextract::oe_get_network(place = "ITS Leeds", mode = "driving")
#' sf_net_tidy <- tidy_oneway(sf_net, implied_oneway = TRUE)
#' }
tidy_oneway <- function(
  net_raw,
  implied_oneway = TRUE
) {
  if (!is.logical(implied_oneway)) {
    stop(
      "The implied_oneway parameter must be a logical value (TRUE or FALSE)."
    )
  }

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

# Function for summarise attributes of edges when converting to sfnetwork
collapse_function <- function(x) {
  paste(unique(x), collapse = ",")
}

check_highway_filter <- function(highway_filter) {
  match.arg(
    highway_filter,
    c(
      "busway",
      "cycleway",
      "footway",
      "living_street",
      "motorway",
      "path",
      "pedestrian",
      "primary",
      "residential",
      "rest_area",
      "service",
      "services",
      "steps",
      "tertiary",
      "track",
      "trunk",
      "unclassified"
    ),
    several.ok = TRUE
  )
}
