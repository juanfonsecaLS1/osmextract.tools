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
    # This is implemented based on https://wiki.openstreetmap.org/wiki/Key:oneway#Implied_oneway_restriction
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
