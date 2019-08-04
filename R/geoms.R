#' @export
GeomTimeline <-  ggplot2::ggproto("GeomTimeline",Geom,
                 required_aes=c("x"),
                 default_aes=aes(alpha=0.5,
                                 shape=19,
                                 y=0.1,
                                 colour="grey",
                                 fill="grey",
                                 size=1.5,
                                 stroke=0.5),
                 draw_key=draw_key_point,
                 draw_panel=function(data, panel_params, coord){

                   coords <- coord$transform(data, panel_params)

                   points <- grid::pointsGrob(x = coords$x,y = coords$y,
                                    pch = coords$shape,
                                    gp = grid::gpar(col = coords$colour,
                                                    alpha = coords$alpha,
                                                    fill = coords$fill,
                                                    fontsize=coords$size*5))

                   lines <- coords %>%
                     tidyr::gather(key = "axis", value = "val", .data$x,.data$y) %>%
                     dplyr::group_by(.data$group, .data$axis) %>%
                     dplyr::summarise(ax_start = min(.data$val), ax_stop =max(.data$val)) %>%
                     tidyr::gather(key = "point", value = "val", .data$ax_start:.data$ax_stop)

                   xs <- lines[lines$axis=="x","val"]

                   ys <- lines[lines$axis=="y","val"]

                   grp <- lines[lines$axis=='x', "group"]

                   timeline <- grid::polylineGrob(x = xs$val,y = ys$val,
                                                  id = grp$group,
                                    gp = grid::gpar(col = "black",
                                                   alpha = 1,
                                                   fill = "black"))

                   grid::gTree(children = grid::gList(points, timeline))


                 })


#' Plot timeline of earthquakes
#'
#' @inheritParams ggplot2::layer
#'
#' @param ... Additional parameters passed onto layer function
#' @examples
#' \dontrun{
#' quakes_df %>%
#'  ggplot(aes(x=DATE,size=EQ_PRIMARY,y=COUNTRY, color=DEATHS, label=LOCATION_NAME))+
#'  geom_timeline()
#'
#' }
#'
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = "identity", position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @export
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr matches
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr desc
GeomTimelineLabel <-  ggplot2::ggproto("GeomTimelineLabel",Geom,
                                       required_aes=c("x", "label"),
                                       default_aes=aes(alpha=0.5,
                                                       shape=19,
                                                       y=0.1,
                                                       colour="grey",
                                                       fill="grey",
                                                       size=1.5,
                                                       stroke=0.5
                                       ),
                                       draw_key=draw_key_point,
                                       draw_panel=function(data, panel_params, coord, n_max=5){

                                         coords <- coord$transform(data, panel_params) %>%
                                           tidyr::nest(-.data$group) %>%
                                           dplyr::mutate(big_quakes = purrr::map(data, ~dplyr::arrange(.,dplyr::desc(size)) %>% head(5))) %>%
                                           dplyr::select(-.data$data) %>%
                                           tidyr::unnest()


                                         label_y <- coords$y+0.05

                                         line_coords <- coords %>%
                                           tibble::rowid_to_column() %>%
                                           dplyr::mutate(x_end = .data$x, y_end=.data$y+0.05) %>%
                                           dplyr::select(.data$rowid, dplyr::matches("x|y")) %>%
                                           tidyr::gather(key = "coord", value = "val", .data$x, .data$x_end, .data$y, .data$y_end)

                                         xs <- line_coords[stringr::str_detect(line_coords$coord, "x"),"val"]

                                         ys <- line_coords[stringr::str_detect(line_coords$coord, "y"),"val"]

                                         line_ids <- line_coords[stringr::str_detect(line_coords$coord, "x"), "rowid"]


                                         qk_lines <- grid::polylineGrob(x = xs,
                                                                        y = ys,
                                                                        id = line_ids,
                                                                        gp = grid::gpar(col="black", lwd=1, fill="black"))

                                         qk_labels <- grid::textGrob(label = coords$label,
                                                                     x = coords$x,
                                                                     y = label_y,
                                                                     just = c("left", "bottom"),
                                                                     rot = 40, gp = grid::gpar(col="black",
                                                                                               fontsize=8))

                                         grid::gTree(children = grid::gList(qk_labels, qk_lines))

                                       })

#' Label large earthquakes with annotation
#'
#' This geom is intended to be used in conjunction with a geom_timeline layer to provide additional annotations to a timeline of earthquakes
#'
#' @inheritParams ggplot2::layer
#' @param n_max Number of points to annotate
#' @param ... Other arguments passed to layer function
#' @examples
#' \dontrun{
#'
#' quakes_df %>%
#'  ggplot(aes(x=DATE,size=EQ_PRIMARY,y=COUNTRY, color=DEATHS, label=LOCATION_NAME))+
#'  geom_timeline()+
#'  geom_timeline_label()+
#'
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, n_max=5, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = "identity", position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max=n_max, ...)
  )
}


