
#' make_square
#'
#' @param p a ggplot object, ideally with discrete x and y aesthetics set
#' (i.e. a heatmap)
#'
#' @return the same object, with the aspect ratio fixed to be the (number of
#' rows)/(number of columns)
make_square <- function(p) {
  .x <- as.character(p$mapping$x)
  .y <- as.character(p$mapping$y)
  ncols <- length(unique(p$data[[.x]]))
  nrows <- length(unique(p$data[[.y]]))
  print(nrows)
  p + theme(aspect.ratio = nrows/ncols)
}

#' Set zeros to NA
#'
#' Set zeros in the data mapped to the fill aesthetic to NA, allowing them to
#' be given a specific color in the heatmap independent of the fill scale.
#'
#' @param p a ggplot object with a `fill' aesthetic
na_zeros <- function(p) {
  .fill <- as.character(p$mapping$fill)
  fill <- p$data[[.fill]]
  fill[fill==0] <- NA
  p$data[[.fill]] <- fill
  p
}

#' Heatmap color scale 1
#'
#' A color scale that moves from light blue to purple for most of the range,
#' with a high-range (default, >90%) color of bright fuschia.
#' Drop in replacement for scale_fill_gradientn.
#'
#' @param threshold the limit where the high-range color appears
#' @param ... arguments passed to scale_fill_gradientn
heatmap_scale_1 <- function(..., na.value="white", threshold=.9) {
  scale_fill_gradientn(
    na.value=na.value,
    colours = c("#e0ecf4", "#9ebcda", "#8856a7", "#dd1c77"),
    values=c(0, threshold/4, threshold, 1),
    ...)
}

#' Saturated rainbow for proportions
#'
#' Adapted from qiimer::saturated_rainbow, this provides a ggplot2
#' scale_fill_gradientn that adopts the conventions from that scale. Namely:
#' The lowest-abundance taxa are in dark blue, empty taxa are white, and
#' highly-abundant taxa (> threshold)
#'
#' @param threshold the limit where the high-range color appears
#' @param ... arguments passed to scale_fill_gradientn
#' @param scale the the maximum of the proportion (i.e. is it 0-1 scaled or 0-100?)
saturated_rainbow <- function(..., na.value="white", threshold=.4, scale=1) {
  rainbow_colors <- rev(rainbow(100*threshold, start=0, end=0.6))
  last_color <- tail(rainbow_colors, n=1)
  colors <- c(rainbow_colors, rep(last_color, 100*(1-threshold)))
  # colors[1] <- "white"
  scale_fill_gradientn(
    na.value=na.value,
    colors = colors,
    limits = c(0,1)*scale,
    breaks = seq(0.1, 0.9, by=0.2)*scale,
    labels = paste0(seq(10, 90, by=20), "%"),
    ...)
}


#' Collapse a heatmap if the x and y-axes include subgroups
#'
#' If the factor defined on the x-axis is a sample but there are actually
#' replicates for each sample, then defining the plot with aes(x=sample)
#' and then collapse_heatmap yields the mean of each replicate.
#'
#' Similarly, for the y-axis, if you're interested in higher-order taxa, set
#' aes(y=Genus) or similar and then collapse_heatmap to yield the mean abundance
#' for that taxa.
#'
#' If both axes contain subgroups, this averages over all of them.
#'
#' @param p a ggplot heatmap with x, y, and fill aesthetics defined
#' @param collapse_fn the function to use to combine all the inner values
#' @param ... additional arguments to collapse_fn
collapse_heatmap <- function(p, collapse_fn = mean, ...) {
  data <- p$data
  .x <- p$mapping$x
  .y <- p$mapping$y
  .fill <- p$mapping$fill

  # Programming with dplyr is a pain. This uses the standard eval modules to
  # fill in the `fill` column correctly and allow the user to specify a collapsing
  # function.
  data <- ungroup(data) %>% group_by_(.x, .y) %>% mutate_(.dots=setNames(
      list(lazyeval::interp(~ fn(x, ...), fn=collapse_fn, x=.fill)),
      c(as.character(.fill))))
  data <- ungroup(data) %>% group_by_(.x) %>% mutate_(.dots=setNames(
    list(~.fill/sum(.fill)), c(as.character(.fill))))
  p$data = data
  p
}


