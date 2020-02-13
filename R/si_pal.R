si_colors <- list(flaming_red = "#EC1D26",
                  dandelion_gold = "#FFC609",
                  arctic_ice_blue = "#7CD8D5",
                  atlantic_teal = "#006B82",
                  global_gray = "#383C3B",
                  jon_pink = "#d197bd")

si_colors <- list(flaming_red = "#ffc712", dandelion_gold = "#82cfcf", "#006a88", "#383c3b", "#eb2029", "#f1c9cc", "#67aa7b", "#e4e318", "#f16921")

si_pal <- function(
  primary = "flaming_red",
  other = "dandelion_gold",
  direction = 1
) {
  stopifnot(primary %in% names(si_colors))

  function(n) {
    if (n > length(si_colors)) warning(paste("SI Color Palette only has", length(si_colors), "colors."))

    if (n == 2) {
      other <- if (!other %in% names(si_colors)) {
        other
      } else {
        si_colors[other]
      }
      color_list <- c(other, si_colors[primary])
    } else {
      color_list <- si_colors[1:n]
    }

    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}

scale_color_si <- function(
  primary = "flaming_red",
  other = "dandelion_gold",
  direction = 1,
  ...
) {
  ggplot2::discrete_scale(
    "colour", "si",
    si_pal(primary, other, direction),
    na.value = "gray90",
    ...
  )
}

scale_fill_si <- function(
  primary = "flaming_red",
  other = "dandelion_gold",
  direction = 1,
  ...
) {
  ggplot2::discrete_scale(
    "fill", "si",
    si_pal(primary, other, direction),
    na.value = "gray90",
    ...
  )
}
#
#
# cars %>%
#   mutate(sicol = factor(rep(1:9, length.out = n()))) %>%
#   ggplot(aes(x = speed, y = dist, color = sicol)) + geom_smooth() + scale_color_si()
# ggsave(filename = "~/temp/line gray.svg")
#
# cars %>%
#   mutate(sicol = factor(rep(1:9, length.out = n()))) %>%
#   ggplot(aes(x = speed, y = dist, color = sicol)) + geom_smooth() + scale_color_si() +theme_minimal()
# ggsave(filename = "~/temp/line white.svg")
#
#
# cars %>%
#   mutate(sicol = factor(rep(1:9, length.out = n()))) %>%
#   count(sicol) %>%
#   mutate(n = n + sample(c(-2, -1, 0, 1, 2), 9, replace = T)) %>%
#   ggplot(aes(x = sicol, y = n, fill = sicol)) + geom_col() + scale_fill_si()
# ggsave(filename = "~/temp/bar grey.svg")
#
# cars %>%
#   mutate(sicol = factor(rep(1:9, length.out = n()))) %>%
#   count(sicol) %>%
#   mutate(n = n + sample(c(-2, -1, 0, 1, 2), 9, replace = T)) %>%
#   ggplot(aes(x = sicol, y = n, fill = sicol)) + geom_col() + scale_fill_si() + theme_minimal()
# ggsave(filename = "~/temp/bar white.svg")
