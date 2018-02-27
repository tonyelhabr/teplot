
# #' @format A character vector.
# #' @export
# te_colors <-
#   c(
#     `firebrick` = "#B22222",
#     `chartreuse` = "#7FFF00",
#     `royalblue` = "#4169E1",
#     `darkorange` = "#FF8C00",
#     `darkmagenta` = "#8B008B",
#     `gold` = "#FFD700",
#     `hotpink` = "#FF69B4",
#     `deepskyblue` = "#00BFFF",
#     `lightgrey` = "#CCCCCC",
#     `darkgrey` = "#8C8C8C"
#   )
te_colors <-
  c(
    `red` = "#B22222",
    `green` = "#7FFF00",
    `blue` = "#4169E1",
    `orange` = "#FF8C00",
    `purple` = "#8B008B",
    `yellow` = "#FFD700",
    `pink` = "#FF69B4",
    `turquoise` = "#00BFFF",
    `lightgrey` = "#CCCCCC",
    `darkgrey` = "#8C8C8C"
  )
devtools::use_data(te_colors, internal = FALSE, overwrite = TRUE)
