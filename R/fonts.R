# Reference: https://github.com/hrbrmstr/hrbrthemes/issues/18.
# WARNING: This will overwrite the Arial Narrow installation.
# install.packages("extrafontdb")  # reset fonttable
# devtools::install_github("hrbrmstr/hrbrthemes", force = TRUE)
# hrbrthemes::import_roboto_condensed()
# library(hrbrthemes)
# d <-
#   read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)
# d[grepl("Light", d$FontName), ]$FamilyName <- font_rc_light
# write.csv(d, extrafont:::fonttable_file(), row.names = FALSE)
# extrafont::loadfonts()
# library(ggplot2)
# ggplot(mtcars, aes(mpg, wt)) +
#   geom_point(aes(color = factor(carb))) +
#   labs(
#     x = "Fuel effiiency (mpg)",
#     y = "Weight (tons)",
#     title = "Seminal ggplot2 scatterplot example",
#     subtitle = "A plot that is only useful for demonstration purposes",
#     caption = "Brought to you by the letter 'g'"
#   ) +
#   scale_color_ipsum() +
#   # theme_ipsum_rc()
#   theme_ipsum(base_family = "Arial Narrow")
