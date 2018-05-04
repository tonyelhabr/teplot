

# # ggplot2::`%+replace%`
# `+%replace%` <-
#   function (e1, e2) {
#     if (!is.theme(e1) || !is.theme(e2)) {
#       stop("%+replace% requires two theme objects", call. = FALSE)
#     }
#     e1[names(e2)] <- e2
#     e1
#   }