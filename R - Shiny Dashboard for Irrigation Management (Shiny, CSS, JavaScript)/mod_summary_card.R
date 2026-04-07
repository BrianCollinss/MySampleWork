

summary_card_ui <- function(
  id,
  value,
  subtitle,
  icon,
  color,
  animate = TRUE
) {
  tags$div(
    singleton(tags$head(
      tags$script(src = "summary_card/bounty.js"),
      tags$script(src = "summary_card/summary_card.js"),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "summary_card/summary_card.css"
      )
    )),
    class = "myapp-summary-card panel",
    style = sprintf("color:%s;", color),
    tags$div(
      class = "myapp-summary-card__content panel-body",
      tags$div(
        class = "myapp-summary-card__text",
        tags$b(
          class = "myapp-summary-card__value",
          class = if (isTRUE(animate)) "myapp-summary-card__value--animated",
          style = if (isTRUE(animate)) sprintf("fill:%s;", color),
          id = paste("summary_card", id, sep = "_"),
          `data-value` = HTML(value),
          title = HTML(value),
          HTML(value)
        ),
        tags$div(
          class = "myapp-summary-card__subtitle",
          subtitle,
          title = subtitle
        ),
      ),
      tags$div(
        class = "myapp-summary-card__icon",
        icon
      )
    )
  )
}
