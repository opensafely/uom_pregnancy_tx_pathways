plot_code_usage <- function(data, date_variable, code_usage_count, label = NULL, colour = NULL) {
  ggplot2::ggplot(data,
    ggplot2::aes(
      x = {{ date_variable }},
      y = {{ code_usage_count }},
      colour = {{ colour }},
      label = {{ label }}
    )
  ) +
    ggplot2::geom_point(
      size = 1,
      alpha = 1
    ) +
    ggplot2::geom_line(
      size = .5,
      alpha = .5
    ) +
    ggplot2::scale_colour_viridis_d(begin = 0, end = .85) +
    ggplot2::scale_x_date(
      breaks = seq(
        as.Date("2011-08-01"),
        as.Date("2023-08-31"),
        by = "year"
      ),
      labels = scales::label_date_short()
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = NULL
    ) +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 10)
    )
}
