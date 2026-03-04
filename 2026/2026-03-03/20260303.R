# Load packages -----------------------------------------------------------

library(tidyverse)
library(colorspace)
library(glue)
library(ggh4x) # for facet_wrap2
library(ggview)
library(ggtext)
library(showtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-03-03")
tortoise_body_condition_cleaned <- tuesdata$tortoise_body_condition_cleaned


# Load fonts --------------------------------------------------------------

# Define local path to Font Awesome files
fa_path <- "assets/fonts/Font Awesome 7 Brands-Regular-400.otf"
font_add(family = "fa-brands", regular = fa_path)
font_add_google("Lexend", family = "Lexend Base", regular.wt = 300, bold.wt = 700)
font_add_google("Lexend", family = "Lexend Book", regular.wt = 500, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)


# Define colors and fonts -------------------------------------------------

family_base <- "Lexend Base"
family_heading <- "Lexend Book"
color_base <- "#535353"
color_ink <- colorspace::adjust_transparency(color_base, alpha = 0.7)
color_heading <- colorspace::adjust_transparency(color_base, alpha = 0.5)
color_text <- colorspace::adjust_transparency(color_base, alpha = 0.4)
color_paper <- "white"
size_base <- 9
size_font_axis <- size_base
size_font_title <- 1.7 * size_base
size_font_subtitle <- 1.2 * size_base
size_font_caption <- 0.75 * size_base
size_font_legend <- 0.9 * size_base


# Define ggplot2 layout  --------------------------------------------------

width_png <- 7
height_png <- 6
units_png <- "in"
dpi_png <- 300
margin_title <- margin(t = 0, b = 5)
margin_subtitle <- margin(t = 5, b = 20)
margin_caption <- margin(t = 15, b = 0, l = -15)
margin_axis_text_y <- margin(r = 2)
margin_legend <- margin(t = 5, b = 5, r = 5, l = 15)
margin_legend_title <- margin(t = 1.5, b = 5, r = 1.5, l = 1.5)
margin_legend_text <- margin(t = 2, r = 4, b = 2, l = 4)
margin_plot <- margin(t = 15, b = 10, l = 15, r = 15)
margin_strip_text <- margin(b = 0)
panel_spacing_y <- unit(9, "mm")
width_subtitle <- 1 
width_caption <- 1.2
lineheight_subtitle <- 1.3
legend_position <- "right"
legend_justification_r <- "top"

stroke_point <- 0.4
alpha_point_fill <- 0.1
linewidth_breaks <- 0.2
alpha_breaks <- 0.3


# Data wrangling ----------------------------------------------------------
  
plotdata <-
  tortoise_body_condition_cleaned |> 
  filter(season == "Summer") |>
  group_by(year, locality, sex) |>
  reframe(
    value = mean(body_condition_index, na.rm = TRUE), 
    value_min = min(body_condition_index, na.rm = TRUE), 
    value_max = max(body_condition_index, na.rm = TRUE), 
    size = n()
  ) |>
  mutate(
    sex = recode_values(sex, "m" ~ "male", "f" ~ "female"), 
    sex = glue("{str_to_sentence(sex)} body condition"), 
    year_offset = recode_values(
      locality, 
      "Konjsko" ~ year, 
      "Beach" ~ year + 0.1, 
      "Plateau" ~ year + 0.2
    ), 
    locality = factor(
      locality, 
      levels = c("Plateau", "Beach", "Konjsko")
      )
  ) |>
  select(
    starts_with("year"), 
    locality, 
    sex, 
    starts_with("value"), 
    size
  ) |>
  # Sort so the larger Plateau circles are underneath
  arrange(locality)

values_colors <- c("#69bfc9", "#9a9894", "#e4b568")
names(values_colors) <- levels(plotdata$locality)

# Define the limits for the group values (year)
min_group <- min(plotdata$year)
max_group <- max(plotdata$year)
expand_group <- c(0.01, 0.4)

# Define the limits and breaks for the metric values
min_metric <- round(min(plotdata$value))-1
max_metric <- round(max(plotdata$value))  
breaks_metric <- seq(min_metric, max_metric, 2)

# Circles 
range_size <- c(0.9, 8)
name_size <- "# Records"
min_size <- min(plotdata$size) 
max_size <- round(max(plotdata$size), -1)
limits_size <- c(min_size, max_size + 1)
breaks_size <- c(1, 10, 100, 200, 640)  # Legend breaks 


# Define text -------------------------------------------------------------

title_text <- glue("Female tortoises were healthier <span style='color:{values_colors[[\"Konjsko\"]]};'>**in places with fewer males**</span>")

subtitle_text <- glue("Average body condition of female and male Hermann's tortoises for three locations at Lake Prespa, North Macedonia: <span style='color:{values_colors[[\"Beach\"]]};'>**Beach**</span>, <span style='color:{values_colors[[\"Konjsko\"]]};'>**Konjsko**</span>, and <span style='color:{values_colors[[\"Plateau\"]]};'>**Plateau**</span> (Summer records only; no 2013 data). A higher score indicates a healthier animal. Circle size shows how many individuals were measured per timepoint. Notably, <span style='color:{values_colors[[\"Plateau\"]]};'>**Plateau**</span> males consistently had the most records.")

caption_text <- glue(
  "**Source:** Sex Ratio Bias Triggers Demographic Suicide in a Dense Tortoise Population 
  (Arsovski et al. 2026) | **Graphic:** <span style='font-family:fa-brands;'>&#xe671;</span> morgangray <span style='font-family:fa-brands;'>&#xf09b;</span> morethangray"
)


# Plot --------------------------------------------------------------------

plot <- 
  ggplot(
  data = plotdata, 
  mapping = aes(x = year_offset, y = value, color = locality)
) +
  ggh4x::facet_wrap2(
    vars(sex), 
    ncol = 1, 
    trim_blank = FALSE
    ) +
  geom_line(alpha = 0.2, linewidth = 1) +
  geom_point(
    shape = 21,
    stroke = stroke_point,
    aes(
      size = size, 
      fill = after_scale(alpha(color, alpha_point_fill))
      )
  ) + 
  scale_size_continuous(
    range = range_size, 
    name = name_size, 
    breaks = breaks_size, 
    limits = limits_size, 
    guide = guide_legend(reverse = TRUE)
    ) +
  scale_colour_manual(values = values_colors, guide = NULL) +
  labs(
    x = NULL, 
    y = NULL, 
    title = title_text, 
    subtitle = subtitle_text, 
    caption = caption_text
    ) +
  scale_y_continuous(
    breaks = breaks_metric,
    limits = c(min_metric, max_metric)
    ) +
  scale_x_continuous(expand = expand_group) +
  geom_hline(
    yintercept = breaks_metric,
    linewidth = linewidth_breaks, 
    alpha = alpha_breaks, 
    color = color_ink, 
    linetype = "dotted"
  ) +
  theme_minimal(
    base_size = size_base,
    base_family = family_base,
    ink = color_ink,
    paper = color_paper 
  ) +
  theme(
    plot.title = element_textbox_simple(
      colour = color_ink,
      hjust = 0,
      halign = 0,
      margin = margin_title,
      family = family_base,
      face = "bold",
      size = size_font_title 
    ), 
    plot.subtitle = element_textbox_simple(
      colour = color_ink,
      hjust = 0,
      halign = 0,
      margin = margin_subtitle,
      family = family_base,
      size = size_font_subtitle,
      lineheight = lineheight_subtitle,
      width = width_subtitle
    ),
    plot.caption = element_textbox_simple(
      colour = color_text,
      hjust = 0,
      halign = 0,
      margin = margin_caption,
      family = family_base,
      size = size_font_caption,
      width = width_caption
    ),
    plot.title.position = "plot",
    plot.margin = margin_plot,
    
    # Axes
    axis.title = element_blank(), 
    axis.text = element_text(
      size = size_font_axis, 
      hjust = 0.5, 
      color = color_text
      ), 
    axis.text.y = element_text(margin = margin_axis_text_y, hjust = 1), 
    
    # Facet panels
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.spacing.y = panel_spacing_y,
    strip.background = element_blank(),
    strip.text.x.top = element_text(
      family = family_heading,
      size = size_font_subtitle, 
      color = color_heading,
      hjust = 0, 
      margin = margin_strip_text
      ), 
    
    # Legend
    legend.position = legend_position, 
    legend.justification.right = legend_justification_r, 
    legend.margin = margin_legend,
    legend.title = element_text(
      family = family_base,
      size = size_font_axis,
      color = color_text,
      margin = margin_legend_title
    ),
    legend.text = element_text(
      family = family_base,
      size = size_font_legend,  
      color = color_text,
      vjust = 0.5,
      margin = margin_legend_text
    )
    
  )  +
  canvas(
    width = width_png, height = height_png,
    units = units_png, bg = color_paper,
    dpi = dpi_png
  )  
  

# Save --------------------------------------------------------------------

save_ggplot(
  plot = plot,
  file = file.path("2026", "2026-03-03", paste0("20260303", ".png"))
)
