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
fa_path <- "/Users/morgangray/Library/Fonts/Font Awesome 7 Brands-Regular-400.otf"
# fa_path <- "/Path/to/Font Awesome 7 Brands-Regular-400.otf"

font_add_google("Lexend", family = "Lexend Base", regular.wt = 300, bold.wt = 700)
font_add_google("Lexend", family = "Lexend Book", regular.wt = 500, bold.wt = 700)
font_add(family = "fa-brands", regular = fa_path)
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

base_family <- "Lexend Base"
heading_family <- "Lexend Book"
color_base <- "#535353"
color_ink <- colorspace::adjust_transparency(color_base, alpha = 0.7)
color_heading <- colorspace::adjust_transparency(color_base, alpha = 0.5)
color_text <- colorspace::adjust_transparency(color_base, alpha = 0.4)
color_paper <- "white"
base_size <- 9
font_size_caption <- 0.7 * base_size
font_size_axis <- base_size
font_size_subtitle <- base_size * 1.1
font_size_title <- base_size * 1.7


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


# Plot configuration ------------------------------------------------------

col_palette <- c("#69bfc9", "#9a9894", "#e4b568")
names(col_palette) <- levels(plotdata$locality)

# Define the limits for the group values (year)
group_min <- min(plotdata$year)
group_max <- max(plotdata$year)

# Define the limits and breaks for the metric values
metric_limit <- round(max(plotdata$value)) 
metric_breaks <- seq(0, metric_limit, 3)
metric_lines <- setdiff(metric_breaks, 0)  # Exclude line at y-intercept
 
# Define the size breaks for the circle legend
size_min <- min(plotdata$size) 
size_max <- round(max(plotdata$size), -1)
size_breaks <- c(1, 10, 100, 200, 640)  


# Define text -------------------------------------------------------------

title <- glue("Female turtles were consistently healthier at <span style='color:{col_palette[[\"Konjsko\"]]};'>**Konjsko**</span>")
st <- glue("Average body condition for female and male Hermann's tortoises in Lake Prespa, North Macedonia from Summer records collected at <span style='color:{col_palette[[\"Beach\"]]};'>**Beach**</span>, <span style='color:{col_palette[[\"Konjsko\"]]};'>**Konjsko**</span>, and <span style='color:{col_palette[[\"Plateau\"]]};'>**Plateau**</span> localities (2008-2023; no data for Summer 2013). Larger body condition index values indicate better health. Circles show the number of records at each timepoint. The greatest number of records were for males at <span style='color:{col_palette[[\"Plateau\"]]};'>**Plateau**</span>.")
cap <- glue(
  "**Source:** Sex Ratio Bias Triggers Demographic Suicide in a Dense Tortoise Population 
  (Arsovski et al. 2026) | **Graphic:** <span style='font-family:fa-brands;'>&#xe671;</span> morgangray <span style='font-family:fa-brands;'>&#xf09b;</span> morethangray"
)


# Plot --------------------------------------------------------------------

plot <- 
  ggplot(
  data = plotdata, 
  mapping = aes(x = year_offset, y = value, color = locality)
) +
  ggh4x::facet_wrap2(vars(sex), ncol = 1, trim_blank = FALSE) +
  geom_line(alpha = 0.2, linewidth = 1) +
  geom_point(
    shape = 21,
    stroke = 0.5,
    aes(
      size = size,
      fill = after_scale(alpha(color, 0.1))
    )
  ) + 
  scale_size_continuous(
    range = c(1, 10), 
    name = "# Records", 
    breaks = size_breaks, 
    limits = c(size_min, size_max + 1), 
    guide = guide_legend(reverse = TRUE)
    ) +
  scale_colour_manual(
    values = col_palette, 
    guide = NULL
    ) +
  labs(
    x = NULL, 
    y = NULL, 
    title = title, 
    subtitle = st, 
    caption = cap
    ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    breaks = metric_breaks,
    limits = c(0, metric_limit)
    ) +
  scale_x_continuous(
    limits = c(group_min, group_max + 0.5)
  ) +
  geom_hline(
    yintercept = 0, 
    linewidth = 0.2, 
    alpha = 0.2, 
    color = color_ink
  ) +
  geom_hline(
    yintercept = metric_lines,
    linewidth = 0.2, 
    alpha = 0.3, 
    color = color_ink, 
    linetype = "dotted"
  ) +
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    ink = color_ink,
    paper = color_paper 
  ) +
  theme(
    plot.title = element_textbox_simple(
      colour = color_ink,
      hjust = 0,
      halign = 0,
      margin = margin(t = 0, b = 2),
      family = base_family,
      face = "bold",
      size = font_size_title 
    ),
    plot.subtitle = element_textbox_simple(
      colour = color_ink,
      hjust = 0,
      halign = 0,
      margin = margin(t = 5, b = 15),
      family = base_family,
      face = "plain",
      size = font_size_subtitle,
      lineheight = 1.3,
      width = 1
    ),
    plot.caption = element_textbox_simple(
      colour = color_text,
      hjust = 0,
      halign = 0,
      margin = margin(t = 10, b = 0, l = -10),
      family = base_family,
      size = font_size_caption,
      width = 1.2
    ),
    plot.title.position = "plot",
    
    # Axes
    axis.title = element_blank(), 
    axis.text = element_text(
      size = font_size_axis, 
      hjust = 0.5, 
      color = color_text
      ), 
    axis.text.y = element_text(margin = margin(r = 2), hjust = 1), 
    
    # Facet panels
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.spacing.y = unit(9, "mm"),
    strip.background = element_blank(),
    strip.text.x.top = element_text(
      family = heading_family,
      size = font_size_subtitle, 
      color = color_heading,
      hjust = 0, 
      margin = margin(b = 3)
      ), 
    
    # Legend
    legend.position = "right", 
    legend.justification.right = "top",
    legend.title = element_text(
      family = base_family,
      size = font_size_axis,
      color = color_text,
      margin = margin(t = 1.5, b = 5, r = 1.5, l = 1.5)
    ),
    legend.text = element_text(
      family = base_family,
      size = 0.9 * font_size_axis,  
      color = color_text,
      vjust = 0.5,
      margin = margin(t = 2, r = 4, b = 2, l = 4)
    ), 
    legend.margin = margin(t = 5, b = 5, r = 5, l = 15), 
    
    # Plot margins
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10)
  )  +
  canvas(
    width = 7, height = 5,
    units = "in", bg = color_paper,
    dpi = 300
  )  
 


# Save --------------------------------------------------------------------

save_ggplot(
  plot = plot,
  file = file.path("2026", "2026-03-03", paste0("20260303", ".png"))
)
