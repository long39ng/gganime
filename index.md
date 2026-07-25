# gganime

`gganime` renders a [ggplot2](https://ggplot2.tidyverse.org) plot
written with [gganimate](https://gganimate.com) syntax as an animated
SVG. The output is a self-contained HTML widget powered by
[Anime.js](https://animejs.com) (through the
[animejs](https://github.com/long39ng/animejs) package), instead of a
GIF or a video.

Because the animation is a single SVG with a timeline over its
attributes:

- it stays sharp at any size, since the marks are vectors rather than a
  raster of fixed pixels;
- the file holds one copy of the scene plus per-element keyframes, not
  one image per frame;
- it plays in the browser with a scrub bar, so a reader can pause and
  step through frames.

## Installation

Install the development version from GitHub with:

``` r

# install.packages("pak")
pak::pak("long39ng/gganime")
```

`gganime` needs [animejs](https://github.com/long39ng/animejs) (\>=
1.1.0).

## Usage

Write the plot exactly as you would with gganimate, then call
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
instead of [`animate()`](https://gganimate.com/reference/animate.html).
Here
[`transition_time()`](https://gganimate.com/reference/transition_time.html)
animates over a continuous variable, sizing each country’s bubble by
population and filling in the `{frame_time}` label between years:

``` r

library(ggplot2)
library(gapminder)
library(gganime)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = "Year: {frame_time}", x = "GDP per capita", y = "life expectancy") +
  transition_time(year) +
  ease_aes("linear")

anime(p)
```

![Bubble chart of life expectancy against GDP per capita, one bubble per
country sized by population, faceted by continent, animating across
years.](reference/figures/README-gapminder.gif)

A [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html)
leaves earlier frames behind the current one:

``` r

p <- ggplot(airquality, aes(Day, Temp)) +
  geom_point() +
  transition_time(Month) +
  shadow_mark(colour = "grey70") +
  labs(title = "Month: {frame_time}")

anime(p)
```

![Scatterplot of daily temperature over a month, with earlier days left
behind in grey as the animation
advances.](reference/figures/README-shadow.gif)

[`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
returns an htmlwidget, so it prints in the RStudio Viewer, embeds in R
Markdown and Quarto, and saves with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).
Loading `gganime` also makes a bare gganimate plot print through
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md) at
the console; set `options(gganime.autoprint = FALSE)` to keep
gganimate’s own output.

In Shiny, use
[`gganimeOutput()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
and
[`renderGganime()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md).

## Supported features

- Transitions:
  [`transition_states()`](https://gganimate.com/reference/transition_states.html),
  [`transition_time()`](https://gganimate.com/reference/transition_time.html),
  [`transition_reveal()`](https://gganimate.com/reference/transition_reveal.html).
- Geoms: points, lines and paths, bars and columns, areas and ribbons.
- Facets:
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  with fixed or free scales.
- Coords:
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html),
  [`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
  /
  [`coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html),
  and
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).
- `enter_*()` / `exit_*()`,
  [`ease_aes()`](https://gganimate.com/reference/ease_aes.html), and
  per-frame labels in the title, subtitle, and caption.
- [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html).

Anything else stops with a message naming the alternative. Current
limits include non-linear coordinate systems
([`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
[`coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html),
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
`shadow_wake()` / `shadow_trail()`, and `view_*()` other than
`view_static()`.
