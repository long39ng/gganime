# Introduction to gganime

gganime takes a ggplot2 plot written with
[gganimate](https://gganimate.com) syntax and renders it as an animated
SVG. You build the plot the same way you would for gganimate, then call
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md) in
place of [`animate()`](https://gganimate.com/reference/animate.html).
The output is a self-contained HTML widget: one vector drawing of the
scene with a timeline over its elements, played in the browser.

gganime reuses gganimate’s build machinery unchanged and only replaces
the final rendering step, so the [gganimate
documentation](https://gganimate.com) remains the reference for the
grammar itself.

The animations below are live. Drag the scrub bar, or use the play and
pause control, to step through a frame at a time.

## Your first animation

Start from a static plot.

``` r

library(ggplot2)
library(gganime)

p <- ggplot(iris, aes(Petal.Width, Petal.Length)) +
  geom_point()
```

Add a transition to animate it.
[`transition_states()`](https://gganimate.com/reference/transition_states.html)
splits the data by a discrete variable and moves the points between the
resulting states.

``` r

anim <- p +
  transition_states(Species, transition_length = 2, state_length = 1)
```

To see the animation with gganimate you would print the object or call
[`animate()`](https://gganimate.com/reference/animate.html). With
gganime you call
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md):

``` r

anime(anim, nframes = 30)
```

## Transitions

gganime supports three of gganimate’s transitions.

[`transition_states()`](https://gganimate.com/reference/transition_states.html),
above, animates between the levels of a discrete variable.

[`transition_time()`](https://gganimate.com/reference/transition_time.html)
animates along a continuous variable, holding it to the data range so
the spacing between frames matches the spacing between values.

``` r

aq <- ggplot(airquality, aes(Day, Temp)) +
  geom_point() +
  transition_time(Month)

anime(aq, nframes = 30)
```

[`transition_reveal()`](https://gganimate.com/reference/transition_reveal.html)
keeps earlier data on screen and reveals the rest along a dimension,
which suits a line growing over time.

``` r

ec <- ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  transition_reveal(date)

anime(ec, nframes = 40)
```

## Frame labels

A transition exposes a set of per-frame variables. Insert them into the
title, subtitle, or caption with [glue](https://glue.tidyverse.org)
syntax, and the text is swapped frame by frame in step with the
geometry.

``` r

aq_labelled <- ggplot(airquality, aes(Day, Temp)) +
  geom_point() +
  transition_time(Month) +
  labs(title = "Month: {frame_time}")

anime(aq_labelled, nframes = 30)
```

Each transition provides different variables.
[`transition_states()`](https://gganimate.com/reference/transition_states.html)
gives `closest_state`;
[`transition_time()`](https://gganimate.com/reference/transition_time.html)
gives `frame_time`;
[`transition_reveal()`](https://gganimate.com/reference/transition_reveal.html)
gives `frame_along`. All three also provide `frame`, `nframes`, and
`progress`.

## Easing, entering, and exiting

[`ease_aes()`](https://gganimate.com/reference/ease_aes.html) sets how
an aesthetic moves between values across a transition. The default is
linear; naming an aesthetic gives it its own easing.

``` r

anime(anim + ease_aes("cubic-in-out"), nframes = 30)
```

`enter_*()` and `exit_*()` control how data that appears or leaves is
drawn, so its entrance and exit can be animated.

``` r

anim_species <- ggplot(iris, aes(Petal.Width, Petal.Length)) +
  geom_point(aes(colour = Species), size = 2) +
  transition_states(Species, transition_length = 2, state_length = 1)

anime(anim_species + enter_fade() + exit_shrink(), nframes = 30)
```

Each family animates a different attribute: fade the paint opacity, grow
and shrink the size, drift, fly and manual the position, recolour the
paint. All of them work with the four supported geoms.

Two behaviours below come from gganimate itself and appear in its gifs
as well. `enter_recolour()` and `exit_recolour()` set `colour` and
`fill` together, and setting the aesthetic a geom does not paint makes
tweenr coerce a logical column, which warns; pass `NA` for that
aesthetic, as in `enter_recolour(colour = "white", fill = NA)` for the
default point shape.
[`enter_grow()`](https://gganimate.com/reference/enter_exit.html) and
[`exit_shrink()`](https://gganimate.com/reference/enter_exit.html) do
not change the geometry of a
[`geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
or
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
layer, so a ribbon that arrives is drawn at its full shape.

## Several layers at once

Layers are animated independently, so you can stack them the way you
would in a static plot. Each layer keeps its own elements even when two
layers draw the same kind of shape.

``` r

aq_layers <- ggplot(
  airquality[airquality$Month %in% c(5, 6, 7), ],
  aes(Day, Temp)
) +
  geom_area(fill = "#9ecae1") +
  geom_line(linewidth = 1, colour = "#08519c") +
  geom_point(size = 2, colour = "#a50f15") +
  labs(title = "Month {closest_state}") +
  transition_states(Month, transition_length = 2, state_length = 1)

anime(aq_layers, nframes = 30)
```

A layer that does not use the transition variable is drawn once and left
in place, which is how you add a fixed reference against the animated
data.

## Leaving a trail

[`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html)
keeps the marks from earlier frames on screen behind the current one, so
a scatter builds up as it advances.

``` r

aq_shadow <- ggplot(airquality, aes(Day, Temp)) +
  geom_point() +
  transition_time(Month) +
  shadow_mark(colour = "grey70") +
  labs(title = "Month: {frame_time}")

anime(aq_shadow, nframes = 30)
```

## Controlling the render

gganime fixes the frame count and playback speed when you call
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md), the
same way gganimate does at
[`animate()`](https://gganimate.com/reference/animate.html):

- `nframes` sets how many frames the animation is sampled at (default
  `100`).
- `fps` sets the playback speed in frames per second (default `10`), or
  give `duration` in seconds to set the speed from the total length.
- `loop` repeats the animation: `TRUE`, `FALSE`, or a number of
  iterations.
- `controls` shows or hides the play and scrub bar.
- `width` and `height` fix the widget size in pixels; leaving them
  `NULL` lets it fill its container.
- `precision` rounds the animated coordinates, which trims the file
  size.

``` r

anime(anim, nframes = 50, duration = 4, loop = FALSE)
```

## Saving and embedding

[`anime()`](https://long39ng.github.io/gganime/reference/anime.md)
returns an htmlwidget, so it prints in the RStudio Viewer, knits into R
Markdown and Quarto documents, and saves to a standalone HTML file with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html):

``` r

htmlwidgets::saveWidget(anime(anim), "iris.html")
```

Loading gganime also makes a bare gganimate plot print through
[`anime()`](https://long39ng.github.io/gganime/reference/anime.md) at
the console, so printing `anim` gives the widget without an explicit
call. Set `options(gganime.autoprint = FALSE)` to keep gganimate’s own
output.

In Shiny, pair
[`renderGganime()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
in the server with
[`gganimeOutput()`](https://long39ng.github.io/gganime/reference/gganime-shiny.md)
in the UI.

## Supported features

- Transitions:
  [`transition_states()`](https://gganimate.com/reference/transition_states.html),
  [`transition_time()`](https://gganimate.com/reference/transition_time.html),
  [`transition_reveal()`](https://gganimate.com/reference/transition_reveal.html).
- Geoms: points, lines and paths, bars and columns, areas and ribbons.
- Several layers in one plot, including two of the same kind, and a
  static layer beside an animated one.
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
- `enter_*()` and `exit_*()`,
  [`ease_aes()`](https://gganimate.com/reference/ease_aes.html), and
  per-frame labels in the title, subtitle, and caption.
- [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html).

Anything else stops with a message naming an alternative. Current limits
include non-linear coordinate systems
([`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
[`coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html),
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
`shadow_wake()` and `shadow_trail()`, and `view_*()` other than
`view_static()`.
