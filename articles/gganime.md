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
library(gganimate)
#> No renderer backend detected. gganimate will default to writing frames to separate files
#> Consider installing:
#> - the `gifski` package for gif output
#> - the `av` package for video output
#> and restarting the R session
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
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
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
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
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
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
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
- `enter_*()` and `exit_*()`,
  [`ease_aes()`](https://gganimate.com/reference/ease_aes.html), and
  per-frame labels in the title, subtitle, and caption.
- [`shadow_mark()`](https://gganimate.com/reference/shadow_mark.html).

Anything else stops with a message naming an alternative. Current limits
include faceted plots (only one panel renders), non-Cartesian coordinate
systems,
[`shadow_wake()`](https://gganimate.com/reference/shadow_wake.html) and
[`shadow_trail()`](https://gganimate.com/reference/shadow_trail.html),
and `view_*()` other than
[`view_static()`](https://gganimate.com/reference/view_static.html).
