# Read one element's animated values back out of a rendered widget's timeline.

# Every value one prop is animated to on one element, in frame order.
track <- function(segment, prop) {
  vapply(segment$props[[prop]], function(k) as.character(k$to), character(1))
}

numeric_track <- function(segment, prop) {
  as.numeric(track(segment, prop))
}

# The props animated across a set of segments, as one sorted set.
animated_props <- function(segments) {
  sort(unique(unlist(lapply(segments, function(s) names(s$props)))))
}
