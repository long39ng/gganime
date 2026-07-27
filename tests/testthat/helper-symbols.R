# pch symbol fixtures, parsed the way export_scene_svg() parses a real export.
# Every gridSVG pch symbol sits in a "-5 -5 10 10" viewBox, and the markup below
# is copied from an export of shapes 0:25.

pch_markup <- list(
  pch0 = '<rect x="-3.75" y="-3.75" width="7.5" height="7.5"/>',
  pch2 = '<polyline points="0,5.83 5.05,-2.92 -5.05,-2.92 0,5.83"/>',
  pch8 = paste0(
    '<polyline points="-5.3,0 5.3,0"/><polyline points="0,-5.3 0,5.3"/>',
    '<polyline points="-3.75,-3.75 3.75,3.75"/>',
    '<polyline points="-3.75,3.75 3.75,-3.75"/>'
  ),
  pch19 = '<circle cx="0" cy="0" r="3.75"/>',
  pch21 = '<circle cx="0" cy="0" r="3.75"/>',
  pch22 = '<rect x="-3.32" y="-3.32" width="6.64" height="6.64"/>',
  pch23 = '<polygon points="-4.7,0 0,4.7 4.7,0 0,-4.7 -4.7,0"/>',
  pch24 = '<polyline points="0,5.83 5.05,-2.92 -5.05,-2.92 0,5.83"/>'
)

# A symbol table for the named pch, keyed the way gridSVG ids them.
pch_symbols <- function(...) {
  which <- c(...)
  ids <- sprintf("gridSVG.pch%d", which)
  symbols <- sprintf(
    '<symbol id="%s" viewBox="-5 -5 10 10">%s</symbol>',
    ids,
    unlist(pch_markup[sprintf("pch%d", which)])
  )
  symbol_table(xml2::read_xml(paste0(
    "<svg>",
    paste0(symbols, collapse = ""),
    "</svg>"
  )))
}
