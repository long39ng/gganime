# panel_affines reports a panel that is missing from the export

    Code
      panel_affines(facet_grid_export(), facet_grid_doc(), unit_ranges(c("1", "9")),
      res = 96)
    Condition
      Error in `panel_affines()`:
      ! The exported SVG is missing a panel.
      x No panel group for PANEL "9".

# panel_affines reports a panel with no exported rectangle

    Code
      panel_affines(list(coords = list()), facet_grid_doc(), unit_ranges("1"), res = 96)
    Condition
      Error in `panel_viewport_rect()`:
      ! Cannot locate the exported rectangle of a panel.
      x PANEL "1" has no panel viewport ancestor in the export.

# a per-panel count mismatch names the panel

    Code
      point_nodes(facet_grid_doc(), 1L, panels = c("1", "1", "1"))
    Condition
      Error in `ordered_data_nodes()`:
      ! Point element count does not match the union.
      x Panel 1: found 2 SVG points but expected 3.

