# Shiny App ----------------------------------------------------------
# Multiple TIFFs as toggleable layers
# with polygon drawing + send visible layers to second map
# and show ON/OFF checkboxes for sent layers.
# Drawn features are cleared after each SAM run.
# Now includes: Cut fire_SAM output with drawn polygons.


options(shiny.maxRequestSize = 500*1024^2)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sortable)
library(leafem)
library(terra)
library(sf)
library(dplyr)

#reticulate::use_condaenv(condaenv = "sam2_env")
wfprogression:::fire_load_sam_once()

# Helper Functions----------------------------------------------------------
#  Map Creation and Raster Handling


# Create base leaflet map with default tiles and center/zoom
create_leaflet <- function() {
  leaflet() %>%
    addTiles() %>%
    setView(lng = 149.95, lat = -32.1, zoom = 9)
}

# Add a 3-band raster to an existing leaflet proxy map
# proxy: leafletProxy object
# r: terra raster object
# layer_id: string to identify layer group
# zoom_first: boolean; zoom to raster bbox if TRUE
add_raster_to_map <- function(proxy, r, layer_id, zoom_first = FALSE) {
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(r)) |> sf::st_transform(4326)
  bbox <- sf::st_bbox(bbox_sf)
  proxy %>%
    addRasterRGB(r, r = 1, g = 2, b = 3, group = layer_id) %>%
    {
      if (zoom_first) fitBounds(., bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) else .
    }
}

# Create leaflet map with merged raster and optionally polygons overlayed
# Adds draw toolbar for polygon/rectangle/circle marker drawing
create_leaflet_with_layers <- function(r_merge, polygons = NULL) {
  map <- leaflet() %>%
    addTiles() %>%
    addRasterRGB(r_merge, r = 1, g = 2, b = 3, group = "merged") %>%
    addDrawToolbar(
      targetGroup = "draw",
      polylineOptions = FALSE,
      circleOptions = FALSE,
      circleMarkerOptions = drawCircleMarkerOptions(
        repeatMode = TRUE,
        fillColor = "black",
        color = "yellow",
        opacity = 1,
        weight = 1,
        fillOpacity = 1
      ),
      polygonOptions = drawPolygonOptions(
        shapeOptions = drawShapeOptions(color = "red"),
        repeatMode = TRUE
      ),
      rectangleOptions = drawRectangleOptions(
        shapeOptions = drawShapeOptions(color = "red"),
        repeatMode = TRUE
      ),
      markerOptions = FALSE,
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
    )
  
  # Add polygons if provided
  if (!is.null(polygons) && nrow(polygons) > 0) {
    map <- map %>%
      addPolygons(
        data = polygons,
        color = "yellow",
        weight = 5,
        fillColor = "lightgrey",
        fillOpacity = 0.6,
        group = "fire_SAM_output",
        popup = ~paste0("ID: ", id),
        layerId = ~as.character(id),
        highlightOptions = highlightOptions(color = "yellow", weight = 3, bringToFront = TRUE)
      )
  }
  
  # Determine bounding box (polygons if present, else raster)
  bbox <- if (!is.null(polygons) && nrow(polygons) > 0) {
    sf::st_bbox(polygons)
  } else {
    sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(r_merge)), 4326))
  }
  
  map %>%
    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
    addLayersControl(
      overlayGroups = c("merged", "fire_SAM_output"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Clean and combine sf polygons by unioning and making valid multipolygons
# new_sf: new polygons to add
# old_sf: existing polygons to combine with (optional)
clean_combine_sf <- function(new_sf, old_sf = NULL) {
  if (is.null(old_sf)) {
    combined <- new_sf
  } else {
    st_geometry(old_sf) <- "geometry"
    st_geometry(new_sf) <- "geometry"
    old_sf <- old_sf %>% select(geometry)
    new_sf <- new_sf %>% select(geometry)
    combined <- rbind(
      st_sf(geometry = st_union(old_sf)),
      st_sf(geometry = st_union(new_sf))
    )%>% st_as_sf()
  }
  
  combined <- combined[!st_is_empty(combined), ]
  combined <- combined %>%
    st_union() %>%
    st_make_valid() %>%
    st_sfc(crs = 4326) %>%
    st_sf(geometry = .) %>%   # ✅ ensures geometry column is named
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON")
  combined <- combined[!st_is_empty(combined), ]
  combined$id <- seq_len(nrow(combined))
  combined
}

# UI Definition ----------------------------------------------------------
ui <- fluidPage(
  titlePanel("Mapping from line scans"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("tif_files", "Upload 3-band TIFF(s)", accept = ".tif", multiple = TRUE),
      verbatimTextOutput("selected_dir"),
      actionButton("send_to_second", "Merge Selected TIFS"),
      
      h4("Preview and select tif/s:"),
      leafletOutput("map", height = 400),
      
      uiOutput("tif_order_ui")
    ),
    
    mainPanel(
      h4("Polygon mapping:"),
      
      # Map with floating buttons overlaid
      div(
        leafletOutput("second_map", height = 600),
        
        absolutePanel(
          top =320, right = 20, width = "auto", draggable = FALSE,
          style = paste(
            "z-index: 500;",
            "background-color: rgba(255,255,255,0.9);",
            "padding: 10px;",
            "border-radius: 8px;",
            "display: flex;",
            "flex-direction: column;",
            "gap: 6px;"
          ),
          
          actionButton("run_sam", "Segment"),
          actionButton("keep_selected", "Keep Selected"),
          actionButton("cut_polygons", "Cut"),
          actionButton("union_polygons", "Union"),
          actionButton("fill_holes", "Fill Holes"), 
          actionButton("undo_last", "Undo")
        )
      ),
      div(style = "display: flex; align-items: center; gap: 10px; margin-top: 10px;",
          textInput("out_dir", "Output gpkg folder", "C:/polygons", width = "300px"),
          actionButton("save_to_gpkg", "Save to gpkg")
      )
    )
  )
)



# ui <- fluidPage(
#   titlePanel("Mapping from line scans"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       # Upload one or multiple 3-band TIFF files
#       fileInput("tif_files", "Upload 3-band TIFF(s)", accept = ".tif", multiple = TRUE),
#       
#   
#       
#       # Show chosen directory for feedback
#       verbatimTextOutput("selected_dir"),
#       
#       #tags$hr(),
#       
#       # Button to merge selected TIF layers and send to second map
#       actionButton("send_to_second", "Merge Selected TIFS"),
#       
#       #tags$hr(),
#       
#       
#       h4("Preview and select tif/s:"),
#       
#       # Main leaflet map output
#       leafletOutput("map", height = 400),
#       # Dynamic UI to reorder TIFs
#       uiOutput("tif_order_ui")
#     ),
#     
#     mainPanel(
#       h4("Polygon mapping:"),
#       # Second map with merged raster + polygons + drawing tools
#       leafletOutput("second_map", height = 600),
#       textInput("out_dir", "Output gpkg folder", "C:/polygons"),
#       # Buttons for SAM segmentation and polygon manipulation
#       actionButton("run_sam", "Segment"),
#       actionButton("keep_selected", "Keep Selected Polygons"),
#       actionButton("cut_polygons", "Cut with drawn polygons"),
#       actionButton("union_polygons", "Union with drawn polygons"),
#       actionButton("undo_last", "Undo"),
# 
# 
#       
#  
#     )
#   )
# )

# Server Logic ----------------------------------------------------------

server <- function(input, output, session) {
  
  
  # Reactive Values Storage----
  vals <- reactiveValues(
    layers = list(),           # Stores names of layers uploaded
    rasters = list(),          # Stores terra raster objects for each layer
    selected_layers = NULL,    # Names of layers selected for merging
    r_merge = NULL,            # Merged raster for second map
    all_p = NULL,
    
    # Combined polygons from SAM output
  )
  vals$selected_ids <- c()     # Stores IDs of selected polygons on second map
  
  vals$history <- list()
  push_history <- function() {
    if (!is.null(vals$all_p)) {
      vals$history[[length(vals$history) + 1]] <- vals$all_p
    }
  }
  
  # Render Main Map -----------------------------------
  # (empty base map on load)
  
  output$map <- renderLeaflet({ create_leaflet() })
  
  # Render Second Map-----------------------------------
  #  (empty base map on load)
  
  output$second_map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 149.95, lat = -32.1, zoom = 9)
  })
  
  # Draw second map -----------------------------------
  # Reactive expression to read drawn features from second map
  # Converts GeoJSON features drawn by user into sf object
  
  all_drawn_sf <- reactive({
    req(input$second_map_draw_all_features)
    sf_obj <- tryCatch(
      geojsonsf::geojson_sf(jsonlite::toJSON(input$second_map_draw_all_features, auto_unbox = TRUE)),
      error = function(e) NULL
    )
    if (is.null(sf_obj) || nrow(sf_obj) == 0) {
      sf::st_sf(id = integer(0), geometry = sf::st_sfc(crs = 4326))
    } else {
      sf_obj
    }
  })
  
  
  
  # Load tiffs -----------------------------------
  # Load and display uploaded TIFFs on main map
  # Clears previous images and layers, adds each TIFF as a toggleable layer
  
  observeEvent(input$tif_files, {
    req(input$tif_files)
    
    vals$layers <- list()
    vals$rasters <- list()
    
    proxy <- leafletProxy("map") |> clearImages()
    
    input_files_sorted <- input$tif_files  # Initial order preserved
    
    for (i in seq_len(nrow(input_files_sorted))) {
      file <- input_files_sorted[i, ]
      tif_name <- tools::file_path_sans_ext(file$name)
      r <- terra::rast(file$datapath) |> terra::aggregate(5)
      if (terra::nlyr(r) < 3) {
        showNotification(paste("TIFF", file$name, "must have at least 3 bands!"), type = "error")
        next
      }
      layer_id <- paste0(tif_name)
      add_raster_to_map(proxy, r, layer_id, zoom_first = (i == 1))
      vals$layers[[layer_id]] <- tif_name
      vals$rasters[[layer_id]] <- r
    }
    
    proxy |> addLayersControl(
      overlayGroups = rev(names(vals$layers)),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  # TIF order ui -----------------------------------
  # UI output for draggable TIF order list (sortable)
  # Lets user reorder TIFF layers for merging order
  
  output$tif_order_ui <- renderUI({
    req(vals$layers)
    rank_list(
      text = "🧭 Drag to set TIF order:",
      labels = names(vals$layers),
      input_id = "tif_order",
      options = sortable_options(multiDrag = TRUE)
    )
  })
  
  # TIF order observe -----------------------------------
  # Observe changes in TIF order from draggable UI
  # Redraws the main map layers in new order and updates layers control
  
  observeEvent(input$tif_order, {
    req(vals$rasters, input$tif_order)
    
    proxy <- leafletProxy("map") %>%
      clearImages() %>%
      clearControls()
    in_tifs <- input$tif_order
    
    for (i in seq_along(in_tifs)) {
      tif_id <- in_tifs[i]
      r <- vals$rasters[[tif_id]]
      if (!is.null(r)) {
        add_raster_to_map(proxy, r, tif_id, zoom_first = (i == 1))
      }
    }
    
    # labels <- as.character(in_tifs)
    # labels[[1]] <- paste0("(top): ", labels[[1]])
    # labels[[length(labels)]] <- paste0("(bottom): ", labels[[length(labels)]])
    # 
    
    print(in_tifs)
    proxy %>% addLayersControl(
      overlayGroups = rev(in_tifs),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  # TIFS merge -----------------------------------
  # Merge selected TIFF layers and display on second map
  # Respects order from draggable UI, puts main TIFF first
  observeEvent(input$send_to_second, {
    
    vals$all_p <- NULL
    vals$history <- list()  # Also clear undo history if you want
    
    active_layers <- input$map_groups
    
    ordered_tifs <- input$tif_order
    main_tif <- ordered_tifs[1]
    
    vals$selected_layers <- active_layers
    
    if (!is.null(active_layers) && length(active_layers) > 0) {
      # Reorder so main_tif is first
      ordered_layers <- c(main_tif, setdiff(active_layers, main_tif))
      
      r_list <- lapply(ordered_layers, function(id) vals$rasters[[id]])
      r_list <- r_list[!sapply(r_list, is.null)]
      
      if (length(r_list) > 0) {
        r_merge <- terra::merge(terra::sprc(r_list)) |> terra::stretch()
        vals$r_merge <- r_merge
        vals$ordered_layers <- ordered_layers  # Store for saving metadata
        output$second_map <- renderLeaflet({ create_leaflet_with_layers(r_merge) })
      }
    }
  })
  
  # SAM -----------------------------------
  # Run SAM segmentation on merged raster with drawn polygons/points as input
  # Updates polygons in vals$all_p and redraws second map with SAM output polygons
  # Clears drawn features on main map after completion
  
  observeEvent(input$run_sam, {
    req(vals$r_merge)
    sf_obj2 <- all_drawn_sf()
    if (nrow(sf_obj2) == 0) {
      showNotification("No drawn features detected!", type = "warning")
      return()
    }
    
    sf_obj2 <- sf_obj2[!sf::st_is_empty(sf_obj2), ] |> sf::st_make_valid()
    geom_types <- tolower(sf::st_geometry_type(sf_obj2))
    polygons_sf <- sf_obj2[geom_types %in% c("polygon", "multipolygon"), ]
    points_sf <- sf_obj2[geom_types %in% c("point"), ]
    
    if (nrow(polygons_sf) == 0) polygons_sf <- NULL
    if (nrow(points_sf) == 0) points_sf <- NULL
    
    vals$polygons_sf <- polygons_sf
    vals$points_sf <- points_sf
    
    # Write merged raster temporarily to disk for SAM processing
    r_merge_path <- file.path(tempdir(), "r_merge.tif")
    terra::writeRaster(vals$r_merge, r_merge_path, overwrite = TRUE, datatype = "INT1U")
    
    showNotification("Running segmentation", type = "message")
    
    # Run SAM (fire_SAM) - external function, returns polygons sf object
    p <- wfprogression::fire_SAM(
      r_merge_path, polygons_sf, points_sf, working_dir = "D:\\temp\\seg\\"
    )
    
    # Combine new SAM polygons with previously stored polygons
    combined_p <- clean_combine_sf(p, vals$all_p)
    
    push_history()
    
    vals$all_p <- combined_p
    
    # Update second map with raster + polygons
    output$second_map <- renderLeaflet({
      create_leaflet_with_layers(vals$r_merge, combined_p)
    })
    
    # Clear drawn features on second map
    leafletProxy("second_map") |> clearGroup("draw")
    
    showNotification("fire_SAM finished! Output added.", type = "message")
  })
  
  # Cut -----------------------------------
  # Cut existing fire_SAM polygons with drawn polygons on second map
  # Uses st_difference, updates polygons and redraws second map
  
  observeEvent(input$cut_polygons, {
    req(vals$all_p)
    drawn_sf <- all_drawn_sf()
    drawn_sf <- drawn_sf[!sf::st_is_empty(drawn_sf), ]
    if (nrow(drawn_sf) == 0) {
      showNotification("No drawn polygons to cut with!", type = "warning")
      return()
    }
    drawn_polygons <- drawn_sf[tolower(sf::st_geometry_type(drawn_sf)) %in% c("polygon", "multipolygon"), ]
    if (nrow(drawn_polygons) == 0) {
      showNotification("No valid polygon geometries found.", type = "warning")
      return()
    }
    diff_result <- sf::st_difference(vals$all_p, sf::st_union(drawn_polygons)) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_cast("POLYGON") %>%
      sf::st_make_valid()
    
    diff_result$id <- seq_len(nrow(diff_result))
    
    push_history()
    
    vals$all_p <- diff_result
    output$second_map <- renderLeaflet({
      create_leaflet_with_layers(vals$r_merge, diff_result)
    })
    showNotification("fire_SAM output cut by drawn polygons!", type = "message")
  })
  
  # Union -----------------------------------
  # Union existing fire_SAM polygons with drawn polygons on second map
  # Combines polygons into a single unified sf object and updates second map
  
  observeEvent(input$union_polygons, {
    req(vals$all_p)
    
    drawn_sf <- all_drawn_sf()
    drawn_sf <- drawn_sf[!sf::st_is_empty(drawn_sf), ]
    
    if (nrow(drawn_sf) == 0) {
      showNotification("No drawn polygons to union with!", type = "warning")
      return()
    }
    
    drawn_polygons <- drawn_sf[tolower(sf::st_geometry_type(drawn_sf)) %in% c("polygon", "multipolygon"), ]
    
    if (nrow(drawn_polygons) == 0) {
      showNotification("No valid polygon geometries found.", type = "warning")
      return()
    }
    
    # Make sure both sides are valid
    drawn_polygons <- sf::st_make_valid(drawn_polygons)
    all_p_valid <- sf::st_make_valid(vals$all_p)
    
    # Combine both as one sf object to safely union
    combined <- rbind(
      all_p_valid |> dplyr::select(geometry),
      drawn_polygons |> dplyr::select(geometry)
    )
    
    combined <- combined[!sf::st_is_empty(combined), ]
    
    if (nrow(combined) == 0) {
      showNotification("Combined result is empty!", type = "warning")
      return()
    }
    
    # Do the union of all geometries
    union_geom <- sf::st_union(combined)
    
    # Wrap union result back to sf object
    union_sfc <- sf::st_sfc(union_geom, crs = 4326)
    union_sfc <- union_sfc[!sf::st_is_empty(union_sfc)]
    
    union_result <- sf::st_sf(geometry = union_sfc)
    union_result$id <- seq_len(nrow(union_result))
    
    push_history()
    
    vals$all_p <- union_result
    
    output$second_map <- renderLeaflet({
      create_leaflet_with_layers(vals$r_merge, union_result)
    })
    
    showNotification("fire_SAM output unioned with drawn polygons!", type = "message")
  })
  
  # Click polys -----------------------------------
  # Polygon selection on second map by clicking shapes
  # Toggles selection state of polygons by id, stores selected ids in reactiveValues
  
  observeEvent(input$second_map_shape_click, {
    click <- input$second_map_shape_click
    if (!is.null(click$id)) {
      if (click$id %in% vals$selected_ids) {
        # Deselect polygon if already selected
        vals$selected_ids <- setdiff(vals$selected_ids, click$id)
      } else {
        # Add polygon id to selection
        vals$selected_ids <- unique(c(vals$selected_ids, click$id))
      }
      showNotification(paste("Selected IDs:", paste(vals$selected_ids, collapse = ", ")))
    }
  })
  
  # Keep -----------------------------------
  # Keep only selected polygons, discards unselected ones
  # Updates second map polygons accordingly
  
  observeEvent(input$keep_selected, {
    req(vals$all_p)
    if (length(vals$selected_ids) == 0) {
      showNotification("No polygons selected!", type = "warning")
      return()
    }
    kept <- vals$all_p %>% dplyr::filter(id %in% vals$selected_ids)
    
    push_history()
    
    vals$all_p <- kept
    output$second_map <- renderLeaflet({
      create_leaflet_with_layers(vals$r_merge, kept)
    })
    showNotification(paste0("Kept ", nrow(kept), " selected polygons."))
    vals$selected_ids <- c()  # Clear selection after keeping
  })
  
  # Save gpkg-----------------------------------
  # Save current polygons to a GeoPackage file
  # Includes metadata fields from selected TIFs and datetime extracted from TIF name
  # Automatically creates output folder if missing
  # Generates unique filename by appending suffix if file exists
  
  observeEvent(input$save_to_gpkg, {
    req(vals$all_p, vals$selected_layers, input$out_dir)
    
    if (!dir.exists(input$out_dir)) dir.create(input$out_dir)
    
    tif_names <- vals$ordered_layers %||% vals$selected_layers
    tifs_used <- paste(tif_names, collapse = ";")
    
    # Extract datetime string from first TIF name, assuming format like "20240701_xyz"
    datetime <- sub("_.*", "", tif_names[[1]])
    
    base_name <- paste0(datetime, "_SAM_output")
    save_dir <- input$out_dir
    ext <- ".gpkg"
    save_path <- file.path(save_dir, paste0(base_name, ext))
    
    # Append suffix if file exists
    i <- 1
    while (file.exists(save_path)) {
      suffix <- paste0("(", i, ")")
      save_path <- file.path(save_dir, paste0(base_name, suffix, ext))
      i <- i + 1
    }
    
    # Add metadata columns and save
    sf_to_save <- vals$all_p %>%
      dplyr::mutate(tifs_used = tifs_used, datetime = datetime)
    
    sf::st_write(sf_to_save, save_path, delete_dsn = TRUE, quiet = TRUE)
    
    showNotification(paste("GeoPackage saved to:", save_path), type = "message")
  })
  
  
  # Undo button -------------------------------------------------------------
  
  observeEvent(input$undo_last, {
    if (length(vals$history) > 0) {
      vals$all_p <- vals$history[[length(vals$history)]]
      vals$history <- vals$history[-length(vals$history)]
      
      output$second_map <- renderLeaflet({
        create_leaflet_with_layers(vals$r_merge, vals$all_p)
      })
      
      showNotification("Undo successful!", type = "message")
    } else {
      showNotification("No previous state to undo.", type = "warning")
    }
  })
  
  
  # Fill holes --------------------------------------------------------------
  
  observeEvent(input$fill_holes, {
    req(vals$all_p)
    
    # Fill holes in each polygon geometry using lwgeom::fill_holes
    # (lwgeom package provides st_fill_holes to fill polygon holes)
    
    if (!requireNamespace("nngeo", quietly = TRUE)) {
      showNotification("Please install 'nngeo' package to fill holes.", type = "error")
      return()
    }
    
    filled <- sf::st_sf(
      geometry = nngeo::st_remove_holes(vals$all_p$geometry),
      crs = sf::st_crs(vals$all_p)
    )
    
    # Preserve id column if exists
    if ("id" %in% colnames(vals$all_p)) {
      filled$id <- vals$all_p$id
    } else {
      filled$id <- seq_len(nrow(filled))
    }
    
    push_history()
    
    vals$all_p <- filled
    
    output$second_map <- renderLeaflet({
      create_leaflet_with_layers(vals$r_merge, vals$all_p)
    })
    
    showNotification("Holes filled in polygons!", type = "message")
  })
  
  
  
  
  
}

# Launch the app----------------------------------------------------------

shinyApp(ui, server)

