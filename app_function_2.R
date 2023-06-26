find_window_type <- function(window_type) {
  if (window_type == "Vinyl") {
    return("Window_Type.Vinyl")
  } else if (window_type == "Aluminum") {
    return("Window_Type.Aluminum")
  } else if (window_type == "Wood") {
    return("Window_Type.Aluminum")
  } else {
    return("Unknown Supplier")
  }
}