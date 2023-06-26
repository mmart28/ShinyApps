find_supplier_location <- function(glass_supplier) {
  if (glass_supplier == "Supplier A") {
    return("Glass_Supplier_Location.Minnesota")
  } else if (glass_supplier == "Supplier B") {
    return("Glass_Supplier_Location.Michigan")
  } else if (glass_supplier == "Supplier C") {
    return("Glass_Supplier_Location.Iowa")
  } else if (glass_supplier == "Supplier D") {
    return("Glass_Supplier_Location.Iowa")
  } else {
    return("Unknown Supplier")
  }
}
