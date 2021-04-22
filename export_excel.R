exportar_caracterizacion_dimensiones <- function  (dimdesc, num_dim){
  for(i in 1:num_dim){
    title <- paste0("Dim ", i)
    writeWorksheetToFile(

      paste0(getwd(), "/test.xlsx"),
      dimdesc[[title]][["quali"]],
      sheet = paste0("caracterizacion_", title),
      rownames = "variables",
      clearSheets = TRUE)
    writeWorksheetToFile(

      paste0(getwd(), "/test.xlsx"),
      dimdesc[[title]][["category"]],
      sheet = paste0("caracterizacion_", title),
      startCol = 6,
      rownames = "valores",
      clearSheets = FALSE)
  }
}
exportar_caracterizacion_dimensiones2 <- function  (dimdesc, num_dim){
  for(i in 1:num_dim){
    title <- paste0("Dim.", i)
    writeWorksheetToFile(

      paste0(getwd(), "/test.xlsx"),
      dimdesc[[title]][["quanti"]],
      sheet = paste0("caracterizacion_", title),
      rownames = "variables",
      clearSheets = TRUE)
  }
}

exportar_eigenvalues_dimensiones <- function (dataset){
  library(XLConnect)
  writeWorksheetToFile(

    paste0(getwd(), "/test.xlsx"),
    dataset$eig,
    sheet="dimensiones",
    rownames = "dimensiones",
    clearSheets = TRUE)
}
