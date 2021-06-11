# require(openxlsx)
# require(fs)
#
# xview <- function(x) {
#   wb <- createWorkbook()
#   wb <- addWorksheet(wb, "temp")
#
#   #file <- file_temp(ext = ".xlsx")
#
#   writeDataTable(wb = wb, sheet = 1, x = x)
#
#   openXL(wb)
#   # file_show(file)
# }
#
#
# #Wants:
#
# If number is between 0 and 1, apply percent formatting
# class(df$Percentage) <- "percentage"
#
# If number is tiny:
# class(df$TinyNumber) <- "scientific"

#' Show a table in Excel
#' @description
#' \lifecycle{experimental}
#' Opens the table in a temporary file in excel.
#' @param .data A table.
#' @examples
#' \dontrun{
#' diamonds %>% xview()
#' }
#' @export

xview <- function (.data) {
  tempFilePath = paste(tempfile(), ".xlsx")
  tempPath = dirname(tempFilePath)
  preferredFile = paste(deparse(substitute(.data)), ".xlsx", sep = "")
  preferredFilePath = file.path(tempPath, preferredFile)

  if(length(dim(.data))>2){
    stop('Too many dimensions')
  }
  if(is.null(dim(.data))){
    .data = as.data.frame(.data)
  }
  #if (is.null(rownames(.data))) {
  #  tmp = 1:nrow(.data)
  # }else {
  #  tmp = rownames(.data)
  #}
  #rownames(.data) = NULL
  #.data = data.frame(RowLabels = tmp, .data)

  if(tibble::has_rownames(data)) .data <- .data %>% tibble::rownames_to_column()

  WriteAttempt = try(
    WriteXLS::WriteXLS(as.character(bquote(.data)), ExcelFileName=preferredFilePath, FreezeRow=1, FreezeCol=1, BoldHeaderRow=T, AdjWidth=F, AutoFilter=T, row.names=F),
    silent = TRUE)
  if ("try-error" %in% class(WriteAttempt)) {
    WriteXLS::WriteXLS(as.character(bquote(.data)), ExcelFileName=tempFilePath, FreezeRow=1, FreezeCol=1, BoldHeaderRow=T, AdjWidth=F, AutoFilter=T, row.names=F)
    shell.exec(tempFilePath)
  } else {
    shell.exec(preferredFilePath)
  }
}
