require(ggplot2)
require(openxlsx)
require(xml2)
require(zip)


# Build Workbook ----------------------------------------------------------

wbfilename = "supervisor.xlsx"
wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Calibri")

addWorksheet(wb, sheetName = "Lime", tabColour = "limegreen")
writeDataTable(wb, sheet = 1, x = mtcars, colNames = TRUE, rowNames = TRUE, bandedRows = FALSE,
              tableStyle = "TableStyleMedium18")
setColWidths(wb, sheet = 1, cols = "A", widths = 18)
s0 <- createStyle(fgFill = "darkgray", fontColour = "white", textDecoration = "bold")
addStyle(wb, 1, s0, rows = 1:1, cols = 1:12, gridExpand = TRUE)
saveWorkbook(wb, wbfilename, overwrite = TRUE) ## save to working directory
#remove(wb, wbfilename, s0)

# Manually apply filter ---------------------------------------------------

ziptemp = "./ziptemp"
unzip(wbfilename, exdir = ziptemp)

xmlfile <- paste(ziptemp, "/xl/tables/table3.xml", sep = "")
x <- read_xml(xmlfile)
autofilternode <- xml_find_first(x, "//d1:autoFilter", xml_ns(x))
nodetoadd <- read_xml("<filterColumn colId=\"0\"><filters blank=\"1\" /></filterColumn>")   #preselect 'blanks' filter on col A (0)
xml_add_child(autofilternode, nodetoadd)
write_xml(x, xmlfile)

xmlfile <- paste(ziptemp, "/xl/worksheets/sheet1.xml", sep = "")
x <- read_xml(xmlfile)
nodes <- xml_find_all(x, "//d1:row[@r>3 and @r<25]", xml_ns(x))     #d1 is because a namespace is in use; the node is named 'row'
xml_set_attr(nodes, "hidden", 1, xml_ns(x))
write_xml(x, xmlfile)

files2zip <- dir(ziptemp, full.names = TRUE)
zip(zipfile = wbfilename, dir(ziptemp, full.names = TRUE), mode = "cherry-pick", include_directories = FALSE)
unlink(ziptemp, recursive = TRUE)   #delete temp directory
#remove(x, xmlfile, autofilternode, nodes, nodetoadd, files2zip, ziptemp)
