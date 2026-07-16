# Regenerates inst/reference.docx: pandoc's default reference document with the
# body-text styles set to justified alignment. The reference docx is where the
# Word output gets every paragraph style, so justification has to live here --
# there is no rmarkdown/officedown option for it.
#
# Run from the project root:  Rscript inst/make_reference_docx.R

pandoc <- file.path(rmarkdown::find_pandoc()$dir, "pandoc")

work <- file.path(tempdir(), "reference-docx")
dir.create(work, showWarnings = FALSE)
ref <- file.path(work, "reference.docx")
system2(pandoc, c("--print-default-data-file", "reference.docx"), stdout = ref)

unzip(ref, exdir = file.path(work, "unzipped"))
styles_path <- file.path(work, "unzipped", "word", "styles.xml")
doc <- xml2::read_xml(styles_path)

# Justify the styles pandoc writes plain paragraphs with. Everything else
# (headings, captions, tables) stays as shipped.
for (id in c("BodyText", "FirstParagraph")) {
  style <- xml2::xml_find_first(doc, sprintf("//w:style[@w:styleId='%s']", id))
  if (is.na(style)) stop("style not found in reference docx: ", id)
  ppr <- xml2::xml_find_first(style, "./w:pPr")
  if (is.na(ppr)) {
    xml2::xml_add_child(style, "w:pPr", .where = 1)
    ppr <- xml2::xml_find_first(style, "./w:pPr")
  }
  if (is.na(xml2::xml_find_first(ppr, "./w:jc"))) {
    jc <- xml2::xml_add_child(ppr, "w:jc")
    xml2::xml_set_attr(jc, "w:val", "both")
  }
}
xml2::write_xml(doc, styles_path)

out <- file.path(getwd(), "inst", "reference.docx")
old <- setwd(file.path(work, "unzipped"))
unlink(out)
utils::zip(out, files = list.files(recursive = TRUE, all.files = TRUE),
           flags = "-r9Xq")
setwd(old)
cat("wrote", out, "\n")
