#' ---
#' title: "Render All"
#' author: "Kevin Lu"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document: 
#'     theme: default 
#'     highlight: tango
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 8 
#'     fig_height: 5 
#' --- 

#' # 1. Render Functions
render_wrapper <- function(filename, output) { 
  
  input <- paste0("./src/", filename, ".R")
  
  if (output == "html_document") { 
    rmarkdown::render(input = input, 
                      output_file <- paste0(here::here(), "/notebooks/", filename, ".html"),
                      knit_root_dir = here::here(), 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = input, 
                      output_file <- paste0(here::here(), "/notebooks/", filename, ".pdf"), 
                      knit_root_dir = here::here(), 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = input, 
                      output_file <- paste0(here::here(), "/notebooks/", filename, ".html"), 
                      knit_root_dir = here::here(), 
                      output_format = "flexdashboard::flex_dashboard")
  }
} 

#' # 2. Render 
render_wrapper("01-load-packages", "html_document")
render_wrapper("02-download-company-list", "html_document")
render_wrapper("03-download-ohlc", "html_document")
render_wrapper("04-download-earnings", "html_document")
render_wrapper("05-create-combined", "html_document")
render_wrapper("06-plot-combined", "html_document")
render_wrapper("07-engineer-features", "html_document")
render_wrapper("08-plot-features", "html_document")
render_wrapper("09-cross-validate-model", "html_document")
render_wrapper("10-train-model", "html_document")
render_wrapper("11-examine-predictions", "html_document")

#' # 3. Beep 
beepr::beep(1)
