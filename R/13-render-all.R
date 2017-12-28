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
  if (output == "html_document") { 
    rmarkdown::render(input = paste0(filename, ".R"), 
                      output_file = paste0("./Notebooks/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = paste0(filename, ".R"), 
                      output_file = paste0("./Notebooks/", filename, ".pdf"), 
                      knit_root_dir = ".", 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = paste0(filename, ".Rmd"), 
                      output_file = paste0("./Notebooks/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "flexdashboard::flex_dashboard")
  }
} 

#' # 2. Render 
render_wrapper("EMS.001 Load Packages", "html_document")
render_wrapper("EMS.002 Download Data", "html_document")
render_wrapper("EMS.003 Clean Data", "html_document")
render_wrapper("EMS.004 Plot Data", "html_document")
render_wrapper("EMS.005 Engineer Features", "html_document")
render_wrapper("EMS.006 Plot Features Model", "html_document")
render_wrapper("EMS.007 Cross Validate Model", "html_document")
render_wrapper("EMS.008 Train Model", "html_document")
render_wrapper("EMS.009 Examine Predictions", "html_document")

#' # 3. Beep 
beepr::beep(1)
