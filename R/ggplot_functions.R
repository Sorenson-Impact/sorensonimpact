
#' Save a branded plot
#' @description Save a \code{ggplot2} plot as png with an optional Sorenson Impact branding bar.
#' @importFrom magrittr "%>%"
#' @param filename Filename to create on disk. If "auto", defaults to the title of the \code{last_plot} within the \code{plot_directory}.
#' @param dir Directory to save file to.  Defaults to "auto", which places plots in a "plots" subdir of the script dir.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width Width in inches (default: 6).
#' @param height Height in inches (default: 4).
#' @param dpi Dots per inch. Defaults to 300.  The resolution of the file will be width*DPI by height*DPI.
#' @param add_logo Logical. Add the sorenson impact branding bar?
#' @param logo_height_ratio Number between 0 and 1, with sensibile values between .01 and .1.  The percent of the image height that the bar should be. Default is 0.05.
#' @param band_color The color of the SI Logo band.  Defaults to \code{SI_design$granite}.
#' @return A png of the last plot with optional SI logo band.
#' @examples
#' SI_colorplot() + ggplot2::ggtitle("My Title")
#' SI_ggsave(add_logo = TRUE)
#' @export
si_ggsave <- function(filename = "auto", dir = "auto", plot = ggplot2::last_plot(), width = 6, height = 4, dpi = 300, add_logo = FALSE, logo_height_ratio = .05, band_color = SI_design$granite) {


  if(dir == "auto"){
    dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "plots")
    if(!dir.exists(dir)) dir.create(dir)
  }
  if(!dir.exists(dir)) stop(cat("Provided dir \"", dir, "\" does not exist."))



  if(filename == "auto" & !is.null(ggplot2::last_plot()$labels$title)) { #if the default "auto" is left, generate a dynamic file name.
    # We use the plot title and (if coming from an Rmd) the title parameter to create the file name
    # !Be careful not to use the same plot title more than once!
    # The following default variable is how the file is saved

    if(exists("params$set_title")) {
      # Abbreviated name of the report:
      # We did this because it's nice to know which report the saved graphs are coming from
      ab_report <- abbreviate(params$set_title)
      filename <- file.path(dir, ggplot2::last_plot()$labels$title, "_", ab_report, ".png")
    } else filename <- file.path(dir, paste0(ggplot2::last_plot()$labels$title, ".png"))

  } else stop("Plot must have a title to use auto filename with SI_ggsave. Add a title or specify the filename.") #User must choose one or the other.




  # First we save the last plot with sensible defaults
  ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi)

  # Now bring it back if we are adding the band
  if(add_logo) {
    plot <- magick::image_read(filename)
    pwidth <- as.data.frame(magick::image_info(plot))$width
    pheight <- as.data.frame(magick::image_info(plot))$height
    # Load the logo and crop it to the width of the default plot, fig_width: 6
    logo <- magick::image_read("~/Github/SI_Project_Template/template_files/SI_logo_background.png") %>%
      magick::image_scale(paste0("x", pheight * logo_height_ratio)) %>% #make the height of the logo equal to a ratio of the height of the plot. Defaults to 5%.
      magick::image_background(band_color, flatten = TRUE) %>%
      magick::image_crop(paste0(pwidth, "x0+0+0")) #make the width of the logo match the width of the plot

    # The final version is stacked on top of the sorenson logo
    final_plot <- magick::image_append(c(plot, logo), stack = TRUE)
    # And then we overwrite the standard ggsave call
    magick::image_write(final_plot, filename)
  }
}

#' Show Sorenson Impact Theme Colors
#' @description Shows the Sorenson Impact theme colors for reference.
#' @importFrom magrittr "%>%"
#' @return A plot of SI colors
#' @examples
#' SI_colorplot()
#' @export
si_colorplot <- function() {
  data.frame("color" = names(unlist(SI_design)),
             "code" = unlist(SI_design), stringsAsFactors = F) %>%
    ggplot2::ggplot() +
      ggplot2::geom_rect(ggplot2::aes(fill = I(code)), xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    ggplot2::facet_wrap(~color)
  }


#' Apply all Sorenson Impact ggplot themes
#' @description Applies all Sorenson Impact custom colors and settings for ggplot
#' @return Invisibly sets SI ggplot theme values.
#' @examples
#' SI_ggplot_update()
#' @export
si_ggplot_update <- function() {
  ggplot2::update_geom_defaults("bar", list(fill = SI_design$pacific))
  ggplot2::update_geom_defaults("smooth", list(colour = SI_design$pacific, fill = SI_design$arctic, alpha = I(2/10)))
  ggplot2::update_geom_defaults("point", list(colour = SI_design$pacific, fill = SI_design$pacific))
  ggplot2::update_geom_defaults("col", list(fill = SI_design$pacific))

  ggplot2::theme_set(ggplot2::theme_minimal())

  if("extrafont" %in% installed.packages()) {
    if("Roboto" %in% extrafont::fonts()) {

      #In order to use the SI font, Roboto, it needs to be installed.  See https://github.com/wch/extrafont
      #Once on the system and imported, also use loadfonts() and loadfonts(device="postscript")

      ggplot2::theme_update(text = ggplot2::element_text(family = "Roboto"),
                            axis.text = ggplot2::element_text(family = "Roboto"),
                            strip.text = ggplot2::element_text(family = "Roboto"))
    }
  } else warning("Package extrafont not installed or Roboto font family not installed.")
}
