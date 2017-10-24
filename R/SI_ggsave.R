SI_ggsave <- function(filename = paste0(plot_directory, last_plot()$labels$title, "_", ab_report, ".png"),
                      width = 6, height = 4, dpi = 300,
                      add_logo = FALSE, logo_height_ratio = .05, band_color = SI_design$granite){

  # Abbreviated name of the report:
  # We did this because it's nice to know which report the saved graphs are coming from
  ab_report <- abbreviate(params$set_title)
  # We use that and the plot title to create the file name
  # !Be careful not to use the same plot title more than once!
  # The following default variable is how the file is saved
  # filename <- paste0(plot_directory, last_plot()$labels$title, "_", ab_report, ".png")

  # First we save the last plot with sensible defaults
  ggsave(filename, width = width, height = height, dpi = dpi)

  # Now bring it back if we are adding the band
  if(add_logo){
    plot <- image_read(filename)
    pwidth <- as.data.frame(image_info(plot))$width
    pheight <- as.data.frame(image_info(plot))$height
    # Load the logo and crop it to the width of the default plot, fig_width: 6
    logo <- image_read("~/Github/SI_Project_Template/template_files/SI_logo_background.png") %>%
      image_scale(paste0("x", pheight * logo_height_ratio)) %>% #make the height of the logo equal to a ratio of the height of the plot. Defaults to 5%.
      image_background(band_color, flatten = TRUE) %>%
      image_crop(paste0(pwidth, "x0+0+0")) #make the width of the logo match the width of the plot

    # The final version is stacked on top of the sorenson logo
    final_plot <- image_append(c(plot, logo), stack = TRUE)
    # And then we overwrite the standard ggsave call
    image_write(final_plot, filename)
  }
}
