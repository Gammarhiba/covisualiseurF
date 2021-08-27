

# Graphical settings
lkp_blue  <- rgb(13, 45, 106, maxColorValue = 255) # LinkPact blue
lkp_green <- rgb(0, 143, 90, maxColorValue = 255)  # LinkPact green
lkp_grey <- rgb(61, 61,61, maxColorValue = 255)  # LinkPact grey
lkp_orange <- rgb(131, 60, 12, maxColorValue = 255)  # LinkPact orange
lkp_blue1 <- rgb(0, 34, 93, maxColorValue = 255)  # LinkPact blue1
lkp_yellow <- rgb(255, 192, 0, maxColorValue = 255)  # LinkPact yellow
lkp_purple <- rgb(112, 48, 160, maxColorValue = 255)  # LinkPact green
lkp_green1 <- rgb(0, 136, 81, maxColorValue = 255)  # LinkPact green1
lkp_blueHomme <- rgb(47, 117, 181, maxColorValue = 255)  # LinkPact blueHomme
lkp_redFemme <- rgb(255, 0, 0, maxColorValue = 255)  # LinkPact green


# Theme for plots
theme_LinkPact <- function (base_size = 12, 
                            base_family = "serif", 
                            base_line_size = base_size / 24, 
                            base_rect_size = base_size / 24) 
{
  half_line <- base_size / 2
  
  ggplot2::theme_bw(base_size, base_family, base_line_size, base_rect_size) +
    theme(text = element_text(family = base_family, face = "plain", 
                              colour = lkp_blue, size = base_size, lineheight = 0.9, 
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                              debug = FALSE),
          title = element_text(face = "bold"),
          axis.text = element_text(size = rel(0.8), colour = "grey30", face = "bold"), 
          axis.ticks = element_line(colour = "grey20"), 
          axis.title = element_text(size = rel(1)),
          legend.background = element_rect(colour = "grey85"), 
          legend.spacing = unit(half_line, "pt"), 
          legend.key.size = unit(1.2, "lines"), 
          legend.text = element_text(size = rel(0.8)),
          legend.position = "right", 
          legend.justification = "center",  
          legend.box.spacing = unit(half_line, "pt"), 
          panel.grid = element_line(colour = "grey92", size = rel(1.5)), 
          panel.grid.minor = element_line(size = rel(0.5)), 
          panel.spacing = unit(half_line, "pt"), 
          panel.ontop = FALSE, 
          strip.background = element_rect(fill = lkp_green, colour = "black"), 
          strip.text = element_text(colour = "white", face = "bold", size = rel(0.8), 
                                    margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)), 
          strip.switch.pad.grid = unit(half_line / 2, "pt"), 
          strip.switch.pad.wrap = unit(half_line / 2, "pt"), 
          plot.background = element_rect(colour = "white"), 
          plot.title = element_text(size = rel(1.4), hjust = 0, vjust = 1, margin = margin(b = half_line)), 
          plot.title.position = "panel", 
          plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = half_line), color = lkp_green,size = rel(1.2)), 
          plot.caption = element_text(size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = half_line), color = lkp_green), 
          plot.caption.position = "panel", 
          plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5), 
          plot.tag.position = "topleft", 
          plot.margin = margin(half_line, 2 * half_line, half_line, half_line))
}

theme_set(theme_LinkPact()) # set for all subsequent plots
