font_add_google("Raleway", family="Raleway", regular.wt=400, bold.wt=700)
showtext_auto(TRUE)

theme_hackathon = function(hjust=TRUE) { 
  theme_output = (
    theme_minimal(base_family="Raleway", base_size=8.75) +
    theme(
      text=element_text(color="#081534"),
      
      plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
      panel.background=element_blank(),
      panel.grid.minor=element_line(color="#EEEEEE", size=0.4),
      panel.spacing=unit(0.25, "cm"), 
      panel.border=element_blank(), 
      
      strip.background=element_rect(fill="#081534"),
      strip.text=element_text(face="bold", size=10, color="white"), 
      
      axis.line=element_blank(),
      
      plot.title=element_textbox(size=14, lineheight=1), 
      plot.title.position="plot",
      
      legend.margin=margin(-2,-2,-2,-2),
      
      ggh4x.axis.nestline.y=element_line(size=0.25),
      ggh4x.axis.nesttext.y=element_text(angle=90, vjust=0.5, hjust=0.5, face="bold")
    )
  )
  if(hjust==TRUE) { 
    theme_output = theme_output + theme(axis.text.x=element_text(hjust=c(0,1)))
  }
  return(theme_output)
}

plot_ao_scatter = function(
    ggplot_obj, 
    col_vector, 
    lim_max, 
    steps_minor, 
    title, 
    xlab, 
    ylab
) { 
  plot = (
    ggplot_obj + 
      
      # Instantiate geoms
      geom_point(aes(shape=text, colour=col), size=3) +
      geom_text_repel(
        data=. %>% filter(text == TRUE), 
        aes(label=str_wrap(var3, 12)), 
        seed=42, 
        min.segment.length=unit(0,"cm"),
        lineheight=0.825,
        size=3,
        family="Raleway",
        hjust=0.5
      ) +
      facet_grid(~Market) +
      
      # Coordinates and scales
      coord_equal(expand=FALSE) +
      scale_shape_manual(values=c(21, 16)) +
      scale_colour_manual(values=col_vector) +
      scale_x_continuous(
        limits=c(0,lim_max), 
        breaks=seq(0,lim_max,lim_max), 
        minor_breaks=seq(0,lim_max,steps_minor),
        labels=function(x) paste0(x, "%")
      ) +
      scale_y_continuous(
        limits=c(0,lim_max), 
        breaks=seq(0,lim_max,lim_max), 
        minor_breaks=seq(0,lim_max,steps_minor),
        labels=function(x) paste0(x, "%")
      ) + 
      guides(shape="none", colour="none") +
      
      # Theme specs
      theme_hackathon() +
      
      # Add labels
      labs(title=title, x=xlab, y=ylab)
  )
  return(plot)
}