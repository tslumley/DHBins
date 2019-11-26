geom_dhb_old<-function(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
	map<-dhmap_hex()
	list(
            geom_map(mapping, data, ..., na.rm=na.rm, show.legend=show.legend, inherit.aes=inherit.aes,map=map),
            expand_limits(map),
            coord_fixed(),
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(), axis.title.y=element_blank(),
                  axis.text.y=element_blank(),panel.grid=element_blank())
	)
	
}


geom_label_dhb<-function(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,short=FALSE){
	d<-data.frame(
		x=dhbs$x,
		y=dhbs$y,
		dhb_labels=if(short) 
					dhbs$shortname 
				else 
					dhbs$printname
	)
	geom_text(data=d, aes(x,y,label=dhb_labels),...)
}


geom_dhb <- function(mapping = NULL, data = NULL,
                      stat = "identity", 
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,coord=TRUE) {



    
    rval<-list( 
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomDHBmap,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )),

        theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(), axis.title.y=element_blank(),
                  axis.text.y=element_blank(),panel.grid=element_blank())
    )
    if(coord){
        rval<-c(rval,
            coord_fixed(),
            expand_limits(dhmap_hex()))
        }
     rval

}


"%||%" <- function (a, b) 
{
    if (!is.null(a)) 
        a
    else b
}

GeomDHBmap <- ggproto("GeomDHBmap", GeomPolygon,
                        default_aes = aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1,
                                          alpha = NA, subgroup = NULL, radius=0.95),
  draw_panel = function(data, panel_params, coord, map) {

      hex_x <- hex_point
      hex_y <- hex_flat
      ## need to reorder sizes to match order
      radius<-data$radius
      if (max(radius)>1)
          radius<-0.95*radius/max(radius)
      
      idx<-dhb_lookup(data$map_id)
      radius<-radius[idx]
	
    map<-na.omit(
        data.frame(
            x=as.vector(t(outer(radius, hex_x) + dhbs$x)),
            y= as.vector(t(outer(radius, hex_y) + dhbs$y)),
            id =rep(dhbs$keyname,each=8)
        )
    )
    common <- intersect(data$map_id, map$id)
    data <- data[data$map_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_params)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data <- data[data_rows, , drop = FALSE]

    grid::polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = grid::gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt
      )
    )
  },

  required_aes = c("map_id")
)




geom_dhbtri <- function(mapping = NULL, data = NULL,
                      stat = "identity", 
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,coord=TRUE) {



    
    rval<-list( 
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomDHBtri,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )),

        theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(), axis.title.y=element_blank(),
                  axis.text.y=element_blank(),panel.grid=element_blank())
    )
    if(coord){
        rval<-c(rval,
            coord_fixed(),
            expand_limits(dhmap_hex()))
        }
     rval

}


## Actually, we probably need to override draw_group as in
## https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
GeomDHBtri <- ggproto("GeomDHBtri", GeomPolygon,
                        default_aes = aes(colour = "NA",  size = 0.5, linetype = 1,
                                          alpha = NA, subgroup = NULL, radius=0.95),
  draw_panel = function(data, panel_params, coord, map) {
      tri_x <- tri_point
      tri_y <- tri_flat
      ## need to reorder sizes to match order
      radius<-data$radius
      if (max(radius)>1)
          radius<-0.95*radius/max(radius)
      
      idx<-with(data, dhb_lookuptri(map_id,class_id))
      radius<-radius[idx]
      data$full_id<-with(data,paste(map_id,as.numeric(as.factor(class_id)),sep="_"))
	
    map<-na.omit(
        data.frame(
            x=as.vector(t(outer(radius, tri_x) + dhbs$x)),
            y= as.vector(t(outer(radius, tri_y) + dhbs$y)),
            id= paste(rep(dhbs$keyname,each=6*4), rep(rep(1:6,nrow(dhbs)),each=4), sep="_")
        )
    )
    common <- intersect(data$full_id, map$id)
    data <- data[data$full_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_params)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$full_id)
    data <- data[data_rows, , drop = FALSE]

    grid::polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = grid::gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt
      )
    )
  },

  required_aes = c("map_id","class_id","fill")
)
