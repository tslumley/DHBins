geom_dhb<-function(mapping = NULL, data = NULL, ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
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
