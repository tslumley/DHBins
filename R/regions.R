
regions<-data.frame(
printname=c("Northland","Auckland","Waikato", "Taranaki",  "Bay of\nPlenty","Manawatu-\nWanganui",
            "Gisborne","Hawke's\nBay","Wellington", "Nelson",
            "Marlborough","Tasman","Canterbury","West Coast","Otago",
            "Southland"),
keyname=c("Northland","Auckland","Waikato", "Taranaki",  "Bay of Plenty","Manawatu-Wanganui",
            "Gisborne","Hawke's Bay","Wellington", "Nelson",
            "Marlborough","Tasman","Canterbury","West Coast","Otago",
            "Southland"),
shortname=c("N","A","W","T","BoP","M-W","G","HB","W","N","M","T","C","WC","O","S"),
x=(1+c(1,2,2,2,3,3,4,4,4,  2,2,1,1,0,0,-1))*(1.5),
y=(1+c(13,12,10,8,9,7,10,8,6, 4,2,3,1,2,-0,-1))*sqrt(3)/2
)

regions_no_tasman<-data.frame(
printname=c("Northland","Auckland","Waikato", "Taranaki",  "Bay of\nPlenty","Manawatu-\nWanganui",
            "Gisborne","Hawke's\nBay","Wellington",
            "Nelson","Marlborough","West Coast","Canterbury","Otago",
            "Southland"),
keyname=c("Northland","Auckland","Waikato", "Taranaki",  "Bay of Plenty","Manawatu-Wanganui",
            "Gisborne","Hawke's Bay","Wellington",
            "Nelson","Marlborough","West Coast","Canterbury","Otago",
            "Southland"),
shortname=c("N","A","W","T","BoP","M-W","G","HB","W","N","M","WC","C","O","S"),
x=c(1,2,2,2,3,3,4,4,4, 2,3,1,2,1,0)*(1.5),
y=c(13,12,10,8,9,7,10,8,6, 4,3,3,2,1,0)*sqrt(3)/2
)


.regaliases<-list(
    Northland=c("Northland","Northland region"),
    Auckland=c("Auckland","Auckland region"),
    Waikato = c("Waikato","Waikato region"),
    Taranaki=c("Taranaki","Taranaki region"),
    "Bay of Plenty"=c("Bay of Plenty","Bay Of Plenty","Bay of Plenty region"),
    "Manawatu-Wanganui"=c("Manawatu-Wanganui","Manawatu-Wanganui region"),
    Gisborne=c("Gisborne","Gisborne region","Gisborne district"),
    "Hawke's Bay"=c("Hawke's Bay","Hawkes Bay","Hawke's Bay region"),
    Wellington=c("Wellington","Wellington region"),
    Nelson=c("Nelson","Nelson region","Nelson City","Nelson Tasman region","Nelson-Tasman region","Nelson Tasman","Nelson-Tasman"),
    Tasman=c("Tasman","Tasman region","Tasman district"),
    Marlborough=c("Marlborough","Marlborough region","Marlborough district"),
    "West Coast"=c("West Coast","West Coast region"),
    Canterbury=c("Canterbury","Canterbury region"),
    Otago=c("Otago","Otago region"),
    Southland=c("Southland","Southland region")
    )


regaliases<-data.frame(keyname=rep(names(.regaliases),sapply(.regaliases,length)),
                    alias=do.call(c,c(.regaliases,use.names=FALSE)),
                    stringsAsFactors =FALSE)


regionbin<-function(radii=NULL,hex_colours="lightskyblue",region_names=NULL,
                    text_colour="black",legend_opts=NULL,border=NULL,short=FALSE,tasman=TRUE,cex=0.7){

    if(tasman){
        regions<-regions
    } else {
        regions<-regions_no_tasman
        }
	if(is.null(radii)){
		radii<-rep(0.95,nrow(regions))
	}
	if( max(radii)>1) radii<-0.95*radii/max(radii)
        if(length(hex_colours)<20) hex_colours<-rep(hex_colours,length.out=nrow(regions))
        if(is.null(region_names)) region_names<-names(radii)
        if (is.null(region_names)) region_names<-names(hex_colours)
	if (!is.null(region_names)){
		idx<-region_lookup(region_names, regions)
		hex_colours<-hex_colours[idx]
                radii<-radii[idx]
	}

        has.legend<-!is.null(legend_opts)
	with(regions,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,7+tasman),ylim=c(-1,12+tasman),axes=FALSE,xlab="",ylab=""))
	with(regions,hexes(x,y,radii,cols=hex_colours,flat=TRUE,border=border))
	if (short)
	  with(regions, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(regions, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}

regiontri<-function(radii=NULL,tri_colours,region_names=NULL,text_colour="black",legend_opts=NULL,short=FALSE,tasman=TRUE,cex=0.7){
        if(tasman){
            regions<-regions
        } else {
            regions<-regions_no_tasman
        }
        if(is.null(radii)){
		radii<-rep(0.95,nrow(regions))
	}
	if( max(radii)>1) radii<-0.95*radii/max(radii)

        if(is.null(region_names)) region_names<-names(radii)
        if (is.null(region_names)) region_names<-rownames(tri_colours)
	if (!is.null(region_names)){
		idx<-region_lookup(region_names,regions)
		tri_colours<-tri_colours[idx,]
                radii<-radii[idx]
	}
         has.legend<-!is.null(legend_opts)
	with(regions,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,7+tasman),ylim=c(-1,12+tasman),axes=FALSE,xlab="",ylab=""))
	with(regions,triangles(x,y,radii,cols=tri_colours,flat=TRUE))
	if (short)
	  with(regions, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(regions, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}


region_lookup<-function(names,regions){
  canonical_name<-region_fixname(names)
  idx2<-match(regions$keyname,canonical_name)
  idx2
}



region_fixname<-function(names){
  idx<-match(names, regaliases$alias)
  if(any(is.na(idx)))
    warning(paste("could not match",paste(names[is.na(idx)],collapse=",")))
  regaliases$keyname[idx]
}


        

region_lookuptri<-function(names, tri_id){
    canonical_name<-region_fixname(names)
    tri_name<-paste(rep(regions$keyname,each=6),rep(1:6,nrow(regions)),sep="_")
    canonical_tri<-paste(canonical_name, as.numeric(as.factor(tri_id)),sep="_")
    idx2<-match(tri_name,canonical_tri)
    idx2
}


regmap_hex<-function(){
    size=rep(0.95,nrow(regions))
    hex_x <- hex_point
    hex_y <- hex_flat
    
    na.omit(
        data.frame(
            x=as.vector(t(outer(size, hex_x) + regions$x)),
            y= as.vector(t(outer(size, hex_y) + regions$y)),
            id =rep(regions$keyname,each=8)
        )
    )
}
