
tri_point<-c(0, 1, 0.5, NA, 0, 0.5, -0.5, NA, 0, -0.5, -1, NA, 0, -1, -0.5, 
NA, 0, -0.5, 0.5, NA, 0, 0.5, 1, NA)

tri_flat<-c(0, 0, 0.866025403784439, NA, 0, 0.866025403784439, 0.866025403784439, 
NA, 0, 0.866025403784439, 0, NA, 0, 0, -0.866025403784439, NA, 
0, -0.866025403784439, -0.866025403784439, NA, 0, -0.866025403784439, 
0, NA)

hex_point<-c(1,.5,-0.5,-1,-0.5,0.5,1,NA)
hex_flat<-c(0, 0.866025403784439,0.866025403784439,0,-0.866025403784439,-0.866025403784439,0,NA)


triangles<-function (center_x, center_y, radii, cols, border = FALSE, asp = 1, flat=FALSE) 
{
	if (flat) {
		tri_x<-tri_point
		tri_y<-tri_flat
	} else{
		tri_y<-tri_point
		tri_x<-tri_flat
	}	
    x <- as.vector(t(outer(radii, tri_x) + center_x))
    y <- as.vector(t(outer(radii * asp, tri_y) + center_y))
    polygon(x, y, col = as.vector(t(cols)), border = if (border) 
        NA
    else as.vector(t(cols)))
    invisible(list(x = x, y = y, col = as.vector(t(cols))))
}

hexes<-function (center_x, center_y, radii, cols, border = NULL, asp = 1, flat=FALSE) 
{
	if (flat) {
		hex_x<-hex_point
		hex_y<-hex_flat
	} else{
		hex_y<-hex_point
		hex_x<-hex_flat
	}	

    x <- as.vector(t(outer(radii, hex_x) + center_x))
    y <- as.vector(t(outer(radii * asp, hex_y) + center_y))
    polygon(x, y, col = cols, border = if (is.null(border))
        NA
    else border)
    invisible(list(x = x, y = y, col = cols))
}


tri_alloc<-function(countmatrix,colours,classes=colours,names=rownames(countmatrix)){
	m<-matrix(classes[apply(countmatrix,1,sl)],
		byrow=TRUE,ncol=6)
	if(!is.null(names)) rownames(m)<-names
	m	
} 

sl<-function (counts) 
{
    nparties <- length(counts)
    nseats<-6
    denominators = 2 * (1:nseats) - 1
    quotients = outer(counts, denominators, "/")
    last = sort(quotients, decreasing = TRUE)[nseats]
    clear <- rowSums(quotients > last)
    borderline <- rowSums(quotients == last)
    borderline[sample(which(borderline > 0), sum(borderline) - 
        (nseats - sum(clear)))] <- 0
    total <- clear + borderline
    error <- counts - sum(counts) * total/6
    rval <- rep(1:nparties, clear + borderline)
    attr(rval, "error") <- error
    rval
}



dhbs<-data.frame(
printname=c("Northland","Waitemata","Counties \nManukau","Taranaki","Auckland",
            "Waikato","Whanganui","Capital\n and Coast", "Bay of\nPlenty","Lakes",
            "Midcentral","Hutt\nValley","Tairawhiti","Hawke's \nBay","Wairarapa",
            "Nelson \nMarlborough","West Coast","Canterbury","South \nCanterbury",
            "Southern"),
keyname=c("Northland","Waitemata","Counties Manukau","Taranaki","Auckland",
          "Waikato","Whanganui","Capital and Coast", "Bay of Plenty","Lakes",
          "Midcentral","Hutt","Tairawhiti","Hawke's Bay","Wairarapa",
          "Nelson Marlborough","West Coast","Canterbury","South Canterbury",
          "Southern"),
shortname=c("N","W","CM","T","A","W","Wh","CC","BOP","L","M","H","T","HB","W","NM","WC","C","SC","S"),
x=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,2,1,2,1,0)*(1.5),
y=c(18,16,14,12,15,13,11,9,14,12,10,8,13,11,9,5,4,3,2,1)*sqrt(3)/2
)

dhmap_hex<-function(){
    size=rep(0.95,20)
    hex_x <- hex_point
    hex_y <- hex_flat
    
    na.omit(
        data.frame(
            x=as.vector(t(outer(size, hex_x) + dhbs$x)),
            y= as.vector(t(outer(size, hex_y) + dhbs$y)),
            id =rep(dhbs$keyname,each=8)
        )
    )
}


dhmap_tri<-function(){
    size=rep(0.95,20)
    tri_x <- tri_point
    tri_y <- tri_flat
    
    na.omit(
        data.frame(
            x=as.vector(t(outer(size, tri_x) + dhbs$x)),
            y= as.vector(t(outer(size, tri_y) + dhbs$y)),
            id =rep(paste(dhbs$keyname,1:6),each=5)
        )
    )
}


.aliases<-list(Northland=c("Northland","NDHB","Northland DHB"),
              Waitemata=c("Waitemata","Waitemat\u0101","Waitemata DHB"),
              "Counties Manukau"=c("Counties Manukau","Counties","CM Health","Counties Manukau DHB"),
              Taranaki=c("TDHB","Taranaki","Taranaki DHB"),
              Auckland=c("ADHB","Auckland","Auckland DHB"),
              Waikato=c("Waikato","Waikato DHB"),
              Whanganui=c("Whanganui","Wanganui","WDHB","Whanganui DHB"),
              "Capital and Coast"=c("Capital and Coast","Capital & Coast","CCDHB","Capital and Coast DHB","Capital & Coast DHB"),
              "Bay of Plenty"=c("Bay of Plenty","Bay Of Plenty","BOP","BoP","BOPDHB","Bay of Plenty DHB"),
              Lakes=c("Lakes","Lakes DHB"),
              Midcentral=c("Midcentral","MidCentral","MDHB","Mid Central DHB","MidCentral DHB"),
              Hutt = c("Hutt","Hutt Valley","Hutt DHB"),
              Tairawhiti=c("Tairawhiti","Tair\u0101whiti","Tairawhiti DHB","Hauora Tair\u0101whiti"),
              "Hawke's Bay"=c("Hawke's Bay","Hawkes Bay","HBDHB","Hawkes Bay DHB"),
              Wairarapa=c("Wairarapa","Wairarapa DHB"),
              "Nelson Marlborough"=c("Nelson Marlborough","Nelson-Marlborough","NMDHB","Nelson Marlborough DHB"),
              "West Coast"=c("West Coast","WCDHB","West Coast DHB"),
              Canterbury=c("Canterbury","CDHB","Canterbury DHB"),
              `South Canterbury`=c("South Canterbury","SCDHB","South Canterbury DHB"),
              Southern=c("Southern","Southern DHB")
)

aliases<-data.frame(keyname=rep(names(.aliases),sapply(.aliases,length)),
                    alias=do.call(c,c(.aliases,use.names=FALSE)),
                    stringsAsFactors =FALSE)

dhb_lookup<-function(names){
  canonical_name<-dhb_fixname(names)
  idx2<-match(dhbs$keyname,canonical_name)
  idx2
}

dhb_lookuptri<-function(names, tri_id){
    canonical_name<-dhb_fixname(names)
    tri_name<-paste(rep(dhbs$keyname,each=6),rep(1:6,nrow(dhbs)),sep="_")
    canonical_tri<-paste(canonical_name, as.numeric(as.factor(tri_id)),sep="_")
    idx2<-match(tri_name,canonical_tri)
    idx2
}

dhb_fixname<-function(names){
  idx<-match(names, aliases$alias)
  if(any(is.na(idx)))
    warning(paste("could not match",paste(names[is.na(idx)],collapse=",")))
  aliases$keyname[idx]
}


dhbin<-function(radius=NULL,hex_colours="lightskyblue",DHB_names=NULL,text_colour="black",legend_opts=NULL,border=NULL,short=FALSE,cex=0.8){
	if(is.null(radius)){
		radius<-rep(0.95,nrow(dhbs))
	}
	if( max(radius)>1) radius<-0.95*radius/max(radius)
        if(length(hex_colours)<20) hex_colours<-rep(hex_colours,length.out=20)
        if(is.null(DHB_names)) DHB_names<-names(radius)
        if (is.null(DHB_names)) DHB_names<-names(hex_colours)
	if (!is.null(DHB_names)){
		idx<-dhb_lookup(DHB_names)
		hex_colours<-hex_colours[idx]
                radius<-radius[idx]
	}

        has.legend<-!is.null(legend_opts)
	with(dhbs,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,9),ylim=c(0,16),axes=FALSE,xlab="",ylab=""))
	with(dhbs,hexes(x,y,radius,cols=hex_colours,flat=TRUE,border=border))
	if (short)
	  with(dhbs, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(dhbs, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}

dhtri<-function(radius=NULL,tri_colours,DHB_names=NULL,text_colour="black",legend_opts=NULL,short=FALSE,cex=0.8){
	if(is.null(radius)){
		radius<-rep(0.95,nrow(dhbs))
	}
	if( max(radius)>1) radius<-0.95*radius/max(radius)

        if(is.null(DHB_names)) DHB_names<-names(radius)
        if (is.null(DHB_names)) DHB_names<-rownames(tri_colours)
	if (!is.null(DHB_names)){
		idx<-dhb_lookup(DHB_names)
		tri_colours<-tri_colours[idx,]
                radius<-radius[idx]
	}
         has.legend<-!is.null(legend_opts)
	with(dhbs,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,9),ylim=c(0,16),axes=FALSE,xlab="",ylab=""))
	with(dhbs,triangles(x,y,radius,cols=tri_colours,flat=TRUE))
	if (short)
	  with(dhbs, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(dhbs, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}

