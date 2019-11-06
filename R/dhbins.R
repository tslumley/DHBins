
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


tri_alloc<-function(countmatrix,colours,names=rownames(countmatrix)){
	m<-matrix(colours[apply(countmatrix,1,sl)],
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

.aliases<-list(Northland=c("Northland","NDHB"),
              Waitemata=c("Waitemata","Waitematā"),
              "Counties Manukau"=c("Counties Manukau","Counties","CM Health"),
              Taranaki=c("TDHB","Taranaki"),
              Auckland=c("ADHB","Auckland"),
              Waikato=c("Waikato"),
              Whanganui=c("Whanganui","Wanganui","WDHB"),
              "Capital and Coast"=c("Capital and Coast","Capital & Coast","CCDHB"),
              "Bay of Plenty"=c("Bay of Plenty","Bay Of Plenty","BOP","BoP","BOPDHB"),
              Lakes=c("Lakes"),
              Midcentral=c("Midcentral","MidCentral","MDHB"),
              Hutt = c("Hutt","Hutt Valley"),
              Tairawhiti=c("Tairawhiti","Tairāwhiti"),
              "Hawke's Bay"=c("Hawke's Bay","Hawkes Bay","HBDHB"),
              Wairarapa=c("Wairarapa"),
              "Nelson Marlborough"=c("Nelson Marlborough","Nelson-Marlborough","NMDHB"),
              "West Coast"=c("West Coast","WCDHB"),
              Canterbury=c("Canterbury","CDHB"),
              `South Canterbury`=c("South Canterbury","SCDHB"),
              Southern=c("Southern")
)

aliases<-data.frame(keyname=rep(names(.aliases),sapply(.aliases,length)),
                    alias=do.call(c,c(.aliases,use.names=FALSE)))

dhb_lookup<-function(names){
  idx<-match(names, aliases$alias)
  if(any(is.na(idx)))
    warning(paste("could not match",paste(names[is.na(idx)],collapse=",")))
  canonical_name<-aliases$keyname[idx]
  idx2<-match(dhbs$keyname,canonical_name)
  idx2
}


dhbin<-function(radii=NULL,hex_colours="lightskyblue",text_colour="black",legend_opts=NULL,border=NULL,short=FALSE,cex=0.8){
	if(is.null(radii)){
		radii<-rep(0.95,nrow(dhbs))
	}
	if( max(radii)>1) radii<-0.95*radii/max(radii)
	if (!is.null(names(hex_colours))){
		idx<-dhb_lookup(names(hex_colours))
		hex_colours<-hex_colours[idx]
	}
        has.legend<-!is.null(legend_opts)
	with(dhbs,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,9),ylim=c(0,16),axes=FALSE,xlab="",ylab=""))
	with(dhbs,hexes(x,y,radii,cols=hex_colours,flat=TRUE,border=border))
	if (short)
	  with(dhbs, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(dhbs, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}

dhtri<-function(radii=NULL,tri_colours,text_colour="black",legend_opts=NULL,short=FALSE,cex=0.8){
	if(is.null(radii)){
		radii<-rep(0.95,nrow(dhbs))
	}
	if( max(radii)>1) radii<-0.95*radii/max(radii)
	if (!is.null(rownames(tri_colours))){
	  idx<-dhb_lookup(rownames(tri_colours))
		tri_colours<-tri_colours[idx,]
	}
        has.legend<-!is.null(legend_opts)
	with(dhbs,plot(x,y,asp=TRUE,type="n",xlim=c(-2-2*has.legend,9),ylim=c(0,16),axes=FALSE,xlab="",ylab=""))
	with(dhbs,triangles(x,y,radii,cols=tri_colours,flat=TRUE))
	if (short)
	  with(dhbs, text(x,y,shortname,cex=cex,col=text_colour))
	else 
	  with(dhbs, text(x,y,printname,cex=cex,col=text_colour))
	if(!is.null(legend_opts)) {
		do.call(legend, c(list(x=-4.5,y=9,bty="n"),legend_opts))
	}
}

