
# rotates a geometry by some angle theta 
st_rotate = function(x, theta){
    
    crs <- st_crs(x)
    r = theta * pi / 180
    rot <- matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
    xg <- st_geometry(x)
    xc <- st_centroid(st_combine(x))
    ret <- (xg-xc) * rot + xc
    
    if ("sf" %in% class(x)) {
        st_geometry(x) <- ret
        st_crs(x) <- crs
        x
    } else {
        st_sfc((xg-xc) * rot + xc, crs = st_crs(x))
    }
}

# extends a linestring by dist in both directions
st_extend_line <- function(x, dist) {
    
    if (st_is_longlat(x)) {
        stop("x cannot be longlat")
    }
    
    if (inherits(x, "sf")) {
        x <- split.data.frame(x, 1:nrow(x))
        f <- "rbind"
    } else if (inherits(x, "sfc")) {
        x <- split(x, 1:length(x))
        f <- "c"
    } else {
        stop("x must be a sf or sfc object")
    }

    lapply(x, function(x) {
        pts <- st_coordinates(x)[, 1:2]
        i <- c(2, nrow(pts) - 1)
        
        theta <- mapply(i, c(1, -1), FUN = function(i, j) {
            Ax <- pts[i-j,1]
            Ay <- pts[i-j,2]
            Bx <- pts[i,1]
            By <- pts[i,2]
            unname(atan2(Ay-By, Ax-Bx))
        })
        
        pts[1,] <- pts[1,] + dist * c(cos(theta[1]), sin(theta[1]))
        pts[nrow(pts),] <- pts[nrow(pts),] + dist * c(cos(theta[2]), sin(theta[2]))
        geom <- st_sfc(st_linestring(pts), crs = st_crs(x))
        if (inherits(x, "sf")) {
            st_sf(st_drop_geometry(x), geometry = geom, crs = st_crs(x))
        } else {
            geom
        }
        
    }) |> do.call(what = f)
}

# adds shading lines inside a polygon, at the given angle and with the
# given density
st_shade <- function(x, angle, density) {
    
    if (inherits(x, "sf")) {
        x <- st_geometry(x)
    }
    
    if (inherits(x, "sfc")) {
        if (length(angle) == 1) angle <- rep(angle, length(x))
        if (length(density) == 1) density <- rep(density, length(x))
        if (length(angle) != length(x)) stop("angle must be length 1 or ", length(x))
        if (length(density) != length(x)) stop("density must be length 1 or ", length(x))
    } else {
        stop("x must be an sfc object")
    }

    gbb <- st_bbox(x)
    cellsize <- max(gbb[3]-gbb[1], gbb[4]-gbb[2]) / density
    
    lapply(1:length(x), function(i) {
        z <- x[i]
        density <- density[i]
        angle <- angle[i]
        bb <- st_bbox(z)
        buf_size <- 0.50 * max(bb[3]-bb[1], bb[4]-bb[2])

        ny = ceiling((bb[4] - bb[2]) / cellsize[i])
        xc <- c(bb[3], bb[1])
        yc <- bb[2] + (0:ny) * cellsize[i]
        
        g <- lapply(1:length(yc), function(j) {
            m <- matrix(c(xc[1], yc[j], xc[2], yc[j]), ncol=2, byrow=TRUE)
            st_sf(group = j, geometry = st_sfc(st_linestring(m)), crs = st_crs(x))
        }) |> do.call(what = rbind)

        g <- st_rotate(g, angle)
        g <- st_extend_line(g, buf_size)
        g <- st_intersection(g, z)
        st_geometry(st_combine(g))
    }) |> do.call(what = c)
}

# examples

# download some country polygons
library(sf)
library(rnaturalearth)
library(ggplot2)

x <- ne_countries(country = c("Norway", "Denmark", "Sweden", "Finland"), returnclass="sf", scale = "medium")
x <- st_transform(x, crs = 32633)

# shade all features, and specify angles individually 
ggplot()+
    geom_sf(data=x, aes(fill=brk_name))+
    geom_sf(data=st_shade(x, angle = c(30,15,45,-75), density = 100), lwd = 1)

# shade one specific feature
ggplot()+
    geom_sf(data=nor, aes(fill=brk_name))+
    geom_sf(data=st_shade(nor[nor$brk_name=="Norway",], angle = 30, density = 100), lwd = 1)



