# NA

    library(terra)
    #> terra 1.8.60
    library(tidyverse)
    library(tidyterra)
    #>
    #> Attaching package: 'tidyterra'
    #> The following object is masked from 'package:stats':
    #>
    #>     filter

    nc <- rast("./AQUA_MODIS.20020704_20250630.L3m.CU.KD.Kd_490.4km.nc")


    ggplot() +
      geom_spatraster(data = nc, aes(fill = Kd_490)) +
      scale_fill_grass_c(palette = "inferno", limits = c(0, 0.1))
    #> <SpatRaster> resampled to 5e+05 cells.

![](https://i.imgur.com/SYVU128.png)
