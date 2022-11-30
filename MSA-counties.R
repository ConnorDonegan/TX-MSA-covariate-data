
dfw <- data.frame(
    County = paste(c("Collin", "Dallas", "Denton", "Ellis", "Hunt", "Kaufman", "Rockwall", "Johnson", "Parker", "Tarrant", "Wise"), "County, TX"),
    MSA = "Dallas"
    )

austin <- data.frame(
    County = paste(c("Williamson", "Travis", "Bastrop", "Caldwell", "Hays"), "County, TX"),
    MSA = "Austin"
    )
    
tonio <- data.frame(
    County = paste(c("Atascosa", "Bandera", "Bexar", "Comal", "Guadalupe", "Kendall", "Medina", "Wilson"), "County, TX"),
    MSA = "San Antonio"
    )

houston <- data.frame(
    County = paste(c("Austin", "Brazoria", "Chambers", "Fort Bend", "Galveston", "Harris", "Liberty", "Montgomery", "Waller"), "County, TX"),
    MSA = "Houston"
   )

paso <- data.frame(
    County = paste(c("El Paso", "Hudspeth"), "County, TX"),
    MSA = "El Paso"
    )

MSAs <- bind_rows(austin, dfw, houston, tonio, paso)

rm(dfw, austin, tonio, houston, paso)
