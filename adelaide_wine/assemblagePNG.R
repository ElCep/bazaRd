##script for assemble png with R
setwd("~/Documents/adelaide_wine_map/")
system("montage -geometry +2+2 wine_in_world_2000.png wine_in_world_2010.png assemblage/montage_world_wine.jpg")
system("montage -geometry +2+2 Cabernet_Franc_2000.png Cabernet_Franc_2010.png assemblage/montage_Cabernet_Franc.jpg")
system("montage -geometry +2+2 Cabernet_Sauvignon_2000.png Cabernet_Sauvignon_2010.png assemblage/montage_Cabernet_Sauv.jpg")
