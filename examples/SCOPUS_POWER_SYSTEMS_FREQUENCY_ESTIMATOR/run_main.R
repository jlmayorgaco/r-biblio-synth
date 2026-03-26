# Run main.r with new architecture
.libPaths('C:/Users/walla/Documents/R/win-library/4.5')
library(bibliometrix)
for (f in list.files('../../R/', pattern = '\\.R$', recursive = TRUE, full.names = TRUE)) source(f)
source('main.r')
