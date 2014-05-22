files <- list.files('csv/')
xzfilesi <- grep('.csv.xz$', files)
xzfilepaths <- paste0('csv/', files[xzfilesi])
csvfilepaths <- gsub('.xz', '', xzfilepaths)

for (fi in 1:length(xzfilepaths)) {
  print(xzfilepaths[fi])
  write.csv(read.csv(xzfilepaths[fi]), file = csvfilepaths[fi])
}
