require(flakeutil)
fnamesuffixes <- c('shallow', 'medium_d', 'deep',
                   'small', 'medium_f', 'large',
                   'clear', 'medium_a', 'dark')
colours <- c('lightblue', 'blue', 'darkblue',
             'pink', 'red', '#DC143C', # crimson
             '#F0DC82', '#CD853F', '#954535') # buff, peru, chestnut
detailednames <- paste(c(rep('depth', times = 3),
                         rep('fetch', times = 3),
                         rep('atten', times = 3)),
                       fnamesuffixes,
                       c(2, 10, 40, 400, 2000, 10000, 0.18, 0.9, 4.5),
                       c(rep('m', times = 3),
                         rep('m', times = 3),
                         rep('m-1', times = 3))
                       )
fnames <- paste('csv/typicalyear_', fnamesuffixes, '.csv.xz', sep = '')
data <- lapply(as.list(1:length(fnames)), function(i) read.csv(fnames[i]))
names(data) <- fnamesuffixes


fnameaffix <- 'summary_comparisons'
print(paste('pdf/selected_variables_typicalyear_', fnameaffix, '.pdf', sep = ''))
pdf(paste('pdf/selected_variables_typicalyear_', fnameaffix, '.pdf', sep = ''), 
    height = 7.5, width = 11.5)
par(mfcol = c(3, 3))
par(mar = c(2, 3, 3, 1))
vii <- c(12, 13, 14, 21, 17, 18, 19, 28)
nameaddition <-
  c(rep('degree C', times = 4),
    '', '', 'inverted',
    'net heat exchange at the top of water (negative means cooling)')
rangeii <- c(1, 1, 1, 2, 3, 3, 4, 5)
rangelist <-
  list(range(unlist(lapply(data, function(x) x[ , 2 + vii[rangeii %in% 1]]))),
       range(unlist(lapply(data, function(x) x[ , 2 + vii[rangeii %in% 2]]))),
       range(unlist(lapply(data, function(x) x[ , 2 + vii[rangeii %in% 3]]))),
       range(unlist(lapply(data, function(x) x[ , 2 + vii[rangeii %in% 4]]))),
       range(unlist(lapply(data, function(x) x[ , 2 + vii[rangeii %in% 5]]))))
rangelist[[4]] <- rev(rangelist[[4]])
comparisons <- list(1:3, 4:6, 7:9)
for (comparisoni in 1:length(comparisons)) {
  comparison <- comparisons[[comparisoni]]
  for (vij in 1:length(vii)) {
    vi <- vii[vij]
    plot(seq(from = as.Date('1999-01-01'),
             to = as.Date('1999-12-31'),
             by = 'day'),
         1:365, 
         type = 'n',
         xlab = 'month',
         ylab = '',
         main = paste(gsub('Output:\n', '', fullnames[vi]),
           '\n', nameaddition[vij]),
         cex.main = 1,
         xaxt = 'n',
         ylim = rangelist[[rangeii[vij]]]
                + ifelse(rangeii[vij] <= 2, -273.15, 0)
         )
    axis.Date(1,
              at = seq(from = as.Date('1999-01-01'),
                to = as.Date('1999-12-01'),
                by = 'month'),
              labels = c('J', 'F', 'M', 'A', 'M', 'J',
                'J', 'A', 'S', 'O', 'N', 'D')
              )
    abline(h = 0, col = 'gray')
    for (ki in 1:length(comparison)) {
      k <- comparison[ki]
      lines(seq(from = as.Date('1999-01-01'),
                to = as.Date('1999-12-31'),
                by = 'day'),
            data[[k]][, 2 + vi] + ifelse(rangeii[vij] <= 2, -273.15, 0),
            col = colours[k]
            )
    }
  }
  plot(0, 0, 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('topleft', legend = detailednames[comparison],
         lty = 1, col = colours[comparison]) 
}
dev.off()
