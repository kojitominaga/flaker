source('../../R/hourlyairtemperature.R')
source('../../R/fractionradiation.R')

ensemblename <- 'KNMI-RACMO2_A1B_ECHAM5-r3_DM_25km_2001-2010'
rootpath <- paste('../exampledata/originals/', ensemblename, '_', sep = '')
firstday <- as.Date('2001-01-01')
lastday <- as.Date('2010-12-31')
dayseq <- seq(from = firstday, to = lastday, by = 'day')
lat <- 59.372066
lon <- 10.848092

rsds <- scan(paste(rootpath, 'rsds.txt', sep = ''))
tas <- scan(paste(rootpath, 'tas.txt', sep = ''))
tasmax <- scan(paste(rootpath, 'tasmax.txt', sep = ''))
tasmin <- scan(paste(rootpath, 'tasmin.txt', sep = ''))

n <- length(rsds)
m <- 24

## solar radiation (sw) 24-hourize
iatmin <- numeric()
for (dayi in 1:length(dayseq)) {
  if (dayi %% 100 == 0) {
    cat(paste('... making solar radiation of day', dayi, ':',
              round(dayi / n * 100),
              'percent done\n'))
  }
  day <- dayseq[dayi]
  iatmin[1:m + (dayi - 1) * m] <-
    fractionradiation(day, lat, lon, m) * rsds[dayi] * m
}
## air temperature 24-hourzize
tain <- numeric()
previousT <- tas[1]
for (dayi in 1:length(dayseq)) {
  if (dayi %% 100 == 0) {
    cat(paste('... making temperature of day', dayi, ':',
              round(dayi / n * 100),
              'percent done\n'))
  }
  day <- dayseq[dayi]
  if (dayi == length(dayseq)) dayi2 <- dayi - 1 else dayi2 <- dayi
  temp <- hourlyairtemperature(day, lat, lon, previousT,
                               tasmin[dayi2], tasmax[dayi2], tas[dayi2],
                               tasmin[dayi2 + 1], tasmax[dayi2 + 1], m)
  tain[1:m + (dayi - 1) * m] <- temp
  previousT <- temp[24]
}

cat(signif(iatmin, digits = 4),
    file = paste('../exampledata/derived/', ensemblename,
      '_rsds_24-hourized.txt', sep = ''),
    sep = '\n')
cat(signif(tain, digits = 4),
    file = paste('../exampledata/derived/', ensemblename,
      '_tas_24-hourized.txt', sep = ''),
    sep = '\n')



