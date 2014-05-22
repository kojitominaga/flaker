require(flaker)
require(flakeutil)
## source('../../R/flake_interface.R')

firstday <- as.Date('2001-01-01')
lastday <- as.Date('2010-12-31')
dayseq <- seq(from = firstday, to = lastday, by = 'day')
dayseqP <- as.POSIXlt(dayseq)
ensemblename <- 'KNMI-RACMO2_A1B_ECHAM5-r3_DM_25km_2001-2010'
rootpath <- paste('../exampledata/originals/', ensemblename, '_', sep = '')
pr <- scan(paste(rootpath, 'pr.txt', sep = ''))
## rsds <- scan(paste(rootpath, 'rsds.txt', sep = ''))
rlds <- scan(paste(rootpath, 'rlds.txt', sep = ''))
wss <- scan(paste(rootpath, 'wss.txt', sep = ''))
## tas <- scan(paste(rootpath, 'tas.txt', sep = ''))
## tasmax <- scan(paste(rootpath, 'tasmax.txt', sep = ''))
## tasmin <- scan(paste(rootpath, 'tasmin.txt', sep = ''))
huss <- scan(paste(rootpath, 'huss.txt', sep = ''))
ps <- scan(paste(rootpath, 'ps.txt', sep = ''))
## rss <- scan(paste(rootpath, 'rss.txt', sep = ''))
## rls <- scan(paste(rootpath, 'rls.txt', sep = ''))
## hfss <- scan(paste(rootpath, 'hfss.txt', sep = ''))
## hfls <- scan(paste(rootpath, 'hfls.txt', sep = ''))

rootpath24 <- paste('../exampledata/derived/', ensemblename, '_', sep = '')
rsds24 <- scan(paste(rootpath24, 'rsds_24-hourized.txt', sep = ''))
tas24 <- scan(paste(rootpath24, 'tas_24-hourized.txt', sep = ''))

height_u <- 10
height_tq <- 2

n <- length(pr)
m <- 24
initialrepeat <- 3

ts.input0 <- data.frame(dMsnowdt_in = rep(pr, each = m),
                        I_atm_in = rsds24,
                        Q_atm_lw_in = rep(rlds, each = m),
                        height_u_in = rep(rep(height_u, times = n), each = m),
                        height_tq_in = rep(rep(height_tq, times = n), each = m),
                        U_a_in = rep(wss, each = m),
                        T_a_in = tas24,
                        q_a_in = rep(huss, each = m),
                        P_a_in = rep(ps, each = m)
                        )
ts.input <- ts.input0[c(rep(1:(m * 365), times = initialrepeat), 1:(n * m)), ]
initiali <- 1:(m * 365 * initialrepeat)

init.state <-
  c(0 + 273.15, 0 + 273.15, 4 + 273.15, 4 + 273.15, 4 + 273.15, 5 + 273.15,
    0.5, 0, 0, 38, 0.4, 4 + 273.15)
names(init.state) <-
  c('T_snow', 'T_ice', 'T_mnw', 'T_wML', 'T_bot', 'T_B1',
    'C_T', 'h_snow', 'h_ice', 'h_ML', 'H_B1', 'T_sfc')

parameters <- c(38, 2000, 0.5,
                mean(ts.input[['T_a_in']]),
                sin(60 / 180 * pi) * 2 * 7.29e-5,
                60 * 60 * 24 / m,
                0, 0.8, 0.8)
names(parameters) <-
  c('depth_w', 'fetch', 'depth_bs', 'T_bs', 'par_Coriolis', 'del_time',
    'albedo_water', 'albedo_ice', 'albedo_snow')


attens <- c(0.18, 0.9, 4.5)
fnameaffixes <- c('clear', 'medium_a', 'dark')
attennames <- paste(fnameaffixes, 'atten. coef. :', attens, 'm-1')
colours <- c('#F0DC82', '#CD853F', '#954535') # buff, peru, chestnut
result <- list()
dm <- list()
typicalyear <- list()

for (i in 1:length(attens)){
  atten <- attens[i]
  fnameaffix <- fnameaffixes[i]
  attenname <- attennames[i]

  optic <- c(atten, 20, 20)
  names(optic) <- paste('extincoef_', c('water', 'ice', 'snow'), sep = '')
  
  result[[i]] <- data.frame(ts.input,
                            runflake(ts.input, parameters, init.state, optic)
                            )[-initiali, ]
  
  print('... calculating daily mean')
  dm[[i]] <- apply(result[[i]], 2, tapply,
                    rep(1:(n * m), each = 24, length.out = nrow(result[[i]])),
                    mean)
  
  if (!file.exists('csv')) dir.create('csv')
  c <- xzfile(paste('csv/dm_', fnameaffix, '.csv.xz', sep = ''), open = 'wb')
  write.csv(data.frame(date = dayseq, signif(dm[[i]], digits = 4)), file = c)
  close(c)
  
  typicalyear[[i]] <- apply(dm[[i]], 2, tapply, dayseqP$yday, mean)[1:365, ]
  ## 1:365 makes it ignore 31 Dec in leap years
  if (!file.exists('csv')) dir.create('csv')
  c <- xzfile(paste('csv/typicalyear_', fnameaffix, '.csv.xz', sep = ''),
              open = 'wb')
  write.csv(data.frame(MonthDay = format(seq(from = as.Date('1999-01-01'),
                         to = as.Date('1999-12-31'),
                         by = 'day'), ## need any non-leap year
                         format = '%m-%d'),
                       signif(typicalyear[[i]], digits = 4)), 
            file = c)
  close(c)
  
  shortnames <- names(result)
  if (!file.exists('pdf')) dir.create('pdf')
  pdf(paste('pdf/allvariables_typicalyear_', fnameaffix, '.pdf', sep = ''), 
      height = 7.5, width = 11.5)
  par(mfcol = c(3, 3))
  par(mar = c(2, 3, 3, 1))
  for (vi in 1:ncol(typicalyear[[i]])) {
    plot(seq(from = as.Date('1999-01-01'),
             to = as.Date('1999-12-31'),
             by = 'day'),
         typicalyear[[i]][1:365, vi],
         type = 'l',
         xlab = 'month',
         ylab = '',
         main = fullnames[vi],
         cex.main = 1,
         xaxt = 'n')
    axis.Date(1,
              at = seq(from = as.Date('1999-01-01'),
                to = as.Date('1999-12-01'),
                by = 'month'),
              labels = c('J', 'F', 'M', 'A', 'M', 'J',
                'J', 'A', 'S', 'O', 'N', 'D')
              )
  }
  dev.off()
}

fnameaffix <- 'comparison_attens'
print(paste('pdf/allvariables_typicalyear_', fnameaffix, '.pdf', sep = ''))
pdf(paste('pdf/allvariables_typicalyear_', fnameaffix, '.pdf', sep = ''), 
    height = 7.5, width = 11.5)
par(mfcol = c(3, 3))
par(mar = c(2, 3, 3, 1))
for (vi in 1:ncol(typicalyear[[1]])) {
  plot(seq(from = as.Date('1999-01-01'),
           to = as.Date('1999-12-31'),
           by = 'day'),
       1:365, 
       type = 'n',
       xlab = 'month',
       ylab = '',
       main = fullnames[vi],
       cex.main = 1,
       xaxt = 'n',
       ylim = range(unlist(lapply(typicalyear, function(x) x[, vi])))
       )
  axis.Date(1,
            at = seq(from = as.Date('1999-01-01'),
                to = as.Date('1999-12-01'),
              by = 'month'),
              labels = c('J', 'F', 'M', 'A', 'M', 'J',
                'J', 'A', 'S', 'O', 'N', 'D')
            )
  for (i in 1:length(attens)) {
    lines(seq(from = as.Date('1999-01-01'),
              to = as.Date('1999-12-31'),
              by = 'day'),
          typicalyear[[i]][, vi],
          col = colours[i]
          )
  }
}
plot(0, 0, 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('topleft', legend = attennames, lty = 1, col = colours) 
dev.off()

