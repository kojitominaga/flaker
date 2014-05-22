require(flaker)
require(flakeutil)
## source('../../R/flake_interface.R')

firstday <- as.Date('2001-01-01')
lastday <- as.Date('2002-12-31')
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

n <- 365 * 1
m <- 24
initialrepeat <- 3

ts.input0 <- data.frame(dMsnowdt_in = rep(pr[1:n], each = m),
                        I_atm_in = rsds24[1:(n * 24)],
                        Q_atm_lw_in = rep(rlds[1:n], each = m),
                        height_u_in = rep(rep(height_u, times = n), each = m),
                        height_tq_in = rep(rep(height_tq, times = n), each = m),
                        U_a_in = rep(wss[1:n], each = m),
                        T_a_in = tas24[1:(n * 24)],
                        q_a_in = rep(huss[1:n], each = m),
                        P_a_in = rep(ps[1:n], each = m)
                        )
ts.input <- ts.input0[c(rep(1:(m * 365), times = initialrepeat), 1:(n * m)), ]
initiali <- 1:(m * 365 * initialrepeat)

init.state <-
  c(0 + 273.15, 0 + 273.15, 4 + 273.15, 4 + 273.15, 4 + 273.15, 5 + 273.15,
    0.5, 0, 0, 38, 0.4, 4 + 273.15)
names(init.state) <-
  c('T_snow', 'T_ice', 'T_mnw', 'T_wML', 'T_bot', 'T_B1',
    'C_T', 'h_snow', 'h_ice', 'h_ML', 'H_B1', 'T_sfc')

optic <- c(2, 20, 20)
names(optic) <- paste('extincoef_', c('water', 'ice', 'snow'), sep = '')


depths <- 1:40
fnameaffixes <- sprintf('depth_%03d_m', depths)
depthnames <- paste(fnameaffixes, ':', depths, 'm')
result <- list()
dm <- list()
typicalyear <- list()

for (i in 1:length(depths)){
  depth <- depths[i]
  fnameaffix <- fnameaffixes[i]
  depthname <- depthnames[i]

  parameters <- c(depth, 10000, 0.5,
                  mean(ts.input[['T_a_in']]),
                  sin(60 / 180 * pi) * 2 * 7.29e-5,
                  60 * 60 * 24 / m,
                  0, 0.8, 0.8)
  names(parameters) <-
    c('depth_w', 'fetch', 'depth_bs', 'T_bs', 'par_Coriolis', 'del_time',
      'albedo_water', 'albedo_ice', 'albedo_snow')
  
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
}
  
temp <- unlist(lapply(dm, function(x) data.frame(x)[['T_sfc']][180]))
plot(temp)
ra2 <- range(unlist(lapply(dm, function(x) data.frame(x)[['T_sfc']])))
plot(0, 0, type = 'n', ylim = c(0, 20), xlim = c(1, 365))
for (i in 1:length(dm)) {
  lines(data.frame(dm[[i]])[['T_sfc']] - 273.15,
        col = rainbow(length(dm))[i])
}



