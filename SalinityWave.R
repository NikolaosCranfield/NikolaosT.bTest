new.data <- R_sta_month0

cells <- unique(new.data$sta)
sta_1E <- new.data[which(new.data$sta == cells[2]), ]

sta_1E$temp_mean <- na.approx(sta_1E$temp_mean)

temp_mean.wav = analyze.wavelet(
  sta_1E,
  "temp_mean",
  loess.span = 0,
  dt = 1,
  dj = 1 / 50,
  lowerPeriod = 2,
  upperPeriod = 160,
  make.pval = F,
  n.sim = 1000
)

wt.avg(temp_mean.wav)
wt.image(rainfall_mean.wav)
