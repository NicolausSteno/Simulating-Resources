library(deSolve)
# Load model, parameters and data ------------------------------------------------
comparison_data <- structure(
  list(
    Year = c(
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 
      1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 
      1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
      2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022
    ), 
    `Real Prices (Cortez et al., 2018) $/Tg billion` = c(
      4.05430100157994, 4.12043968023541, 4.13587203858835, 6.19498956739513, 8.06450955072291,
      9.30130284158009, 6.82110239200019, 7.33918870813466, 8.32685964272292, 7.76027162890778, 
      5.76288353351276, 5.44321325334469, 7.94545992914308, 7.90798134457165, 4.27696788638667, 
      4.63191212850433, 4.08075647304213, 3.93745600262196, 5.09488287909258, 4.93394542769761, 
      3.59573949623539, 2.99387752047067, 3.18127044332782, 2.68743497603369, 2.77782450352949,
      2.77341525828579, 3.49873610087405, 4.91410382410098, 5.13236146366401, 4.62750288326063, 
      4.05430100157994, 3.93525138000011, 3.25402298984883, 3.86911270134464, 4.75757561794971, 
      3.63542270342867, 3.60676260934464, 2.68743497603369, 2.53090676988242, 2.76239214517655, 
      2.37437856373116, 2.40524328043704, 2.60365931640343, 3.9484791157312, 4.71348316551273,
      8.24087936047082, 8.32685964272292, 7.35682568910945, 5.99877815405059, 8.23867473784897, 
      8.87581067556327, 7.94986917438677, 7.28186851996659, 6.75496371334472, 5.83563608003377, 
      5.68037007769431, 5.93035232499751, 6.18827100304985, 6.12367642382515, 5.95615338557968, 
      6.96640892754328, NA
    ), 
    `Real Prices (World Bank) $/Tg billion` = c(
      3.24406229750676, 3.24529921770188, 3.30999392307195, 4.90064495158511, 6.47113483169738,
      7.39318318121391, 5.43023912454796, 5.99536558710986, 6.72344971662373, 6.09831566465937,
      4.42937629378807, 3.97134606875211, 5.78036541991601, 5.4689247746114, 2.95374636834702,
      3.31021629913642, 2.86113772997592, 2.5675164398153, 3.34789915159254, 3.34551666519448,
      2.66765839921454, 2.33677898884761, 2.58119782921292, 2.28347874367683, 2.37466930175288,
      2.0011187459536, 2.36939791414476, 3.24749629574441, 3.5769155243051, 3.21823760580411,
      2.85565628815629, 2.73520083932854, 2.21677481653148, 2.75677120669056, 3.19434802321364,
      2.54418699186992, 2.65048506014746, 2.01223641524736, 1.95144023986766, 2.27822759631491,
      2.06042863359443, 2.06007595772787, 2.23510657453936, 3.37162931372549, 4.19484198023565,
      7.47734649610678, 7.46145283018868, 6.76642007133593, 5.33651666666667, 7.53477958333333,
      7.95332207207207, 7.22536031108826, 6.68377436416087, 6.34325092421442, 5.62865849506299,
      5.17861436170213,6.34115107913669, 6.40804546941446, 6.04034673366834, 6.30620105549881,
      8.51649908592322, 7.99127339976 
    ), 
    `Extraction (Tg/a)` = c(
      3.979, 4.11, 4.179, 4.331, 4.531, 4.51, 4.755, 4.891, 5.568, 5.671, 5.958, 6.391, 6.58, 6.758, 6.841, 
      6.937, 6.937, 7.143, 7.206, 7.064, 7.327, 7.552, 7.669, 7.72, 7.819, 8.079, 8.397, 8.575, 9.163,
      9.061, 8.921, 9.376, 9.381, 9.906, 9.906, 10.903, 11.34, 12.046, 12.562, 12.989, 13.532, 13.358, 13.419,
      14.12, 14.682, 14.75, 15.218, 15.288, 15.631, 16.1, 16.295, 16.711, 18.272, 18.488, 19.133, 20.356,
      20.238, 20.626, 20.552, 20.576, NA, NA
    ), 
    `Recycling (Tg/a)` = c(
      2.3, NA, NA, NA, 3, NA, NA, NA, NA, 3.5, NA, NA, NA, NA, 3.75, NA, NA, NA, NA, 4.75, NA, NA,
      NA, NA, 4.5, NA, NA, NA, NA, 5.1, NA, NA, NA, NA, 6.5, NA, NA, NA, NA, 7, NA, NA, NA, NA, 7.5,
      NA, NA, NA, NA, 8.75, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), 
    `S4 (Tg)` = c(
      75, NA, NA, NA, 90, NA, NA, NA, NA, 100, NA, NA, NA, NA, 125, NA, NA, NA, NA, 150, NA, NA, NA,
      NA, 175, NA, NA, NA, NA, 200, NA, NA, NA, NA, 225, NA, NA, NA, NA, 275, NA, NA, NA, NA, 300, NA,
      NA, NA, NA, 350, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), 
    `S8 (Tg)` = c(
      40, NA, NA, NA, 45, NA, NA, NA, NA, 50, NA, NA, NA, NA, 55, NA, NA, NA, NA, 60, NA, NA, NA, NA, 70,
      NA, NA, NA, NA, 75, NA, NA, NA, NA, 90, NA, NA, NA, NA, 100, NA, NA, NA, NA, 125, NA, NA, NA, NA,
      140, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), 
    `F9 (Tg/a)` = c(
      2, NA, NA, NA, 2.1, NA, NA, NA, NA, 2.5, NA, NA, NA, NA, 3, NA, NA, NA, NA, 4, NA, NA, NA, NA,
      4.75, NA, NA, NA, NA, 5.9, NA, NA, NA, NA, 6.75, NA, NA, NA, NA, 8, NA, NA, NA, NA, 9.25, NA, NA,
      NA, NA, 11, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    ), 
    `F2 (Tg/a)` = c(
      5.127, 5.296, 5.4, 5.739, 6.059, 6.004, 6.324, 6.653, 7.137, 7.212, 7.592, 8.1, 8.187, 8.544,
      8.4, 8.759, 8.884, 9.03, 9.2, 9.261, 9.319, 9.573, 9.541, 9.44, 9.455, 9.92, 10.148, 10.512, 10.687,
      10.804, 10.908, 11.045, 11.124, 11.239, 11.832, 12.677, 13.478, 14.075, 14.578, 14.796, 15.273,
      15.354, 15.638, 15.928, 16.573, 17.295, 17.944, 18.2, 18.356, 18.966, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA
    ), 
    `F3 (Tg/a)` = c(
      1, NA, NA, NA, 1.25, NA, NA, NA, NA, 1.5, NA, NA, NA, NA, 1.6, NA, NA, NA, NA, 1.75, NA, NA, NA, NA,
      1.5, NA, NA, NA, NA, 2.25, NA, NA, NA, NA, 2.75, NA, NA, NA, NA, 3, NA, NA, NA, NA, 3.1, NA, NA, NA,
      NA, 3.6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    )), 
  row.names = c(NA,-62L), class = "data.frame")

dmmcm <- function(parms, times = seq(0, 61, 1), rtol = 1e-6, atol = 1e-6, method = "lsoda",
                  control = list(mxhist = 1e6), funGDP = funGDP) {
  derivs <- function(t, y, parms) {
    with(as.list(c(y, parms)),{
      #S = stocks
      b <- c(Extraction, (1-gamma)*Recycling, gamma*Recycling, 0, -Recycling, 0, 0, 0)
      s = c(s1, s2, s3, s4, s5, s6, s7, s8)
      U[9,4] <- alpha_9
      U[11,5] <- alpha_11
      ds <- b + G %*% U %*% s
      
      if (t <= tau_e) {
        pricelag_e <- Price
        dpricelag_e = 0
      } else {
        pricelag_e <- lagvalue(t - tau_e, 1)
        dpricelag_e <- lagderiv(t - tau_e, 1) 
      }
      if (t <= tau_r) {
        pricelag_r <- Price
        dpricelag_r = 0
      } else {
        pricelag_r <- lagvalue(t - tau_r, 1)
        dpricelag_r <- lagderiv(t - tau_r, 1)
      }
      dPrice = c*Price*((Demand-(Extraction+Recycling))/Demand) + delta
      dDemand = funGDP(t) * Demand * (i/Price) 
      if (dpricelag_e > 0)
        dExtraction = Extraction * (dpricelag_e/pricelag_e) * k_e 
      else
        dExtraction = z_e * dDemand * k_e
      if (dpricelag_r > 0)
        dRecycling = Recycling*(dpricelag_r/pricelag_r)*k_r + Recycling*n*(ds[5]/s5)
      else
        dRecycling = q*s5^n*dDemand + Recycling*n*(ds[5]/s5)
      
      return(list(c(dPrice, dDemand, dExtraction, dRecycling, ds)))
    })}
  y <- c(Price = 3.24406229750676, Demand = 6.279, Extraction = 3.979, 
         Recycling = 2.3, s1 = 0.5, s2 = 0.5, s3 = 0.5, s4 = 75, s5 = 0.5, 
         s6 = 50, s7 = 20, s8 = 40)
  return(dede(y = y, times = times, func = derivs, parms = parms, rtol = rtol,
              atol = atol, method = method, control = control))
}

G <- structure(c(-1, 1, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0,
                 0, 1, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0,
                 0, -1, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, -1, 0,
                 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, -1, 0,
                 1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, -1,
                 0, 0, 1, 0, -1, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0,
                 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0,
                 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0,
                 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1), dim = c(8L, 22L))
#flow parameters
U <- structure(c(9.6, 0, 0, 1.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01,
                 0, 0, 0, 0, 0, 0, 0, 0, 11.2, 0, 0, 0, 0.333333333333333, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0,
                 0, 0, 0, 11.8, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0.045, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.8, 0, 0, 0, 0, 0, 0,
                 0, 0.01, 0, 0, 0, 0, 0, 0, 0, 0.006, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0.01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01), dim = c(22L, 8L))
#forcing function based on GDP growth from World Bank
funGDP <- approxfun(x = c(0:61),
                    y = c(3.789978749,5.316037452,5.185396143,6.558284554,5.549130548,5.712633241,
                          4.152581514,5.942285729,5.814900551,3.968558097,4.276266213,5.616072402,
                          6.407433514,1.79539372,0.633871866,5.303194797,4.099478638,4.137054321,
                          4.175807584,1.877168312,1.93214366,0.393710033,2.64911332,4.674492235,
                          3.698606684,3.442866381,3.73217674,4.638280031,3.754229767,2.865937731,
                          1.459804928,2.069766183,1.808161085,3.305388336,3.092063238,3.593206788,
                          3.8777349,2.819175366,3.55283987,4.51495667,2.009203206,2.304827672,
                          3.109626536,4.471240675,4.004597144,4.4204254,4.381900771,2.069175771,
                          -1.341448145,4.540838287,3.310323684,2.70840268,2.807892132,3.090647561,
                          3.077577537,2.805257996,3.38624182,3.286864016,2.591419861,-3.114741981,
                          5.874225293, 3.080321778)/100,
                    rule = 2)

# parms <- list( #'reference case 2'
#   G = G,
#   U = U,
#   alpha_9 = 0.04,
#   alpha_11 = 1.8,
#   gamma = 1/3,
#   tau_e = 8,
#   tau_r = 2,
#   z_e = 0.85,
#   c = 1.9,
#   i = 3.1,
#   k_e = 0.45,
#   k_r = 0.45,
#   n = 0.15,
#   q = 0.03,
#   delta = 0.5)

parms <- list( #'reference case 1'
  G = G,
  U = U,
  alpha_9 = 0.049,
  alpha_11 = 1.8,
  gamma = 1/3,
  tau_e = 8,
  tau_r = 2,
  z_e = 0.85,
  c = 1.6,
  i = 3.1,
  k_e = 0.60,
  k_r = 0.55,
  n = 0.18,
  q = 0.033,
  delta = 0.53)

# Run,  plot and compare with data ----------------------------------------
out <- dmmcm(parms, funGDP = funGDP)

#compare prices
par(mfrow=c(1,1), 
    mar = c(5, 7, 0.1, 0.1), 
    cex.lab = 2.5, cex.axis = 2)
plot(comparison_data[["Year"]], 
     comparison_data[["Real Prices (World Bank) $/Tg billion"]],
     type = "l", 
     ylim=c(0,max(na.omit(comparison_data[["Real Prices (Cortez et al., 2018) $/Tg billion"]]),
                  data.frame(out)[["Price"]])),
     #     main = "Inflation-adjusted Historical Copper Prices",
     xlab = "",
     ylab = "",
     cex.main = 2.5, lwd = 3)
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = "Prices (billion $/Tg)", 
      mgp = c(5, 1, 0))
lines(comparison_data[["Year"]], 
      comparison_data[["Real Prices (Cortez et al., 2018) $/Tg billion"]], 
      col="black", lwd = 3, lty = "dashed")
lines(comparison_data[["Year"]], 
      data.frame(out)[["Price"]], 
      lwd = 5, lty = 1, col="red")
legend(1980, 9.5, legend=c("World Bank", "Cortez et al. (2018)", "Model Output"),
       col=c("black", "black", "red"), lty = c(1, 2, 1),
       cex=2, lwd = 3)

#compare supply flows
#compare extraction
par(mfrow=c(1,2), 
    mar = c(5, 7, 0.1, 2.5), 
    cex.lab = 2.5, cex.axis = 2)
plot(comparison_data[["Year"]], 
     comparison_data[["Extraction (Tg/a)"]],
     type = "l", 
     ylim=c(3,max(na.omit(comparison_data[["Extraction (Tg/a)"]]),
                  data.frame(out)[["Extraction"]])),
     xlab = "",
     ylab = "",
     cex.main = 2.5, lwd = 3)
#     main = "Global Copper Mining Rates",
#     xlab =  "Year",
#     ylab = "Rate of Extraction (Tg/a)")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = "Rate of Extraction (Tg/a)", 
      mgp = c(4, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out)[["Extraction"]], 
      lwd = 4, lty = 1, col="red")
legend(1962, 20.5, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty = 1, cex = 2, lwd=3)
#compare recycling
#par(mar = c(5, 5, 0.1, 0.1))
plot(comparison_data[["Year"]], 
     data.frame(out)[["Recycling"]],
     type = "l", col="red",
     ylim = c(2,max(data.frame(out)[["Recycling"]],
                    na.omit(comparison_data[["Recycling (Tg/a)"]]))),
     xlab = "",
     ylab = "",
     cex.main = 2.5, lwd = 4)
#     main = "Global Copper Recycling Rates",
#     xlab = "Year",
#     ylab = "Rate of Recycling (Tg/a)")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = "Rate of Recycling (Tg/a)", 
      mgp = c(4, 1, 0))
points(comparison_data[["Year"]], 
       comparison_data[["Recycling (Tg/a)"]], 
       pch = 17, cex = 2)
legend(1962, 9.5, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty=c(NA, 1), pch=c(17, NA), cex = 2, lwd=3)

##compare stocks
#compare s4
par(mfrow=c(1,2), 
    mar = c(5, 7, 0.1, 2.5), 
    cex.lab = 2.5, cex.axis = 2)
plot(comparison_data[["Year"]], 
     data.frame(out)[["s4"]],
     type = "l", col="red", lwd=4,
     ylim=c(50,max(na.omit(comparison_data[["S4 (Tg)"]]), 
                   data.frame(out)[["s4"]])),
     xlab = "",
     ylab = "")
#     main = "Global In-Use Copper",
#     xlab = "Year",
#     ylab = "S4 (Tg)")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = expression(italic("s"[italic("4   ")]) (Tg)), 
      mgp = c(4, 1, 0))
points(comparison_data[["Year"]], 
       comparison_data[["S4 (Tg)"]], 
       pch = 17, cex = 2)
legend(1960, 360, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty=c(NA, 1), pch=c(17, NA), cex = 2, lwd=3)
#compare s8
plot(comparison_data[["Year"]], 
     data.frame(out)[["s8"]],
     type = "l", col="red", lwd=4,
     ylim=c(20,max(na.omit(comparison_data[["S8 (Tg)"]]), 
                   data.frame(out)[["s8"]])),
     xlab = "",
     ylab = "")
#     main = "Global Landfilled Copper",
#     xlab = "Year",
#     ylab = "S8 (Tg)")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = expression(italic("s"[italic("8   ")]) (Tg)), 
      mgp = c(4, 1, 0))
points(comparison_data[["Year"]], 
       comparison_data[["S8 (Tg)"]], 
       pch = 17, cex = 2)
legend(1960, 190, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty=c(NA, 1), pch=c(17, NA), cex = 2, lwd=3)

#compare flows
#f9
par(mfrow=c(1,2), 
    mar = c(5, 7, 0.1, 2.5), 
    cex.lab = 2.5, cex.axis = 2)
plot(comparison_data[["Year"]],
     (parms[["alpha_9"]]*data.frame(out)[["s4"]]),
     type = "l", col="red", lwd=4,
     ylim=c(1,max(na.omit(comparison_data[["F9 (Tg/a)"]]),
                  (parms[["alpha_9"]]*data.frame(out)[["s4"]]))),
     #     main = "F9 (Tg/a)",
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = expression(italic("f"[italic("9    ")]) (Tg/a)), 
      mgp = c(4, 1, 0))
points(comparison_data[["Year"]],
       comparison_data[["F9 (Tg/a)"]],
       pch = 17, cex = 2)
legend(1962, 17, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty=c(NA, 1), pch=c(17, NA), cex = 2, lwd=2)
#f2
plot(comparison_data[["Year"]],
     (parms[["U"]][2,2]*data.frame(out)[["s2"]]),
     type = "l", col="red", lwd=4,
     ylim=c(3,max(na.omit(comparison_data[["F2 (Tg/a)"]]),
                  (parms[["U"]][2,2]*data.frame(out)[["s2"]]))),
     #     main = "F2 (Tg/a)",
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(4, 1, 0))
title(ylab = expression(italic("f"[italic("2    ")]) (Tg/a)), 
      mgp = c(4, 1, 0))
lines(comparison_data[["Year"]],
      comparison_data[["F2 (Tg/a)"]],
      lwd=2)
legend(1962, 25, legend = c("Glöser et al. (2013)", "Model Output"),
       col=c("black", "red"), lty = 1, cex = 2, lwd=2)


# Impact of Reducing Extraction Delays ---------------------------------------------

parms_low <- list( #low delay
  G = G,
  U = U,
  alpha_9 = 0.049,
  alpha_11 = 1.8,
  gamma = 1/3,
  tau_e = 2,
  tau_r = 2,
  z_e = 0.85,
  c = 1.6,
  i = 3.1,
  k_e = 0.60,
  k_r = 0.55,
  n = 0.18,
  q = 0.033,
  delta = 0.53)

parms_midlow <- list( #medium delay
  G = G,
  U = U,
  alpha_9 = 0.049,
  alpha_11 = 1.8,
  gamma = 1/3,
  tau_e = 5,
  tau_r = 2,
  z_e = 0.85,
  c = 1.6,
  i = 3.1,
  k_e = 0.60,
  k_r = 0.55,
  n = 0.18,
  q = 0.033,
  delta = 0.53)

parms_high <- list( #high delay
  G = G,
  U = U,
  alpha_9 = 0.049,
  alpha_11 = 1.8,
  gamma = 1/3,
  tau_e = 32,
  tau_r = 2,
  z_e = 0.85,
  c = 1.6,
  i = 3.1,
  k_e = 0.60,
  k_r = 0.55,
  n = 0.18,
  q = 0.033,
  delta = 0.53)

parms_midhigh <- list( #high delay
  G = G,
  U = U,
  alpha_9 = 0.049,
  alpha_11 = 1.8,
  gamma = 1/3,
  tau_e = 16,
  tau_r = 2,
  z_e = 0.85,
  c = 1.6,
  i = 3.1,
  k_e = 0.60,
  k_r = 0.55,
  n = 0.18,
  q = 0.033,
  delta = 0.53)

out_low <- dmmcm(parms_low, funGDP = funGDP)
out_midlow <- dmmcm(parms_mid, funGDP = funGDP)
out_high <- dmmcm(parms_high, funGDP = funGDP)
out_midhigh <- dmmcm(parms_midhigh, funGDP = funGDP)

#compare prices
par(mfrow=c(2,1), 
    mar = c(4, 5, 0.1, 0.1), 
    cex.lab = 2, cex.axis = 1.5)
plot(comparison_data[["Year"]], 
     data.frame(out)[["Price"]],
     type = "l", col = "red",
     ylim=c(0,max(data.frame(out_low)[["Price"]],
                  data.frame(out_midlow)[["Price"]],
                  data.frame(out)[["Price"]])),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 5)
title(ylab = "Prices (billion $/Tg)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midlow)[["Price"]], 
      col="black", lwd = 5, lty = "dashed")
lines(comparison_data[["Year"]], 
      data.frame(out_low)[["Price"]], 
      lwd = 5, lty = 1, col="black")
legend(1962, 2.9, legend=c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                         as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                         as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1),
       cex=1.5, lwd = 3)
plot(comparison_data[["Year"]], 
     data.frame(out)[["Price"]],
     type = "l", col = "red",
     ylim=c(0,max(data.frame(out_high)[["Price"]],
                  data.frame(out_midhigh)[["Price"]],
                  data.frame(out)[["Price"]])),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 5)
title(xlab = "Year",
      mgp = c(3, 1, 0))
title(ylab = "Prices (billion $/Tg)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midhigh)[["Price"]], 
      col="black", lwd = 5, lty = "dashed")
lines(comparison_data[["Year"]], 
      data.frame(out_high)[["Price"]], 
      lwd = 5, lty = 1, col="black")
legend(1962, 2.9, legend=c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                         as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                         as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1),
       cex=1.5, lwd = 3)

#compare supply flows
#compare extraction --low
par(mfrow=c(2,2), 
    mar = c(4, 7, 0.1, 0.1), 
    cex.lab = 2, cex.axis = 1.5)
plot(comparison_data[["Year"]], 
     data.frame(out)[["Extraction"]],
     type = "l", col = "red",
     ylim=c(3,max(data.frame(out)[["Extraction"]],
                  data.frame(out_low)[["Extraction"]],
                  data.frame(out_midlow)[["Extraction"]])),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 4)
title(ylab = "Rate of Extraction (Tg/a)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midlow)[["Extraction"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_low)[["Extraction"]], 
      lwd = 4, lty = 1, col="black")
legend(1962, 20.5, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                              as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                              as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare recycling --low
plot(comparison_data[["Year"]], 
     data.frame(out)[["Recycling"]],
     type = "l", col="red",
     ylim = c(2,max(max(data.frame(out)[["Recycling"]],
                        data.frame(out_low)[["Recycling"]],
                        data.frame(out_midlow)[["Recycling"]]))),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 4)
title(ylab = "Rate of Recycling (Tg/a)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midlow)[["Recycling"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_low)[["Recycling"]], 
      lwd = 4, lty = 1, col="black")
legend(1962, 9.5, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare extraction --high
plot(comparison_data[["Year"]], 
     data.frame(out)[["Extraction"]],
     type = "l", col = "red",
     ylim=c(3,max(na.omit(comparison_data[["Extraction (Tg/a)"]]),
                  data.frame(out)[["Extraction"]],
                  data.frame(out_high)[["Extraction"]],
                  data.frame(out_midhigh)[["Extraction"]])),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 4)
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = "Rate of Extraction (Tg/a)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midhigh)[["Extraction"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_high)[["Extraction"]], 
      lwd = 4, lty = 1, col="black")
legend(1962, 20.5, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                              as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                              as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare recycling --high
plot(comparison_data[["Year"]], 
     data.frame(out)[["Recycling"]],
     type = "l", col="red",
     ylim = c(2,max(max(data.frame(out)[["Recycling"]],
                        data.frame(out_high)[["Recycling"]],
                        data.frame(out_midhigh)[["Recycling"]]))),
     xlab = "",
     ylab = "",
     cex.main = 1.5, lwd = 4)
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = "Rate of Recycling (Tg/a)", 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midhigh)[["Recycling"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_high)[["Recycling"]], 
      lwd = 4, lty = 1, col="black")
legend(1962, 11.5, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)

##compare stocks
#compare s4 --low
par(mfrow=c(2,2), 
    mar = c(4, 7, 0.1, 0.1), 
    cex.lab = 2, cex.axis = 1.5)
plot(comparison_data[["Year"]], 
     data.frame(out)[["s4"]],
     type = "l", col="red", lwd=4,
     ylim=c(50,max(data.frame(out_low)[["s4"]], 
                   data.frame(out_midlow)[["s4"]],
                   data.frame(out)[["s4"]])),
     xlab = "",
     ylab = "")
title(ylab = expression(italic("s"[italic("4   ")]) (Tg)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midlow)[["s4"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_low)[["s4"]], 
      lwd = 4, lty = 1, col="black")
legend(1960, 360, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare s8 --low
plot(comparison_data[["Year"]], 
     data.frame(out)[["s8"]],
     type = "l", col="red", lwd=4,
     ylim=c(20,max(data.frame(out_low)[["s8"]],
                   data.frame(out_midlow)[["s8"]], 
                   data.frame(out)[["s8"]])),
     xlab = "",
     ylab = "")
title(ylab = expression(italic("s"[italic("8   ")]) (Tg)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midlow)[["s8"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_low)[["s8"]], 
      lwd = 4, lty = 1, col="black")
legend(1960, 190, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare s4 --high
plot(comparison_data[["Year"]], 
     data.frame(out)[["s4"]],
     type = "l", col="red", lwd=4,
     ylim=c(50,max(data.frame(out_high)[["s4"]], 
                   data.frame(out_midhigh)[["s4"]],
                   data.frame(out)[["s4"]])),
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = expression(italic("s"[italic("4   ")]) (Tg)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midhigh)[["s4"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_high)[["s4"]], 
      lwd = 4, lty = 1, col="black")
legend(1960, 360, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#compare s8 --high
plot(comparison_data[["Year"]], 
     data.frame(out)[["s8"]],
     type = "l", col="red", lwd=4,
     ylim=c(20,max(data.frame(out_high)[["s8"]],
                   data.frame(out_midhigh)[["s8"]], 
                   data.frame(out)[["s8"]])),
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = expression(italic("s"[italic("8   ")]) (Tg)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      data.frame(out_midhigh)[["s8"]], 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      data.frame(out_high)[["s8"]], 
      lwd = 4, lty = 1, col="black")
legend(1960, 190, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                             as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)

#compare flows
#f9 --low
par(mfrow=c(2,2), 
    mar = c(4, 7, 0.1, 0.1), 
    cex.lab = 2, cex.axis = 1.5)
plot(comparison_data[["Year"]],
     (parms[["alpha_9"]]*data.frame(out)[["s4"]]),
     type = "l", col="red", lwd=4,
     ylim=c(1,max((parms[["alpha_9"]]*data.frame(out_low)[["s4"]]),
                  (parms[["alpha_9"]]*data.frame(out_midlow)[["s4"]]),
                  (parms[["alpha_9"]]*data.frame(out)[["s4"]]))),
     xlab = "",
     ylab = "")
title(ylab = expression(italic("f"[italic("9    ")]) (Tg/a)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      (parms[["alpha_9"]]*data.frame(out_midlow)[["s4"]]), 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      (parms[["alpha_9"]]*data.frame(out_low)[["s4"]]), 
      lwd = 4, lty = 1, col="black")
legend(1962, 17, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#f2 --low
plot(comparison_data[["Year"]],
     (parms[["U"]][2,2]*data.frame(out)[["s2"]]),
     type = "l", col="red", lwd=4,
     ylim=c(3,max((parms[["U"]][2,2]*data.frame(out_low)[["s2"]]),
                  (parms[["U"]][2,2]*data.frame(out_midlow)[["s2"]]),
                  (parms[["U"]][2,2]*data.frame(out)[["s2"]]))),
     xlab = "",
     ylab = "")
title(ylab = expression(italic("f"[italic("2    ")]) (Tg/a)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      (parms[["U"]][2,2]*data.frame(out_midlow)[["s2"]]), 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      (parms[["U"]][2,2]*data.frame(out_low)[["s2"]]), 
      lwd = 4, lty = 1, col="black")
legend(1962, 25, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midlow$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_low$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#f9 --high
plot(comparison_data[["Year"]],
     (parms[["alpha_9"]]*data.frame(out)[["s4"]]),
     type = "l", col="red", lwd=4,
     ylim=c(1,max((parms[["alpha_9"]]*data.frame(out_high)[["s4"]]),
                  (parms[["alpha_9"]]*data.frame(out_midhigh)[["s4"]]),
                  (parms[["alpha_9"]]*data.frame(out)[["s4"]]))),
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = expression(italic("f"[italic("9    ")]) (Tg/a)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      (parms[["alpha_9"]]*data.frame(out_midhigh)[["s4"]]), 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      (parms[["alpha_9"]]*data.frame(out_high)[["s4"]]), 
      lwd = 4, lty = 1, col="black")
legend(1962, 17, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)
#f2 --high
plot(comparison_data[["Year"]],
     (parms[["U"]][2,2]*data.frame(out)[["s2"]]),
     type = "l", col="red", lwd=4,
     ylim=c(3,max((parms[["U"]][2,2]*data.frame(out_high)[["s2"]]),
                  (parms[["U"]][2,2]*data.frame(out_midhigh)[["s2"]]),
                  (parms[["U"]][2,2]*data.frame(out)[["s2"]]))),
     xlab = "",
     ylab = "")
title(xlab = "Year", 
      mgp = c(3, 1, 0))
title(ylab = expression(italic("f"[italic("2    ")]) (Tg/a)), 
      mgp = c(3, 1, 0))
lines(comparison_data[["Year"]], 
      (parms[["U"]][2,2]*data.frame(out_midhigh)[["s2"]]), 
      lwd = 4, lty = "dashed", col="black")
lines(comparison_data[["Year"]], 
      (parms[["U"]][2,2]*data.frame(out_high)[["s2"]]), 
      lwd = 4, lty = 1, col="black")
legend(1962, 26.5, legend = c(as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_midhigh$tau_e))), 
                            as.expression(bquote(tau[italic("E")] ~ "=" ~.(parms_high$tau_e)))),
       col=c("red", "black", "black"), lty = c(1, 2, 1), cex = 1.7, lwd=2)

sd(data.frame(out_low)[["Price"]])
sd(data.frame(out_midlow)[["Price"]])
sd(data.frame(out)[["Price"]])
sd(data.frame(out_midhigh)[["Price"]])
sd(data.frame(out_high)[["Price"]])
