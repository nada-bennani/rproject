clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)
clim$altitude
clim$p_mean
clim$altitude <- as.numeric(gsub(",","",clim$altitude))
clim$p_mean <- as.numeric(gsub(",","",clim$p_mean))
clim$altitude
clim$p_mean
install.packages("raster")
install.packages("sp")
install.packages("maps")

library(raster)
library(sp)
library(maps)
G1 <- raster::getData(country = "France", level = 1)
library(ggplot2)
ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()
climfar <- clim[1:34, ]
model <- lm(t_mean ~ altitude + lat + lon, data = climfar)
coeffs=coefficients(model)
coeffs
# > coeffs
# (Intercept)     altitude          lat          lon 
# 37.265036409 -0.006413946 -0.533960332  0.032101008 
summary(model)
# > summary(model)
# 
# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfar)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.76492 -0.32755  0.04337  0.24787  2.58927 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7308 on 30 degrees of freedom
# Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
# F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12
#interpretation
# Coefficients:
# Intercept (37.2650364): This is the value of the dependent variable (t_mean) when all predictor variables (altitude, lat, lon) are zero.
# Altitude (-0.0064139): A one-unit increase in altitude is associated with a decrease of approximately 0.0064 units in t_mean, holding other variables constant.
# Latitude (lat) (-0.5339603): A one-unit increase in latitude is associated with a decrease of approximately 0.534 units in t_mean, holding other variables constant.
# Longitude (lon) (0.0321010): A one-unit increase in longitude is associated with an increase of approximately 0.0321 units in t_mean, but it is not statistically significant as the p-value (0.424) is greater than the common significance level of 0.05
# R-squared (0.8329):
# it explains the variability in the dependent variable. 
#In this case, about 83.29% of the variability in t_mean is explained by the model.
#exercice2
model_2 <- lm(t_mean ~ altitude + lat, data = climfar)
summary(model_2)
# > summary(model_2)
# 
# Call:
#   lm(formula = t_mean ~ altitude + lat, data = climfar)
# 
# Residuals:
#   1          2          3          4          5          6          7 
# 4.524e-18  1.192e-18  3.168e-17  4.571e-16  4.854e-19  6.377e-17  6.903e-17 
# 8          9         10         11         12         13         14 
# 6.186e-18 -7.364e-18  6.357e-18 -1.389e-17 -3.978e-18 -3.986e-17 -7.220e-17 
# 15         16         17         18         19         20         21 
# 3.527e-01 -3.527e-01 -3.625e-01  3.625e-01 -2.898e-02  2.898e-02  1.337e-18 
# 22         23         24         25         26         27         28 
# 2.460e-18  2.665e-18  2.455e-18  2.670e-18  3.040e-18  4.634e-18  6.731e-19 
# 29         30         31         32         33         34 
# 6.688e-19  3.235e-18  1.683e-18  1.009e-16 -4.829e-17  2.110e-17 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 37.07660    3.58633  10.338  0.00923 **
#   altitude149 -2.07286    0.87399  -2.372  0.14110   
# altitude151 -1.84754    0.72183  -2.560  0.12472   
# altitude176 -1.98071    0.80690  -2.455  0.13351   
# altitude200 -1.85304    0.76337  -2.427  0.13594   
# altitude205 -1.55338    0.73953  -2.100  0.17049   
# altitude207 -1.36376    0.83971  -1.624  0.24585   
# altitude212 -1.99469    0.88122  -2.264  0.15192   
# altitude220 -1.94520    0.81711  -2.381  0.14026   
# altitude223 -2.53970    0.74876  -3.392  0.07701 . 
# altitude26  -0.79732    0.81313  -0.981  0.43021   
# altitude282 -2.60092    0.76629  -3.394  0.07692 . 
# altitude3   -0.23093    0.72031  -0.321  0.77891   
# altitude329 -2.30613    0.76600  -3.011  0.09489 . 
# altitude35  -0.72825    0.85123  -0.856  0.48239   
# altitude37  -0.12435    0.81872  -0.152  0.89322   
# altitude409 -2.49216    0.72797  -3.423  0.07576 . 
# altitude422 -2.75236    0.83240  -3.307  0.08057 . 
# altitude43  -2.30619    0.71655  -3.218  0.08448 . 
# altitude44  -0.93667    0.98948  -0.947  0.44375   
# altitude47  -1.41169    0.74122  -1.905  0.19714   
# altitude5    0.47852    0.72235   0.662  0.57581   
# altitude52  -0.65918    0.89402  -0.737  0.53770   
# altitude55   2.10458    0.72290   2.911  0.10051   
# altitude63  -0.74680    0.87638  -0.852  0.48390   
# altitude69  -0.72572    0.72040  -1.007  0.41982   
# altitude714 -4.29703    0.74614  -5.759  0.02885 * 
#   altitude73  -0.58933    0.75256  -0.783  0.51558   
# altitude8    0.20044    0.92961   0.216  0.84927   
# altitude94  -0.81948    0.69674  -1.176  0.36057   
# altitude98  -0.99600    0.76579  -1.301  0.32307   
# lat         -0.52119    0.08344  -6.246  0.02469 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5066 on 2 degrees of freedom
# Multiple R-squared:  0.9946,	Adjusted R-squared:  0.9117 
# F-statistic: 11.99 on 31 and 2 DF,  p-value: 0.07983
clim$lat
pred_t_mean <- predict(model_2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
pred_t_mean
#exercie3
install.packages('scatterplot3d')
library(scatterplot3d)
scatter_3d <- with(climfar, scatterplot3d(altitude, lat, t_mean,pch = 16, highlight.3d = TRUE,angle = 45,))
scatter_3d$plane3d(model_2)#4 to 5 interpretation
