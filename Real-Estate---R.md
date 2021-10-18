RedFin Housing Prices for Mar vista
================
Section: B Learning Team: 8 Students: Alex MarMureanu, Caldwell Clarke,
Lyle Seebeck, Kelly Li, Joaquin Trucco

##### Mount up

``` r
library(tidyverse)
library(ggridges)
library(jtools)
library(lmtest)
library(stargazer)
```

##### Get the data set up

``` r
HOUSES <- read.csv("~/Documents/UCLA MBA/Data and Decisions/mba_data_decisions/redfinMarVista.csv", 
                   stringsAsFactors = T)
```

##### Notes on data selection

-   We are looking at single family homes in the Mar Vista area of LA
    that have been sold in the last 12 months.
-   We have included:
    -   CITY - we’re curious if an LA versus MV address makes a
        difference
    -   PRICE - total paid in last purchase
    -   BEDS - number of bedrooms
    -   BATHS - number of bathrooms
    -   SQUARE.FEET - total square footage of the house itself
    -   LOT.SIZE -total square footage of the property
    -   YEAR.BUILT - how old the house is
-   We have ignored a variety of data points, which we’ll explain below:
    -   SALE.TYPE - all houses are sold
    -   SOLD.DATE - this is a categorical variable, so the regression
        cannot run this variable as time-based, which renders it pretty
        useless
    -   PROPERTY.TYPE - All houses in this set are single-family
        residential
    -   ADDRESS - again, this is one-off categorical data, and is not
        related to location in the regression
    -   STATE.OR.PROVINCE - its all CA
    -   ZIP.OR.POSTAL.CODE - because this is numerical, rather than
        categorical, we would get a trend line for numeric growth in zip
    -   LOCATION - this tries to split between Mar Vista, Palms, etc.
        but the data set is very incomplete and has varying notation
    -   DAYS.ON.MARKET - we expect this to drop prices, but the data set
        is pretty incomplete
    -   X..SQUARE.FEET - $/Square Feet
    -   The remaining fields are internal and not useful

##### Regression based on variables selected

``` r
Regression1 = lm(PRICE ~ BEDS + BATHS + CITY + SQUARE.FEET + LOT.SIZE + YEAR.BUILT, data = HOUSES)
#summ(Regression1, digits=3)
stargazer(Regression1,single.row = TRUE, type = "text")
```

    ## 
    ## =====================================================
    ##                            Dependent variable:       
    ##                     ---------------------------------
    ##                                   PRICE              
    ## -----------------------------------------------------
    ## BEDS                    -54,163.320 (37,065.290)     
    ## BATHS                  99,724.510*** (35,680.190)    
    ## CITYMar Vista          -143,538.200 (105,102.300)    
    ## SQUARE.FEET                465.830*** (56.252)       
    ## LOT.SIZE                   103.960*** (13.629)       
    ## YEAR.BUILT               3,212.676*** (956.488)      
    ## Constant            -5,904,715.000*** (1,872,465.000)
    ## -----------------------------------------------------
    ## Observations                       321               
    ## R2                                0.746              
    ## Adjusted R2                       0.741              
    ## Residual Std. Error      382,049.500 (df = 314)      
    ## F Statistic             153.561*** (df = 6; 314)     
    ## =====================================================
    ## Note:                     *p<0.1; **p<0.05; ***p<0.01

#### Problem 1

> For each house, calculate the price per square foot. What is the
> average price per square foot in your selected city?

``` r
HOUSES<- HOUSES %>% mutate(Price_sf = PRICE/SQUARE.FEET)
avg_Price_sf <- mean(HOUSES$Price_sf,na.rm = T)
print(avg_Price_sf)
```

    ## [1] 1055.774

-   The average price per squre foot in the neighborhood selected is
    $1056.

#### Problem 2

> Run a simple linear regression of SALE\_PRICE vs SQUARE\_FEET. What is
> your interpretation of the coefficient?

``` r
lm_prob2 <- lm(data = HOUSES, PRICE ~ SQUARE.FEET)
#summ(lm_prob2, digits=3)
stargazer(lm_prob2 ,single.row = TRUE, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                PRICE           
    ## -----------------------------------------------
    ## SQUARE.FEET             682.486*** (25.215)    
    ## Constant            674,095.000*** (58,998.130)
    ## -----------------------------------------------
    ## Observations                    324            
    ## R2                             0.695           
    ## Adjusted R2                    0.694           
    ## Residual Std. Error   415,659.100 (df = 322)   
    ## F Statistic          732.592*** (df = 1; 322)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

-   The coefficient of Square Feet is 682.486, meaning that for each
    additional unit of square foot, the price of the house increases by
    682.486 dollars.

#### Problem 3

> Run a multiple regression with additional variables for BEDS and
> BATHS. What do you notice?

``` r
lm_prob3 <- lm(data = HOUSES, PRICE ~ SQUARE.FEET + BEDS + BATHS)
#umm(lm_prob3, digits=3)
stargazer(lm_prob3 ,single.row = TRUE, type = "text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                PRICE           
    ## -----------------------------------------------
    ## SQUARE.FEET             653.072*** (53.398)    
    ## BEDS                 -61,205.230 (39,319.040)  
    ## BATHS                66,029.660* (36,472.150)  
    ## Constant            765,291.400*** (89,724.680)
    ## -----------------------------------------------
    ## Observations                    324            
    ## R2                             0.699           
    ## Adjusted R2                    0.696           
    ## Residual Std. Error   413,956.200 (df = 320)   
    ## F Statistic          247.762*** (df = 3; 320)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

-   According to the information of regression 3, the coefficient of
    BEDS is -6.1205228^{4}, meaning that for each additional bedroom,
    the price of the house decreases by -6.1205228^{4} dollars and the
    coefficient of BATHS is 6.6029661^{4}, meaning that for each
    additional bathroom, the price of the house increases by
    6.6029661^{4} dollars. Also both independent variables, BEDS and
    BATHS, are not statistically significant.

#### Problem 4

> If you are analyzing homes other than single family homes (e.g.,
> condos, townhouses), what do you notice about LOT\_SIZE?

-   Our data only contains one observation that’s not single family
    residential.

``` r
cor(HOUSES$PRICE, HOUSES$LOT.SIZE, use = "complete.obs")
```

    ## [1] 0.4753121

``` r
cor(HOUSES$BEDS, HOUSES$LOT.SIZE, use = "complete.obs")
```

    ## [1] 0.2879293

``` r
cor(HOUSES$BATHS, HOUSES$LOT.SIZE, use = "complete.obs")
```

    ## [1] 0.2082299

``` r
cor(HOUSES$SQUARE.FEET, HOUSES$LOT.SIZE, use = "complete.obs")
```

    ## [1] 0.3558679

-   From the correlation tests above, we found that lot size is
    positively correlated with Price, Beds, Baths, and Square Feet.

#### Problem 5

> What fraction of the variation in home prices is explained by your
> selected variables? How does your model change with the addition of
> other variables?

``` r
#summ(Regression1, digits=3)
plot(HOUSES$PRICE ~ HOUSES$LOT.SIZE) # not linearly related
```

![](Real-Estate---R_files/figure-gfm/prob5-1.png)<!-- -->

``` r
HOUSES <- HOUSES %>% mutate(SQUARE.FEET2 = SQUARE.FEET^2,LOT.SIZE2 = LOT.SIZE^2)
Regression2<-lm(PRICE ~ BEDS + BATHS + CITY + SQUARE.FEET+ LOT.SIZE+LOT.SIZE2 + YEAR.BUILT + DAYS.ON.MARKET + ZIP.OR.POSTAL.CODE, data = HOUSES)
stargazer(Regression1,Regression2 ,single.row = TRUE, type = "text")
```

    ## 
    ## =========================================================================================
    ##                                              Dependent variable:                         
    ##                     ---------------------------------------------------------------------
    ##                                                     PRICE                                
    ##                                    (1)                                (2)                
    ## -----------------------------------------------------------------------------------------
    ## BEDS                    -54,163.320 (37,065.290)           -31,874.280 (33,906.430)      
    ## BATHS                  99,724.510*** (35,680.190)          61,175.220* (32,239.920)      
    ## CITYMar Vista          -143,538.200 (105,102.300)          -93,289.520 (86,696.290)      
    ## SQUARE.FEET                465.830*** (56.252)                514.560*** (53.139)        
    ## LOT.SIZE                   103.960*** (13.629)                149.549*** (40.664)        
    ## LOT.SIZE2                                                       -0.004 (0.002)           
    ## YEAR.BUILT               3,212.676*** (956.488)             3,350.826*** (877.754)       
    ## DAYS.ON.MARKET                                               -902.892*** (181.699)       
    ## ZIP.OR.POSTAL.CODE                                          4,523.934* (2,403.289)       
    ## Constant            -5,904,715.000*** (1,872,465.000) -413,662,027.000* (216,336,112.000)
    ## -----------------------------------------------------------------------------------------
    ## Observations                       321                                265                
    ## R2                                0.746                              0.835               
    ## Adjusted R2                       0.741                              0.829               
    ## Residual Std. Error      382,049.500 (df = 314)             310,012.900 (df = 255)       
    ## F Statistic             153.561*** (df = 6; 314)           142.873*** (df = 9; 255)      
    ## =========================================================================================
    ## Note:                                                         *p<0.1; **p<0.05; ***p<0.01

-   74.6% of the variation in home prices can be explained by our
    selected variables (BEDS, BATHS, CITY, SQUARE.FEET, LOT.SIZE ,
    YEAR.BUILT).
-   After adding in quadratic term of lot size (to account for
    non-linear relationship), days on market (account for perception of
    desirability), and zip/postal code (to account for location), the
    new model accounts for 83.5% of the variation in house prices.

#### Problem 6

> Try running your model to predict a typical home price. For example,
> what is the predicted sale price of a 2-bedroom, 2-bath home with
> 1,500 square feet? What is the 95% prediction interval?

``` r
#assume other variables are at the mean values
newdata <- data.frame(BEDS = 2, BATHS = 2, CITY = "Los Angeles", SQUARE.FEET = 1500,
                      LOT.SIZE = mean(HOUSES$LOT.SIZE,na.rm=T), YEAR.BUILT = mean(HOUSES$YEAR.BUILT, na.rm=T))
predict(Regression1, newdata, interval = "confidence", se.fit=T)
```

    ## $fit
    ##       fit     lwr     upr
    ## 1 1850435 1761074 1939795
    ## 
    ## $se.fit
    ## [1] 45417.18
    ## 
    ## $df
    ## [1] 314
    ## 
    ## $residual.scale
    ## [1] 382049.5

``` r
predict_price <- predict(Regression1, newdata, interval = "confidence", se.fit=T)
```

-   Assuming a typical home has 2 bedrooms, 2 bathrooms, 1500 sq ft, and
    a lot size of 6320 sq ft, and built in 1964, it will be sold at
    approximately $1,850,435 (95%CI: 1761074, 1939795).

#### Problem 7

> How might you use your model to identify investment opportunities in
> the current housing market in your city?

-   Idea \#1: If we plot the linear equation that comes from the linear
    regression, we can focus our attention on those outliers, which are
    under the line, and check why those houses differ from the price
    predictions of the model. If we cannot find a logical explanation,
    we can assume that these specific outliers are undervalued. We could
    buy that house and sell it for a profit.

-   Idea \#2: You could find information about houses that were sold a
    few years ago and use this information to review their current
    value. After you do that, you can offer to buy the house at a
    cheaper price than the one that the regression suggests, and then
    you could sell it for a profit.

#### Problem 8

> Based on your first-hand knowledge of the city, what other variable(s)
> would you recommend including to better predict residential home
> prices? Pick one of these variables and collect the data for a small
> sample of the homes listed. What is the correlation between your new
> variable and sales price? Be creative!

Mar Vista is a mostly suburban neighborhood to the west of the 405 and
east of the 10. It has some hills, so we may see some variation in price
based on elevation. It’s also close to some major landmarks, so we
assume that well see some variation from proximity to things like the
beach, airport, and freeways. Because it’s very residential, we also
expect to see some variation based on proximity to parks and schools.
There’s also a cute little downtown strip on Venice and Centinela with
shops, restaurants, and a fantastic farmer’s market on the weekends.
We’ll examine the relationship between price and proximity to this area
by measuring the distance from the house to the farmers market via
Google Maps for a selection of 34 houses.

``` r
cor(HOUSES$PRICE, HOUSES$MARKETDIST, use = "complete.obs")
```

    ## [1] 0.04559644

``` r
lm_prob8 <- lm(data = HOUSES, PRICE ~ MARKETDIST)
#summ(lm_prob8, digits=3)
stargazer(lm_prob8 ,single.row = TRUE, type = "text")
```

    ## 
    ## ==================================================
    ##                          Dependent variable:      
    ##                     ------------------------------
    ##                                 PRICE             
    ## --------------------------------------------------
    ## MARKETDIST             78,161.130 (302,714.300)   
    ## Constant            2,218,828.000*** (404,970.900)
    ## --------------------------------------------------
    ## Observations                      34              
    ## R2                              0.002             
    ## Adjusted R2                     -0.029            
    ## Residual Std. Error    1,011,598.000 (df = 32)    
    ## F Statistic               0.067 (df = 1; 32)      
    ## ==================================================
    ## Note:                  *p<0.1; **p<0.05; ***p<0.01

Oddly enough, the price of the house is positively correlated with
distance to the farmer’s market, meaning a house farther away will be
more expensive. It may be running into other confounding location
variables like proximity to the Mar Vista Recreation Center or
elevation, or it may indicate a preference for distance from the more
active areas. We have a high p-value though, so we cannot confidently
say that the distance to the farmer’s market affects the sale price.
