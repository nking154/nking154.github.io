*===================================*
*             Load data             *
*===================================*

use "/Users/nickking/Desktop/Documents/University/Year 3/Dissertation/SUBMIT.dta", clear


*===================================*
*              Returns              *
*===================================*
tsset t
foreach var in brazil_close us_close uk_close india_close mexico_close {
	gen return_`var' = (D.`var' / L.`var') * 100
}

foreach var in return_india_close return_brazil_close return_uk_close return_mexico_close return_us_close{
	gen squ`var' = (`var'^2)
}


twoway /// 
    kdensity return_us_close, lcolor(blue)    ///
    || kdensity return_uk_close, lcolor(red)  ///
    || kdensity return_brazil_close, lcolor(green) ///
    || kdensity return_india_close, lcolor(orange) ///
    || kdensity return_mexico_close, lcolor(purple) ///
    , legend(order(1 "US" 2 "UK" 3 "Brazil" 4 "India" 5 "Mexico") ///
      position(11) ring(0) cols(1)) ///
	  ytitle("Density", size(small)) ///
	  xtitle("Returns", size(small)) ///
	  ylabel(, nogrid) ///
	  xlabel(, nogrid) 
	  
*===================================*
*          Weighted index           *
*===================================*
local countries "us mexico brazil india uk"
local gdps "0.334278 0.021573 0.0327272 0.04302533 0.04077036"

local i = 1 
foreach country of local countries {
    local gdp: word `i' of `gdps' 
    gen log_`country'risk = (1-`gdp')*log_`country'_epu + `gdp'*log_gepu
    local i = `i' + 1
}

*===================================*
*         Summary Stats             *
*===================================*
local countries "brazil india mexico uk us"
foreach country of local countries {
    estpost sum return_`country'_close log_`country'risk log_`country'_kof `country'_interest log_`country'_volume
}
    outreg2 using "summary_stats.doc", replace sum(log) 

*===================================*
*         Summary Graphs            *
*===================================*
label variable us_close USA
label variable india_close India
label variable brazil_close Brazil
label variable uk_close UK
label variable mexico_close Mexico

// shaded areas for the graph to indicate key shocks
gen shade1 = inrange(monthly_date, daily("01nov2007", "DMY"), daily("30jun2009", "DMY"))
gen shade2 = inrange(monthly_date, daily("01feb2020", "DMY"), daily("30sep2020", "DMY"))
gen shade = shade1 | shade2

quietly summarize log_ukrisk
gen ymin = r(min)
gen ymax = r(max)
local countries "us mexico brazil india uk"
foreach country of local countries {
	set scheme stmono1
    twoway ///
        (rarea ymax ymin monthly_date if shade1, color(gs12%40) lwidth(vvvthick)) ///
        (rarea ymax ymin monthly_date if shade2, color(gs12%40) lwidth(vvvthick)) ///
        (line log_`country'risk monthly_date, lcolor(blue) lwidth(medium) lpattern(solid)) /// Country risk line
        , ///
        title("`country'", color(black) margin(0 0 0 0) size(medium)) ///
        subtitle("Log Risk Over Time", color(black) margin(0 0 0 0) size(small)) ///
        xtitle("Date", color(black)) ///
        ytitle("Log Risk", color(black)) ///
        xlabel(, angle(45)) ///
        legend(off) ///
        graphregion(color(white)) ///
        bgcolor(white) ///
        name(G`country', replace)
}
graph combine Gus Gmexico Gbrazil Gindia Guk, ///
    title("Figure 1. Risk Profiles of Each Nation", color(black) size(medium)) /// 
    graphregion(color(white)) ///
	
*===================================*
*            Regressions            *
*===================================*

*============ Bivariate =============*

local countries "us india brazil uk mexico"
foreach country of local countries {
    reg return_`country'_close log_`country'risk, robust
	estimates store `country'bv
}
estout usbv ukbv indiabv mexicobv brazilbv, cells(b(star fmt(3)) se(par fmt(3)))
outreg2 [usbv ukbv indiabv mexicobv brazilbv] using "bivariate.doc", replace

*============ Multivariate ===========*

local countries "us india brazil uk mexico"
foreach country of local countries {
	reg return_`country'_close log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP, robust
	estimates store `country'mv
	vif
}
estout usmv ukmv indiamv mexicomv brazilmv, cells(b(star fmt(3)) se(par fmt(3)))
outreg2 [usmv ukmv indiamv mexicomv brazilmv] using "multivariate.doc", replace

*===================================*
*           Dickey-Fuller           *
*===================================*

gen trend = _n
local countries "us india brazil uk mexico"
local lags "1 2 1 2 6 1 1 3 1 1 0 3 1 1 1 3 3 1 2 2 2 1"
local i = 1
foreach var in return_us_close log_volatility_close log_usrisk us_NEXP_GDP log_us_volume return_india_close log_indiarisk india_NEXP_GDP log_india_volume return_uk_close log_ukrisk uk_NEXP_GDP log_uk_volume return_mexico_close log_mexicorisk mexico_NEXP_GDP log_mexico_volume return_brazil_close log_brazilrisk brazil_NEXP_GDP log_brazil_volume {
    varsoc `var', maxlag(8)
    local lag: word `i' of `lags'
   
    dfuller `var', trend lag(`lag')
   
    local test_stat = r(DF)
    local p_value = r(p) 
    local lags_used = r(lags) 

   
    local ++i
}

*===================================*
*             VAR Model            	*
*===================================*

local countries "brazil us uk india mexico"
foreach country of local countries {
	
	set scheme stcolor
    set more off
    
	var return_`country'_close log_`country'risk `country'_interest log_volatility_close `country'_NEXP_GDP log_`country'_volume
	estimates store var_`country'
    
// 	varstable, graph
    
	cap drop LOGLEVLS
    
	irf create LOGLEVLS, step(30) set(OIRF, replace)
    irf graph oirf, ///
	impulse(return_`country'_close) ///
	response(log_`country'risk `country'_interest log_volatility_close `country'_NEXP_GDP log_`country'_volume) ///
	level(95) yline(0) ///
	ylabel(, nogrid) ///
	xlabel(, nogrid) ///
	byopts(yrescale title("VAR analysis of `country'", size(medium)))
	
    graph export "var_`country'_irf_graph.png", replace
	
	estat ic // information criteria
	vargranger //granger causality

}

esttab var_brazil var_us var_uk var_india var_mexico, se ar2 label title("VAR Results by Country") mlabel(brazil us uk india mexico)

*===================================*
*        Forecast of VAR            *
*===================================*

gen train = 1 if trend <= 3500 //my training data, using the first 4000 observations, have 205 in the rest of sample to predict with ex-post forecasting
gen test = 1 if trend >= 4000

local countries "brazil us uk india mexico"
foreach country of local countries {
	
    set scheme stcolor
	
    var return_`country'_close log_`country'risk log_volatility_close ///
        log_`country'_volume `country'_interest `country'_NEXP_GDP ///
        if trend <= 4000, lags(4)

    fcast compute forecast_, step(205) dynamic(4000) replace

    gen forecasted_return_`country' = forecast_return_`country'_close

twoway ///
    (line return_`country'_close monthly_date if trend >= 4000, lcolor(blue)) ///
    (line forecasted_return_`country' monthly_date if trend >= 4000, lcolor(red)), ///
	legend(order(1 "Actual" 2 "Forecasted") size(small) position(1)) ///
    name(`country'_var, replace) ///
	legend(off) ///
    xlabel(, angle(45)) ///
    ytitle("Returns") ///
    xtitle("Date") ///
    ylabel(, nogrid) ///
    xlabel(, nogrid) ///
    yline(0, lpattern(dash) lcolor(black)) ///
    title("`country'", size(small))

    predict resid_`country', residuals
}

graph combine us_var uk_var mexico_var brazil_var india_var
graph export "var_combined_forecasts.png", replace

*===================================*
*        RF hyperparameters         *
*===================================*

*========= Commenting out for efficiency when rerunning code ==============*

// local countries "brazil us uk india mexico"
//
// foreach country of local countries {
//     foreach iteration in 100 200 {
//         foreach numvars in 3 4 5 {
//             preserve
//                 drop if missing(return_`country'_close, log_`country'risk, log_`country'_volume, `country'_interest, log_volatility_close, `country'_NEXP_GDP)
//
//                 count
//                 di "Remaining observations after dropping missing data for `country': " r(N)
//          
//             rforest return_`country'_close log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP, ///
//                 iterations(`iteration') numvars(`numvars') type(reg)
//          
//             di "OOB Error for `country' with iterations=`iteration' and numvars=`numvars': " e(OOB_Error)
//
//             restore
//         }
//     }
// }


**=========================================**
**           Ex-post forecasting           **
**=========================================**

*-----------------------------------*
*             Bivariate             *
*-----------------------------------*

*========= ACF/PACF plots to choose lags ==============*
// ac return_us_close
// pac return_us_close
// ac return_uk_close
// pac return_uk_close
// ac return_mexico_close
// pac return_mexico_close
// ac return_brazil_close
// pac return_brazil_close
// ac return_india_close
// pac return_india_close

*========= Commenting out for efficiency when rerunning code ==============*

// local countries "us uk brazil india mexico"
// foreach country of local countries{
// 	arimasoc return_`country'_close
// }
//us (2,0,2) uk (2,0,2) brazil (2,0,2) india (2,0,0) Mexico (2,0,1) are optimal

*========= us uk brazil ===========*
local countries "us uk brazil"
foreach country of local countries{
	tsset t
arima return_`country'_close if train == 1, arima(2,0,2)
predict arima_`country', dynamic(4000)
tsset monthly_date
tsline return_`country'_close arima_`country' if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("`country'", size(small)) ///
name(G`country', replace)
graph export "arimabv_`country'.png", replace

tsset t
arch return_`country'_close if train == 1, arch(1) garch(1)
predict sigma`country', variance
tsline sigma`country'
predict sigma_forecast`country', variance dynamic(4000)
tsset monthly_date
tsline sigma`country' sigma_forecast`country', ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("`country'", size(small)) ///
name(arch`country'bv, replace)

graph export "archbv_`country'.png", replace
tsset t


gen return_lag1`country' = L1.return_`country'_close
gen return_lag2`country' = L2.return_`country'_close
gen volatility`country' = sigma_forecast`country'

rforest return_`country'_close return_lag1`country' return_lag2`country' volatility`country' if !missing(return_`country'_close), type(reg) iterations(200) numvars(3)

predict rf_predicted`country' if test == 1

twoway (line return_`country'_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predicted`country' monthly_date if trend >= 4000, lcolor(red)), ///
       title("Forecast vs Actual Returns for `country'", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(g`country', replace)

graph export "bv_`country'_fcast_graph.png", replace


}

*========= india ===========*

tsset t
arima return_india_close if train == 1, arima(2,0,0)
predict arima_india, dynamic(4000)
tsset monthly_date
tsline return_india_close arima_india if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("india", size(small)) ///
name(Gindia, replace)

graph export "arimabv_india.png", replace

tsset t
arch return_india_close if train == 1, arch(1) garch(1)
predict sigmaindia, variance
tsline sigmaindia
predict sigma_forecastindia, variance dynamic(4000)
tsset monthly_date
tsline sigmaindia sigma_forecastindia, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("india", size(small)) ///
name(archindiabv, replace)

graph export "archbv_india.png", replace
tsset t


gen return_lag1india = L1.return_india_close
gen return_lag2india = L2.return_india_close
gen volatilityindia = sigma_forecastindia

rforest return_india_close return_lag1india return_lag2india volatilityindia if !missing(return_india_close), type(reg) iterations(200) numvars(3)

predict rf_predictedindia if test == 1

twoway (line return_india_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predictedindia monthly_date if trend >= 4000, lcolor(red)), ///
       title("Forecast vs Actual Returns for india", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(gindia, replace)

graph export "bv_india_fcast_graph.png", replace



*========= mexico ===========*

tsset t
arima return_mexico_close if train == 1, arima(2,0,1)
predict arima_mexico, dynamic(4000)
tsset monthly_date
tsline return_mexico_close arima_mexico if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("mexico", size(small)) ///
name(Gmexico, replace)

graph export "arimabv_mexico.png", replace
graph combine Gus Guk Gbrazil Gmexico Gindia
graph export "combined_arimabv.png", replace

tsset t
arch return_mexico_close if train == 1, arch(1) garch(1)
predict sigmamexico, variance
tsline sigmamexico
predict sigma_forecastmexico, variance dynamic(4000)
tsset monthly_date
tsline sigmamexico sigma_forecastmexico, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("mexico", size(small)) ///
name(archmexicobv, replace)

graph export "archbv_mexico.png", replace
graph combine archindiabv archusbv archukbv archbrazilbv archmexicobv
graph export "archbv_combined.png", replace
tsset t


gen return_lag1mexico = L1.return_mexico_close
gen return_lag2mexico = L2.return_mexico_close
gen volatilitymexico = sigma_forecastmexico

rforest return_mexico_close return_lag1mexico return_lag2mexico volatilitymexico if !missing(return_mexico_close), type(reg) iterations(200) numvars(3)

predict rf_predictedmexico if test == 1

twoway (line return_mexico_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predictedmexico monthly_date if trend >= 4000, lcolor(red)), ///
       title("Forecast vs Actual Returns for mexico", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(gmexico, replace)

graph export "bv_mexico_fcast_graph.png", replace



graph combine gmexico gindia guk gus gbrazil
    graph export "bv_combined_fcast_graph.png", replace

*-----------------------------------*
*             Multivariate          *
*-----------------------------------*

*========= us uk brazil ===========*

local countries "us uk brazil"
foreach country of local countries{
tsset t
arima return_`country'_close log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP if train == 1, arima(2,0,2)
predict arima_`country'mv, dynamic(4000)
tsset monthly_date
tsline return_`country'_close arima_`country'mv if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("`country'", size(small)) ///
name(G`country', replace)
graph export "arimamv_`country'.png", replace

tsset t

gen return_lag1`country'mv = L1.return_`country'_close
gen return_lag2`country'mv = L2.return_`country'_close
gen volatility`country'mv = sigma_forecast`country'

rforest return_`country'_close return_lag1`country'mv return_lag2`country'mv volatility`country'mv log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP if !missing(return_`country'_close), type(reg) iterations(200) numvars(5)

matrix list e(importance)

predict rf_predicted`country'mv if test == 1

twoway (line return_`country'_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predicted`country'mv monthly_date if trend >= 4000, lcolor(red)), ///
       title("RF: Forecast vs Actual Returns for `country'", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(g`country', replace)

graph export "mv_`country'_fcast_graph.png", replace

arima return_`country'_close return_lag1`country'mv return_lag2`country'mv volatility`country'mv log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP  if train == 1, arima(2,0,0)

predict arima_`country'g, dynamic(4000)

twoway (line return_`country'_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line arima_`country'g monthly_date if trend >= 4000, lcolor(red)), ///
       title("ARIMA: Forecast vs Actual Returns for `country'", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(`country'g, replace)
	   
}

*========= india ===========*

tsset t
arima return_india_close log_indiarisk log_india_volume india_interest log_volatility_close india_NEXP_GDP if train == 1, arima(2,0,0)
predict arima_indiamv, dynamic(4000)
tsset monthly_date
tsline return_india_close arima_indiamv if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("india", size(small)) ///
name(Gindia, replace)

graph export "arimamv_mexico.png", replace

tsset t

gen return_lag1indiamv = L1.return_india_close
gen return_lag2indiamv = L2.return_india_close
gen volatilityindiamv = sigma_forecastindia

rforest return_india_close return_lag1indiamv return_lag2indiamv volatilityindiamv log_indiarisk log_india_volume india_interest log_volatility_close india_NEXP_GDP if !missing(return_india_close), type(reg) iterations(200) numvars(5)

matrix list e(importance)

predict rf_predictedindiamv if test == 1

twoway (line return_india_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predictedindiamv monthly_date if trend >= 4000, lcolor(red)), ///
       title("RF: Forecast vs Actual Returns for india", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(gindia, replace)

graph export "mv_india_fcast_graph.png", replace

arima return_india_close return_lag1indiamv return_lag2indiamv volatilityindiamv log_indiarisk log_india_volume india_interest log_volatility_close india_NEXP_GDP  if train == 1, arima(2,0,0)

predict arima_indiag, dynamic(4000)
twoway (line return_india_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line arima_indiag monthly_date if trend >= 4000, lcolor(red)), ///
       title("ARIMA: Forecast vs Actual Returns for india", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(indiag, replace)



*========= mexico ===========*

tsset t
arima return_mexico_close log_mexicorisk log_mexico_volume mexico_interest log_volatility_close mexico_NEXP_GDP if train == 1, arima(2,0,1)
predict arima_mexicomv, dynamic(4000)
tsline return_mexico_close arima_mexicomv if test == 1, ///
xlabel(, angle(45)) ylabel(, angle(0)) ///
legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
title("mexico", size(small)) ///
name(Gmexico, replace) 

tsset monthly_date
graph export "arimamv_india.png", replace
graph combine Gus Guk Gmexico Gbrazil Gindia
graph export "combined_arimamv.png", replace

tsset t

gen return_lag1mexicomv = L1.return_mexico_close
gen return_lag2mexicomv = L2.return_mexico_close
gen volatilitymexicomv = sigma_forecastmexico

rforest return_mexico_close return_lag1mexicomv return_lag2mexicomv volatilitymexicomv log_mexicorisk log_mexico_volume mexico_interest log_volatility_close mexico_NEXP_GDP if !missing(return_mexico_close), type(reg) iterations(200) numvars(5)

matrix list e(importance)

predict rf_predictedmexicomv if test == 1

twoway (line return_mexico_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line rf_predictedmexicomv monthly_date if trend >= 4000, lcolor(red)), ///
       title("RF: Forecast vs Actual Returns for mexico", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(gmexico, replace)

graph export "mv_mexico_fcast_graph.png", replace

arima return_mexico_close return_lag1mexicomv return_lag2mexicomv volatilitymexicomv log_mexicorisk log_mexico_volume mexico_interest log_volatility_close mexico_NEXP_GDP  if train == 1, arima(2,0,0)

predict arima_mexicog, dynamic(4000)
twoway (line return_mexico_close monthly_date if trend >= 4000, lcolor(blue)) ///
       (line arima_mexicog monthly_date if trend >= 4000, lcolor(red)), ///
       title("ARIMA: Forecast vs Actual Returns for mexico", size(small)) ///
       xlabel(, angle(45)) ylabel(, angle(0)) ///
       legend(label(1 "Actual") label(2 "Forecast") position(6) cols(2)) ///
       name(mexicog, replace)	   


graph combine gmexico gindia guk gus gbrazil
    graph export "mv_combined_fcast_graph.png", replace
graph combine mexicog indiag ukg usg brazilg


*======= Plots to Visualise Residuals ======*

local countries "us uk mexico india brazil"
foreach country of local countries{
gen residuals_`country' = return_`country'_close - rf_predicted`country'mv
scatter residuals_`country' rf_predicted`country'mv if trend >= 4000, name(scatter_`country', replace) title("`country'")
histogram residuals_`country' if trend >= 4000, normal name(hist_`country', replace) title("`country'")
qnorm residuals_`country' if trend >= 4000, name(qq_`country', replace) title("`country'")
tsline residuals_`country' if trend >= 4000, name(ts_`country', replace) title("`country'")
graph box residuals_`country' if trend >= 4000, name(box_`country', replace) title("`country'")
}

graph combine scatter_us scatter_uk scatter_brazil scatter_india scatter_mexico
graph combine hist_us hist_uk hist_brazil hist_india hist_mexico
graph combine qq_us qq_uk qq_brazil qq_india qq_mexico
graph combine ts_us ts_uk ts_brazil ts_india ts_mexico
graph combine box_us box_uk box_brazil box_india box_mexico


*===================================*
*     RMSE and Diebold Mariano      *
*===================================*
local countries "us uk brazil india mexico"
foreach country of local countries{
gen error_rf`country' = return_`country'_close - rf_predicted`country' if t >= 4000
gen error_rf_mv`country' = return_`country'_close - rf_predicted`country'mv if t >= 4000

gen error_rf_sq`country' = error_rf`country'^2
gen error_rf_mv_sq`country' = error_rf_mv`country'^2

summarize error_rf_sq`country' if t >= 4000
display "RMSE (rf_predicted`country'): " sqrt(r(mean))

summarize error_rf_mv_sq`country' if t >= 4000
display "RMSE (rf_predicted`country'mv): " sqrt(r(mean)) 

gen loss_diff_bv_mv_`country' = error_rf_sq`country' - error_rf_mv_sq`country'

ttest loss_diff_bv_mv_`country' == 0

}

*===================================*
*   	Ex-ante forecast    	    *
*===================================*

set obs 4192

replace trend = _n
tsset trend
local countries "us uk mexico india brazil"
foreach country of local countries{
rforest return_`country'_close return_lag1`country' return_lag2`country' volatility`country' log_`country'risk log_`country'_volume `country'_interest log_volatility_close `country'_NEXP_GDP  if !missing(return_`country'_close), type(reg) iterations(200) numvars(5)
predict rf_`country'mvante if trend >= 4187
}

local countries "us uk mexico india brazil"
foreach country of local countries{
	drop rf_`country'mvante arima_`country'mvante
}
