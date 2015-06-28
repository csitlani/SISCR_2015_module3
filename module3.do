* Module 3

* Exploratory data analysis

use growth.dta, replace

* Declare the dataset to be "panel" data, grouped by id
* with time variable age
xtset id age

* Individual trajectories
xtline length, overlay legend(off)

* By gender
forv i = 1/26 { 
        if `i' < 12 { 
                local plotline1  "`plotline1' line length age if id == `i', lc(red)  || " 
        } 
        else { 
                local plotline2  "`plotline2' line length age if id == `i', lc(blue) || " 
        } 
} 
twoway `plotline1' `plotline2' , legend(order(1 "Females" 12 "Males")) 

* Means by gender
bysort gender: summarize length if age==8
bysort gender: summarize length if age==10
bysort gender: summarize length if age==12
bysort gender: summarize length if age==14

* Female fitted lines
forv i = 1/11 { 
local plotline1  "`plotline1' lfit length age if id == `i', lc(red)  || " 
}
twoway `plotline1', legend(off)

* Covariance and correlation, by gender
reshape wide length, i(id) j(age)
corr length8 length10 length12 length14 if gender==1
corr length8 length10 length12 length14 if gender==2
corr length8 length10 length12 length14 if gender==1, cov
corr length8 length10 length12 length14 if gender==2, cov

* Generalized estimating equations

use growth.dta, replace

* Declare the dataset to be "panel" data, grouped by id* with time variable agextset id age
* Generate a new variable for centered agegen cage = age-8* Fit models with an exchangeable correlation structurehelp xtgeexi: xtgee length i.gender*cage, corr(exch) robustlincom cage + _IgenXcage_2* Examine working correlation structureestat wcorr

* Linear mixed-effects models

use growth.dta, replace

* Declare the dataset to be "panel" data, grouped by id
* with time variable age
xtset id age

* Fit models with random intercepts and slopes
help mixed
gen cage = age-8
mixed length i.gender##c.cage || id:, stddeviations
est store ri
estat ic

mixed length i.gender##c.cage || id: cage, ///
cov(unstructured) stddeviations
est store rs
estat ic

* Use likelihood ratio test and AIC to compare models
lrtest ri rs
