
set more off
clear all
set maxvar 120000

cd ~/TechChange/


* load data
use ./clean/output/covidCPS.dta, clear

* set as panel
gen datem = 12*(year - 1960) + month - 1
xtset cpsidp datem, m

* age
bysort cpsidp (datem): egen age_id = min(age)
gen agesq = age^2

* define people by their modal ind/occ response
bysort cpsidp: egen indmode = mode(ind1990)
bysort cpsidp: egen occmode = mode(occ2010)


* look at telework by industry and occupation
gen telework = 1 if covidtelew == 2
replace telework = 0 if covidtelew == 1
replace telework = . if covidtelew ~=1 & covidtelew~=2

bysort ind1990: egen mn_telework_ind = mean(telework)
bysort occ2010: egen mn_telework_occ = mean(telework)


bysort ind1990: gen n= _n
bysort occ2010: gen nocc= _n
sum mn_telework_ind if n == 1, de
gen highTW_ind = mn_telework_ind > r(p50)
gen lowTW_ind = mn_telework_ind < r(p50)
sum mn_telework_occ if nocc == 1, de
gen highTW_occ = mn_telework_occ > r(p50)
gen lowTW_occ = mn_telework_occ < r(p50)
drop n nocc


* define outcome variables
gen unemp = empstat >=20 & empstat <30
gen nilf = empstat >= 30
gen emp = empstat < 20



**********************************************************
*** STATE-MONTH UNEMP RATE

preserve
collapse (sum) unemp nilf emp (count) cpsidp, by(statefip datem)
decode statefip, gen(state)
saveold ./clean/output/EmpState_CPS_Covid.dta, replace
export delimited using ./clean/output/EmpState_CPS_Covid.csv, replace
restore




***********************************
* EVENT STUDIES
***********************************



* event study regs using occupation variation
eststo clear
eststo unempES: xtreg unemp 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem  i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ , fe   allbaselevels 
eststo nilfES: 	xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ , fe   allbaselevels 
eststo empES: 	xtreg emp 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ , fe   allbaselevels 


eststo unempES_old: xtreg unemp i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=50, fe   allbaselevels 
eststo nilfES_old:  xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	 i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=50, fe   allbaselevels 

eststo unempES_nold: xtreg unemp 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem  i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 
eststo nilfES_nold:  xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 
eststo empES_nold:   xtreg emp 		i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 


eststo unempES_vold: xtreg unemp i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 
eststo nilfES_vold:  xtreg nilf  i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 
eststo empES_vold:   xtreg emp 	 i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 


coefplot unempES , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed]") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/unemp_EventStudyCovid_occ.png, as(png) replace

coefplot nilfES , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF]") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/nilf_EventStudyCovid_occ.png, as(png) replace



coefplot unempES_old , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed] ; Sample = Individuals of Age >= 50") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/unempold_EventStudyCovid_occ.png, as(png) replace

coefplot nilfES_old , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ; Sample = Individuals of Age >= 50") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/nilfold_EventStudyCovid_occ.png, as(png) replace



coefplot unempES_vold , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed] ; Sample = Individuals of Age >= 60") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/unempvold_EventStudyCovid_occ.png, as(png) replace

coefplot nilfES_vold , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ; Sample = Individuals of Age >= 60") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/nilfvold_EventStudyCovid_occ.png, as(png) replace

coefplot  (unempES, label("All Ages")) ( unempES_vold, label("Age >= 60" )) , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7)) ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/unempvoldcomp_EventStudyCovid_occ.png, as(png) replace

	
coefplot (nilfES, label("All Ages")) ( nilfES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/nilfvoldcomp_EventStudyCovid_occ.png, as(png) replace

coefplot (empES, label("All Ages")) ( empES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[employed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/empvoldcomp_EventStudyCovid_occ.png, as(png) replace

coefplot (empES_nold, label("Age < 40")) ( empES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[employed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/empvoldnold_EventStudyCovid_occ.png, as(png) replace




* event study regs using industry variation
eststo clear
eststo unempES: xtreg unemp 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem  i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind , fe   allbaselevels 
eststo nilfES: 	xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind , fe   allbaselevels 

eststo unempES_old: xtreg unemp i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind if age_id >=50, fe   allbaselevels 
eststo nilfES_old:  xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	 i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind if age_id >=50, fe   allbaselevels 


eststo unempES_vold: xtreg unemp i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind if age_id >=60, fe   allbaselevels 
eststo nilfES_vold:  xtreg nilf  i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_ind ib720.datem#c.mn_telework_ind if age_id >=60, fe   allbaselevels 


coefplot  (unempES, label("All Ages")) ( unempES_vold, label("Age >= 60" )) , baselevels omitted ///
	keep(*datem*mn_telework_ind* )  yline(0) xtitle("Month") legend( ring(0) pos(7)) ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_ind = "Jan 2020" 721.datem#*mn_telework_ind = " " 722.datem#*mn_telework_ind = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/unempvoldcomp_EventStudyCovid_ind.png, as(png) replace

	
coefplot (nilfES, label("All Ages")) ( nilfES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_ind* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_ind = "Jan 2020" 721.datem#*mn_telework_ind = " " 722.datem#*mn_telework_ind = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")
	graph export ./analyze/output/nilfvoldcomp_EventStudyCovid_ind.png, as(png) replace





***********************************
* BINSCATTERS
***********************************


* figures

preserve
drop if mi(mn_telework_occ)
drop if mi(mn_telework_ind)

binscatter emp datem, by(highTW_occ) line(connect) ///
	legend(label(1 "Below Median Teleworking in Occupation") label(2 "Above Median Teleworking in Occupation") ring(0) pos(7)) ///
	title("Probability of Employment") subtitle("(conditional on reporting one's occupation)") ytitle("") ///
	xtitle("Month") tlabel(708(6)731,format(%tm)) xline(723)
graph export ./analyze/output/bin_TWocc_emp.png, as(png) replace
 
binscatter unemp datem, by(highTW_ind) line(connect)

binscatter emp datem, by(highTW_ind) line(connect) ///
	legend(label(1 "Below Median Teleworking in Industry") label(2 "Above Median Teleworking in Industry") ring(0) pos(7)) ///
	title("Probability of Employment") subtitle("(conditional on reporting one's industry)") ytitle("") ///
	xtitle("Month") tlabel(708(6)731,format(%tm)) xline(723)
graph export ./analyze/output/bin_TWind_emp.png, as(png) replace

binscatter unemp age, by(highTW_occ) line(connect)

binscatter unemp age if  age >= 40, by(highTW_occ) ///
	legend(label(1 "Below Median Teleworking in Occupation") label(2 "Above Median Teleworking in Occupation") ring(0) pos(4)) ///
	title(Probability of Reporting Unemlpoyment by Age and Occupation Group) ///
	subtitle("(conditional on reporting one's occupation)") ytitle("") 
graph export ./analyze/output/bin_TWocc_ageUnemp.png, as(png) replace
 
	

reg unemp i.highTW_occ##c.age if datem >= tm(2020m3)
reg unemp i.highTW_occ##c.age if datem < tm(2020m3)

restore



***********************************
* EFFECTS ON PEOPLE LAID OFF
***********************************


preserve

* drop people who have been out of work for the past 5 years or never worked
drop if occmode == 9920

* keep people who were unemployed experienced workers at some point in sample
bysort cpsidp: egen everUnemp = max(unemp)
gen unempExp = empstat == 21
bysort cpsidp: egen everUnempExp = max(unempExp)
keep if everUnempExp


* define layoffs as going from emp to unemp and keep peopel with one layoff reported
bysort cpsidp (datem): gen layoff = (unemp == 1 & L.emp == 1)
bysort cpsidp (datem): egen totlayoff = sum(layoff)
keep if totlayoff >0 

* define event time around layoff date
gen datelayoff = layoff * datem
bysort cpsidp: egen datelayoffmax = max(datelayoff)
gen eventTime = datem - datelayoffmax


/*
bysort cpsidp unemp (datem): gen n = _n
replace n = 0 if ~unemp
gen firstunemp = n == 1
replace firstunemp = firstunemp * datem
bysort cpsidp: egen firstunempmax = max(firstunemp)
gen eventTime = datem - firstunempmax


* drop people who went from NILF to Unemp 
*gen nope = (eventTime == -1 & empstat >=30)
*bysort cpsidp : egen maxnope = max(nope)
*drop if maxnope == 1
*drop maxnope nope

binscatter unemp eventTime , by(highTW_occ) line(connect)
binscatter unemp eventTime if age >=60, by(highTW_occ) line(connect)


binscatter nilf eventTime, by(highTW_occ) line(connect)

binscatter nilf eventTime if age >=60, by(highTW_occ) line(connect)

*/


eststo unempES_nold: xtreg unemp 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem  i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 
eststo nilfES_nold:  xtreg nilf 	i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 
eststo empES_nold:   xtreg emp 		i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem 	i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id < 40, fe   allbaselevels 


eststo unempES_vold: xtreg unemp i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 
eststo nilfES_vold:  xtreg nilf  i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 
eststo empES_vold:   xtreg emp 	 i.statefip#c.datem i.indmode#c.datem i.occmode#c.datem   i.datem  mn_telework_occ ib720.datem#c.mn_telework_occ if age_id >=60, fe   allbaselevels 


coefplot (nilfES_nold, label("Age < 40")) ( nilfES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")

coefplot (unempES_nold, label("Age < 40")) ( unempES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[unemployed (looking for work)] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")

coefplot (empES_nold, label("Age < 40")) ( empES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[employed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy")


coefplot ( empES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[employed] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy") ///
	subtitle("Sample: Workers older than 60 who at some point faced a layoff")
graph export ./analyze/output/empvold_layoff.png, as(png) replace

coefplot ( nilfES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[NILF] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy") ///
	subtitle("Sample: Workers older than 60 who at some point faced a layoff")
graph export ./analyze/output/nilfvold_layoff.png, as(png) replace	

coefplot ( unempES_vold, label("Age >= 60" ))  , baselevels omitted ///
	keep(*datem*mn_telework_occ* )  yline(0) xtitle("Month") legend( ring(0) pos(7))  ///
	vert  coeflabels(708.datem#* = "Jan 2019" 709.datem#* = " " 710.datem#* = " " 711.datem#* = " " 712.datem#* = " " 713.datem#* = " "   714.datem#* = "July 2019" 715.datem#* = " " 716.datem#* = " " 717.datem#* = " " 718.datem#* = " " 719.datem#* = " "   720.datem#*mn_telework_occ = "Jan 2020" 721.datem#*mn_telework_occ = " " 722.datem#*mn_telework_occ = " "  723.datem#* = " "  724.datem#* = " "  725.datem#* = " "  726.datem#* = "July 2020"   727.datem#* = " "  728.datem#* = " "   729.datem#* = " " 730.datem#* = " "  , wrap(4)) /// 
	title("Dep. Var. = 1[Unemployed (looking for job)] ") ytitle("Coefficient on ''Fraction Teleworking'' " "Interacted with Month Dummy") ///
	subtitle("Sample: Workers older than 60 who at some point faced a layoff")
graph export ./analyze/output/unempvold_layoff.png, as(png) replace
