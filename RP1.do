*************************************************************
*	Replication Exercise 1.									*
*	Nixon Alberto Torres Candiales							*
*	nat936													*
*	Cuasal Inference										*
*	ECO 385k												*
*	The University of Texas at Austin						*
*	02/27/2020												*
*************************************************************

clear
cd "/Users/Nixon/Desktop/Ma Economics/Spring 20/CI/PS/Assignment 1 (RDD)/Rp/Do"
use ../Data/hansen_dwi.dta, clear

//Dummy
gen d1=1 if bac1>= 0.08
replace d1 = 0 if bac1<0.08
label variable d1 "DUI"
label variable recidivism "Recidivism"
label variable acc "Accident at Scene"
label variable white "White"
label variable male "Male"
label variable age "Age"

//Figure 1
cd ../Figures
hist bac1, bcolor(gray%20) xline(0.08) freq kdensity width(0.00488) ylabel(0(2000)8000) xlabel(0(0.1)0.4) xtitle("BAC") subtitle("BAC histogram") saving(Figure1, replace)
 
 // Q.9 Regression // TABLE 1....
*************************************************************
*************************************************************eststo White : reg white d1 bac1 i.d1#c.bac1 if bac1 <= 0.13 & bac1 >= 0.03
eststo Male : reg male d1 bac1 i.d1#c.bac1 if bac1 <= 0.13 & bac1 >= 0.03
eststo Aged : reg aged d1 bac1 i.d1#c.bac1 if bac1 <= 0.13 & bac1 >= 0.03
eststo Acc : reg acc d1 bac1 i.d1#c.bac1 if bac1 <= 0.13 & bac1 >= 0.03
estimates table White Male Aged Acc, star
cd ../Tables
esttab White Male Aged Acc using table1-a.tex, label b se star replace

*** By RD ROBUST
quietly eststo White: rdrobust white bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) kernel(uniform) vce(hc0)
quietly eststo Male: rdrobust male bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) kernel(uniform) vce(hc0)
quietly eststo Aged: rdrobust aged bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) kernel(uniform) vce(hc0)
quietly eststo Acc: rdrobust acc bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) kernel(uniform) vce(hc0)
estimates table White Male Aged Acc, star
cd ../Tables
esttab White Male Aged Acc using table1-b.tex, label b se star replace
mean white if bac1>=0.03 & bac1<=0.13


 // Q.10 Plots CMOGRAM....
*************************************************************
*************************************************************
 cd ../Figures
label variable bac1 "BAC"
 **** Panel A-D With Quadratic fit
cmogram acc bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g1, replace
cmogram male bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g2, replace
cmogram age bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g3, replace
cmogram white bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g4, replace
graph combine g1.gph g2.gph g3.gph g4.gph, title("BAC and Characteristics")
graph save Figure2-Quadratic, replace
 **** Panel A-D With Linear fit
cmogram acc bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g1', replace
cmogram male bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g2', replace
cmogram age bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g3', replace
cmogram white bac1 if bac1<0.22 & bac1>0.03, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g4', replace
graph combine g1'.gph g2'.gph g3'.gph g4'.gph, title("BAC and Characteristics")
graph save Figure2-Linear, replace


 // Q.10 Plots CMOGRAM....
*************************************************************
*************************************************************
 **** Panel A-D With Quadratic fit
cmogram acc bac1 , cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g1, replace
cmogram male bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g2, replace
cmogram age bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g3, replace
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci histopts(width(0.002))
graph save g4, replace
graph combine g1.gph g2.gph g3.gph g4.gph, title("BAC and Characteristics") 
graph save Figure2-Quadratic-b, replace
 **** Panel A-D With Linear fit
cmogram acc bac1, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g1', replace
cmogram male bac1, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g2', replace
cmogram age bac1, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g3', replace
cmogram white bac1, cut(0.08) scatter line(0.08) lfitci histopts(width(0.002))
graph save g4', replace
graph combine g1'.gph g2'.gph g3'.gph g4'.gph, title("BAC and Characteristics")
graph save Figure2-Linear-b, replace



// Q.11 Regression....
*************************************************************
*************************************************************

gen bac3 = bac1^2
label variable bac3 "BAC^2"
quietly eststo Control : reg recidivism d1 bac1 white male aged acc year if bac1 <= 0.13 & bac1 >= 0.03,r
quietly eststo Linear : reg recidivism d1 bac1 i.d1#c.bac1 white male aged acc year if bac1 <= 0.13 & bac1 >= 0.03,r
quietly eststo Quadratic : reg recidivism d1 bac1 bac3 i.d1#c.bac1 i.d1#c.bac3 white male aged acc year if bac1 <= 0.13 & bac1 >= 0.03,r
estimates table Control Linear Quadratic, keep(d1) star
cd ../Tables
esttab Control Linear Quadratic using table3-b.tex, keep(d1) label b se star replace

quietly eststo Control : reg recidivism d1 bac1 white male aged acc year if bac1 >= 0.055 & bac1 <= 0.105,r
quietly eststo Linear : reg recidivism d1 bac1 i.d1#c.bac1 white male aged acc year if bac1 >= 0.055 & bac1 <= 0.105,r
quietly eststo Quadratic : reg recidivism d1 bac1 bac3 i.d1#c.bac1 i.d1#c.bac3 white male aged acc year if bac1 >= 0.055 & bac1 <= 0.105,r
estimates table Control Linear Quadratic, keep(d1) star
cd ../Tables
esttab Control Linear Quadratic using table3-c.tex, drop(white male aged acc) label b se star replace

 ** RD Robust
quietly eststo rdd1: rdrobust recidivism bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) covs(white year male aged acc) kernel(uniform) vce(hc0)
esttab rdd using table4.tex, star replace
rdrobust recidivism bac1 if bac1>=0.055 & bac1<=0.105, c(0.08) p(1) h(0.05) covs(white year male aged acc) kernel(uniform) vce(hc0)
* MEAN
mean reci if bac1 >= 0.03 & bac1 <= 0.13
mean reci if bac1 >= 0.055 & bac1 <= 0.105


// Q 12 kernel-weighted local polynomial regression
*************************************************************
*************************************************************
cd ../Figures
rdplot recidivism bac1 if bac1 < 0.15 & bac1>0.03, p(2) c(0.08) shade graph_options(legend(off) xlabel(0.03(0.02)0.15) xtitle("BAC") ytitle("Recidivism") subtitle("All offenders") )
graph save Figure3-a, replace
rdplot recidivism bac1 if bac1 < 0.15 & bac1>0.03, p(1) c(0.08) shade graph_options(legend(off) xlabel(0.03(0.02)0.15) xtitle("BAC") ytitle("Recidivism") subtitle("All offenders") )
graph save Figure3-b, replace
//Save the graphs in PNG
gr use Figure1.gph
gr export Figure1.png, as(png) replace 
gr use Figure2-Linear.gph
gr export Figure2-Linear.png, as(png) replace
gr use Figure2-Quadratic.gph
gr export Figure2-Quadratic.png, as(png) replace
gr use Figure2-Linear-b.gph
gr export Figure2-Linear-b.png, as(png) replace
gr use Figure2-Quadratic-b.gph
gr export Figure2-Quadratic-b.png, as(png) replace
gr use Figure3-a.gph
gr export Figure3-a.png, as(png) replace
gr use Figure3-b.gph
gr export Figure3-b.png, as(png) replace

rdrobust recidivism bac1 if bac1>=0.03 & bac1<=0.13, c(0.08) p(1) h(0.05) covs(white male aged acc) kernel(uniform) vce(hc0)
