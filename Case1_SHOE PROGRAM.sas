


Data bodyfat;
	Input age sex $ pctfat;
datalines;
	23 	male		28
	21 	male		16
	39 	female 		31
 	41	male 		26
	49	male 		25
	50	female 		31
	53	female 		35
	53	female 		42
	37	male 		18
	32	male 		33
	25 	female 		28
	32 	female 		39
	45 	female 		30
	51 	female 		26
	18  female		21
	41	male 		22
	64	male 		29
	22  female		19
	44	male 		22
	52	female 		34
;
RUN;


proc ttest;
   var age pctfat;
run;



Proc print data=bodyfat; Run;


Proc means data=bodyfat;
       	Var age Pctfat;
*Class sex;
		where sex = 'female'; 
Run;


proc freq data = bodyfat;
table age; where age > 25;
run;



Proc corr data=bodyfat;
  Var age pctfat;
Run;
Run;


*Proc Plot data=bodyfat;
   *Plot PctFat*age;
*Run;

Proc sort data=bodyfat;
  By sex;
Run;

*Proc Plot data=bodyfat;
   *Plot PctFat*age;
*By sex;
*Run;

*Proc gPlot data=bodyfat;
   *Plot PctFat*age;
*By sex;
*Run;


Proc reg data = bodyfat;
model Pctfat = age ;
Run; 
quit;


data bodyfat2;
set bodyfat;
male = 0;
if sex = 'male' then male = 1;
run;



Proc reg data = bodyfat2;
model Pctfat = age male;
Run; 
quit;



LIBNAME UTD 'C:\mgrigsby\Other\school\UTD'; RUN;

LIBNAME UTD 'C:\mgrigsby\Other\school\UTD\Marketing Analytics Spring 2015\week1'; run;


* testing whether total spend is statistically different in august *;

proc ttest data = merge;
class aug;
var total_spend;
run;


* ;

PROC SORT DATA = UTD.SHOE_DATA; 
BY CUSTOMER_ID; RUN;


PROC SORT DATA = UTD.SHOE_DEMO; 
BY CUSTOMER_ID; RUN;


DATA WORK.MERGE;
MERGE UTD.SHOE_DATA UTD.SHOE_DEMO; 
BY CUSTOMER_ID; RUN;


proc means data = merge;
var spend_per_txn; 
where hhincome > 100000;
run;



proc means data = merge;
var internet_spend; 
where hhage < 40;
run;



PROC FREQ DATA = merge; TABLE total_txns; RUN;


proc reg data = merge;
model total_txns = hhage hhkids cmpns tenure; 
output out = regress p = predicted;
run;


proc countreg data=merge;
   model  total_txns = hhage hhkids cmpns tenure / dist=poisson ;
   output out = poisson pred = predicted;
run;

proc corr data = regress;
var total_txns predicted; run;

proc corr data = poisson;
var total_txns predicted; run;


OUT=SAS-data-set 
names the output data set. 
 XBETA=name 
names the variable that contains estimates of $\mathbf{x}_{i}’\bbeta $. 
 PRED=name 
names the variable that contains the predicted value of the response variable


proc reg data = merge;
model spend_per_txn = tenure cmpns hhincome opens clicks; 
output out = pred p = pred_tottxns; run; quit;



proc reg data = merge;
model total_txns = tenure cmpns hhincome
feb mar apr may jun jul aug sep oct nov dec; 
output out = pred p = pred_tottxns; run; quit;


*** graphs ***;
proc sort data = pred;
by total_txns;
proc gplot data = pred;
plot total_txns*pred_tottxns = 1;
run;



LIBNAME ERNAN 'C:\mgrigsby\Other\school\UTD\Ernanfiles\Lecture1'; RUN;


*** ELASTICITY SEMINAR ***;

proc reg data = merge;
model total_items = 
recency
response
unique_sizes
unique_depts
tenure
retained_flag
cmpns
pct_response
hhage
hhincome/collin;
run; quit;



*** practice calc'ing elast *;

libname mg 'c:\mgrigsby\'; run;


proc reg data = mg.segment1; 
model well_visits =  exam_vca exam_comp_hi exam_comp_lo exam_vca_hi; run; quit;

proc means data = mg.segment1; var well_visits  exam_vca ; run;


*** practice calc'ing elast *;
proc reg data = mg.segment4 ;
model well_visits =  exam_vca 	exam_comp_hi 	exam_comp_lo 	exam_vca_hi  tenure   
share_visit_boarding	any_puppy_kitten; run; quit;



* correct collin by ridge reg *;
proc reg data = mg.segment3;
model felv = flv_vca 
flv_comp_hi 				flv_comp_lo 					flv_vca_hi 
share_well_visits			share_visit_boarding			any_puppy_kitten		
share_visit_food/collin; 	run; quit;


proc reg data = mg.segment3 outvif
outest = mg.s3b 
ridge = 0 to 1 by 0.05;
model felv = flv_vca 
flv_comp_hi 				flv_comp_lo 					flv_vca_hi 
share_well_visits			share_visit_boarding			any_puppy_kitten		
share_visit_food/collin; 	run; quit;


* correct colin by factoring *;
proc factor data = mg.segment3 out = mg.factors nfactors = 6;
var
flv_comp_hi 				flv_comp_lo 					flv_vca_hi 
share_well_visits			share_visit_boarding			any_puppy_kitten;
run;



proc reg data = mg.factors;
model felv = flv_vca 
factor1
factor2
factor3
factor4
factor5/collin; 	run; quit;



* correct collin by removing a variable *;
proc reg data = mg.segment1; 
model neuter =  neut_vca 	neut_comp_hi 			neut_comp_lo 				neut_vca_hi
share_well_visits			tenure					share_visit_boarding		any_puppy_kitten	
share_visit_food			share_visit_grooming	share_visit_laboratory		share_visit_neuter				
share_visit_otc_flea		share_visit_pharmacy	share_visit_spay				
lifetime_visits				mult_cat				mult_dog					mult_dog_and_cat
num_adult/collin;	
run; quit;

*****;

PROC REG DATA = UTD.COLLIN;
MODEL MANH = OCCUP CHECKIN HOURS COMMON WINGS CAP ROOMS/vif collin;
where obs < 25;
RUN; QUIT;


*********************************************************;
** conagra sample **;


LIBNAME CA 'C:\mgrigsby\Other\school\UTD\Marketing Analytics Spring 2015\week3'; run;


proc means data = ca.tripsum13  noprint;
by panid;
output out = tripsum13_summed (drop = _type_ _freq_)
sum(bsktdol) = s_basket_dollar; run;



proc means data = ca.tripdet13   noprint;
by panid;
output out = tripdet13_summed (drop = _type_ _freq_)
sum(units coup_dol dol) = s_units s_coupondol s_dollars; run;













