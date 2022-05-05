%macro
table1(dataset,directory,response,response_format,predictors_all,predictors_cou
ntable,
predictors_continuous,predictors_categorical,categorical_formats);

data _null_; call symput('B',trim(left(input("A0",$hex2.)))); run; %let c=&b&b&b;

/*** LOOP OVER ALL PREDICTORS ***/
%do i=1 %to %sysfunc(countw(&predictors_all));
%let predictor=%scan(&predictors_all,&i);

/*** CATEGORICAL PREDICTORS ***/
%if %eval(%sysfunc(findw(&predictors_categorical,&predictor," ",E))>0) %then %do;
%let
format=%scan(&categorical_formats,%sysfunc(findw(&predictors_categorical,&predictor,"
",E)));
/* summary statistics */
ods output CrossTabFreqs=ProcFreq&predictor;
proc freq data=&directory..&dataset; tables &predictor*&response; run;
ods output close;
data ProcFreq&predictor; set ProcFreq&predictor;
if _TYPE_=10 then &response=-99; /* -99 for Overall */
select;
4
when (_TYPE_=11) Freqdisplay=cat(put(FREQUENCY,comma8.),"
(",strip(put(COLPERCENT,percent_best.)),"%)");
when (_TYPE_=10) Freqdisplay=cat(put(FREQUENCY,comma8.),"
(",strip(put(PERCENT,percent_best.)),"%)");
otherwise Freqdisplay="";
end;
run;
proc sort data=ProcFreq&predictor; by &predictor &response; run;
proc transpose data=ProcFreq&predictor out=Freqs&predictor prefix=frequency;
by &predictor; id &response; var Freqdisplay; where &response NE .; run;
proc sql;
select label into :label from sashelp.vcolumn
where libname="%upcase(&directory)" and memname="%upcase(&dataset)" and
upcase(name)="%upcase(&predictor)";
quit;
data Freqs&predictor; length predictor $98; drop _Name_;
set Freqs&predictor(rename=(&predictor=level));
predictor = upcase("&predictor");
select;
when (level=.) characteristic="&label";
otherwise
characteristic=cat("&c",put(level,%sysfunc(cat(&format,.))));
end;
run;
proc sql;
select min(level) into :min_level from freqs&predictor;
quit;
/* significance test */
proc freq data=&directory..&dataset;
tables &response*&predictor/chisq;
output out=p&predictor pchi;
run;
data p&predictor(keep=predictor P_PCHI level); length predictor $98; set p&predictor;
predictor=upcase("&predictor");
level=&min_level;
run;
/* merge summary and significance test */
data final&predictor;
merge Freqs&predictor p&predictor; by predictor level;
run;
proc append data=final&predictor base=final force; run;
proc datasets nolist;
delete ProcFreq&predictor Freqs&predictor p&predictor final&predictor;
quit;
%end;

/*** CONTINUOUS PREDICTORS ***/
%if %eval(%sysfunc(findw(&predictors_continuous,&predictor," ",E))>0) %then %do;
/* summary statistics */
proc sort data=&directory..&dataset; by &response; run;
proc means data=&directory..&dataset; by &response; var &predictor;
ods output summary=summarya&predictor;
run;
proc means data=&directory..&dataset; var &predictor;
ods output summary=summaryb&predictor;
run;
data summaryb&predictor; set summaryb&predictor;
&response=-99;
run;
data summary&predictor; set summarya&predictor summaryb&predictor;
Freqdisplay=cat(put(&predictor._Mean,comma6.2)," %sysfunc(byte(177))
",strip(put(&predictor._StdDev,comma6.2)));
run;
5
proc transpose data=summary&predictor out=Freqs&predictor prefix=frequency;
id &response; var Freqdisplay;
run;
proc sql;
select label into :label from sashelp.vcolumn
where libname="%upcase(&directory)" and memname="%upcase(&dataset)" and
upcase(name)="%upcase(&predictor)";
quit;
data Freqs&predictor; length predictor $98; drop _Name_; set Freqs&predictor;
predictor = upcase("&predictor");
characteristic="&label";
level=.; /* avoids an error from proc append */
run;
/* significance test */
ods output overallanova=p&predictor;
proc anova data=&directory..&dataset;
class &response; model &predictor=&response;
run; quit;
ods output close;
data p&predictor(keep=predictor P_PCHI); set p&predictor(rename=(probf=P_PCHI));
predictor=upcase("&predictor"); where source='Model';
run;
/* merge summary and significance test */
data final&predictor;
merge Freqs&predictor p&predictor; by predictor;
run;
proc append data=final&predictor base=final force; run;
proc datasets nolist;
delete summarya&predictor summaryb&predictor summary&predictor freqs&predictor
p&predictor final&predictor;
quit;
%end;

/*** COUNTABLE PREDICTORS ***/
%if %eval(%sysfunc(findw(&predictors_countable,&predictor," ",E))>0) %then %do;
/* summary statistics */
proc sort data=&directory..&dataset; by &response; run;
proc means data=&directory..&dataset q1 median q3;
by &response; var &predictor;
ods output summary=summarya&predictor;
run;
proc means data=&directory..&dataset q1 median q3;
var &predictor;
ods output summary=summaryb&predictor;
run;
data summaryb&predictor; set summaryb&predictor;
&response=-99;
run;
data summary&predictor; set summarya&predictor summaryb&predictor;
Freqdisplay=catt(trim(put(&predictor._Median,comma4.)),"
(",trim(put(&predictor._Q1,comma4.)),",",trim(put(&predictor._Q3,comma4.)),")");
run;
proc transpose data=summary&predictor out=Freqs&predictor prefix=frequency;
id &response; var Freqdisplay;
run;
proc sql;
select label into :label from sashelp.vcolumn
where libname="%upcase(&directory)" and memname="%upcase(&dataset)" and
upcase(name)="%upcase(&predictor)";
quit;
data Freqs&predictor; length predictor $98; drop _Name_; set Freqs&predictor;
predictor = upcase("&predictor");
characteristic="&label";
6
level=.; /* avoids an error from proc append */
run;
/* significance test */
proc npar1way data=&directory..&dataset wilcoxon;
var &predictor; class &response;
output out=p&predictor wilcoxon;
run;
data p&predictor(keep=predictor P_PCHI); length predictor $98; set
p&predictor(rename=(P_KW=P_PCHI));
predictor=upcase("&predictor");
run;
/* merge summary and significance test */
data final&predictor;
merge Freqs&predictor p&predictor; by predictor;
run;
proc append data=final&predictor base=final force; run;
proc datasets nolist;
delete summarya&predictor summaryb&predictor summary&predictor freqs&predictor
p&predictor final&predictor;
quit;
%end;
%end;

/*** CREATE COLUMN LABEL MACRO VARIABLES ***/
ods output OneWayFreqs=OneWay&response;
proc freq data=&directory..&dataset;
tables &response;
run;
ods output close;
proc sql;
select &response, &response, put(&response,%sysfunc(cat(&response_format.,.))),
cats(put(Frequency,comma8.))
into :response_levels separated by ' ',:resplvl1-:resplvl999,:resplbl1-
:resplbl999,:respcum1-:respcum999
from OneWay&response;
quit;
proc sql;
select cats(put(max(CumFrequency),comma8.)) into :overall_count from OneWay&response;
quit;
proc datasets nolist; delete OneWay&response; quit;

/*** CREATE FINAL REPORT ***/
ods listing; title; footnote; ods listing close;
ods rtf;
%let st=style(column)=[just=center cellwidth=2.8 cm vjust=bottom font_size=8.5 pt]
style(header)=[just=center font_size=8.5 pt];
options orientation=portrait missing=' ';
proc report data=final nowd style=[cellpadding=6 font_size=8.5 pt rules=none];
column (characteristic frequency_99
%do j=1 %to %sysfunc(countw(&response_levels)); frequency&&resplvl&j %end;
P_PCHI);
define characteristic / display " " ;
define frequency_99 / display "{Overall\line (N=&overall_count)}" &st ;
%do k=1 %to %sysfunc(countw(&response_levels));
define frequency&&resplvl&k / display "{&&resplbl&k\line (N=&&respcum&k)}" &st;
%end;
define P_PCHI / display "{p-value}" &st format=pvalue_best.;
run;
ods rtf close;

%mend table1;