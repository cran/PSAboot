## ----setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE----------
require(knitr)
opts_chunk$set(fig.width = 12,
			   fig.height = 8,
			   fig.align = 'center',
			   out.width = '100%',
			   echo = TRUE,
			   warning = FALSE,
			   message = FALSE)
require(PSAboot)

## -----------------------------------------------------------------------------
boot.M <- 10

## ----BootSizeWarning, echo=FALSE, results='asis'------------------------------
if(boot.M < 100) {
	cat(paste0('**NOTE: This vignette uses ', boot.M, ' bootstrap samples. It is ',
	    'generally recommended to use at least 100 bootstrap samples or more for final publication.**'))
}

## ----install, eval=FALSE------------------------------------------------------
#  devtools::install_github('jbryer/PSAboot')

## ----defineCustomFunction-----------------------------------------------------
boot.matching.1to3 <- function(Tr, Y, X, X.trans, formu, ...) {
	return(boot.matching(Tr=Tr, Y=Y, X=X, X.trans=X.trans, formu=formu, M=3, ...))
}

## ----lalonde.load-------------------------------------------------------------
data(lalonde, package='Matching')
table(lalonde$treat)

## ----lalonde.boot, cache=FALSE------------------------------------------------
lalonde.formu <- treat~age + I(age^2) + educ + I(educ^2) + black +
             hisp + married + nodegr + re74  + I(re74^2) + re75 + I(re75^2) +
             u74 + u75
boot.lalonde <- PSAboot(Tr = lalonde$treat, 
						Y = lalonde$re78,
						X = lalonde,
						formu = lalonde.formu,
						M = 100, 
						seed = 2112)

## ----lalondeSummary-----------------------------------------------------------
summary(boot.lalonde)

## ----lalonde.plot-------------------------------------------------------------
plot(boot.lalonde)

## ----lalonde.histogram, warning=FALSE, message=FALSE--------------------------
hist(boot.lalonde)

## ----lalonde.boxplot----------------------------------------------------------
boxplot(boot.lalonde)

## ----lalonde.matrixplot, warning = FALSE--------------------------------------
matrixplot(boot.lalonde)

## ----lalonde.balance, cache=FALSE---------------------------------------------
lalonde.bal <- balance(boot.lalonde)
lalonde.bal

## ----lalonde.balance.plot-----------------------------------------------------
plot(lalonde.bal)

## ----lalonde.balance.boxplot--------------------------------------------------
boxplot(lalonde.bal)

## ----tutoring.setup-----------------------------------------------------------
require(TriMatch)
require(PSAboot)
data(tutoring, package='TriMatch')
tutoring$treatbool <- tutoring$treat != 'Control'
covs <- tutoring[,c('Gender', 'Ethnicity', 'Military', 'ESL', 'EdMother', 'EdFather',
					'Age', 'Employment', 'Income', 'Transfer', 'GPA')]

table(tutoring$treatbool)

## ----tutoring.psaboot, cache=FALSE--------------------------------------------
tutoring.boot <- PSAboot(Tr=tutoring$treatbool, 
						 Y=tutoring$Grade, 
						 X=covs, 
						 seed=2112,
						 M=boot.M,
						 control.sample.size=918, control.replace=TRUE,
						 treated.sample.size=224, treated.replace=TRUE,
						 methods=c('Stratification'=boot.strata,
						 		  'ctree'=boot.ctree,
						 		  'rpart'=boot.rpart,
						 		  'Matching'=boot.matching,
						 		  'Matching-1-to-3'=boot.matching.1to3,
						 		  'MatchIt'=boot.matchit)
)
summary(tutoring.boot)

## ----tutoring.plot------------------------------------------------------------
plot(tutoring.boot)

## ----tutoring.histogram, message=FALSE, warning=FALSE-------------------------
hist(tutoring.boot)

## ----tutoring.boxplot---------------------------------------------------------
boxplot(tutoring.boot)

## ----tutoring.matrixplot, fig.width = 12, fig.height = 12---------------------
matrixplot(tutoring.boot)

## ----tutroing.balance---------------------------------------------------------
tutoring.bal <- balance(tutoring.boot)
tutoring.bal
plot(tutoring.bal)
boxplot(tutoring.bal)

