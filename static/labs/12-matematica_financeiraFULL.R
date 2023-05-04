## EST053 - Métodos Computacionais para Análise de Risco
## Código da aula 12 - Revisão de Matemática Financeira




## Taxa de Juros e Taxa de Desconto
require(lifecontingencies)


0.05/(1+0.05)
interest2Discount(i=0.05)

## Taxa Nominal e Efetiva
nominal2Real(i=0.06, k=12)
real2Nominal(i=0.06, k=12)

# converter taxa de desconto efetiva para nominal
real2Nominal(i=0.04, k=12, type="discount") 


## Exercício 1
##


1000*(1-0.06/2)^12

annualDiscount = nominal2Real(i=0.06, k=2, type="discount")
i = discount2Interest(annualDiscount)
presentValue(cashFlows=1000, timeIds=6, interestRates=i, probabilities=1)



## Exercício 2
##


pg1 = c(-500,100,200,300,250)
t1 = 0:4
(VP1 = presentValue(cashFlows=pg1, t=t1, i=0.05))

pg2 = c(-700,1000)
t2 = c(0,2)
(VP2 = presentValue(cashFlows=pg2, t=t2, i=0.05))










## Exercício 3
##


i = 0.09
n = 5
100*(1-(1+i)^(-n))/i

100*annuity(i=0.09, n=5, type="immediate")

i = 0.06
10*1/i

10*annuity(i=0.06, n=Inf)

i = 0.06
n = 5
5000*(1+i)*(1-(1+i)^(-n))/i

5000*annuity(i=0.06, n=5, type="due")




## Exercício 4
## 


annuity(i=0.06, n=5, type="due")*5000*1.06^5
5000*accumulatedValue(i=0.06, n=5,type="due")


## Exercício 5
## 


C = 100000
R = C/accumulatedValue(i=0.05,n=10)
R


## Exercício 6
## 


480*annuity(i=0.02,n=16)+ 20*increasingAnnuity(i=0.02,n=16)

(480*annuity(i=0.02,n=16)+ 20*increasingAnnuity(i=0.02,n=16))*1.02^16


## Exercício 7
## 


1200*annuity(i=0.06,n=12,m=1,type="immediate")

1200*annuity(i=0.06,n=12,m=2,type="due")
