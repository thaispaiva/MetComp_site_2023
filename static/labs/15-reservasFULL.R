## EST053 - Métodos Computacionais para Análise de Risco
## Código da aula 15 - Reservas




require(lifecontingencies)

## carregar tabela atuarial
data("soa08Act")


## Exemplo 1 - reserva para seguro de vida inteira

## prêmio
(P = Axn(soa08Act,60)/axn(soa08Act,60))

## reserva t=10
(V = Axn(soa08Act,60+10)-P*axn(soa08Act,60+10))






## Exemplo 2 - reserva para seguro de vida temporário

## prêmio
## 

## reserva t=10
##


## prêmio
P = Axn(soa08Act,60,30)/axn(soa08Act,60,30)


## reserva t=10
(V = Axn(soa08Act,60+10,30-10)-P*axn(soa08Act,60+10,30-10))




## Gráfico da reserva com tempo

## 1) Crie uma função V para avaliar a reserva no tempo t

## 2) Avalie a função para t=0,...,30
## Dica: usar a função 'Vectorize'

## 3) Faça o gráfico


V = function(t) Axn(soa08Act,60+t,30-t)-P*axn(soa08Act,60+t,30-t)
t = 0:30
V = Vectorize(V)(t)
plot(t, V, type="b", pch=20, main="Reserva de seguro temporário por 30 anos")




## Exemplo 3 - reserva para seguro temporario com duração diferente para prêmios

## prêmio
P = Axn(soa08Act,x=25,n=40)/axn(soa08Act,x=25,n=5)

## reserva t=3
( V3 = Axn(soa08Act,x=25+3,n=40-3)-P*axn(soa08Act,x=25+3,n=5-3) )

## reserva t=10
(V10 = Axn(soa08Act,x=25+10,n=40-10) )



## Exemplo 4 - reserva para seguro dotal misto


## prêmio
(P = AExn(soa08Act, 60,20)/axn(soa08Act, 60,20))

## reserva t=10
( V = AExn(soa08Act, 60+10,20-10)-P*axn(soa08Act, 60+10,20-10) )



## Exemplo 5 - reserva para anuidade vitalícia diferida

## prêmio
P = axn(soa08Act,x=55,m=20)/axn(soa08Act,x=55,n=20)

## reserva t=10
( V10 = axn(soa08Act,x=55+10,m=20-10)-P*axn(soa08Act,x=55+10,n=20-10) )

## reserva t=30
( V30 = axn(soa08Act,x=55+30) )



## Fórmula recursiva


## função para encontrar solução para fórmula recursiva
recurrent = function(a,b,ufinal){
  s <- rev(cumprod(c(1, b)))
  return( ( rev(cumsum(s[-1]*rev(a))) + s[1]*ufinal )/rev(s[-1]) )
}






## Exemplo - seguro temporário
## encontrando a reserva usando a fórmula recursiva

## prêmio
P = Axn(soa08Act,60,30)/axn(soa08Act,60,30)

## Sequências de a e b
Vecta = Vectorize(function(t) Axn(soa08Act,t,1))(60+0:29) - P
Vectb = Vectorize(function(t) pxt(soa08Act,t,1))(60+0:29)/1.06

## Sequência da reserva com a fórmula recursiva
Vectv = c(recurrent(a=Vecta,b=Vectb,ufinal=0),0)


## gráfico das reservas
plot(0:30, Vectv, type="b", pch=20, main="Reserva de seguro temporário por 30 anos")

