# Dr. Álvarez
alvarez <-c(72, 92, 108, 97, 72, 69, 117, 87, 94, 109, 94, 96, 112, 80, 100, 81, 86, 113, 107, 77, 84, 99, 73, 69, 103, 103, 117, 100, 66, 66, 84)

# Dr. Benítez
benitez <- c(112, 123, 122, 122, 112, 85, 98, 105, 110, 112, 69, 86, 78, 117, 114, 84, 74, 103, 109, 100)

# Dr. Casas
casas <- c(73, 70, 117, 114, 111, 109, 92, 89, 80, 111, 122, 69, 72, 90, 69, 87, 111, 85, 82, 70)

# Dr. Domínguez
dominguez <- c(119, 76, 94, 74, 107, 115, 66, 94, 122, 68, 95, 88, 82, 114, 109, 105, 97, 95, 88, 101, 114, 110, 116, 124, 72, 120, 106)

#a
t.test(alvarez, mu = 130, alternative = "two.sided")


#b
var.test(casas,dominguez)
t.test(casas,dominguez,conf.level=0.95,alternative="greater",var.equal = T)

x1<-mean(casas)
x2<-mean(dominguez)
s12<-var(casas)
s22<-var(dominguez)
n1<-length(casas)
n2<-length(dominguez)

sp<- sqrt(((n1-1)*s12+(n2-1)*s22)/(n1+n2-2))
(x1-x2)/(sp*sqrt(1/n1+1/n2))

#c
 
numPacientesNoControladosDominguez <- sum(dominguez > 130 | dominguez < 80)
binom.test(numPacientesNoControladosDominguez, length(dominguez), p = 0.15, alternative = "greater")
prop.test(numPacientesNoControladosDominguez,length(dominguez),alternative="greater",p=0.15)



#d
var.test(casas,benitez)
t.test(casas,benitez,conf.level=0.95,alternative="two.sided",var.equal=T)



#e
alvarez2 <- c(62, 90, 100, 95, 74, 70, 107, 87, 95, 109, 95, 90, 102, 81, 105, 80, 
              86, 103, 105, 70, 85, 98, 75, 68, 100, 100, 110, 101, 65, 66, 83)
var.test(alvarez,alvarez2)

t.test(alvarez,alvarez2,paired=T,conf.level=0.95,alternative="less",var.equal=T)
