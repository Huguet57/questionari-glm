# questionari-glm
Qüestionari Estadística II

Dubtes I:
- *ModA Ap. 3: el p-value em surt 2.2e-16... L'enunciat posa 4 decimals, que he de posar, 0.0000?*
+ Sí, es posa 0,000.

- *ModB Ap. 1: quan em pregunten el valor de beta barret, que és un vector amb dos components, que he de donar? El del coeficient de Days?*
+ Sí, es dona el coeficient de Days.

- *ModC Ap. 5: Com es calcula la variància d'un predict?*
+ Es calcula fent, phi·V(mu), on phi és el dispersion parameter, V és particular de cada distribució i mu és E(Y) (o predict(m, type="response")).

Dubtes II:
1.- *ModA,B Ap. 5: quan pregunten el S[H | Days = X] he de multiplicar l'stand1ard error per la derivada també? Demanen l'standard error del predict type="response", no?*

2.- *Què és l'"estimation of the scale parameter obtained with the Perason statistic" i com es calcula amb l'R (o on es troba al summary)? És el mateix que el coeficient de Days de la beta barret? Ens estan preguntant el mateix?*

3.- *Com sabem si el model "fits the mean of H" amb l'anova? Si el p-value és menor a 0.05?*

4.- *Com sabem si existeix "homoscedasticity" en els Pearson residuals? Si el p-value del leveneTest dels residuals és menor a 0.05?*
