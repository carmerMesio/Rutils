
##Gradient descent. Coger las variables respuesta y escalarlas dividiendo por el rango para definir la desv. (S) y en el numerador restar la media.
#Asi hacemos que el gradiente converga mas rapidamente pues todas las variables toman valores razonablemente semejantes.
#rango y media de la muestra i.

##Buscamos la tetha que minimiza una función de perdida J(tetha). Funcion de perdida J decrece con cada iteración.
##El algortimo CONVERGE si la función de costo J decrece en menos de 0.01 en una iteración.
##Gradiente es como en industrial que utilizavamos la derivada de la función para determinar si realmente habiamos llegado al mín o máx de la función
##evaluando si cerca del punto la pendiente se modificava.
##Alpha sera el parametro de aprendizaje que hara que converga J. Si el parametro es demasiado alto la función no convergerá haciendo una forma de U.
##Si es demasiado pequeño convergerá pero muy lentamente.

#Debugging gradient descent. Make a plot with number of iterations on the x-axis. Now plot the 
#cost function, J(θ) over the number of iterations of gradient descent. If J(θ) ever increases, then you probably need to decrease α.

#REgresión multivariante. Si por ejemplo creamos una función cubica de las variables explicativas es importante escalar los datos porque sinó obtendremos valores que
#oscilarán en una escala demasiado elevad 10^2=100 pero 10^3=1000.

