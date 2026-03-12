## Estimar concentración de peligro

1. Hay dos opciones: Estimar la **Concentración** que afecta/protege una fracción seleccionada de especies (%) O estimar la fracción de especies (%) afectada por una concentración seleccionada. Esto afecta el gráfico (línea punteada), el texto que se muestra debajo del gráfico y los cálculos de límites de confianza.
2. Seleccione el número de **muestras bootstrap utilizadas para calcular límites de confianza**. El número recomendado de muestras es 10.000, aunque esto puede llevar algún tiempo para procesar.
Seleccione un número menor de muestras bootstrap para reducir el tiempo de procesamiento.
Las distribuciones se tratan como una sola distribución de mezcla (en oposición a tomar la media) para el cálculo de estimaciones promediadas del modelo.
Las distribuciones no se tratan como una sola distribución para calcular intervalos de confianza, ya que esto aumenta considerablemente el tiempo de procesamiento.

3. Dado que los límites de confianza tardan en calcularse, no se calculan automáticamente; debe presionar el botón `Obtener CL`.
4. **Formatee el gráfico** usando varias entradas en la barra lateral y **descargue el gráfico** en formato PNG o RDS y **la tabla de límites de confianza** en formato CSV o XLSX.
