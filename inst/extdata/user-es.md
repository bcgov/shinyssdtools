Esta aplicación **ajusta distribuciones de sensibilidad de especies a datos de concentración**. La aplicación está construida a partir del paquete R [ssdtools](https://github.com/bcgov/ssdtools) y comparte la misma funcionalidad.
Puede encontrar más información sobre los métodos de ssdtools en [artículos en el sitio web](https://bcgov.github.io/ssdtools/articles/).

*Consejo: Busque y haga clic en los íconos de información en toda la aplicación para encontrar más información sobre una entrada en particular.*

### Paso 1: Proporcionar datos

* Los datos deben proporcionarse para **solo un químico** a la vez.
* Cada especie debe tener solo un valor de concentración.
* Los datos deben tener **al menos una columna** que contenga **al menos 6 valores de concentración positivos y no faltantes**.
* Opcionalmente, se pueden incluir columnas de **especies y grupo**, que se utilizan para etiquetar y colorear la salida del gráfico, respectivamente.
* Se aceptan columnas adicionales, pero no son utilizadas por ninguna función.


<center>

Concentración&nbsp;&nbsp; | Especies&nbsp;&nbsp; | Grupo &nbsp;
--- | --- | ---
2.1 | Oncorhynchus mykiss &nbsp; | Pez
2.4 | Ictalurus punctatus &nbsp;| Pez
4.1 | Micropterus salmoides &nbsp;| Pez
10  | Brachydanio rerio &nbsp;| Pez
15.6 | Carassius auratus &nbsp;| Pez
18.3 | Pimephales promelas &nbsp;| Pez
6 | Daphnia magna &nbsp;| Invertebrado
10 | Opercularia bimarginata &nbsp;| Invertebrado

</center>

Hay tres opciones para proporcionar datos a la aplicación:

1. **Usar el conjunto de datos de demostración de boro**.
    - Vista previa rápida de la funcionalidad de la aplicación en un conjunto de datos que 'funciona'.
    - Cita: [Consejo Canadiense de Ministros del Medio Ambiente. 2009. Directrices canadienses de calidad del agua para la protección de la vida acuática: Boro. En: Directrices de calidad ambiental canadienses, 2009, Consejo Canadiense de Ministros del Medio Ambiente, Winnipeg.](http://ceqg-rcqe.ccme.ca/download/en/324/)
2. **Cargar un archivo csv**.
    - No se aceptan formatos de archivo de Excel. Si tiene un archivo de Excel, intente exportar una hoja de cálculo a csv.
3. **Completar la tabla interactiva**.
    - Las columnas de Especies y Grupo son opcionales. Haga clic en una celda para comenzar a ingresar datos. Haga clic derecho en la tabla para eliminar/insertar filas o columnas. Los nombres de las columnas no se pueden cambiar.

Opcionalmente, ingrese un **nombre de sustancia tóxica** que se utilizará como título predeterminado del gráfico.

Finalmente, obtenga una vista previa de los datos proporcionados en la tabla en el lado derecho de la pestaña. Los datos se pueden descargar en formato CSV o XLSX.

### Paso 2: Ajustar distribuciones

1. Especifique **qué columna contiene valores de concentración**. La aplicación intenta adivinar qué columna contiene valores de concentración basándose en los nombres de las columnas de datos. Esto puede necesitar ser corregido.
2. **Seleccione o deseleccione distribuciones para ajustar los datos**.
El gráfico de distribuciones ajustadas incluye las estimaciones promedio del modelo.
Tenga en cuenta que si dos o más modelos tienen ajustes superpuestos, el soporte para esta forma de modelo estará sobreinflado en los parámetros promediados del modelo.
Consulte el artículo [aquí](https://bcgov.github.io/ssdtools/articles/distributions.html) para obtener más información.
Haga clic en `Actualizar Ajuste` para actualizar las salidas después de cambiar las distribuciones seleccionadas o la opción de reescalado de datos.
3. **Seleccionar si reescalar los datos**
Esto especifica si dejar los valores de concentración sin cambios (sin marcar) o reescalar los valores de concentración dividiendo por la media geométrica de los valores finitos positivos mínimos y máximos. El reescalado proporciona una mejor estabilidad numérica en los casos donde las distribuciones no logran ajustarse. Las estimaciones y las estadísticas de bondad de ajuste no se ven afectadas.
4. Formatee el gráfico usando las entradas en la barra lateral y **descargue el gráfico** en formato PNG o RDS y **la tabla de bondad de ajuste** en formato CSV o XLSX. Seleccione unidades para mostrarlas en el título del eje x.

Información adicional sobre la **tabla de bondad de ajuste**:
Las columnas en la tabla de bondad de ajuste son la distribución (dist), el número de parámetros (npars), el número de observaciones (nobs), la log-verosimilitud (log_lik), el Criterio de Información de Akaike (aic), el Criterio de Información de Akaike corregido para el tamaño de la muestra (aicc), las diferencias de los Criterios de Información (delta), las ponderaciones de los Criterios de Información (wt), el Criterio de Información Bayesiana (bic), la estadística de Anderson-Darling (ad), la estadística de Kolmogorov-Smirnov (ks), y la estadística de Cramer-von Mises (cvm).
La predicción es la estimación promediada del modelo (usando aicc) del ajuste.
La concentración de peligro porcentual es la concentración del químico que se predice que afectará ese porcentaje de las especies probadas.

### Paso 3: Predecir concentración de peligro o porcentaje de especies afectadas
1. Hay dos opciones: Estimar la **Concentración** que afecta/protege una fracción seleccionada de especies (%) O estimar la fracción de especies (%) afectada por una concentración seleccionada. Esto afecta el gráfico (línea punteada), el texto que se muestra debajo del gráfico y los cálculos de límites de confianza.
2. Seleccione el número de **muestras bootstrap utilizadas para calcular límites de confianza**. El número recomendado de muestras es 10.000, aunque esto puede llevar algún tiempo para procesar.
Seleccione un número menor de muestras bootstrap para reducir el tiempo de procesamiento.
Las distribuciones se tratan como una sola distribución de mezcla (en oposición a tomar la media) para el cálculo de estimaciones promediadas del modelo.
Las distribuciones no se tratan como una sola distribución para calcular intervalos de confianza, ya que esto aumenta considerablemente el tiempo de procesamiento.

3. Dado que los límites de confianza tardan en calcularse, no se calculan automáticamente; debe presionar el botón `Obtener CL`.
4. **Formatee el gráfico** usando varias entradas en la barra lateral y **descargue el gráfico** en formato PNG o RDS y **la tabla de límites de confianza** en formato CSV o XLSX.

### Paso 4: Obtener informe BCANZ
Genere un informe en formato HTML o PDF que incluya el gráfico de distribución ajustada, la tabla de bondad de ajuste, el gráfico de ajuste promediado del modelo y la tabla de concentraciones peligrosas/protectoras estimadas.
Cualquier opción seleccionada en la aplicación se incorporará al informe.

### Paso 5: Obtener código R

Copie el código R para reproducir las salidas de forma programática.
El código se genera dinámicamente en función de las entradas del usuario y las funciones ejecutadas dentro de la aplicación (por ejemplo, el código para generar límites de confianza aparecerá después de hacer clic en el botón `Obtener CL`).


