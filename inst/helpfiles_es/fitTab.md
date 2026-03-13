## Ajustar distribuciones


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
