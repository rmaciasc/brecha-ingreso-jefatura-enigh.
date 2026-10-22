# Brecha de ingreso por sexo de la jefatura del hogar en México, 2022-2024

Análisis con la **ENIGH 2022 y 2024** (INEGI) de la brecha de ingreso per cápita entre hogares con jefatura femenina y masculina, y de si se ha cerrado entre ambos años. El análisis se desglosa por **fuente de ingreso** y por ámbito (**Nacional, Urbano y Rural**).

Proyecto para la materia *Estadística e Indicadores Sociales* (2027-1), Equipo 2.

## Pregunta de investigación

¿Existe una brecha de ingresos entre hogares con jefatura femenina y masculina, y se ha cerrado entre 2022 y 2024?

## Qué hace el script

Todo el análisis está en `analisis_brecha_jefatura.R` y se repite para los tres ámbitos:

| Paso | Contenido |
|------|-----------|
| 0 | Preparación de las bases: deflactación a pesos de 2024, ingreso per cápita, deciles nacionales y diseño muestral |
| 1 | Tamaño del grupo y precisión de las estimaciones (CV, criterio INEGI) |
| 2 | Ubicación de los hogares con jefa en la distribución del ingreso |
| 3 | La brecha en cada año (media y mediana, razón y diferencia Mujer/Hombre) |
| 4 | Descomposición de la desigualdad dentro y entre grupos (varianza, Theil, GE(0)) |
| 5 | Gini, Theil y curvas de Lorenz |
| 6 | Cambio 2022-2024 y prueba de significancia (z) |
| 7 | Composición del ingreso por fuente (por jefatura y por decil) |
| 8 | Ejercicio "con y sin" cada fuente: brecha, Gini y Theil dentro/entre |

Las cinco fuentes del ingreso corriente son: trabajo, rentas, transferencias, estimación del alquiler y otros ingresos.

## Datos

Los datos **no se incluyen** en el repositorio por su tamaño. Descarga las bases `concentradohogar` de la ENIGH 2022 y 2024 (formato Stata, `.dta`) desde el sitio de INEGI y guárdalas en la carpeta `datos/` con estos nombres:

```
datos/concentradohogar22.dta
datos/concentradohogar24.dta
```

Fuente: [ENIGH, INEGI](https://www.inegi.org.mx/programas/enigh/nc/2024/).

## Cómo correrlo

1. Instala R y RStudio.
2. Instala los paquetes (solo la primera vez):

   ```r
   install.packages(c("survey", "convey", "dplyr", "ggplot2", "haven",
                      "flextable", "scales", "boot", "Hmisc"))

   # dineq ya no está en CRAN; se instala desde el archivo histórico
   install.packages("https://cran.r-project.org/src/contrib/Archive/dineq/dineq_0.1.0.tar.gz",
                    repos = NULL, type = "source")
   ```
3. Abre el proyecto en RStudio (o cambia la línea `setwd()` por la carpeta del repositorio).
4. Corre `analisis_brecha_jefatura.R` completo.

## Resultados

El script crea la carpeta `resultados/` con:

- `resultados/cuadros/`: 14 cuadros en CSV y HTML.
- `resultados/figuras/`: 11 gráficas en PNG.

## Estructura del repositorio

```
.
├── analisis_brecha_jefatura.R
├── datos/                  # bases de la ENIGH (no incluidas)
├── resultados/
│   ├── cuadros/
│   └── figuras/
├── .gitignore
└── README.md
```

## Notas metodológicas

- Ingreso corriente trimestral del hogar dividido entre el número de integrantes, en pesos de 2024.
- Diseño muestral complejo (UPM, estratos y factor de expansión) con `survey` y `convey`.
- Deciles **nacionales** de hogares por ingreso per cápita, también en los ámbitos urbano y rural.
- Criterio de precisión de INEGI: CV < 15 % alta, 15 a 30 % moderada, mayor a 30 % baja.
- El paso 8 es un ejercicio contable: quita una fuente y supone que todo lo demás queda igual.
