# FieldTrials AID
**Autor:** Daniel Perez (Maestría Ciencia de Datos, Universidad Austral)

## Introducción

La aplicación **FieldTrials AID** ha sido diseñada para optimizar la visualizacion y comunicación de ensayos agronomicos en el cultivos de maíz y soja mediante. Esta app de shiny busca proporcionar una solución integral para los agronomos de campo, técnicos y productores, facilitando la toma de decisiones en el manejo agronómico basado en datos. 

> **Nota:** Esta versión se encuentra exclusivamente para el trabajo práctico N°2 de la materia *Análisis Introductoria a la Ciencia de Datos* (no fue publicada). Los datos solo son a modo de ejemplo y no representan ninguna aproximación a lo que ocurre en el lote agrícola. Todo el desarrollo corresponde al autor.

---

## Problema

**AGD** es una de las principales empresas exportadoras de aceites envasados del país, participando del 30% del total exportado desde Argentina.
Con una actividad exportadora en constante expansión, la empresa cuenta, hoy, con una importante red global de operaciones que permite organizar y eficientizar su presencia en los 5 continentes. 
Además, AGD tiene una explotación agropecuaria de aproximadamente 200 mil hectáreas de campos propios, arrendados o en asociación con terceros productores, en las zonas más aptas de la Argentina para la obtención de soja, girasol, maní, trigo, y maíz, materia prima que luego utiliza en la elaboración de sus productos. Para ello, necesita las últimas tecnologías que ayuden a incrementar los rendimientos de los cultivos.

La empresa cuenta con una **Red de ensayos agrícolas**, donde se ajustan diferentes prácticas de manejo que generan datos de diversas fuentes: climáticas, agronómicas y de maquinarias. 

En este trabajo se aborda únicamente una sección de esa red, que incluye:
- Ensayos de **fertilización y densidad variables** (Manejo Sitio Específico) en el cultivo de maíz.
- Productos foliares en soja y maíz (principalmente bioestimulantes, fungicidas y biocontroladores).

Sabemos lo cambiante que es el negocio agropecuario, asociado principalmente a cuestiones climáticas y a los precios de commodities. Por eso, permitir que el usuario final interactúe con precios, costos y rendimientos es fundamental para tomar decisiones relevantes para el negocio, como la selección de siembras o híbridos.

---
## Objetivo

1. **Optimizar el Análisis de Ensayos Agronómicos:**
   - Proveer una solución eficiente y precisa para analizar datos provenientes de ensayos agrícolas.
   - Evaluar tratamientos y su impacto en diferentes escenarios agronómicos, permitiendo decisiones mejor informadas.

2. **Facilitar la Visualización de Datos y Comparación Geoespacial:**
   - Crear una plataforma interactiva que permita explorar resultados de ensayos en un entorno geoespacial.
   - Filtrar resultados por variables clave como tipo de ensayo, ubicación y fechas.

3. **Proveer Herramientas Personalizadas para las Necesidades de la Empresa:**
   - Adaptar soluciones tecnológicas según los requerimientos específicos de cada organización.

4. **Ahorro de Tiempo y Mejora en la Eficiencia Operativa:**
   - Reducir el tiempo necesario para visualizar grandes volúmenes de datos y generar informes.
   - Fomentar una toma de decisiones estratégica basada en resultados confiables y predicciones precisas.

## Estructura de la Aplicación

La interfaz está organizada en diferente Menu (lateral izquierdo) dentro de un diseño de tablero de control (*shinydashboard*). Cada pestaña Menu un área clave del análisis de datos agronómicos:

- **AID:** Introducción a la aplicación, objetivos y guía de uso.
- **NxDxA:** Análisis de curvas de respuesta basadas en densidad, fertilización y ambiente.
- **Resumen NxDxA:** Resumen de resultados agrupados por híbrido y ambiente.
- **Densidad:** Análisis específico para evaluar la respuesta según densidades de siembra.
- **Comparativo:** Comparación de curvas de respuesta entre dos híbridos en un ambiente específico.
- **Ambientes:** Visualización geoespacial de prescripciones, cosechas y márgenes netos.
- **Foliares:** Visualización y resumen de resultados foliares.

## Detalles de cada Menú

### Menú AID

Introduce el propósito de la aplicación dentro de la materia *Introducción al Análisis Inteligente de Datos* y describe cómo cada funcionalidad contribuye a la comunicación agronómica basada en datos.

### Menú NxDxA

Los datos provienen de un dataset que contiene información del modelo utilizado para su análisis. Este modelo utiliza información geoespacial, incluyendo: mapas de prescripciones de siembra. mapas de cosecha y mapas de fertilización nitrogenada. El diseño de los Ensayos fueron realizados en parcelas divididas en ambientes previamente delimitados (zonas de alto y bajo potencial). Las observaciones se agrupan en: Parcelas Principales (PP) Asociadas con la dosis de fertilizante, Sub-Parcelas (SP): Asociadas con la densidad de siembra. Sub-Sub-Parcelas (SSP):Identificadas cuando hay múltiples observaciones por SP.

El modelo estadístico utilizado se define como:
Rendimiento = Media + F + D + F2 + D2 + F*D + Rep + PP<Rep + SP<PP<Rep + SSP<SP<PP<Rep +Error)

Por simplicidad y tiempo, ademas de **preservar los datos confidenciales de la empresa**, en este ejercicio solo utilizamos directamente el dataset que contiene los coeficientes del modelo para graficar y generar resultados dinámicos. 

Ejemplo de un diseño agronomico aplicado a campo:

![image](https://github.com/user-attachments/assets/9c9fca5f-709c-4926-84ee-5b3c59291fa2)

#### Funcionalidades del Menú NxDxA:
- **Entradas del Usuario:**
  - Selección de híbrido, ambiente y región.
  - Ajustes de precios y costos.
- **Resultados:**
  - Visualización de curvas de rendimiento y margen bruto.
  - Indicadores de Dosis Óptima Agronómica (DOA) y Dosis Óptima Económica (DOE).

### Menú Resumen NxDxA

Proporciona un resumen general de resultados por híbrido y ambiente

#### Funcionalidades del Menú Resumen NxDxA:
- **Características:**
  - Resúmenes tabulares interactivos.
  - Visualización de la eficiencia de uso de fertilizantes (EUF), accesible al hacer clic en un híbrido seleccionado.

### Menú Densidad

Enfocado exclusivamente en modelos de densidad (DxA).

- **Visualización:**
  - Gráficos interactivos para explorar respuestas a diferentes densidades.
  - Resumen de Dosis Óptima Agronómica (DOA) y Dosis Óptima Económica (DOE) por ambiente.

### Menú Comparativo

Permite comparar las curvas de densidad de dos híbridos.

- **Resultados:**
  - Comparación gráfica de rendimiento y margen bruto.
  - Identificación de DOA y DOE para ambos híbridos.

### Menú Ambientes

Muestra mapas geoespaciales de prescripciones, cosechas y márgenes netos.

- **Funcionalidades:**
  - Análisis de coincidencia entre ambientes de prescripción y cosecha.
  - Cálculo y visualización de márgenes netos para diferentes lotes.
  - Mapas interactivos con clasificación por clusters.

### Menú Foliares

Resumen y análisis de tratamientos foliares.

- **Visualización:**
  - Gráficos de barras para comparar tratamientos.
  - Tablas interactivas con diferencias porcentuales respecto al testigo.

---

## Beneficios Clave

1. **Toma de Decisiones Informada:**
   - Resultados confiables basados en datos integrados y modelos predictivos.

2. **Visualización Intuitiva:**
   - Representaciones gráficas claras para identificar patrones rápidamente.

3. **Flexibilidad y Personalización:**
   - Adaptación a las necesidades específicas de cada usuario o empresa.

4. **Ahorro de Tiempo:**
   - Automatización de cálculos y generación de informes.

---

## Conclusión

La aplicación **FieldTrials AID** representa un avance significativo en la integración de análisis agronómicos y tecnologías interactivas. Con herramientas para explorar, comparar y optimizar datos agronomicos, esta shiny está diseñada para ayudar a la comunicacion facil y practica. 

## Anexo

Para pobrar su funcionalidad en otro entorno, se logro publicar alojada en un servidor web para que el usuario puede ingresar directamente desde un link con una contraseña. 
https://agriculturaexperta.com/fieldtrials-aid-austral/
Contraseña: demoaustral


