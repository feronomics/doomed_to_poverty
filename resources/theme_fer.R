# =============================================================
# theme_apa() — Tema reutilizable para ggplot2 con estilo APA
# =============================================================
# Características:
#  - Tipografía Times New Roman, tamaño base 12
#  - Panel cuadrado (aspect.ratio = 1)
#  - Fondo monocromático (blanco/negro, sin color)
#  - Líneas de grilla mayor visibles (gris tenue)
#  - Líneas de ejes más gruesas
#  - Tick marks ("thick marks") visibles y gruesos
#  - Funciones auxiliares para etiquetar el eje Y en Quetzales (Q)
# =============================================================

library(ggplot2)
library(scales)
library(openxlsx)

# --- 1. Registro de la fuente Times New Roman -----------------
# En Windows suele estar disponible directamente. En Mac/Linux
# conviene registrar la fuente con el paquete 'extrafont' o usar
# 'showtext'. Aquí se intenta cargar de forma segura.
.cargar_times <- function() {
  fam <- "Times New Roman"
  if (requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)) {
    # Si showtext está disponible, se asegura el render correcto
    try({
      sysfonts::font_add(family = "Times New Roman",
                         regular = "Times New Roman.ttf")
      showtext::showtext_auto()
    }, silent = TRUE)
  }
  fam
}

# --- 2. El tema ------------------------------------------------
theme_apa <- function(base_size = 12,
                       base_family = "Times New Roman") {

  # Color monocromático único para todos los elementos
  tinta <- "black"
  grilla <- "grey85"   # grilla mayor: visible pero tenue

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # ----- Panel cuadrado y fondo limpio -----
      aspect.ratio       = 1,
      panel.background   = element_rect(fill = "white", colour = NA),
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.border       = element_blank(),

      # ----- Grilla -----
      panel.grid.major   = element_line(colour = grilla, linewidth = 0.4),
      panel.grid.minor   = element_blank(),  # APA: solo grilla mayor

      # ----- Ejes: líneas más gruesas -----
      axis.line          = element_line(colour = tinta, linewidth = 0.9),

      # ----- Tick marks gruesos ("thick marks") -----
      axis.ticks         = element_line(colour = tinta, linewidth = 0.9),
      axis.ticks.length  = unit(4, "pt"),

      # ----- Texto de los ejes -----
      axis.text          = element_text(colour = tinta, size = base_size),
      axis.title         = element_text(colour = tinta, size = base_size),
      axis.title.x       = element_text(margin = margin(t = 8)),
      axis.title.y       = element_text(margin = margin(r = 8), angle = 90),

      # ----- Título y subtítulo (APA: alineados a la izquierda) -----
      plot.title         = element_text(size = base_size, face = "bold",
                                        hjust = 0, margin = margin(b = 6)),
      plot.subtitle      = element_text(size = base_size, hjust = 0,
                                        margin = margin(b = 8)),
      plot.caption       = element_text(size = base_size - 2, hjust = 0,
                                        margin = margin(t = 8)),

      # ----- Leyenda -----
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.title       = element_text(size = base_size),
      legend.text        = element_text(size = base_size),
      legend.position    = "right",

      # ----- Facetas (monocromáticas) -----
      strip.background   = element_rect(fill = "white", colour = tinta,
                                        linewidth = 0.9),
      strip.text         = element_text(colour = tinta, size = base_size),

      complete = TRUE
    )
}

# --- 3. Etiquetas del eje Y en Quetzales (Q) -------------------
# Formateador: antepone "Q" y usa separador de miles.
# Uso:  scale_y_continuous(labels = quetzal_format())
quetzal_format <- function(accuracy = 1, ...) {
  scales::label_dollar(prefix = "Q", accuracy = accuracy,
                        big.mark = ",", ...)
}

# Escala completa lista para usar en el eje Y.
# Uso:  + scale_y_quetzal()
scale_y_quetzal <- function( accuracy = 1) {
  scale_y_continuous(
                     labels = quetzal_format(accuracy = accuracy),
                     )
}

# --- 4. Etiquetas de ejes por defecto --------------------------
# Capa rápida para fijar labs de X e Y de forma consistente.
# Uso:  + labs_apa(x = "Año", y = "Ingresos")
labs_apa <- function(x = "Eje X", y = "Monto (Q)", ...) {
  labs(x = x, y = y, ...)
}

# Escala completa lista para usar en el eje X.
# Uso:  + scale_x_quetzal()
scale_x_quetzal <- function(accuracy = 1) {
  scale_x_continuous(
    labels = quetzal_format(accuracy = accuracy)
  )
}
