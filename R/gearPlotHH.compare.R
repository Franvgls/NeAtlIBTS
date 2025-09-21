#' Plot net opening vs depth from DATRAS or local HH file, with optional overlay
#'
#' @param Survey Nombre de la campaña en DATRAS (p.ej. "SP-NORTH") o un data.frame HH si getICES=FALSE
#' @param years Años a incluir
#' @param quarter Trimestre
#' @param overlayHH (opcional) data.frame con formato HH de la campaña en curso a superponer
#' @param getICES TRUE/FALSE si se deben bajar los datos de DATRAS
#' @param c.inta Intervalo confianza sweeps cortos
#' @param c.intb Intervalo confianza sweeps largos
#' @param col1 Color sweeps largos
#' @param col2 Color sweeps cortos
#' @param colOverlay Color puntos overlay
#' @param pF TRUE = grafica puntos, FALSE = solo líneas
#' @param ti TRUE = incluir título
#' @export
gearPlotHH.compare <- function(
  Survey, years, quarter,
  overlayHH = NULL,
  getICES = TRUE,
  c.inta = .8, c.intb = .3,
  col1 = "darkblue", col2 = "steelblue2", colOverlay = "red",
  pF = TRUE, ti = TRUE
) {
  
  # 1. Cargar datos base
  if (getICES) {
    dumb <- icesDatras::getDATRAS("HH", Survey, years, quarter)
    if (identical(dumb, FALSE)) stop("Combination Survey-Year-Quarter not available in DATRAS")
  } else {
    dumb <- Survey
    if (!all(years %in% unique(dumb$Year))) {
      stop("Some selected years are not in the HH data frame")
    }
  }
  dumb <- dplyr::filter(dumb, HaulVal != "I")
  dumb$sweeplngt <- factor(dumb$SweepLngt)
  
  # 2. Graficar base usando tu función original
  gearPlotHH.nodp(Survey = dumb, years = years, quarter = quarter,
                  c.inta = c.inta, c.intb = c.intb,
                  col1 = col1, col2 = col2,
                  getICES = FALSE, pF = pF, ti = ti)
  
  # 3. Superponer overlay si se pasa
  if (!is.null(overlayHH)) {
    overlayHH <- dplyr::filter(overlayHH, HaulVal != "I")
    points(Netopening ~ Depth, overlayHH, pch = 21, bg = colOverlay, col = colOverlay)
    legend("bottomleft", legend = "Overlay campaign", pch = 21,
           col = colOverlay, pt.bg = colOverlay, bty = "n")
  }
}
