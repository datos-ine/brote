# if (!requireNamespace("munsell", quietly = TRUE)) {
#   install.packages("munsell", repos = "https://webr.r-wasm.org")
# }
library(munsell)

library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(yaml)
library(readr)
library(shinyjs)

debug = FALSE

# =========================================================
# JAVASCRIPT PERSONALIZADO PARA DESCARGAS LOCALES
# =========================================================
# Este script crea un link invisible y descarga el contenido como un "Blob"
# Esto funciona incluso si el Service Worker está desactivado.
js_download_script <- "
shinyjs.downloadFile = function(params) {
    var element = document.createElement('a');
    var blob = new Blob([params.content], {type: 'text/html'});
    element.setAttribute('href', window.URL.createObjectURL(blob));
    element.setAttribute('download', params.filename);
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
};
"


# =========================
# FUNCIONES AUXILIARES
# =========================

colapsar_errores <- function(x) {
  if (length(x) == 0) return("")
  paste("•", x, collapse = "\n")
}

determinar_tipo_variable <- function(col, tipo_especificado = NA) {
  if (length(tipo_especificado) > 1) {
    tipo_especificado <- tipo_especificado[1]
  }
  
  if (length(tipo_especificado) >= 1 && !is.na(tipo_especificado[1])) {
    if (tipo_especificado[1] == "cuantitativa") return("cuantitativa")
    if (tipo_especificado[1] == "cualitativa") return("cualitativa")
  }
  
  if (!is.numeric(col)) return("cualitativa")
  
  valores_no_na <- col[!is.na(col)]
  if (length(valores_no_na) == 0) return("cualitativa")
  
  n_unicos <- length(unique(valores_no_na))
  if (n_unicos <= 10) return("cualitativa")
  
  return("cuantitativa")
}


# =========================
# FUNCIÓN: CREAR VARIABLES INTERMEDIAS
# =========================

crear_variables_intermedias <- function(datos, config_variables, vars_yaml = NULL) {
  datos_mod <- datos
  
  if (!is.null(config_variables) && !is.null(config_variables$calculo_edad)) {
    tryCatch({
      # Configuración para cálculo de edad
      config_edad <- config_variables$calculo_edad
      
      # Obtener nombre de la variable de fecha de nacimiento
      if (!is.null(config_edad$fecha_nacimiento)) {
        var_fecha_nac <- config_edad$fecha_nacimiento
        
        # Determinar fecha de referencia (fecha del brote)
        if (!is.null(config_edad$fecha_brote)) {
          # Posibilidad 1: fecha_brote es el nombre de una variable en los datos
          if (is.character(config_edad$fecha_brote) && config_edad$fecha_brote %in% names(datos_mod)) {
            # Usar la fecha máxima de esa variable como fecha de referencia
            fecha_brote <- max(as.Date(datos_mod[[config_edad$fecha_brote]]), na.rm = TRUE)
            message(sprintf("Usando fecha máxima de '%s' como referencia: %s", 
                           config_edad$fecha_brote, fecha_brote))
          } 
          # Posibilidad 2: fecha_brote es una fecha fija en formato string
          else if (is.character(config_edad$fecha_brote)) {
            tryCatch({
              fecha_brote <- as.Date(config_edad$fecha_brote)
              message(sprintf("Usando fecha de brote fija: %s", fecha_brote))
            }, error = function(e) {
              # Si no se puede parsear como fecha y no es una columna, error
              fecha_brote <<- Sys.Date()
              warning(sprintf("No se pudo interpretar fecha_brote '%s'. Usando fecha actual.", 
                             config_edad$fecha_brote))
            })
          } else {
            # Posibilidad 3: fecha_brote podría ser una referencia a otra variable del YAML
            # (ej: variable de consumo o fecha de inicio)
            if (!is.null(vars_yaml)) {
              # Buscar variable de tiempo o consumo
              if (!is.null(vars_yaml$variable_tiempo_inicio) && 
                  vars_yaml$variable_tiempo_inicio %in% names(datos_mod)) {
                fecha_brote <- max(as.Date(datos_mod[[vars_yaml$variable_tiempo_inicio]]), na.rm = TRUE)
                message(sprintf("Usando fecha máxima de '%s' (variable_tiempo_inicio) como referencia: %s", 
                               vars_yaml$variable_tiempo_inicio, fecha_brote))
              } else if (!is.null(vars_yaml$exposicion)) {
                # Buscar entre las variables de exposición alguna que pueda ser fecha
                # (esto es más heurístico)
                fechas_posibles <- character(0)
                for (exp in vars_yaml$exposicion) {
                  if (exp %in% names(datos_mod) && 
                      any(grepl("fecha|date|consumo", tolower(exp)))) {
                    fechas_posibles <- c(fechas_posibles, exp)
                  }
                }
                if (length(fechas_posibles) > 0) {
                  # Tomar la primera fecha posible y usar su máximo
                  fecha_brote <- max(as.Date(datos_mod[[fechas_posibles[1]]]), na.rm = TRUE)
                  message(sprintf("Usando fecha máxima de '%s' como referencia: %s", 
                                 fechas_posibles[1], fecha_brote))
                } else {
                  # Si no hay nada, usar fecha actual
                  fecha_brote <- Sys.Date()
                  warning("No se pudo determinar fecha de referencia. Usando fecha actual.")
                }
              } else {
                fecha_brote <- Sys.Date()
                warning("No se especificó fecha_brote válida en YAML. Usando fecha actual.")
              }
            } else {
              fecha_brote <- Sys.Date()
              warning("No se pudo determinar fecha de referencia. Usando fecha actual.")
            }
          }
        } else {
          # Si no hay fecha_brote especificada
          if (!is.null(vars_yaml$variable_tiempo_inicio) && 
              vars_yaml$variable_tiempo_inicio %in% names(datos_mod)) {
            fecha_brote <- max(as.Date(datos_mod[[vars_yaml$variable_tiempo_inicio]]), na.rm = TRUE)
            message(sprintf("Usando fecha máxima de '%s' como referencia: %s", 
                           vars_yaml$variable_tiempo_inicio, fecha_brote))
          } else {
            fecha_brote <- Sys.Date()
            warning("No se especificó fecha_brote. Usando fecha actual.")
          }
        }
        
        if (var_fecha_nac %in% names(datos_mod)) {
          # Convertir fecha de nacimiento a Date
          datos_mod[[var_fecha_nac]] <- tryCatch({
            as.Date(datos_mod[[var_fecha_nac]])
          }, error = function(e) {
            # Intentar otros formatos comunes
            as.Date(datos_mod[[var_fecha_nac]], format = "%d/%m/%Y")
          })
          
          # Calcular edad en años
          datos_mod$calculo_edad <- as.integer(
            floor(
              as.numeric(
                difftime(fecha_brote, datos_mod[[var_fecha_nac]], units = "days")
              ) / 365.25
            )
          )
          
          message(sprintf("Variable 'calculo_edad' creada usando fecha de referencia: %s", fecha_brote))
          
          # Verificar si hay edades negativas o extremas
          edades_negativas <- sum(datos_mod$calculo_edad < 0, na.rm = TRUE)
          if (edades_negativas > 0) {
            warning(sprintf("Se encontraron %d edades negativas (posible error en fechas)", 
                           edades_negativas))
          }
          
          edades_extremas <- sum(datos_mod$calculo_edad > 120, na.rm = TRUE)
          if (edades_extremas > 0) {
            warning(sprintf("Se encontraron %d edades > 120 años (posible error en fechas)", 
                           edades_extremas))
          }
        }
      }
    }, error = function(e) {
      warning(sprintf("Error al crear calculo_edad: %s", e$message))
    })
  }
  
  # Crear grupo etario cada 5 años
  if (!is.null(config_variables$grupo_etario_5)) {
    tryCatch({
      # Obtener el nombre de la variable de edad desde la configuración
      var_edad_nombre <- config_variables$grupo_etario_5$variable_edad
      
      if (is.null(var_edad_nombre)) {
        stop("No se especificó variable_edad para grupo_etario_5")
      }
      
      # Verificar que la variable existe en los datos
      if (!var_edad_nombre %in% names(datos_mod)) {
        stop(sprintf("Variable de edad '%s' no encontrada en los datos", var_edad_nombre))
      }
      
      edad_var <- datos_mod[[var_edad_nombre]]
      var_nombre <- var_edad_nombre
      
      # Eliminar valores NA
      edad_var_clean <- edad_var[!is.na(edad_var)]
      if (length(edad_var_clean) == 0) {
        stop(sprintf("Variable '%s' no tiene valores válidos para crear grupos etarios", var_nombre))
      }
      
      # Asegurar que la edad sea numérica
      if (!is.numeric(edad_var)) {
        edad_var <- as.numeric(edad_var)
      }
      
      # Calcular límites para los grupos
      edad_max <- max(edad_var, na.rm = TRUE)
      breaks <- seq(0, ceiling(edad_max / 5) * 5, by = 5)
      
      # Si la edad máxima es menor que el último break, ajustar
      if (edad_max < max(breaks)) {
        breaks <- breaks[breaks <= ceiling(edad_max / 5) * 5]
      }
      
      # Asegurar que tenemos al menos 2 breaks
      if (length(breaks) < 2) {
        breaks <- c(0, 5)
      }
      
      # Crear grupos cada 5 años
      datos_mod$grupo_etario_5 <- cut(
        edad_var,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
      )
      
      # Para el último grupo, usar formato especial si es abierto
      niveles <- levels(datos_mod$grupo_etario_5)
      ultimo_nivel <- niveles[length(niveles)]
      if (grepl(paste0(max(breaks) - 5, "-", max(breaks) - 1), ultimo_nivel)) {
        # Reemplazar último nivel con formato abierto
        niveles[length(niveles)] <- paste0(max(breaks) - 5, "+")
        levels(datos_mod$grupo_etario_5) <- niveles
      }
      
      message(sprintf("Variable 'grupo_etario_5' creada usando '%s'", var_nombre))
      
    }, error = function(e) {
      warning(sprintf("Error al crear grupo_etario_5: %s", e$message))
    })
  }
  
  # Crear grupo etario cada 10 años
  if (!is.null(config_variables$grupo_etario_10)) {
    tryCatch({
      # Obtener el nombre de la variable de edad desde la configuración
      var_edad_nombre <- config_variables$grupo_etario_10$variable_edad
      
      if (is.null(var_edad_nombre)) {
        stop("No se especificó variable_edad para grupo_etario_10")
      }
      
      # Verificar que la variable existe en los datos
      if (!var_edad_nombre %in% names(datos_mod)) {
        stop(sprintf("Variable de edad '%s' no encontrada en los datos", var_edad_nombre))
      }
      
      edad_var <- datos_mod[[var_edad_nombre]]
      var_nombre <- var_edad_nombre
      
      # Eliminar valores NA
      edad_var_clean <- edad_var[!is.na(edad_var)]
      if (length(edad_var_clean) == 0) {
        stop(sprintf("Variable '%s' no tiene valores válidos para crear grupos etarios", var_nombre))
      }
      
      # Asegurar que la edad sea numérica
      if (!is.numeric(edad_var)) {
        edad_var <- as.numeric(edad_var)
      }
      
      # Calcular límites para los grupos
      edad_max <- max(edad_var, na.rm = TRUE)
      breaks <- seq(0, ceiling(edad_max / 10) * 10, by = 10)
      
      # Si la edad máxima es menor que el último break, ajustar
      if (edad_max < max(breaks)) {
        breaks <- breaks[breaks <= ceiling(edad_max / 10) * 10]
      }
      
      # Asegurar que tenemos al menos 2 breaks
      if (length(breaks) < 2) {
        breaks <- c(0, 10)
      }
      
      # Crear grupos cada 10 años
      datos_mod$grupo_etario_10 <- cut(
        edad_var,
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1)
      )
      
      # Para el último grupo, usar formato especial si es abierto
      niveles <- levels(datos_mod$grupo_etario_10)
      ultimo_nivel <- niveles[length(niveles)]
      if (grepl(paste0(max(breaks) - 10, "-", max(breaks) - 1), ultimo_nivel)) {
        # Reemplazar último nivel con formato abierto
        niveles[length(niveles)] <- paste0(max(breaks) - 10, "+")
        levels(datos_mod$grupo_etario_10) <- niveles
      }
      
      message(sprintf("Variable 'grupo_etario_10' creada usando '%s'", var_nombre))
      
    }, error = function(e) {
      warning(sprintf("Error al crear grupo_etario_10: %s", e$message))
    })
  }
  
  return(datos_mod)
}
# =========================
# FUNCIÓN: DESCRIPCIÓN UNIVARIADA 
# =========================

generar_descripcion_univariada <- function(datos, variables_sociodem, config_variables = NULL) {
  
  if (is.null(variables_sociodem) || length(variables_sociodem) == 0) {
    return(list(error = "No se definieron variables sociodemográficas"))
  }
  
  es_formato_nuevo <- FALSE
  nombres_vars <- character(0)
  tipos_vars <- character(0)
  
  if (is.list(variables_sociodem) && length(variables_sociodem) > 0) {
    primer_elemento <- variables_sociodem[[1]]
    
    if (is.list(primer_elemento) && 
        "nombre" %in% names(primer_elemento) && 
        "tipo" %in% names(primer_elemento)) {
      es_formato_nuevo <- TRUE
      
      nombres_vars <- sapply(variables_sociodem, function(x) x$nombre)
      tipos_vars <- sapply(variables_sociodem, function(x) {
        tipo <- x$tipo
        if (length(tipo) > 1) tipo <- tipo[1]
        return(tipo)
      })
    } else {
      nombres_vars <- as.character(variables_sociodem)
      tipos_vars <- rep(NA_character_, length(nombres_vars))
    }
  } else {
    nombres_vars <- as.character(variables_sociodem)
    tipos_vars <- rep(NA, length(nombres_vars))
  }
  
  # Añadir variables creadas automáticamente si están definidas en config_variables
  # y están presentes en los datos
  if (!is.null(config_variables)) {
    # Variables que queremos incluir si existen en los datos
    vars_a_incluir <- c("calculo_edad", "grupo_etario_5", "grupo_etario_10")
    
    for (var_creada in vars_a_incluir) {
      if (var_creada %in% names(datos) && !var_creada %in% nombres_vars) {
        nombres_vars <- c(nombres_vars, var_creada)
        
        # Determinar tipo según la variable
        if (var_creada == "calculo_edad") {
          tipos_vars <- c(tipos_vars, "cuantitativa")
        } else if (var_creada %in% c("grupo_etario_5", "grupo_etario_10")) {
          tipos_vars <- c(tipos_vars, "cualitativa")
        } else {
          tipos_vars <- c(tipos_vars, NA)
        }
      }
    }
  }
  
  vars_faltantes <- setdiff(nombres_vars, names(datos))
  if (length(vars_faltantes) > 0) {
    return(list(error = paste("Variables no encontradas:", paste(vars_faltantes, collapse = ", "))))
  }
  
  resultados <- list()
  
  for (i in seq_along(nombres_vars)) {
    var <- nombres_vars[i]
    tipo_especificado <- if (es_formato_nuevo) tipos_vars[i] else NA
    col_original <- datos[[var]]
    
    tipo_final <- determinar_tipo_variable(col_original, tipo_especificado)
    
    col_procesada <- col_original
    
    if (tipo_final == "cualitativa") {
      col_procesada <- as.factor(col_procesada)
    } else if (tipo_final == "cuantitativa") {
      if (!is.numeric(col_procesada)) {
        col_numerica <- suppressWarnings(as.numeric(as.character(col_procesada)))
        
        n_converted <- sum(!is.na(col_numerica))
        n_total <- sum(!is.na(col_procesada))
        
        if (n_total > 0) {
          proporcion_convertida <- n_converted / n_total
          
          if (proporcion_convertida < 0.5) {
            tipo_final <- "cualitativa"
            col_procesada <- as.factor(col_original)
          } else {
            col_procesada <- col_numerica
          }
        } else {
          tipo_final <- "cualitativa"
          col_procesada <- as.factor(col_original)
        }
      }
    }
    
    if (tipo_final == "cuantitativa") {
      n_validos <- sum(!is.na(col_procesada))
      
      if (n_validos > 0) {
        media_val <- mean(col_procesada, na.rm = TRUE)
        mediana_val <- median(col_procesada, na.rm = TRUE)
        sd_val <- if (n_validos > 1) sd(col_procesada, na.rm = TRUE) else NA
        min_val <- min(col_procesada, na.rm = TRUE)
        max_val <- max(col_procesada, na.rm = TRUE)
        q1_val <- quantile(col_procesada, 0.25, na.rm = TRUE)
        q3_val <- quantile(col_procesada, 0.75, na.rm = TRUE)
      } else {
        media_val <- NA
        mediana_val <- NA
        sd_val <- NA
        min_val <- NA
        max_val <- NA
        q1_val <- NA
        q3_val <- NA
      }
      
      resultados[[var]] <- list(
        variable = var,
        tipo = "numérica",
        tipo_original = class(col_original)[1],
        n = n_validos,
        n_faltantes = sum(is.na(col_procesada)),
        media = media_val,
        mediana = mediana_val,
        desv_std = sd_val,
        min = min_val,
        max = max_val,
        q1 = q1_val,
        q3 = q3_val
      )
    } else {
      if (!is.factor(col_procesada)) {
        col_procesada <- as.factor(col_procesada)
      }
      
      n_validos <- sum(!is.na(col_procesada))
      tabla <- table(col_procesada, useNA = "ifany")
      
      resultados[[var]] <- list(
        variable = var,
        tipo = "categórica",
        tipo_original = class(col_original)[1],
        n = n_validos,
        n_faltantes = sum(is.na(col_procesada)),
        n_categorias = length(levels(col_procesada)),
        categorias = levels(col_procesada),
        frecuencias = as.numeric(tabla),
        porcentajes = round(100 * prop.table(tabla), 1)
      )
    }
  }
  
  return(list(resultados = resultados, error = NULL))
}

# Función auxiliar para convertir resultados univariados a HTML para informe descargable
convertir_univariada_a_html <- function(resultados) {
  if (!is.null(resultados$error)) {
    return("<p class='alert alert-warning'>Error: No hay datos disponibles</p>")
  }
  
  html_parts <- lapply(resultados$resultados, function(res) {
    if (res$tipo == "numérica") {
      sprintf("
        <div style='margin-bottom: 20px;'>
          <h5><strong>%s</strong> (numérica%s)</h5>
          <table class='table table-sm table-bordered' style='width: auto;'>
            <tbody>
              <tr><td>N válidos</td><td>%d</td></tr>
              <tr><td>N faltantes</td><td>%d</td></tr>
              <tr><td>Media</td><td>%.2f</td></tr>
              <tr><td>Mediana</td><td>%.2f</td></tr>
              <tr><td>Desv. Est.</td><td>%.2f</td></tr>
              <tr><td>Mínimo</td><td>%.2f</td></tr>
              <tr><td>Máximo</td><td>%.2f</td></tr>
              <tr><td>Q1</td><td>%.2f</td></tr>
              <tr><td>Q3</td><td>%.2f</td></tr>
            </tbody>
          </table>
        </div>
      ",
      res$variable,
      if(!is.null(res$tipo_original)) paste0(", original: ", res$tipo_original) else "",
      res$n,
      res$n_faltantes,
      res$media,
      res$mediana,
      res$desv_std,
      res$min,
      res$max,
      res$q1,
      res$q3
      )
    } else {
      # Construir filas de la tabla
      filas_html <- paste(sapply(seq_along(res$categorias), function(i) {
        sprintf("<tr><td>%s</td><td>%d</td><td>%s%%</td></tr>",
                as.character(res$categorias[i]),
                res$frecuencias[i],
                res$porcentajes[i])
      }), collapse = "\n")
      
      sprintf("
        <div style='margin-bottom: 20px;'>
          <h5><strong>%s</strong> (categórica%s, %d categorías)</h5>
          <table class='table table-sm table-bordered' style='width: auto;'>
            <thead>
              <tr><th>Categoría</th><th>Frecuencia</th><th>Porcentaje</th></tr>
            </thead>
            <tbody>
              %s
            </tbody>
          </table>
        </div>
      ",
      res$variable,
      if(!is.null(res$tipo_original)) paste0(", original: ", res$tipo_original) else "",
      res$n_categorias,
      filas_html
      )
    }
  })
  
  return(paste(html_parts, collapse = "\n"))
}

# Función para convertir tasas de ataque a HTML para informe
convertir_tasas_a_html <- function(resultados_tasas) {
  if (is.null(resultados_tasas) || length(resultados_tasas) == 0) {
    return("<p class='alert alert-warning'>No se pudieron calcular tasas de ataque</p>")
  }
  
  tablas_html <- lapply(names(resultados_tasas), function(exposicion) {
    res <- resultados_tasas[[exposicion]]
    
    sprintf("
      <div style='margin-bottom: 25px;'>
        <h5><strong>%s</strong> (n=%d)</h5>
        <table class='table table-sm table-bordered' style='width: auto;'>
          <thead>
            <tr>
              <th></th>
              <th>Enfermos</th>
              <th>Total</th>
              <th>Tasa de ataque</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><strong>Expuestos</strong></td>
              <td>%d</td>
              <td>%d</td>
              <td>%s</td>
            </tr>
            <tr>
              <td><strong>No expuestos</strong></td>
              <td>%d</td>
              <td>%d</td>
              <td>%s</td>
            </tr>
            <tr>
              <td><strong>Riesgo Relativo (RR)</strong></td>
              <td colspan='3'>%s</td>
            </tr>
            <tr>
              <td><strong>Diferencia de Riesgo</strong></td>
              <td colspan='3'>%s</td>
            </tr>
          </tbody>
        </table>
      </div>
    ",
    exposicion,
    res$n_total,
    res$casos_expuestos,
    res$n_expuestos,
    ifelse(!is.na(res$tasa_expuestos), sprintf("%.1f%%", res$tasa_expuestos * 100), "N/A"),
    res$casos_no_expuestos,
    res$n_no_expuestos,
    ifelse(!is.na(res$tasa_no_expuestos), sprintf("%.1f%%", res$tasa_no_expuestos * 100), "N/A"),
    ifelse(!is.na(res$riesgo_relativo), sprintf("%.2f", res$riesgo_relativo), "No calculable"),
    ifelse(!is.na(res$diferencia_riesgo), sprintf("%.3f", res$diferencia_riesgo), "No calculable")
    )
  })
  
  return(paste(tablas_html, collapse = "\n"))
}

# Función para convertir odds ratio a HTML para informe
convertir_or_a_html <- function(resultados_or) {
  if (is.null(resultados_or) || length(resultados_or) == 0) {
    return("<p class='alert alert-warning'>No se pudieron calcular odds ratios</p>")
  }
  
  filas_html <- sapply(names(resultados_or), function(exposicion) {
    res <- resultados_or[[exposicion]]
    
    sprintf("<tr><td><strong>%s</strong></td><td>%.2f</td><td>%s</td><td>%s</td><td>%s</td></tr>",
            exposicion,
            res$odds_ratio,
            ifelse(!is.na(res$ic_95_inf), sprintf("[%.2f-%.2f]", res$ic_95_inf, res$ic_95_sup), "N/A"),
            ifelse(!is.na(res$chi_cuadrado), sprintf("%.2f", res$chi_cuadrado), "N/A"),
            ifelse(!is.na(res$p_valor), 
                   ifelse(res$p_valor < 0.001, "<0.001", sprintf("%.3f", res$p_valor)), 
                   "N/A")
    )
  })
  
  html_tabla <- sprintf("
    <table class='table table-sm table-bordered table-striped'>
      <thead>
        <tr>
          <th>Exposición</th>
          <th>Odds Ratio</th>
          <th>IC 95%%</th>
          <th>Chi²</th>
          <th>Valor p</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>
    <div class='alert alert-info' style='margin-top: 15px;'>
      <small>
        <strong>Interpretación:</strong>
        OR > 1: asociación positiva (mayor riesgo), OR < 1: asociación negativa (menor riesgo), 
        OR = 1: no asociación. IC 95%% que no incluye 1 indica significancia estadística.
      </small>
    </div>
  ", paste(filas_html, collapse = "\n"))
  
  return(html_tabla)
}
# ===========================================================================
# NUEVAS FUNCIONES PARA FRECUENCIA_COMBINADA_SINTOMAS
# ===========================================================================

# Función para calcular frecuencia combinada de síntomas
calcular_frecuencia_combinada_sintomas <- function(datos, vars_sintomas) {
  if (is.null(vars_sintomas) || length(vars_sintomas) == 0) {
    return(list(error = "No hay variables de síntomas definidas"))
  }
  
  # Extraer nombres de variables de síntomas
  nombres_sintomas <- sapply(vars_sintomas, function(s) {
    if (is.list(s) && !is.null(s$nombre)) {
      return(s$nombre)
    } else if (is.character(s)) {
      return(s)
    }
    return(NA)
  })
  
  # Filtrar NAs
  nombres_sintomas <- nombres_sintomas[!is.na(nombres_sintomas)]
  
  # Verificar que las variables existan en los datos
  sintomas_validos <- nombres_sintomas[nombres_sintomas %in% names(datos)]
  
  if (length(sintomas_validos) == 0) {
    return(list(error = "Ninguna variable de síntomas encontrada en los datos"))
  }
  
  # Convertir todas las variables a lógico
  datos_sintomas <- datos[, sintomas_validos, drop = FALSE]
  datos_sintomas <- as.data.frame(lapply(datos_sintomas, convertir_a_logico))
  
  # Crear combinaciones como string (ej: "fiebre + nauseas")
  datos_sintomas$combinacion <- apply(datos_sintomas, 1, function(row) {
    sintomas_presentes <- names(row)[row == TRUE & !is.na(row)]
    if (length(sintomas_presentes) == 0) {
      return("Ningún síntoma")
    } else {
      return(paste(sintomas_presentes, collapse = " + "))
    }
  })
  
  # Contar frecuencias de cada combinación
  tabla_freq <- as.data.frame(table(datos_sintomas$combinacion))
  colnames(tabla_freq) <- c("Combinacion_sintomas", "Frecuencia")
  
  # Calcular porcentaje
  tabla_freq$Porcentaje <- round(100 * tabla_freq$Frecuencia / sum(tabla_freq$Frecuencia), 2)
  
  # Ordenar de mayor a menor frecuencia
  tabla_freq <- tabla_freq[order(-tabla_freq$Frecuencia), ]
  rownames(tabla_freq) <- NULL
  
  return(tabla_freq)
}

# Función para convertir frecuencia combinada a HTML
convertir_frec_combinada_a_html <- function(tabla_freq) {
  if (is.null(tabla_freq)) {
    return("<p class='alert alert-warning'>No se pudo calcular la frecuencia combinada de síntomas</p>")
  }
  
  if (!is.null(tabla_freq$error)) {
    return(sprintf("<p class='alert alert-warning'>%s</p>", tabla_freq$error))
  }
  
  filas_html <- apply(tabla_freq, 1, function(row) {
    sprintf("<tr><td>%s</td><td>%s</td><td>%.2f%%</td></tr>",
            row["Combinacion_sintomas"],
            row["Frecuencia"],
            as.numeric(row["Porcentaje"]))
  })
  
  html_tabla <- sprintf("
    <table class='table table-sm table-bordered table-striped'>
      <thead>
        <tr>
          <th>Combinación de síntomas</th>
          <th>Frecuencia</th>
          <th>Porcentaje</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>
    <div class='alert alert-info' style='margin-top: 15px;'>
      <small>
        <strong>Nota:</strong> Esta tabla muestra las combinaciones de síntomas presentes en los casos,
        ordenadas de mayor a menor frecuencia.
      </small>
    </div>
  ", paste(filas_html, collapse = "\n"))
  
  return(html_tabla)
}


render_univariada <- function(resultados) {
  
  if (!is.null(resultados$error)) {
    return(div(
      class = "alert alert-warning",
      resultados$error
    ))
  }
  
  tablas <- lapply(resultados$resultados, function(res) {
    
    if (res$tipo == "numérica") {
      tagList(
        h5(strong(res$variable), 
           " (numérica", 
           if(!is.null(res$tipo_original)) paste0(", original: ", res$tipo_original), 
           ")"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "width: auto;",
          tags$tbody(
            tags$tr(tags$td("N válidos"), tags$td(res$n)),
            tags$tr(tags$td("N faltantes"), tags$td(res$n_faltantes)),
            tags$tr(tags$td("Media"), tags$td(round(res$media, 2))),
            tags$tr(tags$td("Mediana"), tags$td(round(res$mediana, 2))),
            tags$tr(tags$td("Desv. Est."), tags$td(round(res$desv_std, 2))),
            tags$tr(tags$td("Mínimo"), tags$td(round(res$min, 2))),
            tags$tr(tags$td("Máximo"), tags$td(round(res$max, 2))),
            tags$tr(tags$td("Q1"), tags$td(round(res$q1, 2))),
            tags$tr(tags$td("Q3"), tags$td(round(res$q3, 2)))
          )
        ),
        if (length(res$n_faltantes) == 1 && res$n_faltantes > 0) {
          div(
            class = "alert alert-light",
            style = "font-size: 0.8em; padding: 5px;",
            sprintf("%.1f%% de datos faltantes", 100 * res$n_faltantes / (res$n + res$n_faltantes))
          )
        }
      )
    } else {
      filas <- lapply(seq_along(res$categorias), function(i) {
        tags$tr(
          tags$td(as.character(res$categorias[i])),
          tags$td(res$frecuencias[i]),
          tags$td(paste0(res$porcentajes[i], "%"))
        )
      })
      
      tagList(
        h5(strong(res$variable), 
           " (categórica", 
           if(!is.null(res$tipo_original)) paste0(", original: ", res$tipo_original), 
           ", ", res$n_categorias, " categorías)"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "width: auto;",
          tags$thead(
            tags$tr(
              tags$th("Categoría"),
              tags$th("Frecuencia"),
              tags$th("Porcentaje")
            )
          ),
          tags$tbody(filas)
        ),
        if (res$n_faltantes > 0) {
          div(
            class = "alert alert-light",
            style = "font-size: 0.8em; padding: 5px;",
            sprintf("%.1f%% de datos faltantes", 100 * res$n_faltantes / (res$n + res$n_faltantes))
          )
        }
      )
    }
  })
  
  tagList(
    div(
      class = "alert alert-info",
      tags$small(
        "Nota: Las variables se clasifican como 'categóricas' o 'numéricas' basándose en ",
        "la especificación del YAML o reglas heurísticas. ",
        "Variables numéricas con pocos valores únicos pueden tratarse como categóricas."
      )
    ),
    tablas
  )
}

# =========================
# FUNCIONES PARA TASAS DE ATAQUE Y ODDS RATIO
# =========================

convertir_a_logico <- function(x) {
  if (is.character(x)) {
    normalizado <- toupper(trimws(x))
    normalizado <- chartr("ÁÉÍÓÚ", "AEIOU", normalizado)
    
    result <- rep(NA, length(normalizado))
    
    true_values <- c("VERDADERO", "TRUE", "T", "SI", "SÍ", "YES", "Y", "1", "V", "S")
    result[normalizado %in% true_values] <- TRUE
    
    false_values <- c("FALSO", "FALSE", "F", "NO", "N", "0")
    result[normalizado %in% false_values] <- FALSE
    
    return(result)
  } else if (is.logical(x)) {
    return(x)
  } else if (is.numeric(x)) {
    result <- rep(NA, length(x))
    result[x == 1] <- TRUE
    result[x == 0] <- FALSE
    return(result)
  } else if (is.factor(x)) {
    return(convertir_a_logico(as.character(x)))
  } else {
    return(as.logical(x))
  }
}

analizar_valores_logicos <- function(x, nombre = "variable") {
  if (is.character(x)) {
    valores_unicos <- unique(toupper(trimws(x)))
    cat(sprintf("\n%s: Valores únicos encontrados (%d):\n", nombre, length(valores_unicos)))
    print(valores_unicos)
    
    convertidos <- convertir_a_logico(x)
    cat(sprintf("Distribución después de conversión:\n"))
    print(table(convertidos, useNA = "always"))
  }
  return(invisible(NULL))
}

calcular_tasas_ataque <- function(datos, variable_evento, variables_exposicion, variable_participo = NULL, debug = FALSE) {
  
  if (debug) {
    cat("\n=== DEBUGGING TASAS DE ATAQUE ===\n")
    cat(sprintf("Variable evento: %s\n", variable_evento))
    if (variable_evento %in% names(datos)) {
      analizar_valores_logicos(datos[[variable_evento]], variable_evento)
    }
  }
  
  if (!is.null(variable_participo) && variable_participo %in% names(datos)) {
    datos <- datos |> 
      mutate(participa = convertir_a_logico(.data[[variable_participo]])) |>
      filter(participa == TRUE) |>
      select(-participa)
    
    if (debug) {
      cat(sprintf("\nFiltrado por participación (%s): %d filas después de filtrar\n", 
                  variable_participo, nrow(datos)))
    }
  }
  
  resultados <- list()
  
  for (exposicion in variables_exposicion) {
    if (!exposicion %in% names(datos)) {
      if (debug) cat(sprintf("\nExposición %s no encontrada en datos\n", exposicion))
      next
    }
    
    if (debug) {
      cat(sprintf("\n--- Analizando exposición: %s ---\n", exposicion))
      analizar_valores_logicos(datos[[exposicion]], exposicion)
    }
    
    datos_exp <- datos |>
      mutate(
        evento = convertir_a_logico(.data[[variable_evento]]),
        expos = convertir_a_logico(.data[[exposicion]])
      ) |>
      filter(!is.na(evento) & !is.na(expos))
    
    if (debug) {
      cat(sprintf("Filas después de filtrar NAs: %d\n", nrow(datos_exp)))
      cat("Tabla 2x2:\n")
      if (nrow(datos_exp) > 0) {
        tab <- table(
          Exposicion = datos_exp$expos,
          Evento = datos_exp$evento
        )
        print(tab)
      }
    }
    
    if (nrow(datos_exp) == 0) {
      if (debug) cat("No hay datos válidos para esta exposición\n")
      next
    }
    
    tab <- table(
      Exposicion = datos_exp$expos,
      Evento = datos_exp$evento
    )
    
    if (nrow(tab) >= 2 && ncol(tab) >= 2) {
      if ("TRUE" %in% rownames(tab) && "FALSE" %in% rownames(tab)) {
        expuestos_total <- sum(tab["TRUE", ])
        no_expuestos_total <- sum(tab["FALSE", ])
        
        tasa_expuestos <- ifelse(expuestos_total > 0, 
                                 tab["TRUE", "TRUE"] / expuestos_total, 
                                 NA)
        tasa_no_expuestos <- ifelse(no_expuestos_total > 0, 
                                    tab["FALSE", "TRUE"] / no_expuestos_total, 
                                    NA)
        
        rr <- ifelse(!is.na(tasa_no_expuestos) && tasa_no_expuestos > 0,
                     tasa_expuestos / tasa_no_expuestos,
                     NA)
        
        dr <- ifelse(!is.na(tasa_expuestos) && !is.na(tasa_no_expuestos),
                     tasa_expuestos - tasa_no_expuestos,
                     NA)
        
        resultados[[exposicion]] <- list(
          tabla = tab,
          n_expuestos = expuestos_total,
          n_no_expuestos = no_expuestos_total,
          casos_expuestos = tab["TRUE", "TRUE"],
          casos_no_expuestos = tab["FALSE", "TRUE"],
          tasa_expuestos = tasa_expuestos,
          tasa_no_expuestos = tasa_no_expuestos,
          riesgo_relativo = rr,
          diferencia_riesgo = dr,
          n_total = nrow(datos_exp)
        )
      } else {
        if (debug) cat("Tabla no tiene filas TRUE y FALSE\n")
      }
    } else {
      if (debug) cat(sprintf("Tabla no es 2x2: dimensiones %d x %d\n", nrow(tab), ncol(tab)))
    }
  }
  
  if (debug) cat(sprintf("\nTotal de exposiciones procesadas: %d\n", length(resultados)))
  
  return(resultados)
}

calcular_odds_ratio <- function(datos, variable_evento, variables_exposicion, variable_participo = NULL, debug = FALSE) {
  
  if (debug) {
    cat("\n=== DEBUGGING ODDS RATIO ===\n")
    cat(sprintf("Variable evento: %s\n", variable_evento))
    if (variable_evento %in% names(datos)) {
      analizar_valores_logicos(datos[[variable_evento]], variable_evento)
    }
  }
  
  if (!is.null(variable_participo) && variable_participo %in% names(datos)) {
    datos <- datos |> 
      mutate(participa = convertir_a_logico(.data[[variable_participo]])) |>
      filter(participa == TRUE) |>
      select(-participa)
    
    if (debug) {
      cat(sprintf("\nFiltrado por participación (%s): %d filas después de filtrar\n", 
                  variable_participo, nrow(datos)))
    }
  }
  
  resultados <- list()
  
  for (exposicion in variables_exposicion) {
    if (!exposicion %in% names(datos)) {
      if (debug) cat(sprintf("\nExposición %s no encontrada en datos\n", exposicion))
      next
    }
    
    if (debug) {
      cat(sprintf("\n--- Analizando exposición: %s ---\n", exposicion))
      analizar_valores_logicos(datos[[exposicion]], exposicion)
    }
    
    datos_exp <- datos |>
      mutate(
        evento = convertir_a_logico(.data[[variable_evento]]),
        expos = convertir_a_logico(.data[[exposicion]])
      ) |>
      filter(!is.na(evento) & !is.na(expos))
    
    if (debug) {
      cat(sprintf("Filas después de filtrar NAs: %d\n", nrow(datos_exp)))
      cat("Tabla 2x2:\n")
      if (nrow(datos_exp) > 0) {
        tab <- table(
          Exposicion = datos_exp$expos,
          Evento = datos_exp$evento
        )
        print(tab)
      }
    }
    
    if (nrow(datos_exp) == 0) {
      if (debug) cat("No hay datos válidos para esta exposición\n")
      next
    }
    
    tab <- table(
      Exposicion = datos_exp$expos,
      Evento = datos_exp$evento
    )
    
    if (nrow(tab) >= 2 && ncol(tab) >= 2) {
      if ("TRUE" %in% rownames(tab) && "FALSE" %in% rownames(tab)) {
        a <- tab["TRUE", "TRUE"]
        b <- tab["TRUE", "FALSE"]
        c <- tab["FALSE", "TRUE"]
        d <- tab["FALSE", "FALSE"]
        
        or <- ifelse(b > 0 && c > 0, (a * d) / (b * c), NA)
        
        if (!is.na(or) && a > 0 && b > 0 && c > 0 && d > 0) {
          se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
          log_or <- log(or)
          li <- exp(log_or - 1.96 * se_log_or)
          ls <- exp(log_or + 1.96 * se_log_or)
        } else {
          li <- NA
          ls <- NA
        }
        
        total <- a + b + c + d
        if (total > 0) {
          esperado_a <- (a + b) * (a + c) / total
          esperado_b <- (a + b) * (b + d) / total
          esperado_c <- (c + d) * (a + c) / total
          esperado_d <- (c + d) * (b + d) / total
          
          chi2 <- 0
          if (esperado_a > 0) chi2 <- chi2 + ((a - esperado_a)^2 / esperado_a)
          if (esperado_b > 0) chi2 <- chi2 + ((b - esperado_b)^2 / esperado_b)
          if (esperado_c > 0) chi2 <- chi2 + ((c - esperado_c)^2 / esperado_c)
          if (esperado_d > 0) chi2 <- chi2 + ((d - esperado_d)^2 / esperado_d)
          
          p_valor <- pchisq(chi2, df = 1, lower.tail = FALSE)
        } else {
          chi2 <- NA
          p_valor <- NA
        }
        
        resultados[[exposicion]] <- list(
          tabla = tab,
          a = a, b = b, c = c, d = d,
          odds_ratio = or,
          ic_95_inf = li,
          ic_95_sup = ls,
          chi_cuadrado = chi2,
          p_valor = p_valor,
          n_total = nrow(datos_exp)
        )
      }
    }
  }
  
  return(resultados)
}

# =========================
# FUNCIÓN PARA FOREST PLOT
# =========================

crear_forest_plot <- function(resultados_or) {
  if (is.null(resultados_or) || length(resultados_or) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "No hay datos para crear el forest plot", 
                     size = 6) +
             theme_void())
  }
  
  # Crear dataframe para el plot
  datos_plot <- data.frame(
    Exposicion = names(resultados_or),
    OR = sapply(resultados_or, function(x) x$odds_ratio),
    LI = sapply(resultados_or, function(x) x$ic_95_inf),
    LS = sapply(resultados_or, function(x) x$ic_95_sup),
    Significativo = sapply(resultados_or, function(x) {
      if (is.na(x$ic_95_inf) || is.na(x$ic_95_sup)) return(FALSE)
      return(x$ic_95_inf > 1 || x$ic_95_sup < 1)
    })
  )
  
  # Ordenar por OR
  datos_plot <- datos_plot[order(datos_plot$OR, decreasing = TRUE), ]
  datos_plot$Exposicion <- factor(datos_plot$Exposicion, 
                                  levels = rev(datos_plot$Exposicion))
  
  # Calcular límites del gráfico
  min_lim <- min(c(datos_plot$LI, 0.1), na.rm = TRUE) * 0.8
  max_lim <- max(c(datos_plot$LS, 10), na.rm = TRUE) * 1.2
  
  # Crear el forest plot
  p <- ggplot(datos_plot, aes(x = OR, y = Exposicion)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_errorbarh(aes(xmin = LI, xmax = LS), 
                   height = 0.2, 
                   color = ifelse(datos_plot$Significativo, "darkblue", "gray50"),
                   size = 1) +
    geom_point(aes(color = Significativo), 
               size = 3, 
               shape = 18) +
    scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "darkblue")) +
    scale_x_log10(limits = c(min_lim, max_lim),
                  breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                  labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
    labs(
      title = "Forest Plot - Odds Ratios por Exposición",
      x = "Odds Ratio (escala logarítmica)",
      y = "Exposición",
      caption = "Líneas verticales representan intervalos de confianza al 95%\nOR > 1: mayor riesgo, OR < 1: menor riesgo"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 11),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    ) +
    geom_text(aes(label = sprintf("%.2f [%.2f-%.2f]", OR, LI, LS),
                  x = max_lim * 0.9), 
              hjust = 1, vjust = 0, size = 3.5, color = "black")
  
  return(p)
}

render_tabla_tasas <- function(resultados_tasas) {
  if (is.null(resultados_tasas) || length(resultados_tasas) == 0) {
    return(div(
      class = "alert alert-warning",
      tags$h5("No se pudieron calcular tasas de ataque"),
      tags$p("Posibles causas:"),
      tags$ul(
        tags$li("Las variables no contienen valores lógicos (VERDADERO/FALSO, SI/NO, TRUE/FALSE, 1/0)"),
        tags$li("Faltan datos en las variables de evento o exposición"),
        tags$li("Las tablas 2x2 no pudieron construirse (no hay suficiente variación en los datos)"),
        tags$li("Verifique que las columnas en el CSV coincidan con las especificadas en el YAML")
      ),
      tags$p("Tip: Las variables lógicas deben usar valores como: VERDADERO/FALSO, SI/NO, TRUE/FALSE, 1/0")
    ))
  }
  
  tablas <- lapply(names(resultados_tasas), function(exposicion) {
    res <- resultados_tasas[[exposicion]]
    
    tagList(
      h5(strong(exposicion), sprintf(" (n=%d)", res$n_total)),
      tags$table(
        class = "table table-sm table-bordered",
        style = "width: auto; margin-bottom: 20px;",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("Enfermos"),
            tags$th("Total"),
            tags$th("Tasa de ataque")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Expuestos")),
            tags$td(res$casos_expuestos),
            tags$td(res$n_expuestos),
            tags$td(ifelse(!is.na(res$tasa_expuestos), 
                          sprintf("%.1f%%", res$tasa_expuestos * 100), 
                          "N/A"))
          ),
          tags$tr(
            tags$td(strong("No expuestos")),
            tags$td(res$casos_no_expuestos),
            tags$td(res$n_no_expuestos),
            tags$td(ifelse(!is.na(res$tasa_no_expuestos), 
                          sprintf("%.1f%%", res$tasa_no_expuestos * 100), 
                          "N/A"))
          ),
          tags$tr(
            tags$td(strong("Riesgo Relativo (RR)")),
            tags$td(colspan = "3", 
                   ifelse(!is.na(res$riesgo_relativo), 
                          sprintf("%.2f", res$riesgo_relativo), 
                          "No calculable"))
          ),
          tags$tr(
            tags$td(strong("Diferencia de Riesgo")),
            tags$td(colspan = "3", 
                   ifelse(!is.na(res$diferencia_riesgo), 
                          sprintf("%.3f", res$diferencia_riesgo), 
                          "No calculable"))
          )
        )
      )
    )
  })
  
  tagList(tablas)
}

render_tabla_or <- function(resultados_or) {
  if (is.null(resultados_or) || length(resultados_or) == 0) {
    return(div(
      class = "alert alert-warning",
      "No se pudieron calcular odds ratios"
    ))
  }
  
  filas <- lapply(names(resultados_or), function(exposicion) {
    res <- resultados_or[[exposicion]]
    
    tags$tr(
      tags$td(strong(exposicion)),
      tags$td(sprintf("%.2f", res$odds_ratio)),
      tags$td(ifelse(!is.na(res$ic_95_inf), sprintf("[%.2f-%.2f]", res$ic_95_inf, res$ic_95_sup), "N/A")),
      tags$td(ifelse(!is.na(res$chi_cuadrado), sprintf("%.2f", res$chi_cuadrado), "N/A")),
      tags$td(ifelse(!is.na(res$p_valor), 
                    ifelse(res$p_valor < 0.001, "<0.001", sprintf("%.3f", res$p_valor)), 
                    "N/A"))
    )
  })
  
  tagList(
    tags$table(
      class = "table table-sm table-bordered table-striped",
      tags$thead(
        tags$tr(
          tags$th("Exposición"),
          tags$th("Odds Ratio"),
          tags$th("IC 95%"),
          tags$th("Chi²"),
          tags$th("Valor p")
        )
      ),
      tags$tbody(filas)
    ),
    
    div(
      class = "alert alert-info mt-3",
      tags$small(
        strong("Interpretación:"),
        "OR > 1: asociación positiva (mayor riesgo), OR < 1: asociación negativa (menor riesgo), ",
        "OR = 1: no asociación. IC 95% que no incluye 1 indica significancia estadística."
      )
    )
  )
}

# Convierte un gráfico ggplot en una etiqueta <img> con base64
# Función para convertir gráficos a Base64 de forma segura
plot_to_b64 <- function(p) {
  if (is.null(p)) return(NULL)
  library(base64enc)
  tmp <- tempfile(fileext = ".png")
  # Ajustamos dimensiones para el informe
  ggsave(tmp, plot = p, width = 6, height = 4, dpi = 200)
  encoded <- base64enc::base64encode(readBin(tmp, "raw", file.info(tmp)$size))
  unlink(tmp)
  return(paste0("data:image/png;base64,", encoded))
}

# =========================
# UI
# =========================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),
  extendShinyjs(text = js_download_script, functions = c("downloadFile")),
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("datos", "1. Seleccionar Datos (csv o txt)", accept = c(".csv", ".txt")),
      fileInput("yaml", "2. Seleccionar Configuración (yaml)", accept = c(".yaml", ".yml")),
      #checkboxInput("debug", "Modo debug", value = FALSE),
      hr(),
      actionButton("ejecutar", "Generar informe", class = "btn-primary w-100"),
      actionButton("limpiar", "Limpiar todo", class = "btn-warning w-100 mt-2"),
      hr(),
      # Usamos un actionButton normal en lugar de downloadButton para mayor control
      hidden(
        actionButton("btn_descargar_js", "Descargar Informe HTML", class = "btn-success w-100")
      ),
      h5("Estado de carga:"),
      verbatimTextOutput("debug_info"),
      verbatimTextOutput("debug_tipos")
    ),
    
    mainPanel(
      width = 9,
      uiOutput("mensajes"),
      uiOutput("resultado")
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  estado <- reactiveValues(
    datos = NULL,
    solicitud = NULL,
    ejecutado = FALSE,
    resultados_or = NULL,  # Para almacenar resultados de OR
    desc = NULL,
    resultados_tasas = NULL,
    grafico_or = NULL,
    grafico_curva = NULL,
    listo = FALSE
  )
  
  observeEvent(input$datos, {
    tryCatch({
      estado$datos <- readr::read_delim(input$datos$datapath, delim = ";", 
                                locale = locale(decimal_mark = ","),
                                show_col_types = FALSE)
      # RESET CLAVE: Si cargan datos nuevos, el informe anterior desaparece
      estado$ejecutado <- FALSE 
      showNotification("✓ Datos CSV cargados", type = "message")
    }, error = function(e) {
      tryCatch({
        estado$datos <- readr::read_csv(input$datos$datapath, show_col_types = FALSE)
        showNotification("✓ Datos CSV cargados", type = "message")
      }, error = function(e2) {
        showNotification(paste("Error al leer CSV:", e2$message), type = "error")
        estado$datos <- NULL
      })
    })
  })
  
  observeEvent(input$yaml, {
    tryCatch({
      estado$solicitud <- yaml::read_yaml(input$yaml$datapath)
      estado$ejecutado <- FALSE # RESET CLAVE
      showNotification("✓ YAML cargado", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al leer YAML:", e$message), type = "error")
      estado$solicitud <- NULL
    })
  })
  
  # BOTÓN LIMPIAR: Reset total
  observeEvent(input$limpiar, {
    estado$datos <- NULL
    estado$solicitud <- NULL
    estado$ejecutado <- FALSE
    estado$resultados_or <- NULL
    estado$desc <- NULL
    estado$resultados_tasas <- NULL
    estado$grafico_or <- NULL
    estado$grafico_curva <- NULL
    reset("datos")
    reset("yaml")
    showNotification("Vista limpiada", type = "warning")
  })

  
  errores_yaml <- reactive({
    req(estado$solicitud)
    
    errores <- c()
    
    if (is.null(estado$solicitud$definicion_problema)) {
      errores <- c(errores, "Falta sección 'definicion_problema'")
    } else if (is.null(estado$solicitud$definicion_problema$evento)) {
      errores <- c(errores, "Falta 'definicion_problema$evento'")
    }
    
    if (is.null(estado$solicitud$variables)) {
      errores <- c(errores, "Falta sección 'variables'")
    }
    
    errores
  })
  
  errores_mapeo <- reactive({
    req(estado$datos, estado$solicitud)
    
    if (is.null(estado$solicitud$variables)) {
      return(c())
    }
    
    vars_yaml <- estado$solicitud$variables
    vars_datos <- names(estado$datos)
    errores <- c()
    
    if (!is.null(vars_yaml$sociodemograficas)) {
      for (var in vars_yaml$sociodemograficas) {
        
        var_nombre <- if (is.list(var) && "nombre" %in% names(var)) {
          var$nombre
        } else {
          var
        }
        
        if (!var_nombre %in% vars_datos) {
          errores <- c(
            errores,
            paste("Variable sociodemográfica no encontrada:", var_nombre)
          )
        }
      }
    }
    
    if (!is.null(vars_yaml$variable_tiempo_inicio)) {
      var_inicio <- vars_yaml$variable_tiempo_inicio
      if (!var_inicio %in% vars_datos) {
        errores <- c(errores, paste("Variable de tiempo no encontrada:", var_inicio))
      }
    }
    
    if (!is.null(vars_yaml$evento)) {
      var_evento <- vars_yaml$evento
      if (!var_evento %in% vars_datos) {
        errores <- c(errores, paste("Variable de evento no encontrada:", var_evento))
      }
    }
    
    if (!is.null(vars_yaml$exposicion)) {
      for (var in vars_yaml$exposicion) {
        if (!var %in% vars_datos) {
          errores <- c(errores, paste("Variable de exposición no encontrada:", var))
        }
      }
    }
    
    if (!is.null(vars_yaml$participo)) {
      var_participo <- vars_yaml$participo
      if (!var_participo %in% vars_datos) {
        errores <- c(errores, paste("Variable de participación no encontrada:", var_participo))
      }
    }
    
    errores
  })
  
  output$debug_tipos <- renderPrint({
    req(estado$datos, estado$solicitud)
    
    datos <- estado$datos
    vars_sociodem <- estado$solicitud$variables$sociodemograficas
    
    cat("=== DEBUG: TIPOS DE VARIABLES ===\n")
    
    for (var in vars_sociodem) {
      if (is.list(var) && "nombre" %in% names(var)) {
        var_nombre <- var$nombre
        var_tipo <- var$tipo
      } else {
        var_nombre <- var
        var_tipo <- "No especificado"
      }
      
      if (var_nombre %in% names(datos)) {
        col <- datos[[var_nombre]]
        cat(sprintf("%s:\n", var_nombre))
        cat(sprintf("  Tipo YAML: %s\n", var_tipo))
        cat(sprintf("  Tipo R: %s\n", class(col)))
        cat(sprintf("  Valores únicos: %d\n", length(unique(na.omit(col)))))
        cat(sprintf("  Ejemplos: %s\n", paste(head(unique(col), 5), collapse=", ")))
        cat("  ---\n")
      }
    }
  })
  
  informe <- eventReactive(input$ejecutar, {
    
    if (is.null(estado$datos)) {
      return(list(error = "Debe cargar un archivo CSV"))
    }
    
    if (is.null(estado$solicitud)) {
      return(list(error = "Debe cargar un archivo YAML"))
    }
    
    estado$ejecutado <- TRUE # Se marca como ejecutado tras procesar
    
    err_yaml <- errores_yaml()
    if (length(err_yaml) > 0) {
      return(list(error = paste("Errores en YAML:\n", colapsar_errores(err_yaml))))
    }
    
    err_mapeo <- errores_mapeo()
    if (length(err_mapeo) > 0) {
      return(list(error = paste("Errores de mapeo:\n", colapsar_errores(err_mapeo))))
    }
    
    productos_esperados <- estado$solicitud$productos
    productos <- character(0)
    
    if (!is.null(productos_esperados) && length(productos_esperados) > 0) {
      productos <- unlist(productos_esperados, use.names = FALSE)
    }
    
    
    # Crear variables intermedias si están configuradas
    datos_procesados <- estado$datos
    
    # Verificar si hay configuración de variables creadas
    if (!is.null(estado$solicitud$variables_creadas)) {
      datos_procesados <- crear_variables_intermedias(
        datos = datos_procesados,
        config_variables = estado$solicitud$variables_creadas,
        vars_yaml = estado$solicitud$variables  # Pasar las variables del YAML
      )
    }
    
    show("btn_descargar_js")
    
    list(
      datos = datos_procesados,
      vars = estado$solicitud$variables,
      evento = estado$solicitud$definicion_problema$evento,
      productos = productos,
      config_variables = estado$solicitud$variables_creadas,
      error = NULL
    )
  })
  
  output$mensajes <- renderUI({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) {
      div(
        class = "alert alert-danger",
        role = "alert",
        h4(class = "alert-heading", "❌ Error"),
        HTML(gsub("\n", "<br>", info$error))
      )
    } else {
      div(
        class = "alert alert-success",
        role = "alert",
        "✓ Informe generado correctamente"
      )
    }
  })
  
  output$resultado <- renderUI({
    if (!estado$ejecutado) return(NULL)
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) {
      return(NULL)
    }
    
    ev <- info$evento
    productos <- info$productos
    
    elementos <- tagList(
      hr(),
      h2(ev$nombre, style = "color: #2c3e50;"),
      h4("Definición de caso"),
      p(ev$definicion_caso, style = "margin-left: 15px;"),
      h4("Población, lugar y período"),
      tags$ul(
        tags$li(strong("Población: "), ev$poblacion_en_riesgo),
        tags$li(strong("Lugar: "), ev$lugar),
        tags$li(strong("Período: "), ev$periodo_tiempo)
      ),
      hr()
    )
    
    if (length(productos) > 0 && "descripcion_univariada" %in% productos) {
      elementos <- tagList(
        elementos,
        h3("Descripción univariada", style = "color: #34495e;"),
        uiOutput("tabla_univariada"),
        hr()
      )
    }
    
    if (length(productos) > 0 && "tasas_ataque_por_exposicion" %in% productos) {
      elementos <- tagList(
        elementos,
        h3("Tasas de ataque por exposición", style = "color: #34495e;"),
        p("Análisis de tasas de ataque entre expuestos y no expuestos a cada alimento:"),
        uiOutput("tabla_tasas"),
        hr()
      )
    }
    
    if (length(productos) > 0 && "odds_ratio_por_exposicion" %in% productos) {
      elementos <- tagList(
        elementos,
        h3("Odds ratio por exposición", style = "color: #34495e;"),
        p("Asociación entre exposición a alimentos y enfermedad (odds ratio):"),
        uiOutput("tabla_or"),
        h3("Forest Plot de Odds Ratios", style = "color: #34495e;"),
        p("Visualización gráfica de los odds ratios con intervalos de confianza:"),
        plotOutput("forest_plot", height = "600px"),
        hr()
      )
    }
    
    if (length(productos) > 0 && "curva_epidemica" %in% productos) {
      elementos <- tagList(
        elementos,
        h3("Curva epidémica", style = "color: #34495e;"),
        plotOutput("plot_curva", height = "450px"),
        hr()
      )
    }

    if (length(productos) > 0 && "frecuencia_combinada_sintomas" %in% productos) {
      elementos <- tagList(
        elementos,
        h3("Frecuencia combinada de síntomas", style = "color: #34495e;"),
        p("Combinaciones de síntomas presentes en los casos:"),
        uiOutput("tabla_freq_combinada"),
        hr()
      )
    }
    
    return(elementos)
  })
  
  output$tabla_univariada <- renderUI({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) return(NULL)
    
    vars_sociodem <- info$vars$sociodemograficas
    
    if (is.null(vars_sociodem)) {
      return(div(
        class = "alert alert-warning",
        "No se definieron variables sociodemográficas en el YAML"
      ))
    }
    
    desc <- generar_descripcion_univariada(
      datos = info$datos, 
      variables_sociodem = vars_sociodem,
      config_variables = info$config_variables
    )
    
    estado$desc <- desc
    
    render_univariada(desc)
    
  })
  
  output$tabla_tasas <- renderUI({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) return(NULL)
    
    vars <- info$vars
    
    if (is.null(vars$evento) || is.null(vars$exposicion)) {
      return(div(
        class = "alert alert-warning",
        "Faltan definiciones de evento o exposiciones en el YAML"
      ))
    }
    
    variable_evento <- vars$evento
    variables_exposicion <- vars$exposicion
    variable_participo <- vars$participo
    
    if (!variable_evento %in% names(info$datos)) {
      return(div(
        class = "alert alert-warning",
        paste("Variable de evento no encontrada:", variable_evento)
      ))
    }
    
    if (debug) {
      cat("\n=== DEBUG TASAS DE ATAQUE ===\n")
      cat("Variable evento:", variable_evento, "\n")
      cat("Variable participo:", variable_participo, "\n")
      cat("Exposiciones:", paste(variables_exposicion, collapse=", "), "\n")
      
      datos <- info$datos
      cat("\nValores únicos en", variable_evento, ":\n")
      print(table(datos[[variable_evento]], useNA = "always"))
      
      if (!is.null(variable_participo)) {
        cat("\nValores únicos en", variable_participo, ":\n")
        print(table(datos[[variable_participo]], useNA = "always"))
      }
    }
    
    resultados_tasas <- calcular_tasas_ataque(
      datos = info$datos,
      variable_evento = variable_evento,
      variables_exposicion = variables_exposicion,
      variable_participo = variable_participo,
      debug = FALSE #input$debug
    )
    
    estado$resultados_tasas <- resultados_tasas
    
    render_tabla_tasas(resultados_tasas)
  })
  
  output$tabla_or <- renderUI({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) return(NULL)
    
    vars <- info$vars
    
    if (is.null(vars$evento) || is.null(vars$exposicion)) {
      return(div(
        class = "alert alert-warning",
        "Faltan definiciones de evento o exposiciones en el YAML"
      ))
    }
    
    variable_evento <- vars$evento
    variables_exposicion <- vars$exposicion
    variable_participo <- vars$participo
    
    if (!variable_evento %in% names(info$datos)) {
      return(div(
        class = "alert alert-warning",
        paste("Variable de evento no encontrada:", variable_evento)
      ))
    }
    
    if (debug) {
      cat("\n=== DEBUG ODDS RATIO ===\n")
      cat("Variable evento:", variable_evento, "\n")
      cat("Variable participo:", variable_participo, "\n")
      cat("Exposiciones:", paste(variables_exposicion, collapse=", "), "\n")
      
      datos <- info$datos
      cat("\nValores únicos en", variable_evento, ":\n")
        print(table(datos[[variable_evento]], useNA = "always"))
      
      if (!is.null(variable_participo)) {
        cat("\nValores únicos en", variable_participo, ":\n")
        print(table(datos[[variable_participo]], useNA = "always"))
      }
      
      for (exp in variables_exposicion) {
        if (exp %in% names(datos)) {
          cat("\n--- Exposición:", exp, "---\n")
          cat("Valores originales:\n")
          print(table(datos[[exp]], useNA = "always"))
          
          convertidos <- convertir_a_logico(datos[[exp]])
          cat("Valores convertidos:\n")
          print(table(convertidos, useNA = "always"))
        }
      }
    }
    
    # Calcular odds ratio y almacenar en estado reactivo
    resultados_or <- calcular_odds_ratio(
      datos = info$datos,
      variable_evento = variable_evento,
      variables_exposicion = variables_exposicion,
      variable_participo = variable_participo,
      debug = FALSE #input$debug
    )
    
    # Almacenar resultados para el forest plot
    estado$resultados_or <- resultados_or
    
    if (debug && length(resultados_or) > 0) {
      cat("\n=== RESULTADOS ODDS RATIO ===\n")
      for (exp in names(resultados_or)) {
        cat("\nExposición:", exp, "\n")
        print(resultados_or[[exp]])
      }
    }
    
    render_tabla_or(resultados_or)
  })
  
  # =========================
  # FOREST PLOT
  # =========================
  
  output$forest_plot <- renderPlot({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) return(NULL)
    
    # Verificar que se solicitó el producto
    if (!"odds_ratio_por_exposicion" %in% info$productos) {
      return(NULL)
    }
    
    # Usar los resultados almacenados
    if (is.null(estado$resultados_or) || length(estado$resultados_or) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = "No hay datos de odds ratios para crear el forest plot", 
                       size = 6) +
               theme_void())
    }
    
    # Crear el forest plot
    library(ggplot2)
    estado$grafico_or <- crear_forest_plot(estado$resultados_or)
    
    print(estado$grafico_or)
    
  })
  
  output$plot_curva <- renderPlot({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) {
      return(NULL)
    }
    
    if (!"curva_epidemica" %in% info$productos) {
      return(NULL)
    }
    library(ggplot2)
    datos <- info$datos
    var_inicio <- info$vars$variable_tiempo_inicio
    
    if (!var_inicio %in% names(datos)) {
      plot.new()
      text(0.5, 0.5, 
           paste("Error: columna", var_inicio, "no encontrada"), 
           cex = 1.2, col = "red")
      return()
    }
    
    # Detectar el tipo de dato de la variable de tiempo
    dato_tiempo <- datos[[var_inicio]]

  output$tabla_freq_combinada <- renderUI({
    req(input$ejecutar > 0)
    
    info <- informe()
    
    if (!is.null(info$error)) return(NULL)
    
    vars <- info$vars
    
    if (is.null(vars$sintomas)) {
      return(div(
        class = "alert alert-warning",
        "No hay variables de síntomas definidas en el YAML"
      ))
    }
    
    # Calcular frecuencia combinada de síntomas
    tabla_freq <- calcular_frecuencia_combinada_sintomas(
      datos = info$datos,
      vars_sintomas = vars$sintomas
    )
    
    # Almacenar resultados para la descarga
    estado$freq_combinada <- tabla_freq
    
    # Renderizar tabla
    if (!is.null(tabla_freq$error)) {
      return(div(
        class = "alert alert-warning",
        tabla_freq$error
      ))
    }
    
    # Crear tabla HTML
    filas <- apply(tabla_freq, 1, function(row) {
      tags$tr(
        tags$td(row["Combinacion_sintomas"]),
        tags$td(row["Frecuencia"], style = "text-align: center;"),
        tags$td(paste0(row["Porcentaje"], "%"), style = "text-align: center;")
      )
    })
    
    tags$div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm table-bordered table-striped",
        tags$thead(
          tags$tr(
            tags$th("Combinación de síntomas"),
            tags$th("Frecuencia", style = "text-align: center;"),
            tags$th("Porcentaje", style = "text-align: center;")
          )
        ),
        tags$tbody(filas)
      ),
      tags$div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        tags$small(
          tags$strong("Nota:"), " Esta tabla muestra las combinaciones de síntomas presentes en los casos, ordenadas de mayor a menor frecuencia."
        )
      )
    )
  })
    
    # Verificar si hay configuración explícita de unidad de tiempo
    unidad_tiempo_config <- NULL
    if (!is.null(info$vars$unidad_tiempo)) {
      unidad_tiempo_config <- info$vars$unidad_tiempo
    }
    
    # Determinar automáticamente el tipo si no está configurado
    es_fecha <- FALSE
    es_hora <- FALSE
    
    if (!is.null(unidad_tiempo_config)) {
      # Si está configurado, usar esa información
      if (tolower(unidad_tiempo_config) %in% c("fecha", "dia", "días", "dia", "date")) {
        es_fecha <- TRUE
      } else if (tolower(unidad_tiempo_config) %in% c("hora", "horas", "hour", "hours")) {
        es_hora <- TRUE
      }
    } else {
      # Detectar automáticamente
      # Intentar convertir a fecha
      fecha_test <- tryCatch({
        as.Date(dato_tiempo)
      }, error = function(e) NULL)
      
      if (!is.null(fecha_test) && !all(is.na(fecha_test))) {
        es_fecha <- TRUE
      } else if (is.numeric(dato_tiempo)) {
        es_hora <- TRUE
      }
    }
    
    # Preparar datos según el tipo
    if (es_fecha) {
      # Manejo de fechas
      datos_plot <- datos |>
        mutate(tiempo = as.Date(.data[[var_inicio]])) |>
        filter(!is.na(tiempo))
      
      if (nrow(datos_plot) == 0) {
        plot.new()
        text(0.5, 0.5, 
             "No hay datos válidos para graficar", 
             cex = 1.2, col = "gray50")
        return()
      }
      
      # Contar casos por fecha
      datos_count <- datos_plot |>
        group_by(tiempo) |>
        summarise(n_casos = n(), .groups = "drop")
      
      # Completar todas las fechas del rango (incluir fechas sin casos)
      rango_fechas <- seq(min(datos_count$tiempo), max(datos_count$tiempo), by = "1 day")
      datos_count_completo <- data.frame(tiempo = rango_fechas) |>
        left_join(datos_count, by = "tiempo") |>
        mutate(n_casos = ifelse(is.na(n_casos), 0, n_casos))
      
      # Crear datos para líneas horizontales (sin usar crossing de tidyr)
      datos_lineas <- datos_count_completo |>
        filter(n_casos > 1)
      
      if (nrow(datos_lineas) > 0) {
        # Crear producto cartesiano manualmente
        max_casos <- max(datos_count_completo$n_casos)
        datos_lineas <- do.call(rbind, lapply(1:nrow(datos_lineas), function(i) {
          n <- datos_lineas$n_casos[i]
          data.frame(
            tiempo = datos_lineas$tiempo[i],
            n_casos = n,
            linea = 1:(n-1)  # Líneas desde 1 hasta n-1
          )
        }))
      } else {
        datos_lineas <- data.frame(tiempo = as.Date(character(0)), n_casos = numeric(0), linea = numeric(0))
      }
      
      # Crear gráfico de barras con fechas
      # width = 1 para barras pegadas, sin color de borde
      p <- ggplot(datos_count_completo, aes(x = tiempo, y = n_casos)) +
        geom_col(fill = "#006666", color = "white", alpha = 0.8, width = 1)
      
      # Agregar líneas horizontales si hay datos
      if (nrow(datos_lineas) > 0) {
        p <- p + geom_hline(
          data = datos_lineas,
          aes(yintercept = linea),
          color = "white",
          linewidth = 0.3,
          alpha = 0.6
        )
      }
      
      p <- p +
        scale_x_date(
          date_breaks = "1 day",
          date_labels = "%d/%m",
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        scale_y_continuous(
          breaks = function(x) seq(0, ceiling(max(x)), by = 1),
          expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
          title = "Curva epidémica",
          x = "Fecha de inicio de síntomas",
          y = "Número de casos"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (es_hora) {
      # Manejo de horas (numérico)
      datos_plot <- datos |>
        mutate(tiempo = as.numeric(.data[[var_inicio]])) |>
        filter(!is.na(tiempo))
      
      if (nrow(datos_plot) == 0) {
        plot.new()
        text(0.5, 0.5, 
             "No hay datos válidos para graficar", 
             cex = 1.2, col = "gray50")
        return()
      }
      
      # Contar casos por hora
      datos_count <- datos_plot |>
        group_by(tiempo) |>
        summarise(n_casos = n(), .groups = "drop")
      
      # Completar todas las horas del rango (incluir horas sin casos)
      rango_horas <- seq(floor(min(datos_count$tiempo)), ceiling(max(datos_count$tiempo)), by = 1)
      datos_count_completo <- data.frame(tiempo = rango_horas) |>
        left_join(datos_count, by = "tiempo") |>
        mutate(n_casos = ifelse(is.na(n_casos), 0, n_casos))
      
      # Crear datos para líneas horizontales (sin usar crossing de tidyr)
      datos_lineas <- datos_count_completo |>
        filter(n_casos > 1)
      
      if (nrow(datos_lineas) > 0) {
        # Crear producto cartesiano manualmente
        max_casos <- max(datos_count_completo$n_casos)
        datos_lineas <- do.call(rbind, lapply(1:nrow(datos_lineas), function(i) {
          n <- datos_lineas$n_casos[i]
          data.frame(
            tiempo = datos_lineas$tiempo[i],
            n_casos = n,
            linea = 1:(n-1)  # Líneas desde 1 hasta n-1
          )
        }))
      } else {
        datos_lineas <- data.frame(tiempo = numeric(0), n_casos = numeric(0), linea = numeric(0))
      }
      
      # Crear gráfico de barras con horas
      # width = 1 para barras pegadas, sin color de borde
      p <- ggplot(datos_count_completo, aes(x = tiempo, y = n_casos)) +
        geom_col(fill = "#006666", color = "white", alpha = 0.8, width = 1)
      
      # Agregar líneas horizontales si hay datos
      if (nrow(datos_lineas) > 0) {
        p <- p + geom_hline(
          data = datos_lineas,
          aes(yintercept = linea),
          color = "white",
          linewidth = 0.3,
          alpha = 0.6
        )
      }
      
      p <- p +
        scale_x_continuous(
          breaks = seq(floor(min(datos_count$tiempo)), 
                      ceiling(max(datos_count$tiempo)), 
                      by = 1),
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        scale_y_continuous(
          breaks = function(x) seq(0, ceiling(max(x)), by = 1),
          expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
          title = "Curva epidémica",
          x = "Horas desde inicio de síntomas",
          y = "Número de casos"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank()
        )
      
    } else {
      # Si no se puede determinar el tipo
      plot.new()
      text(0.5, 0.5, 
           "No se pudo determinar el tipo de variable de tiempo\n(configure 'unidad_tiempo' en el YAML)", 
           cex = 1.2, col = "orange")
      return()
    }
    
    estado$grafico_curva <- p
    
    # Retornar el gráfico
    print(p)
  })
  
  output$debug_info <- renderText({
    paste0(
      "CSV: ", if(is.null(estado$datos)) "❌" else "✓",
      "\nYAML: ", if(is.null(estado$solicitud)) "❌" else "✓",
      "\nFilas: ", if(!is.null(estado$datos)) nrow(estado$datos) else "0",
      "\nColumnas: ", if(!is.null(estado$datos)) paste(names(estado$datos), collapse=", ") else "N/A",
      "\nEjecutado: ", estado$ejecutado
    )
  })
  
  # --- LÓGICA DE DESCARGA VÍA JAVASCRIPT ---
observeEvent(input$btn_descargar_js, {
  showNotification("Preparando documento...", type = "message")
  
  # Función interna para convertir gráficos a Base64
  get_b64 <- function(p) {
    if (is.null(p)) return(NULL)
    tryCatch({
      tmp <- tempfile(fileext = ".png")
      ggsave(tmp, plot = p, width = 6, height = 3, dpi = 150)
      res <- base64enc::base64encode(readBin(tmp, "raw", file.info(tmp)$size))
      unlink(tmp)
      paste0("data:image/png;base64,", res)
    }, error = function(e) NULL)
  }

  # 1. Procesar gráficos
  forest_img <- get_b64(estado$grafico_or)
  curva_img  <- get_b64(estado$grafico_curva)
  
    # 2. Procesar tablas (convertir a HTML string directamente)
  tabla_desc_html <- if(!is.null(estado$desc)) {
    convertir_univariada_a_html(estado$desc)
  } else { 
    "<p>No hay datos de descriptivos disponibles.</p>"
  }
  
  tabla_tasas_html <- if(!is.null(estado$resultados_tasas)) {
    convertir_tasas_a_html(estado$resultados_tasas)
  } else { 
    "<p>No hay datos de tasas de ataque disponibles.</p>"
  }

  tabla_or_html <- if(!is.null(estado$resultados_or)) {
    convertir_or_a_html(estado$resultados_or)
  } else { 
    "<p>No hay datos de Odds Ratio disponibles.</p>"
  }
  
  tabla_freq_comb_html <- if(!is.null(estado$freq_combinada)) {
    convertir_frec_combinada_a_html(estado$freq_combinada)
  } else { 
    "<p>No hay datos de frecuencia combinada de síntomas disponibles.</p>"
  }
  
  # 3. Construir el cuerpo del informe dinámicamente
  contenido <- list(
    h1("Informe de Investigación de Brote", style="text-align:center; color:#2c3e50;"),
    p(tags$b("Fecha: "), format(Sys.time(), "%d/%m/%Y %H:%M")),
    hr(),
    h2("Descripción univariada"),
    HTML(tabla_desc_html),
    br()
  )
  
  # Añadir tasas
  contenido <- c(contenido, list(
    h2("Tasas de ataque por exposición"),
    HTML(tabla_tasas_html)
  ))
  
  contenido <- c(contenido, list(
    h2("Tabla de Odds Ratio"),
    HTML(tabla_or_html)
  ))
  
  contenido <- c(contenido, list(
    h2("Frecuencia combinada de síntomas"),
    HTML(tabla_freq_comb_html)
  ))
  
    # Añadir Forest Plot solo si existe
  if(!is.null(forest_img)) {
    contenido <- c(contenido, list(
      h2("Forest Plot"),
      img(src = forest_img, style="width:100%; border:1px solid #eee;")
    ))
  }

    # Añadir Curva solo si existe
  if(!is.null(curva_img)) {
    contenido <- c(contenido, list(
      h2("Curva Epidémica"),
      img(src = curva_img, style="width:100%; border:1px solid #eee;")
    ))
  }
  
  # 4. Ensamble final y descarga
  doc_final <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$style("body{font-family:sans-serif; margin:40px; line-height:1.6;} 
                  table{width:100%; border-collapse:collapse; margin:15px 0;} 
                  th,td{border:1px solid #ddd; padding:10px; text-align:left;}
                  th{background:#f8f9fa;} h2{color:#2980b9; border-left:4px solid #2980b9; padding-left:10px; margin-top:30px;}")
    ),
    do.call(tags$body, contenido)
  )

  js$downloadFile(
    filename = paste0("informe_", format(Sys.time(), "%Y%m%d_%H%M"), ".html"),
    content = as.character(doc_final)
  )
})
}

shinyApp(ui, server)
 
