# Función para calcular el promedio de un vector de números
calcular_promedio <- function(numeros) {
  total <- sum(numeros)  # Suma todos los números
  promedio <- total / length(numeros)  # Divide la suma por la cantidad de números
  return(promedio)
}

# Función para solicitar un número con validación
solicitar_numero <- function(prompt_text) {
  numero <- NA  # Inicializa con NA para el bucle
  while (is.na(numero)) {
    entrada <- readline(prompt = prompt_text)
    numero <- suppressWarnings(as.numeric(entrada))
    if (is.na(numero)) {
      cat("Entrada inválida. Por favor, ingresa un número válido.\n")
    }
  }
  return(numero)
}

# Función principal para solicitar varios números y mostrar su promedio
promedio <- function() {
  n <- as.integer(solicitar_numero("¿Cuántos números vas a ingresar? "))  # Solicitar cantidad de números

  if (n <= 0) {
    cat("Por favor, ingresa un número válido mayor que 0.\n")
    return()
  }

  numeros <- numeric(n)  # Crea un vector numérico para almacenar los números

  for (i in 1:n) {
    numeros[i] <- solicitar_numero(paste("Ingresa el número", i, ": "))  # Solicitar cada número
  }

  # Calcular el promedio
  resultado_promedio <- calcular_promedio(numeros)

  # Mostrar el resultado
  print(paste("El promedio es:", resultado_promedio))
}
