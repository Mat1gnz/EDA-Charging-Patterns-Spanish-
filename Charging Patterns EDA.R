install.packages("ggcorrplot")
install.packages("DescTools")
library(DescTools)
library(ggcorrplot)
library(tidyverse)
library(gridExtra)

electric_cars = read.csv(file.choose())

# Eliminamos las columnas que no vamos a utilizar

electric_cars <- electric_cars %>% select(-Charging.Start.Time, -Charging.End.Time)

summary(electric_cars)

################################################################################
# Qué modelo de auto prefiere determinado tipo de conductor.
# Gráfico ''Conductor por Vehículo'
################################################################################

absoluto = ggplot(electric_cars) +
  geom_bar(mapping = aes(x = Vehicle.Model, fill = User.Type), position = "dodge") +
  theme_minimal(base_size = 14) +
  labs(
    y = "Frecuencia absoluta", 
    x = "Modelo", 
    fill = "Tipo de conductor") +
  theme(legend.position = "none")

porcentaje = ggplot(electric_cars) +
  geom_bar(mapping = aes(x = Vehicle.Model, fill = User.Type), position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  labs(
    y = "Frecuencia relativa", 
    x = "Modelo", 
    fill = "Tipo de conductor") +
  theme(legend.position = "none")

grid.arrange(absoluto, porcentaje, ncol = 2) 

################################################################################
# Gráfico Relación de cargas (Ciudad - Momento) barras apiladas
################################################################################

locationXcharge = ggplot(electric_cars) +
  geom_bar(mapping = aes(x = Charging.Station.Location, fill = Time.of.Day), position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  labs(
    y = "Cantidad de Cargas", 
    x = "Ubicación", 
    fill = "Tipo de conductor") +
  theme(legend.position = "none")


calcular_conteo <- function(data, categoria1, categoria2) {
  data %>%
    group_by(!!sym(categoria1), !!sym(categoria2)) %>%
    summarise(conteo = n(), .groups = "drop")
}

conteo_por_barras <- calcular_conteo(electric_cars, "Vehicle.Model", "User.Type")

print(conteo_por_barras)

################################################################################
# Gráfico Relación de cargas (Ciudad - Momento) barras
################################################################################

ciudad_momento <- ggplot(electric_cars) +
  geom_bar(
    mapping = aes(x = Charging.Station.Location, fill = Time.of.Day), color = "white",
    stat = "count",
    position = "dodge") +
  labs(
    x = "Ubicación",
    y = "Cantidad de Cargas") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(hjust = 1 ))


grid.arrange(ciudad_momento, locationXcharge,ncol=2)

################################################################################
# Temperatura
################################################################################

bin_width <- 3 
n <- nrow(electric_cars) 

ggplot(electric_cars) +
  geom_histogram(
    mapping = aes(x = Temperature..Â.C., y = ..count..), 
    binwidth = bin_width,
    fill = "steelblue",
    color = "black",
    alpha = 0.8) +
  geom_density(
    aes(x = Temperature..Â.C., y = ..density.. * n * bin_width),
    color = "red",
    size = 1) +
  labs(
    x = "Temperatura",
    y = "Frecuencia absoluta") +
  theme_minimal(base_size = 14)

ggplot(electric_cars,aes(x= Charging.Station.Location, y= Temperature..Â.C., fill = Charging.Station.Location)) +
  geom_boxplot(alpha=0.7, na.rm = TRUE) +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "point")+
  labs(
    x = "Ciudad",
    y = "Temperatura") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

################################################################################
#                    Años del vehículo 
################################################################################

bin_width <- 1
n <- nrow(electric_cars) 

ggplot(electric_cars) +
  geom_histogram(
    mapping = aes(x = Vehicle.Age..years., y = ..count..), 
    binwidth = bin_width,
    fill = "steelblue",
    color = "black",
    alpha = 0.8) +
  geom_density(
    aes(x = Vehicle.Age..years., y = ..density.. * n * bin_width),
    color = "red",
    size = 1) +
  labs(
    x = "Duración de Carga (Hs)",
    y = "Frecuencia absoluta") +
  theme_minimal(base_size = 14)

ggplot(electric_cars,aes(x= Vehicle.Model, y= Vehicle.Age..years., fill = Vehicle.Model)) +
  geom_boxplot(alpha=0.7, na.rm = TRUE) +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "point")+
  labs(
    x = "Modelo",
    y = "Años") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


################################################################################
# Comparación de tipo de cargador con energía consumida y velocidad de carga.
################################################################################


charging_box = ggplot(electric_cars,aes(x=Charger.Type, y=Charging.Rate..kW., fill = Charger.Type)) +
  geom_boxplot(alpha=0.7, na.rm = TRUE) +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "point")+
  labs(
    x = "Tipo de Cargador",
    y = "Entrega de energía (kW)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

energy_box = ggplot(electric_cars,aes(x=Charger.Type, y=Energy.Consumed..kWh., fill = Charger.Type)) +
  geom_boxplot(alpha=0.7, na.rm = TRUE) +  
  labs(
    x = "Tipo de Cargador",
    y = "Energía consumida (kWh)") +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "point")+
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

grid.arrange(charging_box, energy_box, ncol = 2)

################################################################################
#                           Eficiencia de carga
################################################################################

electric_cars <- electric_cars %>%
  mutate(charging_efficiency = Energy.Consumed..kWh. / Charging.Duration..hours.)

percentiles_eficiencia <- quantile(electric_cars$charging_efficiency, probs = c(0.05, 0.95), na.rm = TRUE)
upper_b <- percentiles_eficiencia[2] # Límite superior (95%)

ggplot(electric_cars %>% filter(charging_efficiency <= upper_b)
       , aes(x = Charger.Type, y = charging_efficiency, fill = Charger.Type)) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", shape = 23, size = 3, color = "black") +
  labs(
    x = "Tipo de Cargador",
    y = "Eficiencia de Carga (kWh/h)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

percentiles <- quantile(electric_cars$distance_efficiency, probs = c(0.05, 0.95), na.rm = TRUE)
################################################################################
#                          Eficiencia por Distancia
################################################################################
electric_cars <- electric_cars %>%
  mutate(charge_rest = ((State.of.Charge..End...) - (State.of.Charge..Start...))/100) 

electric_cars <- electric_cars %>%
  mutate(energy_used = (electric_cars$Battery.Capacity..kWh.) * (electric_cars$charge_rest))

electric_cars <- electric_cars %>%
  mutate(distance_efficiency = (electric_cars$Distance.Driven..since.last.charge...km.) / (electric_cars$energy_used)) 

percentiles <- quantile(electric_cars$distance_efficiency, probs = c(0.05, 0.95), na.rm = TRUE)
lower_bound <- percentiles[1] # Límite inferior (5%)
upper_bound <- percentiles[2] # Límite superior (95%)

five_percent <- ggplot(
  electric_cars %>%
    filter(distance_efficiency >= 0 & distance_efficiency <= upper_bound),
  aes(x = Vehicle.Model, y = distance_efficiency, fill = Vehicle.Model)
) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", shape = 23, size = 3, color = "black") +
  labs(
    x = "Modelo",
    y = "Kilómetros por kWh"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

percentiles_2 <- quantile(electric_cars$distance_efficiency, probs = c(0.10, 0.90), na.rm = TRUE)
lower_bound_2 <- percentiles_2[1] # Límite inferior (5%)
upper_bound_2 <- percentiles_2[2] # Límite superior (95%)

ten_percent <- ggplot(
  electric_cars %>%
    filter(distance_efficiency >= 0 & distance_efficiency <= upper_bound_2),
  aes(x = Vehicle.Model, y = distance_efficiency, fill = Vehicle.Model)
) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", shape = 23, size = 3, color = "black") +
  labs(
    x = "Modelo",
    y = "Kilómetros por kWh"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

grid.arrange(five_percent, ten_percent, ncol = 2)

################################################################################
#                         Gráfico Costo por kWh
################################################################################

electric_cars <- electric_cars %>% 
  mutate(
    cost_per_kWh = Charging.Cost..USD. / energy_used,
    cost_per_kWh = ifelse(is.na(cost_per_kWh), 0, cost_per_kWh)
  )

electric_cars_filtered <- electric_cars %>%
  group_by(Charging.Station.Location) %>%
  mutate(
    Q1 = quantile(cost_per_kWh, 0.25, na.rm = TRUE),
    Q3 = quantile(cost_per_kWh, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_bound = Q3 + 1.5 * IQR) %>%
  filter(cost_per_kWh >= 0 & cost_per_kWh <= upper_bound) %>%
  ungroup() 

ggplot(
  electric_cars_filtered, 
  aes(x = Charging.Station.Location, y = cost_per_kWh, fill = Charging.Station.Location)) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", shape = 23, size = 3, color = "black") +
  labs(
    x = "Ciudad",
    y = "Costo por kW en USD") +
  theme_minimal(base_size = 14) +
  theme(legend.position= "none")


################################################################################
#                             Matriz de correlación
################################################################################

cor_matrix <- cor(electric_cars[, c("Distance.Driven..since.last.charge...km.", "Temperature..Â.C.", "Vehicle.Age..years.", 
                                    "State.of.Charge..Start...", "State.of.Charge..End...", 
                                    "Charging.Cost..USD.", "Charging.Duration..hours.", 
                                    "Charging.Rate..kW.", "Battery.Capacity..kWh.", "energy_used", "distance_efficiency", "charging_efficiency")], 
                  use = "complete.obs")

ggcorrplot(cor_matrix, lab = TRUE)

colnames(electric_cars)


################################################################################
#                    Distribución de la duración de carga
################################################################################


bin_width <- 0.5  
n <- nrow(electric_cars) 

ggplot(electric_cars) +
  geom_histogram(
    mapping = aes(x = Charging.Duration..hours., y = ..count..), 
    binwidth = bin_width,
    fill = "steelblue",
    color = "black",
    alpha = 0.8) +
  geom_density(
    aes(x = Charging.Duration..hours., y = ..density.. * n * bin_width),
    color = "red",
    size = 1) +
  labs(
    x = "Duración de Carga (Hs)",
    y = "Frecuencia absoluta") +
  theme_minimal(base_size = 14)


################################################################################
#                  Gráfico Dispersión de carga (Duración - Costo)
################################################################################


ggplot(electric_cars, aes(x = Charging.Duration..hours., y = Charging.Cost..USD., color = Charger.Type)) +
  geom_point( size = 3, alpha = 0.7) +
  labs(
    x = "Duración de Carga (Hs)",
    y = "Costo de Carga (USD)") +
  theme_minimal(base_size = 14)+
  theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))+
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE)
  


################################################################################
# Gráfico de dispersión (Energía usada x Estado de carga(inicio) x Tipo de usuario)
################################################################################

ggplot(electric_cars %>% filter (energy_used > 0)) +
  geom_point(mapping = aes(x = energy_used, y = State.of.Charge..Start..., color = User.Type)) +
  geom_smooth(mapping =aes(x = energy_used, y = State.of.Charge..Start...), method = "lm", se = TRUE) +
  labs(
    x = "Energia usada (kW)",
    y = "Estado de carga al Inicio (kW)") +
theme_minimal(base_size = 14) +
theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  legend.position = "none",
  axis.text.x = element_text(angle = 0, hjust = 1 ))


################################################################################
# Función para calcular estadísticas descriptivas
################################################################################

# Función sin variable categorica 

calcular_estadisticas_sin_categorica <- function(data, columna) {
  data %>%
    summarise(
      mean = round(mean(.data[[columna]], na.rm = TRUE), 2),
      median = round(median(.data[[columna]], na.rm = TRUE), 2),
      sd = round(sd(.data[[columna]], na.rm = TRUE), 2),
      var = round(var(.data[[columna]], na.rm = TRUE), 2),
      cv = sd(.data[[columna]], na.rm = TRUE) / mean(.data[[columna]], na.rm = TRUE) * 100,
      q1 = round(quantile(.data[[columna]], 0.25, na.rm = TRUE), 2),
      q3 = round(quantile(.data[[columna]], 0.75, na.rm = TRUE), 2),
      min = round(min(.data[[columna]], na.rm = TRUE), 2),
      max = round(max(.data[[columna]], na.rm = TRUE), 2)
    )
}

estadisticas_2 <- calcular_estadisticas_sin_categorica(
  electric_cars %>% filter(Charging.Duration..hours. > 0),
  "Charging.Duration..hours."
)

# Sin filtro > 0

estadisticas_2 <- calcular_estadisticas_sin_categorica(
  electric_cars, "Temperature..Â.C."
)

print(estadisticas_2)


# Función con variable categórica

calcular_estadisticas <- function(data, categorica, columna) {
  data %>% group_by(!!sym(categorica)) %>%
    summarise(
      mean = round(mean(.data[[columna]], na.rm = TRUE), 2),
      median = round(median(.data[[columna]], na.rm = TRUE), 2),
      moda = Mode(.data[[columna]], na.rm = TRUE),
      sd = sd(.data[[columna]], na.rm = TRUE),
      var = var(.data[[columna]], na.rm = TRUE),
      cv = sd(.data[[columna]], na.rm = TRUE) / mean(.data[[columna]], na.rm = TRUE) * 100,
      q1 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
      q3 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
      min = min(.data[[columna]], na.rm = TRUE),
      max = max(.data[[columna]], na.rm = TRUE),
      conteo = n()
    )
}

estadisticas <- calcular_estadisticas(
  electric_cars %>% filter(cost_per_kWh > 0), "Charging.Station.Location",
  "cost_per_kWh"
)

# Sin filtro > 0

estadisticas <- calcular_estadisticas(
  electric_cars, "Vehicle.Model", "Vehicle.Age..years."
)

print(estadisticas)

colnames(electric_cars)

calcular_estadisticas <- function(data, columna) {
  data %>%
    summarise(
      median = round(median(.data[[columna]], na.rm = TRUE), 2),
      mean = round(mean(.data[[columna]], na.rm = TRUE), 2),
      sd = round(sd(.data[[columna]], na.rm = TRUE), 2),
      var = round(var(.data[[columna]], na.rm = TRUE), 2),
      cv = round(sd(.data[[columna]], na.rm = TRUE) / mean(.data[[columna]], na.rm = TRUE), 2 ) ,
      q1 = round(quantile(.data[[columna]], 0.25, na.rm = TRUE), 2),
      q3 = round(quantile(.data[[columna]], 0.75, na.rm = TRUE), 2),
      min = round(min(.data[[columna]], na.rm = TRUE), 2),
      max = round(max(.data[[columna]], na.rm = TRUE), 2)
    )
}

##################### Filtro Superior ##################### 

filtered_data <- electric_cars %>%
  filter(distance_efficiency > 0) %>%
  filter(distance_efficiency <= quantile(distance_efficiency, 0.90, na.rm = TRUE))

estadisticas_filtradas <- calcular_estadisticas(filtered_data, "Vehicle.Model", "distance_efficiency")

print(estadisticas_filtradas)

################### Filtro por Q3 + 1.5 * IQR ################### 

q1 <- quantile(electric_cars$cost_per_kWh, 0.25, na.rm = TRUE)
q3 <- quantile(electric_cars$cost_per_kWh, 0.75, na.rm = TRUE)
iqr <- q3 - q1

upper_limit <- q3 + 1.5 * iqr
lower_limit <- q1 - 1.5 * iqr


filtered_data <- electric_cars %>%
  filter(cost_per_kWh > 0) %>% 
  filter(cost_per_kWh >= lower_limit & cost_per_kWh <= upper_limit)

estadisticas_filtradas <- calcular_estadisticas(filtered_data, "Charging.Station.Location", "cost_per_kWh")

print(estadisticas_filtradas)

