
get_jugadores <- function(){
  output <- gs_title('Clasificación FutbolBA') %>% 
    gs_read('jugadores') %>% 
    filter(Nombre != 'Invitado') %>% 
    select(Jugador = Nombre)
  
  output
}

get_goals_by_player <- function(dat){
  output <- dat %>%
    group_by(Jugador) %>% 
    summarise(
      Partidos = n(),
      GF = sum(GF),
      GC = sum(GC),
      Goleador = round(GF / Partidos,1),
      Defensa = round(GC / Partidos,1)
    ) %>% 
    ungroup()
  
  output
}
get_results_by_player <- function(dat){
  output <- dat %>%
    group_by(Jugador, Resultado) %>% 
    summarise(
      num_partits = n()
    ) %>% 
    ungroup()
  
  output
}

get_clasificacion <- function(results_by_player, goals_by_player, filter_value){
  # initialising tibble
  type_results <-  tibble(Resultado = c('Ganado', 'Empatado', 'Perdido'), 
                          Jugador = c('dummy','dummy','dummy'))
  
  #jugadores <- get_jugadores()
  
  output <- results_by_player %>%
    #right_join(jugadores) %>%
    #mutate(
    #  Resultado = ifelse(is.na(Resultado),'Ganado', Resultado)
    #) %>%
    full_join(type_results) %>%
    spread(Resultado, num_partits, fill = 0) %>% 
    filter(Jugador != 'dummy') %>% 
    mutate(
      Partidos = Ganado + Empatado + Perdido,
      Puntos = Ganado*3 + Empatado*1,
      Coeficiente = round(Puntos / Partidos, 2)
    ) %>%
    filter_(filter_value) %>% 
    left_join(goals_by_player) %>%
    rename(Ganados = Ganado, Empatados = Empatado, Perdidos = Perdido) %>%
    select(Jugador, PJ = Partidos, PG = Ganados, PE = Empatados, PP = Perdidos, Pts = Puntos, Coef = Coeficiente, GF, GC, Gol = Goleador, Def = Defensa) %>% 
    # default ordering by coeficiente
    arrange(-Coef) %>% 
    mutate(
      Coef = ifelse(is.nan(Coef) | is.na(Coef), 0, Coef),
      GF = ifelse(is.nan(GF) | is.na(GF), 0, GF),
      GC = ifelse(is.nan(GC) | is.na(GC), 0, GC),
      Gol = ifelse(is.nan(Gol) | is.na(Gol), 0, Gol),
      Def = ifelse(is.nan(Def) | is.na(Def), 0, Def),
      Dif = GF - GC
    ) %>% 
    # creating position as column
    mutate(`#` = row_number()) %>%
    # orden tablas a mostrar
    select(`#`, Jugador, PJ, PG, PE, PP, Pts, Coef, GF, GC, Dif, Gol, Def)
  
  output
}

# calculo equipómetro

get_equipometro <- function(dat, team_jugadores, n_partidos, team){

  last_n_by_player <- dat %>%
    filter(Jugador %in% team_jugadores) %>% 
    group_by(Jugador) %>% 
    top_n(n_partidos,Fecha)
  
  goals_by_player <- get_goals_by_player(last_n_by_player)
  results_by_player <- get_results_by_player(last_n_by_player)
  
  valoracion <- get_clasificacion(results_by_player, goals_by_player) %>% 
    mutate(
      equipo = team
    ) %>%
    group_by(equipo) %>% 
    summarise(
      Coef_medio = mean(Coef),
      Gol_medio = mean(Gol),
      Def_medio = mean(Def)
    )
  
  valoracion
}