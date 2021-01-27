%-------------------------------------
% Color de cada línea
%-------------------------------------
assert(color(línea_1, rosa)).
assert(color(línea_2, azul_marino)).
assert(color(línea_3, verde_olivo)).
assert(color(línea_4, azul_cielo)).
assert(color(línea_5, amarillo)).
assert(color(línea_6, rojo)).
assert(color(línea_7, naranja)).
assert(color(línea_8, verde_bandera)).
assert(color(línea_9, cafe)).
assert(color(línea_a, morado)).
assert(color(línea_b, gris_verdoso)).
assert(color(línea_12, dorado)).

%-------------------------------------
% Trayectos por línea
%-------------------------------------
assert(trayecto(línea_1, observatorio, pantitlán)).
assert(trayecto(línea_2, cuatro_caminos, tasqueña)).
assert(trayecto(línea_3, indios_verdes, universidad)).
assert(trayecto(línea_4, martín_carrera, santa_anita)).
assert(trayecto(línea_5, politécnico, pantitlán)).
assert(trayecto(línea_6, el_rosario, martín_carrera)).
assert(trayecto(línea_7, el_rosario, barranca_del_muerto)).
assert(trayecto(línea_8, garibaldi, constitución_de_1917)).
assert(trayecto(línea_9, tacubaya, pantitlán)).
assert(trayecto(línea_a, pantitlán, la_paz)).
assert(trayecto(línea_b, buenavista, ciudad_azteca)).
assert(trayecto(línea_12, mixcoac, tláhuac)).

%-----------------------------------------
% línea 1: Observatorio - Pantitlan
%-----------------------------------------
assert(sigue(observatorio, tacubaya, línea_1)).
assert(sigue(tacubaya, juanacatlán, línea_1)).
assert(sigue(juanacatlán, chapultepec, línea_1)).
assert(sigue(chapultepec, sevilla, línea_1)).
assert(sigue(sevilla, insurgentes, línea_1)).
assert(sigue(insurgentes, cuauhtémoc, línea_1)).
assert(sigue(cuauhtémoc, balderas, línea_1)).
assert(sigue(balderas, salto_del_agua, línea_1)).
assert(sigue(salto_del_agua, isabel_la_católica, línea_1)).
assert(sigue(isabel_la_católica, pino_suáres, línea_1)).
assert(sigue(pino_suáres, merced, línea_1)).
assert(sigue(merced, candelaria, línea_1)).
assert(sigue(candelaria, san_lázaro, línea_1)).
assert(sigue(san_lázaro, moctezuma, línea_1)).
assert(sigue(moctezuma, balbuena, línea_1)).
assert(sigue(balbuena, boulevard_puerto_aereo, línea_1)).
assert(sigue(boulevard_puerto_aereo, gómez_farias, línea_1)).
assert(sigue(gómez_farias, zaragoza, línea_1)).
assert(sigue(zaragoza, pantitlán, línea_1)).

%-----------------------------------------
% línea 2: Cuatro Caminos - Tasqueña
%-----------------------------------------
assert(sigue(cuatro_caminos, panteones, línea_2)).
assert(sigue(panteones, tacuba, línea_2)).
assert(sigue(tacuba, cuitláhuac, línea_2)).
assert(sigue(cuitláhuac, popotla, línea_2)).
assert(sigue(popotla, colegio_militar, línea_2)).
assert(sigue(colegio_militar, normal, línea_2)).
assert(sigue(normal, san_cosme, línea_2)).
assert(sigue(san_cosme, revolución, línea_2)).
assert(sigue(revolución, hidalgo, línea_2)).
assert(sigue(hidalgo, bellas_artes, línea_2)).
assert(sigue(bellas_artes, allende, línea_2)).
assert(sigue(allende, zócalo, línea_2)).
assert(sigue(zócalo, pino_suáres, línea_2)).
assert(sigue(pino_suáres, san_antonio_abad, línea_2)).
assert(sigue(san_antonio_abad, chabacano, línea_2)).
assert(sigue(chabacano, viaducto, línea_2)).
assert(sigue(viaducto, xola, línea_2)).
assert(sigue(xola, villa_de_cotéz, línea_2)).
assert(sigue(villa_de_cotéz, nativitas, línea_2)).
assert(sigue(nativitas, portales, línea_2)).
assert(sigue(portales, ermita, línea_2)).
assert(sigue(ermita, general_anaya, línea_2)).
assert(sigue(general_anaya, tasqueña, línea_2)).

%-----------------------------------------
% línea 3: Indios Verdes - Universidad
%-----------------------------------------
assert(sigue(indios_verdes, deportivo_18_de_marzo, línea_3)).
assert(sigue(deportivo_18_de_marzo, potrero, línea_3)).
assert(sigue(potrero, la_raza, línea_3)).
assert(sigue(la_raza, tlatelolco, línea_3)).
assert(sigue(tlatelolco, guerrero, línea_3)).
assert(sigue(guerrero, hidalgo, línea_3)).
assert(sigue(hidalgo, juárez, línea_3)).
assert(sigue(juárez, balderas, línea_3)).
assert(sigue(balderas, niños_heroes, línea_3)).
assert(sigue(niños_heroes, hospital_general, línea_3)).
assert(sigue(hospital_general, centro_médico, línea_3)).
assert(sigue(centro_médico, etiopía, línea_3)).
assert(sigue(etiopía, eugenia, línea_3)).
assert(sigue(eugenia, división_del_norte, línea_3)).
assert(sigue(división_del_norte, zapata, línea_3)).
assert(sigue(zapata, coyoacán, línea_3)).
assert(sigue(coyoacán, viveros, línea_3)).
assert(sigue(viveros, m_a_quevedo, línea_3)).
assert(sigue(m_a_quevedo, copilco, línea_3)).
assert(sigue(copilco, universidad, línea_3)).

%-----------------------------------------
% línea 4: Martin Carrera - Santa Anita
%-----------------------------------------
assert(sigue(martín_carrera, talísman, línea_4)).
assert(sigue(talísman, bondojito, línea_4)).
assert(sigue(bondojito, consulado, línea_4)).
assert(sigue(consulado, canal_del_norte, línea_4)).
assert(sigue(canal_del_norte, morelos, línea_4)).
assert(sigue(morelos, candelaria, línea_4)).
assert(sigue(candelaria, fray_servando, línea_4)).
assert(sigue(fray_servando, jamaica, línea_4)).
assert(sigue(jamaica, santa_anita, línea_4)).

%-----------------------------------------
% línea 5: Politécnico - Pantitlan
%-----------------------------------------
assert(sigue(politécnico, instituto_del_petróleo, línea_5)).
assert(sigue(instituto_del_petróleo, autobuses_del_norte, línea_5)).
assert(sigue(autobuses_del_norte, la_raza, línea_5)).
assert(sigue(la_raza, misterios, línea_5)).
assert(sigue(misterios, valle_goméz, línea_5)).
assert(sigue(valle_goméz, consulado, línea_5)).
assert(sigue(consulado, eduardo_molina, línea_5)).
assert(sigue(eduardo_molina, aragón, línea_5)).
assert(sigue(aragón, oceanía, línea_5)).
assert(sigue(oceanía, terminal_aérea, línea_5)).
assert(sigue(terminal_aérea, hangares, línea_5)).
assert(sigue(hangares, pantitlán, línea_5)).

%-----------------------------------------
% línea 6: El Rosario - Martin Carrera
%-----------------------------------------
assert(sigue(el_rosario, tezozomóc, línea_6)).
assert(sigue(tezozomóc, uam_azcapotzalco, línea_6)).
assert(sigue(uam_azcapotzalco, ferrería, línea_6)).
assert(sigue(ferrería, norte_45, línea_6)).
assert(sigue(norte_45, vallejo, línea_6)).
assert(sigue(vallejo, instituto_del_petróleo, línea_6)).
assert(sigue(instituto_del_petróleo, lindavista, línea_6)).
assert(sigue(lindavista, deportivo_18_de_marzo, línea_6)).
assert(sigue(deportivo_18_de_marzo, la_villa_basílica, línea_6)).
assert(sigue(la_villa_basílica, martín_carrera, línea_6)).

%-----------------------------------------
% línea 7: El Rosario - Barranca del Muerto
%-----------------------------------------
assert(sigue(el_rosario, aquiles_serdán, línea_7)).
assert(sigue(aquiles_serdán, camarones, línea_7)).
assert(sigue(camarones, refinería, línea_7)).
assert(sigue(refinería, tacuba, línea_7)).
assert(sigue(tacuba, san_joaquín, línea_7)).
assert(sigue(san_joaquín, polanco, línea_7)).
assert(sigue(polanco, auditorio, línea_7)).
assert(sigue(auditorio, constituyentes, línea_7)).
assert(sigue(constituyentes, tacubaya, línea_7)).
assert(sigue(tacubaya, san_pedro_de_los_pinos, línea_7)).
assert(sigue(san_pedro_de_los_pinos, san_antonio, línea_7)).
assert(sigue(san_antonio, mixcoac, línea_7)).
assert(sigue(mixcoac, barranca_del_muerto, línea_7)).

%-----------------------------------------
% línea 8: Garibaldi - Constitucion de 1917
%-----------------------------------------
assert(sigue(garibaldi, bellas_artes, línea_8)).
assert(sigue(bellas_artes, san_juan_de_letrán, línea_8)).
assert(sigue(san_juan_de_letrán, salto_del_agua, línea_8)).
assert(sigue(salto_del_agua, doctores, línea_8)).
assert(sigue(doctores, lázaro_cárdenas, línea_8)).
assert(sigue(lázaro_cárdenas, chabacano, línea_8)).
assert(sigue(chabacano, la_viga, línea_8)).
assert(sigue(la_viga, santa_anita, línea_8)).
assert(sigue(santa_anita, coyuya, línea_8)).
assert(sigue(coyuya, iztacalco, línea_8)).
assert(sigue(iztacalco, apatlaco, línea_8)).
assert(sigue(apatlaco, aculco, línea_8)).
assert(sigue(aculco, escuadrón_201, línea_8)).
assert(sigue(escuadrón_201, atlalilco, línea_8)).
assert(sigue(atlalilco, iztapalapa, línea_8)).
assert(sigue(iztapalapa, cerro_de_la_estrella, línea_8)).
assert(sigue(cerro_de_la_estrella, uam_i, línea_8)).
assert(sigue(uam_i, constitución_de_1917, línea_8)).

%-----------------------------------------
% línea 9: Tacubaya - Pantitlan
%-----------------------------------------
assert(sigue(tacubaya, patriotismo, línea_9)).
assert(sigue(patriotismo, chilpancingo, línea_9)).
assert(sigue(chilpancingo, centro_médico, línea_9)).
assert(sigue(centro_médico, lázaro_cárdenas, línea_9)).
assert(sigue(chabacano, jamaica, línea_9)).
assert(sigue(jamaica, mixiuhca, línea_9)).
assert(sigue(mixiuhca, velódromo, línea_9)).
assert(sigue(velódromo, ciudad_deportiva, línea_9)).
assert(sigue(ciudad_deportiva, puebla, línea_9)).
assert(sigue(puebla, pantitlán, línea_9)).

%-----------------------------------------
% línea A: Pantitlan - La Paz
%-----------------------------------------
assert(sigue(pantitlán, agrícola_ambiental, línea_a)).
assert(sigue(agrícola_ambiental, canal_de_san_juan, línea_a)).
assert(sigue(canal_de_san_juan, tepalcates, línea_a)).
assert(sigue(tepalcates, guelatao, línea_a)).
assert(sigue(guelatao, peñón_viejo, línea_a)).
assert(sigue(peñón_viejo, acatitla, línea_a)).
assert(sigue(acatitla, santa_martha, línea_a)).
assert(sigue(santa_martha, los_reyes, línea_a)).
assert(sigue(los_reyes, la_paz, línea_a)).

%-----------------------------------------
% línea B: Buenavista - Cd. Azteca
%-----------------------------------------
assert(sigue(buenavista, guerrero, línea_b)).
assert(sigue(guerrero, garibaldi, línea_b)).
assert(sigue(garibaldi, lagunilla, línea_b)).
assert(sigue(lagunilla, tepito, línea_b)).
assert(sigue(tepito, morelos, línea_b)).
assert(sigue(morelos, san_lázaro, línea_b)).
assert(sigue(san_lázaro, ricardo_flores_magón, línea_b)).
assert(sigue(ricardo_flores_magón, romero_rubio, línea_b)).
assert(sigue(romero_rubio, oceanía, línea_b)).
assert(sigue(oceanía, deportivo_oceanía, línea_b)).
assert(sigue(deportivo_oceanía, bosque_aragón, línea_b)).
assert(sigue(bosque_aragón, villa_aragón, línea_b)).
assert(sigue(villa_aragón, nezahualcóyotl, línea_b)).
assert(sigue(nezahualcóyotl, impulsora, línea_b)).
assert(sigue(impulsora, rio_de_los_remedios, línea_b)).
assert(sigue(rio_de_los_remedios, múzquiz, línea_b)).
assert(sigue(múzquiz, ecatepec, línea_b)).
assert(sigue(ecatepec, olímpica, línea_b)).
assert(sigue(olímpica, plaza_aragón, línea_b)).
assert(sigue(plaza_aragón, ciudad_azteca, línea_b)).

%-----------------------------------------
% línea 12: Mixcoac - Tlahuac
%-----------------------------------------
assert(sigue(mixcoac, insurgentes_sur, línea_12)).
assert(sigue(insurgentes_sur, hospital_20_de_noviembre, línea_12)).
assert(sigue(hospital_20_de_noviembre, zapata, línea_12)).
assert(sigue(zapata, parque_de_los_venados, línea_12)).
assert(sigue(parque_de_los_venados, eje_central, línea_12)).
assert(sigue(eje_central, ermita, línea_12)).
assert(sigue(ermita, mexicaltzingo, línea_12)).
assert(sigue(mexicaltzingo, atlalilco, línea_12)).
assert(sigue(atlalilco, culhuacán, línea_12)).
assert(sigue(culhuacán, san_andrés_tomatlán, línea_12)).
assert(sigue(san_andrés_tomatlán, calle_11, línea_12)).
assert(sigue(calle_11, periférico_oriente, línea_12)).
assert(sigue(periférico_oriente, tezonco, línea_12)).
assert(sigue(tezonco, olivos, línea_12)).
assert(sigue(olivos, nopalera, línea_12)).
assert(sigue(nopalera, zapotitlán, línea_12)).
assert(sigue(zapotitlán, tlaltenco, línea_12)).
assert(sigue(tlaltenco, tláhuac, línea_12)).

%-------------------------------------
% Reglas
%-------------------------------------
assert(conecta(X, Y, L) :-
  sigue(X, Y, L);
  sigue(Y, X, L)).

assert(alcanzable(X, Y) :-
  conecta(X, Y, L)).

assert(alcanzable(X, Y) :-
  conecta(X, Z, _),
  alcanzable(Z, Y, _)).