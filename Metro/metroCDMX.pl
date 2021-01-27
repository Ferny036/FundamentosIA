%-------------------------------------
% Color de cada línea
%-------------------------------------
color(línea_1, rosa).
color(línea_2, azul_marino).
color(línea_3, verde_olivo).
color(línea_4, azul_cielo).
color(línea_5, amarillo).
color(línea_6, rojo).
color(línea_7, naranja).
color(línea_8, verde_bandera).
color(línea_9, cafe).
color(línea_a, morado).
color(línea_b, gris_verdoso).
color(línea_12, dorado).

%-------------------------------------
% Trayectos por línea
%-------------------------------------
trayecto(línea_1, observatorio, pantitlán).
trayecto(línea_2, cuatro_caminos, tasqueña).
trayecto(línea_3, indios_verdes, universidad).
trayecto(línea_4, martín_carrera, santa_anita).
trayecto(línea_5, politécnico, pantitlán).
trayecto(línea_6, el_rosario, martín_carrera).
trayecto(línea_7, el_rosario, barranca_del_muerto).
trayecto(línea_8, garibaldi, constitución_de_1917).
trayecto(línea_9, tacubaya, pantitlán).
trayecto(línea_a, pantitlán, la_paz).
trayecto(línea_b, buenavista, ciudad_azteca).
trayecto(línea_12, mixcoac, tláhuac).

%-----------------------------------------
% línea 1: Observatorio - Pantitlan
%-----------------------------------------
sigue(observatorio, tacubaya, línea_1).
sigue(tacubaya, juanacatlán, línea_1).
sigue(juanacatlán, chapultepec, línea_1).
sigue(chapultepec, sevilla, línea_1).
sigue(sevilla, insurgentes, línea_1).
sigue(insurgentes, cuauhtémoc, línea_1).
sigue(cuauhtémoc, balderas, línea_1).
sigue(balderas, salto_del_agua, línea_1).
sigue(salto_del_agua, isabel_la_católica, línea_1).
sigue(isabel_la_católica, pino_suáres, línea_1).
sigue(pino_suáres, merced, línea_1).
sigue(merced, candelaria, línea_1).
sigue(candelaria, san_lázaro, línea_1).
sigue(san_lázaro, moctezuma, línea_1).
sigue(moctezuma, balbuena, línea_1).
sigue(balbuena, boulevard_puerto_aereo, línea_1).
sigue(boulevard_puerto_aereo, gómez_farias, línea_1).
sigue(gómez_farias, zaragoza, línea_1).
sigue(zaragoza, pantitlán, línea_1).

%-----------------------------------------
% línea 2: Cuatro Caminos - Tasqueña
%-----------------------------------------
sigue(cuatro_caminos, panteones, línea_2).
sigue(panteones, tacuba, línea_2).
sigue(tacuba, cuitláhuac, línea_2).
sigue(cuitláhuac, popotla, línea_2).
sigue(popotla, colegio_militar, línea_2).
sigue(colegio_militar, normal, línea_2).
sigue(normal, san_cosme, línea_2).
sigue(san_cosme, revolución, línea_2).
sigue(revolución, hidalgo, línea_2).
sigue(hidalgo, bellas_artes, línea_2).
sigue(bellas_artes, allende, línea_2).
sigue(allende, zócalo, línea_2).
sigue(zócalo, pino_suáres, línea_2).
sigue(pino_suáres, san_antonio_abad, línea_2).
sigue(san_antonio_abad, chabacano, línea_2).
sigue(chabacano, viaducto, línea_2).
sigue(viaducto, xola, línea_2).
sigue(xola, villa_de_cotéz, línea_2).
sigue(villa_de_cotéz, nativitas, línea_2).
sigue(nativitas, portales, línea_2).
sigue(portales, ermita, línea_2).
sigue(ermita, general_anaya, línea_2).
sigue(general_anaya, tasqueña, línea_2).

%-----------------------------------------
% línea 3: Indios Verdes - Universidad
%-----------------------------------------
sigue(indios_verdes, deportivo_18_de_marzo, línea_3).
sigue(deportivo_18_de_marzo, potrero, línea_3).
sigue(potrero, la_raza, línea_3).
sigue(la_raza, tlatelolco, línea_3).
sigue(tlatelolco, guerrero, línea_3).
sigue(guerrero, hidalgo, línea_3).
sigue(hidalgo, juárez, línea_3).
sigue(juárez, balderas, línea_3).
sigue(balderas, niños_heroes, línea_3).
sigue(niños_heroes, hospital_general, línea_3).
sigue(hospital_general, centro_médico, línea_3).
sigue(centro_médico, etiopía, línea_3).
sigue(etiopía, eugenia, línea_3).
sigue(eugenia, división_del_norte, línea_3).
sigue(división_del_norte, zapata, línea_3).
sigue(zapata, coyoacán, línea_3).
sigue(coyoacán, viveros, línea_3).
sigue(viveros, m_a_quevedo, línea_3).
sigue(m_a_quevedo, copilco, línea_3).
sigue(copilco, universidad, línea_3).

%-----------------------------------------
% línea 4: Martin Carrera - Santa Anita
%-----------------------------------------
sigue(martín_carrera, talísman, línea_4).
sigue(talísman, bondojito, línea_4).
sigue(bondojito, consulado, línea_4).
sigue(consulado, canal_del_norte, línea_4).
sigue(canal_del_norte, morelos, línea_4).
sigue(morelos, candelaria, línea_4).
sigue(candelaria, fray_servando, línea_4).
sigue(fray_servando, jamaica, línea_4).
sigue(jamaica, santa_anita, línea_4).

%-----------------------------------------
% línea 5: Politécnico - Pantitlan
%-----------------------------------------
sigue(politécnico, instituto_del_petróleo, línea_5).
sigue(instituto_del_petróleo, autobuses_del_norte, línea_5).
sigue(autobuses_del_norte, la_raza, línea_5).
sigue(la_raza, misterios, línea_5).
sigue(misterios, valle_goméz, línea_5).
sigue(valle_goméz, consulado, línea_5).
sigue(consulado, eduardo_molina, línea_5).
sigue(eduardo_molina, aragón, línea_5).
sigue(aragón, oceanía, línea_5).
sigue(oceanía, terminal_aérea, línea_5).
sigue(terminal_aérea, hangares, línea_5).
sigue(hangares, pantitlán, línea_5).

%-----------------------------------------
% línea 6: El Rosario - Martin Carrera
%-----------------------------------------
sigue(el_rosario, tezozomóc, línea_6).
sigue(tezozomóc, uam_azcapotzalco, línea_6).
sigue(uam_azcapotzalco, ferrería, línea_6).
sigue(ferrería, norte_45, línea_6).
sigue(norte_45, vallejo, línea_6).
sigue(vallejo, instituto_del_petróleo, línea_6).
sigue(instituto_del_petróleo, lindavista, línea_6).
sigue(lindavista, deportivo_18_de_marzo, línea_6).
sigue(deportivo_18_de_marzo, la_villa_basílica, línea_6).
sigue(la_villa_basílica, martín_carrera, línea_6).

%-----------------------------------------
% línea 7: El Rosario - Barranca del Muerto
%-----------------------------------------
sigue(el_rosario, aquiles_serdán, línea_7).
sigue(aquiles_serdán, camarones, línea_7).
sigue(camarones, refinería, línea_7).
sigue(refinería, tacuba, línea_7).
sigue(tacuba, san_joaquín, línea_7).
sigue(san_joaquín, polanco, línea_7).
sigue(polanco, auditorio, línea_7).
sigue(auditorio, constituyentes, línea_7).
sigue(constituyentes, tacubaya, línea_7).
sigue(tacubaya, san_pedro_de_los_pinos, línea_7).
sigue(san_pedro_de_los_pinos, san_antonio, línea_7).
sigue(san_antonio, mixcoac, línea_7).
sigue(mixcoac, barranca_del_muerto, línea_7).

%-----------------------------------------
% línea 8: Garibaldi - Constitucion de 1917
%-----------------------------------------
sigue(garibaldi, bellas_artes, línea_8).
sigue(bellas_artes, san_juan_de_letrán, línea_8).
sigue(san_juan_de_letrán, salto_del_agua, línea_8).
sigue(salto_del_agua, doctores, línea_8).
sigue(doctores, lázaro_cárdenas, línea_8).
sigue(lázaro_cárdenas, chabacano, línea_8).
sigue(chabacano, la_viga, línea_8).
sigue(la_viga, santa_anita, línea_8).
sigue(santa_anita, coyuya, línea_8).
sigue(coyuya, iztacalco, línea_8).
sigue(iztacalco, apatlaco, línea_8).
sigue(apatlaco, aculco, línea_8).
sigue(aculco, escuadrón_201, línea_8).
sigue(escuadrón_201, atlalilco, línea_8).
sigue(atlalilco, iztapalapa, línea_8).
sigue(iztapalapa, cerro_de_la_estrella, línea_8).
sigue(cerro_de_la_estrella, uam_i, línea_8).
sigue(uam_i, constitución_de_1917, línea_8).

%-----------------------------------------
% línea 9: Tacubaya - Pantitlan
%-----------------------------------------
sigue(tacubaya, patriotismo, línea_9).
sigue(patriotismo, chilpancingo, línea_9).
sigue(chilpancingo, centro_médico, línea_9).
sigue(centro_médico, lázaro_cárdenas, línea_9).
sigue(chabacano, jamaica, línea_9).
sigue(jamaica, mixiuhca, línea_9).
sigue(mixiuhca, velódromo, línea_9).
sigue(velódromo, ciudad_deportiva, línea_9).
sigue(ciudad_deportiva, puebla, línea_9).
sigue(puebla, pantitlán, línea_9).

%-----------------------------------------
% línea A: Pantitlan - La Paz
%-----------------------------------------
sigue(pantitlán, agrícola_ambiental, línea_a).
sigue(agrícola_ambiental, canal_de_san_juan, línea_a).
sigue(canal_de_san_juan, tepalcates, línea_a).
sigue(tepalcates, guelatao, línea_a).
sigue(guelatao, peñón_viejo, línea_a).
sigue(peñón_viejo, acatitla, línea_a).
sigue(acatitla, santa_martha, línea_a).
sigue(santa_martha, los_reyes, línea_a).
sigue(los_reyes, la_paz, línea_a).

%-----------------------------------------
% línea B: Buenavista - Cd. Azteca
%-----------------------------------------
sigue(buenavista, guerrero, línea_b).
sigue(guerrero, garibaldi, línea_b).
sigue(garibaldi, lagunilla, línea_b).
sigue(lagunilla, tepito, línea_b).
sigue(tepito, morelos, línea_b).
sigue(morelos, san_lázaro, línea_b).
sigue(san_lázaro, ricardo_flores_magón, línea_b).
sigue(ricardo_flores_magón, romero_rubio, línea_b).
sigue(romero_rubio, oceanía, línea_b).
sigue(oceanía, deportivo_oceanía, línea_b).
sigue(deportivo_oceanía, bosque_aragón, línea_b).
sigue(bosque_aragón, villa_aragón, línea_b).
sigue(villa_aragón, nezahualcóyotl, línea_b).
sigue(nezahualcóyotl, impulsora, línea_b).
sigue(impulsora, rio_de_los_remedios, línea_b).
sigue(rio_de_los_remedios, múzquiz, línea_b).
sigue(múzquiz, ecatepec, línea_b).
sigue(ecatepec, olímpica, línea_b).
sigue(olímpica, plaza_aragón, línea_b).
sigue(plaza_aragón, ciudad_azteca, línea_b).

%-----------------------------------------
% línea 12: Mixcoac - Tlahuac
%-----------------------------------------
sigue(mixcoac, insurgentes_sur, línea_12).
sigue(insurgentes_sur, hospital_20_de_noviembre, línea_12).
sigue(hospital_20_de_noviembre, zapata, línea_12).
sigue(zapata, parque_de_los_venados, línea_12).
sigue(parque_de_los_venados, eje_central, línea_12).
sigue(eje_central, ermita, línea_12).
sigue(ermita, mexicaltzingo, línea_12).
sigue(mexicaltzingo, atlalilco, línea_12).
sigue(atlalilco, culhuacán, línea_12).
sigue(culhuacán, san_andrés_tomatlán, línea_12).
sigue(san_andrés_tomatlán, calle_11, línea_12).
sigue(calle_11, periférico_oriente, línea_12).
sigue(periférico_oriente, tezonco, línea_12).
sigue(tezonco, olivos, línea_12).
sigue(olivos, nopalera, línea_12).
sigue(nopalera, zapotitlán, línea_12).
sigue(zapotitlán, tlaltenco, línea_12).
sigue(tlaltenco, tláhuac, línea_12).

%-------------------------------------
% Reglas
%-------------------------------------
conecta(X, Y, L) :-
  sigue(X, Y, L);
  sigue(Y, X, L).

alcanzable(X, Y) :-
  conecta(X, Y, L).

alcanzable(X, Y) :-
  conecta(X, Z, _),
  alcanzable(Z, Y, _).