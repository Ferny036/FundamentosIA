%-------------------------------------------------------------------------------
% metroCDMX.pl
%     Modelo la red del Sistema de Transporte Colectivo METRO de la Ciudad de
%     México:
%
%     Operación 'Determinación del trayecto entre dos estaciones':
%     alcanzable(<estación_origen>, <estación_destino>, <trayecto>)
%
%       * <estación_origen>:  Estación donde inicia el trayecto.
%       * <estación_destino>: Estación donde terminará el proyecto.
%       * <trayecto>: Lista de retorno del trayecto completo,
%         desde la <estación_origen> hasta <estación_destino>
%
%     Ejemplo:
%       alcanzable(lázaro_cárdenas, jamaica, Path).
%
%     Retorna:
%       Path = [lázaro_cárdenas, centro_médico, etiopía, eugenia|...]
%
%   Rivera Paredes Fernando Daniel
% Enero, 2021
%-------------------------------------------------------------------------------

%------------------------------------------------------------------------------%
%                         CONOCIMIENTO DECLARATIVO                             %
%------------------------------------------------------------------------------%

%-------------------------------------
% Color de cada linea
%-------------------------------------
color(linea_1, rosa).
color(linea_2, azul_marino).
color(linea_3, verde_olivo).
color(linea_4, azul_cielo).
color(linea_5, amarillo).
color(linea_6, rojo).
color(linea_7, naranja).
color(linea_8, verde_bandera).
color(linea_9, café).
color(linea_a, morado).
color(linea_b, gris_verdoso).
color(linea_12, dorado).

%-------------------------------------
% Trayectos por linea
%-------------------------------------
trayecto(linea_1, observatorio, pantitlán).
trayecto(linea_2, cuatro_caminos, tasqueña).
trayecto(linea_3, indios_verdes, universidad).
trayecto(linea_4, martín_carrera, santa_anita).
trayecto(linea_5, politécnico, pantitlán).
trayecto(linea_6, el_rosario, martín_carrera).
trayecto(linea_7, el_rosario, barranca_del_muerto).
trayecto(linea_8, garibaldi, constitución_de_1917).
trayecto(linea_9, tacubaya, pantitlán).
trayecto(linea_a, pantitlán, la_paz).
trayecto(linea_b, buenavista, ciudad_azteca).
trayecto(linea_12, mixcoac, tláhuac).

%-----------------------------------------
% linea 1: Observatorio - Pantitlan
%-----------------------------------------
sigue(observatorio, tacubaya, linea_1).
sigue(tacubaya, juanacatlán, linea_1).
sigue(juanacatlán, chapultepec, linea_1).
sigue(chapultepec, sevilla, linea_1).
sigue(sevilla, insurgentes, linea_1).
sigue(insurgentes, cuauhtémoc, linea_1).
sigue(cuauhtémoc, balderas, linea_1).
sigue(balderas, salto_del_agua, linea_1).
sigue(salto_del_agua, isabel_la_católica, linea_1).
sigue(isabel_la_católica, pino_suáres, linea_1).
sigue(pino_suáres, merced, linea_1).
sigue(merced, candelaria, linea_1).
sigue(candelaria, san_lázaro, linea_1).
sigue(san_lázaro, moctezuma, linea_1).
sigue(moctezuma, balbuena, linea_1).
sigue(balbuena, boulevard_puerto_aereo, linea_1).
sigue(boulevard_puerto_aereo, gómez_farias, linea_1).
sigue(gómez_farias, zaragoza, linea_1).
sigue(zaragoza, pantitlán, linea_1).

%-----------------------------------------
% linea 2: Cuatro Caminos - Tasqueña
%-----------------------------------------
sigue(cuatro_caminos, panteones, linea_2).
sigue(panteones, tacuba, linea_2).
sigue(tacuba, cuitláhuac, linea_2).
sigue(cuitláhuac, popotla, linea_2).
sigue(popotla, colegio_militar, linea_2).
sigue(colegio_militar, normal, linea_2).
sigue(normal, san_cosme, linea_2).
sigue(san_cosme, revolución, linea_2).
sigue(revolución, hidalgo, linea_2).
sigue(hidalgo, bellas_artes, linea_2).
sigue(bellas_artes, allende, linea_2).
sigue(allende, zócalo, linea_2).
sigue(zócalo, pino_suáres, linea_2).
sigue(pino_suáres, san_antonio_abad, linea_2).
sigue(san_antonio_abad, chabacano, linea_2).
sigue(chabacano, viaducto, linea_2).
sigue(viaducto, xola, linea_2).
sigue(xola, villa_de_cotéz, linea_2).
sigue(villa_de_cotéz, nativitas, linea_2).
sigue(nativitas, portales, linea_2).
sigue(portales, ermita, linea_2).
sigue(ermita, general_anaya, linea_2).
sigue(general_anaya, tasqueña, linea_2).

%-----------------------------------------
% linea 3: Indios Verdes - Universidad
%-----------------------------------------
sigue(indios_verdes, deportivo_18_de_marzo, linea_3).
sigue(deportivo_18_de_marzo, potrero, linea_3).
sigue(potrero, la_raza, linea_3).
sigue(la_raza, tlatelolco, linea_3).
sigue(tlatelolco, guerrero, linea_3).
sigue(guerrero, hidalgo, linea_3).
sigue(hidalgo, juárez, linea_3).
sigue(juárez, balderas, linea_3).
sigue(balderas, niños_heroes, linea_3).
sigue(niños_heroes, hospital_general, linea_3).
sigue(hospital_general, centro_médico, linea_3).
sigue(centro_médico, etiopía, linea_3).
sigue(etiopía, eugenia, linea_3).
sigue(eugenia, división_del_norte, linea_3).
sigue(división_del_norte, zapata, linea_3).
sigue(zapata, coyoacán, linea_3).
sigue(coyoacán, viveros, linea_3).
sigue(viveros, m_a_quevedo, linea_3).
sigue(m_a_quevedo, copilco, linea_3).
sigue(copilco, universidad, linea_3).

%-----------------------------------------
% linea 4: Martin Carrera - Santa Anita
%-----------------------------------------
sigue(martín_carrera, talísman, linea_4).
sigue(talísman, bondojito, linea_4).
sigue(bondojito, consulado, linea_4).
sigue(consulado, canal_del_norte, linea_4).
sigue(canal_del_norte, morelos, linea_4).
sigue(morelos, candelaria, linea_4).
sigue(candelaria, fray_servando, linea_4).
sigue(fray_servando, jamaica, linea_4).
sigue(jamaica, santa_anita, linea_4).

%-----------------------------------------
% linea 5: Politécnico - Pantitlan
%-----------------------------------------
sigue(politécnico, instituto_del_petróleo, linea_5).
sigue(instituto_del_petróleo, autobuses_del_norte, linea_5).
sigue(autobuses_del_norte, la_raza, linea_5).
sigue(la_raza, misterios, linea_5).
sigue(misterios, valle_goméz, linea_5).
sigue(valle_goméz, consulado, linea_5).
sigue(consulado, eduardo_molina, linea_5).
sigue(eduardo_molina, aragón, linea_5).
sigue(aragón, oceanía, linea_5).
sigue(oceanía, terminal_aérea, linea_5).
sigue(terminal_aérea, hangares, linea_5).
sigue(hangares, pantitlán, linea_5).

%-----------------------------------------
% linea 6: El Rosario - Martin Carrera
%-----------------------------------------
sigue(el_rosario, tezozomóc, linea_6).
sigue(tezozomóc, uam_azcapotzalco, linea_6).
sigue(uam_azcapotzalco, ferrería, linea_6).
sigue(ferrería, norte_45, linea_6).
sigue(norte_45, vallejo, linea_6).
sigue(vallejo, instituto_del_petróleo, linea_6).
sigue(instituto_del_petróleo, lindavista, linea_6).
sigue(lindavista, deportivo_18_de_marzo, linea_6).
sigue(deportivo_18_de_marzo, la_villa_basílica, linea_6).
sigue(la_villa_basílica, martín_carrera, linea_6).

%-----------------------------------------
% linea 7: El Rosario - Barranca del Muerto
%-----------------------------------------
sigue(el_rosario, aquiles_serdán, linea_7).
sigue(aquiles_serdán, camarones, linea_7).
sigue(camarones, refinería, linea_7).
sigue(refinería, tacuba, linea_7).
sigue(tacuba, san_joaquín, linea_7).
sigue(san_joaquín, polanco, linea_7).
sigue(polanco, auditorio, linea_7).
sigue(auditorio, constituyentes, linea_7).
sigue(constituyentes, tacubaya, linea_7).
sigue(tacubaya, san_pedro_de_los_pinos, linea_7).
sigue(san_pedro_de_los_pinos, san_antonio, linea_7).
sigue(san_antonio, mixcoac, linea_7).
sigue(mixcoac, barranca_del_muerto, linea_7).

%-----------------------------------------
% linea 8: Garibaldi - Constitucion de 1917
%-----------------------------------------
sigue(garibaldi, bellas_artes, linea_8).
sigue(bellas_artes, san_juan_de_letrán, linea_8).
sigue(san_juan_de_letrán, salto_del_agua, linea_8).
sigue(salto_del_agua, doctores, linea_8).
sigue(doctores, obrera, linea_8).
sigue(obrera, chabacano, linea_8).
sigue(chabacano, la_viga, linea_8).
sigue(la_viga, santa_anita, linea_8).
sigue(santa_anita, coyuya, linea_8).
sigue(coyuya, iztacalco, linea_8).
sigue(iztacalco, apatlaco, linea_8).
sigue(apatlaco, aculco, linea_8).
sigue(aculco, escuadrón_201, linea_8).
sigue(escuadrón_201, atlalilco, linea_8).
sigue(atlalilco, iztapalapa, linea_8).
sigue(iztapalapa, cerro_de_la_estrella, linea_8).
sigue(cerro_de_la_estrella, uam_i, linea_8).
sigue(uam_i, constitución_de_1917, linea_8).

%-----------------------------------------
% linea 9: Tacubaya - Pantitlan
%-----------------------------------------
sigue(tacubaya, patriotismo, linea_9).
sigue(patriotismo, chilpancingo, linea_9).
sigue(chilpancingo, centro_médico, linea_9).
sigue(centro_médico, lázaro_cárdenas, linea_9).
sigue(chabacano, jamaica, linea_9).
sigue(jamaica, mixiuhca, linea_9).
sigue(mixiuhca, velódromo, linea_9).
sigue(velódromo, ciudad_deportiva, linea_9).
sigue(ciudad_deportiva, puebla, linea_9).
sigue(puebla, pantitlán, linea_9).

%-----------------------------------------
% linea A: Pantitlan - La Paz
%-----------------------------------------
sigue(pantitlán, agrícola_ambiental, linea_a).
sigue(agrícola_ambiental, canal_de_san_juan, linea_a).
sigue(canal_de_san_juan, tepalcates, linea_a).
sigue(tepalcates, guelatao, linea_a).
sigue(guelatao, peñón_viejo, linea_a).
sigue(peñón_viejo, acatitla, linea_a).
sigue(acatitla, santa_martha, linea_a).
sigue(santa_martha, los_reyes, linea_a).
sigue(los_reyes, la_paz, linea_a).

%-----------------------------------------
% linea B: Buenavista - Cd. Azteca
%-----------------------------------------
sigue(buenavista, guerrero, linea_b).
sigue(guerrero, garibaldi, linea_b).
sigue(garibaldi, lagunilla, linea_b).
sigue(lagunilla, tepito, linea_b).
sigue(tepito, morelos, linea_b).
sigue(morelos, san_lázaro, linea_b).
sigue(san_lázaro, ricardo_flores_magón, linea_b).
sigue(ricardo_flores_magón, romero_rubio, linea_b).
sigue(romero_rubio, oceanía, linea_b).
sigue(oceanía, deportivo_oceanía, linea_b).
sigue(deportivo_oceanía, bosque_aragón, linea_b).
sigue(bosque_aragón, villa_aragón, linea_b).
sigue(villa_aragón, nezahualcóyotl, linea_b).
sigue(nezahualcóyotl, impulsora, linea_b).
sigue(impulsora, rio_de_los_remedios, linea_b).
sigue(rio_de_los_remedios, múzquiz, linea_b).
sigue(múzquiz, ecatepec, linea_b).
sigue(ecatepec, olímpica, linea_b).
sigue(olímpica, plaza_aragón, linea_b).
sigue(plaza_aragón, ciudad_azteca, linea_b).

%-----------------------------------------
% linea 12: Mixcoac - Tlahuac
%-----------------------------------------
sigue(mixcoac, insurgentes_sur, linea_12).
sigue(insurgentes_sur, hospital_20_de_noviembre, linea_12).
sigue(hospital_20_de_noviembre, zapata, linea_12).
sigue(zapata, parque_de_los_venados, linea_12).
sigue(parque_de_los_venados, eje_central, linea_12).
sigue(eje_central, ermita, linea_12).
sigue(ermita, mexicaltzingo, linea_12).
sigue(mexicaltzingo, atlalilco, linea_12).
sigue(atlalilco, culhuacán, linea_12).
sigue(culhuacán, san_andrés_tomatlán, linea_12).
sigue(san_andrés_tomatlán, calle_11, linea_12).
sigue(calle_11, periférico_oriente, linea_12).
sigue(periférico_oriente, tezonco, linea_12).
sigue(tezonco, olivos, linea_12).
sigue(olivos, nopalera, linea_12).
sigue(nopalera, zapotitlán, linea_12).
sigue(zapotitlán, tlaltenco, linea_12).
sigue(tlaltenco, tláhuac, linea_12).

%------------------------------------------------------------------------------%
%                         CONOCIMIENTO OPERATIVO                               %
%------------------------------------------------------------------------------%

% Valida la adyacencia entre dos estaciones.
conecta(X,Y,L) :- sigue(X,Y,L); sigue(Y,X,L).

% Operación clave para determinar el trayecto entre dos estaciones.
alcanzable(A, Z, C) :- alcanzable_aux(A, [Z], C).

% Caso base (si el nodo origen es el mismo que el destino).
alcanzable_aux(A,[A|C1], [A|C1]).

% Operación recursiva, verifica adyacencia entre estaciones, y que no haya
% reincidencia de estaciones en el trayecto.
alcanzable_aux(A,[Y|C1], C) :-
  conecta(X,Y, _),
  not(member(X, [Y|C1])),
  alcanzable_aux(A,[X, Y|C1], C).