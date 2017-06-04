:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
%get_moves([[[1,5],[2,5]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).
%get_moves(ACTION, GS, B):- get_movable_allied_pieces(B, B, [ALLY|OTHER]), get_coord(ALLY, COORD), get_basic_move_actions_by_depth(B, COORD, 4, [ACTION|Q]).
get_moves(ACTION, GS, B):- get_movable_allied_pieces(B, B, ALLIES), get_all_actions(B, ALLIES, 4, ACTS), get_best_action(B, ACTS, _, ACTION).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS OUTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Debug
debug_log([]):-write('-END-').
debug_log([T|Q]):-writeln(T), debug_log(Q).
debug_log_slow([]):-write('-END-').
debug_log_slow([T|Q]):-writeln(T), sleep(0.2), debug_log(Q).

% list(E) --- vrai si E est une liste
list([]).
list([_|_]).

% list_size(List, Size) --- Determine le nombre d'element dans une liste
list_size([],0).
list_size([_|Q], SIZE):-list_size(Q, R), SIZE is R + 1. 

% concat(List1, List2, List12) --- Concat deux listes
concat([], L, L).
concat([T|Q], L, [T|R]):-concat(Q, L, R).

% append_element_to_all(Element, List, ResultList) --- Ajoute un element a tous les elements de la liste : append(1, [1,2]) --> [ [1,1] , [1,2] ]
append_element_to_all(_, [], []).
append_element_to_all(E, [T|Q], [[E|T]|R]):-list(T), append_element_to_all(E, Q, R), !.
append_element_to_all(E, [T|Q], [[E|[T]]|R]):-append_element_to_all(E, Q, R).

% add_sub_list(List, ResultList) --- imbrique chaque element dans une sous liste : [1,2,3] --> [ [1], [2], [3] ]
add_sub_list([],[]).
add_sub_list([T|Q],[[T]|R]):-add_sub_list(Q,R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICATS / FONCTIONS DE BASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_x --- Renvoie l'absisse a partir d'une piece ou de coordonnees
get_x([X,_], X).
get_x([X,_,_,_], X).

% get_y --- Renvoie l'ordonnee a partir d'une piece ou de coordonnees
get_y([_,Y], Y).
get_y([_,Y,_,_], Y).

% get_coord(Piece, Coord) --- Formate les coordonnees d'une case sous forme d'une liste [x,y]
get_coord([X,Y,_,_], [X,Y]).

% get_type(Piece, Type) --- Determine le type d'une pièce [0,0,rabbit,silver] -> R=rabbit
get_type([_,_,R,_], R).

% get_piece_side(Piece, Side) --- Determine le camp d'une piece  [0,0,rabbit,silver] -> R=silver
get_piece_side([_,_,_,R], R).

% get_piece_by_pos(Board, Coord, ResultPiece) --- Trouve la piece correspondant a la position Coord.
get_piece_by_pos([],_,_):-fail.
get_piece_by_pos([R|_],[X,Y],R):-get_coord(R, [X,Y]),!.
get_piece_by_pos([_|Q],[X,Y],R):-get_piece_by_pos(Q,[X,Y],R).

% get_pieces_by_pos(Board, Coords, ResultPiece) --- Retourne la liste des pièces correspondant aux coordonnées si elles existent.
get_pieces_by_pos(_,[],[]).
get_pieces_by_pos(B, [T|Q], [R|PIECES]):-get_piece_by_pos(B, T, R), get_pieces_by_pos(B, Q, PIECES), !.
get_pieces_by_pos(B, [_|Q], PIECES):-get_pieces_by_pos(B, Q, PIECES).

% empty(Board, Coord) --- Renvoie vrai si la case de coordonnees Coord est vide
empty(B, [X,Y]):- \+get_piece_by_pos(B, [X,Y], _).

% enemy(Piece) --- Renvoie vrai si la piece appartient a l'ennemi
enemy([_,_,_,gold]).

% enemy_around(Board, Coord) --- Renvoie vrai si un ennemi est voisin de la case Coord
enemy_around(B, COORD):-get_neigh(COORD, NEIGHLIST),
                        get_enemies(B, ENLIST),
                        get_pieces_by_pos(ENLIST, NEIGHLIST, R),
                        list_size(R, SIZE),
                        SIZE =\= 0.

% are_enemy(Piece1, Piece2) --- Renvoie vrai si les deux pièces sont dans des camps opposes
are_enemy(P1, P2):-get_piece_side(P1, S1), get_piece_side(P2, S2), dif(S1,S2). 

% ally(Piece) --- Renvoie vrai si la piece est alliee
ally([_,_,_,silver]).

% ally_around(Board, Coord) --- Renvoie vrai si un allie est voisin de la case Coord
ally_around(B, COORD):- get_neigh(COORD, NEIGHLIST),
                        get_allies(B, ALLIST),
                        get_pieces_by_pos(ALLIST, NEIGHLIST, R),
                        list_size(R, SIZE),
                        SIZE =\= 0.

% trap(Coord) --- Vrai si la case est une trape
trap([2,2]).
trap([2,5]).
trap([5,2]).
trap([5,5]).

% dead_piece(Board, Piece) --- Vrai si la piece est une piece qui va tomber (Au dessus d'une trappe sans voisin)
dead_piece(B, [X,Y,_,SIDE]):- trap([X,Y]), enemy([X,Y,_,SIDE]), \+enemy_around(B, [X,Y]).
dead_piece(B, [X,Y,_,SIDE]):- trap([X,Y]), ally([X,Y,_,SIDE]), \+ally_around(B, [X,Y]).

% force(Piece, Result) --- Donne la force d'une piece suivant son type
force([_,_,rabbit,_], 0).
force([_,_,cat,_], 1).
force([_,_,dog,_], 2).
force([_,_,horse,_], 3).
force([_,_,camel,_], 4).
force([_,_,elephant,_], 5).

stronger_than(P1, P2):-force(P1, F1), force(P2, F2), F1 > F2.

% stronger_or_eq_than(Piece1, Piece2) --- Renvoie vrai si Piece1 est plus ou aussi forte que Piece2
stronger_or_eq_than(P1, P2):-force(P1, F1), force(P2, F2), F1 >= F2.

% stronger_or_eq_than_all(Piece, Other) --- Renvoie vrai si la piece est plus ou aussi forte que toutes les autres
stronger_or_eq_than_all(_, []).
stronger_or_eq_than_all(P, [T|Q]):-stronger_or_eq_than(P, T), stronger_or_eq_than_all(P, Q).

% get_enemies(Board, ResultList) --- Renvoie la liste des pieces enemies sur le plateau
get_enemies([], []).
get_enemies([T|Q], [T|OTHERS]):-enemy(T), get_enemies(Q, OTHERS), !.
get_enemies([_|Q], OTHERS):-get_enemies(Q, OTHERS).

% get_allies(Board, ResultList) --- Renvoie la liste des pieces alliees sur le plateau
get_allies([], []).
get_allies([T|Q], [T|OTHERS]):-ally(T), get_allies(Q, OTHERS), !.
get_allies([_|Q], OTHERS):-get_allies(Q, OTHERS).

% get_enemy_by_type(Board, Type, ResultList) --- Renvoie la liste des pieces enemies de type TYPE
get_enemy_by_type([],_,[]).
get_enemy_by_type([T|Q], TYPE, [T|R]):-enemy(T), get_type(T, TYPE), get_enemy_by_type(Q, TYPE, R),!.
get_enemy_by_type([_|Q], TYPE, R):-get_enemy_by_type(Q, TYPE, R).

% get_allies_by_type(Board, Type, ResultList) --- Renvoie la liste des pieces alliees de type TYPE
get_allies_by_type([],_,[]).
get_allies_by_type([T|Q], TYPE, [T|R]):-ally(T), get_type(T, TYPE), get_allies_by_type(Q, TYPE, R),!.
get_allies_by_type([_|Q], TYPE, R):-get_allies_by_type(Q, TYPE, R).

% get_neigh(Coord, NeighboorsList) --- Renvoie la liste des coordonnees des cases voisines : [O, N, E, S] => [1, 2, 3, 4]
% ------Cas particuliers : coins
get_neigh([0,0], [[0,1],[1,0]]):-!.
get_neigh([0,7], [[0,6],[1,7]]):-!.
get_neigh([7,0], [[6,0],[7,1]]):-!.
get_neigh([7,7], [[7,6],[6,7]]):-!.
% ------Cas particuliers : bordures
get_neigh([X,0], [[X2,0],[X,1],[X4,0]]):- !, X2 is X-1, X4 is X+1.
get_neigh([0,Y], [[0,Y1],[0,Y3],[1,Y]]):- !, Y1 is Y-1, Y3 is Y+1.
get_neigh([X,7], [[X,6],[X2,7],[X4,7]]):- !, X2 is X-1, X4 is X+1.
get_neigh([7,Y], [[7,Y1],[6,Y],[7,Y3]]):- !, Y1 is Y-1, Y3 is Y+1.
% ------Cas general
get_neigh([X,Y], [[X,Y1],[X2,Y],[X,Y3],[X4,Y]]):- Y1 is Y-1, X2 is X-1, Y3 is Y+1, X4 is X+1.

% get_empty_pos(Board, ListCoord, EmptyCoord) --- Renvoie la liste des coordonnes des case vides parmis celle de la liste ListCoord
get_empty_pos(_, [], []).
get_empty_pos(B, [T|Q], [T|R]):-empty(B, T), get_empty_pos(B, Q, R),!.
get_empty_pos(B, [_|Q], R):-get_empty_pos(B, Q, R).

% get_empty_neigh(Board, Coord, ResultList) --- Renvoie la liste des voisins vide pour une case de coordonnees Coord
get_empty_neigh(B, [X,Y], R):-get_neigh([X,Y], N), get_empty_pos(B, N, R).

% get_movable_allied_pieces(Board, Pieces, ResultList) --- Renvoie la liste des pieces alliees qui peuvent bouger.
get_movable_allied_pieces(_, [], []).
get_movable_allied_pieces(B, [T|Q], [T|R]) :- ally(T),
                                              movable_piece(B,T),
                                              get_movable_allied_pieces(B, Q, R), !.
get_movable_allied_pieces(B, [_|Q], R) :- get_movable_allied_pieces(B, Q, R), !.

% movable_piece(Board, Piece) --- Indique si une piece est deplacable
% ------Cas entouré de pieces
movable_piece(B, P):- get_coord(P, COORD),
                        get_empty_neigh(B, COORD, LISTNEIGH),
                        list_size(LISTNEIGH, SIZE),
                        SIZE =:= 0, !, fail.
movable_piece(B, P):- get_type(P,rabbit),      %Le lapin ne peut qu'avancer (silver)
                      get_piece_side(P, silver),
                      get_coord(P, [X,_]),
                      get_empty_neigh(B, _, [[NX,_]]),
                      NX is X - 1,
                      !, fail.
movable_piece(B, P):- get_type(P, rabbit),      %Le lapin ne peut qu'avancer (gold)
                      get_piece_side(P, gold),
                      get_coord(P, [X,_]),
                      get_empty_neigh(B, _, [[NX,_]]),
                      NX is X + 1,
                      !, fail.
% ------Cas ou il y a un allie autour
movable_piece(B, P):- enemy(P),
                        get_coord(P, COORD),
                        enemy_around(B, COORD), !.
movable_piece(B, P):- ally(P),
                        get_coord(P, COORD),
                        ally_around(B, COORD), !.
% ------Cas ou on est plus fort que tous les enemies
movable_piece(B, P):- get_coord(P, COORD),
                        get_neigh(COORD, LISTNEIGH),
                        get_pieces_by_pos(B, LISTNEIGH, NEIGHPIECES),
                        stronger_or_eq_than_all(P, NEIGHPIECES), !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS DE L'IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Action pattern : [SCORE, DEPTH, [ [ACTION1] [ACTION2] ]]

% get_all_actions(Board, Pieces, Depth, Actions) --- Renvoie la liste de toute les actions possibles (Depth = nombre de deplacement max par action)
get_all_actions(B, [P|[]], DEPTH, ACTIONS):-DEPTH >= 2,
                                          !,
                                          get_coord(P, COORD),
                                          get_basic_move_actions_by_depth(B, COORD, DEPTH, SUBACT1),
                                          get_pull_actions(B, P, SUBACT2),
                                          concat(SUBACT2, SUBACT1, SUBACT3),
                                          get_push_actions(B, P, SUBACT4),
                                          concat(SUBACT4, SUBACT3, ACTIONS).
get_all_actions(B, [P|[]], DEPTH, ACTIONS):- !,
                                          get_coord(P, COORD),
                                          get_basic_move_actions_by_depth(B, COORD, DEPTH, ACTIONS).
get_all_actions(B, [P|OTHERS], DEPTH, ACTIONS):-DEPTH >=2,
                                          !,
                                          get_all_actions(B, OTHERS, DEPTH, SUBACT1),
                                          get_coord(P, COORD),
                                          get_basic_move_actions_by_depth(B, COORD, DEPTH, SUBACT2),
                                          concat(SUBACT2, SUBACT1, SUBACT3),
                                          get_pull_actions(B, P, SUBACT4),
                                          concat(SUBACT4, SUBACT3, TEMPACT),
                                          get_push_actions(B, P, SUBACT5),
                                          concat(SUBACT5, TEMPACT, ACTIONS).
get_all_actions(B, [P|OTHERS], DEPTH, ACTIONS):- get_all_actions(B, OTHERS, DEPTH, SUBACT1),
                                          get_coord(P, COORD),
                                          get_basic_move_actions_by_depth(B, COORD, DEPTH, SUBACT2),
                                          concat(SUBACT2, SUBACT1, ACTIONS).

% delete_disallowed_neigh_for_rabbit(Coord, Neigh, ResultList) --- Supprime le voisin interdit pour un lapin
delete_disallowed_neigh_for_rabbit(_,[],[]).
delete_disallowed_neigh_for_rabbit([X,Y,rabbit,silver], [[NX,Y]|Q], Q):- NX is X - 1, !.
delete_disallowed_neigh_for_rabbit([X,Y,rabbit,gold], [[NX,Y]|Q], Q):- NX is X + 1, !.
delete_disallowed_neigh_for_rabbit(P, [T|Q], [T|R]):- delete_disallowed_neigh_for_rabbit(P,Q,R).

% get_basic_move_actions_by_depth(Board, StartCoord, Depth, Result) --- Recherche tous les deplacement possible jusqu'a une profondeur donnee : profondeur=1 --> deplacements de 1 case
get_basic_move_actions_by_depth(_,_,0,[]):-!.
get_basic_move_actions_by_depth(B, COORD, DEPTH, R):- get_basic_move_actions(B, COORD, N, ACTIONS),
                                                      D is DEPTH-1,
                                                      basic_move_foreach_neigh(B, COORD, D, N, NACT),
                                                      concat(ACTIONS,NACT,R).

% get_basic_move_actions(Board, StartCoord, Voisins, Result) --- Renvoie les voisins accessibles et les deplacements possibles de 1 case max
get_basic_move_actions(B, COORD, NEIGH, R):-get_empty_neigh(B, COORD, UNSURENEIGH),
                                            get_piece_by_pos(B, COORD, P),
                                            delete_disallowed_neigh_for_rabbit(P, UNSURENEIGH, NEIGH),
                                            !,
                                            add_sub_list(NEIGH, SN),
                                            append_element_to_all(COORD, SN, ACTIONS),
                                            add_sub_list(ACTIONS, R).
get_basic_move_actions(B, COORD, NEIGH, R):-get_empty_neigh(B, COORD, NEIGH),
                                            add_sub_list(NEIGH, SN),
                                            append_element_to_all(COORD, SN, ACTIONS),
                                            add_sub_list(ACTIONS, R).

% basic_move_foreach_neigh(Board, StartCoord, Depth, NeighList, Result) --- Utilisee par get_basic_move_actions_by_depth, renvoie tous les deplacements possible a partir des voisins.
basic_move_foreach_neigh(_,_,_,[],[]).
basic_move_foreach_neigh(B, COORD, D, [T|Q], R):- move_piece(B, COORD, T, NEWB),
                                                  get_piece_by_pos(NEWB, T, PIECE),
                                                  movable_piece(NEWB, PIECE),
                                                  \+dead_piece(NEWB, PIECE),
                                                  get_basic_move_actions_by_depth(NEWB, T, D, ACTIONS),
                                                  append_element_to_all([COORD,T], ACTIONS, NACT),
                                                  basic_move_foreach_neigh(B, COORD, D, Q, OTHERS),
                                                  concat(NACT, OTHERS, R), !.
basic_move_foreach_neigh(B, COORD, D, [_|Q], R):-basic_move_foreach_neigh(B, COORD, D, Q, R).

% get_pull_actions(Board, Piece, ListResult) --- Recherche toutes les actions "tirees" possibles pour la piece passee en parametre
get_pull_actions(B, P, ACTIONS):-bagof(R, pull_action(B, P, R), ACTIONS), !.
get_pull_actions(_, _, []).

% pull_action(Board, Piece, Result) --- Cherche une action "tiree"
pull_action(_, [_,_,rabbit,_], _):-fail.
pull_action(B, P, [pull, [COORD,EN],[NCOORD, COORD]]):-movable_piece(B, P),
                                                      get_coord(P, COORD),
                                                      get_empty_neigh(B, COORD, EMPTYNEIGH),
                                                      get_neigh(COORD, NEIGH),
                                                      get_pieces_by_pos(B, NEIGH, PNEIGH),
                                                      member(EN, EMPTYNEIGH),
                                                      member(N, PNEIGH),
                                                      enemy(N),
                                                      stronger_than(P, N),
                                                      get_coord(N, NCOORD).

% get_push_actions(Board, Piece, ListResult) --- Recherche toutes les actions "pousser" possibles pour la piece passee en parametre
get_push_actions(B, P, ACTIONS):-bagof(R, push_action(B, P, R), ACTIONS), !.
get_push_actions(_, _, []).

% push_action(_, [_,_,rabbit,_], [])
push_action(_, [_,_,rabbit,_], _):-fail.
push_action(B, P, [push, [NCOORD,EN],[COORD, NCOORD]]):-movable_piece(B, P),
                                                      get_coord(P, COORD),
                                                      get_neigh(COORD, NEIGH),
                                                      get_pieces_by_pos(B, NEIGH, PNEIGH),
                                                      member(N, PNEIGH),
                                                      enemy(N),
                                                      stronger_than(P, N),
                                                      get_coord(N, NCOORD),
                                                      get_empty_neigh(B, NCOORD, EMPTYNEIGH),
                                                      member(EN, EMPTYNEIGH).

% move_piece(Board, InitPos, NewPos, NewBoard) --- Deplace la piece a la position InitPos a la position NewPos
move_piece([[X,Y,T,S]|Q], [X,Y], [NX,NY], [[NX,NY,T,S]|Q]):-!.
move_piece([T|Q], ICOORD, NCOORD, [T|R]):-move_piece(Q, ICOORD, NCOORD, R).

% get_compact_move(Action, CaseDepart, CaseArrivee) --- Renvoie la case depart et la case d'arrivee pour une action donnee
get_compact_move([[DEBUT,FIN]|[]], DEBUT, FIN):- !.
get_compact_move([[DEBUT,_]|OTHERS], DEBUT, FIN):- get_compact_move(OTHERS, _, FIN).

% apply_action_on_board(Board, Action, NewBoard) --- Renvoie l'etat du plateau apres application de l'action
apply_action_on_board(B, [pull, [DEBUT1, FIN1], [DEBUT2, FIN2]], NEWB):-!,
                                                                        move_piece(B, DEBUT1, FIN1, TEMPB),
                                                                        move_piece(TEMPB, DEBUT2, FIN2, NEWB).
apply_action_on_board(B, [push, [DEBUT1, FIN1], [DEBUT2, FIN2]], NEWB):-!,
                                                                        move_piece(B, DEBUT1, FIN1, TEMPB),
                                                                        move_piece(TEMPB, DEBUT2, FIN2, NEWB).
apply_action_on_board(B, ACTION, NEWB) :- get_compact_move(ACTION, DEBUT, FIN),
                                          move_piece(B, DEBUT, FIN, NEWB).

% get_best_action(Board, Actions, BestScore, BestAction) --- Renvoie la meilleure action a jouer parmis toute la liste
get_best_action(B, [A|[]], SCORE, A) :- !, apply_action_on_board(B, A, NEWB), board_score(NEWB, NEWB, SCORE).
get_best_action(B, [A|OTHERS], BSCORE, BACTION) :- get_best_action(B, OTHERS, TEMPSCORE, TEMPACTION),
                                                      apply_action_on_board(B, A, NEWB),
                                                      board_score(NEWB, NEWB, ASCORE),
                                                      compare_actions_score(TEMPSCORE, TEMPACTION, ASCORE, A, BSCORE, BACTION).

% compare_actions_score(Score1, Action1, Score2, Action2, BestScore, BestAction) --- Renvoie la meilleure action entre les deux (Si scores egaux => celle qui a le moins de mouvements)
compare_actions_score(S1, A1, S2, _, S1, A1) :- S1 > S2, !.
compare_actions_score(S1, _, S2, A2, S2, A2) :- S1 < S2, !.
compare_actions_score(S1, A1, _, A2, S1, A1) :- list_size(A1, SIZE1), list_size(A2, SIZE2), SIZE1 < SIZE2, !.
compare_actions_score(_, _, S2, A2, S2, A2).



%--->SCORE %%%%%%%% HEURISTIQUES POUR DECISION DE L'IA   ---http://www.techno-science.net/?onglet=glossaire&definition=6418---
% Poids associes : 
%  -- Gagnant : INFINI
%  -- Piece sur le plateau : 20
%  -- Piece en danger (peu etre push) : -10
%  -- Piece freeze : -1
%  -- Distance Lapin : -DISTANCE (si D > 3)

%  -- Enable to push / pull to delete
%  -- Gagnant next time

% board_score(Board, Pieces, Score) --- Calcul un score correspondant a l'etat du plateau (Score eleve = tres favorable, faible = plutot defavorable)
board_score(_, [], 0).
board_score(B, [P|OTHERS], SCORE):-calcul_score_win(P, SWIN),
                        calcul_score_freeze(B, P, SFREEZE),
                        calcul_score_piece_alive(B, P, SALIVE),
                        calcul_score_distance_rabbit(P, SRABBIT),
                        board_score(B, OTHERS, SOTHERS),
                        SCORE is SWIN + SFREEZE + SALIVE + SRABBIT + SOTHERS.


% calcul_score_win(Piece, Score) --- Renvoie un certain score suivant si la piece permet de gagner ou non
calcul_score_win([0,_,rabbit, gold], -2000000):-enemy([0,0,rabbit,gold]),!.
calcul_score_win([0,_,rabbit, gold], 2000000):- !.
calcul_score_win([7,_,rabbit, silver], -2000000):-enemy([7,0,rabbit,silver]),!.
calcul_score_win([7,_,rabbit, silver], 2000000):- !.
calcul_score_win(_, 0).

% calcul_score_freeze(Board, Piece, Score) --- Renvoie un certain score suivant si la pièce est freeze ou pas 
calcul_score_freeze(_, P, 0):- enemy(P), !.
calcul_score_freeze(B, P, 0):- movable_piece(B, P), !.
calcul_score_freeze(_, _, -1).

% calcul_score_piece_alive(Board, Piece, Score) --- Renvoie un certain score suivant si la piece tombe dans une trappe ou non
calcul_score_piece_alive(B, P, 0):- \+dead_piece(B, P), !.
calcul_score_piece_alive(_, P, 20):- enemy(P), !.
calcul_score_piece_alive(_, _, -20).

% calcul_score_distance_rabbit(Piece, Score) --- Renvoie un certain score prenant en compte la distance de la piece avec la ligne final (si lapin)
calcul_score_distance_rabbit([_,_,TYPE,_], 0):- dif(TYPE, rabbit), !.
calcul_score_distance_rabbit(P, 0):- enemy(P), !.
calcul_score_distance_rabbit([X,_,_,gold], 0):- X =< 3, !.
calcul_score_distance_rabbit([X,_,_,silver], 0):- X >= 4, !.
calcul_score_distance_rabbit([X,_,_,gold], -X):- !.
calcul_score_distance_rabbit([X,_,_,silver], -SCORE):- SCORE is 7 - X.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS 

looping(B,ALLIES):-get_all_actions(B,ALLIES,4,R), debug_log_slow(R).

/*
bot:get_basic_move_actions_by_depth([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]],[1,0],4,R), bot:debug_log(R).

%LISTER TOUTE LES ACTIONS
bot:get_movable_allied_pieces([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]],[[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]],R), bot:looping([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]],R).

bot:get_movable_allied_pieces([[0,0,cat,silver],[0,1,elephant,gold],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,horse,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,elephant,silver],[1,1,dog,gold],[1,2,camel,silver],[1,3,rabbit,silver],[1,4,rabbit,silver],[1,5,horse,gold],[1,6,dog,silver],[1,7,dog,silver],[2,0,rabbit,silver],[2,1,cat,gold],[2,2,rabbit,silver],[2,3,rabbit,gold],[2,4,rabbit,gold],[2,6,rabbit,gold],[2,7,camel,gold],[3,0,rabbit,silver],[4,3,rabbit,gold],[4,7,horse,gold],[5,4,rabbit,gold],[6,0,rabbit,gold],[7,0,cat,gold],[7,3,dog,gold],[7,4,rabbit,gold],[7,5,rabbit,gold]],[[0,0,cat,silver],[0,1,elephant,gold],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,horse,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,elephant,silver],[1,1,dog,gold],[1,2,camel,silver],[1,3,rabbit,silver],[1,4,rabbit,silver],[1,5,horse,gold],[1,6,dog,silver],[1,7,dog,silver],[2,0,rabbit,silver],[2,1,cat,gold],[2,2,rabbit,silver],[2,3,rabbit,gold],[2,4,rabbit,gold],[2,6,rabbit,gold],[2,7,camel,gold],[3,0,rabbit,silver],[4,3,rabbit,gold],[4,7,horse,gold],[5,4,rabbit,gold],[6,0,rabbit,gold],[7,0,cat,gold],[7,3,dog,gold],[7,4,rabbit,gold],[7,5,rabbit,gold]],R), bot:looping([[0,0,cat,silver],[0,1,elephant,gold],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,horse,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,elephant,silver],[1,1,dog,gold],[1,2,camel,silver],[1,3,rabbit,silver],[1,4,rabbit,silver],[1,5,horse,gold],[1,6,dog,silver],[1,7,dog,silver],[2,0,rabbit,silver],[2,1,cat,gold],[2,2,rabbit,silver],[2,3,rabbit,gold],[2,4,rabbit,gold],[2,6,rabbit,gold],[2,7,camel,gold],[3,0,rabbit,silver],[4,3,rabbit,gold],[4,7,horse,gold],[5,4,rabbit,gold],[6,0,rabbit,gold],[7,0,cat,gold],[7,3,dog,gold],[7,4,rabbit,gold],[7,5,rabbit,gold]],R).
loopin

*/


/*******
*  IA  *
*******/

% Si mouvement gagnant -> jouer.
% Si tuer enemie -> tuer.
% Poser elephant a cote d'un piege
% Reflexion par action (calcul enemie impossible)
      % Generer action [nbAction, [MOVE 1], [MOVE 2]]
      % Calculer score [score [nbAction, [MOVE 1], [MOVE 2]]]
      % Jouer Meilleur ORDER BY SCORE
      % Boucler si reste des coup
% get_action_value() // Calcul score -> GhostInTheCell
