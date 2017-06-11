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
%get_moves(ACTION, GS, B):- get_movable_allied_pieces(B, B, [ALLY|OTHER]), get_coord(ALLY, Coord), get_basic_move_actions_by_depth(B, Coord, 4, [ACTION|Q]).
%get_moves(ACTION, GS, Board):- get_movable_allied_pieces(Board, Board, ALLIES), get_all_actions(Board, ALLIES, 4, ACTS), get_best_action(Board, ACTS, _, ACTION).
get_moves(Moves, GameState, Board) :- get_movable_allied_pieces(Board, Board, Allies),
                                        get_all_actions(Board, Allies, 4, Actions),
                                        get_best_action(Board, Actions, _, BestAction),
                                        build_moves(Board, BestAction, 0, Moves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS OUTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Debug
debug_log([]):-write('-END-').
debug_log([T|Q]):-writeln(T), debug_log(Q).
debug_log_slow([]):-write('-END-').
debug_log_slow([T|Q]):-writeln(T), sleep(0.2), debug_log_slow(Q).

% list(E) --- vrai si E est une liste
list([]).
list([_|_]).

% list_size(List, Size) --- Determine le nombre d'element dans une liste
list_size([],0).
list_size([_|Q], Size):-list_size(Q, Result), Size is Result + 1. 

% concat(List1, List2, List12) --- Concat deux listes
concat([], L, L).
concat([T|Q], L, [T|Result]):-concat(Q, L, Result).

% append_element_to_all(Element, List, ResultList) --- Ajoute un element a tous les elements de la liste : append(1, [1,2]) --> [ [1,1] , [1,2] ]
append_element_to_all(_, [], []).
append_element_to_all(Element, [T|Q], [[Element|T]|Result]):-list(T), append_element_to_all(Element, Q, Result), !.
append_element_to_all(Element, [T|Q], [[Element|[T]]|Result]):-append_element_to_all(Element, Q, Result).

% add_sub_list(List, ResultList) --- imbrique chaque element dans une sous liste : [1,2,3] --> [ [1], [2], [3] ]
add_sub_list([],[]).
add_sub_list([T|Q],[[T]|Result]):-add_sub_list(Q,Result).



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
get_type([_,_,Type,_], Type).

% get_piece_side(Piece, Side) --- Determine le camp d'une piece  [0,0,rabbit,silver] -> R=silver
get_piece_side([_,_,_,Side], Side).

% get_piece_by_pos(Board, Coord, ResultPiece) --- Trouve la piece correspondant a la position Coord.
get_piece_by_pos([],_,_):-fail.
get_piece_by_pos([Piece|_], [X,Y], Piece):-get_coord(Piece, [X,Y]),!.
get_piece_by_pos([_|Q], [X,Y], Piece):-get_piece_by_pos(Q, [X,Y], Piece).

% get_pieces_by_pos(Board, Coords, ResultPiece) --- Retourne la liste des pièces correspondant aux coordonnées si elles existent.
get_pieces_by_pos(_,[],[]).
get_pieces_by_pos(Board, [T|Q], [Result|Pieces]):-get_piece_by_pos(Board, T, Result), get_pieces_by_pos(Board, Q, Pieces), !.
get_pieces_by_pos(Board, [_|Q], Pieces):-get_pieces_by_pos(Board, Q, Pieces).

% empty(Board, Coord) --- Renvoie vrai si la case de coordonnees Coord est vide
empty(Board, [X,Y]):- \+get_piece_by_pos(Board, [X,Y], _).

% enemy(Piece) --- Renvoie vrai si la piece appartient a l'ennemi
enemy([_,_,_,gold]).

% enemy_around(Board, Coord) --- Renvoie vrai si un ennemi est voisin de la case Coord
enemy_around(Board, Coord) :- get_neigh(Coord, NeighList),
                              get_enemies(Board, EnemyList),
                              get_pieces_by_pos(EnemyList, NeighList, EnPieces),
                              list_size(EnPieces, Size),
                              Size =\= 0.

% are_enemy(Piece1, Piece2) --- Renvoie vrai si les deux pièces sont dans des camps opposes
are_enemy(Piece1, Piece2):-get_piece_side(Piece1, Side1), get_piece_side(Piece2, Side2), dif(Side1,Side2). 

% ally(Piece) --- Renvoie vrai si la piece est alliee
ally([_,_,_,silver]).

% ally_around(Board, Coord) --- Renvoie vrai si un allie est voisin de la case Coord
ally_around(Board, Coord) :- get_neigh(Coord, NeighList),
                              get_allies(Board, AlliesList),
                              get_pieces_by_pos(AlliesList, NeighList, AllPieces),
                              list_size(AllPieces, Size),
                              Size =\= 0.

% trap(Coord) --- Vrai si la case est une trape
trap([2,2]).
trap([2,5]).
trap([5,2]).
trap([5,5]).

% dead_piece(Board, Piece) --- Vrai si la piece est une piece qui va tomber (Au dessus d'une trappe sans voisin)
dead_piece(Board, [X,Y,_,Side]):- trap([X,Y]), enemy([X,Y,_,Side]), \+enemy_around(Board, [X,Y]).
dead_piece(Board, [X,Y,_,Side]):- trap([X,Y]), ally([X,Y,_,Side]), \+ally_around(Board, [X,Y]).

% force(Piece, Result) --- Donne la force d'une piece suivant son type
force([_,_,rabbit,_], 0).
force([_,_,cat,_], 1).
force([_,_,dog,_], 2).
force([_,_,horse,_], 3).
force([_,_,camel,_], 4).
force([_,_,elephant,_], 5).

stronger_than(Piece1, Piece2):-force(Piece1, Force1), force(Piece2, Force2), Force1 > Force2.

% stronger_or_eq_than(Piece1, Piece2) --- Renvoie vrai si Piece1 est plus ou aussi forte que Piece2
stronger_or_eq_than(Piece1, Piece2):-force(Piece1, Force1), force(Piece2, Force2), Force1 >= Force2.

% stronger_or_eq_than_all(Piece, Other) --- Renvoie vrai si la piece est plus ou aussi forte que toutes les autres
stronger_or_eq_than_all(_, []).
stronger_or_eq_than_all(Piece, [T|Q]):-stronger_or_eq_than(Piece, T), stronger_or_eq_than_all(Piece, Q).

% get_enemies(Board, ResultList) --- Renvoie la liste des pieces enemies sur le plateau
get_enemies([], []).
get_enemies([T|Q], [T|Others]):-enemy(T), get_enemies(Q, Others), !.
get_enemies([_|Q], Others):-get_enemies(Q, Others).

% get_allies(Board, ResultList) --- Renvoie la liste des pieces alliees sur le plateau
get_allies([], []).
get_allies([T|Q], [T|Others]):-ally(T), get_allies(Q, Others), !.
get_allies([_|Q], Others):-get_allies(Q, Others).

% get_enemy_by_type(Board, Type, ResultList) --- Renvoie la liste des pieces enemies de type TYPE
get_enemy_by_type([],_,[]).
get_enemy_by_type([T|Q], Type, [T|Result]):-enemy(T), get_type(T, Type), get_enemy_by_type(Q, Type, Result), !.
get_enemy_by_type([_|Q], Type, Result):-get_enemy_by_type(Q, Type, Result).

% get_allies_by_type(Board, Type, ResultList) --- Renvoie la liste des pieces alliees de type TYPE
get_allies_by_type([],_,[]).
get_allies_by_type([T|Q], Type, [T|Result]):-ally(T), get_type(T, Type), get_allies_by_type(Q, Type, Result),!.
get_allies_by_type([_|Q], Type, Result):-get_allies_by_type(Q, Type, Result).

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
get_empty_pos(Board, [T|Q], [T|EmpCoord]):-empty(Board, T), get_empty_pos(Board, Q, EmpCoord),!.
get_empty_pos(Board, [_|Q], EmpCoord):-get_empty_pos(Board, Q, EmpCoord).

% get_empty_neigh(Board, Coord, ResultList) --- Renvoie la liste des voisins vide pour une case de coordonnees Coord
get_empty_neigh(Board, [X,Y], EmpNeigh):-get_neigh([X,Y], Neigh), get_empty_pos(Board, Neigh, EmpNeigh).

% get_movable_allied_pieces(Board, Pieces, ResultList) --- Renvoie la liste des pieces alliees qui peuvent bouger.
get_movable_allied_pieces(_, [], []).
get_movable_allied_pieces(Board, [T|Q], [T|Result]):- ally(T),
                                                      movable_piece(Board,T),
                                                      get_movable_allied_pieces(Board, Q, Result), !.
get_movable_allied_pieces(Board, [_|Q], Result) :- get_movable_allied_pieces(Board, Q, Result), !.

% movable_piece(Board, Piece) --- Indique si une piece est deplacable
% ------La piece peut pousser
movable_piece(Board, Piece):- push_action(Board, Piece, _), !.
% ------Cas entouré de pieces
movable_piece(Board, Piece):- get_coord(Piece, Coord),
                              get_empty_neigh(Board, Coord, ListNeigh),
                              list_size(ListNeigh, Size),
                              Size =:= 0, !, fail.
% ------Deplacement Lapin (pas en arriere)
movable_piece(Board, [X,Y,rabbit,silver]):-get_empty_neigh(Board, [X,Y], EmptyNeigh),
                                          list_size(EmptyNeigh, 1),
                                          BadX is X - 1,
                                          member([BadX,Y], EmptyNeigh),
                                          !, fail.
movable_piece(Board, [X,Y,rabbit,gold]):- get_empty_neigh(Board, [X,Y], EmptyNeigh),
                                          list_size(EmptyNeigh, 1),
                                          BadX is X + 1,
                                          member([BadX,Y], EmptyNeigh),
                                          !, fail.
% ------Cas ou il y a un allie autour
movable_piece(Board, Piece):- enemy(Piece),
                              get_coord(Piece, Coord),
                              enemy_around(Board, Coord), !.
movable_piece(Board, Piece):- ally(Piece),
                              get_coord(Piece, Coord),
                              ally_around(Board, Coord), !.
% ------Cas ou on est plus fort que tous les enemies
movable_piece(Board, Piece):- get_coord(Piece, Coord),
                              get_neigh(Coord, ListNeigh),
                              get_pieces_by_pos(Board, ListNeigh, NeighPieces),
                              stronger_or_eq_than_all(Piece, NeighPieces), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS DE L'IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% build_moves(Board, Action, CoupRestant, ListActionsResultat) --- Construit la liste des actions de l'IA
build_moves(Board, [Type, Init, End], Steps, BuiltActions):- \+list(Type),
                                                            apply_action_on_board(Board, [Type, Init, End], NewBoard),
                                                            get_movable_allied_pieces(NewBoard, NewBoard, Allies),
                                                            Depth is 2 - Steps,
                                                            Depth > 0,
                                                            get_all_actions(NewBoard, Allies, Depth, Actions),
                                                            get_best_action(NewBoard, Actions, NewScore, BestAction),
                                                            board_score(NewBoard, NewBoard, LastScore),
                                                            NewScore >= LastScore,
                                                            !,
                                                            NextSteps is 2 + Steps,
                                                            build_moves(NewBoard, BestAction, NextSteps, NewAction),
                                                            concat([Init, End], NewAction, BuiltActions).
build_moves(_, [Type, Init, End], _, [Init, End]):- \+list(Type), !.
build_moves(Board, [FirstPart|NextPart], Steps, BuiltActions):-list(FirstPart),
                                                            apply_action_on_board(Board, [FirstPart|NextPart], NewBoard),
                                                            get_movable_allied_pieces(NewBoard, NewBoard, Allies),
                                                            list_size([FirstPart|NextPart], ActionSteps),
                                                            Depth is 4 - Steps - ActionSteps,
                                                            Depth > 0,
                                                            get_all_actions(NewBoard, Allies, Depth, Actions),
                                                            get_best_action(NewBoard, Actions, NewScore, BestAction),
                                                            board_score(NewBoard, NewBoard, LastScore),
                                                            NewScore >= LastScore,
                                                            !,
                                                            NextSteps is ActionSteps + Steps,
                                                            build_moves(NewBoard, BestAction, NextSteps, NewAction),
                                                            concat([FirstPart|NextPart], NewAction, BuiltActions).
build_moves(_, Action, _, Action).



% get_all_actions(Board, Pieces, Depth, Actions) --- Renvoie la liste de toute les actions possibles (Depth = nombre de deplacement max par action)
get_all_actions(Board, [Piece|[]], Depth, Actions) :- Depth >= 2,
                                                      !,
                                                      get_coord(Piece, Coord),
                                                      get_basic_move_actions_by_depth(Board, Coord, Depth, SubAct1),
                                                      get_pull_actions(Board, Piece, SubAct2),
                                                      concat(SubAct2, SubAct1, SubAct3),
                                                      get_push_actions(Board, Piece, SubAct4),
                                                      concat(SubAct4, SubAct3, Actions).
get_all_actions(Board, [Piece|[]], Depth, Actions) :- !,
                                                      get_coord(Piece, Coord),
                                                      get_basic_move_actions_by_depth(Board, Coord, Depth, Actions).
get_all_actions(Board, [Piece|Others], Depth, Actions):-Depth >=2,
                                                      !,
                                                      get_all_actions(Board, Others, Depth, SubAct1),
                                                      get_coord(Piece, Coord),
                                                      get_basic_move_actions_by_depth(Board, Coord, Depth, SubAct2),
                                                      concat(SubAct2, SubAct1, SubAct3),
                                                      get_pull_actions(Board, Piece, SubAct4),
                                                      concat(SubAct4, SubAct3, TEMPACT),
                                                      get_push_actions(Board, Piece, SubAct5),
                                                      concat(SubAct5, TEMPACT, Actions).
get_all_actions(Board, [Piece|Others], Depth, Actions):- get_all_actions(Board, Others, Depth, SubAct1),
                                                      get_coord(Piece, Coord),
                                                      get_basic_move_actions_by_depth(Board, Coord, Depth, SubAct2),
                                                      concat(SubAct2, SubAct1, Actions).

% delete_disallowed_neigh_for_rabbit(Piece, Neigh, ResultList) --- Supprime le voisin interdit pour un lapin
delete_disallowed_neigh_for_rabbit(_,[],[]).
delete_disallowed_neigh_for_rabbit([X,Y,rabbit,silver], [[NeighX,Y]|Q], Q):- NeighX is X - 1, !.
delete_disallowed_neigh_for_rabbit([X,Y,rabbit,gold], [[NeighX,Y]|Q], Q):- NeighX is X + 1, !.
delete_disallowed_neigh_for_rabbit(Piece, [T|Q], [T|Result]):- delete_disallowed_neigh_for_rabbit(Piece,Q,Result).

% get_basic_move_actions_by_depth(Board, StartCoord, Depth, Result) --- Recherche tous les deplacement possible jusqu'a une profondeur donnee : profondeur=1 --> deplacements de 1 case
get_basic_move_actions_by_depth(_,_,0,[]):-!.
get_basic_move_actions_by_depth(Board, Coord, Depth, Result):-get_basic_move_actions(Board, Coord, Neigh, Actions),
                                                            NewDepth is Depth - 1,
                                                            basic_move_foreach_neigh(Board, Coord, NewDepth, Neigh, NeighActions),
                                                            concat(Actions, NeighActions, Result).

% get_basic_move_actions(Board, StartCoord, Voisins, Result) --- Renvoie les voisins accessibles et les deplacements possibles de 1 case max
get_basic_move_actions(Board, Coord, Neigh, Result):- get_empty_neigh(Board, Coord, UnsureNeigh),
                                                      get_piece_by_pos(Board, Coord, Piece),
                                                      delete_disallowed_neigh_for_rabbit(Piece, UnsureNeigh, Neigh),
                                                      !,
                                                      add_sub_list(Neigh, SubListedNeigh),
                                                      append_element_to_all(Coord, SubListedNeigh, Actions),
                                                      add_sub_list(Actions, Result).
get_basic_move_actions(Board, Coord, Neigh, Result):- get_empty_neigh(Board, Coord, Neigh),
                                                      add_sub_list(Neigh, SubListedNeigh),
                                                      append_element_to_all(Coord, SubListedNeigh, Actions),
                                                      add_sub_list(Actions, Result).

% basic_move_foreach_neigh(Board, StartCoord, Depth, NeighList, Result) --- Utilisee par get_basic_move_actions_by_depth, renvoie tous les deplacements possible a partir des voisins.
basic_move_foreach_neigh(_,_,_,[],[]).
basic_move_foreach_neigh(Board, Coord, Depth, [Neigh|Q], Result):-move_piece(Board, Coord, Neigh, NewBoard),
                                                                  get_piece_by_pos(NewBoard, Neigh, Piece),
                                                                  movable_piece(NewBoard, Piece),
                                                                  \+dead_piece(NewBoard, Piece),
                                                                  get_basic_move_actions_by_depth(NewBoard, Neigh, Depth, Actions),
                                                                  append_element_to_all([Coord,Neigh], Actions, NewActions),
                                                                  basic_move_foreach_neigh(Board, Coord, Depth, Q, Others),
                                                                  concat(NewActions, Others, Result), !.
basic_move_foreach_neigh(Board, Coord, Depth, [_|Q], Result):-basic_move_foreach_neigh(Board, Coord, Depth, Q, Result).

% get_pull_actions(Board, Piece, ListResult) --- Recherche toutes les actions "tirer" possibles pour la piece passee en parametre
get_pull_actions(Board, Piece, Actions):-bagof(PullAct, pull_action(Board, Piece, PullAct), Actions), !.
get_pull_actions(_, _, []).

% pull_action(Board, Piece, Result) --- Cherche une action "tirer"
pull_action(_, [_,_,rabbit,_], _):-fail.
pull_action(Board, Piece, [pull, [Coord,EmptyCoord],[EnemCoord, Coord]]):-movable_piece(Board, Piece),
                                                                        get_coord(Piece, Coord),
                                                                        get_empty_neigh(Board, Coord, EmptyNeigh),
                                                                        get_neigh(Coord, Neigh),
                                                                        get_pieces_by_pos(Board, Neigh, NeighList),
                                                                        member(EmptyCoord, EmptyNeigh),
                                                                        member(Enem, NeighList),
                                                                        enemy(Enem),
                                                                        stronger_than(Piece, Enem),
                                                                        get_coord(Enem, EnemCoord).

% get_push_actions(Board, Piece, ListResult) --- Recherche toutes les actions "pousser" possibles pour la piece passee en parametre
get_push_actions(Board, Piece, Actions):-bagof(PushAct, push_action(Board, Piece, PushAct), Actions), !.
get_push_actions(_, _, []).

% push_action(Board, Piece, Result) --- Cherche une action "pousser"
push_action(_, [_,_,rabbit,_], _):-fail.
push_action(Board, Piece, _):-get_coord(Piece, Coord),
                              \+ally_around(Board, Coord),
                              get_enemies(Board, Enemies), 
                              get_neigh(Coord, Neigh),
                              get_pieces_by_pos(Enemies, Neigh, NeighList),
                              \+stronger_or_eq_than_all(Piece, NeighList),
                              !,
                              fail.
push_action(Board, Piece, [push, [EnemCoord,EmptyCoord],[Coord, EnemCoord]]):-get_coord(Piece, Coord),
                                                                              get_neigh(Coord, Neigh),
                                                                              get_pieces_by_pos(Board, Neigh, NeighList),
                                                                              member(Enem, NeighList),
                                                                              enemy(Enem),
                                                                              stronger_than(Piece, Enem),
                                                                              get_coord(Enem, EnemCoord),
                                                                              get_empty_neigh(Board, EnemCoord, EmptyNeigh),
                                                                              member(EmptyCoord, EmptyNeigh).



% move_piece(Board, InitPos, NewPos, NewBoard) --- Deplace la piece a la position InitPos a la position NewPos
move_piece([[X,Y,Type,Side]|Q], [X,Y], [NewX,NewY], [[NewX,NewY,Type,Side]|Q]):-!.
move_piece([T|Q], ICoord, NCoord, [T|NewBoard]):-move_piece(Q, ICoord, NCoord, NewBoard).

% get_compact_move(Action, CaseDepart, CaseArrivee) --- Renvoie la case depart et la case d'arrivee pour une action donnee
get_compact_move([[Debut,Fin]|[]], Debut, Fin):- !.
get_compact_move([[Debut,_]|Others], Debut, Fin):- get_compact_move(Others, _, Fin).

% apply_action_on_board(Board, Action, NewBoard) --- Renvoie l'etat du plateau apres application de l'action
apply_action_on_board(Board, [pull, [Debut1, Fin1], [Debut2, Fin2]], NewBoard):-!,
                                                                              move_piece(Board, Debut1, Fin1, TempBoard),
                                                                              move_piece(TempBoard, Debut2, Fin2, NewBoard).
apply_action_on_board(Board, [push, [Debut1, Fin1], [Debut2, Fin2]], NewBoard):-!,
                                                                              move_piece(Board, Debut1, Fin1, TempBoard),
                                                                              move_piece(TempBoard, Debut2, Fin2, NewBoard).
apply_action_on_board(Board, Action, NewBoard):-get_compact_move(Action, Debut, Fin),
                                                move_piece(Board, Debut, Fin, NewBoard).

% get_best_action(Board, Actions, BestScore, BestAction) --- Renvoie la meilleure action a jouer parmis toute la liste
get_best_action(Board, [Action|[]], Score, Action) :- !,
                                                      apply_action_on_board(Board, Action, NewBoard),
                                                      board_score(NewBoard, NewBoard, Score).
get_best_action(Board, [Action|Others], BestScore, BestAct):- get_best_action(Board, Others, TempScore, TempAct),
                                                            apply_action_on_board(Board, Action, NewBoard),
                                                            board_score(NewBoard, NewBoard, ActScore),
                                                            compare_actions_score(TempScore, TempAct, ActScore, Action, BestScore, BestAct).

% compare_actions_score(Score1, Action1, Score2, Action2, BestScore, BestAction) --- Renvoie la meilleure action entre les deux (Si scores egaux => celle qui a le moins de mouvements)
compare_actions_score(Score1, Action1, Score2, _, Score1, Action1) :- Score1 > Score2, !.
compare_actions_score(Score1, _, Score2, Action2, Score2, Action2) :- Score1 < Score2, !.
compare_actions_score(Score1, Action1, _, Action2, Score1, Action1) :- list_size(Action1, SizeAct1), list_size(Action2, SizeAct2), SizeAct1 < SizeAct2, !.
compare_actions_score(_, _, Score2, Action2, Score2, Action2).



%--->SCORE %%%%%%%% HEURISTIQUES POUR DECISION DE L'IA   ---http://www.techno-science.net/?onglet=glossaire&definition=6418---
% Poids associes : 
%  -- Gagnant : INFINI
%  -- Piece sur le plateau : 20
%  -- Piece en danger (peu etre push) : -10
%  -- Piece freeze : -1
%  -- Distance Lapin : -DISTANCE (si D > 3)

%  -- strategie elephant (POS < 2) => -3
%  -- Enable to push / pull to delete
%  -- Gagnant next time

% board_score(Board, Pieces, Score) --- Calcul un score correspondant a l'etat du plateau (Score eleve = tres favorable, faible = plutot defavorable)
board_score(_, [], 0).
board_score(Board, [Piece|Others], Score):-calcul_score_win(Piece, SWin),
                                          calcul_score_freeze(Board, Piece, SFreeze),
                                          calcul_score_piece_alive(Board, Piece, SAlive),
                                          calcul_score_distance_rabbit(Piece, SRabbit),
                                          calcul_score_elephant(Piece, SElephant),
                                          board_score(Board, Others, ScoreOthers),
                                          Score is SWin + SFreeze + SAlive + SRabbit + ScoreOthers + SElephant.


% calcul_score_win(Piece, Score) --- Renvoie un certain score suivant si la piece permet de gagner ou non
calcul_score_win([0,_,rabbit, gold], -2000000):-enemy([0,0,rabbit,gold]),!.
calcul_score_win([0,_,rabbit, gold], 2000000):- !.
calcul_score_win([7,_,rabbit, silver], -2000000):-enemy([7,0,rabbit,silver]),!.
calcul_score_win([7,_,rabbit, silver], 2000000):- !.
calcul_score_win(_, 0).

% calcul_score_freeze(Board, Piece, Score) --- Renvoie un certain score suivant si la pièce est freeze ou pas 
calcul_score_freeze(_, Piece, 0):- enemy(Piece), !.
calcul_score_freeze(Board, Piece, 0):- movable_piece(Board, Piece), !.
calcul_score_freeze(_, _, -1).

% calcul_score_piece_alive(Board, Piece, Score) --- Renvoie un certain score suivant si la piece tombe dans une trappe ou non
calcul_score_piece_alive(Board, Piece, 0):- \+dead_piece(Board, Piece), !.
calcul_score_piece_alive(_, Piece, 30):- enemy(Piece), !.
calcul_score_piece_alive(_, _, -30).

% calcul_score_distance_rabbit(Piece, Score) --- Renvoie un certain score prenant en compte la distance de la piece avec la ligne final (si lapin)
calcul_score_distance_rabbit([_,_,Type,_], 0):- dif(Type, rabbit), !.
calcul_score_distance_rabbit(Piece, 0):- enemy(Piece), !.
calcul_score_distance_rabbit([X,_,_,gold], 0):- X =< 3, !.
calcul_score_distance_rabbit([X,_,_,silver], 0):- X >= 4, !.
calcul_score_distance_rabbit([X,_,_,gold], -X):- !.
calcul_score_distance_rabbit([X,_,_,silver], -Score):- Score is 7 - X.

% calcul_score_elephant(Piece, Score) --- Renvoie un certain score prenant en compte la position de l'elephant sur le plateau
calcul_score_elephant([X, _, elephant, silver], -7):- ally([0, 0, type, silver]), X < 3, !.
calcul_score_elephant([X, _, elephant, gold], -7):- ally([0, 0, type, gold]), X > 4, !.
calcul_score_elephant(_, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS 

looping(B,ALLIES):-get_all_actions(B,ALLIES,4,R), debug_log_slow(R).
%looping2(B):-get_allies(B, ALLIES), get_all_actions(B,ALLIES,4,R), debug_log(R).
looping2(B):-get_all_actions(B,[[1,0,rabbit,silver]],4,R), debug_log_slow(R).
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
