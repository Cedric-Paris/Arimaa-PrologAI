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
get_moves([[[1,5],[2,5]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).


% get_x()
% get_y()
% get_coord()
% get_type()
% get_piece_side()
% get_piece_by_pos()
% empty()
% enemy() -> isEnemy
% ally() -> isAllied
% trap() -> isTrap
% get_enemy_by_type()
% get_allies_by_type()
% get_neigh()
% get_empty_pos()
% get_empty_neigh()
% get_movable_pieces()


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS OUTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Debug
debug_log([]):-write('-END-').
debug_log([T|Q]):-writeln(T), debug_log(Q).

% list_size(List, Size) --- Determine le nombre d'element dans une liste
list_size([],0).
list_size([_|Q], SIZE):-list_size(Q, R), SIZE is R + 1. 



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

% enemy(Piece) --- Renvoie vrai si la piece appartient a l'ennemie'
enemy([_,_,_,gold]).

% ally(Piece) --- Renvoie vrai si la piece est alliee
ally([_,_,_,silver]).

% ally_around(Board, Coord) --- Renvoie vrai si un allie est voisin de la case Coord
ally_around(B, COORD):- get_neigh(COORD, N), get_allies(B, AL), get_pieces_by_pos(AL, N, R), list_size(R, SIZE), SIZE =\= 0. 

% trap(Coord) --- Vrai si la case est une trape
trap([2,2]).
trap([2,5]).
trap([5,2]).
trap([5,5]).

% force(Piece, Result) --- Donne la force d'une piece suivant son type
force([_,_,rabbit,_], 0).
force([_,_,cat,_], 1).
force([_,_,dog,_], 2).
force([_,_,horse,_], 3).
force([_,_,camel,_], 4).
force([_,_,elephant,_], 5).

% stronger_than(Piece1, Piece2) --- Renvoie vrai si Piece1 est plus forte que Piece2
stronger_than(P1, P2):-force(P1, F1), force(P2, F2), F1 > F2.

% stronger_than_all(Piece, Other) --- Renvoie vrai si la piece est plus forte que toutes les autres
stronger_than_all(_, []).
stronger_than_all(P, [T|Q]):-stronger_than(P, T), stronger_than_all(P, Q).

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

% get_movable_pieces(Board, Pieces, ResultList) --- Renvoie la liste des pieces alliees qui peuvent bouger.
get_movable_pieces(_, [], []).
% ------On passe les pieces ennemies
get_movable_pieces(B, [T|Q], R):-enemy(T), get_movable_pieces(B, Q, R), !.
% ------Cas entouré de pieces
get_movable_pieces(B, [T|Q], R):-get_coord(T, COORD), get_empty_neigh(B, COORD, N), list_size(N, SIZE), SIZE =:= 0, get_movable_pieces(B, Q, R), !.
% ------Cas ou il y a un allie autour
get_movable_pieces(B, [T|Q], [T|R]):-get_coord(T, COORD), ally_around(B, COORD), get_movable_pieces(B, Q, R), !.
% ------Cas ou on est plus fort que tous les enemies
get_movable_pieces(B, [T|Q], [T|R]):-get_coord(T, COORD), get_neigh(COORD, N), get_enemies(B, EN), get_pieces_by_pos(EN, N, ENN), stronger_than_all(T, ENN), get_movable_pieces(B, Q, R), !. 
% ------Autres cas: non deplacable
get_movable_pieces(B, [_|Q], R):-get_movable_pieces(B, Q, R), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FONCTIONS DE L'IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IA
% Si mouvement gagnant -> jouer.
% Si tuer enemie -> tuer.
% Poser elephant a cote d'un piege
% Reflexion par action (calcul enemie impossible)
      % Generer action [nbAction, [MOVE 1], [MOVE 2]]
      % Calculer score [score [nbAction, [MOVE 1], [MOVE 2]]]
      % Jouer Meilleur ORDER BY SCORE
      % Boucler si reste des coup
% get_action_value() // Calcul score -> GhostInTheCell
