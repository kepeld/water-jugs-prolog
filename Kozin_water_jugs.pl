/* water_jugs.pl
   Задача переливання (Water Jugs Problem)
   Автор: Козін Андрій
   Курс: Логічне програмування, НаУКМА
   
   Використано BFS для оптимального розв'язку.
   
   Чому не CLP(FD): задача переливання — це пошук в просторі станів,
   а не задача обмежень. CLP(FD) підходить для судоку, розкладів тощо.
*/

:- encoding(utf8).

% ============================================================
% ГОЛОВНІ ПРЕДИКАТИ
% ============================================================

% start/0 - запуск прикладу
start :-
    writeln('=== Задача: [5л, 3л] -> 4л ==='),
    solve([5, 3], [5, 0], 4).

% start2/0 - приклад з трьома посудинами
start2 :-
    writeln('=== Задача: [12л, 8л, 5л] -> 6л ==='),
    solve([12, 8, 5], [12, 0, 0], 6).

% solve(+Caps, +Init, +Goal) is det
% Знаходить оптимальний розв'язок задачі переливання
solve(Caps, Init, Goal) :-
    bfs([[Init, []]], Caps, Goal, [], Solution),
    !,
    reverse(Solution, Steps),
    length(Steps, N),
    format('Знайдено розв\'язок за ~w кроків:~n', [N]),
    print_steps(Steps, Init, Caps, 0).
solve(_, _, _) :-
    writeln('Розв\'язок не знайдено!').

% ============================================================
% ПОШУК В ШИРИНУ (BFS)
% ============================================================

% bfs(+Queue, +Caps, +Goal, +Visited, -Solution) is semidet
bfs([[State, Path]|_], _, Goal, _, Path) :-
    member(Goal, State), !.

bfs([[State, Path]|Rest], Caps, Goal, Visited, Solution) :-
    findall(
        [NewState, [Action|Path]],
        (   move(State, Caps, NewState, Action),
            \+ member(NewState, Visited)
        ),
        NewStates
    ),
    append(Rest, NewStates, NewQueue),
    bfs(NewQueue, Caps, Goal, [State|Visited], Solution).

% ============================================================
% ОПЕРАЦІЇ ПЕРЕЛИВАННЯ
% ============================================================

% move(+State, +Caps, -NewState, -Action) is nondet
% Генерує всі можливі переходи через backtracking

% fill(I) - наповнити посудину I з джерела
move(State, Caps, NewState, fill(I)) :-
    nth1(I, State, Level),
    nth1(I, Caps, Cap),
    Level < Cap,
    set_nth(I, State, Cap, NewState).

% empty(I) - спорожнити посудину I
move(State, _, NewState, empty(I)) :-
    nth1(I, State, Level),
    Level > 0,
    set_nth(I, State, 0, NewState).

% pour(I,J) - перелити з I в J
move(State, Caps, NewState, pour(I, J)) :-
    nth1(I, State, LevelI),
    nth1(J, State, LevelJ),
    I \= J,
    LevelI > 0,
    nth1(J, Caps, CapJ),
    LevelJ < CapJ,
    Space is CapJ - LevelJ,
    Amount is min(LevelI, Space),
    NewLevelI is LevelI - Amount,
    NewLevelJ is LevelJ + Amount,
    set_nth(I, State, NewLevelI, TempState),
    set_nth(J, TempState, NewLevelJ, NewState).

% ============================================================
% ДОПОМІЖНІ ПРЕДИКАТИ
% ============================================================

% set_nth(+Index, +List, +Value, -NewList) is det
set_nth(1, [_|T], X, [X|T]) :- !.
set_nth(I, [H|T], X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    set_nth(I1, T, X, R).

% print_steps(+Actions, +State, +Caps, +Step) is det
print_steps([], State, _, N) :-
    format('~w: ~w (ціль!)~n', [N, State]).
print_steps([Action|Rest], State, Caps, N) :-
    format('~w: ~w~n', [N, State]),
    apply_action(Action, State, Caps, NewState),
    N1 is N + 1,
    print_steps(Rest, NewState, Caps, N1).

% apply_action(+Action, +State, +Caps, -NewState) is det
apply_action(fill(I), State, Caps, NewState) :-
    nth1(I, Caps, Cap),
    set_nth(I, State, Cap, NewState).
apply_action(empty(I), State, _, NewState) :-
    set_nth(I, State, 0, NewState).
apply_action(pour(I, J), State, Caps, NewState) :-
    nth1(I, State, LevelI),
    nth1(J, State, LevelJ),
    nth1(J, Caps, CapJ),
    Space is CapJ - LevelJ,
    Amount is min(LevelI, Space),
    NewLevelI is LevelI - Amount,
    NewLevelJ is LevelJ + Amount,
    set_nth(I, State, NewLevelI, TempState),
    set_nth(J, TempState, NewLevelJ, NewState).

% demo/0 is det
demo :- start, nl, start2.

% ============================================================
% МУЛЬТИПРИЗНАЧЕНІСТЬ ПРЕДИКАТІВ
% ============================================================
/*
  solve/3       - det     (один розв'язок через cut)
  bfs/5         - semidet (успіх або fail)
  move/4        - nondet  (генерує варіанти через backtracking)
  set_nth/4     - det     (один результат)
  print_steps/4 - det     (виведення)
  apply_action/4- det     (один результат)
*/