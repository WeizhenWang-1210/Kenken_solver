kenken(N, C, T) :- sudoku_cell(N,T),satisfy_C(C,T), maplist(fd_labeling, T).

/*////////////////////Start of Sudoku Setup///////////////////*/
/*////////////////////Start of Sudoku Setup///////////////////*/
/*////////////////////Start of Sudoku Setup///////////////////*/
sudoku_cell(N, X) :-
    % array size limits
    len_row(X, N),
    len_col(X, N),
    % finish domain limits
    within_domain(X, N),
    maplist(fd_all_different, X),
    transpose(X, T),
    maplist(fd_all_different, T).
    %maplist(fd_labeling, X).

len_row(X, N) :-
    length(X, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    % http://www.gprolog.org/manual/html_node/gprolog057.html fd_domain(Vars, Lower, Upper)
    fd_domain(HD, 1, N),
    within_domain(TL, N).

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

/*////////////////////End of Sudoku Setup///////////////////*/
/*////////////////////End of Sudoku Setup///////////////////*/
/*////////////////////End of Sudoku Setup///////////////////*/



get(R,C,T,V):-nth(R,T,Tr),nth(C,Tr,V).


satisfy_C([],_).
satisfy_C([HD|TL],T):- satisfy(HD, T), satisfy_C(TL,T).
satisfy(C,T):- isAdd(C), arg(1,C,Sum), arg(2,C,Lists), add(T,Lists,Sum);
               isMult(C), arg(1,C,Prod), arg(2,C,Lists), mult(T,Lists,Prod);
               isDiff(C), arg(1,C,Diff), arg(2,C,Op1), arg(3,C,Op2), diff(T,Op1,Op2,Diff);
               isDiv(C), arg(1,C,Div), arg(2,C,Op1), arg(3,C,Op2), div(T,Op1,Op2,Div).

%+(S,L)
isAdd(C):-functor(C,+,2).
add(_,[],0).
add(T,[Hd|Tl],Sum):- flatten(Hd, Cord), nth(1,Cord,Row), nth(2,Cord,Col), get(Row,Col,T,Val),add(T,Tl,Rest), Sum #= Val+Rest.

%*(P, L)
isMult(C):-functor(C,*,2).
mult(_,[],1).
mult(T,[Hd|Tl],Prod):- flatten(Hd, Cord), nth(1,Cord,Row), nth(2,Cord,Col), get(Row,Col,T,Val),mult(T,Tl,Rest), Prod #= Val*Rest.

%−(D, J, K)
isDiff(C):-functor(C,-,3).
diff(T,Op1,Op2,Diff):- flatten(Op1, Cord1), nth(1,Cord1,Row1), nth(2,Cord1,Col1), get(Row1,Col1,T,Val1),
                       flatten(Op2, Cord2), nth(1,Cord2,Row2), nth(2,Cord2,Col2), get(Row2,Col2,T,Val2),
                       sat_diff(Val1,Val2,Diff).

sat_diff(Val1,Val2,Diff):-Diff#=Val1 - Val2;
                          Diff#=Val2 - Val1.

%/(Q, J, K)
isDiv(C):-functor(C,/,3).
div(T,Op1,Op2,Div):- flatten(Op1, Cord1), nth(1,Cord1,Row1), nth(2,Cord1,Col1), get(Row1,Col1,T,Val1),
                     flatten(Op2, Cord2), nth(1,Cord2,Row2), nth(2,Cord2,Col2), get(Row2,Col2,T,Val2),
                     sat_div(Val1,Val2,Div).

sat_div(Val1,Val2,Div):-Div#=Val1 / Val2;
                        Div#=Val2 / Val1.



plain_kenken(N,C,T):-good_cells(N,T),satisfy_C(C,T).

good_cells(N,X):-
    len_row(X, N),
    len_col(X, N),
    check_rows(X,N),
    transpose(X,T),
    check_rows(T,N).


check_rows([],_).
check_rows([Hd|Tl],N):-good_row(Hd,N),check_rows(Tl,N).

good_row(L,N):-findall(Num, between(1, N, Num), Li),permutation(Li,L).



/*////////////////////Begin of Perfromance Comparison///////////////////*/
/*////////////////////Begin of Perfromance Comparison///////////////////*/
/*////////////////////Begin of Perfromance Comparison///////////////////*/
/*
    When the following queries are supplied to gnu prolog interpreter
statistics(cpu_time,[Start|_]),
plain_kenken(                                                
  4,                                                         
  [                                                             
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ],
  _
),
statistics(cpu_time,[Stop|_]),
Runtime is (Stop - Start).
Runtime = 45
Start = 6029
Stop = 6074 ? a

Runtime = 105
Start = 6029
Stop = 6134

Runtime = 402
Start = 6029
Stop = 6431

Runtime = 463
Start = 6029
Stop = 6492

Runtime = 747
Start = 6029
Stop = 6776

Runtime = 912
Start = 6029
Stop = 6941

Compared to 

statistics(cpu_time,[Start|_]),
kenken(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ],
  _
),
statistics(cpu_time,[Stop|_]),
Runtime is (Stop - Start).

Runtime = 0
Start = 7471
Stop = 7471 ? a

Runtime = 0
Start = 7471
Stop = 7471

Runtime = 0
Start = 7471
Stop = 7471

Runtime = 0
Start = 7471
Stop = 7471

Runtime = 0
Start = 7471
Stop = 7471

Runtime = 0
Start = 7471
Stop = 7471

As you can see, the runtime for finding a solution for kenken is very close to 0, compared to 45/105/402/463/747/912 ms of plain_kenken.
Moreover, grid with size 6 is almost unsolvable for plain_kenken.
*/

/*////////////////////End of Perfromance Comparison///////////////////*/
/*////////////////////End of Perfromance Comparison///////////////////*/
/*////////////////////End of Perfromance Comparison///////////////////*/


/*////////////////////Begin of no-Op Kenken Setup///////////////////*/
/*////////////////////Begin of no-Op Kenken Setup///////////////////*/
/*////////////////////Begin of no-Op Kenken Setup///////////////////*/
/*
noop_kenken(N,C,T,P)
    N-> The side length of the square representing the puzzle grid
    C -> List of all constraints that the puzzle grid need to satisfy
    T -> A list of list of integers. All the lists have length N, representing the N*N grid
    P -> List of all constraints with operation specified
Each constraint in C is of the following form
    ^(G, L)
    Where G is the desired integer value that should be acheived. Only C with length(L,2) will be either
    division or minus. For C with L's length greater than 2, the goal G can be achieved using any combination
    of +, -, *, / on the elements in L. For C with L's length = 1, this is a trivial contraint: it merely
    specifies that the value of this block on the grid should be G.

On Success, T will match the representation of the grid satisfying all constraints, and P will be constraints 
with arithmetic operations specified.


noop_ken_ken_testcase(
    5,
    [
        (2,[[1|1]]),
        (6,[[2|1],[3|1]]),
        (6,[[4|1],[4|2],[4|3]]),
        (4,[[5|1]]),
        (1,[[4|3]]),
        (38,[[1|2],[1|3],[1|4],
             [2|2],
             [3|2],[3|3],[3|4],
                         [4|4],
             [5|2],[5|3],[5|4]]),
        (24,[            [1|5],
             [2|3],[2|4],[2|5]]),
        (9,[[3|5],[4|5],[5|5]])
    ],
    T,
    P
)
?- fd_set_vector_max(255), noop_ken_ken_testcase(N,C), noop_kenken(N,C,T,P).
The output should be the following
T = [[2,1,5,3,4],[5,4,3,1,2],[1,5,4,2,3],[3,2,1,4,5],[4,3,2,5,1]]
P = [
     +(2,[[1|1]]),
        +(6,[[2|1],[3|1]]),
        *(6,[[4|1],[4|2],[4|3]]),
        +(4,[[5|1]]),
        +(1,[[4|3]]),
        +(38,[[1|2],[1|3],[1|4],
             [2|2],
             [3|2],[3|3],[3|4],
                         [4|4],
             [5|2],[5|3],[5|4]]),
        *(24,[            [1|5],
             [2|3],[2|4],[2|5]]),
        +(9,[[3|5],[4|5],[5|5]])
]
?
And this should be the only solution.If you repsond with a ";" the next result should be "no".
*/


