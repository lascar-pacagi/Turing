%Author: Pascal Garcia

%%%%%%%%%%% First part

copy_prog(program(
                     start, 
                     [stop], 
                     [delta(start, ' ', ' ', right, stop),
                      delta(start, 1, ' ', right, s2),
                      delta(s2, 1, 1, right, s2),
                      delta(s2, ' ', ' ', right, s3),
                      delta(s3, 1, 1, right, s3),
                      delta(s3, ' ', 1, left, s4),
                      delta(s4, 1, 1, left, s4),
                      delta(s4, ' ', ' ', left, s5),
                      delta(s5, 1, 1, left, s5),
                      delta(s5, ' ', 1, right, start)
                     ]
                 )
         ).

initial_state(program(InitialState, _, _), InitialState).

final_states(program(_, FinalStates, _), FinalStates).

transitions(program(_, _, Deltas), Deltas).

%%%%% Question 1.1 %%%%%
next(Program, State0, Sym0, Sym1, Dir, State1) :-
         transitions(Program, Deltas),
         member(delta(State0, Sym0, Sym1, Dir, State1), Deltas).
%%%%%

%%%%% Question 1.2 %%%%%
update_tape(tape(Left0, [_ | Right]), Sym, left, tape(Left2, [Last, Sym | Right])) :-
         append(Left1, [Last], Left0),
         (
             Left1 == [], Left2 = [' ']
         ;
             Left1 \== [], Left2 = Left1
         ).

update_tape(tape(Left0, [_ | Right0]), Sym, right, tape(Left1, Right1)) :-
         append(Left0, [Sym], Left1),
         (
             Right0 == [], Right1 = [' ']
         ;
             Right0 \== [], Right1 = Right0
         ).
%%%%%

%%%%% Question 1.3 %%%%%
run_turing_machine(Program, Input0, Output, FinalState) :-
         initial_state(Program, InitialState),
         append(Input0, [' '], Input1),
         run(Program, InitialState, tape([' '], Input1), Output, FinalState).

run(Program, State, tape(Left, Right), Output, FinalState) :-
         final_states(Program, FinalStates),
         member(State, FinalStates),
         append(Left, Right, Output),
         FinalState = State.
       
run(Program, State0, Tape0, Output, FinalState) :-
         tape(_, [Sym0 | _]) = Tape0,
         next(Program, State0, Sym0, Sym1, Dir, State1),
         update_tape(Tape0, Sym1, Dir, Tape1),
         run(Program, State1, Tape1, Output, FinalState).
%%%%%

%%%%% Question 1.4 %%%%%
run_turing_machine(Program, Input0, Output, FinalState, Dump) :-
        initial_state(Program, InitialState),
        append(Input0, [' '], Input1),
        run(Program, InitialState, tape([' '], Input1), Output, FinalState, [(InitialState, tape([' '], Input1))], Dump).

run(Program, State, tape(Left, Right), Output, FinalState, DumpAcc, Dump) :-
        final_states(Program, FinalStates),
        member(State, FinalStates),
        append(Left, Right, Output),
        FinalState = State,
        reverse(DumpAcc, Dump).
        
run(Program, State0, Tape0, Output, FinalState, DumpAcc, Dump) :-
        tape(_, [Sym0 | _]) = Tape0,
        next(Program, State0, Sym0, Sym1, Dir, State1),
        update_tape(Tape0, Sym1, Dir, Tape1),        
        run(Program, State1, Tape1, Output, FinalState, [(State1, Tape1) | DumpAcc], Dump).
%%%%%

%write to meta post format
%compile result with: 
% mpost filename
% epstopdf filename.1
dump_to_mpost(Filename, Dump) :-
        open(Filename, write, Stream),
	write_header(Stream),
        write_dump(0, Dump, Stream),
        write_end(Stream),
        close(Stream).

write_header(Stream) :-
        write(Stream, 'prologues := 1;\n'),
        write(Stream, 'input turing;\n'),
        write(Stream, 'beginfig(1)\n').

write_end(Stream) :-
        write(Stream, 'endfig;\n'),
        write(Stream, 'end').

write_dump(_, [], _).
write_dump(Y, [(State, Tape) | Tapes], Stream) :-
        write(Stream, 'tape(0, '),
        write(Stream, Y),
        write(Stream, 'cm, 1cm, \"'),
        write(Stream, State),
        write(Stream, '\", '),
        write_tape(Tape, Stream),
        write(Stream, ');\n'),
        Y1 is Y - 2,
        write_dump(Y1, Tapes, Stream).

write_tape(tape(Left, Right), Stream) :-
        length(Left, N),
        write(Stream, '\"'),
        append(Left, Right, L),
        (param(Stream), foreach(X, L) do 
            write(Stream, X)        
        ),
        write(Stream, '\", '),
        write(Stream, N),
        write('\n').

%%%%%%%%%%% Optional part        

%busy beaver
busy_beaver_prog(program(
                            start, 
                            [stop], 
                            [delta(start, ' ', 1, right, s2),
                             delta(start, 1, 1, left, s3),
                             delta(s2, ' ', 1, left, start),
                             delta(s2, 1, 1, right, s2),
                             delta(s3, ' ', 1, left, s2),
                             delta(s3, 1, 1, left, stop)
                            ]
                        )
                ).

%make_pairs(+, -): 'a list * ('a * 'a) list
make_pairs([], _, []).
make_pairs([X | L], L2, Res) :-
        make_pairs_aux(X, L2, Pairs),
        make_pairs(L, L2, RemainingPairs),
        append(Pairs, RemainingPairs, Res).

%make_pairs_aux(+, +, -): 'a * 'a list * ('a * 'a) list
make_pairs_aux(_, [], []).
make_pairs_aux(X, [Y | Ys], [(X, Y) | Zs]) :-
        make_pairs_aux(X, Ys, Zs).

complete(S1, Sym, Symbols, Directions, States, Res) :-
        member(Sym1, Symbols),
        member(Dir, Directions),
        member(S2, States),
        Res = delta(S1, Sym, Sym1, Dir, S2).

complete_list([], _, _, _, []).
complete_list([(S, Sym) | Pairs], Symbols, Directions, States, [Delta | Deltas]) :-
        complete(S, Sym, Symbols, Directions, States, Delta),
        complete_list(Pairs, Symbols, Directions, States, Deltas).

%%%%% Question 2.1 %%%%%        
all_programs(Init, States, Symbols, FinalStates, Program) :-
        make_pairs([Init | States], Symbols, Pairs),
        append([Init | States], FinalStates, AllStates),
        complete_list(Pairs, Symbols, [left, right], AllStates, Deltas),
        Program = program(Init, FinalStates, Deltas).
%%%%%

%%%%% Question 2.2 %%%%%
run_turing_machine(Program, Input0, Output, FinalState, NbIterMax) :-
         initial_state(Program, InitialState),
         append(Input0, [' '], Input1),
         run(Program, InitialState, tape([' '], Input1), Output, FinalState, 0, NbIterMax).

run(_, State, tape(Left, Right), Output, FinalState, NbIterMax, NbIterMax) :-
         append(Left, Right, Output),
         FinalState = State.
       
run(Program, State, tape(Left, Right), Output, FinalState, NbIter, NbIterMax) :-
         NbIter < NbIterMax,
         final_states(Program, FinalStates),
         member(State, FinalStates),
         append(Left, Right, Output),
         FinalState = State.
       
run(Program, State0, Tape0, Output, FinalState, NbIter, NbIterMax) :-
         NbIter < NbIterMax,
         tape(_, [Sym0 | _]) = Tape0,
         next(Program, State0, Sym0, Sym1, Dir, State1),
         update_tape(Tape0, Sym1, Dir, Tape1),
         SuccNbIter is NbIter + 1,
         run(Program, State1, Tape1, Output, FinalState, SuccNbIter, NbIterMax).
%%%%%

%%%%% Question 2.3 %%%%%
find_busy_beaver(Init, States, FinalStates, Symbols, NbMaxIter, BusyBeaverNumber, Program) :-
        find_busy_beaver_aux(Init, States, Symbols, FinalStates, NbMaxIter, BusyBeaverNumber, Program).

find_busy_beaver_aux(Init, States, Symbols, FinalStates, NbMaxIter, BusyBeaverNumber, Program) :-
        all_programs(Init, States, Symbols, FinalStates, Program),
        run_turing_machine(Program, [], Output, FinalState, NbMaxIter),
        member(FinalState, FinalStates),
        number_of_ones(Output, BusyBeaverNumber).

number_of_ones([], 0).
number_of_ones([X | Xs], N) :-
        number_of_ones(Xs, N1),
	(
            X == 1, N is N1 + 1
        ;
            X \== 1, N is N1
        ).
%%%%%
