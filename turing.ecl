initial_state(program(InitialState, _, _), InitialState).

final_states(program(_, FinalStates, _), FinalStates).

transitions(program(_, _, Deltas), Deltas).

next(Program, State0, Sym0, Sym1, Dir, State1) :-
         transitions(Program, Deltas),
         member(delta(State0, Sym0, Sym1, Dir, State1), Deltas).

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

main :-
    Program = program(
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
    ),
    Input = [1, 1],
    run_turing_machine(Program, Input, _Output, _FinalState, Dump),
    dump_to_mpost("turing_run.mp", Dump)