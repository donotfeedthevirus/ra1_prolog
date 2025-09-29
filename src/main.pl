:- use_module(base_conhecimento).
:- use_module(motor_inferencia).
:- use_module(interface).

% entrada "interativa"
:- initialization(iniciar, main).

run_test(File) :-
    retractall(motor_inferencia:resposta(_,_)),
    retractall(expected_top(_)),
    consult(File),
    recomenda(_, Ranking),
    Ranking = [trilha(Top, Score)|_],
    ( expected_top(Expected),
      (Top == Expected
      -> format('~w: PASS (top=~w, score=~d)~n', [File, Top, Score])
      ;  format('~w: FAIL (expected=~w got=~w)~n', [File, Expected, Top])
      )
    ; format('~w: WARN (no expected_top/1 in fixture)~n', [File])
    ),
    retractall(motor_inferencia:resposta(_,_)),
    retractall(expected_top(_)).

/*
run_tests/0
- roda todos arquivos tests/perfil_teste_*.pl
*/
run_tests :-
    expand_file_name('tests/perfil_teste_*.pl', Files),
    forall(member(F, Files), run_test(F)),
    halt.
