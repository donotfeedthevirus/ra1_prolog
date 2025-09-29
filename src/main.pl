:- use_module(base_conhecimento).
:- use_module(motor_inferencia).
:- use_module(interface).

% ------------------------------------------------------------
% entrada "interativa"
% Este initialization garante que se rodarmos
% `swipl -s src/main.pl` direto no terminal,
% o programa já começa chamando iniciar/0.
% ------------------------------------------------------------
:- initialization(iniciar, main).

% ------------------------------------------------------------
% run_test/1
% Executa um único arquivo de teste:
% - limpa respostas anteriores
% - consulta o arquivo (que já define respostas e expected_top/1)
% - roda o motor de inferência
% - compara o top obtido com o esperado
% - imprime PASS/FAIL (ou WARN se não tiver expected_top/1)
% ------------------------------------------------------------
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

% ------------------------------------------------------------
% run_tests/0
% Roda todos os arquivos de teste que seguem o padrão
% tests/perfil_teste_*.pl e executa run_test/1 em cada um.
% No fim dá halt para encerrar o Prolog.
% ------------------------------------------------------------
run_tests :-
    expand_file_name('tests/perfil_teste_*.pl', Files),
    forall(member(F, Files), run_test(F)),
    halt.
