:- module(interface, [
    iniciar/0,
    faz_perguntas/0,
    exibe_resultado/1
]).

:- use_module(base_conhecimento).
:- use_module(motor_inferencia).

% helpers para normalizar respostas

normalize_answer(StrIn, s) :-
    string_lower(StrIn, L),
    member(L, ["s", "sim", "y", "yes"]).
normalize_answer(StrIn, n) :-
    string_lower(StrIn, L),
    member(L, ["n", "nao", "não", "no"]).

% fluxo de uso

% ------------------------------------------------------------
% iniciar/0
% Ponto de entrada da interface interativa:
% - limpa as respostas antigas
% - mostra a introdução na tela
% - chama o loop de perguntas
% - roda a recomendação e exibe o resultado
% ------------------------------------------------------------
iniciar :-
    retractall(motor_inferencia:resposta(_,_)),
    writeln('=== Orientador de Trilhas de Computação ==='),
    writeln('Responda as perguntas com s (sim) ou n (não).'),
    writeln('-------------------------------------------'),
    faz_perguntas,
    recomenda(_, Ranking),
    exibe_resultado(Ranking).

% ------------------------------------------------------------
% faz_perguntas/0
% Percorre todas as perguntas definidas em base_conhecimento,
% garante que a ordem dos Ids está certa,
% e para cada uma chama perguntar/1 (abaixo).
% ------------------------------------------------------------
faz_perguntas :-
    findall(Id, pergunta(Id, _, _), Ids),
    sort(Ids, SortedIds),
    forall(member(Id, SortedIds), perguntar(Id)).

% ------------------------------------------------------------
% perguntar/1
% Exibe a pergunta pelo terminal,
% lê a resposta do usuário (string),
% normaliza pra 's' ou 'n',
% e salva como fato resposta(Id, s/n).
% Se a entrada for inválida, avisa e pergunta de novo.
% ------------------------------------------------------------
perguntar(Id) :-
    pergunta(Id, Texto, _Car),
    format('~d) ~w (s/n): ', [Id, Texto]),
    read_line_to_string(user_input, Str),
    ( normalize_answer(Str, Ans) -> assertz(motor_inferencia:resposta(Id, Ans));
        writeln('Entrada inválida, use s/n (ou sim/nao).'),
        perguntar(Id)
    ).

% ------------------------------------------------------------
% exibe_resultado/1
% Recebe o ranking de trilhas,
% imprime todas com suas pontuações,
% e depois mostra a(s) trilha(s) com maior score
% junto com as evidências (características que contaram pontos).
% ------------------------------------------------------------
exibe_resultado(Ranking) :-
    writeln('\n=== Resultado Final ==='),
    forall(member(trilha(T, S), Ranking),
           format('~w: ~d pontos~n', [T, S])),
    nl,
    ( Ranking = [trilha(_, TopScore)|_] ->
        findall(T, member(trilha(T, TopScore), Ranking),TopTrilhas),
        writeln('Recomendação principal:'),
        forall(member(T, TopTrilhas),
               ( format('- ~w~n', [T]),
                 explica(T, Evidencias),
                 format('  Evidências: ~w~n', [Evidencias])
               ))
    ;   writeln('Nenhuma trilha pôde ser recomendada.')
    ).
