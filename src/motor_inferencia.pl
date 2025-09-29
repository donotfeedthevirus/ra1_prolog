:- module(motor_inferencia, [
    calcula_pontuacao/3,
    recomenda/2,
    explica/2
]).

:- use_module(library(lists)).          % sumlist/2
:- use_module(base_conhecimento).       % trilha/2, perfil/3, pergunta/3

% Ler respostas de resposta/2 (populado pela UI ou pelos testes)
:- dynamic resposta/2.

% True quando pergunta Id tem uma resposta positiva 
resposta_positiva(Id) :-
    resposta(Id, s).

% Map pergunta Id -> Caracteristica
caracteristica_da_pergunta(Id, C) :-
    pergunta(Id, _Texto, C).

/*
calcula_pontuacao(+UserCtx, +Trilha, -Pontuacao)
- Mantemos o parâmetro UserCtx só para bater com a assinatura definida no plano,
  mas aqui ele não é usado (as respostas vêm direto dos fatos resposta/2).
- A Pontuacao é calculada somando os pesos de todas as Caracteristicas que o
  usuário respondeu 's', de acordo com os fatos perfil(Trilha, Caracteristica, Peso).
*/

calcula_pontuacao(_UserCtx, Trilha, Pontuacao) :-
    % collect weights for every positive answer that maps to a characteristic
    findall(Peso,
        (
            resposta_positiva(Id),
            caracteristica_da_pergunta(Id, C),
            perfil(Trilha, C, Peso)
        ),
        Pesos),
    sumlist(Pesos, Pontuacao).


/*
recomenda(+UserCtx, -RankingOrdenado)
- Produz uma lista ordenada por pontuação desc.
- Formato: [trilha(Trilha, Pontuacao), ...]
*/
recomenda(UserCtx, RankingOrdenado) :-
    % coleta todas as trilhas
    findall(T, trilha(T, _), Trilhas),
    % calcula (Score, Trilha)
    findall(Score-T,
        (
            member(T, Trilhas),
            calcula_pontuacao(UserCtx, T, Score)
        ),
        PairsAsc),  % still unsorted
    % ordenar por Score desc (convertendo para -Score asc)
    findall(NegScore-T, (member(Score-T, PairsAsc), NegScore is -Score), NegPairs),
    keysort(NegPairs, SortedNegPairs),   % ascending by -Score  => descending by Score
    % volta ao formato legível
    findall(trilha(T,S),
        (member(NegS-T, SortedNegPairs), S is -NegS),
        RankingOrdenado).

/*
explica(+Trilha, -Evidencias)
- Evidencias é uma lista de pares (Caracteristica,Peso) que contribuíram
  para a pontuação dessa Trilha dadas as respostas 's'.
- Útil para exibir "por que" a recomendação foi feita.
*/
explica(Trilha, Evidencias) :-
    % pega todas as caracteristicas positivas vindas das respostas
    setof(C,
          Id^Txt^(resposta_positiva(Id), pergunta(Id, Txt, C)),
          CaracteristicasPositivas), !,
    % para cada característica positiva, se houver peso na trilha, registra
    findall((C, Peso),
        (
            member(C, CaracteristicasPositivas),
            perfil(Trilha, C, Peso)
        ),
        Evidencias).
explica(_Trilha, []) :-
    % Sem respostas positivas, sem evidências.
    % (Usado quando o usuário ainda não respondeu ou perfil vazio)
    true.
