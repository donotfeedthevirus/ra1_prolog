:- use_module(base_conhecimento).
:- use_module(motor_inferencia).
:- use_module(interface).

% entrada "interativa"
:- initialization(iniciar, main).

% testes ainda vao ser implementados
run_tests :-
    writeln('> Testes automatizados ainda nao implementados (Milestone 4).'),
    halt.
