/* perfil focado em web (espera desenvolvimento_web no topo) */
:- dynamic motor_inferencia:resposta/2.

motor_inferencia:resposta(6, s). % frontend
motor_inferencia:resposta(7, s). % backend
motor_inferencia:resposta(8, s). % banco
motor_inferencia:resposta(1, s). % logica
motor_inferencia:resposta(2, n).
motor_inferencia:resposta(3, n).
motor_inferencia:resposta(4, n).
motor_inferencia:resposta(5, n).
motor_inferencia:resposta(9, n).
motor_inferencia:resposta(10, n).

expected_top(desenvolvimento_web).
