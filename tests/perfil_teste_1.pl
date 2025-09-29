/* perfil focado em dados (espera ciência de dados no topo) */
:- dynamic motor_inferencia:resposta/2.

motor_inferencia:resposta(2, s). % matematica
motor_inferencia:resposta(3, s). % estatistica
motor_inferencia:resposta(8, s). % banco_de_dados
motor_inferencia:resposta(1, s). % logica
motor_inferencia:resposta(7, n). % desenvolvimento_backend
motor_inferencia:resposta(6, n). % desenvolvimento_frontend
motor_inferencia:resposta(9, n). % redes
motor_inferencia:resposta(10, n). % seguranca
motor_inferencia:resposta(4, n). % programacao_funcional
motor_inferencia:resposta(5, n). % programacao_logica

expected_top(ciencia_de_dados).
