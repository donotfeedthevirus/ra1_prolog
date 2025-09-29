/* perfil focado em segurança (espera seguranca_da_informacao no topo) */
:- dynamic motor_inferencia:resposta/2.

motor_inferencia:resposta(9, s).   % redes
motor_inferencia:resposta(10, s).  % segurança
motor_inferencia:resposta(1, s).   % logica
motor_inferencia:resposta(2, n).
motor_inferencia:resposta(3, n).
motor_inferencia:resposta(4, n).
motor_inferencia:resposta(5, n).
motor_inferencia:resposta(6, n).
motor_inferencia:resposta(7, n).
motor_inferencia:resposta(8, n).

expected_top(seguranca_da_informacao).
