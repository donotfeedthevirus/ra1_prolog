:- module(base_conhecimento, [
    trilha/2,
    perfil/3,
    pergunta/3,
    lista_trilhas/0,
    lista_perguntas/0
]).

/** <module> Base de Conhecimento
  * - trilha/2: identificador e descrição
  * - perfil/3: pesos por característica em cada trilha (1..5)
  * - pergunta/3: mapeia pergunta -> característica
  */

%% trilha(Id, Descricao).
trilha(inteligencia_artificial, 'Foco em IA prática e fundamentos lógico-matemáticos.').
trilha(desenvolvimento_web, 'Front-end e back-end para aplicações web.').
trilha(seguranca_da_informacao, 'Defesa, ofensiva, criptografia e boas práticas de segurança.').
trilha(ciencia_de_dados, 'Estatística, análise de dados e modelagem.').
trilha(redes_e_infraestrutura, 'Redes, sistemas e operação de infraestrutura.').

/* Domínio de características
   - raciocinio_logico
   - matematica
   - estatistica
   - programacao_funcional
   - programacao_logica
   - desenvolvimento_frontend
   - desenvolvimento_backend
   - banco_de_dados
   - redes
   - seguranca
*/

%% perfil(Trilha, Caracteristica, Peso) com Peso em 1..5

% Inteligência Artificial
perfil(inteligencia_artificial, raciocinio_logico, 5).
perfil(inteligencia_artificial, matematica, 4).
perfil(inteligencia_artificial, estatistica, 5).
perfil(inteligencia_artificial, programacao_funcional, 3).
perfil(inteligencia_artificial, programacao_logica, 4).
perfil(inteligencia_artificial, desenvolvimento_backend, 2).
perfil(inteligencia_artificial, desenvolvimento_frontend, 1).
perfil(inteligencia_artificial, banco_de_dados, 3).
perfil(inteligencia_artificial, redes, 1).
perfil(inteligencia_artificial, seguranca, 2).

% Desenvolvimento Web
perfil(desenvolvimento_web, desenvolvimento_frontend, 5).
perfil(desenvolvimento_web, desenvolvimento_backend, 5).
perfil(desenvolvimento_web, banco_de_dados, 4).
perfil(desenvolvimento_web, raciocinio_logico, 4).
perfil(desenvolvimento_web, programacao_funcional, 2).
perfil(desenvolvimento_web, programacao_logica, 2).
perfil(desenvolvimento_web, matematica, 2).
perfil(desenvolvimento_web, estatistica, 1).
perfil(desenvolvimento_web, redes, 2).
perfil(desenvolvimento_web, seguranca, 3).

% Segurança da Informação
perfil(seguranca_da_informacao, seguranca, 5).
perfil(seguranca_da_informacao, redes, 5).
perfil(seguranca_da_informacao, raciocinio_logico, 4).
perfil(seguranca_da_informacao, desenvolvimento_backend, 3).
perfil(seguranca_da_informacao, matematica, 3).
perfil(seguranca_da_informacao, estatistica, 2).
perfil(seguranca_da_informacao, programacao_logica, 3).
perfil(seguranca_da_informacao, programacao_funcional, 2).
perfil(seguranca_da_informacao, banco_de_dados, 3).
perfil(seguranca_da_informacao, desenvolvimento_frontend,1).

% Ciência de Dados
perfil(ciencia_de_dados, estatistica, 5).
perfil(ciencia_de_dados, matematica, 5).
perfil(ciencia_de_dados, banco_de_dados, 4).
perfil(ciencia_de_dados, raciocinio_logico, 4).
perfil(ciencia_de_dados, programacao_funcional, 3).
perfil(ciencia_de_dados, programacao_logica, 3).
perfil(ciencia_de_dados, desenvolvimento_backend, 2).
perfil(ciencia_de_dados, desenvolvimento_frontend, 1).
perfil(ciencia_de_dados, redes, 1).
perfil(ciencia_de_dados, seguranca, 2).

% Redes e Infraestrutura
perfil(redes_e_infraestrutura, redes, 5).
perfil(redes_e_infraestrutura, seguranca, 4).
perfil(redes_e_infraestrutura, raciocinio_logico, 4).
perfil(redes_e_infraestrutura, desenvolvimento_backend, 3).
perfil(redes_e_infraestrutura, matematica, 3).
perfil(redes_e_infraestrutura, estatistica, 2).
perfil(redes_e_infraestrutura, banco_de_dados, 2).
perfil(redes_e_infraestrutura, programacao_funcional, 1).
perfil(redes_e_infraestrutura, programacao_logica, 2).
perfil(redes_e_infraestrutura, desenvolvimento_frontend, 1).
