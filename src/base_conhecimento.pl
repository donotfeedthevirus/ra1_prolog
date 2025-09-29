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
