# ra1_prolog

> Trabalho prático desenvolvido para a disciplina **Programação Lógica e Funcional**.  
> O sistema implementa um **Orientador de Trilhas em Computação**, que sugere trilhas de estudo com base nas respostas do usuário.

---

## Integrantes

- **Gustavo Muniz**
- **Vinicius Marcon**

## 🛠️ Como rodar

Este projeto foi desenvolvido para ser utilizado com **SWI-Prolog**.  
Fornecemos scripts simples para executar tanto o modo interativo quanto os testes automatizados.

### 1. Pré-requisitos

- Ter o **SWI-Prolog** instalado:
  - [Download oficial](https://www.swi-prolog.org/Download.html)

### 2. Executar em modo interativo (CLI)

```bash
./scripts/run_interactive.sh
```

O programa fará uma série de perguntas (s/n) e ao final recomendará uma ou mais trilhas, junto com as evidências que levaram à recomendação.

### 3. Executar testes automatizados

```bash
./scripts/run_test.sh
```

Isso executa os arquivos `tests/perfil_teste_*.pl`, compara a trilha esperada com a obtida e imprime PASS/FAIL no terminal.

## Estrutura dos Arquivos

- `src/`: código fonte principal
  - `base_conhecimento.pl` — fatos sobre trilhas, características, pesos e perguntas
  - `motor_inferencia.pl` — regras para cálculo de pontuação e recomendação
  - `interface.pl` — interação com o usuário (perguntas e exibição de resultados)
  - `main.pl` — ponto de entrada, inicialização interativa e execução de testes

- `tests/`: perfis de teste automatizados (`perfil_teste_*.pl`)
- `scripts/`: scripts de execução (`run_interactive.sh`, `run_test.sh`)

## Recursos Utilizados

Durante o desenvolvimento, utilizamos como apoio:

- Livro **[Learn Prolog Now](http://www.learnprolognow.org/)** (Clocksin & Mellish)
- Site do professor Frank Alcântara: [frankalcantara.com](https://frankalcantara.com/)
- Documentação oficial do **SWI-Prolog**: [swi-prolog.org](https://www.swi-prolog.org/)
