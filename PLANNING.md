# Planning

## Tasks

- Vini (KB & Inference Engine):
  - [ ] Model the knowledge base: trilhas, perfis + pesos, perguntas.
  - [ ] Implement the inference kernel (pontuação, ranking e recomendação)
  - [ ] ADD COMMENTS BRUV
- G (UX/FLow, Tests, Repo/Docs):
  - [ ] Build CLI predicates (perguntas, leitura/validação s/n, armazenamento com `assertz/1`)
  - [ ] Control the program flow (hellyeah)
  - [ ] Create the automated tests (`perfil_X.pl` files); wire `consult/1` execution for batch tests.
  - [ ] Set up GitHub repo, README.md, project structure, and tidy commit/PR workflow.

## File Structure

```
ra1-prolog/
├─ src/
│  ├─ base_conhecimento.pl (spec-required name)
│  ├─ motor_inferencia.pl (contains calcula_pontuacao/3, recomenda/2, etc.)
│  ├─ interface.pl (iniciar/0, faz_perguntas/0, exibe_resultado/1, I/O, assertz)
│  └─ main.pl (loads modules, entry points, test runner)
├─ tests/
│  ├─ perfil_teste_1.pl (spec: 3+ automated profiles)
│  ├─ perfil_teste_2.pl
│  ├─ perfil_teste_3.pl
│  └─ expect/ (optional: expected results notes)
├─ scripts/
│  ├─ run_interactive.sh swipl -s src/main.pl -g iniciar -t halt
│  └─ run_test.sh swipl -s src/main.pl -g run_tests -t halt
└─ README.md
```
