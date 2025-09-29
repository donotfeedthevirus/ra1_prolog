# Script para rodar os testes sem ter que adicionar muitos argumentos

#!/usr/bin/env bash
set -euo pipefail
swipl -q -s src/main.pl -g run_tests -t halt
