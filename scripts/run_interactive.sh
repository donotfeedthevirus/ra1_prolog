# Script para rodar o programa sem ter que adicionar muitos argumentos

#!/usr/bin/env bash
set -euo pipefail
swipl -q -s src/main.pl -g iniciar -t halt
