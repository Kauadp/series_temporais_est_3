# series_temporais_est_3

Projeto de análise e modelagem de séries temporais usado no Trabalho Est 3.

## Descrição
Este repositório contém um script em R (`main.R`) com análises exploratórias e decomposição de uma série temporal de temperatura média. O script espera um arquivo de dados em `dados/treino.csv` e gera gráficos e decomposições (usando funções de pacotes do tidyverse e forecast). Também há um arquivo PDF (`Trabalho Est 3.pdf`) com o relatório do trabalho.

## Conteúdo importante
- `main.R` — script principal que realiza leitura dos dados, plots e decomposição da série.
  - Pacotes utilizados (ver `main.R`): ggplot2, ggthemes, gridExtra, tidyverse, forecast.
- `dados/` — diretório com os dados. O script lê `dados/treino.csv`.
- `Trabalho Est 3.pdf` — relatório do trabalho.

## Pré-requisitos
- R >= 4.0 (recomenda-se usar RStudio)
- Internet para instalar pacotes, caso não estejam presentes

## Instalação rápida (R)
Abra o R/RStudio na raiz do projeto e execute:

```r
# instalar pacotes necessários (se ainda não instalados)
required <- c("ggplot2","ggthemes","gridExtra","tidyverse","forecast")
to_install <- required[!(required %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
```

Ou instalar manualmente:
```r
install.packages(c("ggplot2","ggthemes","gridExtra","tidyverse","forecast"))
```

## Como rodar
1. Garanta que o arquivo `dados/treino.csv` esteja presente e contém pelo menos:
   - Uma coluna `date` (formato compatível com `as.Date()`).
   - Uma coluna de temperatura média chamada `meantemp` (usada no script).
2. Rodar interativamente no RStudio: abra `main.R` e execute as células/linhas.
3. Rodar pela linha de comando:
```bash
Rscript main.R
```
(Execute a partir da raiz do repositório para que o caminho `dados/treino.csv` seja encontrado.)

## O que o script faz (resumo)
- Lê `dados/treino.csv`.
- Converte a coluna `date` para `Date`.
- Gera gráficos de densidade e séries temporais (ggplot2).
- Converte a série para um objeto `ts` com frequência anual (365.25) e faz decomposição aditiva (`decompose`).
- Plota a decomposição.


## Contato
- Autor: Kauadp — https://github.com/Kauadp

---