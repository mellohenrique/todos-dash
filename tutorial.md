---
title: "Tutorial"
author: "Henrique de Assunção"
date: "25/02/2020"
output: html_document
---




<style type="text/css">
           body {          
           max-width:100%;
           padding:0;
           }
</style>

# Tutorial

## Modelos

O _dashboard_ disponibiliza 3 modelos para realizar simulações, tanto em um único período como em vários períodos. A _comparação_ permite comparar dois modelos no tempo. Os modelos disponíveis são:

1. Modelo FUNDEB, modelo atualmente utilizado no financiamento da educação pelo FUNDEB em que os fundos estaduais são complementados pela União de modo a diminuir as desigualdades de valor aluno ano;
2. Modelo VAAT, modelo alternativo que complementa os recursos totais da educação em estados e municípios, individualmente, de modo a reduzir a desigualdade do valor aluno ano entre os entes;
3. Modelo Híbrido, modelo que combina os modelo VAT e FUNDEB.

## Tabelas de _Inputs_

O usuário pode inserir tabelas com dados de alunos, peso de alunos por etapa de ensino, informações socioeconômicas e informações financeiras, ou utilizar os dados para o ano de 2015 providos pelo Todos pela Educação. Caso não se coloque nenhuma tabela, a simulação será feita com os dados de 2015. As tabelas utilizadas como _input_ devem seguir a seguinte especificação:

> **_Aviso:_**  É importante que as tabelas sejam disponibilizadas em formato CSV padrão brasileiro (colunas separadas por ";", no R esse formato é chamado de csv2).

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Exemplo de tabela de alunos, 8 primeiras linhas</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> ibge </th>
   <th style="text-align:left;"> etapa </th>
   <th style="text-align:right;"> alunos </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> Creche pública - integral </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> Creche pública - parcial </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> Pré-escola pública - integral </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> Pré-escola pública - parcial </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> EF-1 urbano parcial público </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> EF-1 rural parcial público </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> EF-2 urbano parcial  público </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:left;"> EF-2 rural parcial  público </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Exemplo de pesos das etapas, 8 primeiras linhas</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> etapa </th>
   <th style="text-align:right;"> peso </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Creche pública - integral </td>
   <td style="text-align:right;"> 1.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Creche pública - parcial </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pré-escola pública - integral </td>
   <td style="text-align:right;"> 1.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pré-escola pública - parcial </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EF-1 urbano parcial público </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EF-1 rural parcial público </td>
   <td style="text-align:right;"> 1.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EF-2 urbano parcial  público </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EF-2 rural parcial  público </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Exemplo de tabela de financas</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> ibge </th>
   <th style="text-align:right;"> fundeb </th>
   <th style="text-align:right;"> demais_receitas </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 190001 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 290001 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Exemplo de tabela de dados socioeconômicos</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> ibge </th>
   <th style="text-align:right;"> nse </th>
   <th style="text-align:right;"> idhm </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 190001 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 290001 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
</tbody>
</table>

## Opções

Todos os modelos apresentam opções parâmetros customizáveis que permitem ao usuário flexibilidade na análise do financiamento. Certos parâmetros são comuns a todas as funções enquanto outros são específicos. Todas as funções apresentam os seguintes parâmetros:

* **complem_uniao**: percentual do fundo (somatório dos fundos estaduais) que a União complementará;
,* **complem_uniao_vat**: percentual do fundo (somatório dos fundos estaduais) que a União complementará na segunda etapa de complementação do modelo VAT;
* **crescimento_economico**: vetor númerico de crescimento econômico, especificamente do FUNDEB e das demais receitas. O primeiro valor representa a taxa de crescimento do primeiro ano em relação aos dados de finanças colocados como _input_, o segundo valor representa a taxa de crescimento em relação aos valores de finanças do primeiro ano, e assim sucessivamente;
* **crescimento_demografico**: vetor numérico de crescimento demográfico para alunos.
* **fatores_equidade**: parâmetro lógico que controla se a equalização do fundo considerara o vetor de alunos ponderados por etapa ou de alunos ponderados por etapa e informações socioeconomicas;
* **fatores_intra_equidade**: parâmetro logico que controla se a distribuicao do fundo estadual considerara o vetor de alunos ponderados por etapa ou de alunos ponderados por etapa e informações socioeconomicas;
* **min_social**: peso mínimo dado a informação socioeconômica;
* **max_social**: peso máximo dado a informação socioeconômica;
* **min_disp_fiscal**: peso mínimo dado a informação de finanças;
* **max_disp_fiscal**: peso máximo dado a informação de finanças.

> **_Aviso:_**  É importante que as variáveis **complem_uniao**, **complem_uniao_vat**, **crescimento_economico** e **crescimento_demografico** possua o mesmo tamanho.
