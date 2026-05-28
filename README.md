# Projeto: Reinforcement Learning em Haskell[cite: 2]

**Tema Escolhido:** 1. Q-Learning (Clássico)

## 🎯 O Objetivo do Projeto
O nosso objetivo foi implementar um agente inteligente (um "robô") que aprende sozinho a encontrar a saída de um labirinto (uma grade 4x4)

O robô começa no estado inicial (0,0) sem saber nada sobre o mundo e precisa chegar ao estado objetivo em (3,3). Ele aprende por tentativa e erro: recebe uma recompensa positiva (+100) quando chega ao objetivo e pequenas penalizações (-1) (custo de vida) por cada passo extra que dá, para aprender a encontrar o caminho mais curto. Tudo isso foi desenvolvido respeitando a precisão técnica e a pureza da linguagem Haskell (sem alterar o valor de variáveis após serem definidas).

## 🧠 Como o Robô Aprende (A Lógica)
1. **A Tabela Q:** O robô tem um "bloco de notas" (Tabela Q) onde aponta uma nota para cada direção que toma numa determinada casa do tabuleiro.
2. **Exploração vs. Aproveitamento (Epsilon-Greedy):** Em 80% do tempo, o robô usa o seu bloco de notas para dar o melhor passo possível. Nos outros 20% do tempo (epsilon = 0.2), ele arrisca um passo aleatório para tentar descobrir caminhos novos e melhores.
3. **A Matemática (Equação de Bellman):** O aprendizado considera uma Taxa de Aprendizado (Alpha) de 0.1 e um peso para a Visão de Futuro (Gama) de 0.9 ao longo de um treinamento de 2000 episódios.
4. **Imutabilidade:** Como o Haskell não permite apagar e reescrever variáveis, a cada novo passo o robô gera um bloco de notas inteiramente novo e atualizado.

## 🚀 Como executar o projeto
Para testar e ver o robô treinar, siga estes passos:
1. Abra o terminal integrado no VS Code.
2. Inicie o compilador interativo do Haskell com o comando: `ghci`.
3. Carregue o arquivo do projeto com o comando: `:l QLearning.hs`.
4. Escreva `main` e pressione **Enter** para iniciar o treino.
5. No final, o terminal irá imprimir o labirinto desenhado com setas, mostrando o caminho exato que o robô aprendeu.

## 👥 Equipe e Divisão de Tarefas
O projeto foi desenvolvido por uma equipe de 7 pessoas[cite: 1], com a seguinte divisão para garantir a organização funcional do código:
* **Débora Ferreira (O Mundo):** Responsável por criar as regras do labirinto, onde ficam as paredes e os tipos de dados do Estado e da Ação.
* **Laisa Rodrigues (A Matemática):** Responsável por traduzir a fórmula do Q-Learning para o código, ensinando o robô a calcular a melhor nota para a Tabela Q.
* **Gabriel Ramos (O Treino):** Responsável por criar o ciclo de repetição (usando recursividade, já que o Haskell não tem ciclos `for` ou `while`) para fazer o robô jogar milhares de episódios.
* **Nayara Matos (Documentação, Testes e GitHub):** Responsável por redigir este documento didático, testar a compilação no GHCi, garantir a formatação visual do labirinto no terminal, realizar os commits e subir o projeto para o GitHub.
* **Apresentação do Projeto:** A defesa e apresentação da conexão entre IA e Haskell ficaram a cargo de Kethleen Rosa, Rafael Souza, Angelo Bruno e Nayara Matos.