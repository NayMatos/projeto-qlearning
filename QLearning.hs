-- =============================================================================
-- Q-LEARNING CLASSICO EM HASKELL
-- Disciplina: Reinforcement Learning
-- Paradigma: Programacao Funcional Pura com Monada IO isolada na borda
-- VERSAO SEM DEPENDENCIAS EXTERNAS: apenas base + Data.Map
-- VERSAO ASCII: compativel com terminais Windows/MINGW64
-- =============================================================================

module Main where

import qualified Data.Map.Strict as Map  -- Tabela Q como Map imutavel
import Data.Map.Strict (Map)             -- Tipo Map para assinaturas
import Data.List (maximumBy)             -- Para encontrar a acao de maior valor
import Data.Ord (comparing)             -- Comparador para maximumBy
import Data.Maybe (fromMaybe)            -- Para valores padrao em lookups

-- =============================================================================
-- SECAO 0: GERADOR DE NUMEROS PSEUDOALEATORIOS (LCG)
-- =============================================================================
-- Um LCG (Linear Congruential Generator) gera sequencias pseudoaleatorias
-- usando a formula: s' = (a * s + c) mod m
-- Por que fizemos isso na mão? Para isolar efeitos colaterais.
-- Em vez de acionar o sistema operacional (o que exigiria a Mônada IO), 
-- passamos a semente matemática adiante como um bastão em uma corrida de revezamento.
-- Isso mantém o núcleo do Reinforcement Learning 100% puro e previsível.
-- =============================================================================

-- | O gerador e simplesmente um inteiro (a semente atual).
newtype Gen = Gen Int deriving (Show)

-- | Cria um gerador a partir de uma semente inteira.
mkGen :: Int -> Gen
mkGen semente = Gen semente

-- | Avanca o gerador para o proximo estado usando a formula LCG.
-- Constantes identicas as usadas pela biblioteca C padrao (glibc).
avancarGen :: Gen -> Gen
avancarGen (Gen s) =
  let a = 1103515245  -- Multiplicador
      c = 12345       -- Incremento
      m = 2147483647  -- Modulo (2^31 - 1)
  in Gen ((a * s + c) `mod` m)

-- | Gera um Double no intervalo [0.0, 1.0) e retorna o gerador atualizado.
gerarDouble :: Gen -> (Double, Gen)
gerarDouble gen =
  let gen'  = avancarGen gen
      Gen s = gen'
      valor = fromIntegral (abs s) / 2147483647.0
  in (valor, gen')

-- | Gera um Int no intervalo [lo, hi] (inclusive) e retorna o gerador atualizado.
gerarInt :: Int -> Int -> Gen -> (Int, Gen)
gerarInt lo hi gen =
  let gen'  = avancarGen gen
      Gen s = gen'
      valor = lo + (abs s `mod` (hi - lo + 1))
  in (valor, gen')

-- =============================================================================
-- SECAO 1: DEFINICAO DE TIPOS
-- =============================================================================

-- | Um Estado representa a posicao do agente no grid (linha, coluna).
type Estado = (Int, Int)

-- | As quatro acoes possiveis que o agente pode executar a cada passo.
data Acao
  = Cima     -- Move uma linha para cima
  | Baixo    -- Move uma linha para baixo
  | Esquerda -- Move uma coluna para a esquerda
  | Direita  -- Move uma coluna para a direita
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Recompensa e um numero de ponto flutuante.
type Recompensa = Double

-- | A Tabela Q mapeia cada par (Estado, Acao) para um valor Double.
-- Quanto maior o valor Q, melhor e tomar aquela acao naquele estado.
type TabelaQ = Map (Estado, Acao) Double

-- | Lista de todas as acoes, gerada via Enum e Bounded automaticamente.
todasAcoes :: [Acao]
todasAcoes = [minBound .. maxBound]

-- =============================================================================
-- SECAO 2: DEFINICAO DO AMBIENTE
-- Grid 4x4: agente comeca em (0,0) e deve chegar em (3,3).
-- =============================================================================

tamanhoGrid :: Int
tamanhoGrid = 4

estadoObjetivo :: Estado
estadoObjetivo = (3, 3)

estadoInicial :: Estado
estadoInicial = (0, 0)

-- | Paredes: celulas que o agente nao pode ocupar.
paredes :: [Estado]
paredes = [(1, 1), (2, 1), (1, 2)]

-- | Verifica se um estado e valido: dentro do grid E nao e parede.
estadoValido :: Estado -> Bool
estadoValido (r, c) =
  r >= 0 && r < tamanhoGrid
  && c >= 0 && c < tamanhoGrid
  && (r, c) `notElem` paredes

-- | Aplica uma acao ao estado atual. Se o resultado for invalido,
-- o agente permanece no mesmo lugar (bate na parede e volta).
transicao :: Estado -> Acao -> Estado
transicao (r, c) acao =
  let candidato = case acao of
        Cima     -> (r - 1, c)
        Baixo    -> (r + 1, c)
        Esquerda -> (r, c - 1)
        Direita  -> (r, c + 1)
  in if estadoValido candidato then candidato else (r, c)

-- | Recompensa recebida ao entrar em um estado.
recompensa :: Estado -> Recompensa
recompensa estado
  | estado == estadoObjetivo = 100.0  -- Premio ao chegar no objetivo
  | otherwise                = -1.0   -- Punicao por cada passo dado

-- =============================================================================
-- SECAO 3: HIPERPARAMETROS
-- =============================================================================

taxaAprendizado :: Double
taxaAprendizado = 0.1   -- alpha: quanto o agente atualiza seus valores

fatorDesconto :: Double
fatorDesconto = 0.9     -- gamma: quanto o agente valoriza recompensas futuras

epsilon :: Double
epsilon = 0.2           -- 20% chance de explorar, 80% de explotar

numEpisodios :: Int
numEpisodios = 2000

maxPassos :: Int
maxPassos = 200

-- =============================================================================
-- SECAO 4: FUNCOES PURAS DA TABELA Q
-- =============================================================================

-- | Inicializa a Tabela Q com todos os valores em 0.0.
inicializarTabelaQ :: TabelaQ
inicializarTabelaQ =
  Map.fromList
    [ ((estado, acao), 0.0)
    | r    <- [0 .. tamanhoGrid - 1]
    , c    <- [0 .. tamanhoGrid - 1]
    , let estado = (r, c)
    , acao <- todasAcoes
    ]

-- | Consulta o valor Q de um par (Estado, Acao). Retorna 0.0 se nao existir.
obterQ :: TabelaQ -> Estado -> Acao -> Double
obterQ tabelaQ estado acao =
  fromMaybe 0.0 (Map.lookup (estado, acao) tabelaQ)

-- | Retorna o maior valor Q entre todas as acoes possiveis num estado.
maxQ :: TabelaQ -> Estado -> Double
maxQ tabelaQ estado =
  maximum [obterQ tabelaQ estado acao | acao <- todasAcoes]

-- | Retorna a acao com maior valor Q para um estado (politica gulosa/greedy).
melhorAcao :: TabelaQ -> Estado -> Acao
melhorAcao tabelaQ estado =
  maximumBy (comparing (obterQ tabelaQ estado)) todasAcoes

-- | Atualiza a Tabela Q usando a Equacao de Bellman.
--
--   Q(s,a) <- Q(s,a) + alpha * [ r + gamma * max Q(s',a') - Q(s,a) ]
--
-- Lógica para a arguição: 
-- Nova Nota = Nota Antiga + Taxa de Aprendizado * (Recompensa + Visão de Futuro - Nota Antiga)
-- O termo entre colchetes é o "Erro": a diferença entre o que o agente esperava e o que aconteceu.
-- Retorna uma NOVA TabelaQ, garantindo a precisão técnica da imutabilidade no Haskell.

atualizarQ :: TabelaQ -> Estado -> Acao -> Recompensa -> Estado -> TabelaQ
atualizarQ tabelaQ estado acao recomp proximoEstado =
  let qAtual       = obterQ tabelaQ estado acao
      melhorFuturo = maxQ  tabelaQ proximoEstado
      novoValorQ   = qAtual + taxaAprendizado
                       * (recomp + fatorDesconto * melhorFuturo - qAtual)
  in Map.insert (estado, acao) novoValorQ tabelaQ

-- =============================================================================
-- SECAO 5: SELECAO DE ACAO (POLITICA EPSILON-GREEDY)
-- =============================================================================

-- | Seleciona uma acao usando epsilon-greedy com o gerador LCG puro.
-- Retorna a acao escolhida E o gerador avancado.
selecionarAcao :: TabelaQ -> Estado -> Gen -> (Acao, Gen)
selecionarAcao tabelaQ estado gen =
  let (valorD, gen1) = gerarDouble gen                      -- Sorteia [0.0, 1.0)
      (indice, gen2) = gerarInt 0 (length todasAcoes - 1) gen1
      acaoAleatoria  = todasAcoes !! indice                 -- Acao aleatoria
      acaoGulosa     = melhorAcao tabelaQ estado            -- Melhor acao conhecida
  in if valorD < epsilon
     then (acaoAleatoria, gen2)   -- EXPLORACAO: tenta algo novo
     else (acaoGulosa,    gen1)   -- EXPLOTACAO: usa o melhor conhecido

-- =============================================================================
-- SECAO 6: LOOP DE UM EPISODIO (RECURSAO PURA)
-- =============================================================================
-- Em Haskell, não existem laços "for" ou "while". 
-- O agente treina chamando a própria função continuamente (recursão).
-- A cada passo, em vez de alterar a Tabela Q existente (o que quebraria a imutabilidade),
-- a função entrega uma Tabela Q inteiramente nova e atualizada para o próximo passo.
-- =============================================================================

-- | Executa um episodio completo de forma recursiva e pura.
executarEpisodio :: TabelaQ -> Estado -> Gen -> Int -> (TabelaQ, Gen)

-- CASO BASE 1: passos esgotados
executarEpisodio tabelaQ _ gen 0 = (tabelaQ, gen)

-- CASO BASE 2 + CASO RECURSIVO
executarEpisodio tabelaQ estadoAtual gen passosRestantes
  | estadoAtual == estadoObjetivo = (tabelaQ, gen)   -- Chegou! Para aqui.
  | otherwise =
      let (acao, gen1) = selecionarAcao tabelaQ estadoAtual gen
          proximo      = transicao estadoAtual acao
          recomp       = recompensa proximo
          novaTabela   = atualizarQ tabelaQ estadoAtual acao recomp proximo
      in executarEpisodio novaTabela proximo gen1 (passosRestantes - 1)

-- =============================================================================
-- SECAO 7: LOOP DE TREINAMENTO COM IO
-- O nucleo puro (executarEpisodio) faz o trabalho; IO so exibe o progresso.
-- =============================================================================

-- | Treina por N episodios, exibindo progresso a cada 500.
treinarComIO :: TabelaQ -> Gen -> Int -> Int -> IO TabelaQ

-- CASO BASE: todos os episodios concluidos
treinarComIO tabelaQ _ 0 _ = return tabelaQ

-- CASO RECURSIVO: executa um episodio e continua
treinarComIO tabelaQ gen episodiosRestantes totalEpisodios = do
  let episodioAtual     = totalEpisodios - episodiosRestantes + 1
      (novaTabela, novoGen) =
        executarEpisodio tabelaQ estadoInicial gen maxPassos

  if episodioAtual `mod` 500 == 0 || episodioAtual == totalEpisodios
    then do
      let pct = (episodioAtual * 100) `div` totalEpisodios
      putStrLn $ "  [Episodio " ++ show episodioAtual
               ++ "/" ++ show totalEpisodios
               ++ "] " ++ show pct ++ "% concluido"
    else return ()

  treinarComIO novaTabela novoGen (episodiosRestantes - 1) totalEpisodios

-- =============================================================================
-- SECAO 8: VISUALIZACAO DA POLITICA OTIMA (100% ASCII)
-- =============================================================================

-- | Converte acao para seta ASCII (compativel com qualquer terminal).
simboloAcao :: Acao -> String
simboloAcao Cima     = " ^^ "   -- Cima
simboloAcao Baixo    = " vv "   -- Baixo
simboloAcao Esquerda = " << "   -- Esquerda
simboloAcao Direita  = " >> "   -- Direita

-- | Exibe o grid com a politica aprendida.
exibirGrid :: TabelaQ -> IO ()
exibirGrid tabelaQ = do
  putStrLn "\n+================================+"
  putStrLn   "|   POLITICA OTIMA APRENDIDA     |"
  putStrLn   "+================================+"
  putStrLn "  S=Inicio  G=Objetivo  #=Parede"
  putStrLn "  ^=Cima  v=Baixo  <=Esquerda  >=Direita\n"
  putStrLn " +-----+-----+-----+-----+"
  mapM_ (exibirLinhaGrid tabelaQ) [0 .. tamanhoGrid - 1]

exibirLinhaGrid :: TabelaQ -> Int -> IO ()
exibirLinhaGrid tabelaQ r = do
  let celulas = map (conteudoCelula tabelaQ r) [0 .. tamanhoGrid - 1]
  putStrLn $ " |" ++ concatMap (++ "|") celulas
  if r < tamanhoGrid - 1
    then putStrLn " +-----+-----+-----+-----+"
    else putStrLn " +-----+-----+-----+-----+"

conteudoCelula :: TabelaQ -> Int -> Int -> String
conteudoCelula tabelaQ r c
  | (r, c) == estadoInicial  = "  S  "
  | (r, c) == estadoObjetivo = "  G  "
  | (r, c) `elem` paredes    = "  #  "
  | otherwise                = simboloAcao (melhorAcao tabelaQ (r, c))

-- | Exibe os valores Q do estado inicial para todas as acoes.
exibirValoresQ :: TabelaQ -> IO ()
exibirValoresQ tabelaQ = do
  putStrLn "\n+======================================+"
  putStrLn   "|  VALORES Q DO ESTADO INICIAL (0,0)  |"
  putStrLn   "+======================================+"
  mapM_ exibirUm todasAcoes
  where
    exibirUm acao =
      let v    = obterQ tabelaQ estadoInicial acao
          vStr = show (fromIntegral (round (v * 100) :: Int) / 100.0 :: Double)
      in putStrLn $ "  " ++ show acao ++ ":\t" ++ vStr

-- | Exibe a politica completa: para cada estado, qual a melhor acao.
exibirPoliticaCompleta :: TabelaQ -> IO ()
exibirPoliticaCompleta tabelaQ = do
  putStrLn "\n+============================+"
  putStrLn   "|    POLITICA COMPLETA       |"
  putStrLn   "+============================+"
  mapM_ exibirEstado navegaveis
  where
    navegaveis =
      [ (r, c)
      | r <- [0 .. tamanhoGrid - 1]
      , c <- [0 .. tamanhoGrid - 1]
      , (r, c) `notElem` paredes
      , (r, c) /= estadoObjetivo
      ]
    exibirEstado est =
      let acao = melhorAcao tabelaQ est
      in putStrLn $ "  Estado " ++ show est
                  ++ " -> " ++ show acao
                  ++ " " ++ simboloAcao acao

-- =============================================================================
-- SECAO 9: FUNCAO MAIN
-- =============================================================================

main :: IO ()
main = do
  putStrLn "+==========================================+"
  putStrLn "|   Q-LEARNING CLASSICO EM HASKELL        |"
  putStrLn "|   Apenas base + Data.Map (sem deps ext.) |"
  putStrLn "+==========================================+\n"

  putStrLn "[ AMBIENTE ]"
  putStrLn $ "  Grid: " ++ show tamanhoGrid ++ "x" ++ show tamanhoGrid
  putStrLn $ "  Inicio:   " ++ show estadoInicial
  putStrLn $ "  Objetivo: " ++ show estadoObjetivo
  putStrLn $ "  Paredes:  " ++ show paredes

  putStrLn "\n[ HIPERPARAMETROS ]"
  putStrLn $ "  alpha (taxa aprendizado): " ++ show taxaAprendizado
  putStrLn $ "  gamma (fator desconto):   " ++ show fatorDesconto
  putStrLn $ "  epsilon (exploracao):     " ++ show epsilon
  putStrLn $ "  Episodios:                " ++ show numEpisodios
  putStrLn $ "  Passos maximos/episodio:  " ++ show maxPassos

  let tabelaInicial = inicializarTabelaQ  -- Tabela Q zerada (mundo puro)
      gen           = mkGen 42            -- Semente fixa -> resultados reproduziveis

  putStrLn "\n[ TREINAMENTO ]"
  tabelaFinal <- treinarComIO tabelaInicial gen numEpisodios numEpisodios
  putStrLn "  Treinamento concluido!\n"

  exibirValoresQ tabelaFinal
  exibirGrid tabelaFinal
  exibirPoliticaCompleta tabelaFinal

  putStrLn "\n[ INTERPRETACAO DOS RESULTADOS ]"
  putStrLn "  Setas no grid = melhor acao aprendida em cada posicao"
  putStrLn "  Valores Q altos em Direita/Baixo em (0,0) = aprendizado correto"
  putStrLn "  A politica representa o conhecimento acumulado pelo agente"
  putStrLn "\n+==========================================+"
  putStrLn "|   Q-Learning finalizado. Bem-vinda ao RL! |"
  putStrLn "+==========================================+"
