% O jogo
 vitoria(Jogador, Tabuleiro) :-
    Tabuleiro = [Jogador, Jogador, Jogador, _, _, _, _, _, _];
    Tabuleiro = [_, _, _, Jogador, Jogador, Jogador, _, _, _];
    Tabuleiro = [_, _, _, _, _, _, Jogador, Jogador, Jogador];
    Tabuleiro = [Jogador, _, _, Jogador, _, _, Jogador, _, _];
    Tabuleiro = [Jogador, _, _, _, Jogador, _, _, _, Jogador];
    Tabuleiro = [_, Jogador, _, _, Jogador, _, _, Jogador, _];
    Tabuleiro = [_, _, Jogador, _, Jogador, _, Jogador, _, _];
    Tabuleiro = [_, _, Jogador, _, _, Jogador, _, _, Jogador].

%jogo empatado
empate(Tabuleiro) :-
    maplist(ground, Tabuleiro),
     \+ member('_', Tabuleiro).

% Verificar se o jogo terminou (vitória ou empate)
jogo_terminou_vitoria(Jogador, Tabuleiro) :-
    vitoria(Jogador, Tabuleiro).

jogo_terminou_empate(Tabuleiro) :-
    \+ vitoria(_, Tabuleiro),        % Nenhuma vitória para nenhum jogador
    maplist(ground, Tabuleiro),      % Todos os elementos do tabuleiro estão instanciados
    \+ member('_', Tabuleiro).       % Nao existe elementos vazios

%jogo continua
jogo_continua(Tabuleiro) :- \+ jogo_terminou_empate( Tabuleiro).


% Verificar se uma jogada é válida em uma determinada posição
jogada_valida(Posicao, Tabuleiro) :-
    nth0(Posicao, Tabuleiro, Elemento),
    vazio(Elemento),
    \+ (Elemento = 'X' ; Elemento = 'O').

vazio(Elemento) :- var(Elemento).
vazio(Elemento) :- Elemento = '_'.


substituir([_|T], 0, Jogador, [Jogador|T]).

substituir([H|T], I, Jogador, [H|R]) :-
    I > 0,
    I1 is I - 1,
    substituir(T, I1, Jogador, R).


substituir_elemento(Lista, 0, Elemento, [Elemento|Resto]) :-
    Lista = [_|Resto].

substituir_elemento([X|Resto1], Indice, Elemento, [X|Resto2]) :-
    Indice > 0,
    I is Indice - 1,
    substituir_elemento(Resto1, I, Elemento, Resto2).


outro_jogador('X','O').
outro_jogador('O','X').



% Predicado para encontrar uma próxima boa jogada para o jogador que
% leve a vitoria
proximo_movimento(Jogador, Tabuleiro, ProximaJogada, Resultado) :-
    between(0, 8, ProximaJogada), % Iterar sobre as posições do tabuleiro
    jogada_valida(ProximaJogada, Tabuleiro),

    % Simular a jogada e verificar se ela leva à vitória
    copy_term(Tabuleiro, NovoTabuleiro),  % Copiar o tabuleiro para simular a jogada
    copy_term(Tabuleiro, NovoTabuleiro, _),
    substituir_elemento(NovoTabuleiro, ProximaJogada, Jogador, NovoTabuleiroModificado),

       % Verificar se a jogada leva à vitória para o jogador
    jogo_terminou_vitoria(Jogador, NovoTabuleiroModificado) ,
    Resultado is 2,

    %Parar se encontrar uma boa jogada
    !.

%Predicado para impedir derrota
proximo_movimento(Jogador, Tabuleiro, ProximaJogada, Resultado) :-
    outro_jogador(Jogador, Adversario),
    between(0, 8, ProximaJogada), % Iterar sobre as posições do tabuleiro
    jogada_valida(ProximaJogada, Tabuleiro),

    % Simular a jogada e verificar se ela leva à vitória
    copy_term(Tabuleiro, NovoTabuleiro),  % Copiar o tabuleiro para simular a jogada
    copy_term(Tabuleiro, NovoTabuleiro, _),
    substituir_elemento(NovoTabuleiro, ProximaJogada, Adversario, NovoTabuleiroModificado),

       % Verificar se a jogada leva à vitória para o jogador
    jogo_terminou_vitoria(Adversario, NovoTabuleiroModificado) ,
    Resultado is 1,

    %Parar se encontrar uma boa jogada
    !.

%Para dar continuidade no jogo
%proximo_movimento(Jogador, Tabuleiro, ProximaJogada, Resultado) :-
 %   between(0, 8, ProximaJogada), % Iterar sobre as posições do tabuleiro
  %  jogada_valida(ProximaJogada, Tabuleiro),

    % Simular a jogada e verificar se ela leva à vitória
   % copy_term(Tabuleiro, NovoTabuleiro),  % Copiar o tabuleiro para simular a jogada
    %copy_term(Tabuleiro, NovoTabuleiro, _),
    %substituir_elemento(NovoTabuleiro, ProximaJogada, Jogador, NovoTabuleiroModificado),

       % Verificar se a jogada leva à vitória para o jogador
  % \+ jogo_terminou_vitoria(Jogador, NovoTabuleiroModificado) ,
   %melhor_continuar(Jogador, NovoTabuleiroModificado),
  % Resultado is 0,

    %Parar se encontrar uma boa jogada
   % !.

proximo_movimento(Jogador, Tabuleiro, ProximaJogada, Resultado) :-
    % Criar uma lista de posições disponíveis no tabuleiro
    findall(Posicao, (between(0, 8, Posicao), jogada_valida(Posicao, Tabuleiro)), PosicoesDisponiveis),

    % Embaralhar aleatoriamente as posições disponíveis
    random_permutation(PosicoesDisponiveis, PosicoesEmbaralhadas),

    % Testar cada posição disponível em ordem aleatória
    testar_posicoes_continuar(Jogador, Tabuleiro, PosicoesEmbaralhadas, ProximaJogada, Resultado).

% Predicado auxiliar para testar cada posição disponível
testar_posicoes_continuar(_, _, [], _, _) :- !, fail. % Falha se não houver mais posições para testar
testar_posicoes_continuar(Jogador, Tabuleiro, [Posicao | Resto], ProximaJogada, Resultado) :-
    % Simular a jogada na posição atual
    copy_term(Tabuleiro, NovoTabuleiro),
    substituir_elemento(NovoTabuleiro, Posicao, Jogador, NovoTabuleiroModificado),

       (  melhor_continuar(Jogador, NovoTabuleiroModificado) ->
        ProximaJogada = Posicao,
        Resultado = 0
    ;   testar_posicoes_continuar(Jogador, Tabuleiro, Resto, ProximaJogada, Resultado)
    ).


%melhor jogada
melhor_jogada(Jogador, Tabuleiro, MelhorJogada) :-
    ( proximo_movimento(Jogador, Tabuleiro, MelhorJogada, 2), !
    ; proximo_movimento(Jogador, Tabuleiro, MelhorJogada, 1), !
    ; proximo_movimento(Jogador, Tabuleiro, MelhorJogada, 0)
    ).

continua(Jogador, Tabuleiro, R) :-
    proximo_movimento(Jogador, Tabuleiro,_,2),
    R is 2.
continua(Jogador, Tabuleiro, R) :-
    jogo_terminou_empate(Tabuleiro),
    R is 0.
continua(Jogador, Tabuleiro, R) :-
    jogo_continua(Tabuleiro),
    R is 1.

melhor_continuar(Jogador, Tabuleiro) :-
    ( continua(Jogador, Tabuleiro, 2), ! ;
      continua(Jogador, Tabuleiro, 1), ! ;
      continua(Jogador, Tabuleiro, 0)).

