
%Necessidades
:- op( 900,xfy,'::' ).
:- dynamic estafeta/3.
:- dynamic entrega/7.
:- dynamic rua/4.
:- dynamic cliente/4.
:- dynamic encomenda/6.


:- style_check(-singleton).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

%Estafeta(id_estafeta,nome,idade)

estafeta(01 ,joao_vieira ,20).
estafeta(02 ,francisco_novo ,21).
estafeta(03 ,tiago_ribeiro ,20).
estafeta(05 ,diana_pinto ,22).
estafeta(06 ,henrique_lopes ,31).
estafeta(07 ,paulo_oliveira ,18).
estafeta(08 ,francisco_izquierdo ,22).
estafeta(09 ,duarte_lucas ,28).
estafeta(10 ,pedro_magalhaes ,29).
estafeta(11 ,carlos_dias ,22).
estafeta(12 ,daniel_novo ,19).
estafeta(13 ,joao_goncalves ,25).
estafeta(14 ,beatriz_leite ,24).
estafeta(15 ,catia_pereira ,21).
estafeta(16 ,joao_torres ,61).
estafeta(17 ,maria_aurora, 18).
estafeta(18 ,vitorino_donald, 35).


% Cliente ----------------------------------------

%Cliente(id_cliente,nome,idade,Nome_Rua).

cliente(01 ,paulo_rodrigues ,20 ,jose_maria_ottoni).
cliente(02 ,carolina_sousa ,26 ,avenida_5_de_outubro).
cliente(03 ,hugo_machado ,32 ,rua_central).
cliente(04 ,clara_teixeira ,23 ,santa_luzia).
cliente(05 ,joana_rua ,43 ,sao_jorge).
cliente(06 ,flavia_araujo,22,duque_de_caxias).
cliente(07 ,joana_rua ,23 ,projetada).
cliente(08 ,ricardo_guimares ,26 ,rui_barbosa).
cliente(09 ,margarida_santos ,31 ,santa_catarina).
cliente(10 ,joao_gabriel ,33 ,boa_vista).
cliente(11 ,joel_braga ,26 ,sao_luis).
cliente(12 ,joao_tomas ,32 ,vinte_e_quatro).
cliente(13 ,leonardo_freitas ,21 ,sete_ceus).
cliente(14 ,ricardo_ferreira ,18 ,beco_belo).
cliente(15 ,helder_gomes ,26 ,amorosa).
cliente(16 ,david_goncalves ,39 ,travessa_do_rio).
cliente(17 ,goncalo_rodrigues , 26 , santa_maria).
cliente(18 ,marta_pinto ,29 ,lamelas).
cliente(19 ,joao_ramos , 31 ,bairro_feliz).
cliente(20 ,vasco_moreno ,32 ,augusta).
cliente(21 ,beatriz_morais ,38 ,calcada_santana).
cliente(22 ,andre_carreiras ,21 ,avenida_paz).
cliente(23 ,nuno_valente ,28,rua_barros).
cliente(24 ,bruno_mota ,24,caranda).
cliente(25, jose_pinheiro,27,jose_maria_ottoni).
cliente(26, antonio_manuel,19, jose_maria_ottoni).



% Rua --------------------------------------------

% rua(Nome,Freguesia,Cidade,Distancia)

rua(jose_maria_ottoni ,nogueiro,braga,20).
rua(avenida_5_de_outubro ,serafao,fafe,10). 
rua(rua_central,viade_baixo ,montalegre,50).
rua(santa_luzia,barcelos ,barcelos,15).
rua(sao_jorge,barcelos ,barcelos,35). 
rua(duque_de_caxias ,nogueiro,braga,40).
rua(projetada ,fervidelas,montalegre,65).
rua(rui_barbosa,braga ,braga,25).
rua(santa_catarina,barcelos ,barcelos,15).
rua(boa_vista,santo_tirso ,porto,32).
rua(sao_luis ,nogueiro,braga,25).
rua(vinte_e_quatro ,viade_cima,ruivaes,45).
rua(sete_ceus,viade_baixo ,montalegre,50).
rua(beco_belo,barcelos ,barcelos,23).
rua(amorosa,barcelos ,barcelos,31).
rua(travessa_do_rio ,nogueiro,braga,25).
rua(santa_maria ,serafao,fafe,5).
rua(lamelas,fujacal ,braga,29).
rua(bairro_feliz,praca ,lisboa,320).
rua(augusta,padeirinhos,braga ,18).
rua(calcada_santana,vila_cova,fafe,70).
rua(avenida_paz,vila_moura,algarve,531).
rua(rua_barros, gualtar,braga,6).
rua(caranda,fujacal,braga,18).


% Entrega ---------------------------------------


% entrega(id_entrega,Data,id_encomenda,classificacao,transporte,Preço,Tempo_demorado(horas))

entrega(01 ,date(2020,06,12) ,01 ,4.3 ,mota ,43 ,0.6).
entrega(02 ,date(2021,08,13) ,02 ,5.0 ,bicicleta ,31 ,2).  
entrega(03 ,date(2021,01,23) ,03 ,4.6 ,carro ,52 ,0.5).
entrega(04 ,date(2020,06,12) ,04 ,5.0 ,bicicleta ,74 ,3).
entrega(05 ,date(2021,01,23) ,05 ,4.8 ,mota ,6 ,1).
entrega(06 ,date(2021,04,25) ,06 ,1.5 ,bicicleta , 24 ,8).
entrega(07 ,date(2020,04,12) ,07 ,3.2 ,bicicleta ,43 ,5.8).
entrega(08 ,date(2021,11,13) ,08 ,3.9 ,carro ,28 ,0.25).
entrega(09 ,date(2021,11,23) ,09 ,4.6 ,bicicleta ,52 ,3).
entrega(10 ,date(2020,12,12) ,10 ,4.5 ,carro ,58 ,0.32).
entrega(11 ,date(2021,01,23) ,11 ,4.0 ,carro ,6 ,0.25).
entrega(12 ,date(2021,02,25) ,12 ,4.5 ,mota , 24 ,2.25).
entrega(13 ,date(2020,09,12) ,13 ,4.3 ,mota ,43 ,2.5).
entrega(14 ,date(2021,08,13) ,14 ,3.9 ,bicicleta ,23 ,3).
entrega(15 ,date(2021,04,23) ,15 ,2.7 ,bicicleta ,52 ,6.2).
entrega(16 ,date(2020,06,13) ,16 ,4.5 ,mota ,54 ,1.25).
entrega(17 ,date(2020,06,13) ,17 ,4.9 ,carro ,6 ,0.05).
entrega(18 ,date(2021,04,21) ,18 ,4.5 ,carro , 24 ,0.65).
entrega(19 ,date(2020,03,18) ,19 ,2.5 ,mota ,43 ,9.2).
entrega(20 ,date(2021,08,17) ,20 ,3.9 ,mota ,23 ,0.52).
entrega(21 ,date(2021,01,14) ,21 ,2.0 ,bicicleta ,52 ,14).
entrega(22 ,date(2020,04,15) ,22 ,4.5 ,carro ,54 ,5.31).
entrega(23 ,date(2021,09,19) ,23 ,4.9 ,mota ,6 ,0.2).
entrega(24 ,date(2021,04,29) ,24 ,4.5 ,carro , 24 ,0.25).
entrega(25 ,date(2021,09,31) ,36 ,4.0 ,mota, 33, 0.6).
entrega(26, date(2021,06,18) ,37 ,4.5 ,carro, 21, 0.2).
entrega(27, date(2021,07,16) ,29 ,3.7 ,carro, 33, 0.1).
entrega(28, date(2021,08,19) ,30 ,4.1 ,mota, 19, 0.3).
entrega(29, date(2021,07,18) ,31 ,3.8 ,carro ,28 ,0.2).
entrega(30, date(2021,04,25) ,25 ,3.9 ,mota ,31 ,1.45).


% Encomenda ---------------------------------------

% encomenda(id_encomenda,id_cliente,id_estafeta,prazo_pedido(horas),peso(kg),volume(m3))
        
encomenda(01 ,01 ,01 ,03 ,04 ,20).
encomenda(02 ,02 ,02 ,01 ,04 ,12).
encomenda(03 ,03 ,03 ,08 ,75 ,50).
encomenda(04 ,04 ,01 ,03 ,25 ,30).
encomenda(05 ,05 ,04 ,10 ,16 ,15). 
encomenda(06 ,06 ,01 ,07 ,02 ,06).  
encomenda(07 ,18 ,06 ,06 ,03 ,04).        
encomenda(08 ,08 ,01 ,04 ,35 ,20).
encomenda(09 ,09 ,07 ,08 ,05 ,12). 
encomenda(10 ,10 ,08 ,09 ,75 ,50). 
encomenda(11 ,11 ,09 ,06 ,25 ,30).
encomenda(12 ,12 ,10 ,03 ,16 ,15).        
encomenda(13 ,13 ,13 ,10 ,12 ,06).        
encomenda(14 ,14 ,14 ,11 ,03 ,04). 
encomenda(15 ,15 ,15 ,12 ,04 ,20).
encomenda(16 ,16 ,16 ,13 ,14, 12). 
encomenda(17 ,17 ,16 ,06 ,75 ,50).
encomenda(18 ,07 ,18 ,07 ,25 ,30).
encomenda(19 ,19 ,09 ,04 ,16 ,15).        
encomenda(20 ,20 ,10 ,06 ,12 ,06).        
encomenda(21 ,21 ,11 ,08 ,03 ,04).        
encomenda(22 ,22 ,12 ,09 ,24 ,20).        
encomenda(23 ,23 ,10 ,01 ,18 ,12).        
encomenda(24 ,11 ,18 ,07 ,75 ,50).        
encomenda(25 ,03 ,17 ,06 ,20 ,30).        
encomenda(26 ,26 ,16 ,09 ,16 ,15).        
encomenda(27 ,01 ,15 ,08 ,12 ,06).        
encomenda(28 ,02 ,01 ,02 ,03 ,04).        
encomenda(29 ,02 ,01 ,09 ,04 ,20).        
encomenda(30 ,02 ,02 ,01 ,04 ,12).        
encomenda(31 ,01 ,03 ,09 ,75 ,50).        
encomenda(32 ,09 ,11 ,11 ,25 ,30).        
encomenda(33 ,10 ,06 ,08 ,16 ,15).        
encomenda(34 ,13 ,01 ,04 ,12 ,07).        
encomenda(35 ,14 ,02 ,07 ,83 ,04).        
encomenda(36 ,25 ,01 ,08 ,04 ,20). 
encomenda(37 ,26 ,09 ,03 ,15 ,20).       


% Transporte --------------------------------------

% transporte(nome,velocidade(km/h),peso(kg));

transporte(bicicleta ,10 ,5).
transporte(mota ,35 ,20).
transporte(carro ,25 ,100).

% ---------------------------------------------------------------------------------------------------


% Registar e remover qualquer Termo(Estafeta,cliente,encomenda,entrega);

%Extensao do predicado evolucao:

evolucao(Termo):-
               findall(Invariante,+Termo::Invariante,Lista),
               insercao(Termo),
               teste(Lista).

insercao(Termo):-
               assert(Termo).
insercao(Termo):-
               retract(Termo),!,fail.

% ---------------------------------------------------------------------------------------------------

%Extensao do predicado involucao:

involucao(Termo):-
        findall(Invariante,-Termo::Invariante,Lista),
        remocao(Termo),
        teste(Lista).

remocao(Termo):- 
    retract(Termo).

remocao(Termo):-
    assert(Termo),!,fail.

% ---------------------------------------------------------------------------------------------------

teste([]).
teste([H|T]):- H,teste(T).

% ---------------------------------------------------------------------------------------------------


%INVARIANTES:


+estafeta(Id,Nome,Idade) :: (findall((Id),(estafeta(Id,_,_)),S),
   comprimento(S,N), N == 1).

+estafeta(Id,Nome,Idade) :: (findall((Nome,Idade),(estafeta(_,Nome,Idade)),S),
   comprimento(S,N), N == 1).

+cliente(Id,Nome,Idade,Rua) :: (findall((Id),(cliente(Id,_,_,_)),S),
   comprimento(S,N), N == 1).

+cliente(Id,Nome,Idade,Rua) :: (findall((Nome,Idade,Rua),(cliente(_,Nome,Idade,Rua)),S),
   comprimento(S,N), N == 1).

+encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume) :: 
(findall(Id_encomenda,(encomenda(Id_encomenda,_,_,_)),S),comprimento(S,N), N == 1).

+entrega(Id,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado) :: 
(findall((Id),(entrega(Id,_,_,_,_,_,_)),S),comprimento(S,N), N == 1).

+entrega(Id,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado) :: 
(findall((Id_encomenda),(entrega(_,Id_encomenda,_,_,_,_,_)),S),comprimento(S,N), N == 1).

-estafeta(Id,Nome,Idade) :: (findall(Id,encomenda(_,_,Id,_,_,_),S),comprimento(S,N),N == 0).

-cliente(Id,Nome,Idade,Rua) :: (findall(Id,encomenda(_,Id,_,_,_,_),S),comprimento(S,N),N == 0).

% ---------------------------------------------------------------------------------------------------

%Extensao do predicado registar_estafeta:

registar_estafeta(Id,Nome,Idade):-evolucao(estafeta(Id,Nome,Idade)).
remover_estafeta(Id,Nome,Idade):-involucao(estafeta(Id,Nom,Idade)).

%Extensao do predicado registar_cliente:

registar_cliente(Id,Nome,Idade,Rua):-evolucao(cliente(Id,Nome,Idade,Rua)).
remover_cliente(Id,Nome,Idade,Rua):-involucao(cliente(Id,Nom,Idade,Rua)).

%Extensao do predicado registar_encomenda:

registar_encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume):-evolucao(encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)).
remover_encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume):-involucao(encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)).

%Extensao do predicado registar_entrega:

registar_entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado):-evolucao(entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado)).
remover_entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado):-involucao(entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado)).

% ---------------------------------------------------------------------------------------------------




% Funcionalidade 1: identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

%Extensao do predicado estafetaecologico:

estafetaecologico(X):-findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,bicicleta,_,_),
  encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
comprimento(Z,A),A=\=0,!,maisfrequente(Z,X),write('Utilizou/Utilizaram mais vezes bicicleta').
estafetaecologico(X):-findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,mota,_,_),
  encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
comprimento(Z,A),A=\=0,!,maisfrequente(Z,X),write('Utilizou/Utilizaram mais vezes mota').
estafetaecologico(X):-findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,carro,_,_),
  encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
comprimento(Z,A),A=\=0,!,maisfrequente(Z,X),write('Utilizou/Utilizaram mais vezes carro').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 2: identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;


%Como já é considerado que a encomenda foi entregue,noo nosso trabalho a mesma passa a chamar se entrega;

%Extensao do predicado estafetaresponsavel:

estafetaresponsavel(Id_entrega,S):-findall((Id_estafeta,Nome,Idade),
  (entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),
    estafeta(Id_estafeta,Nome,Idade)),S),
  write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 3:identificar os clientes servidos por um determinado estafeta;

%Extensao do predicado clientesservidos:

clientesservidos(Id_estafeta,X):-findall((Id_cliente,Nome,Idade,Rua),(entrega(_,_,Id_encomenda,_,_,_,_),
  encomenda(Id_encomenda,Id_cliente,Id_estafeta,_,_,_),cliente(Id_cliente,Nome,Idade,Rua)),X),
write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Rua').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 4:calcular o valor faturado pela Green Distribution num determinado dia;

%Extensao do predicado dinheironodia:

dinheironodia(Data,S):-
findall(Preço,entrega(_,Data,_,_,_,Preço,_),R),soma(R,S).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 5:identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;

%Extensao do predicado ruamaismovimentada:

ruamaismovimentada(X):-findall((Nome_Rua,Freguesia,Cidade,Distancia),(entrega(_,_,Id_encomenda,_,_,_,_),
  encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),
  rua(Nome_Rua,Freguesia,Cidade,Distancia)),Z),maisfrequente(Z,X).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 6:calcular a classificação média de satisfação de cliente para um determinado estafeta;

%Extensao do predicado classestafeta:

classestafeta(Id_estafeta,X):-
findall((Classificacao),(encomenda(Id_encomenda,_,Id_estafeta,_,_,_),
  entrega(_,_,Id_encomenda,Classificacao,_,_,_)),Z),media(Z,X).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 7:identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;

%Extensao do predicado totalentregasportransporte:

totalentregasportransporte(date(Y,M,D),date(Y1,M1,D1),K):- auxiliar(date(Y,M,D),date(Y1,M1,D1),bicicleta,Z)
,add_tail([],Z,W),auxiliar(date(Y,M,D),date(Y1,M1,D1),mota,A),add_tail(W,A,V),
auxiliar(date(Y,M,D),date(Y1,M1,D1),carro,B),add_tail(V,B,K),
write('Numero de entregas de bicicleta, mota e carro respetivamente').

auxiliar(date(Y,M,D),date(Y1,M1,D1),X,Z):-
findall(Data,entrega(_,Data,_,_,X,_,_),V),maiores(V,date(Y,M,D),H),
menores(H,date(Y1,M1,D1),A),comprimento(A,Z).
% ---------------------------------------------------------------------------------------------------

% Funcionalidade 8:identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
%COMPARAR DATAS

%Extensao do predicado totalentregasporestafeta:

totalentregasporestafeta(X,Y,Z):- findall(Id_estafeta,estafeta(Id_estafeta,_,_),A),
comprimento(A,H),auxiliar2(X,Y,1,H,[],Z),write('Cada elemento é o numero de entregas por estafeta, começando pelo primeiro ordenadamente').

auxiliar2(J,L,H,X,Z,Z):-H>X.
auxiliar2(J,L,H,X,A,Z):-
findall(Data,(entrega(_,Data,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,H,_,_,_)),C),
maiores(C,J,K),menores(K,L,B),comprimento(B,S),add_tail(A,S,N),M is H+1,auxiliar2(J,L,M,X,N,Z).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 9:calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

%Extensao do predicado entreguesenaoentregues:

entreguesenaoentregues(X,Y,Z):-miniaux(X,Y,[],Z),write('Encomendas entregues e não entregues, respetivamente').

miniaux(X,Y,W,Z):- findall(Data,entrega(_,Data,Id_encomenda,_,_,_,_),S),comprimento(S,N),
  findall(Id_encomenda,encomenda(Id_encomenda,_,_,_,_,_),M),comprimento(M,G),
  V is G-N,add_tail(W,N,H),add_tail(H,V,Z).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 10:calcular o peso total transportado por estafeta num determinado dia.

%Extensao do predicado pesototal:

pesototal(Data,X):- findall(Id_estafeta,estafeta(Id_estafeta,_,_),Z),
comprimento(Z,H),funcao(Data,1,H,[],X).


funcao(Data,X,Q,Z,Z):- Q < X.
funcao(Data,X,Y,Q,Z):- funcao2(Data,X,A),add_tail(Q,A,G),V is X+1,funcao(Data,V,Y,G,Z).

funcao2(Data,X,Z):- findall(Peso,(entrega(_,Data,Id_encomenda,_,_,_,_),
  encomenda(Id_encomenda,_,X,_,Peso,_)),V),soma(V,Z).


% ---------------------------------------------------------------------------------------------------

%Predicados Adicionais:

% ---------------------------------------------------------------------------------------------------

% Mostrar todos os estafetas/encomendas/entregas/clientes:

%Extensao do predicado todosestafetas:

todosestafetas(S):-
findall((Id,Nome,Idade),estafeta(Id,Nome,Idade),S),
write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').

% ---------------------------------------------------------

%Extensao do predicado todasencomendas:

todasencomendas(S):-
findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),
  encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

%Extensao do predicado todasentregas:

todasentregas(S):-
findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),
entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),S),
write('Ordem do predicado entrega:Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------

%Extensao do predicado todosclientes:

todosclientes(S):-
findall((Id_cliente,Nome,Idade,Rua),cliente(Id_cliente,Nome,Idade,Rua),S),
write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Nome_Rua').

% ---------------------------------------------------------

%Extensao do predicado todasruas:

todasruas(S):-
findall((Nome_Rua,Freguesia,Cidade,Distancia),rua(Nome_Rua,Freguesia,Cidade,Distancia),S),
write('Ordem do predicado rua: Nome_Rua,Freguesia,Cidade,Distancia').

% ---------------------------------------------------------------------------------------------------

% Mostra as entregas já realizadas por um estafeta :

%Extensao do predicado estafetadeliver:

estafetadeliver(Id_estafeta,S):-
findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),
(encomenda(Id_encomenda,_,Id_estafeta,_,_,_),
entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado)),S),
write('Ordem do predicado entrega: Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------------------------------------------------

% Saber as informações sobre a encomenda dado o Id da entrega:

%Extensao do predicado entregainfoencomenda:

entregainfoencomenda(Id_entrega,S):-
findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),
  (entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)),S),
write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

%Saber tudo sobre um cliente/estafeta/encomenda/entrega pelo seu ID:

%Extensao do predicado clienteinfo:

clienteinfo(Id_cliente,S):-
findall((Id_cliente,Nome,Idade,Rua),cliente(Id_cliente,Nome,Idade,Rua),S),
write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Rua').

% ---------------------------------------------------------

%Extensao do predicado encomendainfo:

encomendainfo(Id_encomenda,S):-
findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),
  encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

%Extensao do predicado entregainfo:

entregainfo(Id_entrega,S):-
findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),
  entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),S),
write('Ordem do predicado entrega: Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------

%Extensao do predicado estafetainfo:

estafetainfo(Id_estafeta,S):-
findall((Id_estafeta,Nome,Idade),estafeta(Id_estafeta,Nome,Idade),S),
write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').


% ---------------------------------------------------------------------------------------------------

% Mostra as encomendas já realizadas por um cliente :

%Extensao do predicado encomendascliente:

encomendascliente(Id_cliente,S):-
findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),
  encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------------------------------------------------

% Mostra o estafeta que fez uma entrega :

%Extensao do predicado estafetadaentrega:

estafetadaentrega(Id_entrega,X):-
findall((Id_estafeta,Nome,Idade),(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),
  encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),X).

% ---------------------------------------------------------------------------------------------------

% Dada uma encomenda verifica qual o meio mais ecologico para ser utilizado

%Extensao do predicado melhorveiculo:

melhorveiculo(Id_encomenda,bicicleta):-findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
cabeca(X,Y),5>=Y,findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),cabeca(Z,W),
findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),
rua(Nome_Rua,Freguesia,Cidade,Distancia)),A),cabeca(A,B),Z>=B/10.

melhorveiculo(Id_encomenda,mota):-findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
cabeca(X,Y),20>=Y,findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),cabeca(Z,W),
findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),
rua(Nome_Rua,Freguesia,Cidade,Distancia)),A),cabeca(A,B),Z>=B/35.

melhorveiculo(Id_encomenda,carro):-findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
cabeca(X,Y),100>=Y,findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),cabeca(Z,W),
findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),
rua(Nome_Rua,Freguesia,Cidade,Distancia)),A),cabeca(A,B),Z>=B/25. 


cabeca([H],H).
% ---------------------------------------------------------------------------------------------------

%Calcula o melhor preço de uma encomenda:

%Extensão do predicado melhorpreco:

melhorpreco(Id_encomenda,X):-findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),
  cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade,Distancia)),Z),cabeca(Z,Y),
melhorveiculo(Id_encomenda,bicicleta),X is Y*0.3.
melhorpreco(Id_encomenda,X):-findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),
  cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade,Distancia)),Z),cabeca(Z,Y),
melhorveiculo(Id_encomenda,mota),X is Y*0.6.
melhorpreco(Id_encomenda,X):-findall(Distancia,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),
  cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade,Distancia)),Z),cabeca(Z,Y),
melhorveiculo(Id_encomenda,carro),X is Y*1.2.

% ---------------------------------------------------------------------------------------------------

%Verifica se uma entrega foi entregue a tempo.

%Extensao do predicado entregueatempo:

entregueatempo(Id_entrega):-
findall(Tempo_demorado,entrega(Id_entrega,_,_,_,_,_,Tempo_demorado),X),cabeca(X,Y),
findall(Prazo_pedido,(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,_,Prazo_pedido,_,_)),Z),
cabeca(Z,W),W>=Y.

% ---------------------------------------------------------------------------------------------------


% Funções auxiliares :

%Extensão do predicado soma:

soma([],0).
soma([H|T],R):-
soma(T,X), 
R is H+X.

% ---------------------------------
%Extensão do predicado media:

media( Lista, Media ):- 
    soma( Lista, Soma ),
    comprimento( Lista, Comprimento), 
    Media is Soma / Comprimento.

% ---------------------------------
%Extensão do predicado comprimento:

comprimento([],0).
comprimento([_|T],Length):-
  comprimento(T,X),
  Length is X+1.

% ---------------------------------
%Extensão do predicado maiores:

maiores([], _, []) :- !.
maiores([Head|Rest], X, L) :- value(Head,A),value(X,B),
  A < B, !,
  maiores(Rest, X, L).
maiores([Head|Rest], X, [Head|L]) :-
  maiores(Rest, X, L).

% ---------------------------------
%Extensão do predicado menores:

menores([], _, []) :- !.
menores([Head|Rest], X, L) :- value(Head,A),value(X,B),
  A > B, !,
  menores(Rest, X, L).
menores([Head|Rest], X, [Head|L]) :-
  menores(Rest, X, L).

% ---------------------------------
%Extensao do predicado value:

value(date(Y,M,D),X):-
X is (Y*365+M*30+D).

% ---------------------------------
%Extensao do predicado add_tail:

add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

% ---------------------------------
%Extensao do predicado maisfrequente:

maisfrequente([],X).
maisfrequente([H|T],X):- aux([H|T],[],0,X).

aux([],Z,A,Z).
aux([H|T],Z,X,Y):- quantosexistem(H,[H|T],0,B),B>X,!,aux(T,[H],B,Y).
aux([H|T],Z,X,Y):- quantosexistem(H,[H|T],0,B),B=:=X,!,add_tail(Z,H,A),aux(T,A,X,Y).
aux([H|T],Z,X,Y):- aux(T,Z,X,Y).

quantosexistem(X,[],A,A).
quantosexistem(X,[X|T],A,Z):-V is A+1, quantosexistem(X,T,V,Z).
quantosexistem(X,[H|T],A,Z):-quantosexistem(X,T,A,Z).

% --------------------------------- 





