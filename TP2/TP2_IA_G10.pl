% Necessidades

:- op( 900,xfy,'::' ).
:- dynamic estafeta/3.
:- dynamic entrega/7.
:- dynamic rua/4.
:- dynamic cliente/4.
:- dynamic encomenda/6.


:- style_check(-singleton).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

% Grafo (Cidades e Freguesias)

aresta(rua_central,sete_ceus,11).
aresta(rua_central,vinte_e_quatro,18).
aresta(rua_central,projetada,9).
aresta(sete_ceus,avenida_5_de_outubro,23).
aresta(avenida_5_de_outubro,calcada_santana,8).
aresta(calcada_santana,projetada,10).
aresta(projetada,travessa_do_rio,17).
aresta(travessa_do_rio,santa_maria,17).
aresta(travessa_do_rio,jose_maria_ottoni,9).
aresta(boa_vista,jose_maria_ottoni,32).
aresta(santa_maria,boa_vista,28).
aresta(santa_maria,avenida_5_de_outubro,9).
aresta(boa_vista,avenida_paz,32).
aresta(avenida_paz,bairro_feliz,29).
aresta(bairro_feliz,duque_de_caxias,27).
aresta(duque_de_caxias,caranda,8).
aresta(duque_de_caxias,lamelas,10).
aresta(caranda,jose_maria_ottoni,5).
aresta(green_distribution,jose_maria_ottoni,7).
aresta(green_distribution,rui_barbosa,6).
aresta(green_distribution,rua_barros,4).
aresta(lamelas,rui_barbosa,8).
aresta(lamelas,sao_jorge,10).
aresta(sao_jorge,beco_belo,9).
aresta(beco_belo,bairro_feliz,32).
aresta(beco_belo,santa_catarina,13).
aresta(santa_catarina,amorosa,9).
aresta(amorosa,rui_barbosa,10).
aresta(santa_luzia,augusta,30).
aresta(augusta,amorosa,16).
aresta(augusta,sao_luis,8).
aresta(sao_luis,rua_barros,7).
aresta(vinte_e_quatro,rua_barros,14).

goal(green_distribution).

% Estimativas-----------------------------------------

estima(rua_central, 25).
estima(sete_ceus, 28).
estima(vinte_e_quatro,19).
estima(projetada,23).
estima(calcada_santana,22).
estima(avenida_5_de_outubro,24).
estima(santa_maria,20).
estima(travessa_do_rio,16).
estima(boa_vista,31).
estima(jose_maria_ottoni,7).
estima(avenida_paz,32).
estima(bairro_feliz,37).
estima(duque_de_caxias,16).
estima(caranda,6).
estima(lamelas,8).
estima(beco_belo,28).
estima(sao_jorge,15).
estima(green_distribution,0).
estima(rui_barbosa,6).
estima(rua_barros,4).
estima(amorosa,15).
estima(santa_catarina,26).
estima(sao_luis,13).
estima(augusta,17).
estima(santa_luzia,53).

% Estafeta ------------------------------------------

% Estafeta(id_estafeta,nome,idade)

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

% Cliente(id_cliente,nome,idade,Nome_Rua).

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

% Rua(Nome,Freguesia,Cidade)

rua(jose_maria_ottoni ,nogueiro,braga).
rua(avenida_5_de_outubro ,serafao,fafe). 
rua(rua_central,viade_baixo ,montalegre).
rua(santa_luzia,barcelos ,barcelos).
rua(sao_jorge,barcelos ,barcelos). 
rua(duque_de_caxias ,nogueiro,braga).
rua(projetada ,fervidelas,montalegre).
rua(rui_barbosa,braga ,braga).
rua(santa_catarina,barcelos ,barcelos).
rua(boa_vista,santo_tirso ,porto).
rua(sao_luis ,nogueiro,braga).
rua(vinte_e_quatro,viade_cima,guimaraes).
rua(sete_ceus,viade_baixo ,barcelos).
rua(beco_belo,barcelos ,barcelos).
rua(amorosa,barcelos ,barcelos).
rua(travessa_do_rio ,nogueiro,braga).
rua(santa_maria ,serafao,fafe).
rua(lamelas,fujacal ,braga).
rua(bairro_feliz,povoa,porto).
rua(augusta,padeirinhos,braga).
rua(calcada_santana,vila_cova,fafe).
rua(avenida_paz,trofa,porto).
rua(rua_barros, gualtar,braga).
rua(caranda,fujacal,braga).



% Entrega ---------------------------------------


% Entrega(id_entrega,Data,id_encomenda,classificacao,transporte,Preço,Tempo_demorado(horas))

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

% Encomenda(id_encomenda,id_cliente,id_estafeta,prazo_pedido(horas),peso(kg),volume(m3))
        
encomenda(01 ,01 ,01 ,25 ,04 ,20).
encomenda(02 ,02 ,02 ,04 ,04 ,12).
encomenda(03 ,03 ,03 ,08 ,75 ,50).
encomenda(04 ,04 ,01 ,03 ,25 ,30).
encomenda(05 ,05 ,04 ,10 ,14 ,15). 
encomenda(06 ,06 ,01 ,07 ,02 ,06).  
encomenda(07 ,18 ,06 ,06 ,03 ,04).      
encomenda(08 ,08 ,01 ,04 ,35 ,20).
encomenda(09 ,09 ,07 ,08 ,05 ,12). 
encomenda(10 ,10 ,08 ,09 ,75 ,50). 
encomenda(11 ,11 ,09 ,06 ,25 ,30).
encomenda(12 ,12 ,10 ,03 ,12 ,15).        
encomenda(13 ,13 ,13 ,10 ,12 ,06).        
encomenda(14 ,14 ,14 ,11 ,03 ,04). 
encomenda(15 ,15 ,15 ,12 ,04 ,20).
encomenda(16 ,16 ,16 ,13 ,14 ,12). 
encomenda(17 ,17 ,16 ,06 ,75 ,50).
encomenda(18 ,07 ,18 ,07 ,25 ,30).
encomenda(19 ,19 ,09 ,04 ,16 ,15).        
encomenda(20 ,20 ,10 ,06 ,12 ,06).        
encomenda(21 ,21 ,11 ,08 ,03 ,04).        
encomenda(22 ,22 ,12 ,09 ,24 ,20).        
encomenda(23 ,23 ,10 ,01 ,18 ,12).        
encomenda(24 ,11 ,18 ,07 ,75 ,50).        
encomenda(25 ,03 ,17 ,06 ,20 ,30).        
encomenda(26 ,26 ,16 ,14 ,16 ,15).        
encomenda(27 ,01 ,15 ,08 ,12 ,06).        
encomenda(28 ,02 ,01 ,02 ,03 ,04).        
encomenda(29 ,02 ,01 ,09 ,04 ,20).        
encomenda(30 ,02 ,02 ,03 ,04 ,12).        
encomenda(31 ,01 ,03 ,09 ,75 ,50).        
encomenda(32 ,09 ,11 ,11 ,25 ,30).        
encomenda(33 ,10 ,06 ,08 ,16 ,15).        
encomenda(34 ,13 ,01 ,04 ,12 ,07).        
encomenda(35 ,14 ,02 ,07 ,83 ,04).        
encomenda(36 ,25 ,01 ,08 ,04 ,20). 
encomenda(37 ,26 ,09 ,03 ,15 ,20).
encomenda(38 ,26 ,16 ,23 ,01 ,15).   
encomenda(39 ,01 ,16 ,23 ,01 ,15).             
encomenda(40 ,01 ,16 ,23 ,10 ,15).
encomenda(41 ,02 ,02 ,04 ,11 ,12).
encomenda(42 ,02 ,02 ,25 ,05 ,12).    

% Transporte --------------------------------------

% transporte(nome,velocidade(km/h),peso(kg));

transporte(bicicleta ,10 ,5).
transporte(mota ,35 ,20).
transporte(carro ,25 ,100).

% ---------------------------------------------------------------------------------------------------


% Registar e remover qualquer Termo(Estafeta,cliente,encomenda,entrega);

% Extensão do predicado evolucao:

evolucao(Termo):-
	findall(Invariante,+Termo::Invariante,Lista),
	insercao(Termo),
	teste(Lista).

insercao(Termo):-
	assert(Termo).
insercao(Termo):-
	retract(Termo),!,fail.

% ---------------------------------------------------------------------------------------------------

% Extensão do predicado involucao:

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

% Extensao do predicado registar_estafeta:

registar_estafeta(Id,Nome,Idade):-
	evolucao(estafeta(Id,Nome,Idade)).

remover_estafeta(Id,Nome,Idade):-
	involucao(estafeta(Id,Nom,Idade)).

% Extensao do predicado registar_cliente:

registar_cliente(Id,Nome,Idade,Rua):-
	evolucao(cliente(Id,Nome,Idade,Rua)).

remover_cliente(Id,Nome,Idade,Rua):-
	involucao(cliente(Id,Nom,Idade,Rua)).

% Extensao do predicado registar_encomenda:

registar_encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume):-
	evolucao(encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)).

remover_encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume):-
	involucao(encomenda(Id,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)).

% Extensao do predicado registar_entrega:

registar_entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado):-
	evolucao(entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado)).

remover_entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado):-
	involucao(entrega(Id_entrega,Data,Id_encomenda,Classificacao,Id_transporte,Preço,Tempo_demorado)).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 1: identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

% Extensao do predicado estafetaecologico:

estafetaecologico(X):-
	findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,bicicleta,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
	comprimento(Z,A),
	A=\=0,!,
	maisfrequente(Z,X),
	write('Utilizou/Utilizaram mais vezes bicicleta').
estafetaecologico(X):-
	findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,mota,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
	comprimento(Z,A),
	A=\=0,!,
	maisfrequente(Z,X),
	write('Utilizou/Utilizaram mais vezes mota').
estafetaecologico(X):-
	findall((Id_estafeta,Nome,Idade),(entrega(_,_,Id_encomenda,_,carro,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),Z),
	comprimento(Z,A),
	A=\=0,!,
	maisfrequente(Z,X),
	write('Utilizou/Utilizaram mais vezes carro').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 2: identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;


% Como já é considerado que a encomenda foi entregue,noo nosso trabalho a mesma passa a chamar se entrega;

% Extensao do predicado estafetaresponsavel:

estafetaresponsavel(Id_entrega,S):-
	findall((Id_estafeta,Nome,Idade),(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),S),
  	write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 3: identificar os clientes servidos por um determinado estafeta;

% Extensao do predicado clientesservidos:

clientesservidos(Id_estafeta,X):-
	findall((Id_cliente,Nome,Idade,Rua),(entrega(_,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,Id_estafeta,_,_,_),cliente(Id_cliente,Nome,Idade,Rua)),X),
	write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Rua').

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 4: calcular o valor faturado pela Green Distribution num determinado dia;

% Extensao do predicado dinheironodia:

dinheironodia(Data,S):-
	findall(Preço,entrega(_,Data,_,_,_,Preço,_),R),
	soma(R,S).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 5: identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;

% Extensao do predicado ruamaismovimentada:

ruamaismovimentada(X):-
	findall((Nome_Rua,Freguesia,Cidade),(entrega(_,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),Z),
	maisfrequente(Z,X).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 6: calcular a classificação média de satisfação de cliente para um determinado estafeta;

% Extensao do predicado classestafeta:

classestafeta(Id_estafeta,X):-
	findall((Classificacao),(encomenda(Id_encomenda,_,Id_estafeta,_,_,_),entrega(_,_,Id_encomenda,Classificacao,_,_,_)),Z),media(Z,X).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 7:identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;

% Extensao do predicado totalentregasportransporte:

totalentregasportransporte(date(Y,M,D),date(Y1,M1,D1),K):-
	auxiliar(date(Y,M,D),date(Y1,M1,D1),bicicleta,Z),
	add_tail([],Z,W),
	auxiliar(date(Y,M,D),date(Y1,M1,D1),mota,A),
	add_tail(W,A,V),
	auxiliar(date(Y,M,D),date(Y1,M1,D1),carro,B),
	add_tail(V,B,K),
	write('Numero de entregas de bicicleta, mota e carro respetivamente').

auxiliar(date(Y,M,D),date(Y1,M1,D1),X,Z):-
	findall(Data,entrega(_,Data,_,_,X,_,_),V),
	maiores(V,date(Y,M,D),H),
	menores(H,date(Y1,M1,D1),A),
	comprimento(A,Z).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 8: identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
% COMPARAR DATAS

% Extensao do predicado totalentregasporestafeta:

totalentregasporestafeta(X,Y,Z):-
	findall(Id_estafeta,estafeta(Id_estafeta,_,_),A),
	comprimento(A,H),
	auxiliar2(X,Y,1,H,[],Z),
	write('Cada elemento é o numero de entregas por estafeta, começando pelo primeiro ordenadamente').

auxiliar2(J,L,H,X,Z,Z):-
	H>X.
auxiliar2(J,L,H,X,A,Z):-
	findall(Data,(entrega(_,Data,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,H,_,_,_)),C),
	maiores(C,J,K),
	menores(K,L,B),
	comprimento(B,S),
	add_tail(A,S,N),
	M is H+1,
	auxiliar2(J,L,M,X,N,Z).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 9:calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

% Extensao do predicado entreguesenaoentregues:

entreguesenaoentregues(X,Y,Z):-
	miniaux(X,Y,[],Z),
	write('Encomendas entregues e não entregues, respetivamente').

miniaux(X,Y,W,Z):-
	findall(Data,entrega(_,Data,Id_encomenda,_,_,_,_),S),
	comprimento(S,N),
  	findall(Id_encomenda,encomenda(Id_encomenda,_,_,_,_,_),M),
	comprimento(M,G),
  	V is G-N,
	add_tail(W,N,H),
	add_tail(H,V,Z).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 10:calcular o peso total transportado por estafeta num determinado dia.

% Extensao do predicado pesototal:

pesototal(Data,X):-
	findall(Id_estafeta,estafeta(Id_estafeta,_,_),Z),
	comprimento(Z,H),
	funcao(Data,1,H,[],X).


funcao(Data,X,Q,Z,Z):-
	Q < X.
funcao(Data,X,Y,Q,Z):-
	funcao2(Data,X,A),
	add_tail(Q,A,G),
	V is X+1,
	funcao(Data,V,Y,G,Z).

funcao2(Data,X,Z):-
	findall(Peso,(entrega(_,Data,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,X,_,Peso,_)),V),soma(V,Z).


% ---------------------------------------------------------------------------------------------------

% Predicados Adicionais:

% ---------------------------------------------------------------------------------------------------

% Mostrar todos os estafetas/encomendas/entregas/clientes:

% Extensao do predicado todosestafetas:

todosestafetas(S):-
	findall((Id,Nome,Idade),estafeta(Id,Nome,Idade),S),
	write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').

% ---------------------------------------------------------

% Extensao do predicado todasencomendas:

todasencomendas(S):-
	findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
	write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

% Extensao do predicado todasentregas:

todasentregas(S):-
	findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),S),
	write('Ordem do predicado entrega:Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------

% Extensao do predicado todosclientes:

todosclientes(S):-
	findall((Id_cliente,Nome,Idade,Rua),cliente(Id_cliente,Nome,Idade,Rua),S),
	write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Nome_Rua').

% ---------------------------------------------------------

% Extensao do predicado todasruas:

todasruas(S):-
	findall((Nome_Rua,Freguesia,Cidade),rua(Nome_Rua,Freguesia,Cidade),S),
	write('Ordem do predicado rua: Nome_Rua,Freguesia,Cidade').

% ---------------------------------------------------------------------------------------------------

% Mostra as entregas já realizadas por um estafeta :

% Extensao do predicado estafetadeliver:

estafetadeliver(Id_estafeta,S):-
	findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),(encomenda(Id_encomenda,_,Id_estafeta,_,_,_),entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado)),S),
	write('Ordem do predicado entrega: Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------------------------------------------------

% Saber as informações sobre a encomenda dado o Id da entrega:

% Extensao do predicado entregainfoencomenda:

entregainfoencomenda(Id_entrega,S):-
	findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume)),S),
	write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

% Saber tudo sobre um cliente/estafeta/encomenda/entrega pelo seu ID:

% Extensao do predicado clienteinfo:

clienteinfo(Id_cliente,S):-
	findall((Id_cliente,Nome,Idade,Rua),cliente(Id_cliente,Nome,Idade,Rua),S),
	write('Ordem do predicado cliente: Id_cliente,Nome,Idade,Rua').

% ---------------------------------------------------------

% Extensao do predicado encomendainfo:

encomendainfo(Id_encomenda,S):-
	findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
	write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------

% Extensao do predicado entregainfo:

entregainfo(Id_entrega,S):-
	findall((Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),entrega(Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado),S),
	write('Ordem do predicado entrega: Id_entrega,Data,Id_encomenda,Classificacao,Transporte,Preço,Tempo_demorado').

% ---------------------------------------------------------

% Extensao do predicado estafetainfo:

estafetainfo(Id_estafeta,S):-
	findall((Id_estafeta,Nome,Idade),estafeta(Id_estafeta,Nome,Idade),S),
	write('Ordem do predicado estafeta: Id_estafeta,Nome,Idade').


% ---------------------------------------------------------------------------------------------------

% Mostra as encomendas já realizadas por um cliente :

% Extensao do predicado encomendascliente:

encomendascliente(Id_cliente,S):-
	findall((Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume),S),
	write('Ordem do predicado encomenda: Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,Volume').

% ---------------------------------------------------------------------------------------------------

% Mostra o estafeta que fez uma entrega :

% Extensao do predicado estafetadaentrega:

estafetadaentrega(Id_entrega,X):-
	findall((Id_estafeta,Nome,Idade),(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,Id_estafeta,_,_,_),estafeta(Id_estafeta,Nome,Idade)),X).

% ---------------------------------------------------------------------------------------------------

% Verifica se uma entrega foi entregue a tempo.

% Extensao do predicado entregueatempo:

entregueatempo(Id_entrega):-
	findall(Tempo_demorado,entrega(Id_entrega,_,_,_,_,_,Tempo_demorado),X),
	cabeca(X,Y),
	findall(Prazo_pedido,(entrega(Id_entrega,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,_,_,Prazo_pedido,_,_)),Z),
	cabeca(Z,W),
	W>=Y.

% ---------------------------------------------------------------------------------------------------


% Funções auxiliares :

% Extensão do predicado soma:

soma([],0).
soma([H|T],R):-
	soma(T,X), 
	R is H+X.

% ---------------------------------
% Extensão do predicado media:

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
add_tail([H|T],X,[H|L]):-
	add_tail(T,X,L).

% ---------------------------------
%Extensao do predicado maisfrequente:

maisfrequente([],X).
maisfrequente([H|T],X):-
	aux([H|T],[],0,X).

aux([],Z,A,Z).
aux([H|T],Z,X,Y):-
	quantosexistem(H,[H|T],0,B),B>X,!,aux(T,[H],B,Y).
aux([H|T],Z,X,Y):-
	quantosexistem(H,[H|T],0,B),B=:=X,!,add_tail(Z,H,A),aux(T,A,X,Y).
aux([H|T],Z,X,Y):-
	aux(T,Z,X,Y).

quantosexistem(X,[],A,A).
quantosexistem(X,[X|T],A,Z):-
	V is A+1,
	quantosexistem(X,T,V,Z).
quantosexistem(X,[H|T],A,Z):-
	quantosexistem(X,T,A,Z).

% --------------------------------- 
%Extensao do predicado cabeca:

cabeca([H],H).

% ---------------------------------------------------------------------------------------------------

% ------------------------------------TRABALHO PRÁTICO 2--------------------------------------------

% ---------------------------------------------------------------------------------------------------

% ----------------------------------ALTERAÇÕES NECESSÁRIAS-------------------------------------------

% Dada uma encomenda verifica qual o meio mais ecologico para ser utilizado

% Extensao do predicado melhorveiculo:

melhorveiculo(Id_encomenda,bicicleta):-
	findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
	cabeca(X,Y),
	5>=Y,
	findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),
	cabeca(Z,W),
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),A),
	cabeca(A,Q),
	resolve_aestrela(Q,B/S),
	W>=S/(10-(Y*0.7)),!.
melhorveiculo(Id_encomenda,mota):-
	findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
	cabeca(X,Y),
	20>=Y,
	findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),
	cabeca(Z,W),
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),A),
	cabeca(A,Q),
	resolve_aestrela(Q,B/S),
	W>=S/(35-(Y*0.5)),!.
melhorveiculo(Id_encomenda,carro):-
	findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),X),
	cabeca(X,Y),
	100>=Y,
	findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),
	cabeca(Z,W),
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),A),
	cabeca(A,Q),
	resolve_aestrela(Q,B/S),
	W>=S/(25-(Y*0.1)),!.

% ---------------------------------------------------------------------------------------------------

% Calcula o melhor preço de uma encomenda:

% Extensão do predicado melhorpreco:

melhorpreco(Id_encomenda,X):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),Z),
	cabeca(Z,Y),
	melhorveiculo(Id_encomenda,bicicleta),
	X is Y*0.3.
melhorpreco(Id_encomenda,X):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),Z),
	cabeca(Z,Y),
	melhorveiculo(Id_encomenda,mota),
	X is Y*0.6.
melhorpreco(Id_encomenda,X):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua),rua(Nome_Rua,Freguesia,Cidade)),Z),
	cabeca(Z,Y),
	melhorveiculo(Id_encomenda,carro),
	X is Y*1.2.

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 1: Gerar os circuitos de entrega, caso existam, que cubram um determinado território(Rua)


todoscircuitos(Nodo, Destino, S, C):-
	findall((SS, CC), depthfirst(Nodo, Destino, SS, CC), S).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 2: Representação dos diversos pontos de entrega em forma de grafo, tendo em conta que apenas se devem ter localizações (rua e/ou freguesia) disponíveis
% (Base de dados)-> Linhas de código do inicio.

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 3: Identificar quais os circuitos com maior número de entregas (por volume e peso), retornando o circuito e o peso+volume de 
% todas as entregas já feitas nas ruas desse circuito

% ---------------------------------------------------------------------------------------------------

% Compara por volume: 

circuitosnumeroentregasv(F,Z):-
	findall(Nome_Rua,(entrega(_,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,_,_,Peso,Volume),cliente(Id_cliente,_,_,Nome_Rua)),X),
	eliminarepetidos(X,[],W),
	aux5(W,[],D),
	aux3v(D,[],0,F,G),
	circuito(G, Z),!.

aux5([],Z,Z).
aux5([H|T],Z,X):-
	resolve_aestrela(H,A),
	aux6(A,B),
	aux4(B,Z,W),
	aux5(T,W,X).

aux4((A,B),[],[(A,B)]).
aux4((A,B),X,[(A,B)|X]).

aux6(A/B,(A,B)).

aux3v([],X,H,H,X).
aux3v([(A,B)|T],C,P,H,X):-
	volumecircuito(A,Z),
	Z>P,
	aux3v(T,A,Z,H,X).
aux3v([(A,B)|T],C,P,H,X):-
	volumecircuito(A,Z),
	Z=<P,
	aux3v(T,C,P,H,X).

volumecircuito([],0).
volumecircuito([green_distribution|T],Z):-
	volumecircuito(T,Z).
volumecircuito([H|T],A):-
	volumeaux(H,W),
	volumecircuito(T,X),
	A is X+W.


volumeaux(H,A):-
	findall((Peso,Volume),(cliente(Id_cliente,_,_,H),encomenda(Id_encomenda,Id_cliente,_,_,Peso,Volume),entrega(_,_,Id_encomenda,_,_,_,_)),Z),
	aux2v(Z,0,A).

aux2v([],X,X).
aux2v([(A,B)|T],X,Y):-
	S is (X+B),
	aux2v(T,S,Y).

% ---------------------------------------------------------------------------------------------------

% Compara por peso: 

circuitosnumeroentregasp(F,Z):-
	findall(Nome_Rua,(entrega(_,_,Id_encomenda,_,_,_,_),encomenda(Id_encomenda,Id_cliente,_,_,Peso,Volume),cliente(Id_cliente,_,_,Nome_Rua)),X),
	eliminarepetidos(X,[],W),
	aux5(W,[],D),
	aux3p(D,[],0,F,G),
	circuito(G, Z),!.

aux3p([],X,H,H,X).
aux3p([(A,B)|T],C,P,H,X):-
	pesocircuito(A,Z),
	Z>P,
	aux3p(T,A,Z,H,X).
aux3p([(A,B)|T],C,P,H,X):-
	pesocircuito(A,Z),
	Z=<P,
	aux3p(T,C,P,H,X).

pesocircuito([],0).
pesocircuito([green_distribution|T],Z):-
	pesocircuito(T,Z).
pesocircuito([H|T],A):-
	pesoaux(H,W),
	pesocircuito(T,X),
	A is X+W.

pesoaux(H,A):-
	findall((Peso,Volume),(cliente(Id_cliente,_,_,H),encomenda(Id_encomenda,Id_cliente,_,_,Peso,Volume),entrega(_,_,Id_encomenda,_,_,_,_)),Z),
	aux2p(Z,0,A).

aux2p([],X,X).
aux2p([(A,B)|T],X,Y):-
	S is (X+A),
	aux2p(T,S,Y).

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 4: Comparar circuitos de entrega tendo em conta os indicadores de produtividade;

comparacircuitos(X,A):-
	findall((Id_encomenda,Prazo_pedido,Peso),(entrega(_,_,Id_encomenda,_,_,_,Tempo_demorado),encomenda(Id_encomenda,Id_cliente,Id_estafeta,Prazo_pedido,Peso,_)),Z),
	auxiliarcircuitos(Z,(0,0,0),0,(A,B,C)),
	findall(Nome_Rua,(encomenda(A,Id_cliente,_,_,_,_),cliente(Id_cliente,_,_,Nome_Rua)),F),
	cabeca(F,G),
	resolve_aestrela(G,T/Y),
	circuito(T,X),!.

auxiliarcircuitos([],X,_,X).
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,bicicleta),
	Velocidade is 10-(0.7*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade>Y,
	auxiliarcircuitos(T,(A,B,C),Produtividade,Z),!.
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,bicicleta),
	Velocidade is 10-(0.7*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade=<Y,
	auxiliarcircuitos(T,W,Y,Z),!.
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,mota),
	Velocidade is 35-(0.5*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade>Y,
	auxiliarcircuitos(T,(A,B,C),Produtividade,Z),!.
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,mota),
	Velocidade is 35-(0.5*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade=<Y,
	auxiliarcircuitos(T,W,Y,Z),!.
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,carro),
	Velocidade is 25-(0.1*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade>Y,
	auxiliarcircuitos(T,(A,B,C),
	Produtividade,Z),!.
auxiliarcircuitos([(A,B,C)|T],W,Y,Z):-
	distanciaencomenda(A,X),
	melhorveiculo(A,carro),
	Velocidade is 25-(0.1*C),
	Tempo is X/Velocidade,
	Produtividade is (B-Tempo+X),
	Produtividade=<Y,
	auxiliarcircuitos(T,W,Y,Z),!.

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 5: Escolher o circuito mais rápido (usando o critério da distância);
% Optámos por fazer uma adaptação da A*

circuitomaisrapido(Nodo,Caminho/Custo):-
	resolve_aestrela(Nodo,Uncaminho/Custo2),
	circuito(Uncaminho,Caminho),
	Custo is Custo2*2.

% ---------------------------------------------------------------------------------------------------

% Funcionalidade 6: Escolher o circuito mais ecologico (usando um criterio de tempo)

circuitoecologico(Id_encomenda,X/Custo):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,Nodo),
	todoscircuitos(green_distribution, Nodo, SS, CC),
	melhorveiculo(Id_encomenda,bicicleta),
	head(SS,S),
	tempomenor(SS,S,(A,B),bicicleta),
	circuito(A,X),
	Custo is B*2.
circuitoecologico(Id_encomenda,X/Custo):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,Nodo),
	todoscircuitos(green_distribution, Nodo, SS, CC),
	melhorveiculo(Id_encomenda,mota),
	head(SS,S),
	tempomenor(SS,S,(A,B),mota),
	circuito(A,X),
	Custo is B*2.
circuitoecologico(Id_encomenda,X/Custo):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,Nodo),
	todoscircuitos(green_distribution,Nodo, SS, CC),
	melhorveiculo(Id_encomenda,carro),
	head(SS,S),
	tempomenor(SS,S,(A,B),carro),
	circuito(A,X),
	Custo is B*2.

head([H|T],H).

tempomenor([],X,X,_).
tempomenor([(A,B)|X],(C,D),Z,bicicleta):-
	E is (D/10),
	G is (B/10),
	E<G,
	tempomenor(X,(C,D),Z,bicicleta).
tempomenor([(A,B)|X],(C,D),Z,bicicleta):-
	E is (D/10),
	G is (B/10), 
	E>=G, 
	tempomenor(X,(A,B),Z,bicicleta).
tempomenor([(A,B)|X],(C,D),Z,mota):- 
	E is (D/35), 
	G is (B/35), 
	E<G, 
	tempomenor(X,(C,D),Z,mota).
tempomenor([(A,B)|X],(C,D),Z,mota):- 
	E is (D/35), 
	G is (B/35), 
	E>=G, 
	tempomenor(X,(A,B),Z,mota).
tempomenor([(A,B)|X],(C,D),Z,carro):- 
	E is (D/25),
	G is (B/25), 
	E<G, 
	tempomenor(X,(C,D),Z,carro).
tempomenor([(A,B)|X],(C,D),Z,carro):- 
	E is (D/25), 
	G is (B/25), 
	E>=G, 
	tempomenor(X,(A,B),Z,carro).

% ---------------------------------------------------------------------------------------------------

% Predicado que elimina todos os repetidos de uma lista

eliminarepetidos([],X,X).
eliminarepetidos([H],X,[H|X]).
eliminarepetidos([H|T],X,Z):-
	member(H,T),
	eliminarepetidos(T,X,Z).
eliminarepetidos([H|T],X,Z):-
	not(member(H,T)),
	eliminarepetidos(T,[H|X],Z).

% ---------------------------------------------------------------------------------------------------

% Predicado que calcula o tempo de uma encomenda a chegar com o veiculo mais ecologico(chegando o maximo a tempo possível)

melhortempo(Id_encomenda,X):-
	findall((Nome_Rua,Peso),(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,(Nodo,Pez)),
	resolve_aestrela(Nodo,A/B),
	melhorveiculo(Id_encomenda,bicicleta),
	Velocidade is 10-(Pez*0.7),
	X is (B/Velocidade)*60.
melhortempo(Id_encomenda,X):-
	findall((Nome_Rua,Peso),(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,(Nodo,Pez)),
	resolve_aestrela(Nodo,A/B),
	melhorveiculo(Id_encomenda,mota),
	Velocidade is 35-(Pez*0.5),
	X is (B/Velocidade)*60.
melhortempo(Id_encomenda,X):-
	findall((Nome_Rua,Peso),(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,(Nodo,Pez)),
	resolve_aestrela(Nodo,A/B),
	melhorveiculo(Id_encomenda,carro),
	Velocidade is 25-(Pez*0.1),
	X is (B/Velocidade)*60.

% ---------------------------------------------------------------------------------------------------

% Predicado que verifica se é possivel enviar mais do que uma encomenda no envio de uma e se sim, quais,retorna false caso a mesma ja tenha sido entregue

verifica(Id_encomenda,T):-
	findall((Peso,Nome_Rua),(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,(A,B)),
	naofoientregue(Id_encomenda),
	resolve_aestrela(B,X/Distancia),
	melhorveiculo(Id_encomenda,G),
	funcaoverifica(X,G,A,[Id_encomenda],TI),
	eliminarepetidos(TI,[],T),!.

funcaoverifica([],_,_,Z,Z).
funcaoverifica([H|T],Veiculo,Peso,A,Z):- 
	encomendasdumarua(H,Encomendas),
	verifica2(Encomendas,Veiculo,Peso,A,B,Peso2),
	funcaoverifica(T,Veiculo,Peso2,B,Z).

verifica2([],bicicleta,Peso,B,B,Peso):-
	!.
verifica2([H|T],bicicleta,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<5,
	Velocidade is (10-(Total*0.7)),
	todalista([H|A],Velocidade),
	verifica2(T,bicicleta,Total,[H|A],B,Peso2).
verifica2([H|T],bicicleta,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total>5,
	verifica2(T,bicicleta,Peso,A,B,Peso2).
verifica2([H|T],bicicleta,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<5,
	Velocidade is (10-(Total*0.7)),
	not(todalista([H|A],Velocidade)),
	verifica2(T,bicicleta,Peso,A,B,Peso2).
verifica2([],carro,Peso,B,B,Peso):-
	!.
verifica2([H|T],carro,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<100,
	Velocidade is (25-(Total*0.1)),
	todalista([H|A],Velocidade),
	verifica2(T,carro,Total,[H|A],B,Peso2).
verifica2([H|T],carro,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total>100,
	verifica2(T,carro,Peso,A,B,Peso2).
verifica2([H|T],carro,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<100,
	Velocidade is (25-(Total*0.1)),
	not(todalista([H|A],Velocidade)),
	verifica2(T,carro,Peso,A,B,Peso2).

verifica2([],mota,Peso,B,B,Peso):-
	!.
verifica2([H|T],mota,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<20,
	Velocidade is (35-(Total*0.5)),
	todalista([H|A],Velocidade),
	verifica2(T,mota,Total,[H|A],B,Peso2).
verifica2([H|T],mota,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total>20,
	verifica2(T,mota,Peso,A,B,Peso2).
verifica2([H|T],mota,Peso,A,B,Peso2):- 
	pesodaencomenda(H,PE),
	Total is PE + Peso,Total=<20,
	Velocidade is (35-(Total*0.5)),
	not(todalista([H|A],Velocidade)),
	verifica2(T,mota,Peso,A,B,Peso2).

todalista([],_).
todalista([H|T],Velocidade):- 
	prazopedidoencomenda(H,X),
	distanciaencomenda(H,Distancia),
	Tempo is Distancia/Velocidade,X>Tempo,
	todalista(T,Velocidade).

encomendasdumarua(H,X):-
	findall(Id_encomenda,(cliente(Id_cliente,_,_,H),encomenda(Id_encomenda,Id_cliente,_,_,_,_),naofoientregue(Id_encomenda)),X).

distanciaencomenda(Id_encomenda,X):-
	findall(Nome_Rua,(encomenda(Id_encomenda,Id_cliente,_,Prazo_pedido,Peso,_),cliente(Id_cliente,_,_,Nome_Rua)),Z),
	cabeca(Z,Nodo),
	resolve_aestrela(Nodo,F/X).

pesodaencomenda(Id_encomenda,X):-
	findall(Peso,encomenda(Id_encomenda,_,_,_,Peso,_),Z),cabeca(Z,X).
prazopedidoencomenda(Id_encomenda,X):-
	findall(Prazo_pedido,encomenda(Id_encomenda,_,_,Prazo_pedido,_,_),Z),
	cabeca(Z,X).

% ---------------------------------------------------------------------------------------------------

% Predicado que verifica se uma encomenda já foi entregue

naofoientregue(Id_encomenda):-
	findall(I,entrega(_,_,Id_encomenda,_,_,_,_),X),
	length(X,0). 

% ---------------------------------------------------------------------------------------------------

%Predicado que retorna o ID de todas as encomendas não entregues

naoentregues(Z):- 
	findall(Id_encomenda,entrega(_,_,Id_encomenda,_,_,_,_),X),
	findall(Id_encomenda,(encomenda(Id_encomenda,_,_,_,_,_),not(member(Id_encomenda,X))),Z).

% ---------------------------------------------------------------------------------------------------

% ------------------------------------------ALGORITMOS DE PROCURA:-----------------------------------

% ---------------------------------------------------------------------------------------------------

% Profundidade (DFS - Depth-First Search)

circuito_depth(Nodo,Destino,X,Y):-
	depthfirst(Nodo,Destino,A,B),
	circuito(A,X),Y is B*2.

depthfirst(Nodo, Destino, [Nodo|Caminho], C) :-
    profundidadeprimeiro(Nodo, Destino, [Nodo], Caminho, C).


profundidadeprimeiro(Destino, Destino,_, [], 0).
profundidadeprimeiro(Nodo, Destino, Historico, [ProxNodo|Caminho], C) :-
    adjacente(Nodo, ProxNodo, C1),
    not(member(ProxNodo, Historico)),
    profundidadeprimeiro(ProxNodo, Destino, [ProxNodo|Historico], Caminho, C2),
	C is C1 + C2.    

adjacente(Nodo, ProxNodo, C) :- 
    aresta(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- 
    aresta(ProxNodo, Nodo, C).

minimo([(P,X)],(P,X)).
minimo([(Px,X)|L],(Py,Y)):-
	minimo(L,(Py,Y)), X>Y. 
minimo([(Px,X)|L],(Px,X)):-
	minimo(L,(Py,Y)), X=<Y.

% ---------------------------------------------------------------------------------------------------
 
% Largura (BFS - Breadth-First Search)

circuito_breadth(S,X):-
	breadthfirst(S,Z),
	circuito(Z,X).

breadthfirst(Start,Solution):-
    breadthfirstaux([[Start]],Solution).

breadthfirstaux([[Node|Path]|_],[Node|Path]):-
    goal(Node).

breadthfirstaux([[N |Path]|Paths],Solution):-
    findall([M,N|Path],(adjacente( N, M,_),not(member(M,[N|Path]))),NewPaths),
    append(Paths, NewPaths, Pathsl), !,
    breadthfirstaux(Pathsl,Solution);
    breadthfirstaux(Paths,Solution).

% ---------------------------------------------------------------------------------------------------

% Busca Iterativa Limitada em Profundidade

circuito_iterativa(Node,Max,S):-
	depth_first_iterative_deepening(Node,Max,C),
	circuito(C,S).

path(Node,Node,[Node]).
path(FirstNode,LastNode,[LastNode|Path]):- 
    path(FirstNode,OneButLast,Path),
    adjacente(OneButLast,LastNode,_),
    not(member(LastNode,Path)).

depth_first_iterative_deepening(Node,Max,Solution):- 
    path(Node,GoalNode,Solution),
    goal(GoalNode),
    length(Solution,Length),
    ((Length =< Max) ; (Length > Max), !, fail).

% ---------------------------------------------------------------------------------------------------

% A* (A estrela)

% O predicado que mostra o seu circuito está apresentado na Funcionalidade 5: circuitomaisrapido


resolve_aestrela(Nodo,Caminho/Custo):-
	estima(Nodo,Estima),
	aestrela([[Nodo]/0/Estima], Caminho/Custo/_),!.

aestrela(Caminhos,Caminho):-
	obtem_melhor(Caminhos,Caminho),
	Caminho = [Nodo|_]/_/_,
	goal(Nodo).

aestrela(Caminhos, SolucaoCaminho):-
	obtem_melhor(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho). 

obtem_melhor([Caminho], Caminho):- 
	!.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
  	Custo1 + Est1 =< Custo2 + Est2, !,
  	obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor(Caminhos, MelhorCaminho).

expande_aestrela(Caminho, ExpCaminhos):-
  findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).

adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    aresta(Nodo, ProxNodo, PassoCusto),
    \+member(ProxNodo, Caminho),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo, Est).
adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    aresta(ProxNodo, Nodo, PassoCusto),
    \+member(ProxNodo, Caminho),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo, Est).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]):- 
	seleciona(E, Xs, Ys).

circuito([],[]).
circuito(Z,X):- 
	reverse(Z,[A|B]),
	append(Z,B,X).

% ---------------------------------------------------------------------------------------------------

% Gulosa

circuito_gulosa(Nodo,Caminho/Custo):-
	resolve_gulosa(Nodo,X/Z),
	circuito(X,Caminho),
	Custo is Z*2.

resolve_gulosa(Nodo, Caminho/Custo):-
    estima(Nodo, Estima),
    agulosa([[Nodo]/0/Estima], Caminho/Custo/_).

agulosa(Caminhos, Caminho) :-
    obtem_melhor_g(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosa(Caminhos, SolucaoCaminho):-
    obtem_melhor_g(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expande_gulosa(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    agulosa(NovoCaminhos, SolucaoCaminho).        

obtem_melhor_g([Caminho], Caminho):-
	!.
obtem_melhor_g([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor_g(Caminhos, MelhorCaminho).

expande_gulosa(Caminho, ExpCaminhos) :-
    findall(NovoCaminho, adjacente2(Caminho,NovoCaminho), ExpCaminhos).    

% ---------------------------------------------------------------------------------------------------