:- [names].
:- use_module(library(random)).
:- dynamic usr_name/1, usr_location/1, information/2, feedback/2, alevel/1, loc/1.

chat:-
        print_welcome,
        conversations.

conversations:-
        repeat, 
        print_prompt(you),
        readin(S),
        gen_reply(S,R),
        print_prompt(me),
        write_list(R),
        is_quit(S), 
        print_report, !,halt.
gen_reply(S, R):-
        is_quit(S), !,
        responses_db(tchau, Res), 
        random_pick(Res, R).
gen_reply(S, R):-
        is_thanks(S), !,
        responses_db(thanked, Res), 
        random_pick(Res, R).
gen_reply(S, R):-
        question(Tree2, S, _Rest), 
        mapping(s2name,Tree1, Tree2), !,
        sentence(Tree1, Rep,[]),
        append(Rep, ['!'], R).
gen_reply(S, R):-
        pattern_name(S, _), !,
        responses_db(my_name, D),
        random_pick(D, R).
gen_reply(S, R):-
        pattern_my_subjects(S, _), !,
        responses_db(my_subjects, D),
        random_pick(D, R).
gen_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2how,Tree1, Tree2),
        sentence(Tree1, Rep,[]), !,
        append(Rep, ['!'], R).
gen_reply(S, R):-
        pattern_me(S, _), !,
        responses_db(me, D),
        random_pick(D, R).
gen_reply(S, R):-
        sentence(Tree1, S, _Rest), !, 
        mapping(s2why,Tree1, Tree2),
        question(Tree2, Rep,[]),
        append(Rep, ['?'], R).
gen_reply(S, R):-
        question(Tree2, S, _Rest), !, 
        mapping(s2q,Tree1, Tree2),
        sentence(Tree1, Rep,[]),
        append([yes, ','|Rep], ['!'], R).

gen_reply(S, R):-
        \+ is_question(S), 
        \+ information(_, _), !,
        get_info(4),
        responses_db(thanks, D),
        random_pick(D, R).
gen_reply(S, R):-
        \+ is_question(S), 
        \+ feedback(_, _), !,
        get_feedback(4),
        responses_db(thanks, D),
        random_pick(D, R).
gen_reply(S, R):-
        \+ is_question(S), !,
        responses_db(random_q, Res),
        random_pick(Res, R).
gen_reply(S, R):- 
        is_question(S), !,
        responses_db(random_s, Res),
        random_pick(Res, R).


is_greeting(S):-
        greeting_db(D),
        intersect(S, D, A),
        A \== [].


is_question(S):-
        member('?', S).


is_thanks(S):-
        thanks_db(D),
        intersect(S, D, A),
        A \== [].


is_quit(S):- 
        subset([tchau], S).


get_location(0).
get_location(N):-
        print_prompt(you),
        readin(L),
        M is N - 1,
        get_location(L, M).
get_location(_, 0).
get_location(X, _):-
        is_valid_loc(X, L), 
        assert(loc(L)), !.
get_location(_, N):- 
        responses_db(get_location, D),
        random_pick(D, R),
        print_prompt(me),
        write_list(R),
        M is N - 1,
        get_location(M).

is_valid_loc([H|_], L):- 
        (info(L, H); next(H,_,_,_,_)), !.
is_valid_loc([_|T], L):-
        is_valid_loc(T, L).


get_feedback(0).
get_feedback(N):-
        questions_db(feedback, D),
        nth_item(D, N, R),
        print_prompt(me),
        write_list(R),
        print_prompt(you),
        readin(S),
        assert(feedback(R, S)),
        M is N - 1,
        get_feedback(M).


get_info(0).
get_info(N):-
        questions_db(info, D),
        nth_item(D, N, Q),
        print_prompt(me),
        write_list(Q),
        print_prompt(you),
        readin(R),
        assert(information(Q, R)),
        get_info(Q, R),
        M is N - 1,
        get_info(M).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, name), !,
        get_usr_name(Q, RL).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, subjects), !,
        get_alevel_info_loop(RL).
get_info(QL, RL):-
        nth_item(QL, 1, Q),
        contains(Q, from), !,
        assert(usr_location(RL)).
get_info(_, _).

get_usr_name(Q):-
        print_prompt(you),
        readin(S),
        get_usr_name(Q, S).
get_usr_name(_, RL):-
        is_valid_name(RL), !.
get_usr_name(Q, _):-
        responses_db(get_name, D), 
        random_pick(D, X), 
        print_prompt(me),
        write_list(X),
        get_usr_name(Q).

is_valid_name(NL):-
        nth_item(NL, 1, N),
        name(N),
        assert(usr_name(N)).

get_alevel_info_loop:-
        print_prompt(you),
        readin(S),
        get_alevel_info_loop(S).
get_alevel_info_loop(S):- 
        is_valid_alevel(S), !.
get_alevel_info_loop(_):- 
        responses_db(get_alevels, D),
        random_pick(D, R),
        print_prompt(me),
        write_list(R),
        get_alevel_info_loop.


is_valid_alevel(S):- 
        alevel_db(D),
        intersect(S, D, A),
        A \== [],
        assert(alevel(A)).

print_welcome:-
        responses_db(greeting, D),
        random_pick(D, W),
        print_prompt(me),
        write_list(W), 
        flush_output. 

print_prompt(me):-
        my_icon(X), write(X), write(': '), flush_output.
print_prompt(you):-
        user_icon(X), write(X), write(': '), flush_output.
my_icon('Bot ').
user_icon('User').


random_pick(Res, R):- 
        length(Res, Length),  
        Upper is Length + 1,
        random(1, Upper, Rand),
        nth_item(Res, Rand, R).


print_report:-
        write('\n--- Relatório ---\n'),
        usr_name(X), usr_location(Y), alevel(Z), 
        write_list(['Nome do usuário: ', X, '\nDe onde: ', Y, '\nEstudando: ', Z]),
        retract(usr_name(X)),retract(usr_location(Y)), retract(alevel(Z)), fail.
print_report:-
        nl, feedback(X, Y), write(X), write(' : '), write_list(Y), 
        retract(feedback(X, Y)), fail.
print_report:-
        nl, information(X, Y), write(X), write(' : '), write_list(Y), 
        retract(information(X, Y)), fail.
print_report.



responses_db(tchau, [
        ['Tchau!'], 
        ['Espero vê-lo de novo.'], 
        ['Tenha um bom dia!'],
        ['Tchau']
        ]).

responses_db(greeting, [
        ['Olá!'], 
        ['Olá, é um prazer conhecer você.'], 
        ['Oi!'],
        ['Bem vindo!']
        ]).

responses_db(change_topic, [
        ['Se importa se eu perguntar algumas coisas?']
        ]).

responses_db(get_alevels, [
        ['Nunca ouvi falar disso!'],
        ['Isso não é uma matéria real...'],
        ['Tem certeza?'],
        ['Ok, eu preciso saber o que você está estudando!'],
        ['Não quer me falar o que está estudando?']
        ]).

responses_db(get_name, [
        ['Esse é o seu nome real?'],
        ['Isso não é o seu nome real...'],
        ['Isso não pode ser o seu nome.'],
        ['Apenas me fale seu nome...'],
        ['Preciso saber o seu nome!'],
        ['Vamos lá, qual é o seu nome?']
        ]).
            
responses_db(my_name, [
        ['My name is Frank, prazer em conhecê-lo.'],
        ['Eu sou o Frank!'],
        ['Meu nome não é importante agora.'],
        ['Frank ao seu dispor, como posso ajudá-lo?']
        ]).

responses_db(my_subjects, [
        ['Estou estudando Ciência da computação!'],
        ['Ciência da computação é ótimo.'],
        ['Esqueça sobre meus estudos...'],
        ['Ciência da computação.'],
        ['Por que você que saber o que eu estou estudando?'],
        ['Esqueça, o que você precisa?']
        ]).

responses_db(thanks, [
        ['Obrigado pela informação!'],
        ['Obrigado, isso é útil.'],
        ['Ok, obrigado.'],
        ['Obrigado por isso.'],
        ['Ótimo.'],
        ['Excelente']
        ]).

responses_db(thanked, [
        ['De nada!'],
        ['A qualquer hora.'],
        ['Feliz por ter sido útil.'],
        ['Sem problema.']
        ]).

responses_db(me, [
        ['Estou ótimo, obrigado por perguntar.'],
        ['Não posso reclmamar!'],
        ['Não tão ruim, e você?'],
        ['Estou bem, eu acho...'],
        ['Sim, eu estou bem, e você?']
        ]).

responses_db(random_q, [
        ['Oh... ok.'],
        ['...o que você quer dizer?'],
        ['Impertinente!.'],
        ['Você é um pouco rude, não?'],
        ['Não seja bobo.'],
        ['Isso é bobeira.'],
        ['Kkkkkk'],
        [':)'],
        ['Hmm.. o que?'],
        ['Desculpa?']
        ]).

responses_db(random_s, [
        ['Não sei...'],
        ['Desculpa, não posso responder essa.'],
        ['Não tenho certeza!'],
        ['Pergunta outra coisa?'],
        ['Oh, voçê vai ter que perguntar isso de alguém.'],
        ['Deculpa, eu sou apenas o Frank.'],
        ['Desculpa, Não lembro de tudo o que disse...'],
        ['Pode repetir?'],
        ['Essa é uma boa questão.'],
        ['Sabe lá!'],
        ['Não. Apenas não.'],
        ['Sim, eu concordo.']
        ]).

questions_db(feedback, [
        ['Certo. Você achou a conversa interessante?'],
        ['Hmm. Você acha que eu sou humano?'],
        ['Ok, obrigado. Eu ajudei?'],
        ['Então, como posso melhorar?']
        ]).

questions_db(info, [
        ['Certo, De onde você é?'],
        ['Haha, certo. Em qual universiade você estuda?'],
        ['Prazer em conhecê-lo. O que você está estudando?'],
        ['Qual é o seu nome?']
        ]).

greeting_db([
        olá, 
        alô,
        oi    
        ]).

thanks_db([
        obrigado,
        obrigada,
        agradecido,
        agradecida
        ]).

alevel_db([matemática,
        matematica,
        c,
        programação,
        programacao,
        java,
        algorítmos,
        algoritmos,
        engenharia,
        software,
        testes,
        prolog,
        ia,
        inteligência,
        artificial,
        estruturas,
        dados,
        arquitetura,
        poo,
        redes,
        compiladores,
        cloud,
        seguranca,
        ml,
        assembly,
        física,
        quimica,
        química,
        geografia,
        biologia,
        história,
        historia,
        psicologia,
        ingles,
        inglês,
        francês,
        frances,
        espanhol,
        alemão,
        alemao,
        italiano,
        idiomas,
        letras,
        literatura,
        religião,
        religiao,
        música,
        musica,
        danca,
        danca
        ]).


sentence( s(X,Y, is, Z) ) --> belonging_phrase(X), abstract_noun(Y),  
                              [is],  special_noun(Z).

sentence(s(X, Y, Z)) --> subject_pronoun(X), indicative_verb(Y), 
                         adjective(Z).

sentence(s(X, Y, Z)) --> subject_phrase(X), verb(Y), object_phrase(Z).

sentence(s(X, Y)) --> subject_tobe_verb(X), prepositional_phrase(Y).

sentence(s(X, Y, Z)) --> question(X), object_pronoun(Y), noun(Z).

belonging_phrase(belong(seu)) --> [seu].
belonging_phrase(belong(meu)) --> [meu].

abstract_noun(abs_noun(nome)) --> [nome].

special_noun(sp_noun(justin)) --> [justin].
special_noun(sp_noun(frank)) --> [frank].


subject_phrase(sp(X)) --> subject_pronoun(X).
subject_phrase(sp(X)) --> noun_phrase(X).

object_phrase(op(X,Y)) --> noun_phrase(X), adverb(Y).
object_phrase(op(X, Y)) --> object_pronoun(X), adverb(Y).

noun_phrase(np(X, Y)) --> determiner(X), noun(Y).
noun_phrase(np(Y)) --> noun(Y).

prepositional_phrase(pp(X, Y)) --> preposition(X), place_name(Y).

preposition(prep(dentro)) --> [dentro].
preposition(prep(em)) --> [em].
preposition(prep(no)) --> [no].
preposition(prep(na)) --> [na].
preposition(prep(de)) --> [de].
preposition(prep(do)) --> [do].
preposition(prep(da)) --> [da].


subject_pronoun(spn(eu)) --> [eu].
subject_pronoun(spn(nós)) --> [nós].
subject_pronoun(spn(tu)) --> [tu].
subject_pronoun(spn(você)) --> [você].
subject_pronoun(spn(eles)) --> [eles].
subject_pronoun(spn(ele)) --> [ele].
subject_pronoun(spn(ela)) --> [ela].
subject_pronoun(spn(isso)) --> [isso].
subject_pronoun(spn(aquilo)) --> [aquilo].
subject_pronoun(spn(quem)) --> [quem].

object_pronoun(opn(você))--> [você].
object_pronoun(opn(seu))--> [seu].
object_pronoun(opn(mim))--> [mim].
object_pronoun(opn(nós))--> [nós].
object_pronoun(opn(eles))--> [eles].
object_pronoun(opn(dele))--> [dele].
object_pronoun(opn(dela))--> [dela].

determiner(dtmnr([])) --> [].
determiner(dtmnr([um])) --> [um].
determiner(dtmnr([uma])) --> [uma].
determiner(dtmnr([o])) --> [o].
determiner(dtmnr([a])) --> [a].
determiner(dtmnr([meu])) --> [meu].
determiner(dtmnr([alguns])) --> [alguns].
determiner(dtmnr([tudo])) --> [tudo].
determiner(dtmnr([aquilo])) --> [aquilo].
determiner(dtmnr([isso])) --> [isso].

noun(noun(robotics_course)) --> [curso_de_robótica].
noun(noun(robotics_course)) --> [curso_de_computação].
noun(noun(nome)) --> [nome].

adverb(ad([muito, muito])) --> [muito, muito].
adverb(ad([como])) --> [como].
adverb(ad([])) --> [].

verb(vb(gostar)) --> [gostar].
verb(vb(amar)) --> [amar].
verb(vb(é)) --> [é].

indicative_verb(ivb(são)) --> [são].
indicative_verb(ivb(sou)) --> [sou].

subject_tobe_verb(s_2b([você, é])) --> [você, é].
subject_tobe_verb(s_2b([eu, sou])) --> [eu, sou].
subject_tobe_verb(s_2b([nós, somos])) --> [nós, somos].
subject_tobe_verb(s_2b([tu, és])) --> [tu, és].

adjective(adj(ótimo)) --> [ótimo].
adjective(adj(bom)) --> [bom].
adjective(adj(certo)) --> [certo].

question(q(porque,faz,S)) --> [porque, faz], sentence(S).
question(q(faz,S)) --> [faz], sentence(S).

question(q(X, Y, Z)) --> adverb(X), indicative_verb(Y), subject_pronoun(Z).
question( q(qual, é, X, Y ) ) -->  [qual, é],  belonging_phrase(X),  
                                     abstract_noun(Y).   

mapping(s2why, 
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(porque,faz,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2why,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(porque,faz,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).


mapping(s2q,
        s(sp(spn(N1)),vb(V),op(opn(N2),ad(X))),
        q(faz,s(sp(spn(P1)),vb(V),op(opn(P2),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1), mapping_opn(N2, P2). 
mapping(s2q,
        s(sp(spn(N1)),vb(V),op(np(noun(N2)),ad(X))),
        q(faz,s(sp(spn(P1)),vb(V),op(np(noun(N2)),ad(X)))) 
        ) :- 
        mapping_spn(N1, P1).

mapping(s2name,
        s( belong(Y1), abs_noun(X2), é, sp_noun(Y2) ),
        q(qual, é, belong(X1), abs_noun(X2) )
        ):-
        mapping_belong(X1, Y1), mapping_noun(X2, Y2).

mapping(s2how,
        s(spn(X1), ivb(Y1), adj(_)),
        q(ad(_), ivb(Y2), spn(Z2))
        ):-
        mapping_spn(X1, Z2), mapping_indicative(Y1, Y2).

mapping_belong(meu,seu).
mapping_belong(seu,meu).

mapping_noun(nome, frank).
mapping_noun(frank, nome).

mapping_indicative(é, sou).
mapping_indicative(sou, é).

mapping_ad(como, certo).
mapping_ad(certo, como).

mapping_spn(eu, você).
mapping_spn(você, eu).

mapping_opn(você,mim).
mapping_opn(mim,você).


intersect([], _, []).
intersect([H|T1], L2, [H|T3]):- 
        member(H, L2), !,
        intersect(T1, L2, T3).
intersect([_|T1], L2, L3):-
        intersect(T1, L2, L3).


write_list([]):- nl.
write_list([H|T]):- write(H), write(' '), write_list(T).


subset([], _).
subset([H|T], L2):- 
        member(H, L2),
        subset(T, L2).

nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
        nth_item(T, N1, X),
        N is N1 + 1.

contains(A, B) :-
  atom(A),
  atom(B),
  name(A, AA),
  name(B, BB),
  contains(AA, BB).
contains(A, B) :-
  atom(A),
  name(A, AA),
  contains(AA, B).
contains(A, B) :-
  sublist(B, A),
  B \= [].

sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).

pattern_name([qual, é, seu, nome, X |_], X):-!.
pattern_name([qual, seu, nome, X |_], X):-!.
pattern_name([como, você, é, chamado, X |_], X):-!.
pattern_name([quem, é, você, X |_], X):-!.
pattern_name([_|T], X):-
        pattern_name(T, X).

pattern_my_subjects([qual, assunto, está, estudando, X |_], X):-!.
pattern_my_subjects([qual, é, seu, curso, na, X |_], X):-!.
pattern_my_subjects([qual, é, seu, nível, X |_], X):-!.
pattern_my_subjects([_|T], X):-
        pattern_my_subjects(T, X).

pattern_me([como, você, está, X |_], X):-!.
pattern_me([você, está, bem, X |_], X):-!.
pattern_me([você, ok, X |_], X):-!.
pattern_me([você, bem, X |_], X):-!.
pattern_me([_|T], X):-
        pattern_me(T, X).

read_in(P):-initread(L),words(P,L,[]).

initread([K1,K2|U]):-get_code(K1),get_code(K2),readrest(K2,U).

readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(10,[]):-!.

readrest(K,[K1|U]):-K=<32,!,get_code(K1),readrest(K1,U).
readrest(_K1,[K2|U]):-get_code(K2),readrest(K2,U).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K+32. 
lc(K,K):-K>96,K<123.

readin(S):- read_in(L), my_filter(L,S).

my_filter([],[]).
my_filter(['\n'|T], R):-  !,
        my_filter(T, R).
my_filter([nb(2), X|T], [Rm|R]):- 
        name(X, CharList),
        q_followed_by_nb(CharList),!,
        name(Rm, [50|CharList]),
        my_filter(T, R).
my_filter([X|T], [X|R]):- 
        my_filter(T, R).

% auto run code
?-chat.

