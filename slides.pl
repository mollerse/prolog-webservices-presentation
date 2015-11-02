%Slide-engnie based on Michael Hendricks Strangeloop 2014 talk
%https://github.com/mndrix/StrangeLoop2014/blob/master/slides/sessions/ProductionProlog-MichaelHendricks.pl

:- use_module(library(sweet), except([in/2])).
:- op(200, fy, (#)).
:- use ansi_term.
:- use clpfd.
:- use list_util.
:- use tty.

:- discontiguous slide/1.

go :-
  once(clause(slide(Name),_)),
  go(Name).

go(Start) :-
  nb_setval(previous_slide,no_previous_slide),
  bagof(Name, Body^clause(slide(Name),Body), Names),
  drop_while(\=(Start), Names, Slides),
  member(Slide,Slides),
  nb_setval(previous_slide, Slide),
  slide(Slide).

resume :-
  nb_getval(previous_slide,Slide),
  Slide \= none,
  go(Slide).

slide(cover) :-
  # "Web Services


         ...in Prolog?



BEKK-Fagdag, 6. November 2015
@mollerse".

% Hei! Jeg heter Stian Møllersen og i dag skal jeg fortelle dere en historie om
% et av mine favorittspråk, nemilig Prolog. En historie om Web Services.

slide(facinasjon) :-
  # "Prolog har alltid skremt og fascinert meg".

% Prolog har alltid fascinert meg, og ofte skremt meg litt. Helt siden jeg ble
% introdusert for det i et fag i 2. eller 3. klasse på universitetet har det
% romstert rundt i underbevistheten. Et språk som var så radikalt anderledes enn
% alt jeg hadde sett før. Et språk som fikk meg til å stille spørsmål ved
% virkeligheten. Kunne dette faktisk fungere?

slide(logisk) :-
  # "Prolog er programmering med logikk

Det ultimate deklarative språket".

% Grunnen til at Prolog skiller seg sånn ut er at Prolog er et logisk
% programmerings språk. Logisk programmering er et programmeringsparadigme, mindre
% kjent enn sine søsken imperativ og funksjonelt, som går ut på å løse problemer
% gjennom bruk av logiske predikater. Et predikat er en sann påstand om en ting.
% Dette gjør at logikk blir det ultimate deklarative språket. All problemløsning
% foregår ved å finne hvilke predikater som er sanne gitt en ting, og på den måten
% komme med påstander.

slide(cssconf1) :-
  # "utviklere + øl = wierd shit".

% Som så mange gode historier, starter også denne med kombinasjonen av utviklere
% og rusmidler. Det var fest etter CSSConf i Berlin i september i år, og en gjeng
% med utviklere sitter rundt et bord og snakker om ting vi har lyst til å lage som
% vil gjøre oss rike og mektige utviklere. Naturligvis er alt web services,
% because reasons.

slide(cssconf2) :-
  # "\"Kan jeg lage en web service i Prolog?\"
- meg".

% Som så mange ganger før sniker Prolog seg frem i bevistheten min, og ideen om å
% lage en web service i Prolog dukker opp. Som kaptein Ahab og den hvite hvalen,
% fortsetter Prolog å tirre meg til å løse tilsynelatende upassende
% problemstillinger med det.

slide(cssconf3) :-
  # "en kveld og en lang lunsjpause senere...".

% Jeg går selvfølgelig fem på, og etter en sen kveld og en litt for lang
% lunsjpause etterpå har jeg noe som fungerer.

slide(erdetfredag) :-
  # "erdet.fredag.pl".

% Og slik oppstod den fantastiske tjenesten erdet.fredag.pl

% `curl erdet.fredag.pl` i annen buffer

% Dette kommer nok ikke til å gjøre meg rik akkurat, men det var enormt
% tilfredsstillende å beseire prolog nok en gang.

slide(bigger) :-
  # "Vi kan jo løse skikkelige problemer med dette!".

% Den neste tanken som slo meg var at dette her kan vi jo løse skikkelige
% problemer med! Prolog, i all sin underfundinghet, er faktisk veldig godt egna
% til å løse en del problemstillinger.

slide(microservices) :-
  # "Vi lever i en verden av mikrotjenester".

% What the shit liksom, vi lever jo tross alt i en verden av mikrotjenester

slide(tools) :-
  # "Burde vi ikke kunne velge beste verktøy?".

% Det burde jo være plass til Prolog i verktøykassa hvis Prolog faktisk er best
% egna til å løse problemet.

slide(constraints1) :-
  # "Constraint Satisfaction Problems...".

% Ta Constraint Satisfaction problemer feks. Den typen problemer er vanvittig
% godt egna for et språk som er ekstremt deklarativt og samtidig er istand til å
% utforske løsningsrom helt selv.

% demo av CSP

demoCSP(_, _) :-
  writeln("4*X + 2*Y #= 24,").

demoCSP(_, _) :-
  writeln("X + Y #= 9,").

demoCSP(_, _) :-
  writeln("[X,Y] ins 0..sup.").

demoCSP(X, Y) :-
  4*X + 2*Y #= 24, X + Y #= 9, [X,Y] ins 0..sup.

slide(constraints2) :-
  # "Hvilke andre problemer er CSP?".

% Hvilke supervanskelige problemer er det som også passer inn under paraplyen
% constraint satisfaction? Vi har layout av nettsider, men det er kanskje litt for
% mye å svelge akkurat nå. Et annet problem som også passer er et spill som er
% veldig populært, og det er jo noe som definitivt burde gi meg masse fame and
% fortune og twitterfølgere.

slide(constraints3) :-
  # "Sudoku!".

% Sudoku! I tillegg til å være supervanskelig å uttale er det også et spill som
% går ut på å tilpasse en løsning til et sett med begrensninger. Alle elementer må
% være mellom 1 og 9, alle elementer på en rad må være unike, etc.

slide(webservices) :-
  # "Jeg eksponerer en Sudoku-solver på internett!".

% Jeg hadde heldigvis en Sudoku-solver liggende fra et tidligere møte med min
% hvite hval. Hva om jeg eksponerte denne på internett? Det må jo gjøre meg rikg
% og mektig, eller om ikke annet gi meg en artig utfordring.


slide(krav) :-
  %Significant whitespace for some basic formatting
  # "Ny funksjonalitet:
- application/json            
- POST                        
- CORS*                       


*by popular demand             ".

% Jeg satte sammen en kravspec til webtjenesten min. Jeg kunne allerede håndtere
% superenkle GET-requests, men denne gangen var jeg nødt til å uppe gamet mitt og
% håndtere JSON over POST requests og, by popular demand, CORS støtte for de som
% ville bruke tjenesten min til å løse Sudokus i nettlesern for fun and profit

slide(json1) :-
  # "Første challenge: JSON".

slide(json) :-
  # "SWI-Prolog got your back:

:- use_module(library(http/http_json)).

http_read_json/2                       
reply_json/1                           ".

% Første utfordring: JSON. Selvfølgelig har SWI-prolog folka en løsning på
% dette. I bibloteket http_json ligger det predikater for å kunne lese _og_ skrive
% JSON. Da var det problemet ute av verden, next!

slide(post) :-
  # "Håndtere POST-requests?".

% Vi må håndtere POST-requests på en god måte. Ellers får vi ingen data.

% print_post_handler.

% Her ser vi at vi sjekker om Requesten er en POST-request, vi parser innholdet
% til en JSON, fyrer det avgårde til solvern vår og svarer med svaret.

slide(cors0) :-
  # "Next up: CORS".

slide(cors1) :-
  # "CORS krever et svar på OPTIONS-requests".

% Også trenger vi CORS headers. Med CORS så er vi nødt til å svare på
% OPTIONS-requests. Det vil kreve at vi håndterer en ekstra type request på samme
% URL. Tricky.

slide(cors2) :-
  # "SWI-Prolog leverer varene:

:- use_module(library(http/http_cors)).

cors_enable/2                          
cors_enable/0                          ".

% Nok en gang har SWI-Prolog folkene levert varene. Med bibloteket http_cors får
% vi muligheten til å både svare med CORS-kapabiliteter og legge på enkel
% CORS-info på alle requests.

% print_handlers x2.

slide(error) :-
  # "Vi trenger litt feilhåndtering også".

% Og what the hey, vi slenger på litt feilhåndtering også, bare for å være gode
% netizens.

% print_handlers x3.

% Her ser vi noe av magien til prolog. Hvordan fungerer egentlig dette? En
% fullstendig forklaring av Prologs mange finurligheter er utenfor scopet til
% denne historien, men kom å finn meg etterpå med en øl så skal du få høre!

slide(sudoku) :-
  # "Nå kan vi løse Sudoku puzzles!".

% Og nå har jeg en fullverdig Sudoku-solver. I tidsriktig ånd stappa jeg hele
% sulamitten i en Docker-container, slang den opp på en Digital Ocean droplet
% bak litt nginx-config. Og voiala:

% demo av solver i annen buffer

% Now witness the power of this fully armed and operational sudoku solver!

slide(github) :-
  # "github/mollerse/sudoku.pl".

% For de som ikke har fått skrekken finnes all koden på github

slide(poeng1) :-
  # "Prolog er nok ikke det neste Node.js.

Så hvorfor gidder jeg?".

% Dette er tidspunktet i historien hvor jeg forteller om alle pengene og
% twitter-følgerne jeg fikk etter at jeg slapp dette prolog-beistet løs på det
% intetanende internettet. Den triste sannheten er at Prolog blir neppe det neste
% Node, eller Go eller Ruby -- så hvorfor gidder jeg dette?

slide(poeng2) :-
  # "\"A language that doesn’t affect the way you think
about programming is not worth knowing.\"

- Alan Perlis".

% En smarting ved navn Alan Perlis sa en gang at "A language that doesn't affect
% the way you think about progamming is not worth knowing." En orntli luring han
% Alan. Men det er her punchlinen for historien ligger. Å løse problemer med et
% språk som er så radikalt forskjellig fra de tingene jeg jobber med til daglig
% gjør at det totale rommet av løsninger jeg er istand til å forestille meg
% utvides. Jeg blir i stand til å se andre vinklinger på problemer. Formulere
% andre løsninger. Ta med meg inspirasjon og tankemåter til de andre språkene jeg
% jobber med. Det gjør meg rett og slett til en bedre programmerer. Med andre ord,
% goodshit all around.

slide(poeng3) :-
  # "Også er det sjukt arti!".

% Også er det sjukt arti

% Takk for meg!

slide(end) :-
  tty_clear,
  tty_size(Rows,_),
  TopSpace is floor(Rows * 0.4),
  forall(between(1,TopSpace,_),nl),
  format("?- fin.~n").

#(Message) :-
  tty_clear,
  tty_size(Rows, _),

  % white space at the top
  string_height(Message, H),
  TopSpace is floor((Rows- 2)/2) + floor(H/2),
  n(TopSpace, nl),

  split_string(Message, "\n", "", Messages),

  maplist(write_msg, Messages), !,

  % white space at the bottom (pushing "true" downwards)
  BottomSpace is (Rows - TopSpace - 2),
  n(BottomSpace,nl).

write_msg(Message) :-
  tty_size(_, Cols),
  % the message itself
  string_length(Message, N),
  SpaceCount is floor((Cols - N) / 2),
  n(SpaceCount, write(" ")),
  ansi_format([bold, fg(cyan)], Message, []),
  nl.

:- meta_predicate n(+,0).
n(N, Goal) :-
  forall(between(1,N,_),Goal).

string_height(Str, N) :-
  string_codes(Str, Chars),
  num_nl(Chars, N).

num_nl([], 1).
num_nl([10|T], N) :-
  num_nl(T, N1),
  N is N1 + 1.
num_nl([_|T], N) :- num_nl(T, N).

print_post_handler :-
  write("sudoku_handler(Req) :-
  option(method(post), Req), !,
  http_read_json_dict(Req, Data, []),
  solve(Data, Solution),
  reply_json(Solution).").

print_handlers :-
  write("sudoku_handler(Req) :-
  option(method(post), Req), !,
  http_read_json_dict(Req, Data, []),
  solve(Data, Solution),
  cors_enable,
  reply_json(Solution).").

print_handlers :-
  write("sudoku_handler(Req) :-
  option(method(options), Req), !,
  cors_enable(Req, [ methods([post]) ]),
  format('~n').").

print_handlers :-
  write("sudoku_handler(_) :-
  throw(http_reply(
    server_error('Method not supported. Only POST.')
  )).").

% Console demos
% curl erdet.fredag.pl

% curl -H "Content-Type: application/json" -X POST -d '{
%   "puzzle": [
%     [8,"?","?","?","?","?","?","?","?"],
%     ["?","?",3,6,"?","?","?","?","?"],
%     ["?",7,"?","?",9,"?",2,"?","?"],
%     ["?",5,"?","?","?",7,"?","?","?"],
%     ["?","?","?","?",4,5,7,"?","?"],
%     ["?","?","?",1,"?","?","?",3,"?"],
%     ["?","?",1,"?","?","?","?",6,8],
%     ["?","?",8,5,"?","?","?",1,"?"],
%     ["?",9,"?","?","?","?",4,"?","?"]
%   ]
% }' sudoku.mornster.net
