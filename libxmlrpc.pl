:- module(xmlrpc,[
		server/0,	% Start server with default port of 8080.
		server/1,	% Start server with an arbitrary port number.
		%server/2,	% Start server with SSl context.
		register/1	% Load module.
	]).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_header')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_session')).
:- use_module(library('http/http_error')).
:- use_module(library('http/http_client')).
:- use_module(library('sgml')).
:- use_module(library('sgml_write')).
:- use_module(library('xpath')).

:- http_handler('/', reply, []).

% Register modules, which contain predicates to be called when requested by the client.
% register(+Module)
register(Module) :- use_module(Module).

% Predicates to start a server
server :- server(8080).
server(Port) :- http_server(http_dispatch, [port(Port)]).
%server(Port,SSLContext) :- http_server(http_dispatch,[port(Port),ssl(SSLContext)]).

% Predicate to handle requests
reply(Request) :-
	member(method(post),Request),!,
	get_post_data(DOM,Request),
	xpath(DOM,//methodCall/methodName,M),
	M=element(methodName,[],[M1]),
	atom_string(M1,M2),
	split_string(M2,".","",[MS,PS]),
	atom_string(Module,MS),
	atom_string(Predicate,PS),
	current_module(Module),
	current_predicate(Predicate/2),
	atom_string(Call,Predicate),
	extract_parameters(Parameters,DOM),
	% Call predicate from module specified by class and method
	call(Call,Results,Parameters),
	% Create response
	create_response(Response_DOM,Results),
	current_output(O),
	set_stream(O,encoding(utf8)),
	write('Content-type: text/xml'),
	nl,nl,
	write('<?xml version="1.0" encoding="UTF-8"?>'),nl,
	xml_write(O,Response_DOM,[header(false)]),nl.

% extract(-Parameters,+List_of_elements
extract([],[]).
extract(Parameters,[element(value,[],[Value])|More_elements]) :-
	extract(PX,More_elements),
	(
		(number(Value),Parameters=[Value|PX]);
		(Value=element(double,[],[F]),atom_number(F,V),Parameters=[V|PX]);
		(Value=element(i4,[],[I]),atom_number(I,V),Parameters=[V|PX])
	).

% extract_parameters(-Parameters,+DOM)
extract_parameters(Parameters,DOM) :-
	xpath(DOM,//methodCall/params/param,P1),
	P1=element(param,[],P2),
	extract(Parameters,P2).

% get_post_data(-DOM,+Request)
get_post_data(DOM,Request) :-
	http_read_data(Request,Data,[to(string)]),
	new_memory_file(M_file),
	open_memory_file(M_file,write,Write_stream),
	write(Write_stream,Data),
	close(Write_stream),
	open_memory_file(M_file,read,Read_stream),
	% Parse XML
	load_xml(Read_stream,DOM,[space(remove)]),
	close(Read_stream),
	free_memory_file(M_file).

% create_response(-Response_DOM,+Results)
create_response(Response_DOM,Results) :-
	TMP=[element(methodResponse, [], [element(params, [], [element(param, [], [element(value, [], [Element])])])])],!,
	(
		(float(Results),Element=element(double, [], [Results]));
		(integer(Results),Element=element(i4,[],[Results]));
		(string(Results),Element=element(string,[],[Results]));
		(atom(Results),Element=Results)
	),
	Response_DOM=TMP.
