:- module(xmlrpc,[
		server/0,	% Start server with default port of 8080.
		server/1,	% Start server with an arbitrary port number.
				% Or start HTTPS server with default port number
		%server/2,	% Start HTTPS server with arbitrary port number.
		register/1	% Load module.
	]).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_error')).
:- use_module(library('http/http_client')).
:- use_module(library('sgml')).
:- use_module(library('sgml_write')).
:- use_module(library('xpath')).

:- http_handler('/', reply, []).

% Register modules, which contain predicates to be called when requested by the client.
% register(+Module)
register(Module) :- use_module(Module).

% Start HTTP server
server :- server(8080).
server(Port) :-
	number(Port),Port>=0,Port=<65535,
	http_server(http_dispatch, [port(Port)]).

% Start HTTPS server
%server(ssl(Cert,Key,Password)) :- server(8443,ssl(Cert,Key,Password)).
%server(Port,ssl(Cert,Key,Password)) :-
%	number(Port),Port>=0,Port=<65535,
%	string(Cert),string(Key),string(Password),
%	http_server(http_dispatch,[port(Port),ssl([certificate_file(Cert),key_file(Key),password(Password)])]).

% Check if the port number is valid.
check_port(Port) :- number(Port),Port>=0,Port=<65535.

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
	call(Call,Result,Parameters),
	% Create response
	create_response(Response_DOM,Result),
	current_output(O),
	set_stream(O,encoding(utf8)),
	write('Content-type: text/xml'),
	nl,nl,
	write('<?xml version="1.0" encoding="UTF-8"?>'),nl,
	xml_write(O,Response_DOM,[header(false)]),nl.

% extract(-Parameters,+List_of_elements
extract([],[]).
extract(Parameters,[element(param,[],[element(value,[],[Value])])|More_elements]) :-
	extract(PX,More_elements),
	(
		(number(Value),Parameters=[Value|PX]);
		(Value=element(double,[],[F]),atom_number(F,V),Parameters=[V|PX]);
		(Value=element(i4,[],[I]),atom_number(I,V),Parameters=[V|PX]);
		(Value=element(string,[],[A]),atom_string(A,S),Parameters=[S|PX])
	).

% extract_parameters(-Parameters,+DOM)
extract_parameters(Parameters,DOM) :-
	xpath(DOM,//methodCall/params,P1),
	P1=element(params,[],P2),
	extract(Parameters,P2).

% get_post_data(-DOM,+Request)
% Retrieve posted XML data.
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

% create_response(-Response_DOM,+fault(faultCode(FC),faultString(FS)))
% Handling of faults detected and reported by the application.
create_response(Response_DOM,fault(faultCode(FC),faultString(FS))) :-
	Response_DOM=[element(methodResponse, [], [element(fault, [], [element(value, [], [element(struct, [], [element(member, [], [element(name, [], [faultCode]), element(value, [], [element(int, [], [FC])])]),element(member, [], [element(name, [], [faultString]), element(value, [], [element(string, [], [FS])])])])])])])].

% create_response(-Response_DOM,+Result)
% Handling of data correctly passed by the application to this library.
create_response(Response_DOM,Result) :-
	TMP=[element(methodResponse, [], [element(params, [], [element(param, [], [element(value, [], [Element])])])])],!,
	(
		(float(Result),Element=element(double, [], [Result]));
		(integer(Result),Element=element(i4,[],[Result]));
		(string(Result),Element=element(string,[],[Result]));
		(atomic(Result),Element=Result)
	),
	Response_DOM=TMP.
