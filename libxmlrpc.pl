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
	% TODO: Read request data, parse XML, call registered predicate requested by remote procedure call
	% and create response XML DOM.

	current_output(O),
	set_stream(O,encoding(utf8)),
	write('Content-type: application/xml'),
	nl,nl,
	write('<?xml version="1.0" encoding="UTF-8"?>'),
	% TODO: Send an actual response.
	xml_write(O,element('response',[],['Response placeholder text']),[header(false)]).
