%% ------------------------------------
%% How to create app
%% http://alancastro.org/2010/05/01/erlang-application-management-with-rebar.html
%% ------------------------------------
wget http://hg.basho.com/rebar/downloads/rebar
chmod u+x rebar
./rebar create-app appid=bristol

%% ------------------------------------
%% How to create server
%% ------------------------------------
./rebar list-templates
./rebar create template=simplesrv srvid=bristol_board_server

%% ------------------------------------
%% How to create node
%% ------------------------------------
mkdir rel
cd rel
../rebar create-node nodeid=bristol_board

%% ------------------------------------
%% How to build
%% ------------------------------------
./rebar clean
./rebar compile
./rebar generate
chmod u+x rel/bristol_board/bin/bristol_board

%% ------------------------------------
%% How to run (run in dtach)
%% ------------------------------------
./rel/bristol_board/bin/bristol_board console
