%% @author Asim Ihsan <asim.ihsan@gmail.com>
%% @copyright 2011 Asim Ihsan
%% @doc bb_database includes.
%% @end
%% ---------------------------------------------------------------------

%% ---------------------------------------------------------------------
%%	Config to connect to the PostgreSQL database.
%% ---------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(host, "localhost").
-define(port, 5432).
-define(username, "ubuntu").
-define(password, "password").
-define(database, "database").

-define(DB_TIMEOUT, 5000).

%% ---------------------------------------------------------------------
%%	Different PostgreSQL pool names and sizes for different purposes.
%% ---------------------------------------------------------------------

% Authorisation pool.  For verifying API keys, CRUD on users.
% Tables: institution, role, articheck_user
-define(pgpool_auth_size, 2).
-define(pgpool_auth_name, pgsql_auth_pool).

% Operations pool.  For doing tasks.
% Tables: template, printout, condition_report, art, note,
% photograph, annotation
-define(pgpool_ops_size, 6).
-define(pgpool_ops_name, pgsql_ops_pool).
