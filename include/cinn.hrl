%% -*- mode:erlang -*-

-ifndef(HEADER_CINN).
-define(HEADER_CINN, true).

-type hostname() :: list().
-type key() :: list().
-type value() :: term().

-define(ZABBIX_HEADER, <<"ZBXD", 1:8/integer>>).

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
