-module(cinn_sender).
-author("Meysam Azad <MeysamAzad81@yahoo.com>").

-include("cinn.hrl").

%% API
-export([
         send/3
        ]).

%%====================================================================
%% API
%%====================================================================
-spec send(Hostname, Key, Value) -> {ok, Response} when
      Hostname :: hostname(),
      Key :: key(),
      Value :: value(),
      Response :: {ok, binary()} | {error, term()}.
send(Hostname, Key, Value) ->
    {ok, ZabbixIp} = get_val(zabbix_ip),
    {ok, ZabbixPort} = get_val(zabbix_port),
    {ok, Transport} = get_val(transport),

    {ok, Socket} = Transport:connect(ZabbixIp, ZabbixPort, [binary,
                                                            {packet, 0},
                                                            {active, false},
                                                            {reuseaddr, true}]),

    BinHostname = to_binary(Hostname),
    BinKey = to_binary(Key),
    BinValue = to_binary(Value),

    JsonMsg = prepare_request(BinHostname, BinKey, BinValue),
    Data = prepare_data(JsonMsg),

    Transport:send(Socket, Data),

    receive_response(Transport, Socket).

%%====================================================================
%% Internal functions
%%====================================================================
get_val(Key) ->
    application:get_env(cinn, Key).

to_binary(Element) when is_list(Element) ->
    list_to_binary(Element);
to_binary(Element) when is_atom(Element) ->
    atom_to_binary(Element, utf8);
to_binary(Element) when is_binary(Element) ->
    Element;
to_binary(Element) when is_integer(Element) ->
    integer_to_binary(Element);
to_binary(Element) when is_float(Element) ->
    float_to_binary(Element).

prepare_request(BinHostname, BinKey, BinValue) when is_binary(BinHostname),
                                                    is_binary(BinKey),
                                                    is_binary(BinValue)->
    %% zabbix protocols based on: http://zabbix.org/wiki/Docs/protocols/zabbix_sender/2.0
    _JsonMsg = jiffy:encode({[{<<"request">>, <<"sender data">>},
                              {<<"data">>,
                               [
                                {[{<<"host">>, BinHostname},
                                  {<<"key">>, BinKey},
                                  {<<"value">>, BinValue}]}
                               ]
                               }]}).

prepare_data(JsonMsg) ->
    BodyLength = byte_size(JsonMsg),

    Header = ?ZABBIX_HEADER,
    PacketLength = <<BodyLength:64/little-integer>>,
    _Data = <<Header/binary, PacketLength/binary, JsonMsg/binary>>.

receive_response(Transport, Socket) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, <<_:5/binary, _:8/binary, Body/binary>>} ->
            {ok, jiffy:decode(Body)};
        {ok, ?ZABBIX_HEADER} ->
            {ok, <<"ACCEPTED!">>};
        {error, Reason} ->
            {error, Reason}
    end.
