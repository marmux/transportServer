-module(mod_getGPSPosition).
-behaviour(gen_mod).
-export([start/2,stop/1]).
-export([on_filter_packet/1]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("transport.hrl").
-define(EJABBERD_DEBUG, true).


start(_Host, _Opts) ->
    ?INFO_MSG("mod_getGPSPosition starting ... \n", []),
    ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 0).

stop(_Host) ->
    ?INFO_MSG("mod_getGPSPosition stopping ... \n", []),
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, on_filter_packet, 0).


fn_get_transport_command(Xdata,CmdStr) -> 
    DataStr = erlang:bitstring_to_list(Xdata),
    ?TR_DEBUG("fn_get_transport_command ~p in Xdata: ~p ~n",[CmdStr,Xdata]),
    CmdStrStart = "<"++CmdStr++">", 
    CmdStrEnd = "</"++CmdStr++">",
    CmdIndexStart = string:str(DataStr,CmdStrStart),
    CmdIndexEnd = string:str(DataStr,CmdStrEnd), 
    {IsCommand,Str_attributes} = 
	if (CmdIndexStart>0) and (CmdIndexEnd>0) and (CmdIndexStart<CmdIndexEnd) ->
		?TR_DEBUG("fn_get_transport_command command found ~p~n",[CmdStr]),
		Int_len_start = CmdIndexStart + string:len(CmdStrStart),
		Values_lenght = CmdIndexEnd - Int_len_start,
		Str_values = string:substr(DataStr, Int_len_start, Values_lenght),
		{true,Str_values}; 
	   true -> {false,""}
	end, 
    {IsCommand,Str_attributes}.


fn_get_command(Body) -> 
    case Body of
	{xmlel,<<"body">>,[],[{xmlcdata,Xdata}]} ->
	    fn_get_command(Xdata,?Transport_commands);
	_ -> {empty,""}
    end.
fn_get_command(_,[]) -> {empty,""};
fn_get_command(Xdata,[ {TrCmdH,TrCmdStr,_,_,_,_} | TrCmdT]) ->
    {IsCommand,Str_attributes} = fn_get_transport_command(Xdata,TrCmdStr),
    if 
	IsCommand -> {TrCmdH,Str_attributes};
	true -> fn_get_command(Xdata,TrCmdT)
    end.


send_ack_response(From, To, StrMsg) -> 
    %%ReceiptId = xml:get_tag_attr_s(<<"id">>, Body),
    SendTo = jlib:jid_to_string(To),
    BitStrMsg = erlang:list_to_bitstring(StrMsg),
    XmlBody = {xmlel,<<"message">>,
	       [{<<"xml:lang">>,<<"en">>},
		{<<"type">>,<<"chat">>},
		{<<"to">>,SendTo}  %{<<"id">>,<<"ab33a">>}
	       ],
	       [{xmlcdata,<<"\n">>},
		{xmlel,<<"body">>,[],[{xmlcdata,BitStrMsg}]},
		{xmlcdata,<<"\n">>},
		{xmlel,<<"active">>,
		 [{<<"xmlns">>,<<"http://jabber.org/protocol/chatstates">>}],
		 []},
		{xmlcdata,<<"\n">>}]},
    ejabberd_router:route(From, To, XmlBody).


fn_get_cmd_attribute_fields(TrCmd) ->
    ?TR_DEBUG("fn_get_cmd_attribute_fields command ~p~n",[TrCmd]),
    [CmdFields|_]= lists:filter(
		     fun({Cmd,_,_,_,_,_}) -> Cmd == TrCmd end,
		     ?Transport_commands),
    ?TR_DEBUG("fn_get_cmd_attribute_fields ~p~n",[CmdFields]),
    CmdFields.

fn_get_cmd_response_msg(TrCmd,Status) ->
    CmdFields = lists:filter(
		  fun({Cmd,_,_,_,_,_}) -> Cmd == TrCmd end,
		  ?Transport_commands),
    case Status of
	success -> [{TrCmd,_,_,Msg,_,_}|_] = CmdFields, Msg;
	failure -> [{TrCmd,_,_,_,Msg,_}|_] = CmdFields, Msg
    end.

fn_get_attr_from_str(AttrStr, StrData) ->
    AttrStrToSearch = AttrStr++"=",
    AttrIndexStart = string:str(StrData,AttrStrToSearch),
    {Found,Value} = 
	if (AttrIndexStart>0) ->
		?TR_DEBUG("fn_get_attr_from_str attr found ~p~n",[AttrStr]),
		Int_len_start = AttrIndexStart + string:len(AttrStrToSearch),
		Values_lenght = string:len(StrData) - Int_len_start + 1,
		Str_value = string:substr(StrData, Int_len_start, Values_lenght),
		{true,Str_value}; 
	   true -> {false,""}
	end, 
    {Found,Value}.


fn_get_attr_from_strList(AttrStr,StrDataCSV) ->
    StrDataList = string:tokens(StrDataCSV,","),
    Results = lists:map(
		fun(X) -> fn_get_attr_from_str(AttrStr,X) end,
		StrDataList),
    FoundAttr = lists:filter(
		  fun({Fo,_}) -> Fo end,
		  Results),
    ?TR_DEBUG("fn_get_attr_from_strList attr ~p, found ~p~n",[AttrStr,StrDataList]),
    ?TR_DEBUG("fn_get_attr_from_strList FoundAttr ~p~n",[FoundAttr]),
    case FoundAttr of
	[] -> {false,""};
	[{Found,Value}|_] -> {Found,Value}
    end.

fn_get_cmd_attribute(TrAttrStr,AttStrTuple) ->
    {AttrName,AttrStr,Required} = AttStrTuple,
    {Found,Value} = fn_get_attr_from_strList(AttrStr,TrAttrStr),
    %%if att is required i.e. obl then returns false
    case Required of 
	obl ->  if 
		    Found -> {true,AttrName,AttrStr,Value};
		    true -> {false,AttrName,AttrStr,""}
		end;
	opc -> {true,AttrName,AttrStr,Value}
    end.


fn_get_cmd_attributes(TrCmd,TrAttrStr) ->
    {TrCmd,_,_,_,_,Attrs} = fn_get_cmd_attribute_fields(TrCmd),
    LoadedAttrs = lists:map(
		    fun(X) ->  fn_get_cmd_attribute(TrAttrStr,X) end,
		    Attrs),
    Required = lists:filter(
		  fun({Found,_,_,_}) -> Found == false end,
		  LoadedAttrs),
    case Required of
	[] -> {success,LoadedAttrs};
	[{_,_,AttrStr,[]}|_] -> Msg = "Attribute "++AttrStr++" is not optional", 
				{failure, Msg}
    end.


fn_get_attr_val_from_tuple(AttrTuples, Attr) ->
    AttrTuple = lists:filter(fun({_,TrAttr,_,_}) -> TrAttr == Attr end, 
			     AttrTuples),    
    case AttrTuple of
	[] -> 
	    ?TR_DEBUG("fn_get_attr_val_from_tuple Attr ~p not found in ~p ~n",
		     [Attr, AttrTuples]), 
	    "";
	[{_,_,_,Val} |_] -> Val
    end.
		

fn_process_command(From,_To,TrCmd,TrAttrStr) ->
    case TrCmd of
	gpspos -> 
	    ?TR_DEBUG("mod_getGPSPosition fn_proccess_command ~p and atrributes ~p~n",[TrCmd,TrAttrStr]),
	    {Status,LoadedAttrs} = fn_get_cmd_attributes(gpspos,TrAttrStr),	    
	    Msg = fn_get_cmd_response_msg(TrCmd,Status),
	    case Status of
		success ->
		    Lat = fn_get_attr_val_from_tuple(LoadedAttrs,lat),
		    Lon = fn_get_attr_val_from_tuple(LoadedAttrs,lon),
		    Time = fn_get_attr_val_from_tuple(LoadedAttrs,time),
		    %%{FromUser,_,_} = jid:split(From),
		    mod_transport_commands:set_gps(From,Lat,Lon,Time),
		    send_ack_response(From, From, Msg),
		    drop;
		failure -> 
		    send_ack_response(From, From, Msg), 
		    drop
	    end;
	getmucs ->
	    ?TR_INFO("mod_getGPSPosition fn_proccess_command ~p and atrributes ~p~n",[TrCmd,TrAttrStr]),
	    Msg = fn_get_cmd_response_msg(TrCmd,success),
	    mod_transport_commands:get_mucs(From),
	    send_ack_response(From, From, Msg),
	    drop;
	empty -> forward
    end.


on_filter_packet(drop) -> drop;
on_filter_packet({From, To, XML } = Packet) ->
    ?TR_DEBUG("Filtering Packet ~p~n",[Packet]),
    case XML of
    	{xmlel, <<"message">>,_,_} ->
	    Body = fxml:get_subtag(XML, <<"body">>),
	    case Body of
		false -> Packet;
		_ -> 
		    ?TR_DEBUG("Body from ~p~n",[Body]),
		    {TrCmd,TrAttStr} = fn_get_command(Body),
		    Action = fn_process_command(From,To,TrCmd,TrAttStr),
		    case Action of
			forward -> Packet;
			drop -> drop
		    end
	    end;
	_ -> Packet
    end.
