-module(mod_transport_commands).
-author('marmux@gmail.com').
-behaviour(gen_mod).
-behaviour(gen_server).
-define(INTERVAL, 20000). % 20 seconds
%%API
-export([start_link/2, start/2, stop/1, 
	 set_gps/4, 
	 broadcast_gps/1,
	 set_busline/1,
	 get_mucs/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("transport.hrl").
-define(EJABBERD_DEBUG, true).
-define(PROCNAME, mod_transport_commands).


start_link(_Host, _Opts) ->
    ?INFO_MSG("mod_transport_commands start_link starting ~p ... ~n",
	      [?MODULE]),     
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Host, Opts) ->
    ?INFO_MSG("mod_transport_commands start starting ~p ... ~n", [?MODULE]),
    start_link(Host,Opts).   

stop(_Host) ->
    ?INFO_MSG("mod_transport_commands stopping ~p ... ~n", [?MODULE]).


init([]) -> %Start first timer
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, dict:new()}.

handle_info(trigger,State) ->
    broadcast_gps([]),      
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.




%%TRANSPORT COMMANDS

%%IT MAINTAINS THE STATE (DICT) OF USERS AND THEIR GPS POS
set_gps(BareJid, Lat, Lon, Time) ->
    gen_server:cast(?MODULE, {set_gps, BareJid, Lat, Lon,Time}).

%%BROADCASTING GPS POSSITIONS FOR ALL LINES
broadcast_gps(Args) ->
    gen_server:cast(?MODULE, {broadcast_gps, Args}).

%%CONFIGURE A BUS LINE, TAKES A STRING FILE DESCRIPTOR
set_busline(FileName) ->
    gen_server:cast(?MODULE, {set_busline, FileName}).

get_mucs(Jid) ->
    gen_server:cast(?MODULE, {get_mucs, Jid}).





%%HANDLE
handle_call(stop, _From,State) -> {stop, normal, ok, State}.


handle_cast({set_gps, Jid, Lat, Lon, Time}, State) ->
    NewState = dict:store(Jid,{Lat,Lon,Time},State),
    ?TR_DEBUG("mod_transport_commands set_gps new state ~p~n", [NewState]),
    {noreply, NewState};
handle_cast({broadcast_gps, _Args}, State) ->
    ?TR_DEBUG("mod_transport_commands broadcast_gps state ~p~n", [State]),
    _Result = broadcast_allgps(State),
    {noreply, State};
handle_cast({set_busline, FileName}, State) ->
    ?TR_DEBUG("mod_transport_commands set with file ~p~n", [FileName]),
    _Result = fn_set_busline(FileName),
    {noreply, State};
handle_cast({get_mucs, Jid}, State) ->
    ?TR_DEBUG("mod_transport_commands  getting mucs for ~p~n", [Jid]),
    _Result = fn_get_mucs(Jid),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.






%%IMPLEMENTATION 
%%%%%%%%%%%%%%%%

fn_get_user_command_str(Cmd) ->
    [{_,CmdStr}|_] = lists:filter(fun({C,_S}) -> C == Cmd end, ?User_commands),
    CmdStr.

%%GETTING MUCS FOR A USER
fn_send_message(From, To, StrMsg) -> 
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


fn_user_in_room(UserJid,RoomBin,MucServiceBin) ->
    RoomStr = binary_to_list(RoomBin),
    UserBinF = jid:to_string(UserJid),
    UserStrF = binary_to_list(UserBinF),
    [UserStr,_Rest] =  string:tokens(UserStrF,"@"),
    [MucRoom, _MucService] = string:tokens(RoomStr,"@"),
    User = list_to_binary(UserStr),
    Room = list_to_binary(MucRoom),
    All_users = mod_muc_admin:get_room_affiliations(Room,MucServiceBin),                               
    %%tuples like {<<"bot1">>,<<"localhost">>,member,<<>>},
    In_room = lists:any(fun({Usr,_,_,_}) -> Usr == User end, All_users),
    In_room.				

fn_filter_room_in_rooms(Room,Rooms) ->
    lists:filter(fun(Rm) -> Room /= Rm end,Rooms).

fn_get_mucs(Jid) ->
    %getting mucs
    [Host|_] = ejabberd_admin:registered_vhosts(),
    Online_rooms = mod_muc_admin:muc_online_rooms(Host),
    Unused_rooms = mod_muc_admin:rooms_unused_list(Host,0),    
    %remove duplicates
    Fil_unused = lists:foldl(
		   fun(X,Acc) -> fn_filter_room_in_rooms(X,Acc) end,
		   Unused_rooms,
		   Online_rooms),
    All_mucs = lists:append(Online_rooms,Fil_unused),
    %%getting users in all mucs
    HostStr = binary_to_list(Host),
    MucServiceStr =  string:concat("conference.",HostStr),
    MucServiceBin = list_to_binary(MucServiceStr),
    All_mucs_with_user = 
	lists:filter(
	  fun(Room) -> fn_user_in_room(Jid,Room,MucServiceBin) end,
	  All_mucs),
    ?TR_DEBUG("mod_transport_commands fn_get_mucs mucs for user ~p ~n", 
	     [All_mucs_with_user]),
    %prepare message
    CmdStr = fn_get_user_command_str(mucs),
    MsgStr = string:concat(?TransportCommandInd++CmdStr,
			   lists:foldl(
			     fun(X,Acc) -> (binary_to_list(X)) ++"," ++ Acc end, 
			     "",
			     All_mucs_with_user)),
    fn_send_message(Jid, Jid, MsgStr),
    ok.
  



%%BROADCAST GPS POSSITIONS
fn_table_entry_to_str(TableEntry) ->
    case TableEntry of
	{Jid,Lat,Lon,Time} ->
	    JidBin = jid:to_string(Jid),
	    JidStr = binary_to_list(JidBin),
	    (JidStr ++ "," ++ Lat ++ "," ++ Lon ++ "," ++ Time);
	_ -> ""
    end.

fn_build_message_from_table(Table) ->    
    lists:foldl(fun(X,Acc) -> fn_table_entry_to_str(X) ++ "&" ++ Acc end, 
		"", Table). 

fn_send_message_to_busline(Busline, Table) ->
    case Table of
	[] -> empty;
	_  ->	  
	    BuslineStr = binary_to_list(Busline),
	    BuslineSplit = string:tokens(BuslineStr,"@"),
	    [MucRoom, MucService] = BuslineSplit,
	    MucRoomBin = list_to_binary(MucRoom),
	    MucServiceBin = list_to_binary(MucService),
	    To = jid:make(MucRoomBin,MucServiceBin,<<>>),
	    %%we take one user from table
	    [{From,_,_,_}|_] = Table,
	    CmdStr=fn_get_user_command_str(gpspos),	    
	    MsgStr = ?TransportCommandInd++CmdStr++fn_build_message_from_table(Table),
	    MsgBin = list_to_binary(MsgStr),
	    Xml = #xmlel{name= <<"message">>,
			 attrs    = [{<<"to">>, Busline},
				     {<<"type">>, <<"groupchat">>} ],
			 children = [#xmlel{name = <<"body">>,
					    attrs = [], 
					    children = [{xmlcdata,MsgBin}] }]},
	    ejabberd_router:route(From,To,Xml),
	    ok
    end.

send_message_to_buslines(MucTables) ->
    Status  = lists:map(fun({MucLine,Table}) -> 
				fn_send_message_to_busline(MucLine,Table) end,
			MucTables),
    Status.

fn_get_gps_for_user(StateDict,MucJid) ->
    %%fetch the current gps possition of a user from the state
    User = jid:from_string(MucJid),
    UserWithGps = dict:find(User,StateDict),
    case UserWithGps of
	{ok, {Lat,Lon,Time}} -> {ok,{User,Lat,Lon,Time}};
	error -> {empty_gps,{}}
    end.

fn_get_gps_for_busline(StateDict,Busline) ->
    %%gett users with gps 
    BuslineStr = binary_to_list(Busline),
    BuslineSplit = string:tokens(BuslineStr,"@"),
    [MucRoom, MucService] = BuslineSplit,
    MucRoomBin = list_to_binary(MucRoom),
    MucServiceBin = list_to_binary(MucService),  
    Users = mod_muc_admin:get_room_occupants(MucRoomBin,MucServiceBin),
    %%users of the form
    %%[{<<"bot3@localhost/Gajim">>,<<"bot3">>,"participant"}]     
    UsersWithGps = lists:map(fun({MucJid,_Nick,_RoleStr}) -> 
				     fn_get_gps_for_user(StateDict,MucJid) end,
			     Users),
    UsersWithGpsFiltered = lists:filter(
			     fun({Res,_}) -> Res == ok end,
			     UsersWithGps),
    UsersWithGpsFilteredFlat = 
	lists:map(fun({_,{User,Lat,Lon,Time}}) -> 
			  {User,Lat,Lon,Time} end,
		  UsersWithGpsFiltered),
    {Busline,UsersWithGpsFilteredFlat}.

broadcast_allgps(StateDict) ->
    %% getting host, we assume there is only one host
    [Host|_] = ejabberd_admin:registered_vhosts(),
    %% getting muc chats from host
    Mucs = mod_muc_admin:muc_online_rooms(Host),
    %% for every muc_room build a table with its users and theirs current gpspos  
    Mucs_GPSs = lists:map(fun(Muc) -> 
				  fn_get_gps_for_busline(StateDict,Muc) end,
			  Mucs),
    _Status = send_message_to_buslines(Mucs_GPSs),
    ?TR_INFO("mod_transport_commands broadcast_allgps Results ~p~n", [Mucs_GPSs]),
    ok.




%%SETTING BUSLINE
%%%%%%%%%%%%%%%%%

fn_check_room_exists(MucJidBin) ->
    [Host|_] = ejabberd_admin:registered_vhosts(),
    Online_rooms = mod_muc_admin:muc_online_rooms(Host),
    Unused_rooms = mod_muc_admin:rooms_unused_list(Host,0),
    InOnline =   lists:any(fun(MucJid) -> MucJid == MucJidBin end,
			   Online_rooms),
    InUnused =   lists:any(fun(MucJid) -> MucJid == MucJidBin end,
			   Unused_rooms),
    InOnline or InUnused.     

fn_create_muc_chat(BuslineStr) ->
    %% getting host, we assume there is only one host
    [Host|_] = ejabberd_admin:registered_vhosts(),
    HostStr = binary_to_list(Host),
    MucServiceStr =  string:concat("conference.",HostStr),
    MucRoomBin = list_to_binary(BuslineStr),
    MucServiceBin = list_to_binary(MucServiceStr),
    MucJid = jid:make(MucRoomBin,MucServiceBin,<<>>),
    MucJidBin = jid:to_string(MucJid), %%to_string returns binary!
    RoomExists = fn_check_room_exists(MucJidBin),
    case RoomExists of
	true -> 
	    ?TR_INFO("mod_transport_commands create_muc_chat: room ~p exists ~n",
		     [BuslineStr]),
	    ok;
	false ->
	    Res1 = mod_muc_admin:create_room(MucRoomBin,MucServiceBin,Host),
	    ?TR_INFO("mod_transport_commands create_muc_chat: room ~p created: ~p ~n",
		     [BuslineStr, Res1]),
	    Res2 =
		mod_muc_admin:change_room_option(MucRoomBin,MucServiceBin,<<"persistent">>,<<"true">>),	    
	    ?TR_INFO("mod_transport_commands create_muc_chat: setting properties: ~p ~n", [Res2]), 
	    ok
    end.

%% this creates a user and adds the user to the muc_room    
fn_create_user(BuslineStr, User,Pass) ->
    [Host|_] = ejabberd_admin:registered_vhosts(),
    UserBin = list_to_binary(User),
    RegisteredUsers = ejabberd_admin:registered_users(Host),    
    IsRegistered = lists:any(fun(Uid) -> Uid == UserBin end,
			     RegisteredUsers),    
    _R = case IsRegistered of
	     true ->
		 ?TR_INFO("mod_transport_commands create_user: user ~p exists ~n",
			  [User]), 
		 ok;
	     false ->
		 PassBin = list_to_binary(Pass),
		 _Res = ejabberd_admin:register(UserBin, Host, PassBin),
		 ?TR_INFO("mod_transport_commands create_user: user ~p registered successfully ~n",
			  [User]),
		 ok		       
	 end,
    %%add user to muc_chat
    HostStr = binary_to_list(Host),
    UserJid = jid:make(UserBin,Host,<<>>),
    UserStrBin = jid:to_string(UserJid),
    MucServiceStr =  string:concat("conference.",HostStr),
    MucRoomBin = list_to_binary(BuslineStr),
    MucServiceBin = list_to_binary(MucServiceStr),
    Res = mod_muc_admin:set_room_affiliation(MucRoomBin,MucServiceBin,UserStrBin,<<"member">>),
    ?TR_INFO("mod_transport_commands create_user: user ~p added to Muc ~p status ~p ~n",
	     [User,MucRoomBin,Res]), 
    ok.

fn_create_users(_BuslineStr, []) -> ok;
fn_create_users(BuslineStr, [UserT|Users]) -> 
    case UserT of 
	{User,Pass} -> 
	    _Res = fn_create_user(BuslineStr, User, Pass),
	    _Res = fn_create_users(BuslineStr, Users);
	_ -> 
	    ?TR_INFO("mod_transport_commands create_users: wrong user format", [UserT]),   
	    ok
    end.

fn_set_admin(BuslineStr,AdminT) ->
    case AdminT of
	{admin, Admin} ->
	    [Host|_] = ejabberd_admin:registered_vhosts(),
	    HostStr = binary_to_list(Host),
	    AdminBin = list_to_binary(Admin),
	    UserJid = jid:make(AdminBin,Host,<<>>),
	    UserStrBin = jid:to_string(UserJid),
	    MucServiceStr =  string:concat("conference.",HostStr),
	    MucRoomBin = list_to_binary(BuslineStr),
	    MucServiceBin = list_to_binary(MucServiceStr),
	    Res = mod_muc_admin:set_room_affiliation(MucRoomBin,MucServiceBin,UserStrBin,<<"admin">>),
	    ?TR_INFO("mod_transport_commands fn_set_admin: admin ~p set fro Muc ~p status ~p ~n",
		     [Admin,MucRoomBin,Res]),
	    ok;
	_ -> 
	    ?TR_INFO("mod_transport_commands fn_set_admin: error in admin structure",
		     []),
	    ok
    end.

fn_add_user_to_roster(UserO,UserR) ->
    UserBinO = list_to_binary(UserO),
    UserBinR = list_to_binary(UserR),
    [Host|_] = ejabberd_admin:registered_vhosts(),
    Res = mod_admin_extra:add_rosteritem(UserBinO,Host,
					 UserBinR,Host,
					 UserBinR,<<>>,<<"both">>),
    ?TR_INFO("mod_transport_commands add_useto_roster Owner ~p add ~p, status ~p ~n",
	     [UserO,UserR,Res]),
    Res.

fn_add_users_to_roster(User,Users) ->
    %%remove User from Users
    OtherUsers = lists:filter(fun(Usr) -> Usr /= User end, Users),
    _Res = lists:map(fun(Usr) -> fn_add_user_to_roster(User,Usr) end,
		     OtherUsers),
    ok.

fn_adjust_users_roster(Users) ->
    %%TODO this part, roster all to all
    UsersFlat = lists:map(fun({User,_Pass}) -> User end, Users),
    _Res = lists:map(fun(User) -> fn_add_users_to_roster(User,UsersFlat) end, UsersFlat),
    ok.

fn_process_busline_info([BuslineInfo|_]) ->
    case BuslineInfo of
	{BuslineStr,Users,Admin} ->
	    _Res = fn_create_muc_chat(BuslineStr),
	    _Res = fn_create_users(BuslineStr,Users),
	    _Res = fn_set_admin(BuslineStr,Admin),
	    _Res = fn_adjust_users_roster(Users),
	    ok;
	_ -> 
	    ?TR_INFO("mod_transport_commands wrong file structure ~p~n",[BuslineInfo]),
	    error 
    end.

fn_set_busline(FileName) ->
    {Res,BuslineInfo} = file:consult(FileName),
    case Res of
	ok -> 
	    ?TR_INFO("mod_transport_commands loaded bus info ~p~n",[BuslineInfo]),
	    _Res = fn_process_busline_info(BuslineInfo),
	    ok;
	error -> 
	    ?TR_INFO("mod_transport_commands error in reading filename ~p~n",[FileName]),
	    error
    end.

