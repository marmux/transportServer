-define(Userfeedback,true).

%%setting TRDEBUG to undefined will not display debug messages
-undef(TRDEBUG). 
%%-define(TRDEBUG,true).


-type(transport_command() :: 
	gpspos | chatmsg | adduser | addunit | help | empty).

-type(transport_attribute() :: 
	lat | lon | userId | msg | chatmsg_to | passwd | unitId | 
	placa | unitDesc).

-type transport_answer() :: success | failure.

-define(TransportCommandInd,"<transportCommand>").
-define(Max_users_line, 100).


-define(TR_INFO(MsgStr,Args),?INFO_MSG("TR_INFO:"++MsgStr,Args)).

-ifndef(TRDEBUG).
-define(TR_DEBUG(MsgStr,Args), true).
-else.
-define(TR_DEBUG(MsgStr,Args),?INFO_MSG("TR_DEBUG:"++MsgStr,Args)).
-endif.


%%here list available commands 
%%{cmdtype,cmdstr,Description including %%format,ans succ, ans failure }

-define(Transport_commands,
	[
	 {gpspos,
	  "gpspos",
	  "Use <gpspos>lat=Double,lon=Double</gpspos>",
	  ?TransportCommandInd++"gps received succesfully",
	  ?TransportCommandInd++"error in processing gps position",
	  [{lat,"lat",obl},{lon,"lon",obl},{time,"time",obl}]
	 },
	 {
	   getmucs,
	   "getmucs",
	   "Use <getmucs></getmucs>",
	   ?TransportCommandInd++"processing request async getmucs",
	   ?TransportCommandInd++"error in processing getmucs",
	   []
	 },
	 {chatmsg,
	  "chatmsg",
	  "Use <chatmsg>to=UserId,text=String</chatmsg> to send a chat message", 
	  ?TransportCommandInd++"chat delivered successfully",
	  ?TransportCommandInd++"error in delivering chat",
	  [{userId,"userId",obl},{text,"text",obl}]
	 },	 
	 {adduser,
	  "adduser",
	  "Use <adduser>userId=Int,passwd=String,unitId=Int</adduser>",
	  ?TransportCommandInd++"User added succesfully",
	  ?TransportCommandInd++"Error in adding user",
	  [{userId,"userId",obl},{passwd,"passwd",obl},{unitId,"unitId",obl}]
	 },
	 {addunit,
	  "addunit",
	  "Use <addunit>unitId=Int,placa=String,Descripcion=String</addunit>",
	  ?TransportCommandInd++"Unit added succesfully",
	  ?TransportCommandInd++"Error in adding unit",
	  [{unitId,"unitId",obl},{placa,"placa",obl},{unitDesc,"unitDesc",opc}]
	 }, 
	 {help,
	  "help",
	  "Use as:",
	  ?TransportCommandInd,
	  ?TransportCommandInd,
	  []
	 }
	]
       ).

-define(User_commands, 
	[
	 {gpspos,"<gpspos>"},
	 {mucs,"<mucs>"}
	]
       ).


%% -define(BUSLINES,[
%% 		  {antiquilla,"antiquilla"},
%% 		  {vallecito,"vallecito"}  		  
%% 		 ]).
