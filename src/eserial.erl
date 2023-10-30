
-module(eserial).

%% API
-export([
  open/2,
  close/1,
  recv_time/2,
  recv_length/2,
  recv_length/3,
  recv_delim/2,
  recv_delim/3,
  send/2,
  clear/1
]).

-export([init/2]).

%%-------Test------------------
-export([
  get_packet_test/0
]).

%%-------API-------------------
open(Port,Options) when is_binary(Port)->
  open(binary_to_list( Port ), Options);
open(Port,Options)->
  Default=[
    {baudrate,9600},
    {parity,0},
    {stopbits,1},
    {bytesize,8}
  ],
  Program=create_program_file(Port),
  Cmd=build_cmd(Program++" -port "++Port,Options,Default),
  PID = spawn_link(?MODULE, init, [self(), Cmd]),
  receive
    {PID,ok}->{ok,PID};
    {PID,{error,Error}}->{error,Error}
  after
    3000->{error,timeout}
  end.

close(Port)->Port!{self(),close}.

send(Port,Data)->
  Port!{self(),send,Data},
  receive
    {Port,ok}->ok
  after
    3000->{error,closed}
  end.

%% Wait for Timeout
recv_time(Port,Timeout)->
  recv(Port,Timeout,time).

%% Receive Length bytes
recv_length(Port,Length)->recv_length(Port,Length,none).
recv_length(Port,Length,Timeout)->
  recv(Port,Timeout,{length,Length}).

% Receive until Delim
recv_delim(Port,Delim)->recv_delim(Port,Delim,none).
recv_delim(Port,Delim,Timeout)->
  recv(Port,Timeout,{delim,Delim}).

recv(Port,Timeout,Type)->
  Port!{self(),recv,Timeout,Type},
  receive
    {Port,{data,<<>>}}->{error,timeout};
    {Port,{data,Data}}->{ok,Data}
  end.

clear(Port)->Port!{self(),clear},ok.

%%----------Implementation-------------
init(Owner,Cmd)->
  Port=open_port({spawn,Cmd},[stream, overlapped_io, use_stdio, in, out, binary, exit_status]),
  receive
    {Port, {data, <<"OK">>}}->
      Owner!{self(),ok},
      loop(Port,Owner,<<>>);
    {Port, {data, <<"ERROR: ",Error/binary>>}}->
      port_close(Port),
      Owner!{self(),{error,Error}}
  after
    3000->Owner!{self(),{error,timeout}}
  end.

loop(Port,Owner,Buf)->
  receive
    {Port, {exit_status, Status}} ->
      throw({port_error,Status});
    {Owner,send,Data}->
      port_command(Port,[Data]),
      Owner!{self(),ok},
      loop(Port,Owner,Buf);
    {Owner,recv,Timeout,Type}->
      {Packet,Tail}=receive_packet(Port,Buf,Type,Timeout),
      Owner!{self(),{data,Packet}},
      loop(Port,Owner,Tail);
    {Owner,clear}->
      wait_data(50,Port),
      loop(Port,Owner,<<>>);
    {Owner,close}->
      port_close(Port),
      ok
  end.

receive_packet(Port,Buf,Type,Timeout)->
  case get_packet(Type,Buf) of
    {<<>>,Buf}->wait_packet(Port,Buf,Type,Timeout);
    {Packet,Tail}->{Packet,Tail}
  end.
wait_packet(Port,Buf,Type,Timeout)->
  Start=erlang:monotonic_time(1000),
  Data=wait_data(Timeout,Port),
  case get_packet(Type,<<Buf/binary,Data/binary>>) of
    {<<>>,NewBuf}->
      case tail_timeout(Timeout,Start,erlang:monotonic_time(1000)) of
        0->
          case Type of
            time->{NewBuf,<<>>};
            _->{<<>>,NewBuf}
          end;
        TailTimeout->wait_packet(Port,NewBuf,Type,TailTimeout)
      end;
    {Packet,Tail}->{Packet,Tail}
  end.
wait_data(none,Port)->
  receive
    {Port, {data, Data}}->Data
  end;
wait_data(Timeout,Port)->
  receive
    {Port, {data, Data}}->Data
  after
    Timeout-><<>>
  end.

tail_timeout(none,_Start,_End)->none;
tail_timeout(Timeout,Start,End)->
  Tail=Timeout-(End-Start),
  if
    Tail<0 ->0;
    true -> Tail
  end.

%%---------Packet types--------------------------
get_packet(time,Data)->{<<>>,Data};
get_packet({length,Length},Data)->
  case Data of
    <<Packet:Length/binary,Tail/binary>>->{Packet,Tail};
    _->{<<>>,Data}
  end;
get_packet({delim,Delim},Data)->
  case binary:split(Data,Delim) of
    [Head,Tail]->
      {<<Head/binary,Delim/binary>>,Tail};
    _->{<<>>,Data}
  end.


%%---------Internal helpers----------------------
create_program_file(PortName)->
  Dir=code:priv_dir(?MODULE)++"/",
  Source=
  case os:type() of
    {unix, linux}->"eserial";
    {win32, _}->"eserial.exe"
  end,
  SourcePath=Dir++Source,
  case filelib:is_file(SourcePath) of true->ok; false->throw({file_not_found,SourcePath}) end,
  case os:type() of
    {unix, linux}->SourcePath;
    {win32, _}->
      ResultPath=Dir++PortName++"_"++Source,
      case filelib:is_file(ResultPath) of
        true->ok;
        false->
          case file:copy(SourcePath, ResultPath) of {error,Error}->throw(Error); _->ok  end
      end,
      ResultPath
  end.

build_cmd(Program,Options,Default)->
  Program++
  " -baudrate "++integer_to_list(proplists:get_value(baudrate,Options,proplists:get_value(baudrate,Default)))++
  " -parity "++integer_to_list(proplists:get_value(parity,Options,proplists:get_value(parity,Default)))++
  " -stopbits "++integer_to_list(proplists:get_value(stopbits,Options,proplists:get_value(stopbits,Default)))++
  " -bytesize "++integer_to_list(proplists:get_value(bytesize,Options,proplists:get_value(bytesize,Default))).

%%----------Little testing-----------------
get_packet_test()->
  test_packet_length(),
  test_packet_delim(),
  ok.

test_packet_length()->
  {<<>>,<<>>}=get_packet({length,10},<<>>),
  {<<>>,<<"12345">>}=get_packet({length,10},<<"12345">>),
  {<<>>,<<"123456789">>}=get_packet({length,10},<<"123456789">>),
  {<<"1234567891">>,<<>>}=get_packet({length,10},<<"1234567891">>),
  {<<"1234567891">>,<<"2345">>}=get_packet({length,10},<<"12345678912345">>).

test_packet_delim()->
  {<<>>,<<>>}=get_packet({delim,<<"\r\n">>},<<>>),
  {<<>>,<<"12345">>}=get_packet({delim,<<"\r\n">>},<<"12345">>),
  {<<>>,<<"12345\r">>}=get_packet({delim,<<"\r\n">>},<<"12345\r">>),
  {<<"12345\r\n">>,<<>>}=get_packet({delim,<<"\r\n">>},<<"12345\r\n">>),
  {<<"12345\r\n">>,<<"567">>}=get_packet({delim,<<"\r\n">>},<<"12345\r\n567">>).
