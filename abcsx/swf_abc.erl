#!/usr/bin/env escript
%% -*- erlang -*-
%%! debug verbose

%% Make a simple swf file from an abs file.
%%
%% Usage: ./swf_abc.erl width height classname abcfile
%% Output: classname.swf

main([Width, Height, SpriteClass, ABCFile]) ->
    Spec = 
	{{frame_size, list_to_integer(Width) * 20, list_to_integer(Height) * 20},
	 {frame_rate, 30},
	 {frame_count, 1},
	 [{file_attribute},
	  {do_abc, ABCFile},
	  {symbol_class, SpriteClass},
	  {show_frame},
	  {end_tag}]},
    
    {ok, S} = file:open(lists:append(SpriteClass, ".swf"), [binary, write]),
    file:write(S, swf(Spec)),
    file:close(S);
main(_) -> io:format("Usage: swf_abc.erl width height classname abcfile~n").

% SWF Structure

swf(Spec) ->
    Version = 10,
    Rest = swf_rest(Spec),
    FileLength = size(Rest) + 8,
    <<"FWS",
     Version:8/unsigned-little,
     FileLength:32/unsigned-little,
     Rest/binary>>.

swf_rest({{frame_size, Width, Height},
	  {frame_rate, FrameRate},
	  {frame_count, FrameCount},
	  Tags}) ->
    FrameSize = rectangle({rectangle, 0, Width, 0, Height}),
    FrameRateData = fixed8dot8(FrameRate),
    TagsData = list_to_binary([tag(X) || X <- Tags]),
    <<FrameSize/binary,
     FrameRateData/binary,
     FrameCount:16/unsigned-little,
     TagsData/binary>>.

% Basic Data Types

nbits_unsigned(XS) -> % Necessary bits size for a list of integer values.
    Max = lists:max([abs(X) || X <- XS]),
    trunc(math:log(Max) / math:log(2)) + 1.
nbits_signed(XS) -> nbits_unsigned(XS) + 1.

fixed8dot8(N)->
    IntegerPart = trunc(N),
    SmallPart = trunc((N - IntegerPart) / 1 * 256),
    <<SmallPart:8, IntegerPart:8>>.

rectangle({rectangle, Xmin, Xmax, Ymin, Ymax}) ->
    Nbits = nbits_signed([Xmin, Xmax, Ymin, Ymax]),
    padding(<< Nbits:5,
	     Xmin:Nbits/signed-big,
	     Xmax:Nbits/signed-big,
	     Ymin:Nbits/signed-big,
	     Ymax:Nbits/signed-big>>).

rgb({rgb, R, G, B}) -> <<R:8, G:8, B:8>>.

string_type(String) -> list_to_binary([String, 0]).

% Tag Format

record_header_body(Type, Body) -> record_header_body(Type, Body, size(Body)).

record_header_body(Type, Body, Length) when Length < 63 ->
     <<TagCodeAndLength:16/unsigned-big>> = <<Type:10, Length:6>>,
    [<<TagCodeAndLength:16/unsigned-little>>, Body];

record_header_body(Type, Body, Length) ->
    <<TagCodeAndLength:16/unsigned-big>> = <<Type:10, 63:6>>,
    [<<TagCodeAndLength:16/unsigned-little>>,
     <<Length:32/unsigned-little>>,
     Body].

% Control Tags

tag({end_tag}) -> record_header_body(0, <<>>);
tag({show_frame}) -> record_header_body(1, <<>>);
tag({set_background_color, RGB}) -> record_header_body(9, rgb(RGB));

% FileAttributes: HasMetaData 0, UseNetwork 0, HasAS3 1
tag({file_attribute}) -> record_header_body(69, <<2#00001000,0,0,0>>);

tag({symbol_class, SpriteClass}) ->
    NumSymbols = 1,
    Tag1 = 0,
    Name1 = string_type(SpriteClass),
    record_header_body(76, <<NumSymbols:16/unsigned-little,
			    Tag1:16/unsigned-little,
			    Name1/binary>>);

tag({do_abc, Filename}) ->
    {ok, ABCData} = file:read_file(Filename),
    Flags = 0,
    Name = string_type(Filename),
    record_header_body(82, <<Flags:32/unsigned-little,
			    Name/binary,
			    ABCData/binary>>).

%% bitstring utilities

padding(Bits) ->
    Padding = 8 - bit_size(Bits) rem 8,
    <<Bits/bitstring, 0:Padding>>.
