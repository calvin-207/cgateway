%%%-------------------------------------------------------------------
%%% @author calvin
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 协议解析器，用于加解密协议
%%% @end
%%% Created : 12. 八月 2016 14:24
%%%-------------------------------------------------------------------
-module(cgateway_parser).
-author("calvin").

%% API
-export([decode/1,
    encode/1]).

%% @doc 解密
decode(UncodeData) ->
    UncodeData.

%% @doc 加密
encode(EncodeData) ->
    EncodeData.

