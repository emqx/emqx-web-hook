%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_web_hook_api).

-import(minirest,  [return/1]).

-rest_api(#{ name => list_all_metrics
           , method => 'GET'
           , path => "/webhook/metrics"
           , func => list
           , descr => "A list of web hook metrics of all nodes in the cluster"}).

-rest_api(#{ name => list_node_metrics
           , method => 'GET'
           , path => "/nodes/:atom:node/webhook/metrics"
           , func => list
           , descr => "A list of web hook metrics of a node"}).

-export([list/2]).

list(Bindings, _Params) when map_size(Bindings) == 0 ->
    return({ok, [[{node, Node}, {metrics, Metrics}]
                              || {Node, Metrics} <- emqx_web_hook:get_metrics()]});

list(#{node := Node}, _Params) ->
    case emqx_web_hook:get_metrics(Node) of
        {error, Reason} -> return({error, Reason});
        Metrics         -> return({ok, Metrics})
    end.