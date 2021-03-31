-module(pt_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.

all() ->
    [
        % {group, jose_cfrg_curves},
        % {group, jose_ecdh_1pu},
        % {group, jose_jwe},
        % {group, jose_jwk},
        % {group, jose_jws},
        % {group, rfc7520}
    ].

groups() ->
    [
        % {jose_cfrg_curves, [parallel], [
        %     jose_cfrg_curves_a_1,
        %     jose_cfrg_curves_a_2,
        %     jose_cfrg_curves_a_3,
        %     jose_cfrg_curves_a_4,
        %     jose_cfrg_curves_a_5,
        %     jose_cfrg_curves_a_6,
        %     jose_cfrg_curves_a_7
        % ]},
        % {jose_ecdh_1pu, [parallel], [
        %     jose_ecdh_1pu_a
        % ]},
        % {jose_jwe, [parallel], [
        %     jwe_a_1,
        %     jwe_a_2,
        %     jwe_a_3
        % ]},
        % {jose_jwk, [parallel], [
        %     jwk_c,
        %     jwk_rsa_multi
        % ]},
        % {jose_jws, [parallel], [
        %     jws_a_1,
        %     jws_a_2,
        %     jws_a_3,
        %     jws_a_4,
        %     jws_a_5
        % ]},
        % {rfc7520, [parallel], [
        %     rfc7520_5_9
        % ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================
