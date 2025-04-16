-module(despachante_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===============================
%% SUITE: VALORES VÁLIDOS
%% ===============================

cpf_valid_test_() ->
    [
        {"accepts clean cpf", ?_assert(despachante:valid(cpf, "29537995593"))},
        {"accepts formatted cpf", ?_assert(despachante:valid(cpf, "295.379.955-93"))}
    ].

%% ===============================
%% SUITE: MODO ESTRITO
%% ===============================

cpf_strict_mode_test_() ->
    [
        {"accepts clean numeric only", ?_assert(despachante:valid(cpf, "29537995593", true))},
        {"rejects formatted string", ?_assertNot(despachante:valid(cpf, "295.379.955-93", true))},
        {"rejects mixed garbage", ?_assertNot(despachante:valid(cpf, "295...379$$955--93", true))},
        {"accepts exactly 11 digits", ?_assert(despachante:valid(cpf, "52139989171", true))}
    ].

%% ===============================
%% SUITE: ENTRADAS REPETIDAS / PADRÕES INVÁLIDOS
%% ===============================

cpf_common_invalids_test_() ->
    Invalids = [
        "00000000000", "11111111111", "22222222222",
        "33333333333", "44444444444", "55555555555",
        "66666666666", "77777777777", "88888888888",
        "99999999999", "12345678909"
    ],
    lists:map(fun(Val) ->
        {lists:concat(["repeated invalid: ", Val]),
         ?_assertNot(despachante:valid(cpf, Val))}
    end, Invalids).

%% ===============================
%% SUITE: LIXO, STRINGS CURTAS, TEXTOS, NIL
%% ===============================

cpf_invalid_inputs_test_() ->
    [
        {"empty string", ?_assertNot(despachante:valid(cpf, ""))},
        {"nil-like atom", ?_assertNot(despachante:valid(cpf, undefined))},
        {"junk letters", ?_assertNot(despachante:valid(cpf, "aaa.bbb.ccc-dd"))},
        {"too short string", ?_assertNot(despachante:valid(cpf, "123"))},
        {"less than 11 digits", ?_assertNot(despachante:valid(cpf, "5213998917"))},
        {"more than 11 digits", ?_assertNot(despachante:valid(cpf, "5213998917111"))},
        {"too long + correct prefix", ?_assertNot(despachante:valid(cpf, "295379955931234"))},
        {"garbage is rejected", ?_assertNot(despachante:valid(cpf, "295$$.379\n955...93"))}
    ].

%% ===============================
%% SUITE: ERROS COMBINADOS
%% ===============================

cpf_mixed_edge_cases_test_() ->
    [
        {"formatted + extra digit", ?_assertNot(despachante:valid(cpf, "295.379.955-931"))},
        {"garbage only", ?_assertNot(despachante:valid(cpf, "@@@###$$$%%%"))},
        {"number + text", ?_assertNot(despachante:valid(cpf, "29537995593abc"))},
        {"CPF-like com espaços", ?_assertNot(despachante:valid(cpf, "295 379 955 93"))},
        {"garbage in strict mode", ?_assertNot(despachante:valid(cpf, "295\n379 955 93", true))}
    ].
