%% @doc
%% Módulo para validação de documentos brasileiros.
%% Atualmente suporta validação de CPF com ou sem pontuação,
%% e modo estrito opcional.
%%
%% Exemplo:
%% > despachante:valid(cpf, "825.205.780-25").
%% true
%%
%% > despachante:valid(cpf, "82520578025", true).
%% true
%%
%% > despachante:valid(cpf, "825.205.780-25", true).
%% false

-module(despachante).

-export([valid/2, valid/3]).

%% API

%% @doc Valida um documento com o modo flexível (pontuação opcional).
-spec valid(atom(), string()) -> boolean().
valid(Type, Doc) ->
    valid(Type, Doc, false).

%% @doc Valida um documento com opção de modo estrito.
%% Quando `Strict` é true, o número deve conter apenas dígitos.
-spec valid(atom(), string(), boolean()) -> boolean().
valid(cpf, CPFStr, Strict) ->
    case (Strict andalso not is_only_digits(CPFStr)) of
        true -> false;
        false ->
            Cleaned = clean_binary(CPFStr),
            case Cleaned of
                [_A1,_A2,_A3,_A4,_A5,_A6,_A7,_A8,_A9,D1,D2] ->
                    First9 = lists:sublist(Cleaned, 9),
                    First10 = lists:sublist(Cleaned, 10),
                    FirstValid = validate_digit(First9, lists:seq(10,2,-1), D1),
                    SecondValid = validate_digit(First10, lists:seq(11,2,-1), D2),
                    edge_cases(Cleaned) andalso FirstValid andalso SecondValid;
                _ -> false
            end
    end;
valid(_, _, _) ->
    false.

%% Auxiliares

%% @doc Retorna true se a string contém apenas dígitos.
-spec is_only_digits(string()) -> boolean().
is_only_digits(Str) ->
    lists:all(fun(Char) -> Char >= $0 andalso Char =< $9 end, Str).

%% @doc Remove todos os caracteres não numéricos da string.
-spec clean_binary(string()) -> [integer()].
clean_binary(Str) ->
    [C - $0 || <<C>> <= list_to_binary(Str), C >= $0, C =< $9].

%% @doc Retorna true se o CPF tem 11 dígitos e não é uma sequência repetida.
-spec edge_cases([integer()]) -> boolean().
edge_cases(CPF) ->
    length(CPF) == 11 andalso not all_equal(CPF).

%% @doc Verifica se todos os elementos da lista são iguais.
-spec all_equal([integer()]) -> boolean().
all_equal([H | T]) -> all_same(H, T).

-spec all_same(integer(), [integer()]) -> boolean().
all_same(_, []) -> true;
all_same(H, [H | T]) -> all_same(H, T);
all_same(_, _) -> false.

%% @doc Valida o dígito verificador com base nas regras do CPF.
-spec validate_digit([integer()], [integer()], integer()) -> boolean().
validate_digit(Digits, Weights, Digit) ->
    Sum = sum_products(Digits, Weights),
    R = (Sum * 10) rem 11,
    R == Digit orelse (R == 10 andalso Digit == 0).

%% @doc Soma dos produtos de duas listas: Digits e Weights.
-spec sum_products([integer()], [integer()]) -> integer().
sum_products([], []) -> 0;
sum_products([D | DT], [W | WT]) ->
    D * W + sum_products(DT, WT).
