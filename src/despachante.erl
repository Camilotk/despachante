%% @doc
%% Módulo para validação de documentos brasileiros.
%% Atualmente suporta validação de CPF com ou sem pontuação,
%% e modo estrito opcional.
%%
%% Exemplos:
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

%% ============================================================================
%% API
%% ============================================================================

%% @doc Retorna true se a string contém apenas dígitos.
-spec is_only_digits(string()) -> boolean().
is_only_digits(Str) ->
    lists:all(fun(Char) -> Char >= $0 andalso Char =< $9 end, Str).

%% @doc Valida um documento com o modo flexível (pontuação opcional).
-spec valid(atom(), string()) -> boolean().
valid(Type, Doc) ->
    valid(Type, Doc, false).

%% @doc Valida um documento com modo estrito (sem pontuação).
-spec valid(atom(), string(), boolean()) -> boolean().
valid(_, Doc, _) when not is_list(Doc) ->
    false;
valid(cpf, Doc, Strict) ->
    case Strict andalso not is_only_digits(Doc) of
        true -> false;
        false ->
            case clean_binary(Doc) of
                [_,_,_,_,_,_,_,_,_,D1,D2] = Digits ->
                    Base9  = lists:sublist(Digits, 9),
                    Base10 = lists:sublist(Digits, 10),
                    edge_cases(Digits)
                    andalso validate_digit(Base9,  lists:seq(10,2,-1), D1)
                    andalso validate_digit(Base10, lists:seq(11,2,-1), D2);
                _ -> false
            end
    end;
valid(_, _, _) ->
    false.

%% ============================================================================
%% Funções auxiliares internas
%% ============================================================================

%% @private Permite apenas números e pontuação oficial do CPF.
-spec is_valid_cpf_char(integer()) -> boolean().
is_valid_cpf_char(C) ->
    (C >= $0 andalso C =< $9) orelse C =:= $. orelse C =:= $-.

%% @doc Extrai e valida somente números de uma string, rejeita se conter caracteres inválidos.
-spec clean_binary(string()) -> [integer()].
clean_binary(Str) when is_list(Str) ->
    try
        Bin = list_to_binary(Str),
        AllChars = binary_to_list(Bin),
        case lists:all(fun is_valid_cpf_char/1, AllChars) of
            true ->
                [C - $0 || C <- AllChars, C >= $0, C =< $9];
            false ->
                [] %% será rejeitado no match pattern de 11 dígitos
        end
    catch
        error:_ -> []
    end.

%% @doc Verifica se o CPF tem 11 dígitos, não é sequência repetida
%%         e não está na lista de inválidos conhecidos.
-spec edge_cases([integer()]) -> boolean().
edge_cases(CPF) ->
    length(CPF) == 11
    andalso not all_equal(CPF)
    andalso not is_common_invalid(CPF).

%% @doc Lista CPF inválido conhecido usado em exemplos (como em bancos de dados fake).
-spec is_common_invalid([integer()]) -> boolean().
is_common_invalid([1,2,3,4,5,6,7,8,9,0,9]) -> true;
is_common_invalid(_) -> false.

%% @doc Verifica se todos os elementos da lista são iguais.
-spec all_equal([integer()]) -> boolean().
all_equal([H | T]) ->
    lists:all(fun(X) -> X =:= H end, T);
all_equal([]) ->
    true.

%% @doc Valida o dígito verificador com base na soma ponderada.
-spec validate_digit([integer()], [integer()], integer()) -> boolean().
validate_digit(Digits, Weights, Expected) ->
    R = (sum_products(Digits, Weights) * 10) rem 11,
    R =:= Expected orelse (R =:= 10 andalso Expected =:= 0).

%% @doc Soma dos produtos entre elementos e pesos.
-spec sum_products([integer()], [integer()]) -> integer().
sum_products([], []) -> 0;
sum_products([D | DT], [W | WT]) ->
    D * W + sum_products(DT, WT).
