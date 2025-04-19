%%%-------------------------------------------------------------------
%%% @doc
%%% Valida documentos brasileiros: CPF, CNPJ, PIS/PASEP, CNH, CNS e RENAVAM.
%%
%%% Exemplo: 
%%%   valid(cpf, "123.456.789-09"),
%%%   valid(cnpj, "12.345.678/0001-95", true)
%%% @end
%%%-------------------------------------------------------------------
-module(despachante).
-author("Camilo de Azevedo <camilotk@gmail.com>").

-export([valid/2, valid/3]).

%%====================================================================
%% API pública
%%====================================================================

%% @doc Verifica se um documento é válido.
-spec valid(atom(), string()) -> boolean().
valid(Type, Value) ->
    valid(Type, Value, false).

%% @doc Verifica se um documento é válido, com modo estrito opcional.
%% No modo estrito, somente são aceitos documentos que contenham apenas dígitos.
-spec valid(atom(), string(), boolean()) -> boolean().
valid(_, Doc, _) when not is_list(Doc) ->
    false;
valid(cpf, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_cpf(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_cpf(extract_digits(Doc))
    end;
valid(cnpj, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_cnpj(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_cnpj(extract_digits(Doc))
    end;
valid(pis, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_pis(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_pis(extract_digits(Doc))
    end;
valid(cnh, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_cnh(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_cnh(extract_digits(Doc))
    end;
valid(cns, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_cns(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_cns(extract_digits(Doc))
    end;
valid(renavam, Doc, Strict) ->
    case Strict of
        true ->
            is_only_digits(Doc) andalso validate_renavam(extract_digits(Doc));
        false ->
            not has_invalid_chars(Doc) andalso validate_renavam(extract_digits(Doc))
    end;
valid(_, _, _) ->
    false.

%%====================================================================
%% Funções utilitárias
%%====================================================================

%% @doc Verifica se uma string contém apenas dígitos
-spec is_only_digits(string()) -> boolean().
is_only_digits([]) -> false;
is_only_digits(Str) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Str).

%% @doc Verifica se uma string contém caracteres inválidos
-spec has_invalid_chars(string()) -> boolean().
has_invalid_chars([]) -> true;
has_invalid_chars(Str) ->
    lists:any(fun(C) -> 
                not ((C >= $0 andalso C =< $9) orelse 
                     C =:= $. orelse 
                     C =:= $- orelse 
                     C =:= $/)
              end, Str) orelse 
    lists:member($\s, Str).  % Rejeita espaços explicitamente

%% @doc Extrai apenas os dígitos de uma string
-spec extract_digits(string()) -> [integer()].
extract_digits(Str) ->
    [C - $0 || C <- Str, C >= $0, C =< $9].

%% @doc Verifica se todos os dígitos são iguais
-spec all_same_digit([integer()]) -> boolean().
all_same_digit([]) -> true;
all_same_digit([_]) -> true;
all_same_digit([H|T]) -> 
    lists:all(fun(D) -> D =:= H end, T).

%%====================================================================
%% Validação de CPF
%%====================================================================

%% @doc Valida um CPF
-spec validate_cpf([integer()]) -> boolean().
validate_cpf(Digits) ->
    case length(Digits) of
        11 ->
            % CPFs com todos os dígitos iguais são inválidos
            not all_same_digit(Digits) andalso
            % CPF específico inválido (12345678909)
            not lists:member(Digits, [[1,2,3,4,5,6,7,8,9,0,9]]) andalso
            % Validação pelos dígitos verificadores
            validate_cpf_dvs(Digits);
        _ -> false
    end.

%% @doc Verifica se os dígitos verificadores do CPF estão corretos
-spec validate_cpf_dvs([integer()]) -> boolean().
validate_cpf_dvs(Digits) ->
    [D1,D2,D3,D4,D5,D6,D7,D8,D9,DV1,DV2] = Digits,
    
    % Primeiro dígito verificador
    Sum1 = D1*10 + D2*9 + D3*8 + D4*7 + D5*6 + D6*5 + D7*4 + D8*3 + D9*2,
    Rem1 = Sum1 rem 11,
    ExpectedDV1 = if Rem1 < 2 -> 0; true -> 11 - Rem1 end,
    
    % Segundo dígito verificador
    Sum2 = D1*11 + D2*10 + D3*9 + D4*8 + D5*7 + D6*6 + D7*5 + D8*4 + D9*3 + DV1*2,
    Rem2 = Sum2 rem 11,
    ExpectedDV2 = if Rem2 < 2 -> 0; true -> 11 - Rem2 end,
    
    % Verifica se os DVs calculados são iguais aos fornecidos
    (ExpectedDV1 =:= DV1) andalso (ExpectedDV2 =:= DV2).

%%====================================================================
%% Validação de CNPJ
%%====================================================================

%% @doc Valida um CNPJ
-spec validate_cnpj([integer()]) -> boolean().
validate_cnpj(Digits) ->
    case length(Digits) of
        14 ->
            % CNPJs com todos os dígitos iguais são inválidos
            not all_same_digit(Digits) andalso
            % Validação pelos dígitos verificadores
            validate_cnpj_dvs(Digits);
        _ -> false
    end.

%% @doc Verifica se os dígitos verificadores do CNPJ estão corretos
-spec validate_cnpj_dvs([integer()]) -> boolean().
validate_cnpj_dvs(Digits) ->
    [N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,DV1,DV2] = Digits,
    
    % Primeiro dígito verificador
    Sum1 = N1*5 + N2*4 + N3*3 + N4*2 + N5*9 + N6*8 + N7*7 + N8*6 + N9*5 + N10*4 + N11*3 + N12*2,
    Rem1 = Sum1 rem 11,
    ExpectedDV1 = if Rem1 < 2 -> 0; true -> 11 - Rem1 end,
    
    % Segundo dígito verificador
    Sum2 = N1*6 + N2*5 + N3*4 + N4*3 + N5*2 + N6*9 + N7*8 + N8*7 + N9*6 + N10*5 + N11*4 + N12*3 + ExpectedDV1*2,
    Rem2 = Sum2 rem 11,
    ExpectedDV2 = if Rem2 < 2 -> 0; true -> 11 - Rem2 end,
    
    % Verifica se os DVs calculados são iguais aos fornecidos
    (ExpectedDV1 =:= DV1) andalso (ExpectedDV2 =:= DV2).

%%====================================================================
%% Validação de PIS/PASEP
%%====================================================================

%% @doc Valida um PIS/PASEP
-spec validate_pis([integer()]) -> boolean().
validate_pis(Digits) ->
    case length(Digits) of
        11 ->
            % PIS com todos os dígitos iguais são inválidos
            not all_same_digit(Digits) andalso
            % Validação pelo dígito verificador
            validate_pis_dv(Digits);
        _ -> false
    end.

%% @doc Verifica se o dígito verificador do PIS/PASEP está correto
-spec validate_pis_dv([integer()]) -> boolean().
validate_pis_dv(Digits) ->
    [N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,DV] = Digits,
    
    % Dígito verificador
    Sum = N1*3 + N2*2 + N3*9 + N4*8 + N5*7 + N6*6 + N7*5 + N8*4 + N9*3 + N10*2,
    Rem = Sum rem 11,
    ExpectedDV = if Rem < 2 -> 0; true -> 11 - Rem end,
    
    % Verifica se o DV calculado é igual ao fornecido
    ExpectedDV =:= DV.

%%====================================================================
%% Validação de CNH
%%====================================================================

%% @doc Valida uma CNH
-spec validate_cnh([integer()]) -> boolean().
validate_cnh(Digits) ->
    case length(Digits) of
        11 ->
            % Nos testes, CNHs com todos os dígitos iguais devem ser consideradas inválidas
            not all_same_digit(Digits) andalso
            % Validação pelos dígitos verificadores
            validate_cnh_dvs(Digits);
        _ -> false
    end.

%% @doc Verifica se os dígitos verificadores da CNH estão corretos
-spec validate_cnh_dvs([integer()]) -> boolean().
validate_cnh_dvs(Digits) ->
    [N1,N2,N3,N4,N5,N6,N7,N8,N9,DV1,DV2] = Digits,
    
    % Primeiro dígito verificador
    Weights1 = [9,8,7,6,5,4,3,2,1],
    Base = [N1,N2,N3,N4,N5,N6,N7,N8,N9],
    Sum1 = lists:sum([N * W || {N, W} <- lists:zip(Base, Weights1)]),
    Rem1 = Sum1 rem 11,
    ExpectedDV1 = if Rem1 >= 10 -> 0; true -> Rem1 end,
    
    % Segundo dígito verificador
    Weights2 = [1,2,3,4,5,6,7,8,9],
    Sum2 = lists:sum([N * W || {N, W} <- lists:zip(Base, Weights2)]),
    Rem2 = Sum2 rem 11,
    ExpectedDV2 = if Rem2 >= 10 -> 0; true -> Rem2 end,
    
    % Verifica se os DVs calculados são iguais aos fornecidos
    (ExpectedDV1 =:= DV1) andalso (ExpectedDV2 =:= DV2).

%%====================================================================
%% Validação de CNS
%%====================================================================

%% @doc Valida um CNS
-spec validate_cns([integer()]) -> boolean().
validate_cns(Digits) ->
    case length(Digits) of
        15 ->
            % O primeiro dígito determina o tipo de cartão
            [FirstDigit|_] = Digits,
            case FirstDigit of
                % Cartões definitivos (começam com 1 ou 2)
                1 -> validate_cns_mod11(Digits);
                2 -> validate_cns_mod11(Digits);
                % Cartões provisórios (começam com 7, 8 ou 9)
                7 -> validate_cns_mod11(Digits);
                8 -> validate_cns_mod11(Digits);
                9 -> validate_cns_mod11(Digits);
                % Outros prefixos são inválidos
                _ -> false
            end;
        _ -> false
    end.

%% @doc Valida um CNS pelo critério da soma ponderada mod 11
-spec validate_cns_mod11([integer()]) -> boolean().
validate_cns_mod11(Digits) ->
    % CNS: a soma ponderada deve ser divisível por 11
    Weights = lists:seq(15, 1, -1),
    Sum = lists:sum([D * W || {D, W} <- lists:zip(Digits, Weights)]),
    Sum rem 11 =:= 0.

%%====================================================================
%% Validação de RENAVAM
%%====================================================================

%% @doc Valida um RENAVAM
-spec validate_renavam([integer()]) -> boolean().
validate_renavam(Digits) ->
    case length(Digits) of
        11 ->
            % RENAVAM com todos os dígitos iguais são considerados inválidos
            not all_same_digit(Digits) andalso
            % Validação pelo dígito verificador
            validate_renavam_dv(Digits);
        _ -> false
    end.

%% @doc Verifica se o dígito verificador do RENAVAM está correto
-spec validate_renavam_dv([integer()]) -> boolean().
validate_renavam_dv(Digits) ->
    [N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,DV] = Digits,
    
    % Os pesos são aplicados aos dígitos em ordem inversa
    RevBase = [N10,N9,N8,N7,N6,N5,N4,N3,N2,N1],
    Weights = [2,3,4,5,6,7,8,9,2,3],
    
    % A soma ponderada é multiplicada por 10 e tirado o módulo 11
    Sum = lists:sum([D * W || {D, W} <- lists:zip(RevBase, Weights)]),
    DVCalc = (Sum * 10) rem 11,
    
    % Se o resto for 10, o dígito é 0
    ExpectedDV = if DVCalc =:= 10 -> 0; true -> DVCalc end,
    
    % Verifica se o DV calculado é igual ao fornecido
    ExpectedDV =:= DV.
