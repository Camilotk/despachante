-module(despachante_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Testes para CPF
%%====================================================================

cpf_valid_test_() ->
    [
        {"CPF válido limpo", ?_assert(despachante:valid(cpf, "29537995593"))},
        {"CPF válido formatado", ?_assert(despachante:valid(cpf, "295.379.955-93"))},
        {"CPF válido com traços", ?_assert(despachante:valid(cpf, "295-379-955-93"))},
        {"CPF válido com pontos", ?_assert(despachante:valid(cpf, "295.379.95593"))},
        {"CPF válido com diferentes separadores", ?_assert(despachante:valid(cpf, "295.379-955.93"))},
        {"CPF válido 1", ?_assert(despachante:valid(cpf, "52139989171"))},
        {"CPF válido 2", ?_assert(despachante:valid(cpf, "76771096084"))},
        {"CPF válido 3", ?_assert(despachante:valid(cpf, "41056914068"))},
        {"CPF válido 4", ?_assert(despachante:valid(cpf, "94996283041"))},
        {"CPF válido 5", ?_assert(despachante:valid(cpf, "46228958038"))},
        {"CPF válido 6", ?_assert(despachante:valid(cpf, "86267552012"))},
        {"CPF válido formatado 2", ?_assert(despachante:valid(cpf, "521.399.891-71"))}
    ].

cpf_invalid_test_() ->
    [
        {"CPF inválido 1", ?_assertNot(despachante:valid(cpf, "47393545634"))},
        {"CPF inválido 2", ?_assertNot(despachante:valid(cpf, "95395994104"))},
        {"CPF inválido 3", ?_assertNot(despachante:valid(cpf, "05384614790"))},
        {"CPF inválido 4", ?_assertNot(despachante:valid(cpf, "24861277584"))},
        {"CPF inválido (dígito errado)", ?_assertNot(despachante:valid(cpf, "29537995592"))},
        {"CPF inválido (todos iguais 1)", ?_assertNot(despachante:valid(cpf, "11111111111"))},
        {"CPF inválido (todos iguais 2)", ?_assertNot(despachante:valid(cpf, "00000000000"))},
        {"CPF inválido (todos iguais 3)", ?_assertNot(despachante:valid(cpf, "99999999999"))},
        {"CPF inválido (fora do padrão)", ?_assertNot(despachante:valid(cpf, "12345678909"))},
        {"CPF inválido (curto demais)", ?_assertNot(despachante:valid(cpf, "1234567890"))},
        {"CPF inválido (longo demais)", ?_assertNot(despachante:valid(cpf, "123456789012"))},
        {"CPF inválido (caracteres)", ?_assertNot(despachante:valid(cpf, "abc.def.ghi-jk"))},
        {"CPF inválido (espaços)", ?_assertNot(despachante:valid(cpf, "295 379 955 93"))},
        {"CPF inválido (vazio)", ?_assertNot(despachante:valid(cpf, ""))},
        {"CPF inválido (undefined)", ?_assertNot(despachante:valid(cpf, undefined))},
        {"CPF inválido (primeiro DV errado)", ?_assertNot(despachante:valid(cpf, "29537995693"))},
        {"CPF inválido (segundo DV errado)", ?_assertNot(despachante:valid(cpf, "29537995594"))},
        {"CPF inválido (lixo)", ?_assertNot(despachante:valid(cpf, "295$$.379\n955...93"))},
        {"CPF inválido (número+texto)", ?_assertNot(despachante:valid(cpf, "29537995593abc"))}
    ].

%%====================================================================
%% Testes para CNPJ
%%====================================================================

cnpj_valid_test_() ->
    [
        {"CNPJ válido limpo", ?_assert(despachante:valid(cnpj, "11222333000181"))},
        {"CNPJ válido formatado", ?_assert(despachante:valid(cnpj, "11.222.333/0001-81"))},
        {"CNPJ válido com traços", ?_assert(despachante:valid(cnpj, "11-222-333/0001-81"))},
        {"CNPJ válido com pontos", ?_assert(despachante:valid(cnpj, "11.222.333/000181"))},
        {"CNPJ válido 2", ?_assert(despachante:valid(cnpj, "44073196000110"))},
        {"CNPJ válido 3", ?_assert(despachante:valid(cnpj, "77778009000152"))},
        {"CNPJ válido 4", ?_assert(despachante:valid(cnpj, "48412199000191"))},
        {"CNPJ válido 5", ?_assert(despachante:valid(cnpj, "03068097000162"))},
        {"CNPJ válido 6", ?_assert(despachante:valid(cnpj, "20687642000118"))},
        {"CNPJ válido 7", ?_assert(despachante:valid(cnpj, "38175021000110"))},
        {"CNPJ válido 8", ?_assert(despachante:valid(cnpj, "23280617000103"))},
        {"CNPJ válido 9", ?_assert(despachante:valid(cnpj, "88136577000176"))},
        {"CNPJ válido 10", ?_assert(despachante:valid(cnpj, "34395742000185"))},
        {"CNPJ válido formatado 2", ?_assert(despachante:valid(cnpj, "26.954.289/0001-71"))},
        {"CNPJ válido formatado 3", ?_assert(despachante:valid(cnpj, "04.584.077/0001-07"))}
    ].

cnpj_invalid_test_() ->
    [
        {"CNPJ inválido (dígito errado)", ?_assertNot(despachante:valid(cnpj, "11222333000182"))},
        {"CNPJ inválido (todos iguais 1)", ?_assertNot(despachante:valid(cnpj, "11111111111111"))},
        {"CNPJ inválido (todos iguais 2)", ?_assertNot(despachante:valid(cnpj, "00000000000000"))},
        {"CNPJ inválido (todos iguais 3)", ?_assertNot(despachante:valid(cnpj, "99999999999999"))},
        {"CNPJ inválido (curto demais)", ?_assertNot(despachante:valid(cnpj, "1122233300018"))},
        {"CNPJ inválido (longo demais)", ?_assertNot(despachante:valid(cnpj, "112223330001811"))},
        {"CNPJ inválido (caracteres)", ?_assertNot(despachante:valid(cnpj, "aa.bbb.ccc/dddd-ee"))},
        {"CNPJ inválido (espaços)", ?_assertNot(despachante:valid(cnpj, "11 222 333 0001 81"))},
        {"CNPJ inválido (vazio)", ?_assertNot(despachante:valid(cnpj, ""))},
        {"CNPJ inválido (undefined)", ?_assertNot(despachante:valid(cnpj, undefined))},
        {"CNPJ inválido (primeiro DV errado)", ?_assertNot(despachante:valid(cnpj, "11222333000281"))},
        {"CNPJ inválido (segundo DV errado)", ?_assertNot(despachante:valid(cnpj, "11222333000182"))},
        {"CNPJ inválido (lixo)", ?_assertNot(despachante:valid(cnpj, "11.222$$.333\n0001...81"))},
        {"CNPJ inválido (número+texto)", ?_assertNot(despachante:valid(cnpj, "11222333000181abc"))},
        {"CNPJ inválido (primeiro DV)", ?_assertNot(despachante:valid(cnpj, "11222333000191"))}
    ].

%%====================================================================
%% Testes para PIS
%%====================================================================

pis_valid_test_() ->
    [
        {"PIS válido limpo", ?_assert(despachante:valid(pis, "16739457132"))},
        {"PIS válido formatado", ?_assert(despachante:valid(pis, "797.14454.89-8"))},
        {"PIS válido com traços", ?_assert(despachante:valid(pis, "797-14454-898"))},
        {"PIS válido com pontos", ?_assert(despachante:valid(pis, "797.14454.898"))},
        {"PIS válido 2", ?_assert(despachante:valid(pis, "14420208542"))},
        {"PIS válido 3", ?_assert(despachante:valid(pis, "39361410064"))},
        {"PIS válido 4", ?_assert(despachante:valid(pis, "76199478813"))},
        {"PIS válido 5", ?_assert(despachante:valid(pis, "44953685731"))},
        {"PIS válido 6", ?_assert(despachante:valid(pis, "34737684571"))},
        {"PIS válido 7", ?_assert(despachante:valid(pis, "91545944906"))},
        {"PIS válido 8", ?_assert(despachante:valid(pis, "76626330726"))},
        {"PIS válido 9", ?_assert(despachante:valid(pis, "08114469011"))},
        {"PIS válido 10", ?_assert(despachante:valid(pis, "07486189993"))},
        {"PIS válido formatado 2", ?_assert(despachante:valid(pis, "769.17475.64-0"))},
        {"PIS válido formatado 3", ?_assert(despachante:valid(pis, "183.37363.03-2"))}
    ].

pis_invalid_test_() ->
    [
        {"PIS inválido (dígito errado)", ?_assertNot(despachante:valid(pis, "12053525127"))},
        {"PIS inválido (todos iguais 1)", ?_assertNot(despachante:valid(pis, "11111111111"))},
        {"PIS inválido (todos iguais 2)", ?_assertNot(despachante:valid(pis, "00000000000"))},
        {"PIS inválido (todos iguais 3)", ?_assertNot(despachante:valid(pis, "99999999999"))},
        {"PIS inválido (curto demais)", ?_assertNot(despachante:valid(pis, "1205352512"))},
        {"PIS inválido (longo demais)", ?_assertNot(despachante:valid(pis, "120535251281"))},
        {"PIS inválido (caracteres)", ?_assertNot(despachante:valid(pis, "aaa.bbbb.ccc-d"))},
        {"PIS inválido (espaços)", ?_assertNot(despachante:valid(pis, "120 5352 512 8"))},
        {"PIS inválido (vazio)", ?_assertNot(despachante:valid(pis, ""))},
        {"PIS inválido (undefined)", ?_assertNot(despachante:valid(pis, undefined))},
        {"PIS inválido (dígito errado 2)", ?_assertNot(despachante:valid(pis, "12023364472"))},
        {"PIS inválido (dígito errado 3)", ?_assertNot(despachante:valid(pis, "12089509883"))},
        {"PIS inválido (lixo)", ?_assertNot(despachante:valid(pis, "120$$.5352\n512...8"))},
        {"PIS inválido (número+texto)", ?_assertNot(despachante:valid(pis, "12053525128abc"))},
        {"PIS inválido (dígito errado 4)", ?_assertNot(despachante:valid(pis, "12075017932"))}
    ].

%%====================================================================
%% Testes para CNH
%%====================================================================

cnh_valid_test_() ->
    [
        {"CNH válida 1", ?_assert(despachante:valid(cnh, "23589275003"))},
        {"CNH válida 2", ?_assert(despachante:valid(cnh, "67589194024"))},
        {"CNH válida 3", ?_assert(despachante:valid(cnh, "18571079845"))},
        {"CNH válida 4", ?_assert(despachante:valid(cnh, "72821384207"))},
        {"CNH válida 5", ?_assert(despachante:valid(cnh, "28383939640"))},
        {"CNH válida 6", ?_assert(despachante:valid(cnh, "73678694394"))},
        {"CNH válida 7", ?_assert(despachante:valid(cnh, "45914218712"))},
        {"CNH válida 8", ?_assert(despachante:valid(cnh, "61642236356"))},
        {"CNH válida 9", ?_assert(despachante:valid(cnh, "35957469495"))},
        {"CNH válida 10", ?_assert(despachante:valid(cnh, "54204353900"))},
        {"CNH válida 11", ?_assert(despachante:valid(cnh, "74132291573"))},
        {"CNH válida 12", ?_assert(despachante:valid(cnh, "53894146519"))},
        {"CNH válida 13", ?_assert(despachante:valid(cnh, "99536436975"))},
        {"CNH válida formatada", ?_assert(despachante:valid(cnh, "148.682.642-03"))},
        {"CNH válida com traços", ?_assert(despachante:valid(cnh, "957-220-920-35"))}
    ].

cnh_invalid_test_() ->
    [
        {"CNH inválida (dígito errado 1)", ?_assertNot(despachante:valid(cnh, "02650306462"))},
        {"CNH inválida (dígito errado 2)", ?_assertNot(despachante:valid(cnh, "02650306460"))},
        {"CNH inválida (todos iguais 1)", ?_assertNot(despachante:valid(cnh, "11111111111"))},
        {"CNH inválida (todos iguais 2)", ?_assertNot(despachante:valid(cnh, "00000000000"))},
        {"CNH inválida (todos iguais 3)", ?_assertNot(despachante:valid(cnh, "99999999999"))},
        {"CNH inválida (curto demais)", ?_assertNot(despachante:valid(cnh, "0265030646"))},
        {"CNH inválida (longo demais)", ?_assertNot(despachante:valid(cnh, "026503064611"))},
        {"CNH inválida (caracteres)", ?_assertNot(despachante:valid(cnh, "aaa.bbb.ccc-dd"))},
        {"CNH inválida (espaços)", ?_assertNot(despachante:valid(cnh, "026 503 064 61"))},
        {"CNH inválida (vazio)", ?_assertNot(despachante:valid(cnh, ""))},
        {"CNH inválida (undefined)", ?_assertNot(despachante:valid(cnh, undefined))},
        {"CNH inválida (dígito errado 3)", ?_assertNot(despachante:valid(cnh, "04463004503"))},
        {"CNH inválida (dígito errado 4)", ?_assertNot(despachante:valid(cnh, "01943736169"))},
        {"CNH inválida (lixo)", ?_assertNot(despachante:valid(cnh, "026$$.503\n064...61"))},
        {"CNH inválida (número+texto)", ?_assertNot(despachante:valid(cnh, "02650306461abc"))}
    ].

%%====================================================================
%% Testes para CNS
%%====================================================================

cns_valid_test_() ->
    [
        {"CNS válido 1", ?_assert(despachante:valid(cns, "283204477850003"))},
        {"CNS válido 2", ?_assert(despachante:valid(cns, "171427909280000"))},
        {"CNS válido 3", ?_assert(despachante:valid(cns, "291188136500008"))},
        {"CNS válido 4", ?_assert(despachante:valid(cns, "902309125473140"))},
        {"CNS válido 5", ?_assert(despachante:valid(cns, "906161762742522"))},
        {"CNS válido 6", ?_assert(despachante:valid(cns, "254754183030000"))},
        {"CNS válido 7", ?_assert(despachante:valid(cns, "989247262118517"))},
        {"CNS válido 8", ?_assert(despachante:valid(cns, "838549318935653"))},
        {"CNS válido 9", ?_assert(despachante:valid(cns, "871106875121785"))},
        {"CNS válido 10", ?_assert(despachante:valid(cns, "239396515040003"))},
        {"CNS válido 11", ?_assert(despachante:valid(cns, "702514333998537"))},
        {"CNS válido 12", ?_assert(despachante:valid(cns, "937477185358577"))},
        {"CNS válido 13", ?_assert(despachante:valid(cns, "702605242628389"))},
        {"CNS válido formatado 1", ?_assert(despachante:valid(cns, "833.3564.7964.3881"))},
        {"CNS válido formatado 2", ?_assert(despachante:valid(cns, "229.5360.8252.0005"))}
    ].

cns_invalid_test_() ->
    [
        {"CNS inválido (dígito errado 1)", ?_assertNot(despachante:valid(cns, "706004608779682"))},
        {"CNS inválido (dígito errado 2)", ?_assertNot(despachante:valid(cns, "758232072458002"))},
        {"CNS inválido (todos iguais 1)", ?_assertNot(despachante:valid(cns, "111111111111111"))},
        {"CNS inválido (todos iguais 2)", ?_assertNot(despachante:valid(cns, "000000000000000"))},
        {"CNS inválido (todos iguais 3)", ?_assertNot(despachante:valid(cns, "999999999999999"))},
        {"CNS inválido (curto demais)", ?_assertNot(despachante:valid(cns, "70600460877968"))},
        {"CNS inválido (longo demais)", ?_assertNot(despachante:valid(cns, "7060046087796811"))},
        {"CNS inválido (caracteres)", ?_assertNot(despachante:valid(cns, "aaa.bbbb.cccc.dddd"))},
        {"CNS inválido (espaços)", ?_assertNot(despachante:valid(cns, "706 0046 0877 9681"))},
        {"CNS inválido (vazio)", ?_assertNot(despachante:valid(cns, ""))},
        {"CNS inválido (undefined)", ?_assertNot(despachante:valid(cns, undefined))},
        {"CNS inválido (dígito inicial 3)", ?_assertNot(despachante:valid(cns, "306004608779681"))},
        {"CNS inválido (dígito inicial 4)", ?_assertNot(despachante:valid(cns, "406004608779681"))},
        {"CNS inválido (lixo)", ?_assertNot(despachante:valid(cns, "706$$.0046\n0877...9681"))},
        {"CNS inválido (número+texto)", ?_assertNot(despachante:valid(cns, "706004608779681abc"))}
    ].

%%====================================================================
%% Testes para RENAVAM
%%====================================================================

renavam_valid_test_() ->
    [
        {"RENAVAM válido 1", ?_assert(despachante:valid(renavam, "02107218549"))},
        {"RENAVAM válido 2", ?_assert(despachante:valid(renavam, "60184842283"))},
        {"RENAVAM válido 3", ?_assert(despachante:valid(renavam, "48740819407"))},
        {"RENAVAM válido 4", ?_assert(despachante:valid(renavam, "57831500526"))},
        {"RENAVAM válido 5", ?_assert(despachante:valid(renavam, "03465254270"))},
        {"RENAVAM válido 6", ?_assert(despachante:valid(renavam, "58648902813"))},
        {"RENAVAM válido 7", ?_assert(despachante:valid(renavam, "27533756334"))},
        {"RENAVAM válido 8", ?_assert(despachante:valid(renavam, "39649066540"))},
        {"RENAVAM válido 9", ?_assert(despachante:valid(renavam, "38845724077"))},
        {"RENAVAM válido 10", ?_assert(despachante:valid(renavam, "73579956941"))},
        {"RENAVAM válido 11", ?_assert(despachante:valid(renavam, "64076511762"))},
        {"RENAVAM válido 12", ?_assert(despachante:valid(renavam, "89797993128"))},
        {"RENAVAM válido 13", ?_assert(despachante:valid(renavam, "37283984448"))},
        {"RENAVAM válido formatado 1", ?_assert(despachante:valid(renavam, "891.937.676-70"))},
        {"RENAVAM válido formatado 2", ?_assert(despachante:valid(renavam, "854.557.060-50"))}
    ].

renavam_invalid_test_() ->
    [
        {"RENAVAM inválido (dígito errado 1)", ?_assertNot(despachante:valid(renavam, "63314716632"))},
        {"RENAVAM inválido (dígito errado 2)", ?_assertNot(despachante:valid(renavam, "00471354136"))},
        {"RENAVAM inválido (todos iguais 1)", ?_assertNot(despachante:valid(renavam, "11111111111"))},
        {"RENAVAM inválido (todos iguais 2)", ?_assertNot(despachante:valid(renavam, "00000000000"))},
        {"RENAVAM inválido (todos iguais 3)", ?_assertNot(despachante:valid(renavam, "99999999999"))},
        {"RENAVAM inválido (curto demais)", ?_assertNot(despachante:valid(renavam, "6331471663"))},
        {"RENAVAM inválido (longo demais)", ?_assertNot(despachante:valid(renavam, "633147166311"))},
        {"RENAVAM inválido (caracteres)", ?_assertNot(despachante:valid(renavam, "aaa.bbb.ccc-dd"))},
        {"RENAVAM inválido (espaços)", ?_assertNot(despachante:valid(renavam, "633 147 166 31"))},
        {"RENAVAM inválido (vazio)", ?_assertNot(despachante:valid(renavam, ""))},
        {"RENAVAM inválido (undefined)", ?_assertNot(despachante:valid(renavam, undefined))},
        {"RENAVAM inválido (dígito errado 3)", ?_assertNot(despachante:valid(renavam, "82682240107"))},
        {"RENAVAM inválido (dígito errado 4)", ?_assertNot(despachante:valid(renavam, "18550734793"))},
        {"RENAVAM inválido (lixo)", ?_assertNot(despachante:valid(renavam, "633$$.147\n166...31"))},
        {"RENAVAM inválido (número+texto)", ?_assertNot(despachante:valid(renavam, "63314716631abc"))}
    ].

%%====================================================================
%% Testes para modo estrito (todos os tipos)
%%====================================================================

strict_mode_test_() ->
    [
        {"CPF - aceita numérico no modo estrito", ?_assert(despachante:valid(cpf, "29537995593", true))},
        {"CPF - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(cpf, "295.379.955-93", true))},
        {"CNPJ - aceita numérico no modo estrito", ?_assert(despachante:valid(cnpj, "11222333000181", true))},
        {"CNPJ - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(cnpj, "11.222.333/0001-81", true))},
        {"PIS - aceita numérico no modo estrito", ?_assert(despachante:valid(pis, "12053525128", true))},
        {"PIS - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(pis, "120.5352.512-8", true))},
        {"CNH - aceita numérico no modo estrito", ?_assert(despachante:valid(cnh, "02650306461", true))},
        {"CNH - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(cnh, "026.503.064-61", true))},
        {"CNS - aceita numérico no modo estrito", ?_assert(despachante:valid(cns, "783802458524702", true))},
        {"CNS - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(cns, "783.802.458.524.702", true))},
        {"RENAVAM - aceita numérico no modo estrito", ?_assert(despachante:valid(renavam, "95772016140", true))},
        {"RENAVAM - rejeita formatado no modo estrito", ?_assertNot(despachante:valid(renavam, "957.720.161-40", true))}
    ].

%%====================================================================
%% Testes para tipo de documento inválido
%%====================================================================

invalid_doc_type_test_() ->
    [
        {"Tipo de documento inválido", ?_assertNot(despachante:valid(titulo_eleitor, "123456789012"))},
        {"Tipo de documento como atom desconhecido", ?_assertNot(despachante:valid(rg, "12345678"))},
        {"Tipo de documento como string", ?_assertNot(despachante:valid("cpf", "29537995593"))},
        {"Tipo de documento como número", ?_assertNot(despachante:valid(123, "29537995593"))}
    ].
