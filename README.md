# Despachante

**Validação de documentos brasileiros em Erlang.**

`despachante` é uma biblioteca Erlang para validação de documentos brasileiros como CPF, CNPJ, PIS/PASEP, CNH, CNS e RENAVAM.  

## Documentos suportados

- `cpf` — Cadastro de Pessoa Física  
- `cnpj` — Cadastro Nacional de Pessoa Jurídica  
- `pis` — Programa de Integração Social / PASEP  
- `cnh` — Carteira Nacional de Habilitação  
- `cns` — Cartão Nacional de Saúde  
- `renavam` — Registro Nacional de Veículos Automotores

## Exemplo de uso

```erlang
%% Validação flexível (com ou sem pontuação)
despachante:valid(cpf, "825.205.780-25").

%% Validação estrita (somente dígitos)
despachante:valid(cpf, "82520578025", true).
```

## Instalação

Adicione `despachante` ao seu `rebar.config`:

```erlang
{deps, [
  {despachante, "0.2.0"}
]}.
```

Ou, se estiver usando `mix` com o `rebar3_hex`, adicione ao seu `mix.exs`:

```elixir
def deps do
  [
    {:despachante, "~> 0.2.0"}
  ]
end
```
