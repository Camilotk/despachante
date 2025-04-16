# Despachante

**Validação de documentos brasileiros em Erlang.**

O `despachante` é uma biblioteca criada para validar informações de documentos brasileiros como CPF, CNPJ, RG, PIS, entre outros — tudo isso de forma modular, extensível e escrita em Erlang puro.

---

## Propósito

O Brasil tem muitos formatos e regras específicas pra documentos. O `despachante` quer resolver isso com uma base sólida, testável e simples de usar.

---

## ✅ Documentos suportados

- `cpf` — Validação com ou sem pontuação, modo estrito opcional

> Mais formatos como `cnpj`, `rg`, `pis`, `cns` e `renavam` serão adicionados.

---

## 🔍 Exemplo de uso

```erlang
%% modo flexível
despachante:valid(cpf, "825.205.780-25").

%% modo estrito (somente números)
despachante:valid(cpf, "82520578025", true).
```

---

## Roadmap

- [x] Validação de CPF
- [ ] Validação de CNPJ
- [ ] Mock de CPF/CNPJ válidos
- [ ] Validação de PIS/PASEP
- [ ] Normalização de entrada (com/sem máscara)
