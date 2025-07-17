# 🔐 Crypto Hashing in eyelet

This eyelet logic program demonstrates how to compute **cryptographic hashes** (MD5, SHA-1, SHA-256, SHA-512) from a string literal using N3 Logic and the `crypto:` vocabulary.

It simulates typical data transformation steps useful in **authentication**, **integrity checking**, and **data fingerprinting** within RDF-based systems.

---

## 📚 Prefixes

```turtle
@prefix crypto: <http://www.w3.org/2000/10/swap/crypto#> .
@prefix log:    <http://www.w3.org/2000/10/swap/log#> .
@prefix var:    <http://www.w3.org/2000/10/swap/var#> .
@prefix :       <http://example.org/#> .
```

---

## 🧾 Fact: Input Parameter

```turtle
:Let :param "hello world" .
```

This assigns the input string `"hello world"` to the param of `:Let`.

---

## 🔍 Query: Compute Cryptographic Hashes

```turtle
[ log:graph (
    [ log:triple (:Let :param var:X) ]
    [ log:triple (var:X crypto:md5 var:md5) ]
    [ log:triple (var:X crypto:sha var:sha) ]
    [ log:triple (var:X crypto:sha256 var:sha226) ]
    [ log:triple (var:X crypto:sha512 var:sha512) ]
)] log:impliesAnswer [ log:graph (
    [ log:triple (var:Y :subject var:X) ]
    [ log:triple (var:Y crypto:md5 var:md5) ]
    [ log:triple (var:Y crypto:sha var:sha) ]
    [ log:triple (var:Y crypto:sha256 var:sha226) ]
    [ log:triple (var:Y crypto:sha512 var:sha512) ]
)].
```

This query:

* Binds the value `"hello world"` to `var:X`
* Computes various hash digests:

  * `crypto:md5` → MD5 hash
  * `crypto:sha` → SHA-1 hash
  * `crypto:sha256` → SHA-256 hash
  * `crypto:sha512` → SHA-512 hash
* Produces a result node (`var:Y`) describing the subject and its digests

---

### 🧪 Sample Output (expected structure)

```turtle
_:result :subject "hello world" ;
         crypto:md5 "5eb63bbbe01eeed093cb22bb8f5acdc3" ;
         crypto:sha "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed" ;
         crypto:sha256 "b94d27b9934d3e08a52e52d7da7dabfa..." ;
         crypto:sha512 "309ecc489c12d6eb4cc40f50c902f2b4..." .
```

*(actual hash values will be computed by eyelet or an N3 reasoning engine that supports `crypto:` functions)*

---

> **TIP:** Use `crypto:` predicates to bind cryptographic functions directly into RDF-based reasoning.

> **NOTE:** Hashing in eyelet can be used for identifying content, verifying integrity, or linking anonymized data.

> **Reference:** Built on the [N3 Crypto vocabulary](http://www.w3.org/2000/10/swap/crypto#), this example integrates hash functions into logical inference using eyelet.

