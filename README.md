![](https://travis-ci.org/derniercri/coers.svg?branch=master)

coers
=====

Coers is a very small library to provide small coercion
on primitive types in Erlang. This library was built
essentially for internal tools at derniercri.io

Build & test
-----
    $ # Compile the library
    $ rebar3 compile
    $ # run the tests using eUnit
    $ rebar3 eunit


Usage
--------

You can run an erlang shell with `coers` completely loaded : `rebar3 shell` :

```shell 
===> Verifying dependencies...
===> Compiling coers
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.1  (abort with ^G)
```

### Basics

Each coercion is wrapped into a special record: 

```erlang
-record(result, {
  succeeded :: boolean(),
  value     :: term()
}).
```

If a coercion fail, the `value` member is assigned with a default value and the `succeed`
member is `false`. If the coersion succeed, the `value` member becomes the coerced data and the 
`succeed` member becomes `true`.

You can use these 3 combinators to have information about coercion status : 

-  `-spec succeed(result()) -> boolean().`
-  `-spec fail(result()) -> boolean().`
-  `-spec value(result()) -> term().`

For example : 

```shell
1> X = coers:to_int("10").
{result,true,10}
2> Y = coers:to_int("foo").
{result,false,0}
3> [coers:succeed(X), coers:succeed(Y), coers:fail(X), coers:fail(Y)].
[true,false,false,true]
4> [coers:value(X), coers:value(Y)].
[10,0]
```

Documentation
--------

This page exposes the feature list : <http://xvw.github.io/coers/coers.html>


