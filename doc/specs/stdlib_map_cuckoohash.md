---
title: Unordered map implementation (Cuckoo hash)
...

# The `stdlib_map_cuckoohash` module

[TOC]


## Introduction

This module implements an unordered map using the [cuckoo hashing](https://en.wikipedia.org/wiki/Cuckoo_hashing) approach.


## Derived types

The `cuckoo_hash` derived type is provided as implementation of a `map_class`.


### `cuckoo_hash` implementation of hash map

The `cuckoo_hash` data type provides an implementation of the [[map\_class]] abstract base type.
Only additions to the base class are listed here.
