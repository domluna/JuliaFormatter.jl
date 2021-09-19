<img src="https://rawcdn.githack.com/JuliaDiff/ChainRulesCore.jl/b0b8dbf26807f8f6bc1a3c073b6720b8d90a8cd4/docs/src/assets/logo.svg" width="256"/>

# ChainRules

[![CI](https://github.com/JuliaDiff/ChainRules.jl/workflows/CI/badge.svg?branch=master)](https://github.com/JuliaDiff/ChainRules.jl/actions?query=workflow%3ACI)
[![Travis](https://travis-ci.org/JuliaDiff/ChainRules.jl.svg?branch=master)](https://travis-ci.org/JuliaDiff/ChainRules.jl)
[![Coveralls](https://coveralls.io/repos/github/JuliaDiff/ChainRules.jl/badge.svg?branch=master)](https://coveralls.io/github/JuliaDiff/ChainRules.jl?branch=master)
[![PkgEval](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/C/ChainRules.svg)](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/report.html)
[![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)
[![ColPrac: Contributor's Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor's%20Guide-blueviolet)](https://github.com/SciML/ColPrac)
[![DOI](https://zenodo.org/badge/157899791.svg)](https://zenodo.org/badge/latestdoi/157899791)

**Docs:**
[![](https://img.shields.io/badge/docs-master-blue.svg)](https://juliadiff.org/ChainRulesCore.jl/dev)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://juliadiff.org/ChainRulesCore.jl/stable)

The ChainRules package provides a variety of common utilities that can be used by downstream automatic differentiation (AD) tools to define and execute forward-, reverse-, and mixed-mode primitives.

The core logic of ChainRules is implemented in [ChainRulesCore.jl](https://github.com/JuliaDiff/ChainRulesCore.jl).
To add ChainRules support to your package, by defining new `rrule`s or `frules`, you only need to depend on the very light-weight package ChainRulesCore.jl.
This repository contains ChainRules.jl, which is what people actually use directly.
ChainRules reexports all the ChainRulesCore functionality, and has all the rules for the Julia standard library.


Here are some of the core features of the package:

- Mixed-mode composability without being coupled to a specific AD implementation.
- Extensible rules: package authors can add rules (and thus AD support) to the functions in their packages, without needing to make a PR to ChainRules.jl .
- Control-inverted design: rule authors can fully specify derivatives in a concise manner that supports computational efficiencies, so we will only compute as much as the user requests.
- Propagation semantics built-in, with default implementations that allow rule authors to easily opt-in to common optimizations (fusion, increment elision, memoization, etc.).
