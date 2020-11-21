# osc

*Erlang support for the Open Sound Control protocol *

[![Build Status][gh-actions-badge]][gh-actions]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

This is an implementation of the Open Sound Control (OSC) protocol written
in Erlang with OTP.

For more information about OSC, see the links below in the 
[Resources](#resources) section.

## Use

Compile:

    rebar3 compile

Test:

    rebar3 eunit

## Resources

* [OSC](https://en.wikipedia.org/wiki/Open_Sound_Control) on WikiPedia
* [Intro to OSC](http://opensoundcontrol.org/introduction-osc)
* [Open Sound Control](https://www.cnmat.berkeley.edu/opensoundcontrol)
* A [Common Lisp OSC implementation](https://github.com/jamieforth/osc)


[//]: ---Named-Links---

[logo]: priv/images/logo-v2.png
[logo-large]: priv/images/logo-v2-large.png
[github]: https://github.com/erlsci/osc
[gh-actions-badge]: https://github.com/erlsci/osc/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/erlsci/osc/actions
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/erlsci/osc/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/erlsci/osc/tags
[github-tag-badge]: https://img.shields.io/github/tag/erlsci/osc.svg
[github-downloads]: https://img.shields.io/github/downloads/erlsci/osc/total.svg
