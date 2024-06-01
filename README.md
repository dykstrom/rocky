# Rocky

<div style="text-align: left">

[![Open Issues](https://img.shields.io/github/issues/dykstrom/rocky)](https://github.com/dykstrom/rocky/issues)
[![Latest Release](https://img.shields.io/github/v/release/dykstrom/rocky?display_name=release)](https://github.com/dykstrom/rocky/releases)
![Downloads](https://img.shields.io/github/downloads/dykstrom/rocky/total)
![License](https://img.shields.io/github/license/dykstrom/rocky)
![Top Language](https://img.shields.io/github/languages/top/dykstrom/rocky)

</div>

Rocky is an [XBoard](https://www.gnu.org/software/xboard) compatible chess engine written in the [Roc](https://www.roc-lang.org) programming language.


## Installation

Rocky is currently available for Linux and macOS.

Download the latest release for your OS from the GitHub
[releases page](https://github.com/dykstrom/rocky/releases),
and unzip it in a directory of your choice, for example `~/rocky`.


### XBoard

Edit the XBoard resource file `~/.xboardrc` and add a new line to the option `-firstChessProgramNames`, like this:

`"Rocky" -fcp ./rocky -fd ~/rocky`


## About

Rocky is still in an early stage, and is currently very weak. It has no opening book, no depth search, no time management, and limited positional knowledge. Over time, some of these things will hopefully improve.
