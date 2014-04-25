#!/bin/sh

erl -pa $PWD/deps/*/ebin -pa $PWD/ebin -s poldercast test
