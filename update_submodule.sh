#!/bin/bash

git submodule update --init --recursive

git submodule foreach git reset --hard

git submodule foreach git checkout master
