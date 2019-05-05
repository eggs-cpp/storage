# Eggs.Storage
#
# Copyright Agustin K-ballo Berge, Fusion Fenix 2019
#
# Distributed under the Boost Software License, Version 1.0. (See accompanying
# file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.0)

project(Eggs.Storage CXX)

# Build
add_library(_eggs_storage INTERFACE)
target_include_directories(_eggs_storage INTERFACE include)

add_library(Eggs::Storage ALIAS _eggs_storage)
