#!/bin/bash

set -ex

wget -qO- https://github.com/Kitware/CMake/releases/download/v3.16.3/cmake-3.16.3-Linux-x86_64.tar.gz | sudo tar xz --strip=1 -C /usr/local/
which cmake
cmake --version
