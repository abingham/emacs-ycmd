#!/bin/bash

export YCMD_PATH="${HOME}/ycmd"

if [ ! -d "$YCMD_PATH/.git" ]; then
  git clone --depth=1 --recursive https://github.com/Valloric/ycmd ${YCMD_PATH}
fi

pushd ${YCMD_PATH}

git pull
git submodule update --init --recursive

export EXTRA_CMAKE_ARGS="-DCMAKE_CXX_COMPILER=/usr/lib/ccache/c++ -DCMAKE_C_COMPILER=/usr/lib/ccache/cc"
python build.py --clang-completer --gocode-completer --tern-completer

npm install -g typescript

popd
