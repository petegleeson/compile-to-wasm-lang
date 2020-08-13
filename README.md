## Compiling LLVM

```
cd llvm-project
tar xvf llvm-10.0.1.src.tar.xz
mkdir build
cd build
cmake ../llvm -DCMAKE_INSTALL_PREFIX=<full-path>/llvm-project/out -DLLVM_ENABLE_ASSERTIONS=On -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=WebAssembly -DLLVM_ENABLE_PROJECTS="lld"
cmake --build . --target install
```

Build and run

```
LLVM_SYS_100_PREFIX=<full-path>/llvm-project/out cargo build
LLVM_SYS_100_PREFIX=<full-path>/llvm-project/out cargo run
```
