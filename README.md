## Compiling LLVM

```
cd llvm
tar xvf llvm-10.0.1.src.tar.xz
mkdir llvm-10.0.1/build
cd llvm-10.0.1/build
cmake .. -DCMAKE_INSTALL_PREFIX=<full-path>/llvm/llvm-10.0.1 -DLLVM_ENABLE_ASSERTIONS=On -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=WebAssembly
cmake --build . --target install
```

Build and run

```
LLVM_SYS_100_PREFIX=<full-path>/llvm/llvm-10.0.1 cargo build
LLVM_SYS_100_PREFIX=<full-path>/llvm/llvm-10.0.1 cargo run
```
