# `llvm-msvc`


### add Something for ollvm
```
Original obfuscation passes including bogus control flow (-bcf), split basic block (-split), control flow flattening (-fla) and substitution (-sub) and MBA-substitution(-mba-subs) and Indirect-Call(-ind-call) and StringXor(-string-obfus) and ConstVarXor(-const-obfus) and VM-Flattening (-vm-fla).
``` 

### 感谢
```
https://github.com/gmh5225, 日天同学的llvm-msvc让人心情激动

```
### 参考
```
https://github.com/KomiMoe/Arkari/tree/llvm-17.x
https://github.com/AimiP02/BronyaObfus
https://github.com/za233/LLVMMyPass
https://github.com/DreamSoule/ollvm17
https://github.com/wwh1004/ollvm-16
```

### 计划
```
data-obfus 崩溃问题(目测与传统OLLVM冲突)
const-obfus 崩溃问题(目测与传统OLLVM冲突)
vm-fla和传统ollvm冲突
移植xVMP(LLVM18 IR重建xVMPInxx和处理PASS代码)
```

[![llvm-msvc-build](https://github.com/backengineering/llvm-msvc/actions/workflows/llvm-msvc-build.yml/badge.svg?branch=dev)](https://github.com/backengineering/llvm-msvc/actions/workflows/llvm-msvc-build.yml)
[![GitHub license](https://img.shields.io/github/license/backengineering/llvm-msvc)](https://github.com/backengineering/llvm-msvc/blob/main/LICENSE)

[![Github All Releases](https://img.shields.io/github/downloads/backengineering/llvm-msvc/total.svg)](https://github.com/backengineering/llvm-msvc/releases) 
[![GitHub release](https://img.shields.io/github/release/backengineering/llvm-msvc.svg)](https://github.com/backengineering/llvm-msvc/releases) 

llvm-msvc is a compiler based LLVM that is not restricted by MSVC. The goal is to have the same experience as MSVC on Windows.
You can use naked functions anywhere, as well as add custom support such as obfuscation.

## Features:
- SEH support.
- Compatible with MSVC syntax as much as possible.
- Windows Driver support.(Now only X64)
- Intrinsic support.
- Naked X64 inline asm support.
- Support multiple cores compilation.
- Support ``/MP`` when precompiled headers are present.


## FAQ
### Why do we make this project?
- Clang uses the GCC standard, MSVC has its own special syntax.
- Some of the code is rather hacky. Unable to submit to official.
- Waiting for official fix is ​​too long.

### How to compile?

```batch
X86：clang+lld+debug
    
mkdir build-debug-64
pushd build-debug-64
cmake .. -G "Visual Studio 17 2022" -A X64 -DLLVM_ENABLE_PROJECTS="clang;lld" -DCMAKE_INSTALL_PREFIX=E:\llvm\install-debug-64 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_OBFUSCATION_LINK_INTO_TOOLS=ON -DCMAKE_BUILD_TYPE=Debug ../llvm
msbuild /m -p:Configuration=Debug INSTALL.vcxproj

or
cmake -Bbuild -A X64 -DLLVM_ENABLE_PROJECTS="clang;lld" -DCMAKE_INSTALL_PREFIX=E:\llvm\install-debug-64 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_OBFUSCATION_LINK_INTO_TOOLS=ON -DCMAKE_BUILD_TYPE=Debug llvm


X86：clang+lld+RelWithDebInfo

mkdir build-RelWithDebInfo-64
pushd build-RelWithDebInfo-64
cmake .. -G "Visual Studio 17 2022" -A X64 -DLLVM_ENABLE_PROJECTS="clang;lld" -DCMAKE_INSTALL_PREFIX=E:\llvm\install-RelWithDebInfo-64 -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_ENABLE_ZLIB=OFF -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_OBFUSCATION_LINK_INTO_TOOLS=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_USE_CRT_RELEASE=MT ../llvm
msbuild /m -p:Configuration=RelWithDebInfo INSTALL.vcxproj 

X86：clang+lld+release

mkdir build-release-64
pushd build-release-64
cmake .. -G "Visual Studio 17 2022" -A X64 -DLLVM_ENABLE_PROJECTS="clang;lld" -DCMAKE_INSTALL_PREFIX=E:\llvm\install-release-64 -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_ENABLE_ZLIB=OFF -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_OBFUSCATION_LINK_INTO_TOOLS=ON -DCMAKE_BUILD_TYPE=release -DLLVM_USE_CRT_RELEASE=MT ../llvm
msbuild /m -p:Configuration=release INSTALL.vcxproj 
```

### 混淆例子
Add To VS Project Compiler Cmdline
```
-mllvm -sub -mllvm -sub_loop=1 -mllvm -split -mllvm -split_num=3 -mllvm -bcf -mllvm -bcf_loop=1 -mllvm -bcf_prob=40 -mllvm -string-obfus -mllvm -ind-call -mllvm -fla -mllvm -mba-subs
```
#### 需要修改载研究的部分
```
-mllvm -data-obfus -mllvm -const-obfus -mllvm -const-glb -mllvm -string-obfus -mllvm -ind-call -mllvm -vm-fla -mllvm -mba-subs
```
### How to contribute?
- https://github.com/HyunCafe/contribute-practice
- https://docs.github.com/en/get-started/quickstart/contributing-to-projects

### How can I learn LLVM?
If you don't know how to learn LLVM, you can check out this [repository](https://github.com/gmh5225/awesome-llvm-security) of mine.

### Can it run on linux?
No.

### Can it run on macos?
No.

## Credits
- LLVM
- Some anonymous people


