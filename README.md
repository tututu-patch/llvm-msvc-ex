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
https://github.com/gmh5225/awesome-llvm-security#ollvm

```

### 计划
- [x] 在vm-fla-sym添加反符号执行和反内存追踪
- [x] vm-fla-enc 对vm-fla的部分数据加密
- [ ] MBA-subs的bug
- [ ] 移植xVMP
- [x] 在vm-fla-enc中使用间接全局变量访问


[![llvm-msvc-build](https://github.com/backengineering/llvm-msvc/actions/workflows/llvm-msvc-build.yml/badge.svg?branch=dev)](https://github.com/backengineering/llvm-msvc/actions/workflows/llvm-msvc-build.yml)
[![GitHub license](https://img.shields.io/github/license/backengineering/llvm-msvc)](https://github.com/backengineering/llvm-msvc/blob/main/LICENSE)

[![Github All Releases](https://img.shields.io/github/downloads/backengineering/llvm-msvc/total.svg)](https://github.com/backengineering/llvm-msvc/releases) 
[![GitHub release](https://img.shields.io/github/release/backengineering/llvm-msvc.svg)](https://github.com/backengineering/llvm-msvc/releases) 

llvm-msvc is a compiler based on LLVM that isn't limited by MSVC. The aim is to provide the same experience as MSVC on Windows. You can use naked functions anywhere and also add custom support like obfuscation.

## Features:
- Compatible with MSVC syntax as much as possible.
- Improved SEH stability.
- Added some special Intrinsic functions(``__vmx_vmread``/``__vmx_write``).
- Supports x64/ARM64 windows drivers.
- Allows naked X64 inline asm.
- Enables multiple cores compilation.
- Supports ``/MP`` when precompiled headers are present.
- Supports ``/GL`` (LTO optimization).


## FAQ
### Why do we make this project?
- Clang follows the GCC standard, while MSVC has its own unique syntax.
- Some of the code is pretty hacky. Can't submit it officially.
- Waiting for the official fix takes too long.

### How to compile?

```batch

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
#### 最大保护（文件将超过10MB）
```
-mllvm -data-obfus -mllvm -const-obfus -mllvm -string-obfus -mllvm -ind-call -mllvm -vm-fla -mllvm -fla -mllvm -sub -mllvm -sub_loop=1 -mllvm -split -mllvm -split_num=3 -mllvm -bcf -mllvm -bcf_loop=1 -mllvm -bcf_prob=40 -mllvm -vm-fla-enc -mllvm -vm-fla-sym
```
#### 单纯使用特色部分（轻量模式）
```
-mllvm -data-obfus -mllvm -const-obfus -mllvm -string-obfus -mllvm -ind-call -mllvm -vm-fla -mllvm -vm-fla-enc -mllvm -vm-fla-sym
```
#### 需要修改载研究的部分
```

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


