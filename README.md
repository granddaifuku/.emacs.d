# My Emacs setting

## PC setting up

### ccls setting
For more detail, see [github ccls wiki](https://github.com/MaskRay/ccls/wiki/Build)  
1. install llvm  
```
$ brew install llvm
$ (Follow some instructions shown after installation)
```


2. install ccls  
```
$ cd /usr/local/opt
$ git clone $(ccls git repository)
```

3. build 
```
$ brew info llvm
$ cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/Cellar/llvm/<Your Version>/lib/cmake
$ cmake --build Release
```

### rtags setting
For more detail, see [github rtags](https://github.com/Andersbakken/rtags/wiki)
1. install rtags  
```
$ cd /usr/local/opt
$ git clone --recursive https://github.com/Andersbakken/rtags.git
$ cd rtags
```

2. out-source build  
```
$ mkdir build && cd build
$ brew info llvm
$ cmake -DLIBCLANG_LLVM_CONFIG_EXECUTABLE=/usr/local/Cellar/llvm/<llvm-version>/bin/llvm-config -DRTAGS_BUILD_CLANG=1 ..
$ make
$ make install
```
