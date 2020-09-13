# My Emacs setting

## PC setting up

### ccls setting
For more detail, see [github ccls wiki](https://github.com/MaskRay/ccls/wiki/Build)  
1. Install llvm  
```
$ brew install llvm
$ (Follow some instructions shown after installation)
```


2. Install ccls  
```
$ cd /usr/local/opt
$ git clone $(ccls git repository)
```

3. Build 
```
$ cd ccls
$ mkdir build && cd build
$ brew info llvm
$ cmake -H.. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/Cellar/llvm/<Your Version>/lib/cmake
$ cmake --build Release
```

4. Create compile_commands.json
```
$ cmake -H.. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
$ cd <c++ project root dir>
$ ln -s /usr/local/opt/ccls/build/Debug/compile_commands.json .
$ touch .ccls-root
```

5. In case you want to use "bits/stdc++.h"
```
$ cd /usr/local/Cellar/llvm/<llvm version>/lib/clang/<llvm version>/include
$ mkdir bits && cd bits
$ touch stdc++.h (copy the content of stdc++.h to it)
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
$ cmake LIBCLANG_LLVM_CONFIG_EXECUTABLE=/usr/local/Cellar/llvm/10.0.1/bin/llvm-config OPENSSL_ROOT_DIR=/usr/local/opt/openssl cmake -DRTAGS_BUILD_CLANG=1 -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
$ sudo make
$ sudo make install
```
