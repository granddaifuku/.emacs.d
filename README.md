# My Emacs setting

## PC setting up

### ccls setting
For more detail, see [github ccls wiki](https://github.com/MaskRay/ccls/wiki/Build)  
1. install llvm  
```
$ brew install llvm
$ (Follow some instructions showen after installation)
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
