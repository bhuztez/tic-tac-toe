this project starts as a simple echo server.

compile server

```
$ erl -make
```

run server

```
$ erl -pz ebin
1> application:start(ttt).
```

run ttt.py will start a telnet connection.

whatever you enter, server will echo back. press Ctrl-D to quit.


