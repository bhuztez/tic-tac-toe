compile server

```
$ erl -make
```

run server

```
$ erl -pz ebin
1> application:start(ttt).
```

run client
```
python ttt.py
```
