#!/usr/bin/env python2

import sys
from telnetlib import Telnet
from shlex import split
from subprocess import Popen, PIPE, STDOUT, call
from time import sleep


if call(split("erlc ttt.erl")):
    exit(1)

erl = Popen(split("erl -s ttt -noshell"), stdin=PIPE, stdout=PIPE, stderr=STDOUT)
sleep(1)
try:
    c1 = Telnet("localhost", 9999)
    c2 = Telnet("localhost", 9999)

    sys.stdout.write('c2 ' + c2.read_some())
    sys.stdout.write('c1 ' + c1.read_some())
    c1.write("PONG\r\n4\r\n")
    sys.stdout.write('c2 ' + c2.read_some())
    c2.write("PONG\r\n0\r\n")
    sys.stdout.write('c1 ' + c1.read_some())
    c1.write("PONG\r\n1\r\n")
    sys.stdout.write('c2 ' + c2.read_some())
    c2.write("PONG\r\n3\r\n")
    sys.stdout.write('c1 ' + c1.read_some())
    c1.write("PONG\r\n7\r\n")
    sys.stdout.write('c2 ' + c2.read_some())
    sys.stdout.write('c1 ' + c1.read_some())
    sys.stdout.write('c2 ' + c2.read_some())
    sys.stdout.write('c1 ' + c1.read_some())

    c1.close()
    c2.close()
finally:
    erl.terminate()
    erl.wait()
    sys.stdout.write(erl.stdout.read())
    erl.stdin.close()
    erl.stdout.close()

