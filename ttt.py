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

    c1.write("ping\n")
    assert c2.read_some() == "ping\n"
    c2.write("pong\n")
    assert c1.read_some() == "pong\n"

    c1.close()
    c2.close()
finally:
    erl.terminate()
    erl.wait()
    sys.stdout.write(erl.stdout.read())
    erl.stdin.close()
    erl.stdout.close()

