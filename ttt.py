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
    c = Telnet("localhost", 9999)
    c.interact()
    c.close()
finally:
    erl.terminate()
    erl.wait()
    sys.stdout.write(erl.stdout.read())
    erl.stdin.close()
    erl.stdout.close()

