#!/usr/bin/env python2

from telnetlib import Telnet


c1 = Telnet("localhost", 9999)
c2 = Telnet("localhost", 9999)

c1.write("ping\n")
assert c2.read_some() == "ping\n"
c2.write("pong\n")
assert c1.read_some() == "pong\n"

c1.close()
c2.close()

