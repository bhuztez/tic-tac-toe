#!/usr/bin/env python2

from telnetlib import Telnet


c = Telnet("localhost", 9999)
c.interact()
c.close()

