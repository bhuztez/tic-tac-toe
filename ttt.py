#!/usr/bin/env python2

import pygtk
pygtk.require ('2.0')

from twisted.internet import gtk2reactor
gtk2reactor.install()

from twisted.internet import reactor
from twisted.internet.protocol import ClientCreator
from twisted.protocols.basic import LineReceiver
from twisted.python import log

import gobject, gtk
import math, re, sys
log.startLogging(sys.stdout)



class NoughtMixin(object):


    def _draw(self, context, width, height):
        context.set_line_width(min(width, height)/12)
        context.arc(width/2, height/2, min(width, height)/3, 0, 2 * math.pi)
        context.stroke()



class CrossMixin(object):


    def _draw(self, context, width, height):
        rad = min(width, height)/3
        context.set_line_width(min(width, height)/12)
        context.move_to(width/2-rad, height/2-rad)
        context.line_to(width/2+rad, height/2+rad)
        context.move_to(width/2-rad, height/2+rad)
        context.line_to(width/2+rad, height/2-rad)
        context.stroke()



class Shape(gtk.DrawingArea):

    __gsignals__ = { "expose-event": "override" }


    def __init__(self):
        gtk.DrawingArea.__init__(self)
        self.set_size_request(50, 50)


    def do_expose_event(self, event):
        width, height = self.window.get_size()
        context = self.window.cairo_create()
        self._draw(context, width, height)



class Button(gtk.DrawingArea):

    __gsignals__ = {
        "expose-event": "override" ,
        "enter-notify-event": "override",
        "leave-notify-event": "override",
        "button-press-event": "override",
        "clicked": (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()) }


    def __init__(self):
        gtk.DrawingArea.__init__(self)
        self.set_size_request(50, 50)
        self.add_events(gtk.gdk.ENTER_NOTIFY_MASK | gtk.gdk.LEAVE_NOTIFY_MASK | gtk.gdk.BUTTON_PRESS_MASK)


    def do_expose_event(self, event):
        if self.get_state() == gtk.STATE_PRELIGHT:
            self._draw(self.window.cairo_create(), *self.window.get_size())

    
    def do_enter_notify_event(self, event):
        self.set_state(gtk.STATE_PRELIGHT)


    def do_leave_notify_event(self, event):
        self.set_state(gtk.STATE_NORMAL)


    def do_button_press_event(self, event):
        if event.button == 1:
            self.emit("clicked")



class NoughtShape(NoughtMixin, Shape): pass
class CrossShape(CrossMixin, Shape): pass
class NoughtButton(NoughtMixin, Button): pass
class CrossButton(CrossMixin, Button): pass



class Board(gtk.Table):

    COORS = [
        (0,1,0,1), (1,2,0,1), (2,3,0,1),
        (0,1,1,2), (1,2,1,2), (2,3,1,2),
        (0,1,2,3), (1,2,2,3), (2,3,2,3)]


    def __init__(self):
        gtk.Table.__init__(self, rows=3, columns=3, homogeneous=True)
        self.set_size_request(150, 150)
        self.widgets = [None] * 9


    def __getitem__(self, key):
        return self.widgets[key]


    def __setitem__(self, key, value):
        widget = self.widgets[key]
        if widget:
            self.table.remove(widget)

        self.attach(value, *Board.COORS[key], xpadding=10, ypadding=10)
        value.show()



class GUI(LineReceiver):


    def build_widgets(self):
        top = gtk.HBox()
        top.set_size_request(0, 32)
        self.button = gtk.ToggleButton("start")
        self.button.set_size_request(50, 0)
        self.label = gtk.Label("status")
        top.pack_start(self.button, expand=False, fill=False)
        top.pack_end(self.label)

        self.board = Board()

        body = gtk.HBox()
        body.pack_start(self.board)

        textview = gtk.TextView()
        textview.set_size_request(100, 150)
        body.pack_end(textview, expand=False)

        self.textbuffer = textview.get_buffer()

        self.container = gtk.VBox()
        self.container.pack_start(top, expand=False, fill=False)
        self.container.pack_end(body)

        self.button.connect('toggled', self.on_toggle)

        window = gtk.Window()    
        window.connect('destroy', lambda obj: reactor.stop())
        window.add(self.container)
        window.show_all()


    def __init__(self, domain="localhost", port=9999):
        self.address = (domain, port)

        self.build_widgets()

        self.deferred = None
        self.status = None
        self.timeout = None

        self.STATUSES = {
            "READY":  ( re.compile(r'^READY$'),           self.on_ready,  ("PING", "PLACE")   ), 
            "PING":   ( re.compile(r'^PING (\d+)$'),      self.on_ping,   ("PLACE", "RESULT") ),
            "PLACE":  ( re.compile(r'^([0-8])$'),         self.on_place,  ("PING", "RESULT")  ),
            "RESULT": ( re.compile(r'^(WIN|LOSE|DRAW)$'), self.on_result, None                ),
            }


    def on_toggle(self, widget):
        if widget.get_active():
            self.connect()
        else:
            self.disconnect()


    def connect(self):
        creator = ClientCreator(reactor, lambda: self)
        self.deferred = creator.connectTCP(*self.address)
        self.deferred.addCallback(self.got_protocol)
        self.deferred.addErrback(self.fail_protocol)


    def disconnect(self):
        if self.deferred:
            self.deferred.cancel()
        elif self.transport:
            self.transport.loseConnection()


    def connectionLost(self, reason):
        self.deferred = None
        self.label.set_text('DISCONNECTED')
        self.status = None
        self.button.set_active(False)
        self.board.set_sensitive(False)
        if self.timeout:
            gobject.source_remove(self.timeout)
            self.timeout = None


    def fail_protocol(self, reason):
        self.deferred = None
        self.label.set_text('CONNECTION ERROR')
        self.button.set_active(False)


    def got_protocol(self, protocol):
        self.deferred = None
        self.label.set_text('CONNECTED')
        self.status = ("READY",)
        self.textbuffer.set_text("")


    def lineReceived(self, line):
        self.textbuffer.insert(self.textbuffer.get_end_iter(), '%s\n'%(line))

        for s in self.status:
            regex, callback, next_status = self.STATUSES[s]
            match = regex.match(line)
            if match:
                callback(*match.groups())
                self.status = next_status
                return

        self.disconnect()


    def sendLine(self, line):
        LineReceiver.sendLine(self, line)
        self.textbuffer.insert(self.textbuffer.get_end_iter(), '%s\n'%(line))


    def on_ready(self):
        for i in range(9):
            button = CrossButton()
            button.connect('clicked', self.on_click, i)
            self.board[i] = button

        self.board.set_sensitive(False)


    def on_ping(self, timeout):
        self.board.set_sensitive(True)
        self.timeout = gobject.timeout_add(1000, self.on_timeout, iter(range(int(timeout), 0, -1)))
        self.sendLine('PONG')
        self.label.set_text("30")


    def on_place(self, place):
        self.board[int(place)] = NoughtShape()


    def on_result(self, result):
        pass


    def on_click(self, widget, i):
        if self.timeout:
            gobject.source_remove(self.timeout)
        self.board.set_sensitive(False)
        self.board[i] = CrossShape()
        self.sendLine("%d"%(i))



    def on_timeout(self, data):
        try:
            self.label.set_text("%d"%(data.next()-1))
            return True
        except StopIteration:
            self.disconnect()
        


def main():
    GUI()
    reactor.run()


if __name__ == '__main__':
    main()


