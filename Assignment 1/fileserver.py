#!/usr/bin/env python

import select
import socket
import sys
import thread


class FileServer:
    def __init__(self, host="127.0.0.1", port=8000):
        self.host = host
        self.port = port
        self.channels = {}
        self.users = {}
        self.server_sockets = []

    def start(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        try:
            self.socket.bind((self.host, self.port))
        except socket.error as e:
            print "Could not bind port %s: %s." % (self.port, e)
            sys.exit(1)
        self.socket.listen(5)

        print "Started server on port {0}:{1}...".format(self.host, self.port)

        while True:
            try:
                reading_sockets, _, _ = select.select(self.server_sockets
                                                      + [self.socket], [], [])
                for i, sock in enumerate(reading_sockets):
                    if sock is self.socket:
                        self.accept_connections()
                    else:
                        thread.start_new_thread(self.proccess_requests,
                                                (self.users[i],))
            except (KeyboardInterrupt, SystemExit, select.error):
                break

        self.socket.close()

    def accept_connections(self):
        (sock, _) = self.socket.accept()
        self.server_sockets.append(sock)

        thread.start_new_thread(self.proccess_requests, (new_user,))

    def recv_all(self, sock):
        total = ''
        try:
            data = sock.recv(2048)
            total += data
        except socket.error, e:
            print "Socket error: {0}".format(e)

        return total

    def proccess_requests(self, user):
        data = self.recv_all(user.socket)

if __name__ == '__main__':
    port = sys.argv[1]
    server = FileServer("127.0.0.1", int(port))
    server.start()
