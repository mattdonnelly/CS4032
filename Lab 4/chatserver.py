#!/usr/bin/env python

import socket
import sys
import thread


class ChatServer:
    def __init__(self, port=8000):
        self.host = "127.0.0.1"
        self.port = port

    def start(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind((self.host, self.port))
        self.socket.listen(10)

        print "Started listening on port {0}...".format(self.port)

        self.accept_connections()

    def accept_connections(self):
        try:
            while 1:
                (conn, address) = self.socket.accept()
                thread.start_new_thread(self.proccess_requests, (conn,))

        except (KeyboardInterrupt, SystemExit):
            print "Closing connection..."
            conn.close()

    def proccess_requests(self, conn):
        for message in self.read_messages(conn):
            print message
            conn.sendall(message)

    def read_messages(self, conn, recv_buffer=4096, delimiter='\r\n\r\n'):
        buffer = ''
        data = True
        while data:
            data = conn.recv(recv_buffer)
            buffer += data

            while buffer.find(delimiter) != -1:
                msg, buffer = buffer.split(delimiter, 1)
                yield msg
