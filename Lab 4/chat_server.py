#!/usr/bin/env python

import platform
import random
import socket
import sys
import thread


class User:
    def __init__(self, nick, conn):
        self.nick = nick
        self.conn = conn


class Channel:
    def __init__(self, id, name, users):
        self.id = id
        self.name = name
        self.users = users


class ChatServer:
    def __init__(self, port=8000):
        self.host = "127.0.0.1"
        self.port = port
        self.channels = {}

    def start(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind((self.host, self.port))
        self.socket.listen(10)

        print "Started listening on port {0}...".format(self.port)

        self.accept_connections()

    def accept_connections(self):
        while 1:
            (conn, address) = self.socket.accept()
            thread.start_new_thread(self.proccess_requests, (conn,))

    def proccess_requests(self, conn):
        try:
            for message in self.read_messages(conn):
                words = message.split()

                if words[0] == "JOIN_CHATROOM:":
                    self.handleJoin(conn, words)
        except (KeyboardInterrupt, SystemExit):
            print "Closing connection..."
            conn.close()

    def read_messages(self, conn, recv_buffer=4096, delimiter='\r\n\r\n'):
        buffer = ''
        data = True
        while data:
            data = conn.recv(recv_buffer)
            buffer += data

            while buffer.find(delimiter) != -1:
                msg, buffer = buffer.split(delimiter, 1)
                yield msg

    def handleJoin(self, conn, words):
        channel_name = words[1]
        user_name = words[7]

        print channel_name
        print user_name

        user = User(user_name, conn)

        channel = None
        channel_id = 0

        if channel_name in self.channels:
            channel = self.channels[channel_name]
            channel_id = channel.id

            if user_name not in channel.users:
                channel.users[user_name] = user
        else:
            channel_id = random.randrange(2147483647)
            channel = Channel(channel_id, channel_name, [user])

        self.channels[channel_name] = channel

        response = "JOINED_CHATROOM: {0}\n"\
                   "SERVER_IP: {1}\n"\
                   "PORT: {2}\n"\
                   "ROOM_REF: {3}\n"\
                   "JOIN_ID: {4}\n"

        response = response.format(channel_name,
                                   platform.node(),
                                   self.port,
                                   channel_id,
                                   random.randrange(2147483647))

        conn.sendall(response)
