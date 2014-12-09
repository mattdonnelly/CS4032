#!/usr/bin/env python

import select
import socket
import sys
import thread


class User:
    def __init__(self, sock, nick, id):
        self.socket = sock
        self.nick = nick
        self.id = id


class Channel:
    def __init__(self, id, name, users):
        self.id = id
        self.name = name
        self.users = []


class ChatServer:
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

    def recv_all(self, sock):
        total = ''
        try:
            data = sock.recv(2048)
            total += data
        except socket.error, e:
            print "Socket error: {0}".format(e)

        return total

    def accept_connections(self):
        (sock, _) = self.socket.accept()
        user_id = len(self.users)
        new_user = User(sock, "", user_id)
        self.users[user_id] = new_user
        self.server_sockets.append(sock)

        print "New client joined with id: {0}".format(user_id)

        thread.start_new_thread(self.proccess_requests, (new_user,))

    def proccess_requests(self, user):
        data = self.recv_all(user.socket)

        words = data.splitlines()
        words = map(lambda s: s.split(), words)

        if len(words) > 0:
            if words[0][0] == "JOIN_CHATROOM:":
                user.nick = words[3][1]
                self.handle_join(user, words[0][1])
            if words[0][0] == "LEAVE_CHATROOM:":
                self.hande_leave(user, words[0][1])

    def handle_join(self, user, channel_name):
        channel = None
        channel_id = None

        if channel_name in self.channels:
            channel = self.channels[channel_name]
            channel_id = channel.id

            if user not in channel.users:
                channel.users.append(user)
        else:
            channel_id = len(self.channels)
            channel = Channel(channel_id, channel_name, [user])

        self.channels[channel_name] = channel

        response = "JOINED_CHATROOM: {0}\n"\
                   "SERVER_IP: {1}\n"\
                   "PORT: {2}\n"\
                   "ROOM_REF: {3}\n"\
                   "JOIN_ID: {4}\n"

        response = response.format(channel_name,
                                   socket.getfqdn(),
                                   self.port,
                                   channel_id,
                                   len(channel.users))

        user.socket.sendall(response)

    def handle_leave(self, user, channel_name):
        pass
