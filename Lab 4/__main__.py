#!/usr/bin/env python

import sys
from chat_server import *

if __name__ == '__main__':
    port = sys.argv[1]
    server = ChatServer("127.0.0.1", int(port))
    server.start()
