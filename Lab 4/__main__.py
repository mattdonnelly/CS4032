#!/usr/bin/env python

import sys
from chat_server import *

if __name__ == '__main__':
    port = sys.argv[1]
    server = ChatServer()
    server.start()
