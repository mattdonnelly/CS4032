#!/usr/bin/env python

import sys
from chatserver import *

if __name__ == '__main__':
    port = sys.argv[1]
    server = ChatServer(int(port))
    server.start()
