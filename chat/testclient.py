import struct
import socket
import json
import pprint
import sys
from StringIO import StringIO

SERVER = "localhost"
PORT = 8097

PARTICIPANT = "231e39b9-aa11-475c-920d-8f84007137da"
CHATS = ["06d617d1-229b-f770-5bd7-b9e44553ded2"]

sequence = 0

def send(sock, message):
    global sequence
    message["message_id"] = sequence
    sequence += 1
    
    msg = json.dumps(message)
    print "Sending", len(msg), msg
    prefix = struct.pack(">I", len(msg))
    sock.sendall(prefix)
    sock.sendall(msg)


def getBytes(sock, length):
    buffer = StringIO()
    byteCount = 0
        
    while byteCount < length:
        data = sock.recv(length - byteCount)
        buffer.write(data)
        byteCount += len(data)
    return buffer.getvalue()


def loop(sock, once=False):
    while True:
        length = struct.unpack(">I", getBytes(sock, 4))[0]
        print "Message length:", length

        message = json.loads(getBytes(sock, length))
        pprint.pprint(message)
        if once:
            break

    

if __name__ == "__main__":

    sock = socket.create_connection((SERVER, PORT))

    send(sock, {
            "method": "subscribe",
            "participant": PARTICIPANT,
            "chats": CHATS
            })

    send(sock, {
            "method": "amihungry"
            })

    # send(sock, {
    #         "method": "get_hungry"
    #         })

    # send(sock, {
    #         "method": "stop_hungry"
    #         })
    
    # send(sock, {
    #         "method": "create_chat",
    #         "f": "4e14599df9212807fa000000"
    #         })

    # send(sock, {
    #         "method": "join_chat",
    #         "chat": "6bf7ad49-2808-a76b-8722-b3c58559453b"
    #         })

    if len(sys.argv) > 1:
        message = " ".join(sys.argv[1:]).decode("utf-8")

        send(sock, {
                "method": "message",
                "chat": CHATS[0],
                "message": message
                })

        loop(sock, True)
    else:

        loop(sock)
