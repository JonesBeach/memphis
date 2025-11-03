from memphis.net import listen

sock = listen(("127.0.0.1", 8080))
print("Listening...")
conn, _ = sock.accept()
data = conn.recv(1024)
print("Sending...")
conn.send(b"HTTP/1.1 200 OK\r\n\r\nHello from Memphis!")
print("Closing...")
conn.close()
