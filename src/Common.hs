module Common where

badRequestPacket = "HTTP/1.1 400 Bad Request\r\n\
                   \Proxy-agent: proxyServer\r\n\
                   \Content-length: 11\r\n\
                   \Connection: close\r\n\
                   \Content-Type: text/html; charset=iso-8859-1\r\n\
                   \\r\n\
                   \Bad Request"


methodNotAllowed = "HTTP/1.1 405 Method Not Allowed\r\n\
                   \Proxy-agent: proxyServer\r\n\
                   \Content-length: 18\r\n\
                   \Connection: close\r\n\
                   \Content-Type: text/html; charset=iso-8859-1\r\n\
                   \\r\n\
                   \Method Not Allowed"


forbiddenPacket = "HTTP/1.1 403 Forbidden\r\n\
                   \Proxy-agent: proxyServer\r\n\
                   \Content-length: 9\r\n\
                   \Connection: close\r\n\
                   \Content-Type: text/html; charset=iso-8859-1\r\n\
                   \\r\n\
                   \Forbidden"

badGatewayPacket = "HTTP/1.1 400 Bad Gateway\r\n\
                   \Proxy-agent: proxyServer\r\n\
                   \Content-length: 11\r\n\
                   \Connection: close\r\n\
                   \Content-Type: text/html; charset=iso-8859-1\r\n\
                   \\r\n\
                   \Bad Gateway"

connectEstKPacket = "HTTP/1.1 200 Connection established\r\nProxy-agent:proxyServer.hs\r\n\r\n"

allowedMethods = ["GET", "POST", "HEAD", "CONNECT"]
allowedVersion = ["HTTP/1.1","HTTP/1.0"]

blockedDomain = []
