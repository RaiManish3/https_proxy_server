module ParserTest where

-- TODO :: convert this project into a package
import HttpParser

xxx = scanner "GET /hello.htm HTTP/1.1\r\nUser-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)\r\nHost: www.tutorialspoint.com\r\nAccept-Language: en-us\r\nAccept-Encoding: gzip, deflate\r\nConnection:Close\r\nContent-Length:4\r\n\r\n" (emptyHttpParserInfo HttpRequestParser) emptyHttpPacket

yyy = scanner "POST /cgi-bin/process.cgi HTTP/1.1\r\nUser-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)\r\nHost: www.tutoialspoint.com\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 7\r\nAccept-Language: en-us\r\nAccept-Encoding: gzip, deflate\r\nConnection: Keep-Alive\r\n\r\nhello\r\n" (emptyHttpParserInfo HttpRequestParser) emptyHttpPacket

zzz = scanner "HTTP/1.1 200 OK Darling\r\nDate: Mon, 27 Jul 2009 12:28:53 GMT\r\nServer: Apache/2.2.14 (Win32)\r\nLast-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r\nContent-Length: 48\r\nContent-Type: text/html\r\nConnection: Closed\r\n\r\n<html><body><h1>Hello, World!</h1></body></html>" (emptyHttpParserInfo HttpResponseParser) emptyHttpPacket

qqq = scanner "GET /hello.htm HTTP/1.1\r\n" (emptyHttpParserInfo HttpRequestParser) emptyHttpPacket
