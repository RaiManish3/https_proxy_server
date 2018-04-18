## About
This application serves as a proxy web server for http/s methods such as GET, POST, HEAD and CONNECT.
Furthermore Chunk Parsing Transfer Encoding is also implemented.
To serve the request new threads are spawned. I did not find C equivalent of '__select__' for Haskell.
So this uses polling mechanism to check the socket that are ready to recieve or send data.

One can filter out the sites that they don't want the client to access. For now, one needs to specify
the whole host name in the filter list while passing as commandline argument since the code does a
string comparision rather than prefix or suffix matching.
Also one can find statistics related to the number of requests resulted in success, failure and filter.


## Running the program
``` bash
    $: bash startServer.sh [filter list seperated by space]
```

Example:
``` bash
    $: bash startServer.sh www.facebook.com:443 www.google.com:443
```

## Issues
* The application consumes a lot a memory when accessing several sites together. As when cellphones are connected to it, it almost everytime hangs the server.
* Sometimes the pages do not get fully loaded. A refresh is required to load all the contents.
