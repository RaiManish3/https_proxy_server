## About
This application serves as a proxy web server for http/s methods such as GET, POST, HEAD and CONNECT.
Furthermore Chunk Parsing Transfer Encoding is also implemented.
To serve the request new threads are spawned. I did not find C equivalent of '__select__' for Haskell suitable to the project.
So this uses polling mechanism to check the socket that are ready to recieve or send data.

One can filter out the sites that they don't want the client to access. For now, one needs to specify
the whole host name in the filter list while passing as commandline argument since the code does a
string comparision rather than prefix or suffix matching.
Also one can find statistics related to the number of requests resulted in success, failure and filter.

## Implementation Details
[ This ] (proposal/cs653_project.pdf) describes the modules used in this project.

## Usage
``` bash
    $: bash startServer.sh [filter list seperated by space]
```

Example:
``` bash
    $: bash startServer.sh www.facebook.com:443 www.google.com:443
```

Closing the program:
```bash
    $: kill SIGUSR2 [pid]           # SIGINT is disabled.
```

Checking the statistics:
```bash
    $: kill SIGUSR1 [pid]
```
<br>

One can check the memory this application is using by plotting the graph thorugh the following command.
__NOTE__: It requires certain python libraries such as psrecord.
```bash
    $: bash checkStats.sh [plot_number]
```

## Issues
* __Graceful exit__ : It is not always possible to gracefully exit the program as some thread might be reading from a socket.
* __Starvation__ : If too many clients are connected, this would create a lot of threads leading to two major problems.
  First, some threads might get scheduled much later. Second, memory consumption of the program will rise.
