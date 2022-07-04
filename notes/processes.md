# Processes
New Kitsu processes can be spawned from a `.kit` file, which defines methods for interacting with the process.
Processes spawned this way can be utilized and killed by other processes with the necessary permissions.
Furthermore, processes can be spawned and interacted with over a newtwork by exposing a `.kit` file on a port.
This creates a simple system for managing a safe swarm of processes that can span multiple machines.