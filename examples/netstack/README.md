# TCP/IP Stack

There are two (closely-related) implementations here:

 - [`monolithic-lowlevel`](monolithic-lowlevel/) is the original
   implementation, originally written for `minimart`, a language that
   followed our ESOP 2014 paper quite closely. Porting it to a
   monolithic-assertion-set Syndicate dialect helped substantially
   simplify the code.

 - [`incremental-highlevel`](incremental-highlevel/) is a port of
   `monolithic-lowlevel` to the Syndicate high-level DSL
   ("`syndicate/actor`"). Moving from the low-level Syndicate style to
   the high-level style also drastically simplified the code.

## Linux Firewall Configuration

Imagine a setup where the machine you are running this code has IP
192.168.1.10. This code claims 192.168.1.222 for itself. Now, pinging
192.168.1.222 from some other machine, say 192.168.1.99, will cause
the local kernel to receive the pings and then *forward them on to
192.168.1.222*, which because of the gratuitous ARP announcement, it
knows to be on its own Ethernet MAC address. This causes the ping
requests to repeat endlessly, each time with one lower TTL.

One approach to solving the problem is to prevent the kernel from
forwarding packets addressed to 192.168.1.222. To do this,

    sudo iptables -I FORWARD -d 192.168.1.222 -j DROP
