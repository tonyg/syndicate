Ideas on TCP unit testing:
<https://www.snellman.net/blog/archive/2015-07-09-unit-testing-a-tcp-stack/>

Check behaviour around TCP zero-window probing. Is the correct
behaviour already a consequence of the way `send-outbound` works?

Do something smarter with TCP timers and RTT estimation than the
nothing that's already being done.

TCP options negotiation.
 - SACK
 - Window scaling
