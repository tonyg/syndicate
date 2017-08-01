If two connections, A and B, exist, and
 - A and B are both in #room1
 - A and B are both in #room2
 - A renames itself to A'
then B receives two ":A NICK A'" messages. This shouldn't happen.

When X quits (disconnects), peers in channels X was in get a PART.
They should get a QUIT.
