If two connections, A and B, exist, and
 - A and B are both in #room1
 - A and B are both in #room2
 - A renames itself to A'
then B receives two ":A NICK A'" messages. This shouldn't happen.

A similar problem occurs for ":B QUIT" messages when A is in two
channels that B is also in, and B disconnects.

When A is a member of #room1, which has B and C in it, and A
disconnects, the session code futilely sends ":B PART #room1" and ":C
PART #room1", even though the connection is now gone.
