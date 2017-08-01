When A is a member of #room1, which has B and C in it, and A
disconnects, the session code futilely sends ":B PART #room1" and ":C
PART #room1", even though the connection is now gone.
