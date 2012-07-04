-record(state, {name,
	children = [],
	config = [],
	users = []}).

-record(msg, {from,
	body}).

-record(privmsg, {from,
	to,
	body}).
