-define(REQUEST, 16#80).
-define(RESPONSE, 16#81).

-define(GET, 16#00).
-define(SET, 16#01).
-define(ADD, 16#02).
-define(REPLACE, 16#03).
-define(DELETE, 16#04).
-define(INCREMENT, 16#05).
-define(DECREMENT, 16#06).
-define(QUIT, 16#07).
-define(FLUSH, 16#08).
-define(GETQ, 16#09).
-define(NOOP, 16#10).
-define(VERSION, 16#11).
-define(GETK, 16#12).
-define(GETKQ, 16#13).
-define(APPEND, 16#14).
-define(PREPEND, 16#15).
-define(STAT, 16#16).
-define(SETQ, 16#17).
-define(ADDQ, 16#18).
-define(REPLACEQ, 16#19).
-define(DELETEQ, 16#20).
-define(INCREMENTQ, 16#21).
-define(DECREMENTQ, 16#22).
-define(QUITQ, 16#23).
-define(FLUSHQ, 16#24).
-define(APPENDQ, 16#25).
-define(PREPENDQ, 16#26).

-record(message, {magic,
		  op,
		  data_type=16#00,
		  reserved_status=16#00,
		  opaque=16#00,
		  cas=16#00,
		  flags=16#deadbeef,
		  expires=16#00,
		  extras = <<>>,
		  key = <<>>,
		  value = <<>>}).
