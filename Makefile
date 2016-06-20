all:
	./rebar get-deps compile escriptize

clean:
	rm -rf deps ebin user_time_latency