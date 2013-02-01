-module(tokenizer).

-export([start/0, runPerfTest/1, testUrlValidityAndAge/0]).

start() ->
	runPerfTest(100).
	
%%% A configurable age for how long a token is valid
getAge() -> 300.

getFlooredTime(Age) ->
	nowSeconds() - (nowSeconds() rem Age).

getKey() ->
	"SecretKeyKnownToUs".

nowSeconds() ->
	calendar:datetime_to_gregorian_seconds({date(), time()}).

%%% Token takes a time floored to the current "age"
%%% appends the Url to it, and then checksums it.
%%% This ensures each URL is "signed" to a certain age
%%% and outside of that age, it goes invalid.
getUrlToken(Url) ->
	crypto:rc4_encrypt(getKey(), hash(string:join([Url, integer_to_list(getFlooredTime(getAge()))], ""))).

validateUrl(Url, Token) ->
	ExpectedHash = crypto:rc4_encrypt(getKey(), Token),
	hash(string:join([Url, integer_to_list(getFlooredTime(getAge()))], "")) == ExpectedHash.

hash(Data) ->
       crypto:sha(Data).


randomHttpScheme() ->
	case random:uniform() > 0.5 of
		true -> "https";
		false -> "http"
	end.

randomDomain() ->
         case random:uniform(10) of
		1 -> "www.google.com";
		2 -> "www.amazon.com";
		3 -> "www.microsoft.com";
		4 -> "www.erlang.org";
		5 -> "www.bing.com";
		6 -> "www.xbox.com";
		7 -> "www.apple.com";
		8 -> "www.veritas.com";
		9 -> "www.netscape.com";
		10 -> "www.gnu.org"
	 end.

randomPath() ->
         case random:uniform(10) of
		1 -> "/";
		2 -> "/foo";
		3 -> "/foo/bar";
		4 -> "/foobar";
		5 -> "/search/images/";
		6 -> "/games/video/";
		7 -> "/devices/iOS/iPad/mini";
		8 -> "/regexmatcher";
		9 -> "/gnu/is/not/unix";
		10 -> "/call/it/gnulinux/not/linux"
	 end.

randomQueryString() ->
         case random:uniform(10) of
		1 -> "?";
		2 -> "?productid=201";
		3 -> "?productname=hello+world&productdomain=toys";
		4 -> "?stupidparameter=stupid";
		5 -> "?param1=1&param2=2&param3=3&param4=4";
		6 -> "?q=s";
		7 -> "?random=parameters&fun=todo";
		8 -> "?nothing=to+see+here";
		9 -> "?search=harry+potter";
		10 -> "?linuxvariant=ubuntu"
	 end.


randomUrl() ->
	string:join([randomHttpScheme(), "://", randomDomain(), randomPath(), randomQueryString()], "").

simulateValidationRequest(_Url, _Token, 0)  ->
	ok;
simulateValidationRequest(Url, Token, Attempts) ->
	%%%% Sleep an arbitrary amount of time up the age of the token
	timer:sleep(random:uniform(getAge())),
	case validateUrl(Url, Token) of
		%%% io:fwrite("Validation succeeded. Procceeding to attempt another.~n", []),
		true ->  simulateValidationRequest(Url, Token, Attempts-1);
		false -> ok
	end.

testUrlValidityAndAge() ->
	%% randomize the seed so every url really ends up being unique.  Otherwise the calls to 
	%% random will repeat on a predicatable pattern per process.
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	Url = randomUrl(),
	Token = getUrlToken(Url),
	%%io:fwrite("New url spawned ~s!~n", [Url]),
	%%% Simulate a few validation requests at abrupt time intervals
	simulateValidationRequest(Url, Token, random:uniform(1000)).

%%%% Create and validate a few hundred thousand URLs
runPerfTest(0) ->
	io:fwrite("Jobs spawned.~n", []);
runPerfTest(N) when N >= 1 ->
	spawn(tokenizer, testUrlValidityAndAge, []),
	runPerfTest(N-1).


	
