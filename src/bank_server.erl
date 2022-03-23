%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2022 14:53
%%%-------------------------------------------------------------------
-module(bank_server).
-author("teodoraardeleanu").

-behaviour(gen_server).

%% API
-export([start_link/0, balance/0, transactions/0, charge/1, withdraw/1, deposit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type amount() :: integer().
-type action() :: charge | withdraw | deposit.

%% Default user's initial balance to 100 Bambeuros due to promotion on signup.
-record(account, {
  balance      = 100 :: amount(),
  transactions = []  :: [{action(), amount()}]
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start / Create a bank account for a user.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec balance() -> Balance :: amount().
balance() ->
  gen_server:call(?MODULE, balance).

-spec transactions() -> Transactions :: [{action(), amount()}].
transactions() ->
  io:format("Showing transactions in order of most recent: ~n"),
  gen_server:call(?MODULE, transactions).

-spec charge(amount()) -> Balance :: amount().
charge(Amount) ->
  gen_server:call(?MODULE, {charge, Amount}).

-spec withdraw(amount()) -> Balance :: amount().
withdraw(Amount) ->
  gen_server:call(?MODULE, {withdraw, Amount}).

-spec deposit(amount()) -> Balance :: amount().
deposit(Amount) ->
  gen_server:call(?MODULE, {deposit, Amount}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
  {ok, #account{}}.

%% @private
%% @doc Handling call messages
handle_call(balance, _From, Account = #account{balance = Balance}) ->
  {reply, io:format("Balance is at ~p.~n", [Balance]), Account};
handle_call(transactions, _From, Account = #account{transactions = []}) ->
  {reply, io:format("No transactions made yet.~n"), Account};
handle_call(transactions, _From, Account = #account{transactions = Transactions}) ->
  FormattedTransactions = lists:foreach(fun({Action, Amount}) ->
    io:format("~p -> ~p Bambeuros;~n", [Action, Amount])
  end, Transactions),
  {reply, FormattedTransactions, Account};
handle_call({charge, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({charge, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  case Balance > Amount of
    true  ->
      NewAccount = Account#account{balance = Balance - Amount, transactions = [{charge, Amount} | Transactions]},
      NewBalance = NewAccount#account.balance,
      {reply, io:format("Charged ~p Bambeuros, balance is now at ~p.~n", [Amount, NewBalance]), NewAccount};
    false ->
      {reply, 0, Account}
  end;
handle_call({withdraw, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({withdraw, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  case Balance > Amount of
    true  ->
      NewAccount = Account#account{balance = Balance - Amount, transactions = [{withdraw, Amount} | Transactions]},
      NewBalance = NewAccount#account.balance,
      {reply, io:format("Withdrawn ~p Bambeuros, balance is now at ~p.~n", [Amount, NewBalance]), NewAccount};
    false ->
      {reply, Balance, #account{}}
  end;
handle_call({deposit, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({deposit, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  NewAccount = Account#account{balance = Balance + Amount, transactions = [{deposit, Amount} | Transactions]},
  NewBalance = NewAccount#account.balance,
  {reply, io:format("Deposited ~p Bambeuros, balance is now at ~p.~n", [Amount, NewBalance]), NewAccount};
handle_call(_Request, _From, State = #account{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Request, Account = #account{}) ->
  {noreply, Account}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, Account = #account{}) ->
  {noreply, Account}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State = #account{}) ->
  ok.

%% @private
%% @doc Convert process account when code is changed
code_change(_OldVsn, Account = #account{}, _Extra) ->
  {ok, Account}.
