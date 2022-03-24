%%%-------------------------------------------------------------------
%%% @author teodoraardeleanu
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% A bank server that allows user account creation and user actions such as:
%%% - view their balance
%%% - view their transactions
%%% - pay/get charged
%%% - withdraw money
%%% - deposit
%%% - transfer to other users

%%% @end
%%% Created : 23. Mar 2022 14:53
%%%-------------------------------------------------------------------
-module(bank_server).
-author("teodoraardeleanu").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create_account/0, show_balance/1, show_transactions/1, transfer/3, pay/2, withdraw/2, deposit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type amount() :: integer().
-type action() :: charge | withdraw | deposit | transfer | received.

%% Default user's initial balance to 100 Bambeuros due to promotion on signup.
-record(account, {
  balance      = 100 :: amount(),
  transactions = []  :: [{action(), amount()}]
}).

%%%===================================================================
%%% External API functions
%%%===================================================================

%% @private
%% @doc Starts up the bank process.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Creates a user account by starting up a process.
create_account() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  io:format("Congratulations, you have received 100 free Bambeuros for signing up with us!~n"),
  Pid.

%% @private
%% @doc Shows the user's balance.
show_balance(UserPid) ->
  Balance = gen_server:call(UserPid, balance),
  io:format("Balance stands at ~p.~n", [Balance]).

%% @private
%% @doc Shows the user's list of transactions.
show_transactions(UserPid) ->
  Transactions = gen_server:call(UserPid, transactions),
  case Transactions of
    empty ->
      io:format("No transactions were made. ~n");
    Transactions1 ->
      io:format("Showing transactions in order of most recent: ~n"),
      lists:foreach(fun({Action, Amount}) -> io:format("~p -> ~p Bambeuros;~n", [Action, Amount]) end, Transactions1)
  end.

%% @private
%% @doc Transfers money to a different user.
transfer(User, User1, Amount) ->
  {NewAmount, NewBalance} = gen_server:call(User, {transfer, User1, Amount}),
  case NewAmount of
    0 ->
      io:format("Transaction denied! Not enough funds in the bank account. Balance stands at ~p~n", [NewBalance]);
    PosAmount ->
      io:format("Transferred ~p Bambeuros to User: ~p. Balance now stands at ~p.~n", [PosAmount, User1, NewBalance])
  end.

%% @private
%% @doc Makes a payment out of the user's bank account.
pay(User, Amount) ->
  {NewAmount, NewBalance} = gen_server:call(User, {pay, Amount}),
  case NewAmount of
    0 ->
      io:format("Transaction denied! Not enough funds in the bank account. Balance stands at ~p~n", [NewBalance]);
    PosAmount ->
      io:format("Charged ~p Bambeuros. Balance now stands at ~p.~n", [PosAmount, NewBalance])
  end.

%% @private
%% @doc Withdraws moeny out of the user's bank account.
withdraw(User, Amount) ->
  {NewAmount, NewBalance} = gen_server:call(User, {withdraw, Amount}),
  case NewAmount of
    0 ->
      io:format("Transaction denied! Not enough funds in the bank account. Balance stands at ~p~n", [NewBalance]);
    PosAmount ->
      io:format("Withdrew ~p Bambeuros. Balance now stands at ~p.~n", [PosAmount, NewBalance])
  end.

%% @private
%% @doc Makes a deposit to the user's bank account.
deposit(User, Amount) ->
  {NewAmount, NewBalance} = gen_server:call(User, {deposit, Amount}),
  case NewAmount of
    0 ->
      io:format("Transaction denied! Not enough funds in the bank account. Balance stands at ~p~n", [NewBalance]);
    PosAmount ->
      io:format("Deposited ~p Bambeuros. Balance now stands at ~p.~n", [PosAmount, NewBalance])
  end.

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
  {reply, Balance, Account};

handle_call(transactions, _From, Account = #account{transactions = []}) ->
  {reply, empty, Account};
handle_call(transactions, _From, Account = #account{transactions = Transactions}) ->
  {reply, Transactions, Account};

handle_call({transfer, OtherUser, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  case Balance >= Amount of
    true ->
      NewAccount = Account#account{balance = Balance - Amount, transactions = [{transfer, Amount} | Transactions]},
      NewBalance = NewAccount#account.balance,
      received(OtherUser, Amount),
      {reply, {Amount, NewBalance}, NewAccount};
    false ->
      {reply, {0, Balance}, Account}
  end;

handle_call({pay, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({pay, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  case Balance >= Amount of
    true  ->
      NewAccount = Account#account{balance = Balance - Amount, transactions = [{pay, Amount} | Transactions]},
      NewBalance = NewAccount#account.balance,
      {reply, {Amount, NewBalance}, NewAccount};
    false ->
      {reply, {0, Balance}, Account}
  end;

handle_call({withdraw, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({withdraw, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  case Balance >= Amount of
    true  ->
      NewAccount = Account#account{balance = Balance - Amount, transactions = [{withdraw, Amount} | Transactions]},
      NewBalance = NewAccount#account.balance,
      {reply, {Amount, NewBalance}, NewAccount};
    false ->
      {reply, {Amount, Balance}, #account{}}
  end;

handle_call({deposit, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({deposit, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  NewAccount = Account#account{balance = Balance + Amount, transactions = [{deposit, Amount} | Transactions]},
  NewBalance = NewAccount#account.balance,
  {reply, {Amount, NewBalance}, NewAccount};

handle_call({received, Amount}, _From, Account) when Amount =< 0 ->
  {reply, 0, Account};
handle_call({received, Amount}, _From, Account = #account{balance = Balance, transactions = Transactions}) ->
  NewAccount = Account#account{balance = Balance + Amount, transactions = [{received, Amount} | Transactions]},
  NewBalance = NewAccount#account.balance,
  {reply, {Amount, NewBalance}, NewAccount};

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Receives money from another user.
received(User, Amount) ->
  {NewAmount, NewBalance} = gen_server:call(User, {received, Amount}),
  io:format("User ~p Received ~p Bambeuros. Balance now stands at ~p.~n", [User, NewAmount, NewBalance]).
