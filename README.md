Bambank
=====

An online bank with its own currency. The bank will give new customers 100 free Bambeuros on signup.
This application is written in Erlang OTP.


### Features
* create a user's account
* view a user's transactions and balance
* send Bambeuros to other users

Build
-----
In order to run the application, rebar3 will need to be downloaded and installed.
Download the latest version here: http://www.rebar3.org/.

    curl -O https://s3.amazonaws.com/rebar3/rebar3

Use chmod to make it executable, then add it to your environment variable PATH:
    
    chmod +x rebar3
    export PATH=$PATH:your-current-directory

To clean:

    $ rebar3 clean

To compile:

    $ rebar3 compile

To open shell so we can run the below instructions:

    $ rebar3 shell

### Instructions

Create a user account:

    User = bank_server:create_account().

Show the user's balance / transactions:

    bank_server:show_balance(User).
    bank_server:show_transactions(User).

Make a payment out of their bank account:

    bank_server:pay(User, 21).

Withdraw cash from their bank account:

    bank_server:withdraw(User, 5).

Deposit money into their bank account:

    bank_server:deposit(User, 135).

Create another account to transfer money to:

    User1 = bank_server:create_account().
    bank_server:transfer(User, User1, 30). 

Check final balance of users and list of transactions:
    
    bank_server:show_balance(User).
    bank_server:show_transactions(User).
    
    bank_server:show_balance(User1).
    bank_server:show_transactions(User1).

# Future improvements
* unit tests
* Add a unique username or userid for each created user
* validate inputs (check the input passed in the functions are the correct type)
* use lager library for print statements instead of io:format