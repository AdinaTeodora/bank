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

To compile:

    $ rebar3 compile

To run:

    $ rebar3 shell





