# maildir_commander
maildir commander is a wrapper arond maildir-utils to make it more versatile. `mu server` is already state-less; however, it cannot be used by more than one client at a time. maildir_commander runs one instance of `mu server` in the background and make it an always-on daemon, so mu4e, command-line client and future web client can share the same underlying `mu server` instance through maildir_commander.

## dependency

* [mu](https://www.djcbsoftware.nl/code/mu/) 1.4+
* erlang OTP 21+
* socat, which is used to re-export the `mu server` compatible interface
* inotify-tools, for watching the Maildir for new mails

## the server

maildir_commander can be used as a dependancy of a larger application or a stand-alone application. In the stand-aline mode, just run the release. It will run `mu server` in the background and communicate with it through a port. All functionalities of `mu server` are exported as erlang APIs via the `mc_mu_api` module.

The `mu server` will be ran on demand. If theere is nothing to do for some time (60 seconds by default) the `mu server` will be shut dowm to save memory. The on/off of `mu server` is automatic and user does not need to care about it except some small startup latency every once in a while. 

## the local socket

maildir_commander will also listen on a Unix domain socket `$HOME/.mc_server_sock`. The socket once connected, behave exactly like `mu server`. All commands are passed through to the shared `mu server` instance, with the only exception of the `cmd:quit` command, which is locally handled. so:

```
$ socat UNIX-CONNECT:$HOME/.mc_server_sock -
```

behave exactly like `mu server`, except now you can run multiple instances. I have a shell script in `scripts/mc` that can be a drop in repleacement of mu (now only `mc server` is supported, in the future I plan to re-implement more mu client side sub-commands). If you run maildir_commander as a daemon, `mu` will not work. To continue use `mu4e`, you would have to configure `mu4e` to use `mc` as the alternative `mu` binary.

## the watcher

`maildir_commander` also has a watcher that watchs the Maildir for new mails and add to the mu database automatically. Given that `maildir_commander` always do a full index on startup, there is little reason to run indexing yourself anymore.

The watcher will also move new mails to the cur dir as per [maildir standard](https://cr.yp.to/proto/maildir.html). Therefore, you must ensure that the IMAP server is not getting in its way. If you are using exim, please make sure the local delivery is done via `maildir_home`, not with a IMAP specific LDA such as `dovecot_delivery`. Also please turn off the auto movement of new mails in your IMAP server. In dovecot, it is controlled in `/etc/dovecot/conf.d/10-mail.conf` with a line as:

```
maildir_empty_new = no
```

## the archiver

`maildir_commander` contains an email archiver that scan your inbox and archive emailto `.Archive` maildir, based on the following rules:

* Mails are grouped into conversations first. Conversation that have emails within 30 days will be left alone. For all other conversations:
* If a conversation contains emails from me (as defined by the `my_addresses` configuration key), archive the whole conversation, will all attachments stripped off to save diskspace
* If the conversation has nothing to do with me, delete the whole conversation

Mails archived will still be searchable, just not in the inbox anymore. Right now the rules are not tunable, or rather, it is tuned to suit my need. My goal is to keep my inbox at constant size with minimal manual intervention. 

## Future works

maildir_commander is meant to be the lower layer of more sofisticated MUAs such as a web client, an IMAP client, etc, so they can run along side with the command line client and `mu4e`. In most cases you want to install [LIV](https://github.com/derek-zhou/liv), which is a full webmail client based on Phoenix LiveView. 
