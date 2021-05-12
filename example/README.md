# Example project to test hapistrano's working_dir feature

We're going to test the hapistrano's `working_dir` feature by deploying this project to a local server (Virtual machine).
To do this we need to:

1. Install [VirtualBox][virtualbox]
2. Install [Vagrant][vagrant]
3. Make sure you have hapistrano installed. You can install it via [stack] by running
    ```bash
    stack install hapistrano
    ```
4. You must have a ssh key with the name `id_rsa`.If you're not sure this [article][ssh] can be helfpul.
5. Go to the `/hapistano/example` directory.
6. Execute in your terminal the next line. `*`
    ```bash
    #This could take a couple of minutes
    $ vagrant up
    $ hap deploy
    ```
    If everything went good, this should trigger the deployment procces to the virtual machine.
7. To check that the project was built you can ssh the vagrant vm, and do the folowing:
    ```bash
    vagrant ssh
    cd /tmp/hap-examle/current/example
   stack build
   ```
   Nothing should happen since your project is already compiled.

`*` A know issue occurs if you have other vagrant vms. When trying to run `hap deploy` you could get the following console result. To avoid this issue remove the line that contains the previous RSA host key and try running `hap deploy` again.

```bash
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
Someone could be eavesdropping on you right now (man-in-the-middle attack)!
It is also possible that the RSA host key has just been changed.
The fingerprint for the RSA key sent by the remote host is
<host key>.
Please contact your system administrator.
Add correct host key in /path/to/.ssh/known_hosts to get rid of this message.
Offending key in /path/to/.ssh/known_hosts:<line>
RSA host key for [ip-or-host]:<port> has changed and you have requested strict checking.
Host key verification failed.
```

[virtualbox]: https://www.virtualbox.org/wiki/Downloads
[vagrant]: https://www.vagrantup.com/docs/installation
[ssh]: https://docs.github.com/en/github/authenticating-to-github/checking-for-existing-ssh-keys
[stack]: https://docs.haskellstack.org/en/stable/README/
