# Example project to test hapistrano's working_dir feature

We're going to test the hapistrano's `working_dir` feature by deploying this project to a local server (Virtual machine).
To do this we need to:

1. Install [VirtualBox](virtualbox)
2. Install [Vagrant](vagrant)
4. You must have a ssh key with the name `id_rsa`.
    If you're not sure this [article](ssh) can be helfpul.
5. Go to the `/hapistano/example` directory.
6. Execute in your terminal the next line.
    ```bash
    vagrant up
    ```
    If everything went good, this should trigger the deployment procces to the virtual machine.
9. To check that the project was built you can ssh the vagrant vm, and do the folowing:
    ```bash
    vagrant ssh
    cd /tmp/hap-examle/current/example
   stack build
   ```
   Nothing should happen since your project is already compiled.

[virtualbox]: https://www.virtualbox.org/wiki/Downloads
[vagrant]: https://www.vagrantup.com/docs/installation
[ssh]: https://docs.github.com/en/github/authenticating-to-github/checking-for-existing-ssh-keys
