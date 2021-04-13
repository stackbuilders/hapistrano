# How to test working_dir feature in a remote server (Virtual Machine)

We're going to test the working_dir hapistrano feature by deploying this project
to a local server (Virtual machine). To do this we need to:
We're assuming that you have this repo locally.

1. Install virtualbox - link here
2. Install vagrant - link here
3. create a vagrant vm
4. copy our ssh keys to vagrant
5. ssh the vagrant vm
6. install stack - link here
7. go to /hapistrano/example and run `hap deploy`
8. That should be.
9. go to the terminal you have the vagrant vm and go to /tmp/hap-examle/current/example
    and try running `stack build` nothing should happen since your project is already compiled.
