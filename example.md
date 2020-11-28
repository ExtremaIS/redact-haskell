# GitHub

This example shows one way to document the login credentials of a GitHub
account, with inline secrets as well as fenced secrets.

## example

* Username: example
* <https://github.com/example>
* Email: <example@example.com>

* <https://github.com/login>
* Password: `oWu5;@u#2^fuK59'3#pXS:9xWgPe$<Hi`

* SSH Identifier: github.com-example-20201124
* SSH Password: `i&2IenM2a>#GL)*;@[LnD4<AU)WSj/?R`

SSH public key (id_ed25519.pub):

~~~
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINpToPygZliXcQ/9t5Q2qTr1qBQf+HpQyRPSVdYdFYBC github.com-example-20201124
~~~

SSH private key (id_ed25519):

```
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAACmFlczI1Ni1jdHIAAAAGYmNyeXB0AAAAGAAAABBjip9e+4
0TwyaK+PFQL139AAAAEAAAAAEAAAAzAAAAC3NzaC1lZDI1NTE5AAAAINpToPygZliXcQ/9
t5Q2kTr1qBQf+HpQyRPSVdYdFYBCAAAAoNlAn70rG4l01yr+HGc5Tc0FMza6Krb68PI6XG
sH5+VZs+ovgUwnkL97SxkbgLmd88Jteb8acovK6eYgxvUalNxT+lenT1ozmyrS3OHv8T3K
EX9NtcuXPjkHWpRhi9zyLicEQclxchBWF2H8Z70VD60mqghvX3I1w/o8oz/emQw3pOa0aj
OfUzqWjcw9rZHatiBpIeHXsesdYyIiVYIeS4Q=
-----END OPENSSH PRIVATE KEY-----
```

SSH configuration:

~~~
Host github.com
  Hostname=github.com
  IdentityFile=~/.ssh/github.com-example-20201124/id_ed25519

Host gist.github.com
  Hostname=gist.github.com
  IdentityFile=~/.ssh/github.com-example-20201124/id_ed25519
~~~
