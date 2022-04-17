# Powermail #

An email-based assistant and organiser.

## SystemD configuration ##

To run Powermail as a systemd service, create a unit file similar to the following at /etc/systemd/service/powermail.service:

```bash
[Unit]
Description=Powermail
After=network.target

[Service]
Environment=DOTNET_ENVIRONMENT=systemd
ExecStart=<path to powermail executable>
KillMode=process
Restart=on-failure
RestartPreventExitStatus=255
WorkingDirectory=<path to powermail directory>
RuntimeDirectory=powermail
RuntimeDirectoryMode=0755

[Install]
Wants=network.target
Alias=powermail.service
```

Install the service with:
`systemctl install powermail`

To configure the required secrets, set up an override to modify the environment:
`systemctl edit powermail`

Add the following lines to the systemd override:

```
[Service]
Environment="Mailer__Host=<url of mail host>"
Environment="Mailer__Port=<port (e.g. 465)>"
Environment="Mailer__User=<login for the mail host>"
Environment="Mailer__Password=<password for the mail host>"
```
