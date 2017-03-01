emq_webhook_plugin
=====

EMQ broker plugin to catch broker hooks through webhook.<br>
[http://emqtt.io](http://emqtt.io)<br>
[http://codersgarage.com](http://codersgarage.com)

Setup
-----
##### In Makefile,

DEPS += emq_webhook_plugin

dep_emq_webhook_plugin = git https://github.com/CodersGarage/emq_webhook_plugin master

##### In relx.config

{emq_webhook_plugin, load}

##### In _rel/emqttd/etc/plugins/emq_webhook_plugin.config
```
[
  {
    emq_webhook_plugin, [
    {"api_url", "http://127.0.0.1/hooks"},
    {"api_key", "123456"}
  ]
  }
].
```

API
----
* client.connected
```json
{
  "api_key": "123456",
  "action": "client_connected",
  "clientId": "Paho12123123123"
}
```

* client.disconnected
```json
{
  "api_key": "123456",
  "action": "client_disconnected",
  "clientId": "Paho12123123123"
}
```

* client.subscribe
```json
{
  "api_key": "123456",
  "action": "client_subscribe",
  "clientId": "Paho12123123123",
  "username": "sakib"
}
```

* client.unsubscribe
```json
{
  "api_key": "123456",
  "action": "client_unsubscribe",
  "clientId": "Paho12123123123",
  "username": "sakib"
}
```

* session.created
```json
{
  "api_key": "123456",
  "action": "session_created",
  "clientId": "Paho12123123123",
  "username": "sakib"
}
```

* session.subscribed
```json
{
  "api_key": "123456",
  "action": "session_subscribed",
  "clientId": "Paho12123123123",
  "username": "sakib",
  "topic": "/demo/topic"
}
```

* session.unsubscribed
```json
{
  "api_key": "123456",
  "action": "session_unsubscribed",
  "clientId": "Paho12123123123",
  "username": "sakib",
  "topic": "/demo/topic"
}
```

* session.terminated
```json
{
  "api_key": "123456",
  "action": "session_terminated",
  "clientId": "Paho12123123123",
  "username": "sakib",
  "reason": "unknown reason"
}
```

* message.publish
```json
{
  "api_key": "123456",
  "action": "message_publish",
  "topic": "/demo/topic",
  "message": "Hello World",
  "from": "Paho12123123123"
}
```

* message.delivered
```json
{
  "api_key": "123456",
  "action": "message_delivered",
  "topic": "/demo/topic",
  "message": "Hello World",
  "from": "Paho12123123123",
  "username": "sakib"
}
```

* message.acknowledged
```json
{
  "api_key": "123456",
  "action": "message_acknowledged",
  "topic": "/demo/topic",
  "message": "Hello World",
  "from": "Paho12123123123",
  "username": "sakib"
}
```

LICENSE
-------
Copyright © Coders Garage Technologies Ltd<br/>
Distributed under [MIT](https://github.com/CodersGarage/emq_webhook_plugin/blob/master/LICENSE) license.

Contributors
------
* [Sakib Sami](https://github.com/s4kibs4mi)
