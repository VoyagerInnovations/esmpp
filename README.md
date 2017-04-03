# esmpp - Erlang/OTP SMPP Client

[![Build Status](https://travis-ci.org/VoyagerInnovations/esmpp.svg?branch=master)](https://travis-ci.org/VoyagerInnovations/esmpp) [![Hex.pm](https://img.shields.io/hexpm/v/esmpp.svg)](https://hex.pm/packages/esmpp)

**esmpp** connects erlang applications to SMSCs via the SMPP v3.4 protocol.

## Supported Commands

* bind\_transmitter
* bind\_receiver
* bind\_transceiver
* submit\_sm
* deliver\_sm
* enquire\_link

Other commands will be supported in the future versions.

## OTP Version

**Required**: OTP 18 and later

## Setup

**esmpp** can be added as a dependency via [hex.pm](https://hex.pm/packages/esmpp)

```erlang
{deps, [
  {esmpp, "0.0.6"}
]}. 
```

Then include **esmpp** in your application's `.app.src` file

```erlang
{applications, [
  kernel,
  stdlib,
  esmpp
]}.
```

## Usage

Simply call `esmpp:start_link/1` with the following options as map:

* `name` - If provided, the internal `gen_server` will be registered with this name. see `gen_server:start_link/4`
* `mode` - Type of SMPP binding for the client. `transmitter`, `transceiver`, or `receiver`
* `host` - Hostname or IP address of the SMPP server
* `port` - Port of the SMPP server
* `system_id` - Identifier for the SMPP client
* `password` - Password used to authenticate with the `system_id`
* `system_type` - Identifier for the type of SMPP Client
* `callback_mo` - Module and function tuple to be executed when a mobile originating message has been received
* `callback_dr` - Module and function tuple to be executed when a delivery receipt has been received

```erlang
{ok, C} = esmpp:start_link(#{
   mode => transceiver,
   host => "127.0.0.1",
   port => 2775,
   system_id => "smppclient",
   password => "password",
   system_type => "Test",
   callback_dr => {mymodule, myfunction}
}).
```

### Sending SMS

SMS messages are sent by calling `esmpp:send_sms/4`, `esmpp:send_sms/5`, or `esmpp:send_sms/6`. The first four parameters are as follows:

* `Connection` - Process identifier of `esmpp`'s `gen_server` returned by `esmpp:start_link/1`
* `Sender` - Number or mask to be set as sender of the message
* `Destination` - MSISDN to receive the message
* `Message` - UTF-8 encoded message

The last parameter in `esmpp:send_sms/5` is an optional map with the following possible keys:

* `service_type` - Indicates the SMS application service associated with the message
* `protocol_id` - Network specific protocol identifier
* `priority_flag` - Designates the priority level of the message. `0` is the lowest, while `3` is the highest
* `delivery_time` - Either the absolute date and time or relative time from the current SMSC time which the delivery of the message is to be first attempted
* `validity_period` - SMSC expiration time that indicates when the message should be discarded when the message is not delivered
* `reg_delivery` - Indicates if a delivery receipt will be requested from the SMSC.
    * 0 - No delivery receipt requested
    * 1 - Delivery receipt requested (success or failure)
    * 2 - Delivery receipt requested (failure only)
* `replace_flag` - Used to request the SMSC to replace a previously submitted message, that is pending delivery. The SMSC will replace the existing message if the source address, destination address, and service type match the same fields in the new message

The last parameter in `esmpp:send_sms/6` is another optional map of the optional parameters to be passed in the last part of the `submit_sm` packet. Complete information is at the generated documentation inside `doc/`

```erlang
Ids = esmpp:send_sms(C, <<"12345">>, <<"639473371390">>, <<"Hello">>).
```

#### Return Type

All functions return a list of message id tuples:

```erlang
[{message_id, MessageId}]
```

`MessageId` is a (binary) string that was associated to the submitted message in the SMPP server.

#### UCS2/UTF-16 Support

Messages that are outside the standard GSM 03.38 character set are automatically detected and encoded with UTF-16. This includes emojis.

#### Long Messages

Long messages are automatically concatenated when they exceed the standard 140 byte limit. 

## TODOs

* Create common tests
* Implement remaining commands
