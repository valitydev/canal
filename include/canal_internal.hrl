-include("canal_defaults.hrl").


-define(CONTENT_TYPE_JSON, "application/json").


% macros

-define(APP, canal).
-define(GET_ENV(Key), application:get_env(?APP, Key, undefined)).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(GET_OPT(Atom), canal_utils:getopt(Atom)).
-define(ENCODE(Thing), jsone:encode(Thing)).
-define(DECODE(Thing), jsone:decode(Thing, [{object_format, map}, {'keys', binary}])).


% types

-type req_id()  :: httpc:request_id().
-type req() :: {{pid(), reference()}, write | read, binary()}.
-type auth_method() :: {approle | ldap | kubernetes, binary(), binary()}.
