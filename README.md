# elm-django-channels

A library to ease communication between an [Elm](https://elm-lang.org) based frontend and a [Django Channels](https://channels.readthedocs.io/) based Websocket bakend.

The library assumes that the [data binding](https://channels.readthedocs.io/en/latest/binding.html) functionality of Django Channels is used. There is also a very simple "initial data" loading mechanism for those who wish to use websockets for everything (instead of for instance a API endpoint which provides the initial data).

## API documentation

See http://package.elm-lang.org/packages/valberg/elm-django-channels/latest

## Example

There is a very simple example Todo application (surprise!) at https://github.com/valberg/elm-django-channels-example/, which uses all of the functionality of `elm-django-channels`
