#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:style 'unnumbered #:tag "lab27"]{Lab 27: Chat in a Universe}

Your goal is to create a working chat client in a universe setting.

You can either modify your working chat client, or edit the provided code for chat clients.

@link["chatclient.rkt"]{Provided Client}

The server IP to register is @bold{[IP]} and port is
@bold{[PORT]}. Refresh this page if you are having trouble connecting.

Refer to the universe documentation
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{here} for
details for connecting to the server.

@bold{Ex 1}: Implement @tt{receive-message} to be used in the chat client's . The server sends the following types of messages:

@itemlist[

  @item{@tt{(list 'enter @emph{Symbol})}, when a new client connects;}

  @item{@tt{(list 'exit @emph{Symbol})}, when a client exits;}

  @item{@tt{(list 'message @emph{Symbol} @emph{String})}, when a client sends a message;}

  @item{@tt{(list 'error @emph{String})}, when you send an invalid package to the server.}

]

Your client will need to display connections and disconnections as messages from
the server, as well as add any messages from clients to the local chat history.

@bold{Ex 2}: Implement @tt{send-message}. The server expects a message to be
sent in the form above: @tt{(list 'message @emph{Symbol} @emph{String})}. Your
client should clear its local content in order to be ready to enter and send a
new message.
