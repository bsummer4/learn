# The Smash Language

Smash is Bourne shell syntax with Tcl features.

## Reconciling Bash and Tcl

What are the core conceptual differences between Bash and Tcl, and how can
we reconcile them?

They are actually extremely similar. The core difference is that bash
functions behave more-or-less like unix processes. They take a list of
(string) arguments, write to stdout/stderr, and return an error code.
Tcl procs are very close. They return a (strict) string and can throw an
exception.

How can we reconcile these differences?

Or, more specifically, how can we tweak the Tcl `proc` concept so that unix
processes can be cleanly substituted for them?

Let's look at some more specific, problematic examples:

### Unix processes can "return" partial data even when they exit with an error.

    function outputThenThrow () { echo hi; return 1; }

### Unix processes can "return" infinite strings.

    function infiniteResult () { while 1; do echo hi; done; }

### The "return value" of a unix process can be used either as a side-effect or as a value.

    function theOutputIsAStrangeIdea () {
        echo hi        # Using echo's output as a side-effect
        foo=$(echo hi) # Using echo's output as a value.
        echo $foo      # 
    }


### If we capture both semantics in Haskell's type system, what does that look like?

It would look like the following. Note that the Unix proc takes and returns
an input stream, where the Tcl proc is just a simple stream.

    type UnixProc = [ByteString] -> ConduitM ByteString ByteString IO Word8
    type TclProc = [ByteString] -> IO ByteString

Is there any core tcl functionality that's incompatible with the UnixProc idea?

Well, Tcl "strings" can be represented internally as structured values, what
does this mean once we start streaming them? I guess it's not a problem? We
just can't split up these values arbitrarily. So, something like this?

    # Axiom: There's an isomorphism between TclVal and ByteString.
    type SmashProc = [TclVal] -> ConduitM TclVal TclVal IO Word8

Cool, that works. Let's drop the error code and just have non-zero procs
throw an exception.

    type SmashProc = [TclVal] -> ConduitM TclVal TclVal IO ()

Tcl procs also have access to their environment. They can manipulate variables
in their stack frame and in the stack frame above them. I don't think this
causes any conflict.

    type SmashStackFrame = Map ByteString TclVal
    type TclState        = NonEmpty TclStackFrame
    type SmashIO         = StateT TclState ()
    type SmashProc       = [TclVal] -> ConduitM TclVal TclVal SmashIO ()

One complication here is that the bourne shell implicitly concatenates all
outputs of a procedure when you try to capture it as a value. Since we're
dealing with structured values that might not be have a semantically useful
concatenation operation (for example, references to objects), we can't do
this. So, what does it mean to capture output as a return value? I guess this
just dumps us into the lisp/scheme world of multiple return values:

    proc example () {
        x, y, z = $(yes | take 3)
        echo y
    }

I've always found this to be annoyingly inelegant and error-prone, but that's
not too bad. Or is it? Does this example run forever, or do we kill `yes`
once it's output two lines?

    x, y = $(yes)

Also, in this model we start to care about where the division points lie
between outputs, which unix-lang programs are very loose about. Let's say we
`cat` file. Here's an example using an imaginary `await` command.

    cat files | { x <- await; echo "==== $x ===="; }

Does the `await` operation give out each line, or does it give out the
results of each chunk that's `read()` from the pipe? Lines are an arbitrary
(and problematic) semantic choice, and `read()` depends on implementation
details of cat and also of pipes in the host operating system.

This isn't going to work! Unix process output a single streamed value,
not a stream of values. The semantics should look more like:

    type SmashProc = ([TclVal], StreamingTclVal) -> SmashIO StreamingTclVal

Is this viable? Instead of having an await operation (to listen on the
`stdin` of that proc), we would take an explicit streamed value (`stdin`)
and be able to `read` from that. This is sorta like lazy values, except
there's an active producer that can fail instead of a pure value that's
evaluated as needed. Also, it's possible to imperatively grab partial values
off of a stream without eating the whole stream.

    yes | {
        x <- read
        y <- read
        echo "first value: $x"
        echo "second value: $y"
        echo "extra values:"
        sed 's/^/an extra value: /'
    }

Can we make this into a first-class concept without insanity. Can we have
streaming values that can transparently be either in-process or between
process (pipes)?

I suppose so, we just need a variety of ways to collect data from a streaming value.

Also, how do exceptions work in a world where the producer of a streaming value is disconnected from it's consumer?

    streamingValue = { yield 3; throw foo; yield 4 }

Do we then need to have two types of values, then? Streamed values and strict
values? I guess not, we can just treat streamed values in the same way as
we treat objects: as procedures that we can takes values from.

    v = stream { cat file }
    firstChunk = [$v read]

We can have an implicit streamed values as the `stdin` associated with each
stack frame. This allows us to write bash-like code that just pulls data
from the procs input as desired.

    cat file | { x <- read; echo "first line is: $x" }

This line of thought brings us back to a more Tcl-like semantics with just
a little extra baggage. Something like this:

    type SmashStackFrame = { vars: Map ByteString TclVal, stdin: TclVal }
    type TclState        = NonEmpty TclStackFrame
    type SmashIO         = StateT TclState ()
    type SmashProc       = [TclVal] -> SmashIO ???TODO???

So what then should the return type of a SmashProc be? I guess it's just a
`TclVal` that could potentially be a stream? Some operations (like piping)
would expect a stream and you'd get an error if it wasn't one. Is this
confusing?

I think yes. Does this code make sense?

    proc yieldThenReturn {} {
        echo foo
        return "bar"
    }

Right, bash procedures always have an explicit output stream; they don't
have a (very meaningful) concept of a return value. If we adopt this
paradigm, then what's the point of having strict values in the first place?
All procedures necessarily return streams.

I suppose we can have a concept of returning values that's just not available
to unix processes. Unix processes always write to their output stream and
just return their error code as a TclVal, and all `proc`s would have an
implicit output stream as well. We would need separate syntax for grabbing
the return value of a procedure, capturing it's output as a value, and
capturing it's output as a stream.

    proc id {x} { return $x }
    x = $(echo hi)
    y = [id hi]

Sometimes we're going to want to be able to capture both the output and
the return value. It's going to be tricky to make this syntactially nice,
but I think that it works conceptually. Let's invent some shitty syntax,
and then iterate from there.

    $> x -- Capture the output into a strict value and write it into the variable `x`.
    @> y -- Capture the output into a streaming value and write it into the variable `x`.
    > fn -- Stream the output into a file

That doesn't really work, because we want to have nested expressions, not
just capture the output into a variable. How about this?

    echo $(read)          | cat
    echo [expr { 3 + 4 }] | cat

Not too bad. This gets a little hairy, since uncapture output would just go
into the output of the greater scope. Possible a little confusing, but not
too bad.

    proc thisProcWouldEchoFoo {} {
        errCode = [echo foo]
    }

Can we capture both the output and the return value?

    proc outputAndReturn {} {
        yield "foo"
        return "bar"
    }

    bar = [outputAndReturn $>foo]
    echo "foo: $foo"
    echo "bar: $bar"

Alright, this all seems plausible. It's not entirely clear that the result
of this enterprise will be sane or desirable, but it does seem possible to
tie things together in a consistent way!

The semantics would like something like this:

    newtype Ref = MkRef Word64

    data SmashVal = TclBytes !ByteString
                  | TclStr   !Text
                  | TclInt   !Int64
                  | TclRef   !Ref
                  ...

    newtype InputStream  = MkInputStream  (IO (Maybe SmashVal))
    newtype OutputStream = MkOutputStream (SmashVal -> IO ())

    data StackFrame = MkStackFrame { env:    Map ByteString SmashVal
                                   , input:  InputStream
                                   , output: OutputStream
                                   }

    newtype TclState = MkTclState (NonEmpty TclStackFrame)
    type SmashIO a   = StateT TclState IO a
    type SmashProc   = [SmashVal] -> SmashIO SmashVal
