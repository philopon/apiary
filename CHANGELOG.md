# 0.15.2
* you can set status and response headers anywhere.
* deprecate lbs.
* add bytes, lazyBytes(~ lbs), text, lazyText, showing, string, char request body functions. these functions **append** request body.
* add reset function to reset request body to empty.

# 0.15.1
* enhance performance(especially parsing parameter).

# 0.15.0
* enhance performance(new router).
* add anyPath function.

# 0.14.0
* change First Strategy behaviour(check first param only).
* merge method and stdMethod function.
* rename function (response -> rawResponse)
* add apiary-purescript

## documentation
* changed how to generate documentation. use defaultDocumentationAction action.
* now, condition which is after document function, will be documented. use noDoc function.
* abolish ':' query document api. use (??) function.
* deprecated () route decument api. use [].


# 0.13.0
* Option that generate full embed documentation.
* Add precondition, rpHtml functions.

## API documentation page
* precondition
* multi action
* use route string as id of html


# 0.12.8
* Option that doesn't generate documentation.
* Change fail of ActionT behaviour.
  ( Pass next action -> return 500 error immediately.)

## API documentation page
* collapse


# 0.12.5
* Enhance API documentation.
* Description using variable.

# 0.12.0
* Automatically generate API documentation.
* Remove Functor restriction from ApiaryT.

# 0.11.1
* Generalize proxy.
* Fix capture behaviour.
* Add endPath function.

# 0.11.0
* Change capture internal implimentation.
* Add path, fetch pure capture function.

## apiary-clientsession
* Add prefix SessionConfig fields.


# 0.10.0
* wai-3.0.
* Add response function.
* Add MonadCatch, MonadMask instances to ActionT.
