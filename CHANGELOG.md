# 1.2.1
* reduce dependencies.
* relax version restriction of monad-control.
* move pure capture function.
* export hoistActionT, focus', noExtension.

# 1.2.0
* good bye wai-2.
* add Web.Apiary.Develop module for develop static file.
* *change* response body function behaviour(append to set).
* add append* functions.
* remove MonadHas class. add MonadExt class.
* add action vault for extension.
* change binary package to cereal package.

## apiary-websockets
* fix actionWithWebSockets don't exec action.

## apiary-session/apiary-clientsession/apiary-authenticate
* new session system.

# 1.1.4
* enhance API documentation.
* add MonadHas class.
* add devFile action.

# 1.1.3
* wildcard, parameter Accept header.

## apiary-mongoDB(1.1.1)
* MongoAccess class

## apiary-logger(1.1.1)
* Logging class

## apiary-persistent(1.1.1)
* RunSQL class

# 1.1.2
* re-export Web.Apiary from Web.Apiary.Heroku.

# 1.1.1
* add request test to web API documentation.

# 1.1.0
* Added Extension class. now Extension can add middleware.
* Added apiary-helics submodule.

# 1.0.0
* included named parameter. motivation:

    changing filter order

    `filterA . filterB . action $ \a b -> act` to

    `filterB . filterA . action $ \b a -> act`
                                   ~~~
    changes argument order. it's too bad...

* simplified Strategy type class.
* removed DEPRECATED functions.
* changed runner type.
* changed module structure.
* API freeze. I'll pay attention to compatibility, maybe...

# 0.17.2
* send 302 if file not midified.

# 0.17.1
* relax switchQuery.

              old   new
    key       True  True
    key=true  False True
    key=false False False

# 0.17.0
* fix not accept in multi Accept header.
* add greedy path capture('\*\*').
* add switchQuery filter.
* add Heroku module.
* add google analytics support to API documentation.

## Extension API
* change interface.
* fix bug which immediately finalize.
* deprecate preAction.
* Category initializer only ghc-7.8+.

## new extensions
* apiary-mongoDB
* apiary-memcached

# 0.16.0
* new Extension API.
* add middleware function.
* remove Typeable restriction from Path/Query class.
* add Optional strategy, (=?!:) query fetcher.
* add accept filter.
* add Path/Query instances to Day.

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
