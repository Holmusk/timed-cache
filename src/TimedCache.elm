module TimedCache exposing (Model,Msg,init,update, sub, clearErrors)

{-| A module to help manage accessing values that are obtained remotely and are accessed more often than they are fetched

An example of this is maybe the notification count on your social feed which
is a value you would need to fetch over HTTP and need in multiple places
but might not want to fetch as often as you use it.

@docs Model,Msg,init,update,sub,clearErrors
-}

import Time exposing (Posix)
import Task exposing (Task)

{-|-}
type alias Model e a =
  { value : a
  -- ^ The current value of the cache.
  , maxAge : Int
  -- ^ How old can the value be before considered stale. In ms
  , errors : List e
  -- ^ List of accumulated errors
  , lastFetched : Maybe Posix
  -- ^ When was the last time this data was fetched. Nothing if unknown/never fetched
  , fetch : Task e a
  -- ^ The action to fetch the value needed. Modelled as a maybe since the action
  --   can fail. If it fails, the old value is held.
  }


{-| Create the initial model and cmds

The arguments in order are:
  + The task to fetch your value
  + The maxAge; how old can a value get before needing to be refreshed. In ms
  + An initial value
-}
init : Task e a -> Int -> a -> (Model e a, Cmd (Msg e a))
init task maxAge a =
  let
    model =
      { value = a
      , maxAge = maxAge
      , lastFetched = Nothing
      , fetch = task
      , errors = []
      }
  in
    (model, fetchValue model)

{-|-}
type Msg e a =
    Tick Posix
  | NewValue (Result e a)
  | UpdateLastFetched Posix
  | ClearErrors

{-|-}
update : Model e a -> Msg e a -> (Model e a, Cmd (Msg e a))
update model msg =
  case msg of
    Tick now -> handleTick model now
    NewValue newValue -> handleNewValue model newValue
    UpdateLastFetched now -> ({model | lastFetched = Just now }, Cmd.none)
    ClearErrors -> ({model | errors = []}, Cmd.none)

handleTick : Model e a -> Posix -> (Model e a, Cmd (Msg e a))
handleTick model now =
  let shouldFetch =
        case model.lastFetched of
          Just lastFetchedTime -> Time.posixToMillis now >= Time.posixToMillis lastFetchedTime + model.maxAge
          Nothing -> True
      cmd = if shouldFetch then fetchValue model else Cmd.none
      newModel = if shouldFetch then { model | lastFetched = Just now } else model
  in (newModel, cmd)

handleNewValue : Model e a -> Result e a -> (Model e a, Cmd (Msg e a))
handleNewValue model newValue =
  case newValue of
    Ok a -> ({model | value = a}, updateLastFetched)
    Err e -> ({model | errors = e :: model.errors}, updateLastFetched)

fetchValue : Model e a -> Cmd (Msg e a)
fetchValue model = Task.attempt NewValue model.fetch

updateLastFetched : Cmd (Msg e a)
updateLastFetched = Task.perform UpdateLastFetched Time.now

{-|-}
clearErrors : Msg e a
clearErrors = ClearErrors

{-|-}
sub : Model e a -> Sub (Msg e a)
sub model = Time.every (toFloat model.maxAge) Tick
