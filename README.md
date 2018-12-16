# timed-cache

A simple elm module to help manage remote data that is accessed more often
than it is fetched.

Say you have a task that fetches the number of pending notifications via a http
call. You want to display the pending numbers in a badge on your navbar but don't
want to make the http call with every navbar page reload. You also want to ensure
that this is updated every 1 min to ensure you are showing the correct number to
your users.

A contrived example of how to use this module with the scenerio above:

```elm
import TimedCache as TC

fetchPendingNotificationCount : Task Http.Error Int
fetchPendingNotificationCount = ...

-- We first init the model that we need:
(initModel, initCmd) =
  TC.init
    fetchPendingNotificationCount
    (1000 * 60) -- Fetch a new value every 1 minute
    0 -- The initial count of notifications should be 0

-- Our applications' model
type alias Model =
  { notifications : TC.Model Http.Error Int
  , ... -- Other fields of your model
  }

type Msg =
    NoOp
  | NotificationMsg (TC.Msg Http.Error Int)

update : Model -> Msg -> (Model, Cmd Msg)
update model msg =
  case msg of
    NoOp -> (model, Cmd.none)
    NotificationMsg subMsg ->
      let (m, c) = TC.update model.notifications subMsg
      in ({model | notifications = m}, Cmd.map NotificationMsg c)

sub : Model -> Sub Msg
sub model =
  Sub.batch
    [ Sub.map NotificationMsg <| TC.sub model.notifications
    , ... -- Other subscriptions you may need
    ]

view : Model -> Html Msg
view model =
  let notificationCount = String.fromInt model.notifications.value
  in div [] [ text <| "Number of notifications: " ++ notificationCount ]

```
