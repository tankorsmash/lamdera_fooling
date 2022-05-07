### User Workflow


#### Connection

1. Frontend connects
2. Backend gets a subscription that has its session and client id
3. Backend sends that client the current click count to properly initialize it

#### Interaction

1. User clicks, HTML fires a `FrontendMsg`
2. Frontend processes the `Msg`, fires a ToBackend `Cmd`
3. Backend receives the `Cmd` and a `Msg`
4. Backend increments its "database" model
5. Backend broadcasts the new click count to all Frontend clients via a `Cmd` with `Lamdera.sendToFrontend`
6. Each Frontend client receives a `ToFrontend` message which it then uses to update its local model


### Deploying


1. Make a code change in `src/`
2. `git commit -am "commit message"`
3. `lamdera check` to generate a migration file (see `/src/Evergreen/Migrate/V4.elm`, similar to Django's database migrations, only more manual)
4. edit the migration and ensure all updated old types can be converted into new types
5. `lamdera deploy` to deploy to the server
6. refresh `appname.lamdera.app` to see it live (http://second.lamdera.app, in my case)
